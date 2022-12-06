#' emissions-factors-subpartW.R
#' This function reads and formats equipment fugitive emission rates from the
#' current Greenhouse Gas Reporting Rule 
#' (https://www.ecfr.gov/current/title-40/chapter-I/subchapter-C/part-98)
#' and the 2022 proposed revisions 
#' (https://www.regulations.gov/document/EPA-HQ-OAR-2019-0424-0001).

# Read component counts from current subpart W (gas)
get_gas_component_counts_subpartw_current <- function(
  subpartW_f, 
  gas_sheet
) {

  gas_comp <- setDT(read.xlsx(subpartW_f, sheet = gas_sheet, rows = 4:16, colNames = T))
  setnames(gas_comp, c("Major.equipment", "Open-ended.lines", "Pressure.relief.valves"), c("Equipment", "OEL", "PRV"))
  gas_comp <- melt(gas_comp, id.vars = c("Equipment", "Type", "Region"), measure.vars = c("Valves", "Connectors", "OEL", "PRV"),
                   variable.name = "Component", value.name = "Component.Count")

  # Need to use "Other" emissions factor for PRV

  gas_comp

}

# Read component counts from current subpart W (oil)
get_oil_component_counts_subpartw_current <- function(
  subpartW_f, 
  oil_sheet
) {

  oil_comp <- setDT(read.xlsx(subpartW_f, sheet = oil_sheet, rows = 4:12, colNames = T))
  setnames(oil_comp, c("Major.equipment", "Open-ended.lines", "Other.components"), c("Equipment", "OEL", "Other"))
  oil_comp <- melt(oil_comp, id.vars = c("Equipment", "Type", "Region"), measure.vars = c("Valves", "Flanges","Connectors", "OEL", "Other"),
                   variable.name = "Component", value.name = "Component.Count")

  oil_comp

}

# Combine component counts from current subpart W
get_equip_comp_counts_subpartw_current <- function(
  subpartw_xlsx = "data-raw/emissions_factors.xlsx",
  gas_sheet = "Table W-1B Subpart W",
  oil_sheet = "Table W-1C Subpart W"
) {
  
  gas_comp_counts <- get_gas_component_counts_subpartw_current(subpartw_xlsx, gas_sheet)
  oil_comp_counts <- get_oil_component_counts_subpartw_current(subpartw_xlsx, oil_sheet)
  comp_counts <-rbind(gas_comp_counts, oil_comp_counts)[
    Component != "Flanges", .(Component.Count = sum(Component.Count)), by = .(Equipment, Type, Region)][
      , .(Component.Count = mean(Component.Count)), by = .(Equipment, Type)
    ]
  
  comp_counts
  
}

# Read emission factors from proposed subpart W (2022)
get_subpartw_equip_ef_proposal <- function(
  subpartw_xlsx = "data-raw/emissions_factors.xlsx", 
  ef_sheet = "Table W-1A Subpart W"
) {
  
  # emissions factors are initially in scf/hour/equipment
  ef_table <- setDT(read.xlsx(subpartw_xlsx, sheet = ef_sheet, colNames = T))
  setnames(ef_table, c("Emission.factor.(scf/hour/component)", "Equipment"), c("ef", "Source"))
  
  # some cleaning 
  ef_table[, Source := case_when(
    str_detect(Source, "Low Bleed") ~ "LB Controllers",
    str_detect(Source, "Intermittent Bleed") ~ "IB Controllers",
    str_detect(Source, "High Bleed") ~ "HB Controllers",
    str_detect(Source, "Heater Treater") ~ "Heater/Treater",
    TRUE ~ Source
  )]
  
  # calculate tons/year/equipment for methane. EF in whole gas per hour per component
  ef_table[, `:=` (
    CH4_TPY = ef * fifelse(Type == "Gas", methane_per_whole_gas$Gas, methane_per_whole_gas$Oil) * scf_to_kg * lbs_per_kg * tons_per_lb * hours_per_year
  )]
  
  ef_table
  
}




