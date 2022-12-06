#' emissions-factors-rutherford.R
#' This function reads and formats equipment fugitive emission rates from the
#' Rutherford et al. (2021) study 
#' (https://www.nature.com/articles/s41467-021-25017-4).

# Emission factors from Rutherford et al. (2021) Supplementary Table 3 and 4
get_rutherford_equip_ef <- function(subpartw_xlsx = "data-raw/emissions_factors.xlsx", ef_sheet = "Rutherford") {
  
  # emission factors in kg/day
  ef_table <- setDT(read.xlsx(subpartw_xlsx, sheet = ef_sheet, colNames = T))
  setnames(ef_table, c("Emission.Factor.(kg/day)", "Equipment"), c("ef", "Source"))
  
  # some cleaning 
  ef_table[, Source := case_when(
    str_detect(Source, "Compressor") ~ "Compressor",
    str_detect(Source, "Heater") ~ "Heater/Treater",
    TRUE ~ Source
  )]
  
  # calculate tons/year/equipment for methane. EF in kg CH4 per hour per component
  ef_table[, `:=` (
    CH4_TPY = ef * lbs_per_kg * tons_per_lb * days_per_year
  )]
  
  ef_table

}