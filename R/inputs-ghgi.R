#' inputs-ghgi.R
#' These functions load and process data from the Greenhouse Gas Inventory (GHGI).

# Read gas system activity data
get_ghgi_gas_act <- function(
  ghgi_gas_xlsx
) {
  
  ## Natural gas annex
  # read data
  gas_act_dta = setDT(read.xlsx(ghgi_gas_xlsx, sheet = "Sheet1", startRow = 2, colNames = T))
  # reshape to long
  gas_act_dta = melt.data.table(gas_act_dta, id.vars = c("Segment", "Source", "Units"), variable.name = c("Year"), value.name = c("Count"))
  # remove entries without data
  gas_act_dta = gas_act_dta[!(is.na(Count))]
  
  gas_act_dta
}

# Read gas system emission factor data
get_ghgi_gas_ef <- function(
  ghgi_gas_xlsx
) {
  
  ## Natural gas annex
  # read data
  gas_ef_dta <- setDT(read.xlsx(ghgi_gas_xlsx, sheet = "Sheet2", startRow = 2, colNames = T))
  # reshape to long
  gas_ef_dta <- melt.data.table(gas_ef_dta, id.vars = c("Segment", "Source", "Units"), variable.name = c("Year"), value.name = c("EF"))
  # remove entries without data
  gas_ef_dta <- gas_ef_dta[!(is.na(EF))]
  
  gas_ef_dta
}

# Read petroleum system activity data
get_ghgi_oil_act <- function(
  ghgi_oil_xlsx
) {
  
  # Petroleum annex
  # read data
  oil_act_dta = setDT(read.xlsx(ghgi_oil_xlsx, sheet = "Sheet1", startRow = 2, colNames = T))
  # reshape to long
  oil_act_dta = melt.data.table(oil_act_dta, id.vars = c("Segment", "Source", "Units"), variable.name = c("Year"), value.name = c("Count"))
  # remove entries without data
  oil_act_dta = oil_act_dta[!(is.na(Count))]
  
  oil_act_dta
}

# Read petroleum system emission factor data
get_ghgi_oil_ef <- function(
  ghgi_oil_xlsx
) {
  
  ## Petroleum annex
  # read data
  oil_ef_dta <- setDT(read.xlsx(ghgi_oil_xlsx, sheet = "Sheet2", startRow = 2, colNames = T))
  # reshape to long
  oil_ef_dta <- melt.data.table(oil_ef_dta, id.vars = c("Segment", "Source", "Units"), variable.name = c("Year"), value.name = c("EF"))
  # remove entries without data
  oil_ef_dta <- oil_ef_dta[!(is.na(EF))]
  
  oil_ef_dta
  
}

# Merge and format activity and emission factor data
summ_ghgi_equip_data <- function(
  gas_act_dta, 
  oil_act_dta, 
  gas_ef_dta, 
  oil_ef_dta
){
  
  # well counts
  gas_well_count = gas_act_dta[Segment == "PROD" & Source == "Total Active Gas Wells" & Year == 2019, Count]
  oil_well_count = oil_act_dta[Segment == "PROD" & Source == "Total Oil Wells" & Year == 2019, Count]
  
  # equipment lists (for different levels of EF disaggregation)
  equip_list_four = c(rep("Wellheads", 4))
  equip_list_three = c(rep("Separators", 3),
                       rep("Headers", 3))
  equip_list_two = c(rep("Dehydrators", 2), 
                     rep("Compressors", 2), 
                     rep("LB Controllers", 2), 
                     rep("HB Controllers", 2),
                     rep("IB Controllers", 2),
                     rep("Pumps", 2),
                     rep("Heaters", 2),
                     rep("Heater/Treaters", 2),
                     rep("Meters/Piping", 2))
  
  # create data table with equipment counts and emissions factors
  ghgi_prod_equip_data = 
    data.table(Segment = "PROD",
               Source = c(equip_list_four, equip_list_three, equip_list_two), 
               Type = c(rep(c("Gas", "Gas", "Oil", "Oil"), length(unique(equip_list_four))),
                        rep(c("Gas", "Oil", "Oil"), length(unique(equip_list_three))), 
                        rep(c("Gas", "Oil"), length(unique(equip_list_two)))),
               SubType = c(rep(c("Conv.", "HF", "Heavy Crude", "Light Crude"), length(unique(equip_list_four))),
                           rep(c(NA_character_, "Heavy Crude", "Light Crude"), length(unique(equip_list_three))), 
                           rep(NA_character_, length(equip_list_two))), 
               Count = c(gas_act_dta[Segment == "PROD" & Source == "Non-associated Gas Wells (less fractured wells)" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Gas Wells with Hydraulic Fracturing" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Oil Wellheads (heavy crude)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Oil Wellheads (light crude)" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Separators" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Separators (heavy crude)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Separators (light crude)" & Year == 2019, Count],
                         0,
                         oil_act_dta[Segment == "PROD" & Source == "Headers (heavy crude)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Headers (light crude)" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Dehydrators" & Year == 2019, Count],
                         0,
                         gas_act_dta[Segment == "PROD" & Source == "Compressors" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Compressors" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "(Low Bleed)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Pneumatic Devices, Low Bleed" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "(High Bleed)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Pneumatic Devices, High Bleed" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "(Intermittent Bleed)" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Pneumatic Devices, Int Bleed" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Chemical Injection Pumps" & Year == 2019, Count],
                         oil_act_dta[Segment == "PROD" & Source == "Chemical Injection Pumps" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Heaters" & Year == 2019, Count],
                         0,
                         0,
                         oil_act_dta[Segment == "PROD" & Source == "Heater/Treaters (light crude)" & Year == 2019, Count],
                         gas_act_dta[Segment == "PROD" & Source == "Meters/Piping" & Year == 2019, Count],
                         0),
               EF = c(gas_ef_dta[Segment == "PROD" & Source == "Non-associated Gas Wells (less fractured wells)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Gas Wells with Hydraulic Fracturing" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Oil Wellheads (heavy crude)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Oil Wellheads (light crude)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Separators" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Separators (heavy crude)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Separators (light crude)" & Year == 2019, EF],
                      0,
                      oil_ef_dta[Segment == "PROD" & Source == "Headers (heavy crude)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Headers (light crude)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Dehydrators" & Year == 2019, EF],
                      0,
                      gas_ef_dta[Segment == "PROD" & Source == "Compressors" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Compressors" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "(Low Bleed)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Pneumatic Devices, Low Bleed" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "(High Bleed)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Pneumatic Devices, High Bleed" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "(Intermittent Bleed)" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Pneumatic Devices, Int Bleed" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Chemical Injection Pumps" & Year == 2019, EF],
                      oil_ef_dta[Segment == "PROD" & Source == "Chemical Injection Pumps" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Heaters" & Year == 2019, EF],
                      0,
                      0,
                      oil_ef_dta[Segment == "PROD" & Source == "Heater/Treaters (light crude)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "PROD" & Source == "Meters/Piping" & Year == 2019, EF],
                      0)
    )
  
  # create aggregate controllers row
  ghgi_prod_equip_data = rbindlist(list(ghgi_prod_equip_data, ghgi_prod_equip_data[grepl("Controllers", Source, ignore.case = T),
                                                                                   .(Source = "Controllers", Count = sum(Count), EF = weighted.mean(EF, Count)), 
                                                                                   by = .(Segment, Type, SubType)]), 
                                   use.names = T)
  
  # calculate weighted-average emissions factors
  ghgi_prod_equip_data = ghgi_prod_equip_data[, .(Count = sum(Count), EF = weighted.mean(EF, Count)), by = .(Segment, Source, Type)][, EF := fifelse(is.nan(EF), 0, EF)]
  
  # calculate equipment per well
  ghgi_prod_equip_data[, Equip_Per_Well := Count*fifelse(Type == "Gas", 1/gas_well_count, 1/oil_well_count)]
  
  # equipment lists (for different levels of EF disaggregation)
  equip_list = c("LB Controllers", "IB Controllers", "HB Controllers", "Pumps")
  
  # create data table with emissions factors
  ghgi_nonprod_equip_data = 
    data.table(Segment = rep(segment_order[-1], each = length(equip_list)),
               Source = rep(equip_list, length(segment_order)-1), 
               Type = "Gas",
               SubType = NA_character_, 
               Count = c(gas_act_dta[Segment == "G&B" & Source == "Low-Bleed Pneumatic Devices" & Year == 2019, Count],
                         gas_act_dta[Segment == "G&B" & Source == "Intermittent Bleed Pneumatic Devices" & Year == 2019, Count],
                         gas_act_dta[Segment == "G&B" & Source == "High-bleed Pneumatic Devices" & Year == 2019, Count],
                         gas_act_dta[Segment == "G&B" & Source == "Pneumatic Pumps" & Year == 2019, Count],
                         NA_real_,
                         NA_real_,
                         NA_real_,
                         NA_real_,
                         gas_act_dta[Segment == "TRANS" & Source == "(Low Bleed)" & Year == 2019, Count],
                         gas_act_dta[Segment == "TRANS" & Source == "(Intermittent Bleed)" & Year == 2019, Count],
                         gas_act_dta[Segment == "TRANS" & Source == "(High Bleed)" & Year == 2019, Count],
                         NA_real_,
                         gas_act_dta[Segment == "STOR" & Source == "(Low Bleed)" & Year == 2019, Count],
                         gas_act_dta[Segment == "STOR" & Source == "(Intermittent Bleed)" & Year == 2019, Count],
                         gas_act_dta[Segment == "STOR" & Source == "(High Bleed)" & Year == 2019, Count],
                         NA_real_),
               EF = c(gas_ef_dta[Segment == "G&B" & Source == "Low-Bleed Pneumatic Devices" & Year == 2019, EF],
                      gas_ef_dta[Segment == "G&B" & Source == "Intermittent Bleed Pneumatic Devices" & Year == 2019, EF],
                      gas_ef_dta[Segment == "G&B" & Source == "High-bleed Pneumatic Devices" & Year == 2019, EF],
                      gas_ef_dta[Segment == "G&B" & Source == "Pneumatic Pumps" & Year == 2019, EF],
                      NA_real_,
                      NA_real_,
                      NA_real_,
                      NA_real_,
                      gas_ef_dta[Segment == "TRANS" & Source == "(Low Bleed)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "TRANS" & Source == "(Intermittent Bleed)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "TRANS" & Source == "(High Bleed)" & Year == 2019, EF],
                      NA_real_,
                      gas_ef_dta[Segment == "STOR" & Source == "(Low Bleed)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "STOR" & Source == "(Intermittent Bleed)" & Year == 2019, EF],
                      gas_ef_dta[Segment == "STOR" & Source == "(High Bleed)" & Year == 2019, EF],
                      NA_real_)
    )
  
  ghgi_equip_data = rbindlist(list(ghgi_prod_equip_data, ghgi_nonprod_equip_data), fill = T)
  
  # calculate emissions factors in tons-per year
  ghgi_equip_data = ghgi_equip_data[, .(Segment, Source, Type, Count, Equip_Per_Well,
                                        CH4_TPY = EF * lbs_per_kg * tons_per_lb)
  ]
  
  ghgi_equip_data
  
}
