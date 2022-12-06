#' secondary-emissions-funs-ong.R
#' These functions calculate secondary emission impacts associated with control
#' options that feature combustion.

# Format and assign segments
assign_segment <- function(
  .dta
) {
  
  .dta %>% mutate(segment = case_when(
    str_detect(segment, "^PROC$") ~ "Processing",
    str_detect(segment, "^(TRANS)|(STOR)$") ~ "Transmission",
    TRUE ~ "Production"
    
  ))

}

# Format and assign emissions categories
assign_emissions_category <- function(
  .dta
) {
  
  .dta %>%
    mutate(emissions_category = case_when(
      str_detect(mp, "pneu_dev") ~ "Pneumatic Devices",
      str_detect(mp, "controller") ~ "Pneumatic Devices",
      str_detect(mp, "pump") ~ "Pneumatic Devices", 
      str_detect(mp, "storage_vessel") ~ "Storage Vessels",
      str_detect(mp, "wellsite") ~ "Fugitives",
      str_detect(mp, "procplant") ~ "Fugitives",
      str_detect(mp, "storstation") ~ "Fugitives",
      str_detect(mp, "transstation") ~ "Fugitives",
      str_detect(mp, "gbstation") ~ "Fugitives",
      str_detect(mp, "comp") ~ "Compressors",
      str_detect(mp, "liq_unl") ~ "Liquids Unloading"
    ))
  
}

# Get secondary emissions factors
secondary_emissions_assumptions <- function(
  
) {
  
  mg_ratios <- tibble(segment = c("Production", "Processing", "Transmission"), methane_to_gas_ratio = c(.829, .870, .928))
  combustion_efficiency <- .95
  
  #MMbtu/Mcf
  gas_energy_content <- list(oil_well_compl = 1089*(1000/1000000), other_prod_and_ts = 1025*(1000/1000000))
  
  #tons/MMbtu
  #CO2 is metric tons, so it's converted to short tons
  emissions_factor <- list(THC = .14/2000, CO = .37/2000, NOx = .068/2000, PM = .00256/2000, CO2 = (132.277357308 / 1000)*(1/metric_tons_per_short_ton)) 
  
  list(mg_ratios = mg_ratios, combustion_efficiency = combustion_efficiency,
       methane_mcf_per_ton = methane_mcf_per_ton, gas_energy_content = gas_energy_content,
       emissions_factor = emissions_factor)
  
}

# Calculate secondary emissions
get_secondary_emissions_data <- function(
  scn_comp
) {
  
  assumptions <- secondary_emissions_assumptions()
  mg_ratios <- assumptions$mg_ratios
  methane_mcf_per_ton <- assumptions$methane_mcf_per_ton
  combustion_efficiency <- assumptions$combustion_efficiency
  gas_energy_content <- assumptions$gas_energy_content
  emissions_factor <- assumptions$emissions_factor
  
  # Get data, calculate gas/energy not captured
  dta <- scn_comp %>% select(mp, attrs, segment, location, vintage_bin, year, Methane, gas_capture) %>%
    assign_segment() %>%
    left_join(mg_ratios, by = "segment") %>%
    mutate(total_gas = -Methane/methane_to_gas_ratio*methane_mcf_per_ton,
           gas_not_captured = total_gas - gas_capture,
           energy_not_captured = gas_not_captured*gas_energy_content$other_prod_and_ts)
  
  # calculate secondary emissions, in short tons
  dta <- dta %>% mutate(THC = energy_not_captured*(combustion_efficiency*emissions_factor$THC),
                        CO = energy_not_captured*(combustion_efficiency*emissions_factor$CO),
                        NOx = energy_not_captured*(combustion_efficiency*emissions_factor$NOx),
                        PM = energy_not_captured*(combustion_efficiency*emissions_factor$PM),
                        CO2 = energy_not_captured*(combustion_efficiency*emissions_factor$CO2)) %>%
    select(mp, year, segment, attrs, Methane, total_gas, gas_capture, gas_not_captured, energy_not_captured,
           THC, CO, NOx, PM, CO2)
  
  dta
  
}

# Calculate aggregates of secondary emissions
# Only storage vessels have vapor combustion, so aggregate is total secondary emissions from storage vessels
get_secondary_emissions_table_aggregate <- function(
  scn_comp, 
  mp_list, 
  mp_attrs_list, 
  ng_price
) {
  
  dta <- get_secondary_emissions_data(scn_comp)
  
  # Secondary emissions impacts by year
  year_tbl <- dta %>% group_by(year) %>%
    summarize(
    across(.cols = c("THC", "CO", "NOx", "PM", "CO2"), .fns = ~sum(.x), .names = "{.col}"),
    .groups = "drop"
  ) %>%
    mutate(year = as.character(year))
  
  tot_tbl <- dta %>%
    summarize(
      across(.cols = c("THC", "CO", "NOx", "PM", "CO2"), .fns = ~sum(.x), .names = "{.col}"),
      .groups = "drop"
    ) %>% 
    mutate(year = "Total") %>% relocate(year)
  
  tbl <- bind_rows(year_tbl, tot_tbl)
  
  tbl
  
}

# Construct secondary emissions table
get_secondary_emissions_table_breakout <- function(
  scn_comp, 
  years = c(2026, 2035)
) {
  
  dta <- get_secondary_emissions_data(scn_comp) %>% assign_emissions_category() %>%
    filter(year %in% years)
  
  # Secondary emissions impacts by year
  tbl <- dta %>% group_by(emissions_category, year) %>%
    summarize(
      across(.cols = c("THC", "CO", "NOx", "PM", "CO2"), .fns = ~sum(.x), .names = "{.col}"),
      .groups = "drop"
    ) %>%
    mutate(year = as.character(year)) %>%
    relocate(emissions_category, year)
  
  tbl
  
}










