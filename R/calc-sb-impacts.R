#' calc-sb-impacts.R
#' These functions clean data, perform impacts calculations, and format results 
#' for the small business analysis.

# -----------------------------------------------------------------------------------------------------------------#

# ==== Functions ==== #

# This section defines the functions that clean the parent/cost data and construct the RIA tables. 

# Takes parent data for a segment and produces revenue summary info for table
get_revenue_summary <- function(
  .dta, 
  .segment
) {
  
  .dta %>% group_by(SB.Status.Strict) %>%
    summarise(N = n(), 
              Mean.Revenue = mean(Revenue, na.rm = TRUE),
              Median.Revenue = median(Revenue, na.rm = TRUE)) %>%
    mutate(Segment = .segment) %>% relocate(Segment)
  
}

# Takes cost data for a segment and produces cost summary info for table
get_cost_summary <- function(
  .dta, 
  .segment
) {
  
  .dta %>% 
    select(SB.Status.Strict, cost_ann_7, cost_ann_7_wgas, cost_onetime, cost_capital, cost_om) %>%
    group_by(SB.Status.Strict) %>%
    summarise(N = n(),
              cost_ann_7 = mean(cost_ann_7, na.rm = TRUE),
              cost_ann_7_wgas = mean(cost_ann_7_wgas, na.rm = TRUE),
              cost_onetime = mean(cost_onetime, na.rm = TRUE),
              cost_capital = mean(cost_capital, na.rm = TRUE),
              cost_om = mean(cost_om, na.rm = TRUE)) %>%
    mutate(Segment = .segment) %>% relocate(Segment)
  
}

# Takes cost data for a segment and produces cost summary info for table
get_median_cost_summary <- function(
  .dta, 
  .segment
) {
  
  .dta %>% 
    select(SB.Status.Strict, cost_ann_7, cost_ann_7_wgas, cost_onetime, cost_capital, cost_om) %>%
    group_by(SB.Status.Strict) %>%
    summarise(N = n(),
              cost_ann_7 = median(cost_ann_7, na.rm = TRUE),
              cost_ann_7_wgas = median(cost_ann_7_wgas, na.rm = TRUE),
              cost_onetime = median(cost_onetime, na.rm = TRUE),
              cost_capital = median(cost_capital, na.rm = TRUE),
              cost_om = median(cost_om, na.rm = TRUE)) %>%
    mutate(Segment = .segment) %>% relocate(Segment)
  
}


# Takes cost data for a segment and produces CSR summary info for table
get_csr_summary <- function(
  .dta, 
  .segment
) {
  
  .dta %>% 
    select(SB.Status.Strict, csr_w_gas, csr_wo_gas, csr_onetime) %>%
    group_by(SB.Status.Strict) %>%
    mutate(g1 = csr_wo_gas > .01, g3 = csr_wo_gas > .03,
           g1_wgas = csr_w_gas > .01, g3_wgas = csr_w_gas > .03,
           g1_onetime = csr_onetime > .01, g3_onetime = csr_onetime > .03) %>%
    summarise(N = n(),
              csr_w_gas = mean(csr_w_gas, na.rm = TRUE),
              csr_wo_gas = mean(csr_wo_gas, na.rm = TRUE),
              csr_w_gas_greater_than_1 = sum(g1_wgas),
              csr_w_gas_greater_than_3 = sum(g3_wgas),
              csr_wo_gas_greater_than_1 = sum(g1),
              csr_wo_gas_greater_than_3 = sum(g3),
              csr_onetime_greater_than_1 = sum(g1_onetime),
              csr_onetime_greater_than_3 = sum(g3_onetime)) %>%
    mutate(Segment = .segment) %>% relocate(Segment)
  
}

# -----------------------------------------------------------------------------------------------------------------#

# ==== Costs ==== #

# This section constructs the data that is fed to functions to create the CSR and median cost summary tables.
get_sb_costs <- function(
  sb_parent_data, 
  scn_comps_supplemental, 
  scn_proj_nonwellsite, 
  wellsite_bins_proj, 
  mp_list_supp, 
  mp_attrs_list_supp
){
  
  # ==== Unpack data ==== #
  sb_gb_data <- sb_parent_data$compressors %>% filter(Gathering.Stations > 0)
  sb_ts_data <- sb_parent_data$compressors %>% filter(TS.Stations > 0)
  sb_processor_data <- sb_parent_data$processors
  sb_wellsite_data <- sb_parent_data$wellsites %>% filter(Has.Completion.2019 > 0)
  
  # Proportions of non-well sites in Colorado/California
  CO_CA_props <- get_CO_CA_props(scn_proj_nonwellsite)
  proc_prop <- CO_CA_props$proc_prop
  ts_prop <- CO_CA_props$ts_prop 
  gb_prop <- CO_CA_props$gb_prop
  
  # Unpack cost per facility data
  sb_scn_comp <- scn_comps_supplemental$`baseline-prop`
  sb_costs_raw <- get_sb_costs_raw(sb_scn_comp, type = "NSPS", mp_list_supp, mp_attrs_list_supp) %>% get_sb_cost_source()
  
  sb_wellsite_costs <- get_sb_cost_per_wellsite(sb_costs_raw, wellsite_bins_proj)
  sb_proc_costs <- get_sb_cost_per_processor(sb_costs_raw)
  sb_ts_costs <- get_sb_cost_per_TS(sb_costs_raw)
  sb_gb_costs <- get_sb_cost_per_GB(sb_costs_raw)
  
  # Calculate Wellsite Costs
  # Adjusts for Wellhead-only sites and CO/CA state policy
  sb_wellsite_cost_data <- sb_wellsite_data %>%
    mutate(Wellsites = (Completed.Wellsites - CO.CA.NM.PA.Completed.Wellsites), Non.Low.Wellsites = Wellsites) %>%
    mutate(cost_ann_7 = Wellsites*sb_wellsite_costs$wo_gas + Liquids.Unloading.Events*sb_wellsite_costs$wo_gas_liq_unl + Non.Low.Wellsites*sb_wellsite_costs$wo_gas_tank,
           cost_ann_7_wgas = Wellsites*sb_wellsite_costs$w_gas + Liquids.Unloading.Events*sb_wellsite_costs$w_gas_liq_unl + Non.Low.Wellsites*sb_wellsite_costs$w_gas_tank,
           cost_onetime = Wellsites*sb_wellsite_costs$onetime + Liquids.Unloading.Events*sb_wellsite_costs$onetime_liq_unl + Non.Low.Wellsites*sb_wellsite_costs$onetime_tank,
           cost_capital = Wellsites*sb_wellsite_costs$capital + Liquids.Unloading.Events*sb_wellsite_costs$capital_liq_unl + Non.Low.Wellsites*sb_wellsite_costs$capital_tank,
           cost_om = Wellsites*sb_wellsite_costs$om + Liquids.Unloading.Events*sb_wellsite_costs$om_liq_unl + Non.Low.Wellsites*sb_wellsite_costs$om_tank) %>% 
    select(Ultimate.Parent, SB.Status.Strict, Revenue, contains("cost_"), Wellsites, Non.Low.Wellsites, Liquids.Unloading.Events, Gas.Mcf, Oil.Bbl, Product.Revenue, Has.Completion.2019) %>%
    mutate(csr_wo_gas = cost_ann_7 / Revenue, csr_w_gas = cost_ann_7_wgas / Revenue, csr_onetime = cost_onetime / Revenue)
  
  # Calculate Processor Costs
  # Adjusts for CO/CA state policy
  sb_proc_cost_data <- sb_processor_data %>%
    mutate(cost_ann_7 = (Proc.Plants - CO.CA.Proc.Plants)*sb_proc_costs$wo_gas, 
           cost_ann_7_wgas = (Proc.Plants - CO.CA.Proc.Plants)*sb_proc_costs$w_gas,
           cost_onetime = (Proc.Plants - CO.CA.Proc.Plants)*sb_proc_costs$onetime,
           cost_capital = (Proc.Plants - CO.CA.Proc.Plants)*sb_proc_costs$capital,
           cost_om = (Proc.Plants - CO.CA.Proc.Plants)*sb_proc_costs$om) %>%
    select(Ultimate.Parent, SB.Status.Strict, Revenue, contains("cost_"), Proc.Plants) %>%
    mutate(csr_wo_gas = cost_ann_7 / Revenue, csr_w_gas = cost_ann_7_wgas / Revenue, csr_onetime = cost_onetime / Revenue) 
  
  # Calculate TS costs
  # Adjusts for CO/CA state policy
  
  sb_ts_cost_data <- sb_ts_data %>%
    mutate(cost_ann_7 = (1 - ts_prop)*TS.Stations*sb_ts_costs$wo_gas, 
           cost_ann_7_wgas = (1 - ts_prop)*TS.Stations*sb_ts_costs$w_gas,
           cost_onetime = (1 - ts_prop)*TS.Stations*sb_ts_costs$onetime,
           cost_capital = (1 - ts_prop)*TS.Stations*sb_ts_costs$capital,
           cost_om = (1 - ts_prop)*TS.Stations*sb_ts_costs$om) %>%
    select(Ultimate.Parent, SB.Status.Strict, Revenue, contains("cost_"), TS.Stations) %>%
    mutate(csr_wo_gas = cost_ann_7 / Revenue, csr_w_gas = cost_ann_7_wgas / Revenue, csr_onetime = cost_onetime / Revenue) 
  
  # Calculate GB costs
  # Adjusts for CO/CA state policy
  
  sb_gb_cost_data <- sb_gb_data %>%
    mutate(cost_ann_7 = (1 - gb_prop)*Gathering.Stations*sb_gb_costs$wo_gas, 
           cost_ann_7_wgas = (1 - gb_prop)*Gathering.Stations*sb_gb_costs$w_gas,
           cost_onetime = (1 - gb_prop)*Gathering.Stations*sb_gb_costs$onetime,
           cost_capital = (1 - gb_prop)*Gathering.Stations*sb_gb_costs$capital,
           cost_om = (1 - gb_prop)*Gathering.Stations*sb_gb_costs$om) %>%
    select(Ultimate.Parent, SB.Status.Strict, Revenue, contains("cost_"), Gathering.Stations) %>%
    mutate(csr_wo_gas = cost_ann_7 / Revenue, csr_w_gas = cost_ann_7_wgas / Revenue, csr_onetime = cost_onetime / Revenue) 
  
  sb_cost_data = list(wellsites = sb_wellsite_cost_data, processors = sb_proc_cost_data, 
                      gb_compressors = sb_gb_cost_data, ts_compressors = sb_ts_cost_data)
  
}

# -----------------------------------------------------------------------------------------------------------------#

# ==== Revenue Table ==== #

# This section creates the revenue summary table in the RIA.
get_sb_rev_table <- function(
  sb_parent_data
){
  
  sb_gb_data <- sb_parent_data$compressors %>% filter(Gathering.Stations > 0)
  sb_ts_data <- sb_parent_data$compressors %>% filter(TS.Stations > 0)
  sb_processor_data <- sb_parent_data$processors
  sb_wellsite_data <- sb_parent_data$wellsites %>% filter(Has.Completion.2019 > 0)
  
  sb_gb_revenue <- sb_gb_data %>% get_revenue_summary(.segment = "Gathering and Boosting") 
  sb_ts_revenue <- sb_ts_data %>% get_revenue_summary(.segment = "Transmission and Storage")
  sb_processor_revenue <- sb_processor_data %>% get_revenue_summary(.segment = "Processing")
  sb_wellsite_revenue <- sb_wellsite_data %>% get_revenue_summary(.segment = "Production") 
  
  rev_table <- bind_rows(sb_wellsite_revenue , sb_processor_revenue, sb_ts_revenue, sb_gb_revenue)
  
}

# -----------------------------------------------------------------------------------------------------------------#

# ==== Cost Table ==== #

# This section creates the median cost summary table in the RIA. 
get_sb_median_cost_table <- function(
  sb_cost_data
){
  
  sb_gb_cost_data = sb_cost_data$gb_compressors
  sb_ts_cost_data = sb_cost_data$ts_compressors
  sb_proc_cost_data = sb_cost_data$processors
  sb_wellsite_cost_data = sb_cost_data$wellsites

  sb_gb_median_cost_table <- sb_gb_cost_data %>% get_median_cost_summary(.segment = "Gathering and Boosting") 
  sb_ts_median_cost_table <- sb_ts_cost_data %>% get_median_cost_summary(.segment = "Transmission and Storage")
  sb_processor_median_cost_table <- sb_proc_cost_data %>% get_median_cost_summary(.segment = "Processing")
  sb_wellsite_median_cost_table <- sb_wellsite_cost_data %>% get_median_cost_summary(.segment = "Production") 
  
  median_cost_table <- bind_rows(sb_wellsite_median_cost_table , sb_processor_median_cost_table, sb_ts_median_cost_table, sb_gb_median_cost_table)
  
}

# -----------------------------------------------------------------------------------------------------------------#

# ==== CSR Table ==== #

# This function creates the CSR summary table in the RIA. 
get_sb_csr_table <- function(
  sb_cost_data
){
  
  sb_gb_cost_data = sb_cost_data$gb_compressors
  sb_ts_cost_data = sb_cost_data$ts_compressors
  sb_proc_cost_data = sb_cost_data$processors
  sb_wellsite_cost_data = sb_cost_data$wellsites
  
  sb_gb_csr_table <- sb_gb_cost_data %>% get_csr_summary(.segment = "Gathering and Boosting") 
  sb_ts_csr_table <- sb_ts_cost_data %>% get_csr_summary(.segment = "Transmission and Storage")
  sb_processor_csr_table <- sb_proc_cost_data %>% get_csr_summary(.segment = "Processing")
  sb_wellsite_csr_table <- sb_wellsite_cost_data %>% get_csr_summary(.segment = "Production") 
  
  csr_table <- bind_rows(sb_wellsite_csr_table , sb_processor_csr_table, sb_ts_csr_table, sb_gb_csr_table)
  
}



# -----------------------------------------------------------------------------------------------------------------#

# ==== NAICS Table ==== #

# This section creates the NAICS table in the RIA.
get_sb_naics_table <- function(
  sb_parent_data
){
 
  wellsite_parents <- sb_parent_data$wellsites %>% filter(Has.Completion.2019 > 0) %>%
    select(Ultimate.Parent, contains("Naics"), SB.Status.Strict)
  
  processor_parents <- sb_parent_data$processors %>%
    select(Ultimate.Parent, contains("Naics"), SB.Status.Strict)
  
  compressor_parents <- sb_parent_data$compressor %>%
    select(Ultimate.Parent, contains("Naics"), SB.Status.Strict)
  
  parent_data <- bind_rows(wellsite_parents, processor_parents, compressor_parents) %>%
    mutate(Naics.Code = case_when(
      Naics.Code == "211120" ~ Naics.Code,
      Naics.Code == "211130" ~ Naics.Code,
      Naics.Code == "213111" ~ Naics.Code,
      Naics.Code == "213112" ~ Naics.Code,
      Naics.Code == "486210" ~ Naics.Code,
      TRUE ~ "Many"),
      Naics.Desc = case_when(
        Naics.Code == "211120" ~ "Crude Petroleum Extraction",
        Naics.Code == "211130" ~ "Natural Gas Extraction",
        Naics.Code == "213111" ~ "Drilling Oil and Gas Wells",
        Naics.Code == "213112" ~ "Support Activities for Oil and Gas Wells",
        Naics.Code == "486210" ~ "Pipeline Transportation of Natural Gas",
        TRUE ~ "Other")
    ) %>% distinct(.keep_all = TRUE)
  
  naics_table <- parent_data %>%
    group_by(Naics.Code, Naics.Desc) %>%
    summarise(Firm.Count = n(),
              Small.Count = sum(SB.Status.Strict == "Small Business")) %>%
    mutate(Small.Perc = Small.Count / Firm.Count)
   
}
