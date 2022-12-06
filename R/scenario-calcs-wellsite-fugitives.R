#' scenario-calcs-wellsite-fugitives.R
#' These functions calculate cost and emission impacts for fugitive emissions at
#' well sites.

# Extract mp list for wellsite fugitives from activity data
get_mp_list_fugitives <- function(
  ad_proj
) {
  
  ad_proj %>%mutate(Model.Plant = str_replace(Model.Plant, "_Block[0-9]+$", "")) %>%
    select(Model.Plant, Segment) %>% distinct(Model.Plant, .keep_all = TRUE) %>%
    rename(mp = Model.Plant, segment = Segment) %>%
    mutate(gascomp_category = "Prod",
           descr = NA,
           source = "fugitives",
           source_detail = "Fugitive Emissions") %>%
    as_tibble()
  
}

# Extract mp attribute list for wellsite fugitives from activity data
get_mp_attrs_list_fugitives <- function(
  ad_proj
) {
  
  ad_proj %>% mutate(Model.Plant = str_replace(Model.Plant, "_Block[0-9]+$", "")) %>%
    select(Model.Plant, Type, Segment) %>% distinct(Model.Plant, Type, .keep_all = TRUE) %>%
    rename(mp = Model.Plant, attrs = Type, segment = Segment) %>%
    mutate(detail_1 = "Fugitive Emissions",
           detail_2 = "Wellpad Fugitives",
           detail_3 = case_when(
             str_detect(mp, "EquipBin1") ~ "1: No equipment, no tanks",
             str_detect(mp, "EquipBin2") ~ "2: Tanks, no equipment",
             str_detect(mp, "EquipBin3") ~ "3: Equipment = 1, no tanks",
             str_detect(mp, "EquipBin4") ~ "4: Equipment > 1, no tanks",
             str_detect(mp, "EquipBin5") ~ "5: Equipment = 1 and tanks",
             str_detect(mp, "EquipBin6") ~ "6: Equipment > 1 and tanks"
           )) %>% as_tibble()
  
}

# Extract appropriate cost and emission parameters
select_fugitive_parameters <- function(
  fugitive_parameters, 
  repair_rate = "30 day",
  leak_rate = "0.50%"
) {
  
  fugitive_parameters %>%
    filter(fug_leak_rate == leak_rate & fug_repair_rate == repair_rate) %>%
    select(!c("fug_leak_rate", "fug_repair_rate", "fug_mp_emissions")) %>% as_tibble()
  
}

# Take a model-plant/fate projection and perform calculations to add emissions 
# and cost projections
add_scenario_calcs_wellsite_fug <- function(
  mpf_proj, fugitive_parameters,
  ng_price,
  repair_rate = "30 day",
  fug_leak_rate = "0.50%"
) {
  
  # variable lists
  em_vars   <- syms(list("Methane", "VOC", "HAP"))
  gas_vars  <- syms(list("emissions_wholegas", "flare_wholegas", "gas_capture", "gas_revenue", "CH4_CO2e"))
  cost_vars <- syms(list("capital_cost", "annual_cost"))
  
  em_vars_c <- as.character(em_vars)
  gas_vars_c <- as.character(gas_vars)
  cost_vars_c <- as.character(cost_vars)
  
  # get fugitive parameters and merge to model plants, rename variables to 
  # format used 
  fug_param <- select_fugitive_parameters(fugitive_parameters)
  
  result <- mpf_proj %>%
    left_join(fug_param, by = c("fug_mp", "fate")) %>%
    rename(
      mp = Model.Plant, 
      segment = Segment,
      location = State,
      vintage_bin = Reg.Bin,
      year = Year,
      fac_count = Fac.Count,
      new_count = New.Count
    ) %>%
    mutate(vint_trunc = if_else(is.na(as.integer(as.character(Vintage.Bin))),
                                   as.integer(2019), as.integer(as.character(Vintage.Bin))),
           emissions_wholegas = NA,
           flare_wholegas = NA,
           attrs = Type)
  
  # add natural gas price and gas capture
  # adjust emissions by reduction percentage
  result <- result %>% left_join(ng_price, by = "year") %>%
    mutate(gas_capture = Methane*gas_capture_pct*methane_mcf_per_ton/gas_to_methane_prod,
           Methane = Methane*(1 - reduction_pct),
           VOC = VOC*(1 - reduction_pct),
           HAP = HAP*(1 - reduction_pct))
  
  # add gas variables
  # gas capture in Mcf
  result <- result %>% 
    mutate(gas_revenue = gas_capture*ng_price,
           CH4_CO2e = Methane * GWP_CH4 * metric_tons_per_short_ton)
  
  result <-  result %>% mutate(across(.cols = all_of(c(em_vars_c, gas_vars_c, cost_vars_c)),
                           .fns = ~.x * fac_count))
  
  # calculate annualized cost w and w/o gas revenue
  result <- result %>%
    
    # calculate CRFs for capital cost annualization
    mutate(crf_3 = get_crf(.03, pmax(1, control_lifetime)), crf_7 = get_crf(.07, pmax(1, control_lifetime))) %>%
    
    # calculate annualized costs w/ and w/o revenue at 3% and 7%
    mutate(ann_3 = crf_3*capital_cost+annual_cost, ann_7 = crf_7*capital_cost+annual_cost) %>%
    mutate(ann_3_wgas = crf_3*capital_cost+annual_cost-gas_revenue, ann_7_wgas = crf_7*capital_cost+annual_cost-gas_revenue)
  
  result <- result %>%
    mutate(capital_cost = if_else(control_lifetime > 0 & 
                                    ((vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr & year >= nsps.start.yr & 
                                        (((year - nsps.start.yr) %% round(control_lifetime)) == 0)) |
                                       (vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr & year >= vint_trunc & 
                                          (((year - vint_trunc) %% round(control_lifetime) == 0))) |
                                       (vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") & year >= eg.start.yr & 
                                          (((year - eg.start.yr) %% round(control_lifetime) == 0)))), capital_cost, 0)
    ) %>%
    mutate(new_count = if_else(year == vint_trunc & year > base.yr, fac_count, 0))
  
  # standardize columns/order
  result <- result %>% select(
    mp, attrs, segment, location, vintage_bin, year, fac_count, new_count,
    fate, control_lifetime, reduction_pct, emissions_wholegas, 
    Methane, VOC, HAP, flare_wholegas, gas_capture, capital_cost, annual_cost,
    ng_price, CH4_CO2e, gas_revenue, ann_3, ann_7, ann_3_wgas, ann_7_wgas
  )
  
  # Aggregate over blocks
  result <- result %>% mutate(mp = str_replace(mp, "_Block[0-9]+$", "")) %>% 
    group_by(mp, attrs, segment, location, vintage_bin, year, fate, control_lifetime, reduction_pct) %>%
    summarise(across(.cols = all_of(c("fac_count", "new_count", em_vars_c, gas_vars_c, cost_vars_c, "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas")),
                     .fns = ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    left_join(ng_price, by = "year")
  
  result
  
}
