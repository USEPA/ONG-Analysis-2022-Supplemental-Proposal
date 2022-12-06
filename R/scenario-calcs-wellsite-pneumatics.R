#' scenario-calcs-wellsite-pneumatics.R
#' These functions calculate cost and emission impacts for pneumatic devices at
#' well sites.

# extract mp list for wellsite pneumatics from activity data
get_mp_list_pneumatic <- function(
  ad_proj
) {
  
  ad_proj %>% mutate(Model.Plant = str_replace(Model.Plant, "_Block[0-9]+$", "")) %>%
    select(Model.Plant, Segment) %>% distinct(Model.Plant, .keep_all = TRUE) %>%
    rename(mp = Model.Plant, segment = Segment) %>%
    mutate(gascomp_category = "Prod",
           descr = NA,
           source = ifelse(str_detect(mp, "controller"), "pneucontr", "pumps"),
           source_detail =  ifelse(str_detect(mp, "controller"), "Well Pad Pneumatic Controllers", "Well Pad Pumps")) %>%
    as_tibble()
  
}

# extract mp attribute list for wellsite pneumatics from activity data
get_mp_attrs_list_pneumatic <- function(
  ad_proj
) {
  
  ad_proj %>% mutate(Model.Plant = str_replace(Model.Plant, "_Block[0-9]+$", "")) %>%
    select(Model.Plant, Type, Segment) %>% distinct(Model.Plant, Type, .keep_all = TRUE) %>%
    rename(mp = Model.Plant, attrs = Type, segment = Segment) %>%
    mutate(detail_1 = ifelse(str_detect(mp, "controller"), "pneucontr", "pumps"),
           detail_2 = ifelse(str_detect(mp, "controller"), "Well Pad Pneumatic Controllers", "Well Pad Pumps"),
           detail_3 = NA
           ) %>% as_tibble()
  
}

# Take a model-plant/fate projection and perform calculations to add emissions 
# and cost projections
add_scenario_calcs_wellsite_pneu <- function(
  mpf_proj, 
  pneumatic_parameters,
  ng_price
) {
  
  # Equipment life and crf
  # controllers and pumps
  dev_life <- 15
  pv_life <- 10
  bat_life <- 4
  panel_life <- 15
  compr_life <- 6
  
  ctrl_crf_3 <- get_crf(r = .03, life = dev_life)
  ctrl_crf_7 <- get_crf(r = .07, life = dev_life)
  
  pv_crf_3 <- get_crf(r = .03, life = pv_life)
  pv_crf_7 <- get_crf(r = .07, life = pv_life)
  
  bat_crf_3 <- get_crf(r = .03, life = bat_life)
  bat_crf_7 <- get_crf(r = .07, life = bat_life)
  
  panel_crf_3 <- get_crf(r = .03, life = panel_life)
  panel_crf_7 <- get_crf(r = .07, life = panel_life)
  
  compr_crf_3 <- get_crf(r = .03, life = compr_life)
  compr_crf_7 <- get_crf(r = .07, life = compr_life)
  
  # variable lists
  em_vars   <- syms(list("Methane", "VOC", "HAP"))
  gas_vars  <- syms(list("emissions_wholegas", "flare_wholegas", "gas_capture", "gas_revenue", "CH4_CO2e"))
  cost_vars <- syms(list("capital_cost", "annual_cost", "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas"))
  
  em_vars_c <- as.character(em_vars)
  gas_vars_c <- as.character(gas_vars)
  cost_vars_c <- as.character(cost_vars)
  
  # get fugitive parameters and merge to model plants, rename variables to 
  # format used 
  result <- mpf_proj %>% 
    rename(
      mp = Model.Plant, 
      segment = Segment,
      location = State,
      vintage_bin = Reg.Bin,
      year = Year,
      fac_count = Fac.Count,
      new_count = New.Count
    ) %>%
    left_join(pneumatic_parameters, by = c("fate", "segment")) %>%
    mutate(vintage_trunc = if_else(is.na(as.integer(as.character(Vintage.Bin))),
                                   as.integer(2019), as.integer(as.character(Vintage.Bin))),
           emissions_wholegas = NA,
           flare_wholegas = NA,
           attrs = Type)
  
  #add reduction percentage and gas capture percentage
  result <- result %>%
    mutate(reduction_pct = case_when(
      str_detect(mp, "^lb_controller") ~ pnc_er_pct_per_lb_GHGI,
      str_detect(mp, "^ib_controller") ~ pnc_er_pct_per_ib_GHGI,
      str_detect(mp, "^hb_controller") ~ pnc_er_pct_per_hb_GHGI,
      str_detect(mp, "^pump") ~ pump_er_pct_GHGI
    ),
    gas_capture_pct = pneu_gas_cap_ind*reduction_pct)
  
  # add natural gas price and gas capture
  # adjust emissions by reduction percentage
  result <- result %>% left_join(ng_price, by = "year") %>%
    mutate(gas_capture = Methane*methane_mcf_per_ton/gas_to_methane_prod*gas_capture_pct,
           Methane = Methane*(1 - reduction_pct),
           VOC = VOC*(1 - reduction_pct),
           HAP = HAP*(1 - reduction_pct))
  
  # add gas variables
  # gas capture in Mcf
  result <- result %>% 
    mutate(gas_revenue = gas_capture*ifelse(is.na(ng_price), 0, ng_price),
           CH4_CO2e = Methane * GWP_CH4 * metric_tons_per_short_ton)
  
  # add annual cost
  result <- result %>%
    mutate(annual_cost = case_when(
      str_detect(mp, "pump") ~ pneu_ann_cost_other/Pneu_per_site + pneu_ann_cost_dev_per_pump,
      str_detect(mp, "^lb") ~ pneu_ann_cost_other/Pneu_per_site + pneu_ann_cost_dev_per_lb,
      str_detect(mp, "^ib") ~ pneu_ann_cost_other/Pneu_per_site + pneu_ann_cost_dev_per_ib,
      str_detect(mp, "^hb") ~ pneu_ann_cost_other/Pneu_per_site + pneu_ann_cost_dev_per_hb
    ))
  
  # add capital cost
  result <- result %>%
    mutate(
      pv_capital_cost = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pnc,
        str_detect(mp, "pump") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pump,
        # replacement units
        str_detect(mp, "controller") & 
          ((year > nsps.start.yr & (year - nsps.start.yr) %% pv_life == 0 & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year > vintage_trunc & (year - vintage_trunc) %% pv_life == 0 & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year > eg.start.yr & (year - eg.start.yr) %% pv_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pnc,
        str_detect(mp, "pump") & 
          ((year > nsps.start.yr & (year - nsps.start.yr) %% pv_life == 0 & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year > vintage_trunc & (year - vintage_trunc) %% pv_life == 0 & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year > eg.start.yr & (year - eg.start.yr) %% pv_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pump,
        TRUE ~ 0
      ),
      bat_capital_cost = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pnc,
        str_detect(mp, "pump") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pump,
        # replacement units
        str_detect(mp, "controller") & 
          ((year > nsps.start.yr & (year - nsps.start.yr) %% bat_life == 0 & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year > vintage_trunc & (year - vintage_trunc) %% bat_life == 0 & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year > eg.start.yr & (year - eg.start.yr) %% bat_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pnc,
        str_detect(mp, "pump") & 
          ((year > nsps.start.yr & (year - nsps.start.yr) %% bat_life == 0 & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year > vintage_trunc & (year - vintage_trunc) %% bat_life == 0 & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year > eg.start.yr & (year - eg.start.yr) %% bat_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pump,
        TRUE ~ 0
      ),
      dev_capital_cost = case_when(
        str_detect(mp, "lb_controller") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_dev_per_lb,
        str_detect(mp, "ib_controller") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_dev_per_ib,
        str_detect(mp, "hb_controller") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_dev_per_hb,
        str_detect(mp, "pump") & 
          ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_dev_per_pump,
        TRUE ~ 0
      ),
      panel_capital_cost = case_when(
        ((year == nsps.start.yr & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year == vintage_trunc & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ pneu_cap_cost_panel_init_base/Pneu_per_site,
        TRUE ~ 0
      )
    )
  
  # add annualized capital cost (3%)
  result <- result %>%
    mutate(
      pv_capital_cost_ann3 = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_3*(pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_3*(pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pump),
        # replacement units
        str_detect(mp, "controller") & 
          ((year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_3*(pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_3*(pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pump),
        TRUE ~ 0
      ),
      bat_capital_cost_ann3 = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_3*(pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_3*(pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pump),
        # replacement units
        str_detect(mp, "controller") & 
          ((year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_3*(pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_3*(pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pump),
        TRUE ~ 0
      ),
      dev_capital_cost_ann3 = case_when(
        str_detect(mp, "lb_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_3*pneu_cap_cost_dev_per_lb,
        str_detect(mp, "ib_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_3*pneu_cap_cost_dev_per_ib,
        str_detect(mp, "hb_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_3*pneu_cap_cost_dev_per_hb,
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_3*pneu_cap_cost_dev_per_pump,
        TRUE ~ 0
      ),
      panel_capital_cost_ann3 = case_when(
        ((year >= nsps.start.yr & year < (nsps.start.yr + panel_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + panel_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + panel_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          panel_crf_3*pneu_cap_cost_panel_init_base/Pneu_per_site,
        TRUE ~ 0
      )
    )
  
  # add annualized capital cost (7%)
  result <- result %>%
    mutate(
      pv_capital_cost_ann7 = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_7*(pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_7*(pneu_cap_cost_pv_init_base/Pneu_per_site + pneu_cap_cost_pv_init_per_pump),
        # replacement units
        str_detect(mp, "controller") & 
          ((year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_7*(pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + pv_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          pv_crf_7*(pneu_cap_cost_pv_repl_base/Pneu_per_site + pneu_cap_cost_pv_repl_per_pump),
        TRUE ~ 0
      ),
      bat_capital_cost_ann7 = case_when(
        # first investment
        str_detect(mp, "controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_7*(pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_7*(pneu_cap_cost_bat_init_base/Pneu_per_site + pneu_cap_cost_bat_init_per_pump),
        # replacement units
        str_detect(mp, "controller") & 
          ((year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_7*(pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pnc),
        str_detect(mp, "pump") & 
          ((year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= (vintage_trunc + bat_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          bat_crf_7*(pneu_cap_cost_bat_repl_base/Pneu_per_site + pneu_cap_cost_bat_repl_per_pump),
        TRUE ~ 0
      ),
      dev_capital_cost_ann7 = case_when(
        str_detect(mp, "lb_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_7*pneu_cap_cost_dev_per_lb,
        str_detect(mp, "ib_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_7*pneu_cap_cost_dev_per_ib,
        str_detect(mp, "hb_controller") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_7*pneu_cap_cost_dev_per_hb,
        str_detect(mp, "pump") & 
          ((year >= nsps.start.yr & year < (nsps.start.yr + dev_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
             (year >= vintage_trunc & year < (vintage_trunc + dev_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
             (year >= eg.start.yr & year < (eg.start.yr + dev_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          ctrl_crf_7*pneu_cap_cost_dev_per_pump,
        TRUE ~ 0
      ),
      panel_capital_cost_ann7 = case_when(
        ((year >= nsps.start.yr & year < (nsps.start.yr + panel_life) & vintage_bin == "OOOOb" & vintage_trunc <= nsps.start.yr) | 
           (year >= vintage_trunc & year < (vintage_trunc + panel_life) & vintage_bin == "OOOOb" & vintage_trunc > nsps.start.yr) |
           (year >= eg.start.yr & year < (eg.start.yr + panel_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")))  ~ 
          panel_crf_7*pneu_cap_cost_panel_init_base/Pneu_per_site,
        TRUE ~ 0
      )
    )
  
  # aggregate costs
  result <- result %>%
    mutate(capital_cost = pv_capital_cost + bat_capital_cost + dev_capital_cost + panel_capital_cost,
           ann_3 = annual_cost + dev_capital_cost_ann3 + pv_capital_cost_ann3 +
             bat_capital_cost_ann3 + panel_capital_cost_ann3,
           ann_3_wgas = annual_cost + dev_capital_cost_ann3 + pv_capital_cost_ann3 +
             bat_capital_cost_ann3 + panel_capital_cost_ann3 - gas_revenue,
           ann_7 = annual_cost + dev_capital_cost_ann7 + pv_capital_cost_ann7 +
             bat_capital_cost_ann7 + panel_capital_cost_ann7,
           ann_7_wgas = annual_cost + dev_capital_cost_ann7 + pv_capital_cost_ann7 +
             bat_capital_cost_ann7 + panel_capital_cost_ann7 - gas_revenue
           
    )
  
  # sum over facilities
  result <-  result %>% mutate(across(.cols = all_of(c(em_vars_c, gas_vars_c, cost_vars_c)),
                                      .fns = ~.x * fac_count))
  
  # placeholder control lifetime for output
  result <- result %>% mutate(control_lifetime = dev_life)
  
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
    summarise(across(.cols = all_of(c("fac_count", "new_count", em_vars_c, gas_vars_c, cost_vars_c)),
                     .fns = ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    left_join(ng_price, by = "year")
  
  result
  
}
