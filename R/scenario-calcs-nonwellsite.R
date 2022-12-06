#' scenario-calcs-nonwellsite.R
#' These functions calculate cost and emission impacts at processing plants and
#' compressor stations.

# Take a model-plant/fate projection and perform calculations to add emissions 
# and cost projections
add_scenario_calcs_nonwellsite <- function(
  mpf_proj, 
  mpf_chars, 
  ng_price = ng_price
) {
  
  # variable lists
  em_vars   <- syms(list("Methane", "VOC", "HAP"))
  gas_vars  <- syms(list("flare_wholegas", "gas_capture"))
  cost_vars <- syms(list("capital_cost", "annual_cost"))
  
  em_vars_c <- as.character(em_vars)
  gas_vars_c <- as.character(gas_vars)
  cost_vars_c <- as.character(cost_vars)
  
  # add additional fields based on mp-fate key
  result <- left_join(mpf_proj, mpf_chars, by = c("mp", "segment", "fate")) %>%
    mutate(control_lifetime = if_else(is.na(control_lifetime), 0, control_lifetime))
  
  result <- left_join(result, ng_price, by = c("year"))
  
  # calculate emissions totals, gas flaring and capture
  result <- result %>%
    mutate(across(.cols = all_of(c(em_vars_c, gas_vars_c, cost_vars_c)),
                  .fns = ~.x * fac_count)) %>%
    
    # calculate CH4_CO2e
    mutate(CH4_CO2e = Methane * GWP_CH4 * metric_tons_per_short_ton) %>%
    
    # calculate annualized costs w/o revenue
    mutate(gas_revenue = gas_capture * ng_price)
  
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
  
  result <- result %>% select(
    mp, attrs, segment, location, vintage_bin, year, fac_count, new_count,
    fate, control_lifetime, reduction_pct, emissions_wholegas, 
    Methane, VOC, HAP, flare_wholegas, gas_capture, capital_cost, annual_cost,
    ng_price, CH4_CO2e, gas_revenue, ann_3, ann_7, ann_3_wgas, ann_7_wgas
  )
  
  # Aggregate over truncated vintages
  result <- result %>%
    group_by(mp, attrs, segment, location, vintage_bin, year, fate, control_lifetime, reduction_pct) %>%
    summarise(across(.cols = all_of(c("fac_count", "new_count", 
                                      em_vars_c, "emissions_wholegas", 
                                      gas_vars_c, "gas_revenue", "CH4_CO2e", 
                                      cost_vars_c, "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas")),
                     .fns = ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    left_join(ng_price, by = "year")
  
  result
}

# Take a model-plant/fate projection for pneumatic devices and perform 
# calculations to add emissions and cost projections
add_scenario_calcs_nonwellsite_pneu <- function(
  mpf_proj, 
  pneumatic_parameters,
  ng_price, 
  ghgi_equip_data
) {
  
  # equipment life and crf
  # controllers and pumps
  dev_life <- 15
  pv_life <- 10
  bat_life <- 4
  panel_life <- 15
  compr_life <- 6
  oth_life <- 15
  
  dev_crf_3 <- get_crf(r = .03, life = dev_life)
  dev_crf_7 <- get_crf(r = .07, life = dev_life)
  
  pv_crf_3 <- get_crf(r = .03, life = pv_life)
  pv_crf_7 <- get_crf(r = .07, life = pv_life)
  
  bat_crf_3 <- get_crf(r = .03, life = bat_life)
  bat_crf_7 <- get_crf(r = .07, life = bat_life)
  
  panel_crf_3 <- get_crf(r = .03, life = panel_life)
  panel_crf_7 <- get_crf(r = .07, life = panel_life)
  
  compr_crf_3 <- get_crf(r = .03, life = compr_life)
  compr_crf_7 <- get_crf(r = .07, life = compr_life)
  
  oth_crf_3 <- get_crf(r = .03, life = oth_life)
  oth_crf_7 <- get_crf(r = .07, life = oth_life)
  
  # variable lists
  em_vars   <- syms(list("Methane", "VOC", "HAP"))
  gas_vars  <- syms(list("emissions_wholegas", "flare_wholegas", "gas_capture", "gas_revenue", "CH4_CO2e"))
  cost_vars <- syms(list("capital_cost", "annual_cost", "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas"))
  
  em_vars_c <- as.character(em_vars)
  gas_vars_c <- as.character(gas_vars)
  cost_vars_c <- as.character(cost_vars)
  
  # insert zeros where necessary
  result = setDT(mpf_proj)[, `:=` (pump_count_stn = fifelse(is.na(pump_count_stn), 0, pump_count_stn),
                                   emissions_wholegas = NA_real_,
                                   flare_wholegas = NA_real_)]
  
  # get fugitive parameters and merge to model plants
  result = merge.data.table(result, pneumatic_parameters, by = c("fate", "segment"))
  
  # add natural gas price
  result = merge.data.table(result, ng_price, by = "year", all.x = T)
  
  # calculate emissions and gas capture
  result[, `:=` 
         (
           Methane_pnc = fcase(segment %in% c("G&B"), 
                               (1 - pnc_er_pct_per_lb_GHGI)*ghgi_equip_data[Segment == "G&B" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                 (1 - pnc_er_pct_per_ib_GHGI)*ghgi_equip_data[Segment == "G&B" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                 (1 - pnc_er_pct_per_hb_GHGI)*ghgi_equip_data[Segment == "G&B" & Source == "HB Controllers", CH4_TPY]*hb_count_stn,
                               segment %in% c("TRANS"), 
                               (1 - pnc_er_pct_per_lb_GHGI)*ghgi_equip_data[Segment == "TRANS" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                 (1 - pnc_er_pct_per_ib_GHGI)*ghgi_equip_data[Segment == "TRANS" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                 (1 - pnc_er_pct_per_hb_GHGI)*ghgi_equip_data[Segment == "TRANS" & Source == "HB Controllers", CH4_TPY]*hb_count_stn,
                               segment %in% c("STOR"), 
                               (1 - pnc_er_pct_per_lb_GHGI)*ghgi_equip_data[Segment == "STOR" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                 (1 - pnc_er_pct_per_ib_GHGI)*ghgi_equip_data[Segment == "STOR" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                 (1 - pnc_er_pct_per_hb_GHGI)*ghgi_equip_data[Segment == "STOR" & Source == "HB Controllers", CH4_TPY]*hb_count_stn),
           Methane_pump = fcase(segment %in% c("G&B"),
                                (1 - pump_er_pct_GHGI)*ghgi_equip_data[Segment == "G&B" & Source == "Pumps", CH4_TPY]*pump_count_stn,
                                segment %in% c("TRANS"), 0,
                                segment %in% c("STOR"), 0)
         )
  ][, `:=` 
    (
      VOC_pnc = fcase(segment %in% c("G&B"), Methane_pnc*methane_to_voc_prod, 
                      segment %in% c("TRANS", "STOR"), Methane_pnc*methane_to_voc_trans),
      VOC_pump = fcase(segment %in% c("G&B"), Methane_pump*methane_to_voc_prod, 
                       segment %in% c("TRANS", "STOR"), Methane_pump*methane_to_voc_trans)
    )
  ][, `:=` 
    (
      HAP_pnc = fcase(segment %in% c("G&B"), VOC_pnc*voc_to_hap_prod, 
                      segment %in% c("TRANS", "STOR"), VOC_pnc*voc_to_hap_trans),
      HAP_pump = fcase(segment %in% c("G&B"), VOC_pump*voc_to_hap_prod, 
                       segment %in% c("TRANS", "STOR"), VOC_pump*voc_to_hap_trans)
    )
  ][, `:=` 
    (
      gas_capture_pnc = fcase(segment %in% c("G&B"), 
                              pneu_gas_cap_ind*methane_mcf_per_ton/gas_to_methane_prod*
                                (pnc_er_pct_per_lb_GHGI*ghgi_equip_data[Segment == "G&B" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                   pnc_er_pct_per_ib_GHGI*ghgi_equip_data[Segment == "G&B" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                   pnc_er_pct_per_hb_GHGI*ghgi_equip_data[Segment == "G&B" & Source == "HB Controllers", CH4_TPY]*hb_count_stn),
                              segment %in% c("TRANS"), 
                              pneu_gas_cap_ind*methane_mcf_per_ton/gas_to_methane_trans*
                                (pnc_er_pct_per_lb_GHGI*ghgi_equip_data[Segment == "TRANS" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                   pnc_er_pct_per_ib_GHGI*ghgi_equip_data[Segment == "TRANS" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                   pnc_er_pct_per_hb_GHGI*ghgi_equip_data[Segment == "TRANS" & Source == "HB Controllers", CH4_TPY]*hb_count_stn),
                              segment %in% c("STOR"), 
                              pneu_gas_cap_ind*methane_mcf_per_ton/gas_to_methane_trans*
                                (pnc_er_pct_per_lb_GHGI*ghgi_equip_data[Segment == "STOR" & Source == "LB Controllers", CH4_TPY]*lb_count_stn + 
                                   pnc_er_pct_per_ib_GHGI*ghgi_equip_data[Segment == "STOR" & Source == "IB Controllers", CH4_TPY]*ib_count_stn + 
                                   pnc_er_pct_per_hb_GHGI*ghgi_equip_data[Segment == "STOR" & Source == "HB Controllers", CH4_TPY]*hb_count_stn)),
      gas_capture_pump = fcase(segment %in% c("G&B"), 
                               pneu_gas_cap_ind*methane_mcf_per_ton/gas_to_methane_prod*
                                 pump_er_pct_GHGI*ghgi_equip_data[Segment == "G&B" & Source == "Pumps", CH4_TPY]*pump_count_stn,
                               segment %in% c("TRANS"), 0,
                               segment %in% c("STOR"), 0)
    )
  ]
  
  # add gas variables
  # gas capture in Mcf
  result[, `:=` (gas_revenue_pnc = gas_capture_pnc*fifelse(is.na(ng_price), 0, ng_price),
                 gas_revenue_pump = gas_capture_pump*fifelse(is.na(ng_price), 0, ng_price),
                 CH4_CO2e_pnc = Methane_pnc * GWP_CH4 * metric_tons_per_short_ton,
                 CH4_CO2e_pump = Methane_pump * GWP_CH4 * metric_tons_per_short_ton)
  ]
  
  # calculate total number of devices
  result[, pnc_count_stn := lb_count_stn + ib_count_stn + hb_count_stn]
  result[, dev_count_stn := lb_count_stn + ib_count_stn + hb_count_stn + pump_count_stn]
  
  # add annual cost
  result[, `:=` 
         (annual_cost_compr_base = pneu_ann_cost_compr_base,
           annual_cost_compr_pnc =  pneu_ann_cost_compr_per_lb_quad*lb_count_stn^2 + 
             pneu_ann_cost_compr_per_ib_quad*ib_count_stn^2 + 
             pneu_ann_cost_compr_per_hb_quad*hb_count_stn^2 + 
             pneu_ann_cost_compr_per_lbxib*lb_count_stn*ib_count_stn + 
             pneu_ann_cost_compr_per_lbxhb*lb_count_stn*hb_count_stn + 
             pneu_ann_cost_compr_per_ibxhb*ib_count_stn*hb_count_stn +
             pneu_ann_cost_compr_per_lb_lin*lb_count_stn + 
             pneu_ann_cost_compr_per_ib_lin*ib_count_stn + 
             pneu_ann_cost_compr_per_hb_lin*hb_count_stn,
           annual_cost_dev_pnc = pneu_ann_cost_dev_per_lb*lb_count_stn + 
             pneu_ann_cost_dev_per_ib*ib_count_stn + 
             pneu_ann_cost_dev_per_hb*hb_count_stn,
           annual_cost_dev_pump = pneu_ann_cost_dev_per_pump*pump_count_stn,
           annual_cost_oth = pneu_ann_cost_other)
  ][, 
    `:=` (annual_cost_base = annual_cost_compr_base + annual_cost_oth,
          annual_cost_pnc = annual_cost_compr_pnc + annual_cost_dev_pnc,
          annual_cost_pump = annual_cost_dev_pump
    )
  ]
  
  # add capital cost
  result[, `:=`
         (
           capital_cost_pv_base = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_init_base,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% pv_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_repl_base,
             default = 0
           ),
           capital_cost_pv_pnc = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% pv_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_pv_pump = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_init_per_pump*pump_count_stn,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% pv_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% pv_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_pv_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_bat_base = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_init_base,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% bat_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_repl_base,
             default = 0
           ),
           capital_cost_bat_pnc = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% bat_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_bat_pump = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_init_per_pump*pump_count_stn,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% bat_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% bat_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_bat_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_compr_base = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_compr_init_base,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% compr_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% compr_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% compr_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_compr_repl_base,
             default = 0
           ),
           capital_cost_compr_pnc = fcase(
             # first investment
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_compr_init_per_lb_quad*lb_count_stn^2 + 
               pneu_cap_cost_compr_init_per_ib_quad*ib_count_stn^2 + 
               pneu_cap_cost_compr_init_per_hb_quad*hb_count_stn^2 + 
               pneu_cap_cost_compr_init_per_lbxib*lb_count_stn*ib_count_stn + 
               pneu_cap_cost_compr_init_per_lbxhb*lb_count_stn*hb_count_stn + 
               pneu_cap_cost_compr_init_per_ibxhb*ib_count_stn*hb_count_stn +
               pneu_cap_cost_compr_init_per_lb_lin*lb_count_stn + 
               pneu_cap_cost_compr_init_per_ib_lin*ib_count_stn + 
               pneu_cap_cost_compr_init_per_hb_lin*hb_count_stn,
             # replacement units
             (year > nsps.start.yr & (year - nsps.start.yr) %% compr_life == 0 & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year > vint_trunc & (year - vint_trunc) %% compr_life == 0 & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year > eg.start.yr & (year - eg.start.yr) %% compr_life == 0 & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_compr_repl_per_lb_quad*lb_count_stn^2 + 
               pneu_cap_cost_compr_repl_per_ib_quad*ib_count_stn^2 + 
               pneu_cap_cost_compr_repl_per_hb_quad*hb_count_stn^2 + 
               pneu_cap_cost_compr_repl_per_lbxib*lb_count_stn*ib_count_stn + 
               pneu_cap_cost_compr_repl_per_lbxhb*lb_count_stn*hb_count_stn + 
               pneu_cap_cost_compr_repl_per_ibxhb*ib_count_stn*hb_count_stn +
               pneu_cap_cost_compr_repl_per_lb_lin*lb_count_stn + 
               pneu_cap_cost_compr_repl_per_ib_lin*ib_count_stn + 
               pneu_cap_cost_compr_repl_per_hb_lin*hb_count_stn,
             default = 0
           ),
           capital_cost_dev_pnc = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_dev_per_lb*lb_count_stn+pneu_cap_cost_dev_per_ib*ib_count_stn+pneu_cap_cost_dev_per_hb*hb_count_stn,
             default = 0
           ),
           capital_cost_dev_pump = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             pneu_cap_cost_dev_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_panel_base = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pneu_cap_cost_panel_init_base,
             default = 0
           ),
           capital_cost_oth_pnc = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pneu_cap_cost_oth_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           )
         )
  ]
  
  # add annualized capital cost (3%)
  result[, `:=`
         (
           capital_cost_pv_base_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_init_base,
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_repl_base,
             default = 0
           ),
           capital_cost_pv_pnc_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_pv_pump_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_init_per_pump*pump_count_stn,
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_3*pneu_cap_cost_pv_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_bat_base_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_init_base,
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_repl_base,
             default = 0
           ),
           capital_cost_bat_pnc_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_bat_pump_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_init_per_pump*pump_count_stn,
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_3*pneu_cap_cost_bat_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_compr_base_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_3*pneu_cap_cost_compr_init_base,
             # replacement units
             (year >= (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_3*pneu_cap_cost_compr_repl_base,
             default = 0
           ),
           capital_cost_compr_pnc_ann3 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_3*(pneu_cap_cost_compr_init_per_lb_quad*lb_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_ib_quad*ib_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_hb_quad*hb_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_lbxib*lb_count_stn*ib_count_stn + 
                            pneu_cap_cost_compr_init_per_lbxhb*lb_count_stn*hb_count_stn + 
                            pneu_cap_cost_compr_init_per_ibxhb*ib_count_stn*hb_count_stn +
                            pneu_cap_cost_compr_init_per_lb_lin*lb_count_stn + 
                            pneu_cap_cost_compr_init_per_ib_lin*ib_count_stn + 
                            pneu_cap_cost_compr_init_per_hb_lin*hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_3*(pneu_cap_cost_compr_repl_per_lb_quad*lb_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_ib_quad*ib_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_hb_quad*hb_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_lbxib*lb_count_stn*ib_count_stn + 
                            pneu_cap_cost_compr_repl_per_lbxhb*lb_count_stn*hb_count_stn + 
                            pneu_cap_cost_compr_repl_per_ibxhb*ib_count_stn*hb_count_stn +
                            pneu_cap_cost_compr_repl_per_lb_lin*lb_count_stn + 
                            pneu_cap_cost_compr_repl_per_ib_lin*ib_count_stn + 
                            pneu_cap_cost_compr_repl_per_hb_lin*hb_count_stn),
             default = 0
           ),
           capital_cost_dev_pnc_ann3 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             dev_crf_3*(pneu_cap_cost_dev_per_lb*lb_count_stn+pneu_cap_cost_dev_per_ib*ib_count_stn+pneu_cap_cost_dev_per_hb*hb_count_stn),
             default = 0
           ),
           capital_cost_dev_pump_ann3 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             dev_crf_3*pneu_cap_cost_dev_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_panel_base_ann3 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             panel_crf_3*pneu_cap_cost_panel_init_base,
             default = 0
           ),
           capital_cost_oth_pnc_ann3 = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             oth_crf_3*pneu_cap_cost_oth_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           )
         )
  ]
  
  # add annualized capital cost (7%)
  result[, `:=`
         (
           capital_cost_pv_base_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_init_base,
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_repl_base,
             default = 0
           ),
           capital_cost_pv_pnc_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_pv_pump_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_init_per_pump*pump_count_stn,
             # replacement units
             (year >= (nsps.start.yr + pv_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + pv_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + pv_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             pv_crf_7*pneu_cap_cost_pv_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_bat_base_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_init_base,
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_repl_base,
             default = 0
           ),
           capital_cost_bat_pnc_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_init_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_repl_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           ),
           capital_cost_bat_pump_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_init_per_pump*pump_count_stn,
             # replacement units
             (year >= (nsps.start.yr + bat_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + bat_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + bat_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")),
             bat_crf_7*pneu_cap_cost_bat_repl_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_compr_base_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_7*pneu_cap_cost_compr_init_base,
             # replacement units
             (year >= (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_7*pneu_cap_cost_compr_repl_base,
             default = 0
           ),
           capital_cost_compr_pnc_ann7 = fcase(
             # first investment
             (year >= nsps.start.yr & year < (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & year < (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & year < (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_7*(pneu_cap_cost_compr_init_per_lb_quad*lb_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_ib_quad*ib_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_hb_quad*hb_count_stn^2 + 
                            pneu_cap_cost_compr_init_per_lbxib*lb_count_stn*ib_count_stn + 
                            pneu_cap_cost_compr_init_per_lbxhb*lb_count_stn*hb_count_stn + 
                            pneu_cap_cost_compr_init_per_ibxhb*ib_count_stn*hb_count_stn +
                            pneu_cap_cost_compr_init_per_lb_lin*lb_count_stn + 
                            pneu_cap_cost_compr_init_per_ib_lin*ib_count_stn + 
                            pneu_cap_cost_compr_init_per_hb_lin*hb_count_stn),
             # replacement units
             (year >= (nsps.start.yr + compr_life) & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= (vint_trunc + compr_life) & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= (eg.start.yr + compr_life) & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             compr_crf_7*(pneu_cap_cost_compr_repl_per_lb_quad*lb_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_ib_quad*ib_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_hb_quad*hb_count_stn^2 + 
                            pneu_cap_cost_compr_repl_per_lbxib*lb_count_stn*ib_count_stn + 
                            pneu_cap_cost_compr_repl_per_lbxhb*lb_count_stn*hb_count_stn + 
                            pneu_cap_cost_compr_repl_per_ibxhb*ib_count_stn*hb_count_stn +
                            pneu_cap_cost_compr_repl_per_lb_lin*lb_count_stn + 
                            pneu_cap_cost_compr_repl_per_ib_lin*ib_count_stn + 
                            pneu_cap_cost_compr_repl_per_hb_lin*hb_count_stn),
             default = 0
           ),
           capital_cost_dev_pnc_ann7 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             dev_crf_7*(pneu_cap_cost_dev_per_lb*lb_count_stn+pneu_cap_cost_dev_per_ib*ib_count_stn+pneu_cap_cost_dev_per_hb*hb_count_stn),
             default = 0
           ),
           capital_cost_dev_pump_ann7 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             dev_crf_7*pneu_cap_cost_dev_per_pump*pump_count_stn,
             default = 0
           ),
           capital_cost_panel_base_ann7 = fcase(
             (year >= nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year >= vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year >= eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             panel_crf_7*pneu_cap_cost_panel_init_base,
             default = 0
           ),
           capital_cost_oth_pnc_ann7 = fcase(
             (year == nsps.start.yr & vintage_bin == "OOOOb" & vint_trunc <= nsps.start.yr) | 
               (year == vint_trunc & vintage_bin == "OOOOb" & vint_trunc > nsps.start.yr) |
               (year == eg.start.yr & vintage_bin %in% c("OOOOc", "OOOO", "OOOOa")), 
             oth_crf_7*pneu_cap_cost_oth_per_pnc*(lb_count_stn+ib_count_stn+hb_count_stn),
             default = 0
           )
         )
  ]
  
  result[, `:=` (capital_cost_base = capital_cost_pv_base + capital_cost_bat_base + capital_cost_compr_base + capital_cost_panel_base,
                 capital_cost_pnc = capital_cost_pv_pnc + capital_cost_bat_pnc + capital_cost_compr_pnc + capital_cost_dev_pnc + capital_cost_oth_pnc,
                 capital_cost_pump = capital_cost_pv_pump + capital_cost_bat_pump + capital_cost_dev_pump,
                 capital_cost_base_ann3 = capital_cost_pv_base_ann3 + capital_cost_bat_base_ann3 + capital_cost_compr_base_ann3 + capital_cost_panel_base_ann3,
                 capital_cost_pnc_ann3 = capital_cost_pv_pnc_ann3 + capital_cost_bat_pnc_ann3 + capital_cost_compr_pnc_ann3 + 
                   capital_cost_dev_pnc_ann3 + capital_cost_oth_pnc_ann3,
                 capital_cost_pump_ann3 = capital_cost_pv_pump_ann3 + capital_cost_bat_pump_ann3 + capital_cost_dev_pump_ann3,
                 capital_cost_base_ann7 = capital_cost_pv_base_ann7 + capital_cost_bat_base_ann7 + capital_cost_compr_base_ann7 + capital_cost_panel_base_ann7,
                 capital_cost_pnc_ann7 = capital_cost_pv_pnc_ann7 + capital_cost_bat_pnc_ann7 + capital_cost_compr_pnc_ann7 + 
                   capital_cost_dev_pnc_ann7 + capital_cost_oth_pnc_ann7,
                 capital_cost_pump_ann7 = capital_cost_pv_pump_ann7 + capital_cost_bat_pump_ann7 + capital_cost_dev_pump_ann7)]
  
  # split sites into device-specific model plants
  result = rbindlist(list(result[, .(mp = "pneucontr", attrs, segment, location, vintage_bin, year, fac_count, dev_count = pnc_count_stn, 
                                     new_count = pnc_count_stn*(vint_trunc == year),
                                     fate, control_lifetime = dev_life, reduction_pct = NA_real_, emissions_wholegas,
                                     Methane = Methane_pnc, VOC = VOC_pnc, HAP = HAP_pnc, flare_wholegas, gas_capture = gas_capture_pnc,
                                     capital_cost = capital_cost_pnc + (pnc_count_stn/dev_count_stn)*capital_cost_base,
                                     annual_cost = annual_cost_pnc + (pnc_count_stn/dev_count_stn)*annual_cost_base,
                                     CH4_CO2e = CH4_CO2e_pnc, gas_revenue = gas_revenue_pnc,
                                     ann_3 = annual_cost_pnc + (pnc_count_stn/dev_count_stn)*annual_cost_base + 
                                       capital_cost_pnc_ann3 + (pnc_count_stn/dev_count_stn)*capital_cost_base_ann3,
                                     ann_7 = annual_cost_pnc + (pnc_count_stn/dev_count_stn)*annual_cost_base + 
                                       capital_cost_pnc_ann7 + (pnc_count_stn/dev_count_stn)*capital_cost_base_ann7)], 
                          result[, .(mp = "pumps", attrs, segment, location, vintage_bin, year, fac_count, dev_count = pump_count_stn, 
                                     new_count = pump_count_stn*(vint_trunc == year),
                                     fate, control_lifetime = dev_life, reduction_pct = NA_real_, emissions_wholegas,
                                     Methane = Methane_pump, VOC = VOC_pump, HAP = HAP_pump, flare_wholegas, gas_capture = gas_capture_pump,
                                     capital_cost = capital_cost_pump + (pump_count_stn/dev_count_stn)*capital_cost_base,
                                     annual_cost = annual_cost_pump + (pump_count_stn/dev_count_stn)*annual_cost_base,
                                     CH4_CO2e = CH4_CO2e_pump, gas_revenue = gas_revenue_pump,
                                     ann_3 = annual_cost_pump + (pump_count_stn/dev_count_stn)*annual_cost_base + 
                                       capital_cost_pump_ann3 + (pump_count_stn/dev_count_stn)*capital_cost_base_ann3,
                                     ann_7 = annual_cost_pump + (pump_count_stn/dev_count_stn)*annual_cost_base + 
                                       capital_cost_pump_ann7 + (pump_count_stn/dev_count_stn)*capital_cost_base_ann7)]))
  
  # remove empty entries
  result = result[dev_count > 0]
  
  # aggregate costs
  result[, `:=` (ann_3_wgas = ann_3 - gas_revenue, ann_7_wgas = ann_7 - gas_revenue)]
  
  
  # sum over facilities
  cols = c(em_vars_c, gas_vars_c, cost_vars_c)
  result[, (cols) := lapply(.SD, "*", fac_count), .SDcols = cols]
  
  # update facility counts to reflect devices
  result[, fac_count := dev_count]
  
  # Aggregate over truncated vintages
  cols = c("fac_count", "new_count", cols)
  result = result[, lapply(.SD, sum, na.rm = T), 
                  by = .(mp, attrs, segment, location, vintage_bin, year, fate, control_lifetime, reduction_pct), 
                  .SDcols = cols]
  
  result = merge.data.table(result, ng_price, by = c("year"), all.x = T)
  
  result
  
}
