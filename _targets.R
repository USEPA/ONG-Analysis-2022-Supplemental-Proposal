#' _targets.R
#' Configures and defines the targets pipeline.

library(targets)
library(tarchetypes)

# load packages and custom functions
source("scripts/setup.R") 

# choose c("st", "ntnl") to use either state or national activity data
ad_st_or_ntnl <- "st" 

tar_plan(
  
  #### Auxiliary data on activity levels, emissions factors, component counts, well production decline rates, and gas prices --------------------
  
  # GHGI activity and equipment emissions factors
  tar_target(ghgi_gas_xlsx, "data-raw/ghgi_2021_gas_annex_tbl.xlsx", format = "file"),
  tar_target(gas_act_dta, get_ghgi_gas_act(ghgi_gas_xlsx)),
  tar_target(gas_ef_dta, get_ghgi_gas_ef(ghgi_gas_xlsx)),
  tar_target(ghgi_oil_xlsx, "data-raw/ghgi_2021_oil_annex_tbl.xlsx", format = "file"),
  tar_target(oil_act_dta, get_ghgi_oil_act(ghgi_oil_xlsx)),
  tar_target(oil_ef_dta, get_ghgi_oil_ef(ghgi_oil_xlsx)),
  tar_target(ghgi_equip_data, summ_ghgi_equip_data(gas_act_dta, oil_act_dta, gas_ef_dta, oil_ef_dta)),
  
  # Subpart W-based equipment emission factors and Rutherford et al. (2021)-based equipment emission factors
  tar_target(subpartw_xlsx, "data-raw/emissions_factors.xlsx", format = "file"),
  tar_target(ef_equip_subpartw, get_subpartw_equip_ef_proposal(subpartw_xlsx)),
  tar_target(ef_equip_rutherford, get_rutherford_equip_ef(subpartw_xlsx)),
  tar_target(equip_comp_counts_subpartw_current, get_equip_comp_counts_subpartw_current()),
  
  # FEAST model-based component counts
  tar_target(bser_mp_components_csv, "data-raw/bser_mp_components.csv", format = "file"),
  tar_target(bser_mp_components, fread(bser_mp_components_csv)),
  
  # ICR data
  tar_file(icr_surv_xlsx, "data-raw/icr_part_one.xlsx"),
  tar_target(icr_wellsites, comb_icr_well_and_site_data(icr_surv_xlsx)),
  tar_target(icr_equip_prop, get_icr_equip_props(icr_wellsites)),
  tar_target(icr_equip_avg, get_icr_equip_avg(icr_wellsites)),
  
  # Enverus production time series
  # Note: Commented lines in this section of code require proprietary data input; 
  # please contact the U.S. EPA Docket Office for more information.
  # tar_target(wells_prod_csv, "data-raw/wells_prism_2010-2020_2022-05-04.csv", format = "file"),
  # tar_target(wells_prod_ts, load_wells_prod(wells_prod_csv)),
  # tar_target(decl_rates, calc_wells_decl(wells_prod_ts)),
  # tar_target(decl_rates, {
  #   tar_load(wells_prod_ts)
  #   calc_wells_decl(wells_prod_ts)
  #   }),
  
  # EIA API data
  tar_target(ng_price_rds, "data-raw/ng_price.rds", format = "file"),
  tar_target(ng_price, readRDS(ng_price_rds)),
  tar_target(aeo_well_proj_rds, "data-raw/aeo_well_proj.rds", format = "file"),
  tar_target(aeo_well_proj, readRDS(aeo_well_proj_rds)),
  
  #### Site projections --------------------
  
  # Well sites 
  # Note:Ccommented lines in this section require proprietary data; please contact the
  # U.S. EPA Docket Office for more information.
  # tar_target(wells19_csv, "data-raw/wells19.csv", format = "file"),
  # tar_target(wellsite_clean_dta, clean_enverus_data(wells19_csv)),
  # tar_target(wellsite_bins_proj_noequip, wellsite_binning_and_projection(wellsite_clean_dta, aeo_well_proj, decl_rates)),
  tar_target(wellsite_bins_proj_equip, {
    tar_load(wellsite_bins_proj_noequip)
    wellsite_equip_chars_proj(wellsite_bins_proj_noequip, icr_equip_prop, icr_equip_avg, ghgi_equip_data)
    }),
  tar_target(wellsite_bins_proj_pneu, wellsite_pneumatics_proj(wellsite_bins_proj_equip, ghgi_equip_data)),
  tar_target(wellsite_bins_proj_tanks, wellsite_tank_characteristics(wellsite_bins_proj_pneu)),
  tar_target(wellsite_bins_proj, {
    wellsite_bins_proj = wellsite_bins_proj_tanks[order(State, Type, Init.ProdRate.Bin, ProdRate.Bin, Vintage.Bin, Site.Type.Bin, equip_bin, Has_Pumps, Year)
    ][, Block.ID := .GRP, by = .(State, Type, Init.ProdRate.Bin, Vintage.Bin, Site.Type.Bin, equip_bin, Has_Pumps)
    ]
    }),
  
  # Calculate "no-control" emissions at well sites
  tar_target(wellsite_bins_proj_emis, wellsites_no_control_emissions(wellsite_bins_proj, ghgi_equip_data, ef_equip_subpartw, ef_equip_rutherford, 
                                                                      equip_comp_counts_subpartw_current, bser_component_ef)),
  
  # Natural gas processing plants
  tar_file(proc_plant_csv, "data-raw/natural_gas_processing_plants_hifld_2022-07-03.csv"),
  tar_target(proc_st_proj, get_proc_plant_act_proj(proc_plant_csv, gas_act_dta, oil_act_dta)),
  
  # Compressor stations
  tar_file(compr_station_csv, "data-raw/compressor_stations.csv"),
  tar_target(compr_stn_proj, get_compr_station_act_base(compr_station_csv)),
  tar_target(gb_stn_st_proj, get_gb_station_act_proj(compr_stn_proj, gas_act_dta, oil_act_dta)),
  tar_target(trans_stn_st_proj, get_trans_station_act_proj(compr_stn_proj, gas_act_dta, oil_act_dta)),
  tar_target(stor_stn_st_proj, get_stor_station_act_proj(compr_stn_proj, gas_act_dta, oil_act_dta)),
  
  #### Activity data projections --------------------
  
  ## Non-well site + liquids unloading
  # calculate activity levels for affected facilities
  tar_target(ad_proj_list_nonwellsite, get_activity_projections_nonwellsite(
    gas_act_dta,
    oil_act_dta,
    
    gb_stn_st_proj,
    proc_st_proj,
    trans_stn_st_proj,
    stor_stn_st_proj,
    
    wellsite_bins_proj
  )),
  
  tar_target(ad_ntnl_proj_nonwellsite, {
    ad_ntnl_proj_nonwellsite <- 
      ad_proj_list_nonwellsite$"National" %>% transform_ad_proj(., level = "National")}), 
  tar_target(ad_st_proj_nonwellsite, {
    ad_st_proj_nonwellsite <- 
      ad_proj_list_nonwellsite$"State" %>% transform_ad_proj(., level = "State")}), 
  
  tar_target(ad_ntnl_proj_nonwellsite_pneu, {
    ad_ntnl_proj_nonwellsite_pneu <- 
      ad_proj_list_nonwellsite$"National Pneumatics" %>% transform_ad_proj_pneu(., level = "National")}), 
  tar_target(ad_st_proj_nonwellsite_pneu, {
    ad_st_proj_nonwellsite_pneu <- 
      ad_proj_list_nonwellsite$"State Pneumatics" %>% transform_ad_proj_pneu(., level = "State")}), 
  
  # choose state or national
  ad_proj_nonwellsite = if(ad_st_or_ntnl == "st") ad_st_proj_nonwellsite else ad_ntnl_proj_nonwellsite,
  ad_proj_nonwellsite_pneu = if(ad_st_or_ntnl == "st") ad_st_proj_nonwellsite_pneu else ad_ntnl_proj_nonwellsite_pneu,
  
  ## Well sites
  # fugitives
  tar_target(ad_proj_wellsite_fug, get_wellsite_fug_ad(wellsite_bins_proj, wellsite_bins_proj_emis)),
  
  # storage vessels
  tar_target(ad_proj_wellsite_tanks, get_wellsite_tanks_ad(wellsite_bins_proj, wellsite_bins_proj_emis)),
  
  # pneumatic devices
  tar_target(ad_proj_wellsite_pneu, get_wellsite_pneu_ad(wellsite_bins_proj, wellsite_bins_proj_emis)),
  
  
  #### Control measure cost and emissions --------------------
  
  ## Well site fugitives
  # identify data
  tar_target(fugitive_parameters_csv, "data-raw/fugitive_control_parameter_inputs.csv", format = "file"),
  
  # isolate annual O&M costs
  tar_target(fugitive_parameters, fread(fugitive_parameters_csv)[, `:=`(annual_cost = annual_cost - 0.16747*capital_cost)]),
  
  # component-level fugitive emissions factors
  tar_target(bser_component_ef, get_bser_component_ef_all(fugitive_parameters, bser_mp_components)),
  
  ## Well site storage vessels
  # collect parameters
  tar_target(tank_parameters, get_tank_parameters()),
  
  ## Pneumatic devices
  # collect parameters
  tar_target(pneumatic_parameters, get_pneumatic_parameters(ghgi_equip_data, ef_equip_subpartw)),
  
  ## Non-well site (except pneumatics) and liquids unloading
  # identify data
  tar_target(cost_and_emissions_base_xlsx, "data-raw/cost_and_emissions_inputs.xlsx", format = "file"),
  
  # format data
  tar_target(mp_fates_chars_all_nonwellsite, read_cost_and_emissions_xlsx_nonwellsite(
    cost_and_emissions_base_xlsx = cost_and_emissions_base_xlsx) %>%
      filter(!is.na(mp), !is.na(segment), !is.na(fate))),
  
  #### Apply model plant fates --------------------
  # policy scenario functions define how model plants AD projections are mapped to emissions controls under a particular policy scenario based on a 
  # combination of federal and state regulations. They result in mp-fate projections, which are similar to the AD projections, but with the addition 
  # of information on emissions controls in the `fate` column.
  
  ## Well sites
  # fugitives
  tar_target(mpf_proj_wellsite_fug, {
    mpf_proj_wellsite_fug <- ad_proj_wellsite_fug %>% {
      list(
        "baseline_2022" = mutate(., fate = baseline_2022_w_state_policy_fugitives(.)) %>% as_tibble(),
        "proposal_2022_prop"  = mutate(., fate = proposal_2022_prop_phasein_fugitives(.)) %>% as_tibble(),
        "proposal_2022_less"  = mutate(., fate = proposal_2022_less_phasein_fugitives(.)) %>% as_tibble(),
        "proposal_2022_more"  = mutate(., fate = proposal_2022_more_phasein_fugitives(.)) %>% as_tibble()
      )
    }
  }),
  
  # storage vessels
  tar_target(mpf_proj_wellsite_tanks, {
    mpf_proj_wellsite_tanks <- ad_proj_wellsite_tanks %>% {
      list(
        "baseline_2022" = mutate(., fate = baseline_2022_w_state_policy_tanks(.)) %>% as_tibble(),
        "proposal_2022_prop"  = mutate(., fate = proposal_2022_prop_phasein_tanks(.)) %>% as_tibble(),
        "proposal_2022_less"  = mutate(., fate = proposal_2022_less_phasein_tanks(.)) %>% as_tibble(),
        "proposal_2022_more"  = mutate(., fate = proposal_2022_more_phasein_tanks(.)) %>% as_tibble()
      )
    }
  }),
  
  # pneumatic devices
  tar_target(mpf_proj_wellsite_pneu, {
    mpf_proj_wellsite_pneu <- ad_proj_wellsite_pneu %>% {
      list(
        "baseline_2022" = mutate(., fate = baseline_2022_w_state_policy_pneumatic(.)) %>% as_tibble(),
        "proposal_2022_prop"  = mutate(., fate = proposal_2022_prop_phasein_pneumatic(.)) %>% as_tibble(),
        "proposal_2022_less"  = mutate(., fate = proposal_2022_less_phasein_pneumatic(.)) %>% as_tibble(),
        "proposal_2022_more"  = mutate(., fate = proposal_2022_more_phasein_pneumatic(.)) %>% as_tibble()
      )
    }
  }),
  
  ## Non-well site + liquids unloading
  # all but pneumatic devices
  tar_target(mpf_proj_nonwellsite, {
    mpf_proj_nonwellsite <- ad_proj_nonwellsite %>% {
      list(
        "baseline_2022"       = mutate(., fate = baseline_2022_w_state_policy_nonwellsite(.)),
        "proposal_2022_prop"  = mutate(., fate = proposal_2022_prop_phasein_nonwellsite(.)),
        "proposal_2022_less"  = mutate(., fate = proposal_2022_less_phasein_nonwellsite(.)),
        "proposal_2022_more"  = mutate(., fate = proposal_2022_more_phasein_nonwellsite(.))
      )
    }}),
  
  # pneumatic devices
  tar_target(mpf_proj_nonwellsite_pneu, {
    mpf_proj_nonwellsite_pneu <- ad_proj_nonwellsite_pneu %>% {
      list(
        "baseline_2022"       = mutate(., fate = baseline_2022_w_state_policy_nonwellsite(.)),
        "proposal_2022_prop"  = mutate(., fate = proposal_2022_prop_phasein_nonwellsite(.)),
        "proposal_2022_less"  = mutate(., fate = proposal_2022_less_phasein_nonwellsite(.)),
        "proposal_2022_more"  = mutate(., fate = proposal_2022_more_phasein_nonwellsite(.))
      )
    }}),
  
  #### Scenario costs and emissions -----------------
  # Calculate using the unit-level cost and emissions information for each model plant and control fate combination
  
  ## Well sites
  # fugitives
  tar_target(scn_proj_wellsite_fug, {
    scn_proj_wellsite_fug <- {
      list(
        "baseline_2022"      = add_scenario_calcs_wellsite_fug(mpf_proj_wellsite_fug[["baseline_2022"]], fugitive_parameters, ng_price),
        "proposal_2022_prop" = add_scenario_calcs_wellsite_fug(mpf_proj_wellsite_fug[["proposal_2022_prop"]], fugitive_parameters, ng_price),
        "proposal_2022_less" = add_scenario_calcs_wellsite_fug(mpf_proj_wellsite_fug[["proposal_2022_less"]], fugitive_parameters, ng_price),
        "proposal_2022_more" = add_scenario_calcs_wellsite_fug(mpf_proj_wellsite_fug[["proposal_2022_more"]], fugitive_parameters, ng_price)
      )
    }
  }),
  
  # storage vessels
  tar_target(scn_proj_wellsite_tanks, {
    scn_proj_wellsite_tanks <- {
      list(
        "baseline_2022"      = add_scenario_calcs_wellsite_tanks(mpf_proj_wellsite_tanks[["baseline_2022"]], tank_parameters, ng_price),
        "proposal_2022_prop" = add_scenario_calcs_wellsite_tanks(mpf_proj_wellsite_tanks[["proposal_2022_prop"]], tank_parameters, ng_price),
        "proposal_2022_less" = add_scenario_calcs_wellsite_tanks(mpf_proj_wellsite_tanks[["proposal_2022_less"]], tank_parameters, ng_price),
        "proposal_2022_more" = add_scenario_calcs_wellsite_tanks(mpf_proj_wellsite_tanks[["proposal_2022_more"]], tank_parameters, ng_price)
      )
    }
  }),
  
  # pneumatic devices
  tar_target(scn_proj_wellsite_pneu, {
    scn_proj_wellsite_pneu <- {
      list(
        "baseline_2022"      = add_scenario_calcs_wellsite_pneu(mpf_proj_wellsite_pneu[["baseline_2022"]], pneumatic_parameters, ng_price),
        "proposal_2022_prop" = add_scenario_calcs_wellsite_pneu(mpf_proj_wellsite_pneu[["proposal_2022_prop"]], pneumatic_parameters, ng_price),
        "proposal_2022_less" = add_scenario_calcs_wellsite_pneu(mpf_proj_wellsite_pneu[["proposal_2022_less"]], pneumatic_parameters, ng_price),
        "proposal_2022_more" = add_scenario_calcs_wellsite_pneu(mpf_proj_wellsite_pneu[["proposal_2022_more"]], pneumatic_parameters, ng_price)
      )
    }
  }),
  
  ## Non-well sites + liquids unloading
  # all but pneumatic devices
  tar_target(scn_proj_nonwellsite, {
    scn_proj_nonwellsite <- mp_fates_chars_all_nonwellsite %>% {
      list(
        "baseline_2022"       = add_scenario_calcs_nonwellsite(mpf_proj_nonwellsite[["baseline_2022"]], ., ng_price),
        "proposal_2022_prop"  = add_scenario_calcs_nonwellsite(mpf_proj_nonwellsite[["proposal_2022_prop"]], ., ng_price),
        "proposal_2022_less"  = add_scenario_calcs_nonwellsite(mpf_proj_nonwellsite[["proposal_2022_less"]], ., ng_price),
        "proposal_2022_more"  = add_scenario_calcs_nonwellsite(mpf_proj_nonwellsite[["proposal_2022_more"]], ., ng_price)
      )
    }
  }),
  
  # pneumatic devices
  tar_target(scn_proj_nonwellsite_pneu, {
    scn_proj_nonwellsite_pneu <- {
      list(
        "baseline_2022"      = add_scenario_calcs_nonwellsite_pneu(mpf_proj_nonwellsite_pneu[["baseline_2022"]], pneumatic_parameters, ng_price, ghgi_equip_data),
        "proposal_2022_prop" = add_scenario_calcs_nonwellsite_pneu(mpf_proj_nonwellsite_pneu[["proposal_2022_prop"]], pneumatic_parameters, ng_price, ghgi_equip_data),
        "proposal_2022_less" = add_scenario_calcs_nonwellsite_pneu(mpf_proj_nonwellsite_pneu[["proposal_2022_less"]], pneumatic_parameters, ng_price, ghgi_equip_data),
        "proposal_2022_more" = add_scenario_calcs_nonwellsite_pneu(mpf_proj_nonwellsite_pneu[["proposal_2022_more"]], pneumatic_parameters, ng_price, ghgi_equip_data)
      )
    }
  }),
  
  ## Combine scenario calculations
  tar_target(scn_proj_supplemental, {
    scn_proj_supplemental <- {
      list(
        "baseline_2022"      = rbind(scn_proj_wellsite_fug[["baseline_2022"]], 
                                     scn_proj_wellsite_tanks[["baseline_2022"]],
                                     scn_proj_wellsite_pneu[["baseline_2022"]], 
                                     scn_proj_nonwellsite[["baseline_2022"]],
                                     scn_proj_nonwellsite_pneu[["baseline_2022"]]),
        "proposal_2022_prop" = rbind(scn_proj_wellsite_fug[["proposal_2022_prop"]], 
                                     scn_proj_wellsite_tanks[["proposal_2022_prop"]],
                                     scn_proj_wellsite_pneu[["proposal_2022_prop"]], 
                                     scn_proj_nonwellsite[["proposal_2022_prop"]], 
                                     scn_proj_nonwellsite_pneu[["proposal_2022_prop"]]),
        "proposal_2022_less" = rbind(scn_proj_wellsite_fug[["proposal_2022_less"]], 
                                     scn_proj_wellsite_tanks[["proposal_2022_less"]],
                                     scn_proj_wellsite_pneu[["proposal_2022_less"]], 
                                     scn_proj_nonwellsite[["proposal_2022_less"]], 
                                     scn_proj_nonwellsite_pneu[["proposal_2022_less"]]),
        "proposal_2022_more" = rbind(scn_proj_wellsite_fug[["proposal_2022_more"]], 
                                     scn_proj_wellsite_tanks[["proposal_2022_more"]],
                                     scn_proj_wellsite_pneu[["proposal_2022_more"]], 
                                     scn_proj_nonwellsite[["proposal_2022_more"]], 
                                     scn_proj_nonwellsite_pneu[["proposal_2022_more"]])
      )
    }
  }),
  
  #### Compare scenarios -----------------
  
  tar_target(scn_comps_supplemental, {
    scn_comps_supplemental <- list(
      "baseline-prop" = compare_two_scenarios(scn_proj_supplemental[["baseline_2022"]], scn_proj_supplemental[["proposal_2022_prop"]]),
      "baseline-less" = compare_two_scenarios(scn_proj_supplemental[["baseline_2022"]], scn_proj_supplemental[["proposal_2022_less"]]),
      "baseline-more" = compare_two_scenarios(scn_proj_supplemental[["baseline_2022"]], scn_proj_supplemental[["proposal_2022_more"]])
    )
  }),
  
  #### Model plant attribute lists for output reporting --------------------
  
  ## Well sites
  # fugitives
  tar_target(wellsite_fug_mp_list, get_mp_list_fugitives(ad_proj_wellsite_fug)),
  tar_target(wellsite_fug_mp_attrs_list, get_mp_attrs_list_fugitives(ad_proj_wellsite_fug)),
  
  # storage vessels
  tar_target(wellsite_tanks_mp_list, get_mp_list_tanks(ad_proj_wellsite_tanks)),
  tar_target(wellsite_tanks_mp_attrs_list, get_mp_attrs_list_tanks(ad_proj_wellsite_tanks)),
  
  # pneumatic devices
  tar_target(wellsite_pneu_mp_list, get_mp_list_pneumatic(ad_proj_wellsite_pneu)),
  tar_target(wellsite_pneu_mp_attrs_list, get_mp_attrs_list_pneumatic(ad_proj_wellsite_pneu)),
  
  ## Non-well sites + liquids unloading
  # identify data
  tar_target(parameters_xlsx, "data-raw/parameters.xlsx", format = "file"),
  
  # format data
  tar_target(nonwellsite_mp_list, {
    nonwellsite_mp_list <- read_excel(parameters_xlsx, sheet = "mp_list")
  }),
  tar_target(nonwellsite_mp_attrs_list, {
    nonwellsite_mp_attrs_list <- read_excel(parameters_xlsx, sheet = "mp_attrs_list")
  }),
  
  # Supplemental
  mp_list_supp = rbind(nonwellsite_mp_list, wellsite_fug_mp_list, wellsite_tanks_mp_list, wellsite_pneu_mp_list),
  mp_attrs_list_supp = rbind(nonwellsite_mp_attrs_list, wellsite_fug_mp_attrs_list, wellsite_tanks_mp_attrs_list, wellsite_pneu_mp_attrs_list),
  
  #### Output reporting -----------------
  
  ## Gas revenue percentages by segments
  tar_target(gas_revenue_pct, {
    gas_revenue_pct <- scn_comps_supplemental$`baseline-prop` %>%
      select(segment, gas_revenue) %>%
      mutate(total_gas_revenue = sum(gas_revenue)) %>%
      group_by(segment) %>%
      summarise(sum(gas_revenue), mean(total_gas_revenue)) %>%
      mutate(gas_revnue_pct = `sum(gas_revenue)`/`mean(total_gas_revenue)`)
  }),
  
  # SC-CH4 data
  tar_target(sc_ch4_xlsx, "data-raw/tsd-sc-ghg.xlsx", format = "file"),
  tar_target(sc_ch4,
             read_excel(sc_ch4_xlsx, sheet = "2019$_per_short_ton") %>%
               select(year, contains("CH4")) %>%
               filter(year %in% 2023:2035) %>%
               mutate(year = as.character(year))),
  
  ## Aggregate annual costs by gas/oil; these figures form the input for the market analysis in chapter 4
  # format data
  tar_target(og_costs, {
    og_costs <- scn_comps_supplemental$`baseline-prop` %>%
      mutate(Product = case_when(
        segment != "PROD" ~ 'Gas',
        str_detect(str_to_lower(attrs), 'gas') | str_detect(str_to_lower(mp), 'gas') ~ 'Gas',
        str_detect(str_to_lower(mp), 'liq') ~ 'Gas',
        TRUE ~ 'Oil'
      )) %>% group_by(year, Product) %>%
      summarise(across(contains('ann_'), .fns = sum)) %>%
      filter(year >= 2023) %>%
      pivot_longer(cols = contains("ann_"), names_to = "cost_type") %>%
      pivot_wider(names_from = year, values_from = value) %>%
      mutate(gas = ifelse(str_detect(cost_type, "wgas"), "with gas recovery", ""),
             year = paste(Product, ifelse(str_detect(cost_type, "3"), "3%", "7%"), gas, sep = " ")) %>%
      select(year, !c(Product, cost_type, gas))
  }),
  
  # export
  tar_target(og_costs_csv, {
    fwrite(og_costs, file = 'Output/csv/og_costs_supplemental.csv', eol = "\n")
    'Output/csv/og_costs_supplemental.csv'}, 
    format = "file"),
  
  ## Change tracking
  # compile summary lists
  tar_target(scn_summ_list_supplemental_prop, {map(
    list(scn_comps_supplemental[["baseline-prop"]]),
    .f = ~ scn_summ_tab(.x, mp_list = mp_list_supp, mp_attrs_list = mp_attrs_list_supp)
  ) %>%
      set_names(c("baseline-prop"))}),
  
  tar_target(scn_summ_list_supplemental_less, {map(
    list(scn_comps_supplemental[["baseline-less"]]),
    .f = ~ scn_summ_tab(.x, mp_list = mp_list_supp, mp_attrs_list = mp_attrs_list_supp)
  ) %>%
      set_names(c("baseline-less"))}),
  
  tar_target(scn_summ_list_supplemental_more, {map(
    list(scn_comps_supplemental[["baseline-more"]]),
    .f = ~ scn_summ_tab(.x, mp_list = mp_list_supp, mp_attrs_list = mp_attrs_list_supp)
  ) %>%
      set_names(c("baseline-more"))}),
  
  tar_target(scn_summ_all_supplemental, {
    # all scenario summary info from above, as a single df w/ a scn_comp column indicating scenario
    scn_summ_all <- c(scn_summ_list_supplemental_prop, scn_summ_list_supplemental_less, scn_summ_list_supplemental_more) %>%
      bind_rows(.id = "scn_comp")
  }),
  
  # combine scenarios
  tar_target(change_tracking_scenario_summaries, {
    csv_tbl_path <- "output/csv/change-tracking"
    
    rnd_scn_summ <- function(data) mutate(data, across(where(is.numeric), ~round(.x, digits = 0)))
    
    write_text_tbl(rnd_scn_summ(scn_summ_list_supplemental_prop[["baseline-prop"]]), path(csv_tbl_path, "scn_summ_supplemental_prop.tbl"))
    write_text_tbl(rnd_scn_summ(scn_summ_list_supplemental_less[["baseline-less"]]), path(csv_tbl_path, "scn_summ_supplemental_less.tbl"))
    write_text_tbl(rnd_scn_summ(scn_summ_list_supplemental_more[["baseline-more"]]), path(csv_tbl_path, "scn_summ_supplemental_more.tbl"))
    
    c("output/csv/change-tracking/scn_summ_supplemental_prop.tbl",
      "output/csv/change-tracking/scn_summ_supplemental_less.tbl",
      "output/csv/change-tracking/scn_summ_supplemental_more.tbl")
    
  }, format = "file"),
  
  ## Export to Excel
  # render markdown file
  tar_render(tables_xlsx_output_2022_supplemental,
             "docs/tables_xlsx_output_2022_supplemental.Rmd",
             output_dir = "output/docs"),
  
  #### Small Business -------------------------
  
  # Note: The commented lines in this section require proprietary data; please contact the
  # U.S. EPA Docket Office for more information.
  # tar_target(sb_data_xlsx, "data-raw/small_business_data.xlsx", format = "file"),
  # tar_target(proc_comp_ids_xlsx, "data-raw/hoovers_processing_plants_and_compressors.xlsx", format = "file"),

  # tar_target(wellsite_sb_data, get_wellsite_sb_data(wellsite_bins_proj)),
  # tar_target(sb_facility_data, get_sb_facility_data(sb_data_xlsx, wells19_csv, proc_comp_ids_xlsx)),
  # tar_target(sb_parent_data, get_sb_parent_data(sb_facility_data, wellsite_bins_proj, gas_act_dta, ng_price)),
  # tar_target(sb_cost_data, get_sb_costs(sb_parent_data, scn_comps_supplemental, scn_proj_nonwellsite,
  #                                       wellsite_bins_proj, mp_list_supp, mp_attrs_list_supp)),
  
  tar_target(sb_rev_table, {
    tar_load(sb_parent_data)
    get_sb_rev_table(sb_parent_data)
  }),
  tar_target(sb_median_cost_table, {
    tar_load(sb_cost_data)
    get_sb_median_cost_table(sb_cost_data)
  }),
  tar_target(sb_csr_table, {
    tar_load(sb_cost_data)
    get_sb_csr_table(sb_cost_data)
  }),
  tar_target(sb_naics_table, {
    tar_load(sb_parent_data)
    get_sb_naics_table(sb_parent_data)
  }),
  
  ## Export to Excel
  # render markdown file
  tar_render(sb_results_supplemental_NSPS,
             "docs/sb_results_supplemental_NSPS.Rmd",
             output_dir = "output/docs"),
  
)
  

