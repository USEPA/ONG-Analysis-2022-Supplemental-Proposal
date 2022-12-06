#' mpf-funs-supplemental.R
#' These functions take activity data scenarios (in the form of a data frame)
#' and return a vector of fates.

#### NON-WELLSITE ====
# Excludes wellsites (except liquids unloading)

#### Baseline Scenario Functions ------------------------

# Calculate mp-fate-proj assuming no required control
no_control_baseline_nonwellsite <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(fate = case_when(
      
      mp == "gbstation"           ~ "bau",
      
      str_detect(mp, "^procplant_") ~ "bau",
      
      mp == "transstation"        ~ "bau",
      mp == "storstation"         ~ "bau",
      
      mp == "recip_comp"          ~ "bau",
      
      mp == "wet_cntfgl_comp"         ~ "bau",
      mp == "dry_cntfgl_comp"         ~ "bau",
      
      mp == "pneu_dev"             ~ "bau",
      
      mp %in% c("liq_unl_event_w_lift", "liq_unl_event_no_lift") ~ "bau",
      
      TRUE ~ NA_character_
    ))
  
  result <- scn_fate$fate
}

# main baseline for 2022 supplemental reflects CRA reversal of some 2020 policy review components
baseline_2022_nonwellsite <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(fate = case_when(
      
      # Fugitives and leaks
      mp %in% c("gbstation", "transstation", "storstation") &
        vintage_bin %in% c("OOOOa", "OOOOb") ~ "ogi_quarterly",
      mp %in% c("gbstation", "transstation", "storstation") ~ "bau",
      
      mp %in% c("procplant_small_voc", "procplant_large_voc") & 
        vintage_bin == "OOOOc" ~ "VV",
      mp %in% c("procplant_small_voc", "procplant_large_voc") & 
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "VVa",
      mp %in% c("procplant_small_nonvoc", "procplant_large_nonvoc") ~ "bau",
      
      # Pneumatic devices
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOc") ~ "bau_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOc", "OOOO") ~ "bau_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOO", "OOOOa") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      
      # Compressors
      # reciprocating
      mp == "recip_comp" &
        segment %in% c("G&B", "PROC") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "seals_26khrs",
      mp == "recip_comp" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "seals_26khrs",
      mp == "recip_comp" ~ "bau",
      
      # wet seal centrifugal
      mp == "wet_cntfgl_comp" & segment == "G&B" ~ "bau", 
      mp == "wet_cntfgl_comp" &
        segment %in% c("PROC") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" ~ "bau",
      
      # dry seal centrifugal
      mp == "dry_cntfgl_comp" ~ "bau",
      
      # Liquids Unloading
      mp %in% c("liq_unl_event_w_lift", "liq_unl_event_no_lift") ~ "bau",

      TRUE ~ NA_character_
    ))
  
  #result <- more_stringent(state_fate, scn_fate$fate)
  result <- scn_fate$fate
}

# combine baseline fates with state policy fates
baseline_2022_w_state_policy_nonwellsite <- function(
  ad_proj
) {
  combo_w_state_policy_nonwellsite(ad_proj, baseline_2022_nonwellsite(ad_proj))
}

# set baseline fates for certain facilities in California and Colorado to be the same as the proposal
combo_w_state_policy_nonwellsite <- function(
  ad_proj, 
  otherwise_fate, 
  scenario = "prop"
) {
  
  proposal_fate <- proposal_2022_prop_nonwellsite(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      # fugitives
      location %in% c("CA", "CO") &
        (str_detect(mp, "^wellsite_") |
           str_detect(mp, "^procplant_") |
           mp %in% c("gbstation", "transstation", "storstation")) ~ proposal_fate,
      
      # pneumatic devices
      mp == "pneu_dev" &
        location %in% c("CA", "CO") ~ proposal_fate,
      
      # compressors
      mp %in% c("recip_comp", "wet_cntfgl_comp") & 
        (location == "CA" |
           (location == "CO" &
              vintage_bin == "OOOOc" &
              segment %in% c("PROC", "G&B"))) ~ proposal_fate,
      
      # liquids unloading
      str_detect(mp, "^liq_unl") &
        location %in% c("CO") ~ proposal_fate,
      
      TRUE ~ otherwise_fate
    ))
  
  result <- scn_fate$fate
}

#### Policy Scenario Functions ------------------------------

# PROPOSAL SCENARIO
proposal_2022_prop_nonwellsite <- function(
  ad_proj
) {

  state_fate <- no_control_baseline_nonwellsite(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      # Fugitives
      
      mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_quarterly",
      
      mp %in% c("procplant_small_voc", "procplant_large_voc",
                "procplant_small_nonvoc", "procplant_large_nonvoc") ~ "ogi_bimonthly", 
      
      # Pneumatic devices
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_solar_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_solar_new",
      mp == "pneu_dev" &
        segment %in% c("TRANS") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_elec_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_elec_new",
      mp == "pneu_dev" &
        segment %in% c("STOR") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_air_retro",
      mp == "pneu_dev" &
        segment %in% c("STOR") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_air_new",
      
      # Compressors
      mp == "recip_comp" ~ "seals_annual",
      
      mp == "wet_cntfgl_comp" & segment == "G&B" ~ "3_scfm_limit", 
      mp == "wet_cntfgl_comp" &
        segment %in% c("PROC") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" ~ "3_scfm_limit",
      
      mp == "dry_cntfgl_comp" ~ "3_scfm_limit",
      
      # Liquids Unloading
      mp %in% c("liq_unl_event_w_lift", "liq_unl_event_no_lift") ~ "best_mgmt",
      
      TRUE ~ NA_character_
    ))
  
  result <- scn_fate$fate
}

# assign start years for proposal fates
proposal_2022_prop_phasein_nonwellsite <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_nonwellsite(ad_proj, proposal_2022_prop_nonwellsite(ad_proj))
  baseline_fate <- combo_w_state_policy_nonwellsite(ad_proj, baseline_2022_nonwellsite(ad_proj))
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      vintage_bin == "OOOOb" & year >= nsps.start.yr ~ proposal_fate,
      
      vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") & year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    ))
  
  result <- scn_fate$fate
}

# LESS STRINGENT SCENARIO
proposal_2022_less_nonwellsite <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_nonwellsite(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      # Fugitives
      mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_quarterly",
      
      mp %in% c("procplant_small_voc", "procplant_large_voc",
                "procplant_small_nonvoc", "procplant_large_nonvoc") ~ "ogi_bimonthly", 
      
      # Pneumatic devices
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOc") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOc", "OOOO") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOO", "OOOOa") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa") ~ "high_to_low_bleed_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      
      # Compressors
      mp == "recip_comp" ~ "seals_annual",
      
      mp == "wet_cntfgl_comp" & segment == "G&B" ~ "3_scfm_limit", 
      mp == "wet_cntfgl_comp" &
        segment %in% c("PROC") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" ~ "3_scfm_limit",
      
      mp == "dry_cntfgl_comp" ~ "3_scfm_limit",
      
      # Liquids Unloading
      mp %in% c("liq_unl_event_w_lift", "liq_unl_event_no_lift") ~ "best_mgmt",
      
      TRUE ~ NA_character_
    ))
  
  result <- scn_fate$fate
}

# assign start years for proposal fates
proposal_2022_less_phasein_nonwellsite <- function(
  ad_proj
) {
  
  
  proposal_fate <- combo_w_state_policy_nonwellsite(ad_proj, proposal_2022_less_nonwellsite(ad_proj))
  baseline_fate <- combo_w_state_policy_nonwellsite(ad_proj, baseline_2022_nonwellsite(ad_proj))
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      vintage_bin == "OOOOb" & year >= nsps.start.yr ~ proposal_fate,
      
      vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") & year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    ))
  
  result <- scn_fate$fate
}

# MORE STRINGENT SCENARIO
proposal_2022_more_nonwellsite <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_nonwellsite(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      # Fugitives
      
      mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_quarterly",
      
      mp %in% c("procplant_small_voc", "procplant_large_voc",
                "procplant_small_nonvoc", "procplant_large_nonvoc") ~ "ogi_bimonthly", 
      
      # Pneumatic devices
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_solar_retro",
      mp == "pneu_dev" &
        segment %in% c("G&B") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_solar_new",
      mp == "pneu_dev" &
        segment %in% c("TRANS") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_elec_retro",
      mp == "pneu_dev" &
        segment %in% c("TRANS") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_elec_new",
      mp == "pneu_dev" &
        segment %in% c("STOR") &
        vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "zero_emis_air_retro",
      mp == "pneu_dev" &
        segment %in% c("STOR") &
        vintage_bin %in% c("OOOOb") ~ "zero_emis_air_new",
      
      # Compressors
      mp == "recip_comp" ~ "seals_annual",
      
      mp == "wet_cntfgl_comp" & segment == "G&B" ~ "3_scfm_limit", 
      mp == "wet_cntfgl_comp" &
        segment %in% c("PROC") &
        vintage_bin %in% c("OOOO", "OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" &
        segment %in% c("TRANS", "STOR") &
        vintage_bin %in% c("OOOOa", "OOOOb") ~ "route_new_combustion_dev",
      mp == "wet_cntfgl_comp" ~ "3_scfm_limit",
      
      mp == "dry_cntfgl_comp" ~ "3_scfm_limit",
      
      # Liquids Unloading
      mp %in% c("liq_unl_event_w_lift", "liq_unl_event_no_lift") ~ "best_mgmt",
      
      TRUE ~ NA_character_
    ))
  
  result <- scn_fate$fate
}

# assign start years for proposal fates
proposal_2022_more_phasein_nonwellsite <- function(
  ad_proj
) {
  
  
  proposal_fate <- combo_w_state_policy_nonwellsite(ad_proj, proposal_2022_more_nonwellsite(ad_proj))
  baseline_fate <- combo_w_state_policy_nonwellsite(ad_proj, baseline_2022_nonwellsite(ad_proj))
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      vintage_bin == "OOOOb" & year >= nsps.start.yr ~ proposal_fate,
      
      vintage_bin %in% c("OOOOc", "OOOO", "OOOOa") & year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    ))
  
  result <- scn_fate$fate
}

#### FUGITIVES ====
#### Baseline Scenario Functions ------------------------

# calculate mp-fate-proj assuming no required control
no_control_baseline_fugitives <- function(
  ad_proj
) {

 scn_fate <- ad_proj %>%
   mutate(fug_fate = "bau") %>% as_tibble()
 
 result <- scn_fate$fug_fate

}

# main baseline for 2022 supplemental reflects CRA reversal of some 2020 policy review components
baseline_2022_fugitives <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(fug_fate = case_when(
      str_detect(Model.Plant, "EquipBin1") & Reg.Bin %in% c("OOOOa", "OOOOb") ~ "bau",
      str_detect(Model.Plant, "EquipBin2") & Reg.Bin %in% c("OOOOa", "OOOOb") ~ "bau",
      Reg.Bin %in% c("OOOOa", "OOOOb") ~ "ogi_semi",
      TRUE ~ "bau"
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate 
  
}

# combine baseline fates with state policy fates
baseline_2022_w_state_policy_fugitives <- function(
  ad_proj
) {
  
  combo_w_state_policy_fugitives(ad_proj, baseline_2022_fugitives(ad_proj))  
  
}

# set baseline fates for certain facilities in California and Colorado to be the same as the proposal
combo_w_state_policy_fugitives <- function(
  ad_proj, 
  otherwise_fate, 
  scenario = "prop"
) {
  
  proposal_fate <- proposal_2022_prop_fugitives(ad_proj)
  
  scn_fate <- ad_proj %>% as_tibble()
  
  scn_fate <- scn_fate %>% mutate(
    fug_fate = case_when(
      
      State %in% c("CO", "CA", "NM", "PA") ~ proposal_fate,
      
      TRUE ~ otherwise_fate)
    )
  
  result <- scn_fate$fug_fate
  
}

#### Policy Scenario Functions ------------------------------

# PRIMARY PROPOSAL SCENARIO 
proposal_2022_prop_fugitives <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_fugitives(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    fug_fate = case_when(
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Single" ~ "avo_quarterly",
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Multi" ~ "ogi_semi_avo_quarterly",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Single" ~ "avo_quarterly",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Multi" ~ "ogi_semi_avo_quarterly",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Single" ~ "avo_quarterly",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Multi" ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin4") ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin5") ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin6") ~ "ogi_quarterly_avo_bimonthly"
      
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate
  
}

# assign start years for proposal fates
proposal_2022_prop_phasein_fugitives <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_fugitives(ad_proj, proposal_2022_prop_fugitives(ad_proj))
  baseline_fate <- combo_w_state_policy_fugitives(ad_proj, baseline_2022_fugitives(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(fug_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate
  
}

# LESS STRINGENT SCENARIO 
proposal_2022_less_fugitives <- function(
  ad_proj
) {
  
  proposal_fate <- proposal_2022_prop_fugitives(ad_proj) 
  
  scn_fate <- ad_proj %>% mutate(
    fug_fate = case_when(
      State %in% c("CO", "CA", "NM", "PA") ~ proposal_fate,
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Single" ~ "bau",
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Multi" ~ "ogi_semi",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Single" ~ "bau",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Multi" ~ "ogi_semi",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Single" ~ "bau",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Multi" ~ "ogi_quarterly", 
      str_detect(Model.Plant, "EquipBin4") ~ "ogi_quarterly", 
      str_detect(Model.Plant, "EquipBin5") ~ "ogi_quarterly", 
      str_detect(Model.Plant, "EquipBin6") ~ "ogi_quarterly"
      
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate    
  
}

# assign start years for proposal fates
proposal_2022_less_phasein_fugitives <- function(
  ad_proj
) {
  
  proposal_fate <- proposal_2022_less_fugitives(ad_proj)
  baseline_fate <- combo_w_state_policy_fugitives(ad_proj, baseline_2022_fugitives(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(fug_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate 
  
}

# MORE STRINGENT SCENARIO
proposal_2022_more_fugitives <- function(
  ad_proj
) {

  state_fate <- no_control_baseline_fugitives(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    fug_fate = case_when(
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Single" ~ "avo_quarterly",
      str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Multi" ~ "ogi_semi_avo_quarterly",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Single" ~ "avo_quarterly",
      str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Multi" ~ "ogi_semi_avo_quarterly",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Single" ~ "ogi_semi_avo_quarterly",
      str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Multi" ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin4") ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin5") ~ "ogi_quarterly_avo_bimonthly", 
      str_detect(Model.Plant, "EquipBin6") ~ "ogi_quarterly_avo_bimonthly"
      
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate  
  
}

# assign start years for proposal fates
proposal_2022_more_phasein_fugitives <- function(
  ad_proj
) {
  
  proposal_fate <- proposal_2022_more_fugitives(ad_proj)
  baseline_fate <- combo_w_state_policy_fugitives(ad_proj, baseline_2022_fugitives(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(fug_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$fug_fate  
  
}

#### TANKS ====
#### Baseline Scenario Functions ------------------------

# Calculate mp-fate-proj assuming no required control
no_control_baseline_tanks <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(tank_fate = "bau") %>% as_tibble()
  
  result <- scn_fate$tank_fate
  
}

#' Main baseline for 2022 supplemental reflects CRA reversal of some 2020 policy review components
baseline_2022_tanks <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(tank_fate = case_when(
      Reg.Bin %in% c("OOOOc") ~ "bau",
      Reg.Bin %in% c("OOOO", "OOOOa") & (VOC_per_tank_start >= 6) ~ "combustor_retro",
      Reg.Bin %in% c("OOOOb") & (VOC_per_tank_start >= 6) ~ "combustor_new",
      TRUE ~ "bau"
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate 
  
}

# combine baseline fates with state policy fates
baseline_2022_w_state_policy_tanks <- function(
  ad_proj
) {
  combo_w_state_policy_tanks(ad_proj, baseline_2022_tanks(ad_proj))
}

# set baseline fates for certain facilities in California and Colorado to be the same as the proposal
combo_w_state_policy_tanks <- function(
  ad_proj, 
  otherwise_fate, 
  scenario = "prop"
) {
  
  proposal_fate <- proposal_2022_prop_tanks(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(tank_fate = case_when(
      
        State %in% c("CA", "CO") ~ proposal_fate,
      
      TRUE ~ otherwise_fate
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate
}

#### Policy Scenario Functions ------------------------------

proposal_2022_prop_tanks <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_tanks(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    tank_fate = case_when(
     Reg.Bin %in% c("OOOOb") & (CH4_per_battery_start >= 20 | VOC_per_battery_start >= 6) ~ "combustor_new",
     Reg.Bin %in% c("OOOOc") & CH4_per_battery_EG >= 20 ~ "combustor_retro",
     Reg.Bin %in% c("OOOO", "OOOOa") & (CH4_per_battery_EG >= 20 | VOC_per_tank_start >= 6) ~ "combustor_retro",
     TRUE ~ "bau"
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate
  
}

# assign start years for proposal fates
proposal_2022_prop_phasein_tanks <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_tanks(ad_proj, proposal_2022_prop_tanks(ad_proj))
  baseline_fate <- combo_w_state_policy_tanks(ad_proj, baseline_2022_tanks(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(tank_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate
  
}

# LESS STRINGENT SCENARIO
proposal_2022_less_tanks <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_tanks(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    tank_fate = case_when(
      Reg.Bin %in% c("OOOOb") & (CH4_per_battery_start >= 20 | VOC_per_battery_start >= 6) ~ "combustor_new",
      Reg.Bin %in% c("OOOOc") & CH4_per_battery_EG >= 20 ~ "combustor_retro",
      Reg.Bin %in% c("OOOO", "OOOOa") & (CH4_per_battery_EG >= 20 | VOC_per_tank_start >= 6) ~ "combustor_retro",
      TRUE ~ "bau"
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate    
  
}

# assign start years for proposal fates
proposal_2022_less_phasein_tanks <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_tanks(ad_proj, proposal_2022_less_tanks(ad_proj))
  baseline_fate <- combo_w_state_policy_tanks(ad_proj, baseline_2022_tanks(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(tank_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate 
  
}

# MORE STRINGENT SCENARIO
proposal_2022_more_tanks <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_tanks(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    tank_fate = case_when(
      Reg.Bin %in% c("OOOOb") & (CH4_per_battery_start >= 20 | VOC_per_battery_start >= 6) ~ "combustor_new",
      Reg.Bin %in% c("OOOOc") & CH4_per_battery_EG >= 20 ~ "combustor_retro",
      Reg.Bin %in% c("OOOO", "OOOOa") & (CH4_per_battery_EG >= 20 | VOC_per_tank_start >= 6) ~ "combustor_retro",
      TRUE ~ "bau"
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate  
  
}

# assign start years for proposal fates
proposal_2022_more_phasein_tanks <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_tanks(ad_proj, proposal_2022_more_tanks(ad_proj))
  baseline_fate <- combo_w_state_policy_tanks(ad_proj, baseline_2022_tanks(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(tank_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$tank_fate  
  
}

#### PNEUMATICS ====
#### Baseline Scenario Functions ------------------------

# Calculate mp-fate-proj assuming no required control
no_control_baseline_pneumatic <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>%
    mutate(pneumatic_fate = case_when(
      Reg.Bin == "OOOOb" ~ "bau_new",
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") ~ "bau_retro"
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate
  
}

# Main baseline for 2022 supplemental reflects CRA reversal of some 2020 policy review components
baseline_2022_pneumatic <- function(
  ad_proj
) {
  
  scn_fate <- ad_proj %>% as_tibble() %>% 
    mutate(pneumatic_fate = case_when(
      
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOa") ~ "bau_retro",
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOb") ~ "bau_new",
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOc", "OOOO")~ "bau_retro",
      
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOO","OOOOa") ~ "high_to_low_bleed_retro",
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOOc") ~ "bau_retro",
      
      
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate 
  
}

# combine baseline fates with state policy fates
baseline_2022_w_state_policy_pneumatic <- function(
  ad_proj
) {
  combo_w_state_policy_pneumatic(ad_proj, baseline_2022_pneumatic(ad_proj))
}

# set baseline fates for certain facilities in California and Colorado to be the same as the proposal
combo_w_state_policy_pneumatic <- function(
  ad_proj, 
  otherwise_fate, 
  scenario = "prop"
) {
  
  proposal_fate <- proposal_2022_prop_pneumatic(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(pneumatic_fate = case_when(
      
      State %in% c("CA", "CO") ~ proposal_fate,
      
      TRUE ~ otherwise_fate
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate
}

#### Policy Scenario Functions ------------------------------
# Less/more stringent is same as proposal option for now

proposal_2022_prop_pneumatic <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_pneumatic(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    pneumatic_fate = case_when(
      Reg.Bin %in% c("OOOOb") ~ "zero_emis_solar_new",
      Reg.Bin %in% c("OOOO", "OOOOa", "OOOOc") ~ "zero_emis_solar_retro",
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate
  
}

# assign start years for proposal fates
proposal_2022_prop_phasein_pneumatic <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_pneumatic(ad_proj, proposal_2022_prop_pneumatic(ad_proj))
  baseline_fate <- combo_w_state_policy_pneumatic(ad_proj, baseline_2022_pneumatic(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(pneumatic_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate
  
}

# LESS STRINGENT SCENARIO
proposal_2022_less_pneumatic <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_pneumatic(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    pneumatic_fate = case_when(
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOa") ~ "bau_retro",
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOb") ~ "bau_new",
      str_detect(Model.Plant, "pump") & Reg.Bin %in% c("OOOOc", "OOOO")~ "bau_retro",
      
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOO","OOOOa") ~ "high_to_low_bleed_retro",
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOOb") ~ "high_to_low_bleed_new",
      str_detect(Model.Plant, "controller") & Reg.Bin %in% c("OOOOc") ~ "high_to_low_bleed_retro",
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate    
  
}

# assign start years for proposal fates
proposal_2022_less_phasein_pneumatic <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_pneumatic(ad_proj, proposal_2022_less_pneumatic(ad_proj))
  baseline_fate <- combo_w_state_policy_pneumatic(ad_proj, baseline_2022_pneumatic(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(pneumatic_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate 
  
}

# MORE STRINGENT SCENARIO
proposal_2022_more_pneumatic <- function(
  ad_proj
) {
  
  state_fate <- no_control_baseline_pneumatic(ad_proj)
  
  scn_fate <- ad_proj %>% mutate(
    pneumatic_fate = case_when(
      Reg.Bin %in% c("OOOOb") ~ "zero_emis_solar_new",
      Reg.Bin %in% c("OOOO", "OOOOa", "OOOOc") ~ "zero_emis_solar_retro",
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate  
  
}

# assign start years for proposal fates
proposal_2022_more_phasein_pneumatic <- function(
  ad_proj
) {
  
  proposal_fate <- combo_w_state_policy_pneumatic(ad_proj, proposal_2022_more_pneumatic(ad_proj))
  baseline_fate <- combo_w_state_policy_pneumatic(ad_proj, baseline_2022_pneumatic(ad_proj))  
  
  scn_fate <- ad_proj %>%
    
    mutate(pneumatic_fate = case_when(
      
      Reg.Bin == "OOOOb" & Year >= nsps.start.yr ~ proposal_fate,
      
      Reg.Bin %in% c("OOOOc", "OOOO", "OOOOa") & Year >= eg.start.yr ~ proposal_fate,
      
      TRUE ~ baseline_fate
    )) %>% as_tibble()
  
  result <- scn_fate$pneumatic_fate  
  
}
