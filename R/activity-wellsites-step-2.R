#' activity-wellsites-step-2.R
#' These functions convert well site projections (with equipment) to affected
#' facility projections using a standardized format.

# Get fugitive component emissions
get_wellsite_emissions <- function(
  emissions_measure, 
  .wellsite_bins_proj_emis
  ) {
  
  # select single year
  .dta <- .wellsite_bins_proj_emis %>%
    select(Block.ID, Year, ProdRate.Bin, 
           Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
           Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
           contains("Fug_Comp"))
  
  if(emissions_measure == "BSER") {
    
    .dta <- .dta %>% select(Block.ID, Year, ProdRate.Bin, 
                            Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                            Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                            contains("BSER_halfpercentleak")) %>%
      rename(Methane = Fug_Comp_CH4_Site_BSER_halfpercentleak,
             VOC = Fug_Comp_VOC_Site_BSER_halfpercentleak,
             HAP = Fug_Comp_HAP_Site_BSER_halfpercentleak)
    
  }
  
  else if(emissions_measure == "GHGI") {
    
    .dta <- .dta %>% select(Block.ID, Year, ProdRate.Bin, 
                            Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                            Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                            contains("GHGI")) %>%
      rename(Methane = Fug_Comp_CH4_Site_GHGI,
             VOC = Fug_Comp_VOC_Site_GHGI,
             HAP = Fug_Comp_HAP_Site_GHGI)
    
  }
  
  else if(emissions_measure == "Rutherford") {
    
    .dta <- .dta %>% select(Block.ID, Year, ProdRate.Bin, 
                            Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                            Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                            contains("Rutherford")) %>%
      rename(Methane = Fug_Comp_CH4_Site_Rutherford,
             VOC = Fug_Comp_VOC_Site_Rutherford,
             HAP = Fug_Comp_HAP_Site_Rutherford)
    
  }
  
  else if(emissions_measure == "GHGRP") {
    
    .dta <- .dta %>% select(Block.ID, Year, ProdRate.Bin, 
                            Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                            Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                            contains("GHGRP")) %>%
      rename(Methane = Fug_Comp_CH4_Site_GHGRP,
             VOC = Fug_Comp_VOC_Site_GHGRP,
             HAP = Fug_Comp_HAP_Site_GHGRP)
    
  }
  
  else{
    
    stop("emissions_measure must be BSER, GHGI, Rutherford, or GHGRP")
    
  }
  
  .dta %>% distinct(Block.ID, Year, ProdRate.Bin, 
                    Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                    Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                    Methane, VOC, HAP) %>% arrange(Block.ID) %>% as.data.table()
  
}

# Activity data for fugitives
get_wellsite_fug_ad <- function(
  wellsite_bins_proj,
  wellsite_bins_proj_emis,
  emissions_measure = "BSER"
  ) {
  
  # get Block.ID site-level fugitive component emissions
  stopifnot(emissions_measure %in% c("Rutherford", "GHGI", "GHGRP", "BSER"))
  fug_emissions <- get_wellsite_emissions(emissions_measure = "BSER", wellsite_bins_proj_emis)
  
  fug_ad <- wellsite_bins_proj
  
  # extract needed columns
  fug_ad <- wellsite_bins_proj[, Reg.Bin := fcase(
    Vintage.Bin %in% c("Pre-2000", "2000-2011"), "OOOOc",
    Vintage.Bin == "2012-2015", "OOOO",
    Vintage.Bin %in% c("2016-2019", "2020", "2021"), "OOOOa", 
    default = "OOOOb"
  )][, .(Block.ID, Site.Type.Bin, State, Year, Vintage.Bin, Reg.Bin, Type, 
         ProdRate.Bin,  
         Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
         Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
         equip_bin, Wellsite.Count,
         Cum.Wellsites = Wellsite.Count,
         New.Wellsites = Wellsite.Count*(Vintage.Bin == Year))]
  
  # merge with emissions data
  fug_ad <- merge(fug_ad, fug_emissions, by = c("Block.ID", "Year", "ProdRate.Bin",  
                                                               "Oil.Bbl.Site.Start", "Oil.Bbl.Site.EG", "Oil.Bbl.Site.NSPS",
                                                               "Cond.Bbl.Site.Start", "Cond.Bbl.Site.EG", "Cond.Bbl.Site.NSPS"
  ))
  
  # assign to production and model plant bins
  fug_st_proj_std <- fug_ad[, .(
    Model.Plant = str_c("wellsite_", Type, "_", fcase(
      str_detect(ProdRate.Bin, "<=3"), "ProdBin1",
      str_detect(ProdRate.Bin, "3-15"), "ProdBin2",
      str_detect(ProdRate.Bin, "15-100"), "ProdBin3",
      str_detect(ProdRate.Bin, ">100"), "ProdBin4"
    ), "_EquipBin", str_extract(equip_bin, "^([123456])"), "_Block", Block.ID),
    Segment = "PROD",
    Block.ID, Type, Site.Type.Bin, State, Reg.Bin, Year, Vintage.Bin, ProdRate.Bin, 
    Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
    Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
    Fac.Count=Cum.Wellsites, New.Count=New.Wellsites, Methane, VOC, HAP)][
      , fug_mp := fcase(str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Single", 1,
                        str_detect(Model.Plant, "EquipBin1") & Site.Type.Bin == "Multi", 2,
                        str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Single", 1,
                        str_detect(Model.Plant, "EquipBin2") & Site.Type.Bin == "Multi", 2,
                        str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Single", 1,
                        str_detect(Model.Plant, "EquipBin3") & Site.Type.Bin == "Multi", 3,
                        str_detect(Model.Plant, "EquipBin4"), 3,
                        str_detect(Model.Plant, "EquipBin5"), 3,
                        str_detect(Model.Plant, "EquipBin6"), 3)]
  
  fug_st_proj_std
  
}

# Activity data for storage vessels
get_wellsite_tanks_ad <- function(
  wellsite_bins_proj,
  wellsite_bins_proj_emis
  ) {
  
  # extract needed columns
  tank_ad <- wellsite_bins_proj[, Reg.Bin := fcase(
    Vintage.Bin %in% c("Pre-2000", "2000-2011"), "OOOOc",
    Vintage.Bin == "2012-2015", "OOOO",
    Vintage.Bin %in% c("2016-2019", "2020", "2021"), "OOOOa",
    default = "OOOOb"
  )][, .(Block.ID, State, Year, Vintage.Bin,  
         Reg.Bin, Type, Has.Tank.Battery,
         ProdRate.Bin,  
         Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
         Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
         Cum.Tanks = Wellsite.Count*Tank.Count.Site,
         New.Tanks = Wellsite.Count*Tank.Count.Site*(Vintage.Bin == Year),
         Tanks.Per.Site = Tank.Count.Site)]
  
  # restrict to sites with tank batteries
  tank_ad <- tank_ad[Has.Tank.Battery == TRUE]
  
  # construct new columns in emissions data
  tank_emissions <- wellsite_bins_proj_emis[, .(Block.ID, Year, ProdRate.Bin,  
                                                Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
                                                Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                                                Has.Tank.Battery, Tank.Count.Site,
                                                Tank_CH4_Cond_Site_EPTanks, Tank_CH4_Crude_Site_EPTanks,
                                                Tank_VOC_Cond_Site_EPTanks, Tank_VOC_Crude_Site_EPTanks,
                                                CH4_per_site = Tank_CH4_Cond_Site_EPTanks + Tank_CH4_Crude_Site_EPTanks,
                                                VOC_per_site = Tank_VOC_Cond_Site_EPTanks + Tank_VOC_Crude_Site_EPTanks,
                                                CH4_per_battery = Tank_CH4_Cond_Site_EPTanks + Tank_CH4_Crude_Site_EPTanks,
                                                VOC_per_battery = Tank_VOC_Cond_Site_EPTanks + Tank_VOC_Crude_Site_EPTanks,
                                                CH4_per_battery_start = Tank_CH4_Site_EPTanks_Start, 
                                                VOC_per_battery_start = Tank_VOC_Site_EPTanks_Start,
                                                CH4_per_battery_EG = Tank_CH4_Site_EPTanks_EG, 
                                                VOC_per_battery_EG = Tank_VOC_Site_EPTanks_EG,
                                                CH4_per_battery_NSPS = Tank_CH4_Site_EPTanks_NSPS, 
                                                VOC_per_battery_NSPS = Tank_VOC_Site_EPTanks_NSPS,
                                                CH4_per_tank = fifelse(Tank.Count.Site == 0, 0, (Tank_CH4_Cond_Site_EPTanks + Tank_CH4_Crude_Site_EPTanks)/Tank.Count.Site),
                                                VOC_per_tank = fifelse(Tank.Count.Site == 0, 0, (Tank_VOC_Cond_Site_EPTanks + Tank_VOC_Crude_Site_EPTanks)/Tank.Count.Site),
                                                CH4_per_tank_start = fifelse(Tank.Count.Site == 0, 0, Tank_CH4_Site_EPTanks_Start/Tank.Count.Site), 
                                                VOC_per_tank_start = fifelse(Tank.Count.Site == 0, 0, Tank_VOC_Site_EPTanks_Start/Tank.Count.Site),
                                                CH4_per_tank_EG = fifelse(Tank.Count.Site == 0, 0, Tank_CH4_Site_EPTanks_EG/Tank.Count.Site), 
                                                VOC_per_tank_EG = fifelse(Tank.Count.Site == 0, 0, Tank_VOC_Site_EPTanks_EG/Tank.Count.Site),
                                                CH4_per_tank_NSPS = fifelse(Tank.Count.Site == 0, 0, Tank_CH4_Site_EPTanks_NSPS/Tank.Count.Site), 
                                                VOC_per_tank_NSPS = fifelse(Tank.Count.Site == 0, 0, Tank_VOC_Site_EPTanks_NSPS/Tank.Count.Site))]
  
  # restrict emissions data to sites with tank batteries
  tank_emissions <- tank_emissions[Has.Tank.Battery == TRUE]
  
  # merge with emissions data
  tank_ad <- merge(tank_ad, tank_emissions, by = c("Block.ID", "Year", "ProdRate.Bin",  
                                                   "Oil.Bbl.Site.Start", "Oil.Bbl.Site.EG", "Oil.Bbl.Site.NSPS",
                                                   "Cond.Bbl.Site.Start", "Cond.Bbl.Site.EG", "Cond.Bbl.Site.NSPS",
                                                   "Has.Tank.Battery"))
  
  # create standardized emissions columns
  tank_ad <- tank_ad[, `:=`(CH4 = CH4_per_battery, VOC = VOC_per_battery, HAP = VOC_per_battery*voc_to_hap_prod)]
  
  # Fac.Count and New.Count converted from tanks to tank batteries
  tank_st_proj_std <- tank_ad[, .(
    Model.Plant = str_c("storage_vessel_", Type, "_Block", Block.ID),
    Block.ID,
    Type,
    Segment = "PROD",
    State, Reg.Bin, Year, 
    Vintage.Bin, ProdRate.Bin,
    Fac.Count=Cum.Tanks/Tank.Count.Site, New.Count=New.Tanks/Tank.Count.Site, 
    Methane = CH4, VOC, HAP,
    CH4_per_site, VOC_per_site,
    CH4_per_tank, VOC_per_tank,
    CH4_per_tank_start, VOC_per_tank_start,
    CH4_per_tank_EG, VOC_per_tank_EG,
    CH4_per_tank_NSPS, VOC_per_tank_NSPS,
    CH4_per_battery, VOC_per_battery,
    CH4_per_battery_start, VOC_per_battery_start,
    CH4_per_battery_EG, VOC_per_battery_EG,
    CH4_per_battery_NSPS, VOC_per_battery_NSPS,
    Tanks.Per.Site)]
  
  tank_st_proj_std
  
}

# Get pneumatic device emissions
get_pneumatic_emissions <- function(
  emissions_measure = "GHGI", 
  .wellsite_bins_proj_emis
  ) {
  
  pneumatic_emissions <- .wellsite_bins_proj_emis %>% 
    select(Block.ID, Year, ProdRate.Bin, 
           Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
           Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
           contains("PnC"), contains("Pumps")) %>% as_tibble()
  
  if(emissions_measure == "GHGI") {
    pneumatic_emissions <- pneumatic_emissions %>%
      select(Block.ID, Year, 
             Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
             Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
             ProdRate.Bin, contains("GHGI")) %>%
      rename_with(~str_remove(., "_GHGI"))
  }
  
  else if(emissions_measure == "GHGRP") {
    pneumatic_emissions <- pneumatic_emissions %>%
      select(Block.ID, Year, ProdRate.Bin, 
             Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
             Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
             contains("GHGRP")) %>%
      rename_with(~str_remove(., "_GHGRP"))
  }
  
  else{
    
    stop("emissions_measure must be GHGI or GHGRP")
    
  }
  
  pneumatic_emissions %>% as.data.table()
  
}

# Activity data for pneumatic devices
get_wellsite_pneu_ad <- function(
  wellsite_bins_proj, 
  wellsite_bins_proj_emis, 
  emissions_measure = "GHGI"
  ) {
  
  
  #site emissions for pumps, LB,IB,HB
  pneumatic_emissions <- get_pneumatic_emissions(emissions_measure = "GHGI",
                                                 wellsite_bins_proj_emis)
  
  #select columns, merge with site emissions
  pneumatic_ad <- wellsite_bins_proj[, .(
    Block.ID, Has_HB_PnC, Type, Year, State, 
    Vintage.Bin, ProdRate.Bin, 
    Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
    Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
    Wellsite.Count,
    Has_Pumps, Pumps_per_site,
    LB_per_site = PnC_per_site_lb,
    IB_per_site = PnC_per_site_ib,
    HB_per_site = PnC_per_site_hb
  )][, Reg.Bin := fcase(
    Vintage.Bin %in% c("Pre-2000", "2000-2011"), "OOOOc",
    Vintage.Bin == "2012-2015", "OOOO",
    Vintage.Bin %in% c("2016-2019", "2020", "2021"), "OOOOa",
    default = "OOOOb"
  )]
  
  pneumatic_ad <- merge(pneumatic_ad, pneumatic_emissions, by = c("Block.ID", "Year", "ProdRate.Bin",  
                                                                  "Oil.Bbl.Site.Start", "Oil.Bbl.Site.EG", "Oil.Bbl.Site.NSPS",
                                                                  "Cond.Bbl.Site.Start", "Cond.Bbl.Site.EG", "Cond.Bbl.Site.NSPS"), 
                        allow.cartesian = TRUE) 
  
  pneumatic_ad <- pneumatic_ad %>% as_tibble() %>%
    mutate(new_rows = 4) %>%
    uncount(new_rows) %>% 
    mutate(row = rep(1:4, n()/4)) %>% relocate(row) %>%
    mutate(equipment = case_when(
      row == 1 ~ "lb_controller",
      row == 2 ~ "ib_controller",
      row == 3 ~ "hb_controller",
      row == 4 ~ "pump"
    )) %>% filter(!(equipment == "pump" & !Has_Pumps))
  
  #add counts
  pneumatic_ad <- pneumatic_ad %>% as.data.table() %>%
    mutate(Fac.Count = case_when(
      equipment == "lb_controller" ~ LB_per_site*Wellsite.Count,
      equipment == "ib_controller" ~ IB_per_site*Wellsite.Count,
      equipment == "hb_controller" ~ HB_per_site*Wellsite.Count,
      equipment == "pump" ~ Pumps_per_site*Wellsite.Count
    ),
    New.Count = Fac.Count*(Vintage.Bin == Year),
    PnC_per_site = LB_per_site + IB_per_site + HB_per_site) 
  
  #add emissions
  pneumatic_ad <- pneumatic_ad %>%
    mutate(Methane = case_when(
      equipment == "lb_controller" ~ (PnC_LB_CH4_Site)/LB_per_site + Fug_PnC_CH4_Site/PnC_per_site,
      equipment == "ib_controller" ~ (PnC_IB_CH4_Site)/IB_per_site + Fug_PnC_CH4_Site/PnC_per_site,
      equipment == "hb_controller" ~ (PnC_HB_CH4_Site)/HB_per_site + Fug_PnC_CH4_Site/PnC_per_site,
      equipment == "pump" ~ (Pumps_CH4_Site)/Pumps_per_site
    ),
    VOC = case_when(
      equipment == "lb_controller" ~ (PnC_LB_VOC_Site)/LB_per_site + Fug_PnC_VOC_Site/PnC_per_site,
      equipment == "ib_controller" ~ (PnC_IB_VOC_Site)/IB_per_site + Fug_PnC_VOC_Site/PnC_per_site,
      equipment == "hb_controller" ~ (PnC_HB_VOC_Site)/HB_per_site + Fug_PnC_VOC_Site/PnC_per_site,
      equipment == "pump" ~ (Pumps_VOC_Site)/Pumps_per_site
    ),
    HAP = case_when(
      equipment == "lb_controller" ~ (PnC_LB_HAP_Site)/LB_per_site + Fug_PnC_HAP_Site/PnC_per_site,
      equipment == "ib_controller" ~ (PnC_IB_HAP_Site)/IB_per_site + Fug_PnC_HAP_Site/PnC_per_site,
      equipment == "hb_controller" ~ (PnC_HB_HAP_Site)/HB_per_site + Fug_PnC_HAP_Site/PnC_per_site,
      equipment == "pump" ~ (Pumps_HAP_Site)/Pumps_per_site
    )) %>% as.data.table()
  
  pneu_st_proj_std <- pneumatic_ad[, .(
    Model.Plant = str_c(equipment, "_", Type, "_Block", Block.ID),
    Block.ID,
    Type,
    Segment = "PROD",
    State, Reg.Bin, Year, 
    Vintage.Bin, ProdRate.Bin, 
    Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS,
    Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
    Fac.Count, New.Count, 
    Methane, VOC, HAP,
    Pneu_per_site = PnC_per_site + Pumps_per_site)][,
                                                    `:=`(
                                                      Methane = fifelse(is.nan(Methane), 0, Methane),
                                                      VOC = fifelse(is.nan(VOC), 0, VOC),
                                                      HAP = fifelse(is.nan(HAP), 0, HAP)
                                                    )
                                                    
    ]
  
  pneu_st_proj_std %>% as_tibble()
  
}

# Activity data for liquids unloading
get_liquids_unloading_ad <- function(
  wellsite_bins_proj,
  gas_act_dta,
  liq.unl.plunge.mnl.frac = .76,
  liq.unl.plunge.events.per.well = 7.7,
  liq.unl.wo.plunge.events.per.well = 5.6
) {
  
  # calculate "wellhead-only" fraction
  who.frac <- sum(wellsite_bins_proj[str_detect(wellsite_bins_proj$equip_bin, "^[12]"), .(Gas.Well.Count)])/sum(wellsite_bins_proj[, .(Gas.Well.Count)])
  
  ## gas wells
  # initialize projection table
  well_liq_unl_proj = wellsite_bins_proj[Type == "Gas" & str_detect(equip_bin, "^[3456]:"), .(State, Vintage.Bin, Year, Gas.Well.Count)]
  well_liq_unl_proj[, `:=` (
    Reg.Bin = fcase(
      Vintage.Bin %in% c("Pre-2000", "2000-2011"), "OOOOc",
      Vintage.Bin == "2012-2015", "OOOO",
      Vintage.Bin %in% c("2016-2019", "2020", "2021"), "OOOOa", 
      default = "OOOOb"
    ), 
    Vint.Trunc = fcase(
      Vintage.Bin %in% c("Pre-2000", "2000-2011", "2012-2015", "2016-2019"), as.integer(2019),
      Vintage.Bin %in% seq(base.yr+1, end.yr), as.integer(as.character(Vintage.Bin))
    ))]
  
  # expand to create a plunger lift distinction
  well_liq_unl_proj = CJ.table(well_liq_unl_proj, data.table(Plunger.Lift = c("Yes", "No")))[order(Year, State, Vintage.Bin, Reg.Bin, Plunger.Lift)]
  
  # calculate fraction of wells that are venting
  well.gas.count.2019 = (1-who.frac)*gas_act_dta[Segment == "PROD" & Source == "Total Active Gas Wells" & Year == 2019, Count]
  liq.unl.plunge.count.2019 = gas_act_dta[Segment == "PROD" & Source == "Liquids Unloading with Plunger Lifts" & Year == 2019, Count]
  liq.unl.plunge.frac.2019 = liq.unl.plunge.count.2019/well.gas.count.2019
  liq.unl.wo.plunge.count.2019 = gas_act_dta[Segment == "PROD" & Source == "Liquids Unloading without Plunger Lifts" & Year == 2019, Count]
  liq.unl.wo.plunge.frac.2019 = liq.unl.wo.plunge.count.2019/well.gas.count.2019
  
  # apply fractions of wells performing manual liquids unloading operations
  well_liq_unl_proj <- well_liq_unl_proj[, .(State, Vintage.Bin, Vint.Trunc, Reg.Bin, Plunger.Lift, Year, Gas.Well.Count,
                                             Event.Count = (liq.unl.plunge.events.per.well*liq.unl.plunge.mnl.frac*liq.unl.plunge.frac.2019*(Plunger.Lift == "Yes") +
                                                              liq.unl.wo.plunge.events.per.well*liq.unl.wo.plunge.frac.2019*(Plunger.Lift == "No"))*Gas.Well.Count)]
  
  well_liq_unl_proj <- well_liq_unl_proj[, .(Event.Count = sum(Event.Count)),
                                         by = .(State, Year, Reg.Bin, Vint.Trunc, Plunger.Lift)]
  
  well_liq_unl_st_proj_std <- well_liq_unl_proj[, .(
    Model.Plant = str_c("liq_unl_event_", if_else(Plunger.Lift == "Yes", "w_lift", "no_lift")),
    Type = NA_character_,
    Segment = "PROD",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count=Event.Count)]
  
  well_liq_unl_st_proj_std
  
}
