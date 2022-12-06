#' activity-wellsites-step-1.R
#' These functions construct well site projections from base year data from
#' Enverus. Sites are binned according to several characteristics, such as
#' vintage and production rate. The projections also include estimates of
#' equipment counts at the binned sites, which are calibrated to match base year
#' estimates at the national level for existing inventories.

# Clean base year wellsite information from Enverus
clean_enverus_data <- function(
  wells19_csv
) {
 
  ## Base year wells/wellsites data
  
  # read data Enverus wells data
  wells19 = fread(wells19_csv, check.names = T)[!(State %in% c("NGOM"))]
  
  ## clean base year data
  # remove unneeded columns, rename
  well_prod_dta = wells19[, .(
    Well.ID = API_UWI, 
    Pad.ID = WellPadID, 
    State,
    DB,
    FirstProdDate = parse_date_multi(FirstProdDate, formats = c("%m/%d/%Y", "%Y-%m-%d")),
    CompletionDate = parse_date_multi(FirstProdDate, formats = c("%m/%d/%Y", "%Y-%m-%d")),
    Well.Count = fifelse(!is.na(WellCount), WellCount, 1),
    Gas.Mcf.Entity = fifelse(!is.na(GasProd_MCF), GasProd_MCF, 0),
    Oil.Bbl.Entity = fifelse(!is.na(LiquidsProd_BBL), LiquidsProd_BBL, 0))]
  
  
  # Calculate Entity-Level Variables
  # (need entity-level variables for binning entities without Pad.ID's)
  
  well_prod_dta[, Modify.Date.Entity := as.IDate(pmax(FirstProdDate, CompletionDate, na.rm = TRUE))]
  well_prod_dta[, Modify.Date.Entity := fifelse(is.na(Modify.Date.Entity), as.IDate("1980-01-01"), Modify.Date.Entity)]
  well_prod_dta[, GOR.Entity := Gas.Mcf.Entity/Oil.Bbl.Entity]
  well_prod_dta[, Type.Entity := fifelse(GOR.Entity > 100, "Gas", "Oil")]
  well_prod_dta[, Cond.Bbl.Entity := fifelse(Type.Entity == "Gas", Oil.Bbl.Entity, 0)]
  well_prod_dta[, BOE.Entity := Oil.Bbl.Entity + Gas.Mcf.Entity/6]
  well_prod_dta[, ProdRate.BOEpd.Entity := BOE.Entity / 365 / Well.Count]
  well_prod_dta[, ProdRate.Class.Entity := fifelse(ProdRate.BOEpd.Entity > 15, "Non-low Production", "Low Production")]
  
  # Calculate Aggregate Site-Level Variables (rows still represent entities though)
  
  well_prod_dta[, `:=` (
    Well.Count.Site = sum(Well.Count),
    Gas.Well.Count.Site = sum(Well.Count*(Type.Entity == "Gas")),
    Oil.Well.Count.Site = sum(Well.Count*(Type.Entity == "Oil")),
    Well.ID.Site = 1:.N, 
    Modify.Date.Site = max(Modify.Date.Entity, na.rm = TRUE), # Taking most recent of all wells
    Gas.Mcf.Site = sum(Gas.Mcf.Entity), 
    Oil.Bbl.Site = sum(Oil.Bbl.Entity), 
    Cond.Bbl.Site = sum(Cond.Bbl.Entity)
  ), by = .(Pad.ID)]
  
  well_prod_dta[, GOR.Site := Gas.Mcf.Site/Oil.Bbl.Site]
  well_prod_dta[, BOE.Site := Oil.Bbl.Site + Gas.Mcf.Site/6]
  well_prod_dta[, ProdRate.BOEpd.Site := BOE.Site / 365 / Well.Count.Site]
  well_prod_dta[, Site.Type.Site := fifelse(Well.Count.Site > 1, "Multi", "Single")]
  well_prod_dta[, Site.Type.Site := factor(Site.Type.Site, levels = c("Single", "Multi"))]
  
  
  # Determine subset with wellsite info (Well Pad ID & <= 100 wells per site)
  # (other entities will rely on national averages for single/multi, wells per pad, and thus for production allocation)
  well_prod_dta[, has_wellsite_info := !is.na(Pad.ID) & Well.Count.Site <= 100]
  
  #### Choose values to use for binning, preferring pad level if a Pad ID exists
  # GOR (Type), Modify.Date (Vintage.Bin), ProdRate.BOEpd (ProdRate.Bin)
  
  well_prod_dta[, GOR := fifelse(has_wellsite_info, GOR.Site, GOR.Entity)]
  well_prod_dta[, Modify.Date := fifelse(has_wellsite_info, Modify.Date.Site, Modify.Date.Entity)]
  well_prod_dta[, ProdRate.BOEpd := fifelse(has_wellsite_info, ProdRate.BOEpd.Site, ProdRate.BOEpd.Entity)]
  
  well_prod_dta
  
}

# Summarize baseyear wellsite information from Enverus into blocks of wellsites
convert_wellsite_data_to_bins <- function(
  well_prod_dta, 
  decl_rates, 
  init.yr
) {
  
  # categorize entities as gas or oil based on GOR
  well_prod_dta[, Type := fifelse(GOR > 100, "Gas", "Oil")]
  
  # classify into production rate bins
  # create factor of bins, and check none are missing
  well_prod_dta[, ProdRate.Bin := case_when(
    ProdRate.BOEpd > 100 ~ ">100 BOE/day/well",
    ProdRate.BOEpd > 15 ~ "15-100 BOE/day/well",
    ProdRate.BOEpd > 3 ~ "3-15 BOE/day/well",
    TRUE ~ "<=3 BOE/day/well"
  )][, Prod.Class := fifelse(ProdRate.BOEpd > 15, "Non-low Production", "Low Production")]
  well_prod_dta[, ProdRate.Bin := factor(
    ProdRate.Bin, levels = c("<=3 BOE/day/well", "3-15 BOE/day/well", "15-100 BOE/day/well", ">100 BOE/day/well"))]
  stopifnot(! anyNA(well_prod_dta$ProdRate.Bin))
  
  # Split data subsets for with and without wellsite info
  nowellsite_data_subset <- well_prod_dta[has_wellsite_info == FALSE, .(
    State,
    Well.Count,
    Modify.Date,
    Type, Vintage.Bin, ProdRate.Bin, Prod.Class,
    Gas.Mcf.Entity, Oil.Bbl.Entity, Cond.Bbl.Entity, GOR, ProdRate.BOEpd
  )]
  
  # Assign initial production rate column (used to split into single/multi-well pads)
  nowellsite_data_subset[, Init.ProdRate.Bin := ProdRate.Bin]
  
  # Wellsite aggregates already calculated and selected above, so simulating aggregation by taking first well per pad
  # and only keeping site-level vars
  wellsite_data_subset <- well_prod_dta[has_wellsite_info & Well.ID.Site == 1, .(
    State,
    Well.Count.Site, Gas.Well.Count.Site, Oil.Well.Count.Site,
    Modify.Date,
    Type, Vintage.Bin, ProdRate.Bin, Prod.Class, Init.ProdRate.Bin = ProdRate.Bin,
    Gas.Mcf.Site, Oil.Bbl.Site, Cond.Bbl.Site, GOR,
    Wellsite.Count = 1
  ), by = .(Pad.ID)]
  
  wellsite_data_subset[, GOR.Site := Gas.Mcf.Site/Oil.Bbl.Site]
  wellsite_data_subset[, BOE.Site := Oil.Bbl.Site + Gas.Mcf.Site/6]
  wellsite_data_subset[, ProdRate.BOEpd.Site := BOE.Site / 365 / Well.Count.Site]
  
  # For this subset only, categorize by site type
  wellsite_data_subset[, Site.Type.Bin := fifelse(Well.Count.Site > 1, "Multi", "Single")]
  wellsite_data_subset[, Site.Type.Bin := factor(Site.Type.Bin, levels = c("Single", "Multi"))]
  
  ## Construct future year data projections
  # Initialize data tables
  wellsite_data_subset_proj = wellsite_data_subset[, `:=` (Year = init.yr, ProdRate.Bin.Prev = NA_character_)]
  setcolorder(wellsite_data_subset_proj, "Year")
  nowellsite_data_subset_proj = nowellsite_data_subset[, `:=` (Year = init.yr, ProdRate.Bin.Prev = NA_character_, 
                                                               BOE.Entity = Oil.Bbl.Entity + Gas.Mcf.Entity/6)]
  setcolorder(nowellsite_data_subset_proj, "Year")
  
  for (i in (init.yr+1):(init.yr + end.yr - base.yr)){
    
    # create temporary data table to perform calculations in
    temp1 = wellsite_data_subset_proj[Year == (i-1)][, Year := i]
    temp2 = nowellsite_data_subset_proj[Year == (i-1)][, Year := i]
    
    temp1 = merge.data.table(temp1, decl_rates, by = c("ProdRate.Bin"))
    temp2 = merge.data.table(temp2, decl_rates, by = c("ProdRate.Bin"))
    
    # calculate next year production
    temp1[, `:=` (Gas.Mcf.Site = (1-Gas.Decl)*Gas.Mcf.Site,
                  Oil.Bbl.Site = (1-Oil.Decl)*Oil.Bbl.Site,
                  Cond.Bbl.Site = (1-Oil.Decl)*Cond.Bbl.Site,
                  BOE.Site = (1-Oil.Decl)*Oil.Bbl.Site + (1-Gas.Decl)*Gas.Mcf.Site/6,
                  ProdRate.BOEpd.Site = ((1-Oil.Decl)*Oil.Bbl.Site + (1-Gas.Decl)*Gas.Mcf.Site/6) / 365 / Well.Count.Site)]
    temp2[, `:=` (Gas.Mcf.Entity = (1-Gas.Decl)*Gas.Mcf.Entity,
                  Oil.Bbl.Entity = (1-Oil.Decl)*Oil.Bbl.Entity,
                  Cond.Bbl.Entity = (1-Oil.Decl)*Cond.Bbl.Entity,
                  BOE.Entity = (1-Oil.Decl)*Oil.Bbl.Entity + (1-Gas.Decl)*Gas.Mcf.Entity/6,
                  ProdRate.BOEpd = ((1-Oil.Decl)*Oil.Bbl.Entity + (1-Gas.Decl)*Gas.Mcf.Entity/6) / 365 / Well.Count)]
    
    # update production rate bin and class
    temp1[, ProdRate.Bin.Prev := ProdRate.Bin]
    temp1[, ProdRate.Bin := fcase(
      ProdRate.BOEpd.Site > 100, ">100 BOE/day/well",
      ProdRate.BOEpd.Site > 15, "15-100 BOE/day/well",
      ProdRate.BOEpd.Site > 3, "3-15 BOE/day/well",
      ProdRate.BOEpd.Site <= 3, "<=3 BOE/day/well",
      default = NA_character_
    )][, Prod.Class := fifelse(ProdRate.BOEpd.Site > 15, "Non-low Production", "Low Production")][
      , c("Gas.Decl", "Oil.Decl") := NULL
    ]
    temp2[, ProdRate.Bin.Prev := ProdRate.Bin]
    temp2[, ProdRate.Bin := fcase(
      ProdRate.BOEpd > 100, ">100 BOE/day/well",
      ProdRate.BOEpd > 15, "15-100 BOE/day/well",
      ProdRate.BOEpd > 3, "3-15 BOE/day/well",
      ProdRate.BOEpd <= 3, "<=3 BOE/day/well",
      default = NA_character_
    )][, Prod.Class := fifelse(ProdRate.BOEpd > 15, "Non-low Production", "Low Production")][
      , c("Gas.Decl", "Oil.Decl") := NULL
    ]
    
    # stitch new year of data to existing data table
    wellsite_data_subset_proj = rbindlist(list(wellsite_data_subset_proj, temp1), use.names = T)
    nowellsite_data_subset_proj = rbindlist(list(nowellsite_data_subset_proj, temp2), use.names = T)
  }
  
  # Pathway for subset *with* wellsite info:
  
  # Aggregate into by-state bins, to be combined with nowellsite data subset later
  wellsite_st_bins <- wellsite_data_subset_proj[, .(
    Well.Count = sum(Well.Count.Site),
    Gas.Well.Count = sum(Gas.Well.Count.Site), 
    Oil.Well.Count = sum(Oil.Well.Count.Site),
    Wellsite.Count = sum(Wellsite.Count),
    Modify.Date = max(Modify.Date),
    Gas.Mcf = sum(Gas.Mcf.Site),
    Oil.Bbl = sum(Oil.Bbl.Site),
    Cond.Bbl = sum(Cond.Bbl.Site)
  ), by = .(Year, State, Type, Vintage.Bin, ProdRate.Bin, Prod.Class, 
            ProdRate.Bin.Prev, Init.ProdRate.Bin, Site.Type.Bin)]
  
  wellsite_st_bins[, `:=` (Wells.per.site = Well.Count / Wellsite.Count, 
                           Gas.Wells.per.site = Gas.Well.Count / Wellsite.Count, 
                           Oil.Wells.per.site = Oil.Well.Count / Wellsite.Count)]
  
  # Among the data subset with well IDs, calculate national average proportions
  # aggregate to all bins (except State)
  wellsite_ntnl_bins <- wellsite_data_subset_proj[, .(
      Well.Count = sum(Well.Count.Site),
      Wellsite.Count = sum(Wellsite.Count),
      Gas.Mcf = sum(Gas.Mcf.Site),
      Oil.Bbl = sum(Oil.Bbl.Site),
      Cond.Bbl = sum(Cond.Bbl.Site)
    ), by = .(Year, Type, Vintage.Bin, ProdRate.Bin, Site.Type.Bin)]
  
  wellsite_ntnl_bins[, Wells.per.site := Well.Count / Wellsite.Count]
  
  # Calculate proportions to use for data without well pad info
  # Conditional on (Type, Vintage.Bin, ProdRate.Bin) -> (Proportion on single/multi, wells/gas/liquids per pad)
  wellsite_ntnl_bins_split <- wellsite_ntnl_bins[Year == init.yr, .(
    Site.Type.Bin, # prevent aggregation
    Prop.Well.Count = Well.Count / sum(Well.Count),
    Prop.Wellsite.Count = Wellsite.Count / sum(Wellsite.Count), # TODO: maybe unneeded
    Wells.per.site,
    Prop.Gas.Mcf = Gas.Mcf / sum(Gas.Mcf),
    Prop.Oil.Bbl = Oil.Bbl / sum(Oil.Bbl)
  ), by = .(Type, Vintage.Bin, ProdRate.Bin)]
  
  
  # Pathway for data *without* well pad info:
  nowellsite_st_bins <- nowellsite_data_subset_proj[, .(
    Well.Count = sum(Well.Count),
    Modify.Date = max(Modify.Date),
    Gas.Mcf = sum(Gas.Mcf.Entity),
    Oil.Bbl = sum(Oil.Bbl.Entity),
    Cond.Bbl = sum(Cond.Bbl.Entity)
  ), by = .(Year, State, Type, Vintage.Bin, ProdRate.Bin, Prod.Class, ProdRate.Bin.Prev, Init.ProdRate.Bin)]
  
  nowellsite_st_bins_split = merge.data.table(nowellsite_st_bins, wellsite_ntnl_bins_split, 
                                              by.x = c("Type", "Vintage.Bin", "Init.ProdRate.Bin"),
                                              by.y = c("Type", "Vintage.Bin", "ProdRate.Bin"), all.x = T, allow.cartesian = T)
  nowellsite_st_bins_split[, Well.Count := Well.Count * Prop.Well.Count]
  nowellsite_st_bins_split[, Gas.Well.Count := (Type == "Gas")*Well.Count]
  nowellsite_st_bins_split[, Oil.Well.Count := (Type == "Oil")*Well.Count]
  nowellsite_st_bins_split[, Gas.Wells.per.site := (Type == "Gas")*Wells.per.site]
  nowellsite_st_bins_split[, Oil.Wells.per.site := (Type == "Oil")*Wells.per.site]
  nowellsite_st_bins_split[, Gas.Mcf := Gas.Mcf * Prop.Gas.Mcf]
  nowellsite_st_bins_split[, Oil.Bbl := Oil.Bbl * Prop.Oil.Bbl]
  nowellsite_st_bins_split[, Cond.Bbl := Cond.Bbl * Prop.Oil.Bbl]
  nowellsite_st_bins_split[, c("Prop.Well.Count", "Prop.Wellsite.Count", "Prop.Gas.Mcf", "Prop.Oil.Bbl") := NULL]
  nowellsite_st_bins_split[, Wellsite.Count := Well.Count / Wells.per.site]
  
  
  stopifnot(
    all(names(nowellsite_st_bins_split) %in% names(wellsite_st_bins)) &
      all(names(wellsite_st_bins) %in% names(nowellsite_st_bins_split))
  )
  
  wellsite_st_bins <- rbindlist(list(
    wellsite_st_bins,
    nowellsite_st_bins_split
  ), use.names = TRUE)
  
  # combine overlapping entries from well site and no-well site data subsets
  wellsite_st_bins <- wellsite_st_bins[Well.Count > 0, .(
    Well.Count = sum(Well.Count),
    Gas.Well.Count = sum(Gas.Well.Count), 
    Oil.Well.Count = sum(Oil.Well.Count),
    Wellsite.Count = sum(Wellsite.Count),
    Modify.Date = max(Modify.Date),
    Gas.Mcf = sum(Gas.Mcf),
    Oil.Bbl = sum(Oil.Bbl),
    Cond.Bbl = sum(Cond.Bbl)
  ), by = .(Year, State, Type, Vintage.Bin, ProdRate.Bin, Prod.Class, 
            ProdRate.Bin.Prev, Init.ProdRate.Bin, Site.Type.Bin)
  ][, `:=` (Wells.per.site = Well.Count / Wellsite.Count, 
            Gas.Wells.per.site = Gas.Well.Count / Wellsite.Count, 
            Oil.Wells.per.site = Oil.Well.Count / Wellsite.Count)
  ]
  
  prop_diff <- function(x, y) abs(x - y)/y
  
  # post-checks: total wells, gas, and oil production unchanged
  stopifnot(prop_diff(wellsite_st_bins[Year == init.yr, sum(Well.Count)], well_prod_dta[, sum(Well.Count)]) < .001)
  stopifnot(prop_diff(wellsite_st_bins[Year == init.yr, sum(Gas.Mcf)], well_prod_dta[, sum(Gas.Mcf.Entity)]) < .001)
  stopifnot(prop_diff(wellsite_st_bins[Year == init.yr, sum(Oil.Bbl)], well_prod_dta[, sum(Oil.Bbl.Entity)]) < .001)
  
  wellsite_st_bins
}

# Summarize baseyear wellsite information from Enverus into blocks of wellsites
wellsite_binning_and_projection <- function(
  well_prod_dta, 
  aeo_well_proj, 
  decl_rates, 
  new_well_proxy_yr = 2018
) { 
  
  # assign base year dataset to new data table
  well_prod_base_dta = well_prod_dta
  
  # classify wells by vintage bins
  # create factor of bins, and check none are missing
  well_prod_base_dta[, Vintage.Bin := case_when(
    Modify.Date >  as.IDate("2015-09-18") ~ "2016-2019",
    Modify.Date >= as.IDate("2011-08-24") ~ "2012-2015",
    Modify.Date >= as.IDate("2000-01-01") ~ "2000-2011", # extra breakout
    TRUE ~ "Pre-2000")]
  well_prod_base_dta[, Vintage.Bin := factor(
    Vintage.Bin, levels = c("Pre-2000", "2000-2011", "2012-2015", "2016-2019"))]
  stopifnot(! anyNA(well_prod_dta$Vintage.Bin))
  
  # convert base year well sites to bins of well sites
  wellsite_bins_base_proj = convert_wellsite_data_to_bins(well_prod_base_dta, decl_rates, base.yr)
  
  # subset out data for recent vintage sites
  well_prod_proj_dta = well_prod_dta[year(Modify.Date) %in% c(new_well_proxy_yr)]
  well_prod_proj_dta[, Vintage.Bin := NA_integer_]
  
  # convert recent vintage well sites to bins of well sites
  wellsite_bins_new_tmpl_proj = convert_wellsite_data_to_bins(well_prod_proj_dta, decl_rates, 0)
  
  # initialize data table to hold projections of wells constructed after the base year
  wellsite_bins_new_proj = data.table()
  
  # construct projections for well sites constructed after the base year
  for (i in (base.yr + 1):end.yr){
    
    aeo.wells = aeo_well_proj[Year == i, Wells.drilled]
    temp = wellsite_bins_new_tmpl_proj
    temp[, Vintage.Bin := as.integer(Vintage.Bin)]
    temp[, `:=` (Vintage.Bin = i, Start.Year = i, Modify.Date = as.IDate(ISOdate(i, 1, 1)))]
    temp = temp[Start.Year + Year <= end.yr]
    temp[, Well.Count.Ttl := sum(Well.Count), by = .(Year)]
    
    temp[, `:=` (Well.Count = aeo.wells/Well.Count.Ttl*Well.Count, 
                 Gas.Well.Count = aeo.wells/Well.Count.Ttl*Gas.Well.Count,
                 Oil.Well.Count = aeo.wells/Well.Count.Ttl*Oil.Well.Count,
                 Wellsite.Count = aeo.wells/Well.Count.Ttl*Wellsite.Count,
                 Gas.Mcf = aeo.wells/Well.Count.Ttl*Gas.Mcf,
                 Oil.Bbl = aeo.wells/Well.Count.Ttl*Oil.Bbl,
                 Cond.Bbl = aeo.wells/Well.Count.Ttl*Cond.Bbl)]
    
    # stitch new year of data to existing data table
    wellsite_bins_new_proj = rbindlist(list(wellsite_bins_new_proj, temp))
    
  }
  
  # assign year
  wellsite_bins_new_proj[, Year := Start.Year + Year]
  
  # remove unneeded columns
  wellsite_bins_new_proj[, c("Start.Year", "Well.Count.Ttl") := NULL]
  
  # combine base year well site bin projections with post-base year projections
  wellsite_bins_proj = rbindlist(list(wellsite_bins_base_proj, wellsite_bins_new_proj), use.names = T)
  
  # merge retirement percentages
  retr.pctg = data.table(Type = c(rep("Gas", 4), rep("Oil", 4)),
                         ProdRate.Bin = rep(c(">100 BOE/day/well", "15-100 BOE/day/well", "3-15 BOE/day/well", "<=3 BOE/day/well"), 2),
                         Retire.Frac = c(0, 0, 0.01, 0.044, 0, 0, 0.016, 0.067))
  
  wellsite_bins_proj = merge.data.table(wellsite_bins_proj, retr.pctg,
                                        by.x = c("Type", "ProdRate.Bin.Prev"), by.y = c("Type", "ProdRate.Bin"), all.x = T)[
    order(State, Type, Vintage.Bin, Site.Type.Bin, Init.ProdRate.Bin, Year)
  ]

  # restrict retirement to sites that are at least five years old
  wellsite_bins_proj[, Retire.Frac := fifelse(is.na(Retire.Frac) | (Year < (year(Modify.Date) + 5) & Year > base.yr), 0, Retire.Frac)]
  
  # initialize data tables
  temp_build = wellsite_bins_proj[Year == base.yr
  ][, 
    `:=` (ProdRate.Bin.Prev = NULL,
          Oil.Bbl.Site.Start = Oil.Bbl/Wellsite.Count, 
          Oil.Bbl.Site.EG = NA_real_, 
          Oil.Bbl.Site.NSPS = NA_real_,
          Cond.Bbl.Site.Start = Cond.Bbl/Wellsite.Count, 
          Cond.Bbl.Site.EG = NA_real_, 
          Cond.Bbl.Site.NSPS = NA_real_
    )
  ]
  setnames(wellsite_bins_proj, "Well.Count", "Well.Count.Old")
  
  # apply retirement percentages recursively
  for (i in (base.yr + 1):end.yr){
    
    # create temporary data table to perform calculations in
    temp0 = temp_build[Year == (i-1), .(Type, State, Vintage.Bin, Site.Type.Bin, 
                                        Init.ProdRate.Bin, ProdRate.Bin, Well.Count.New = Well.Count,
                                        Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS, 
                                        Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS)]
    temp1 = wellsite_bins_proj[Year %in% i & !(Vintage.Bin == i)]
    temp2 = wellsite_bins_proj[Year %in% i & Vintage.Bin == i][
      , `:=` (Well.Count = Well.Count.Old, 
              Oil.Bbl.Site.Start = Oil.Bbl/Wellsite.Count, 
              Oil.Bbl.Site.EG = fifelse(Vintage.Bin == eg.start.yr, Oil.Bbl/Wellsite.Count, NA_real_), 
              Oil.Bbl.Site.NSPS = fifelse(Vintage.Bin == nsps.start.yr, Oil.Bbl/Wellsite.Count, NA_real_),
              Cond.Bbl.Site.Start = Cond.Bbl/Wellsite.Count, 
              Cond.Bbl.Site.EG = fifelse(Vintage.Bin == eg.start.yr, Cond.Bbl/Wellsite.Count, NA_real_), 
              Cond.Bbl.Site.NSPS = fifelse(Vintage.Bin == nsps.start.yr, Cond.Bbl/Wellsite.Count, NA_real_)
              )
    ][
      ,
      c("Well.Count.Old", "ProdRate.Bin.Prev", "Retire.Frac") := NULL
    ]
    
    temp1 = merge.data.table(temp1, temp0, 
                             by.x = c("Type", "State", "Vintage.Bin", "Site.Type.Bin", "Init.ProdRate.Bin", "ProdRate.Bin.Prev"),
                             by.y = c("Type", "State", "Vintage.Bin", "Site.Type.Bin", "Init.ProdRate.Bin", "ProdRate.Bin"))
    
    temp1[, Well.Count.Old.Comb := sum(Well.Count.Old), by = .(Type, State, Vintage.Bin, Site.Type.Bin, 
                                                               Init.ProdRate.Bin, ProdRate.Bin.Prev,
                                                               Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS, 
                                                               Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS)]
    
    cols = c("Well.Count.Old", "Gas.Well.Count", "Oil.Well.Count", "Wellsite.Count", "Gas.Mcf", "Oil.Bbl", "Cond.Bbl")
    temp1[, (cols) := lapply(.SD, "*", (1 - Retire.Frac)*(Well.Count.New/Well.Count.Old.Comb)), .SDcols = cols][, Well.Count := Well.Count.Old]
    temp1[, c("Well.Count.Old", "Well.Count.New","Well.Count.Old.Comb") := NULL]
    temp1[, `:=` (
      Oil.Bbl.Site.EG = fifelse(Year == eg.start.yr, Oil.Bbl/Wellsite.Count, Oil.Bbl.Site.EG), 
      Oil.Bbl.Site.NSPS = fifelse(Year == nsps.start.yr, Oil.Bbl/Wellsite.Count, Oil.Bbl.Site.NSPS),
      Cond.Bbl.Site.EG = fifelse(Year == eg.start.yr, Cond.Bbl/Wellsite.Count, Cond.Bbl.Site.EG), 
      Cond.Bbl.Site.NSPS = fifelse(Year == nsps.start.yr, Cond.Bbl/Wellsite.Count, Cond.Bbl.Site.NSPS)
    )
    ]
    
    temp1 = temp1[,.(
      Well.Count = sum(Well.Count),
      Gas.Well.Count = sum(Gas.Well.Count), 
      Oil.Well.Count = sum(Oil.Well.Count),
      Wellsite.Count = sum(Wellsite.Count),
      Modify.Date = max(Modify.Date),
      Gas.Mcf = sum(Gas.Mcf),
      Oil.Bbl = sum(Oil.Bbl),
      Cond.Bbl = sum(Cond.Bbl)
    ), by = .(Year, State, Type, Vintage.Bin, ProdRate.Bin, Prod.Class, 
              Init.ProdRate.Bin, Site.Type.Bin,
              Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS, 
              Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS)
    ][, `:=` (Wells.per.site = Well.Count / Wellsite.Count,
              Gas.Wells.per.site = Gas.Well.Count / Wellsite.Count,
              Oil.Wells.per.site = Oil.Well.Count / Wellsite.Count)]
    
    temp_build = rbindlist(list(temp_build, temp1, temp2), use.names = T, fill = T)
  }
  
  temp_build[, Retire.Frac := NULL]
  
  wellsite_bins_proj = temp_build
  
  wellsite_bins_proj
  
}  
  
# Merge wellsite bins with ICR data
wellsite_equip_chars_proj <- function(
  wellsite_bins_proj, 
  icr_equip_prop, 
  icr_equip_avg, 
  ghgi_equip_data
) {
  
  # restrict GHGI equipment data to production segment only
  ghgi_equip_data = ghgi_equip_data[Segment == "PROD"][, `:=` (Segment = NULL, Count = NULL)]
  
  # adjust equipment bin proportions to account for heater-treaters and process heaters that result
  # in sites with one piece of major equipment based on ICR data having more than one;
  # see "Summary Stats" tab in "data-raw/api_survey_data.xlsx".
  icr_equip_prop[, equip_bin_new := equip_bin]
  temp = icr_equip_prop[grepl("Equipment = 1", equip_bin)
  ][, equip_bin_new := gsub("=", ">", equip_bin_new)
  ][, equip_bin_new := gsub("3", "4", equip_bin_new)
  ][, equip_bin_new := gsub("5", "6", equip_bin_new)
  ][, prop := prop*(1 - .33*(site_type == "Oil") - .6*(site_type == "Gas"))]
  icr_equip_prop[, prop := prop*(1 - .67*(grepl("3|5", equip_bin) & site_type == "Oil" ) - .4*(grepl("3|5", equip_bin) & site_type == "Gas"))]
  icr_equip_prop = rbindlist(list(icr_equip_prop, temp))
  
  # define initial production class
  wellsite_bins_proj[, Init.Prod.Class := fifelse(Init.ProdRate.Bin %in% c("<=3 BOE/day/well", "3-15 BOE/day/well"), 
                                                  "Low Production", "Non-low Production")]
  
  # merge wellsite bin data with equipment bin proportions
  wellsite_bins_proj = merge.data.table(wellsite_bins_proj, icr_equip_prop,
                                        by.x = c("Type", "Init.Prod.Class", "Site.Type.Bin"), 
                                        by.y = c("site_type", "site_prod_lvl", "site_bin"),
                                        all.x = T, allow.cartesian = T)
  
  # apply equipment bin proportions
  cols = c("Well.Count", "Gas.Well.Count", "Oil.Well.Count", "Wellsite.Count", "Gas.Mcf", "Oil.Bbl", "Cond.Bbl")
  wellsite_bins_proj[, (cols) := lapply(.SD, "*", prop), .SDcols = cols]
  
  # subset base year
  wellsite_bins_base = wellsite_bins_proj[Year == base.yr]
  
  # determine per-well equipment factors based on calibrating base-year data to GHGI
  wellsite_equip_factors = wellsite_equip_prop_base(wellsite_bins_base, icr_equip_avg, ghgi_equip_data)
  
  # sum over "new" equipment bin
  wellsite_bins_proj = wellsite_bins_proj[, lapply(.SD, sum), 
                                          by = .(Type, Prod.Class, Site.Type.Bin, Year, State,
                                                 Vintage.Bin, ProdRate.Bin, Init.ProdRate.Bin, Modify.Date, 
                                                 Wells.per.site, Gas.Wells.per.site, Oil.Wells.per.site,
                                                 Oil.Bbl.Site.Start, Oil.Bbl.Site.EG, Oil.Bbl.Site.NSPS, 
                                                 Cond.Bbl.Site.Start, Cond.Bbl.Site.EG, Cond.Bbl.Site.NSPS,
                                                 equip_bin = equip_bin_new),
                                          .SDcols = cols]
  
  # merge equipment factors with projection table
  wellsite_bins_proj = merge.data.table(wellsite_bins_proj, wellsite_equip_factors,
                                        by.x = c("Init.ProdRate.Bin", "Site.Type.Bin", "equip_bin"),
                                        by.y = c("ProdRate.Bin", "Site.Type.Bin", "equip_bin"))
  
  wellsite_bins_proj
  
}

wellsite_equip_prop_base <- function(
  wellsite_bins_base, 
  icr_equip_avg, 
  ghgi_equip_data
) {
  
  # merge with equipment info
  wellsite_equip_baseyear = merge.data.table(wellsite_bins_base[, .(Prod.Class, Site.Type.Bin, equip_bin, equip_bin_new, State, 
                                                                    Type, Vintage.Bin, ProdRate.Bin, Gas.Well.Count, Oil.Well.Count)], 
                                             icr_equip_avg, 
                                             by.x = c("Prod.Class", "Site.Type.Bin", "equip_bin"), 
                                             by.y = c("site_prod_lvl", "site_bin", "equip_bin"),
                                             all.x = T)
  
  # add equipment averages for heater-treaters and process heaters;
  # see "Summary Stats" tab in "data-raw/api_survey_data.xlsx".
  wellsite_equip_baseyear[, `:=` (
    heat_well_gas_api = 1*(grepl("3|5", equip_bin) & grepl("4|6", equip_bin_new) & Site.Type.Bin == "Single") +
      1*(grepl("3|5", equip_bin) & grepl("4|6", equip_bin_new) & Site.Type.Bin == "Multi") +
      0.28*(grepl("4|6", equip_bin) & Site.Type.Bin == "Single") +
      0.02*(grepl("4|6", equip_bin) & Site.Type.Bin == "Multi"),
    heat_well_oil_api = 0, 
    heat_treat_well_gas_api = 0,
    heat_treat_well_oil_api = 1.08*(grepl("3|5", equip_bin) & grepl("4|6", equip_bin_new) & Site.Type.Bin == "Single") +
      0.73*(grepl("3|5", equip_bin) & grepl("4|6", equip_bin_new) & Site.Type.Bin == "Multi") +
      0.88*(grepl("4|6", equip_bin) & Site.Type.Bin == "Single") +
      0.54*(grepl("4|6", equip_bin) & Site.Type.Bin == "Multi")
  )]
  
  # Combine new equipment bins to account for bin switches induced by heaters/heater-treaters
  wellsite_equip_baseyear = wellsite_equip_baseyear[, .(wells_gas_temp = sum(Gas.Well.Count),
                                                       wells_oil_temp = sum(Oil.Well.Count),
                                                       sep_gas_temp = sum(Gas.Well.Count*sep_well_gas_icr), 
                                                       sep_oil_temp = sum(Oil.Well.Count*sep_well_oil_icr),
                                                       deh_gas_temp = sum(Gas.Well.Count*deh_well_gas_icr), 
                                                       deh_oil_temp = sum(Oil.Well.Count*deh_well_oil_icr),
                                                       compr_gas_temp = sum(Gas.Well.Count*(recip_compr_well_gas_icr+cent_compr_well_gas_icr)), 
                                                       compr_oil_temp = sum(Oil.Well.Count*(recip_compr_well_oil_icr+cent_compr_well_oil_icr)),
                                                       heat_gas_temp = sum(Gas.Well.Count*heat_well_gas_api), 
                                                       heat_oil_temp = sum(Oil.Well.Count*heat_well_oil_api),
                                                       heat_treat_gas_temp = sum(Gas.Well.Count*heat_treat_well_gas_api), 
                                                       heat_treat_oil_temp = sum(Oil.Well.Count*heat_treat_well_oil_api),
                                                       tanks_gas_temp = sum(Gas.Well.Count*(small_tanks_well_gas_icr + large_tanks_well_gas_icr)), 
                                                       tanks_oil_temp = sum(Oil.Well.Count*(small_tanks_well_oil_icr + large_tanks_well_oil_icr))), 
                                                   by = .(Site.Type.Bin, ProdRate.Bin, equip_bin_new)]
  
  # Calculate new equipment/well factors
  wellsite_equip_factors = wellsite_equip_baseyear[, .(Site.Type.Bin, ProdRate.Bin, equip_bin = equip_bin_new, 
                                                       wells_gas_temp, wells_oil_temp,
                                                       sep_well_gas_temp = sep_gas_temp/wells_gas_temp,
                                                       sep_well_oil_temp = sep_oil_temp/wells_oil_temp,
                                                       deh_well_gas_temp = deh_gas_temp/wells_gas_temp,
                                                       deh_well_oil_temp = deh_oil_temp/wells_oil_temp,
                                                       compr_well_gas_temp = compr_gas_temp/wells_gas_temp,
                                                       compr_well_oil_temp = compr_oil_temp/wells_oil_temp,
                                                       heat_well_gas_temp = heat_gas_temp/wells_gas_temp,
                                                       heat_well_oil_temp = heat_oil_temp/wells_oil_temp,
                                                       heat_treat_well_gas_temp = heat_treat_gas_temp/wells_gas_temp,
                                                       heat_treat_well_oil_temp = heat_treat_oil_temp/wells_oil_temp,
                                                       tanks_well_gas = tanks_gas_temp/wells_gas_temp,
                                                       tanks_well_oil = tanks_oil_temp/wells_oil_temp)]
  
  # Calculate major equipment per site 
  # step 1: calculate aggregate equipment totals implied by ICR & Enverus
  wellsite_equip_factors[, `:=` (wells_total_gas = sum(wells_gas_temp),
                                 wells_total_oil = sum(wells_oil_temp),
                                 wells_equip_total_gas = sum(wells_gas_temp*grepl("[3-6]", equip_bin)),
                                 wells_equip_total_oil = sum(wells_oil_temp*grepl("[3-6]", equip_bin)),
                                 sep_total_gas = sum(wells_gas_temp*sep_well_gas_temp), 
                                 sep_total_oil = sum(wells_oil_temp*sep_well_oil_temp),
                                 deh_total_gas = sum(wells_gas_temp*deh_well_gas_temp), 
                                 deh_total_oil = sum(wells_oil_temp*deh_well_oil_temp),
                                 compr_total_gas = sum(wells_gas_temp*compr_well_gas_temp), 
                                 compr_total_oil = sum(wells_oil_temp*compr_well_oil_temp),
                                 heat_total_gas = sum(wells_gas_temp*heat_well_gas_temp), 
                                 heat_total_oil = sum(wells_oil_temp*heat_well_oil_temp),
                                 heat_treat_total_gas = sum(wells_gas_temp*heat_treat_well_gas_temp), 
                                 heat_treat_total_oil = sum(wells_oil_temp*heat_treat_well_oil_temp))]
  
  # step 3: adjust ICR major equipment factors to match GHGI factors in aggregate
  wellsite_equip_factors[, `:=` (sep_well_gas = (ghgi_equip_data[Source == "Separators" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                   sep_total_gas)*sep_well_gas_temp,
                                 sep_well_oil = (ghgi_equip_data[Source == "Separators" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                   sep_total_oil)*sep_well_oil_temp,
                                 deh_well_gas = (ghgi_equip_data[Source == "Dehydrators" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                   deh_total_gas)*deh_well_gas_temp,
                                 deh_well_oil = (ghgi_equip_data[Source == "Dehydrators" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                   deh_total_oil)*deh_well_oil_temp,
                                 compr_well_gas = (ghgi_equip_data[Source == "Compressors" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                     compr_total_gas)*compr_well_gas_temp,
                                 compr_well_oil = (ghgi_equip_data[Source == "Compressors" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                     compr_total_oil)*compr_well_oil_temp,
                                 heat_well_gas = (ghgi_equip_data[Source == "Heaters" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                    heat_total_gas)*heat_well_gas_temp,
                                 heat_well_oil = (ghgi_equip_data[Source == "Heaters" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                    pmax(heat_total_oil,1))*heat_well_oil_temp,
                                 heat_treat_well_gas = (ghgi_equip_data[Source == "Heater/Treaters" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                          heat_total_gas)*heat_well_gas_temp,
                                 heat_treat_well_oil = (ghgi_equip_data[Source == "Heater/Treaters" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                          pmax(heat_total_oil,1))*heat_well_oil_temp,
                                 head_well_gas = fifelse(grepl("[3-6]", equip_bin), 
                                                         (wells_total_gas/wells_equip_total_gas)*
                                                           ghgi_equip_data[Source == "Headers" & Type == "Gas", Equip_Per_Well], 0),
                                 head_well_oil = fifelse(grepl("[3-6]", equip_bin), 
                                                         (wells_total_oil/wells_equip_total_oil)*
                                                           ghgi_equip_data[Source == "Headers" & Type == "Oil", Equip_Per_Well], 0),
                                 meters_well_gas = ghgi_equip_data[Source == "Meters/Piping" & Type == "Gas", Equip_Per_Well],
                                 meters_well_oil = ghgi_equip_data[Source == "Meters/Piping" & Type == "Oil", Equip_Per_Well])
  ]
  
  # export subset of columns
  wellsite_equip_factors =
    wellsite_equip_factors[, .(Site.Type.Bin, ProdRate.Bin, equip_bin, sep_well_gas, sep_well_oil, 
                               deh_well_gas, deh_well_oil, compr_well_gas, compr_well_oil, 
                               heat_well_gas, heat_well_oil, heat_treat_well_gas, heat_treat_well_oil, 
                               head_well_gas, head_well_oil, meters_well_gas, meters_well_oil,
                               tanks_well_gas, tanks_well_oil)
    ]
  
}

# Given wellsite equipment, return pneumatic controllers
wellsite_pneumatics_proj <- function(
  wellsite_bins_proj, 
  ghgi_equip_data
) {
  
  # restrict GHGI equipment data to production segment only
  ghgi_equip_data = ghgi_equip_data[Segment == "PROD"][, `:=` (Segment = NULL, Count = NULL)]
  
  # Identify blocks as producing liquids; separators at sites with liquids production
  # are assumed to have more controllers
  wellsite_bins_proj[, Has_Liq_Prod := fifelse(Oil.Bbl > 0, TRUE, FALSE)]

  # Identify blocks as having high bleed controllers or not (depends on age and location)
  wellsite_bins_proj[, Has_HB_PnC := fifelse(Modify.Date < as.IDate("2011-08-24") & !(State %in% c("CA", "CO", "UT")), TRUE, FALSE)]

  # subset base year
  wellsite_bins_base = wellsite_bins_proj[Year == base.yr]

  # determine per-well equipment factors based on calibrating base-year data to GHGI
  wellsite_pnc_factors = wellsite_pneumatics_equip_factors(wellsite_bins_base, ghgi_equip_data)

  # merge pneumatic factors with projection table
  wellsite_bins_proj = merge.data.table(wellsite_bins_proj, wellsite_pnc_factors,
                                        by = c("Site.Type.Bin", "ProdRate.Bin", "equip_bin", "Has_HB_PnC", "Has_Liq_Prod"))

  # calculate number of controllers per site
  wellsite_bins_proj[, `:=` (PnC_per_site_lb = PnC_well_gas_lb*Gas.Wells.per.site+PnC_well_oil_lb*Oil.Wells.per.site,
                             PnC_per_site_ib = PnC_well_gas_ib*Gas.Wells.per.site+PnC_well_oil_ib*Oil.Wells.per.site,
                             PnC_per_site_hb = PnC_well_gas_hb*Gas.Wells.per.site+PnC_well_oil_hb*Oil.Wells.per.site)]
  
  # split blocks into sites w/ and w/out pumps
  wellsite.pump.frac.gas = .3 # see "Summary Stats" tab in "data-raw/allen_et_al_2013_SI.xlsx"
  wellsite.pump.frac.oil = .25 # see "Summary Stats" tab in "data-raw/api_survey_data.xlsx"
  temp.col = c("Well.Count", "Gas.Well.Count", "Oil.Well.Count", "Wellsite.Count", "Gas.Mcf", "Oil.Bbl", "Cond.Bbl")
  wellsite_bins_proj = CJ.table(wellsite_bins_proj, data.table(Has_Pumps = c(TRUE, FALSE)))
  
  # remove blocks that don't have equipment but have pumps
  wellsite_bins_proj = wellsite_bins_proj[!Has_Pumps | (Has_Pumps & grepl("[3-6]", equip_bin))]
  wellsite_bins_proj[, (temp.col) := lapply(.SD, function(x){x*fcase(Type == "Gas" & Has_Pumps & grepl("[3-6]", equip_bin), wellsite.pump.frac.gas, 
                                                                     Type == "Gas" & !Has_Pumps & grepl("[3-6]", equip_bin), 1-wellsite.pump.frac.gas,
                                                                     Type == "Oil" & Has_Pumps & grepl("[3-6]", equip_bin), wellsite.pump.frac.oil, 
                                                                     Type == "Oil" & !Has_Pumps & grepl("[3-6]", equip_bin), 1-wellsite.pump.frac.oil,
                                                                     default = 1)}), .SDcols = temp.col]
  
  # allocate pumps in proportion to number of controllers
  pumps.per.pnc.gas = ghgi_equip_data[Source == "Pumps" & Type == "Gas", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Gas", Equip_Per_Well]
  pumps.per.pnc.oil = ghgi_equip_data[Source == "Pumps" & Type == "Oil", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Oil", Equip_Per_Well]
  
  # calculate additional pumps per PnC factor to append to base pump allocation
  Pumps_PnC_addn = wellsite_bins_proj[Year == 2019, sum(pumps.per.pnc.gas*(PnC_well_gas_lb+PnC_well_gas_ib+PnC_well_gas_hb)*Gas.Well.Count +
                                                          pumps.per.pnc.oil*(PnC_well_oil_lb+PnC_well_oil_ib+PnC_well_oil_hb)*Oil.Well.Count - 
                                                          (Has_Pumps)*Wellsite.Count)]/
    wellsite_bins_proj[Year == 2019, sum((Has_Pumps)*Wellsite.Count*(PnC_per_site_lb+PnC_per_site_ib+PnC_per_site_hb))]
  
  # calculate pumps per site for wellsite bin projections
  wellsite_bins_proj[, Pumps_per_site := fifelse(Has_Pumps, 1+Pumps_PnC_addn*(PnC_per_site_lb+PnC_per_site_ib+PnC_per_site_hb), 0)]
  
  wellsite_bins_proj
}

wellsite_pneumatics_equip_factors <- function(
  wellsite_bins_base, ghgi_equip_data
) {
  
  wellsite_pnc_baseyear = wellsite_bins_base
  
  # Calculate pneumatic controllers at sites
  # step 1: apply PnC per major equipment factors from literature (based on data from Allen et al. (2015))
  wellsite_pnc_baseyear[, `:=` (PnC_well_gas = 0.42 + 1.5*heat_well_gas + (1+1*(Has_Liq_Prod))*sep_well_gas + 2.5*deh_well_gas + 
                                  4.3*compr_well_gas,
                                PnC_well_oil = 0.42 + 2*sep_well_oil + 4.3*compr_well_oil)]
  
  # step 2: calculate total controllers implied by factors
  wellsite_pnc_baseyear[, `:=` (wells_total_gas = sum(Wellsite.Count*Gas.Wells.per.site), 
                                wells_total_oil = sum(Wellsite.Count*Oil.Wells.per.site),
                                PnC_total_gas = sum(Wellsite.Count*Gas.Wells.per.site*PnC_well_gas), 
                                PnC_total_oil = sum(Wellsite.Count*Oil.Wells.per.site*PnC_well_oil))]
  
  # step 3: adjust PnC factors to match GHGI factors in aggregate
  wellsite_pnc_baseyear[, `:=` (PnC_well_gas = (ghgi_equip_data[Source == "Controllers" & Type == "Gas", Equip_Per_Well]*wells_total_gas/
                                                  PnC_total_gas)*PnC_well_gas,
                                PnC_well_oil = (ghgi_equip_data[Source == "Controllers" & Type == "Oil", Equip_Per_Well]*wells_total_oil/
                                                  PnC_total_oil)*PnC_well_oil)]
  
  
  # step 4: apply site level well counts and sum across oil/gas
  wellsite_pnc_baseyear[, `:=` (PnC_per_site_gas = Gas.Wells.per.site*PnC_well_gas,
                                PnC_per_site_oil = Oil.Wells.per.site*PnC_well_oil,
                                PnC_per_site = Gas.Wells.per.site*PnC_well_gas + Oil.Wells.per.site*PnC_well_oil)]
  
  # split controllers into types
  # step 1: calculate GHGI fractions for controller types
  prop_lb_gas = ghgi_equip_data[Source == "LB Controllers" & Type == "Gas", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Gas", Equip_Per_Well]
  prop_ib_gas = ghgi_equip_data[Source == "IB Controllers" & Type == "Gas", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Gas", Equip_Per_Well]
  prop_hb_gas = ghgi_equip_data[Source == "HB Controllers" & Type == "Gas", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Gas", Equip_Per_Well]
  prop_lb_oil = ghgi_equip_data[Source == "LB Controllers" & Type == "Oil", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Oil", Equip_Per_Well]
  prop_ib_oil = ghgi_equip_data[Source == "IB Controllers" & Type == "Oil", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Oil", Equip_Per_Well]
  prop_hb_oil = ghgi_equip_data[Source == "HB Controllers" & Type == "Oil", Equip_Per_Well]/
    ghgi_equip_data[Source == "Controllers" & Type == "Oil", Equip_Per_Well]
  
  # step 2: calculate total controllers and totals within the HB PnC bins
  wellsite_pnc_baseyear[, `:=` (PnC_total_gas = sum(Wellsite.Count*PnC_per_site_gas), 
                                PnC_total_oil = sum(Wellsite.Count*PnC_per_site_oil)),
  ][, `:=` (PnC_total_gas_HB_status = sum(Wellsite.Count*PnC_per_site_gas), 
            PnC_total_oil_HB_status = sum(Wellsite.Count*PnC_per_site_oil)), by = .(Has_HB_PnC)]
  
  # step 3: calculate per-well factors of each type of controller within each block
  wellsite_pnc_baseyear[, `:=` (PnC_well_gas_lb = PnC_well_gas*(prop_lb_gas/(prop_lb_gas+prop_ib_gas))*
                                  (1 - (Has_HB_PnC)*prop_hb_gas*PnC_total_gas/PnC_total_gas_HB_status),
                                PnC_well_oil_lb = PnC_well_oil*(prop_lb_oil/(prop_lb_oil+prop_ib_oil))*
                                  (1 - (Has_HB_PnC)*prop_hb_oil*PnC_total_oil/PnC_total_oil_HB_status),
                                PnC_well_gas_ib = PnC_well_gas*(prop_ib_gas/(prop_lb_gas+prop_ib_gas))*
                                  (1 - (Has_HB_PnC)*prop_hb_gas*PnC_total_gas/PnC_total_gas_HB_status),
                                PnC_well_oil_ib = PnC_well_oil*(prop_ib_oil/(prop_lb_oil+prop_ib_oil))*
                                  (1 - (Has_HB_PnC)*prop_hb_oil*PnC_total_oil/PnC_total_oil_HB_status),
                                PnC_well_gas_hb = PnC_well_gas*(Has_HB_PnC)*prop_hb_gas*PnC_total_gas/PnC_total_gas_HB_status,
                                PnC_well_oil_hb = PnC_well_oil*(Has_HB_PnC)*prop_hb_oil*PnC_total_oil/PnC_total_oil_HB_status)
  ]
  
  # find the unique entries
  wellsite_pnc_factors = unique(wellsite_pnc_baseyear[, .(Site.Type.Bin, ProdRate.Bin, equip_bin, Has_HB_PnC, Has_Liq_Prod,
                                                          PnC_well_gas_lb, PnC_well_oil_lb,
                                                          PnC_well_gas_ib, PnC_well_oil_ib,
                                                          PnC_well_gas_hb, PnC_well_oil_hb)])
  
  # export
  wellsite_pnc_factors
  
}

# Given wellsite characteristics, return tank battery PTE characteristics
wellsite_tank_characteristics <- function(
  wellsite_bins_proj
) {
  
  # fill tank information
  wellsite_tank_st_bins <- wellsite_bins_proj
  
  # use ICR data to determine share of sites with tank batteries
  wellsite_tank_st_bins[, Has.Tank.Battery := fifelse(grepl("2|5|6", equip_bin), TRUE, FALSE)]
  wellsite_tank_st_bins[, Tank.Count.Site := (Has.Tank.Battery)*(tanks_well_gas*Gas.Wells.per.site + tanks_well_oil*Oil.Wells.per.site)]
  wellsite_tank_st_bins[, `:=` (Tank.Crude.TP.Bbl = (Has.Tank.Battery)*(Oil.Bbl-Cond.Bbl), 
                                Tank.Cond.TP.Bbl = (Has.Tank.Battery)*(Cond.Bbl),
                                Tank.Crude.TP.Bbl.Site.Start = (Has.Tank.Battery)*(Oil.Bbl.Site.Start-Cond.Bbl.Site.Start), 
                                Tank.Cond.TP.Bbl.Site.Start = (Has.Tank.Battery)*(Cond.Bbl.Site.Start),
                                Tank.Crude.TP.Bbl.Site.EG = (Has.Tank.Battery)*(Oil.Bbl.Site.EG-Cond.Bbl.Site.EG), 
                                Tank.Cond.TP.Bbl.Site.EG = (Has.Tank.Battery)*(Cond.Bbl.Site.EG),
                                Tank.Crude.TP.Bbl.Site.NSPS = (Has.Tank.Battery)*(Oil.Bbl.Site.NSPS-Cond.Bbl.Site.NSPS), 
                                Tank.Cond.TP.Bbl.Site.NSPS = (Has.Tank.Battery)*(Cond.Bbl.Site.NSPS))]
  
}
