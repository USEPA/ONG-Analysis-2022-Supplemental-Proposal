#' activity-nonwellsites.R
#' These functions construct projections of counts of affected facilities at 
#' natural gas processing plants and compressor stations from raw data.

# NGPP activity projections
get_proc_plant_act_proj <- function(
  proc_plant_csv, 
  gas_act_dta, 
  oil_act_dta
){
  
  # retirement rates
  retire.rate = 0.01
  
  # percent of processing facilities classified as "small"
  small.frac = 0.8
  
  #---------- -
  ## Base year ----
  #---------- -
  
  ## Processing plants
  # read data
  proc_dta = fread(proc_plant_csv, check.names = T)
  
  # restrict to natural gas processing plants
  proc_dta = proc_dta[COUNTRY %in% c("USA")]
  
  # classify plants as large or small based on capacity percentiles
  # NOTE: this assumes plants without capacity data are small (coded as -999 in HIFLD)
  proc.small.cut = quantile(proc_dta[, GASCAP], small.frac, na.rm = T)
  proc_dta[, Size.Cat := fifelse(GASCAP <= proc.small.cut, "Small", "Large")
  ][,
    Size.Cat := fifelse(is.na(Size.Cat), "Small", Size.Cat)
  ]
  
  # calculate base year (2020) plants
  proc_st_tot = proc_dta[, .(Plant.Count = .N), by = .(State = STATE, Size.Cat)]
  
  #---------- -
  ## Projections ----
  #---------- -
  
  # calculate plant distributions
  proc.plant.tot = proc_st_tot[, sum(Plant.Count)]
  proc_st_tot[, Prop := Plant.Count/proc.plant.tot]
  
  # initialize national and state/size projection tables
  proc_ntnl_proj = CJ.table(CJ.table(data.table(Reg.Bin = c("OOOO", "OOOOa", "OOOOb", "OOOOc")), data.table(Vint.Trunc = seq(base.yr, end.yr))), 
                            data.table(Year = seq(base.yr, end.yr)))
  proc_ntnl_proj = proc_ntnl_proj[Vint.Trunc <= Year][(Reg.Bin %in% c("OOOO", "OOOOc") & Vint.Trunc == 2019) | 
                                                        (Reg.Bin %in% c("OOOOa") & Vint.Trunc <= prop.sign.yr) |
                                                        (Reg.Bin %in% c("OOOOb") & Vint.Trunc > prop.sign.yr)]
  proc_st_proj = CJ.table(proc_st_tot, proc_ntnl_proj)
  
  # assign plant counts for key years
  proc.count.2011 = gas_act_dta[Segment == "PROC" & Source == "Plant Fugitives" & Year == 2011, Count]
  proc.count.2015 = gas_act_dta[Segment == "PROC" & Source == "Plant Fugitives" & Year == 2015, Count]
  proc.count.2020 = proc.plant.tot
  
  # calculate starting counts in each regulatory bin
  proc.count.OOOOc.base = proc.count.2011
  proc.count.OOOO.base = proc.count.2015 - (1-retire.rate)^(2015-2011)*proc.count.OOOOc.base
  proc.count.OOOOa.base = proc.count.2020 - proc.count.OOOO.base - (1-retire.rate)^(2020-2011)*proc.count.OOOOc.base
  proc.count.new = proc.count.OOOOa.base/(2020-2015)
  
  # calculate base year vintages into the future
  proc_ntnl_proj[, Plant.Count := fcase(Reg.Bin == "OOOOc" & Vint.Trunc == base.yr, (1 - retire.rate)^(Year - 2011)*proc.count.OOOOc.base,
                                        Reg.Bin == "OOOO" & Vint.Trunc == base.yr, proc.count.OOOO.base,
                                        Reg.Bin == "OOOOa" & Vint.Trunc == base.yr, proc.count.OOOOa.base - proc.count.new,
                                        Reg.Bin == "OOOOa" & Vint.Trunc > base.yr & Vint.Trunc <= prop.sign.yr, proc.count.new,
                                        Reg.Bin == "OOOOb", proc.count.new,
                                        default = NA_real_)]
  
  # allocate national totals to state/size bins
  proc_st_proj = merge.data.table(proc_st_proj[, .(State, Size.Cat, Reg.Bin, Vint.Trunc, Year, Prop)], proc_ntnl_proj, by = c("Reg.Bin", "Vint.Trunc", "Year"))
  proc_st_proj[, `:=` (Plant.Count = Prop*Plant.Count)]
  
  proc_st_proj
  
}

# Compressor station base year activity
get_compr_station_act_base <- function(
  compr_station_csv
){
  
  ## Compressor stations
  stn_dta = fread(compr_station_csv, check.names = T)[!(State.Province %in% "FO GULF")]
  
  stn_dta
  
}

# G&B compressor station activity projections
get_gb_station_act_proj <- function(
  stn_dta, 
  gas_act_dta, 
  oil_act_dta
){
  
  # retirement rates
  retire.rate = 0.01
  
  #---------- -
  ## Base year ----
  #---------- -
  
  # assign gathering and boosting stations
  gb_stn_dta = stn_dta[Pipeline.Type == "Gathering"]
  
  # calculate base year (2019) plants
  gb_stn_st_tot = gb_stn_dta[, .(Station.Count = .N), by = .(State = State.Province)]
  
  #---------- -
  ## Projections ----
  #---------- -
  
  # calculate station proportion by state in base year
  gb.stn.tot = gb_stn_st_tot[, sum(Station.Count)]
  gb_stn_st_tot[, Prop := Station.Count/gb.stn.tot]
  
  # initialize national and state/size projection tables
  gb_stn_ntnl_proj = CJ.table(CJ.table(data.table(Reg.Bin = c("OOOO", "OOOOa", "OOOOb", "OOOOc")), data.table(Vint.Trunc = seq(base.yr, end.yr))), 
                              data.table(Year = seq(base.yr, end.yr)))
  gb_stn_ntnl_proj = gb_stn_ntnl_proj[Vint.Trunc <= Year][(Reg.Bin %in% c("OOOO", "OOOOc") & Vint.Trunc == 2019) | 
                                                            (Reg.Bin %in% c("OOOOa") & Vint.Trunc <= prop.sign.yr) |
                                                            (Reg.Bin %in% c("OOOOb") & Vint.Trunc > prop.sign.yr)]
  gb_stn_st_proj = CJ.table(gb_stn_st_tot, gb_stn_ntnl_proj)
  
  # assign station counts for key years
  gb.stn.count.2011 = gas_act_dta[Segment == "G&B" & Source == "Yard Piping" & Year == 2011, Count]
  gb.stn.count.2015 = gas_act_dta[Segment == "G&B" & Source == "Yard Piping" & Year == 2015, Count]
  gb.stn.count.2019 = gas_act_dta[Segment == "G&B" & Source == "Yard Piping" & Year == 2019, Count]
  
  # calculate starting counts in each regulatory bin
  gb.stn.count.OOOOc.base = gb.stn.count.2011
  gb.stn.count.OOOO.base = gb.stn.count.2015 - (1-retire.rate)^(2015-2011)*gb.stn.count.OOOOc.base
  gb.stn.count.OOOOa.base = gb.stn.count.2019 - gb.stn.count.OOOO.base - (1-retire.rate)^(2019-2011)*gb.stn.count.OOOOc.base
  gb.stn.count.new = gb.stn.count.OOOOa.base/(2019-2015)
  
  # calculate projected station counts
  gb_stn_ntnl_proj[, Station.Count := fcase(Reg.Bin == "OOOOc" & Vint.Trunc == base.yr, (1 - retire.rate)^(Year - 2011)*gb.stn.count.OOOOc.base,
                                        Reg.Bin == "OOOO" & Vint.Trunc == base.yr, gb.stn.count.OOOO.base,
                                        Reg.Bin == "OOOOa" & Vint.Trunc == base.yr, gb.stn.count.OOOOa.base,
                                        Reg.Bin == "OOOOa" & Vint.Trunc > base.yr & Vint.Trunc <= prop.sign.yr, gb.stn.count.new,
                                        Reg.Bin == "OOOOb", gb.stn.count.new,
                                        default = NA_real_)]
  
  # allocate national totals to state/size bins
  gb_stn_st_proj = merge.data.table(gb_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Prop)], gb_stn_ntnl_proj, by = c("Reg.Bin", "Vint.Trunc", "Year"))
  gb_stn_st_proj[, `:=` (Station.Count = Prop*Station.Count)]
  
  gb_stn_st_proj
   
}

# Transmission compressor station activity projections
get_trans_station_act_proj <- function(
  stn_dta, 
  gas_act_dta, 
  oil_act_dta
){
  
  # retirement rates
  retire.rate = 0.01
  
  #---------- -
  ## Base year ----
  #---------- -
  
  # assign transmission compressor stations
  trans_stn_dta = stn_dta[Pipeline.Type == "Transmission"]
  
  # calculate base year (2019) stations
  trans_stn_st_tot = trans_stn_dta[, .(Station.Count = .N), by = .(State = State.Province)]
  
  #---------- -
  ## Projections ----
  #---------- -
  
  # calculate station distributions
  trans.stn.tot = trans_stn_st_tot[, sum(Station.Count)]
  trans_stn_st_tot[, Prop := Station.Count/trans.stn.tot]
  
  # initialize national and state/size projection tables
  trans_stn_ntnl_proj = CJ.table(CJ.table(data.table(Reg.Bin = c("OOOO", "OOOOa", "OOOOb", "OOOOc")), data.table(Vint.Trunc = seq(base.yr, end.yr))), 
                                 data.table(Year = seq(base.yr, end.yr)))
  trans_stn_ntnl_proj = trans_stn_ntnl_proj[Vint.Trunc <= Year][(Reg.Bin %in% c("OOOO", "OOOOc") & Vint.Trunc == 2019) | 
                                                                  (Reg.Bin %in% c("OOOOa") & Vint.Trunc <= prop.sign.yr) |
                                                                  (Reg.Bin %in% c("OOOOb") & Vint.Trunc > prop.sign.yr)]
  trans_stn_st_proj = CJ.table(trans_stn_st_tot, trans_stn_ntnl_proj)
  
  # assign station counts for key years
  trans.stn.count.2011 = gas_act_dta[Segment == "TRANS" & Source == "Station + Compressor Fugitive Emissions" & Year == 2011, Count]
  trans.stn.count.2015 = gas_act_dta[Segment == "TRANS" & Source == "Station + Compressor Fugitive Emissions" & Year == 2015, Count]
  trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "Station + Compressor Fugitive Emissions" & Year == 2019, Count]
  
  # calculate starting counts in each regulatory bin
  trans.stn.count.OOOOc.base = trans.stn.count.2011
  trans.stn.count.OOOO.base = trans.stn.count.2015 - (1-retire.rate)^(2015-2011)*trans.stn.count.OOOOc.base
  trans.stn.count.OOOOa.base = trans.stn.count.2019 - trans.stn.count.OOOO.base - (1-retire.rate)^(2019-2011)*trans.stn.count.OOOOc.base
  trans.stn.count.new = trans.stn.count.OOOOa.base/(2019-2015)
  
  # calculate projected station counts
  trans_stn_ntnl_proj[, Station.Count := fcase(Reg.Bin == "OOOOc" & Vint.Trunc == base.yr, (1 - retire.rate)^(Year - 2011)*trans.stn.count.OOOOc.base,
                                            Reg.Bin == "OOOO" & Vint.Trunc == base.yr, trans.stn.count.OOOO.base,
                                            Reg.Bin == "OOOOa" & Vint.Trunc == base.yr, trans.stn.count.OOOOa.base,
                                            Reg.Bin == "OOOOa" & Vint.Trunc > base.yr & Vint.Trunc <= prop.sign.yr, trans.stn.count.new,
                                            Reg.Bin == "OOOOb", trans.stn.count.new,
                                            default = NA_real_)]
  
  # allocate national totals to state/size bins
  trans_stn_st_proj = merge.data.table(trans_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Prop)], trans_stn_ntnl_proj, by = c("Reg.Bin", "Vint.Trunc", "Year"))
  trans_stn_st_proj[, `:=` (Station.Count = Prop*Station.Count)]
  
  trans_stn_st_proj

}

# Storage compressor station activity projections
get_stor_station_act_proj <- function(
  stn_dta, 
  gas_act_dta, 
  oil_act_dta
){
  
  # retirement rates
  retire.rate = 0.01
  
  #---------- -
  ## Base year ----
  #---------- -
  
  # assign storage compressor stations
  stor_stn_dta = stn_dta[Pipeline.Type == "Storage"]
  
  # calculate base year (2019) stations
  stor_stn_st_tot = stor_stn_dta[, .(Station.Count = .N), by = .(State = State.Province)]
  
  #---------- -
  ## Projections ----
  #---------- -
  
  # calculate station distributions
  stor.stn.tot = stor_stn_st_tot[, sum(Station.Count)]
  stor_stn_st_tot[, Prop := Station.Count/stor.stn.tot]
  
  # initialize national and state/size projection tables
  stor_stn_ntnl_proj = CJ.table(CJ.table(data.table(Reg.Bin = c("OOOO", "OOOOa", "OOOOb", "OOOOc")), data.table(Vint.Trunc = seq(base.yr, end.yr))), 
                                data.table(Year = seq(base.yr, end.yr)))
  stor_stn_ntnl_proj = stor_stn_ntnl_proj[Vint.Trunc <= Year][(Reg.Bin %in% c("OOOO", "OOOOc") & Vint.Trunc == 2019) | 
                                                            (Reg.Bin %in% c("OOOOa") & Vint.Trunc <= prop.sign.yr) |
                                                            (Reg.Bin %in% c("OOOOb") & Vint.Trunc > prop.sign.yr)]
  stor_stn_st_proj = CJ.table(stor_stn_st_tot, stor_stn_ntnl_proj)
  
  # assign station counts for key years
  stor.stn.count.2011 = gas_act_dta[Segment == "STOR" & Source == "Station + Compressor Fugitive Emissions" & Year == 2011, Count]
  stor.stn.count.2015 = gas_act_dta[Segment == "STOR" & Source == "Station + Compressor Fugitive Emissions" & Year == 2015, Count]
  stor.stn.count.2019 = gas_act_dta[Segment == "STOR" & Source == "Station + Compressor Fugitive Emissions" & Year == 2019, Count]
  
  # calculate starting counts in each regulatory bin
  stor.stn.count.OOOOc.base = stor.stn.count.2011
  stor.stn.count.OOOO.base = stor.stn.count.2015 - (1-retire.rate)^(2015-2011)*stor.stn.count.OOOOc.base
  stor.stn.count.OOOOa.base = stor.stn.count.2019 - stor.stn.count.OOOO.base - (1-retire.rate)^(2019-2011)*stor.stn.count.OOOOc.base
  stor.stn.count.new = stor.stn.count.OOOOa.base/(2019-2015)
  
  # calculate base year (2019) count for each regulatory bin
  stor_stn_ntnl_proj[, Station.Count := fcase(Reg.Bin == "OOOOc" & Vint.Trunc == base.yr, (1 - retire.rate)^(Year - 2011)*stor.stn.count.OOOOc.base,
                                              Reg.Bin == "OOOO" & Vint.Trunc == base.yr, stor.stn.count.OOOO.base,
                                              Reg.Bin == "OOOOa" & Vint.Trunc == base.yr, stor.stn.count.OOOOa.base,
                                              Reg.Bin == "OOOOa" & Vint.Trunc > base.yr & Vint.Trunc <= prop.sign.yr, stor.stn.count.new,
                                              Reg.Bin == "OOOOb", stor.stn.count.new,
                                              default = NA_real_)]
  
  # allocate national totals to state/size bins
  stor_stn_st_proj = merge.data.table(stor_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Prop)], stor_stn_ntnl_proj, by = c("Reg.Bin", "Vint.Trunc", "Year"))
  stor_stn_st_proj[, `:=` (Station.Count = Prop*Station.Count)]
  
  stor_stn_st_proj
  
}

# Non-wellsite affected facility projections
# Does not include wellsites, pneumatic controllers, storage vessels
get_activity_projections_nonwellsite <- function(
  gas_act_dta,
  oil_act_dta,
  gb_stn_st_proj,
  proc_st_proj,
  trans_stn_st_proj,
  stor_stn_st_proj,
  wellsite_bins_proj
) {
  
  # Switches and parameters ---------------------------
  
  # percent of compressors in G&B assumed to be reciprocating/centrifugal
  gb.recip.frac = .89
  gb.cntfgl.frac = .03
  
  #-------------------- -
  ## Pneumatic devices ----
  #-------------------- -
  
  ## gathering and boosting stations
  gb_stn_pneu_proj = gb_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # expand to create a distinction between sites with and without high-bleeds
  gb_stn_pneu_proj = CJ.table(gb_stn_pneu_proj, data.table(High.Bleed = c("Has hbPnC", "No hbPnC")))[order(Year, State, Reg.Bin, Vint.Trunc)]
  
  # calculate devices per station based on GHGI
  pnc.lb.gb.stn.count.2019 = gas_act_dta[Segment == "G&B" & Source == "Low-Bleed Pneumatic Devices" & Year == 2019, Count]
  pnc.hb.gb.stn.count.2019 = gas_act_dta[Segment == "G&B" & Source == "High-bleed Pneumatic Devices" & Year == 2019, Count]
  pnc.ib.gb.stn.count.2019 = gas_act_dta[Segment == "G&B" & Source == "Intermittent Bleed Pneumatic Devices" & Year == 2019, Count]
  pnc.gb.stn.count.2019 = pnc.lb.gb.stn.count.2019 + pnc.hb.gb.stn.count.2019 + pnc.ib.gb.stn.count.2019
  pnc.per.gb.stn.2019 = pnc.gb.stn.count.2019/gb_stn_st_proj[Year == base.yr, sum(Station.Count)]
  pumps.gb.stn.count.2019 = gas_act_dta[Segment == "G&B" & Source == "Pneumatic Pumps" & Year == 2019, Count]
  pumps.per.gb.stn.2019 = pumps.gb.stn.count.2019/gb_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of controllers per station
  gb_stn_pneu_proj[, `:=` (PnC.Station = pnc.per.gb.stn.2019)]
  
  # segment vintages into coarser regulatory bins
  gb_stn_pneu_proj[, Reg.Bin2 := fifelse(Reg.Bin %in% c("OOOOc"), "EG", "NSPS")]
  
  # calculate national total for controllers by coarse reg bin
  gb_stn_pneu_proj[, PnC.Natl.Reg.Bin2 := sum(pnc.per.gb.stn.2019 * Station.Count*(High.Bleed == "Has hbPnC"), na.rm = T), by = .(Year, Reg.Bin2)]
  
  # calculate high-bleed fraction of total controllers at EG stations in base year
  pnc.hb.gb.stn.frac.2019 = (pnc.hb.gb.stn.count.2019/pnc.gb.stn.count.2019)*
    gb_stn_pneu_proj[Year == base.yr & High.Bleed == "Has hbPnC", sum(pnc.per.gb.stn.2019 * Station.Count)]/
    gb_stn_pneu_proj[Year == base.yr & Reg.Bin %in% c("OOOOc") & High.Bleed == "Has hbPnC", sum(pnc.per.gb.stn.2019 * Station.Count)]
  
  # calculate national total for high-bleed controllers in all years
  gb_stn_pneu_proj[, PnC.HB.Natl := pnc.hb.gb.stn.frac.2019*PnC.Natl.Reg.Bin2]
  
  # calculate national number of sites by coarser reg bin
  gb_stn_pneu_proj[, Stn.Count.Natl.Reg.Bin2 := sum(Station.Count*(High.Bleed == "Has hbPnC")), by = .(Reg.Bin2, Year)]
  
  # distribute sites according to high-bleed controller status
  gb_stn_pneu_proj[, Station.Count := fifelse(Reg.Bin2 == "EG", pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                  (High.Bleed == "Has hbPnC") + 
                                               (Station.Count - pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                  (High.Bleed == "No hbPnC"))*(!High.Bleed == "Has hbPnC"),
                                               Station.Count*(!High.Bleed == "Has hbPnC"))]
  
  # calculate controller breakdown for each site type
  gb_stn_pneu_proj[, HB.Count.Station := fifelse(High.Bleed == "Has hbPnC", pmax(1,PnC.HB.Natl/Stn.Count.Natl.Reg.Bin2), 0)]
  gb_stn_pneu_proj[, IB.Count.Station := (PnC.Station - HB.Count.Station)*pnc.ib.gb.stn.count.2019/(pnc.gb.stn.count.2019-pnc.hb.gb.stn.count.2019)]
  gb_stn_pneu_proj[, LB.Count.Station := PnC.Station - HB.Count.Station - IB.Count.Station]
  gb_stn_pneu_proj[, Pump.Count.Station := pumps.per.gb.stn.2019]
  
  # remove rows without sites
  gb_stn_pneu_proj = gb_stn_pneu_proj[Station.Count > 0]
  
  gb_stn_pneu_proj[, .(sum(Station.Count), 
                       sum(Station.Count*HB.Count.Station), 
                       sum(Station.Count*IB.Count.Station), 
                       sum(Station.Count*LB.Count.Station), 
                       sum(Station.Count*Pump.Count.Station)), by = .(Year)]
  
  ## transmission compressor stations
  trans_stn_pneu_proj = trans_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # expand to create a distinction between sites with and without high-bleeds
  trans_stn_pneu_proj = CJ.table(trans_stn_pneu_proj, data.table(High.Bleed = c("Has hbPnC", "No hbPnC")))[order(Year, State, Reg.Bin, Vint.Trunc)]
  
  # calculate devices per station based on GHGI
  pnc.lb.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "(Low Bleed)" & Year == 2019, Count]
  pnc.hb.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "(High Bleed)" & Year == 2019, Count]
  pnc.ib.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "(Intermittent Bleed)" & Year == 2019, Count]
  pnc.trans.stn.count.2019 = pnc.lb.trans.stn.count.2019 + pnc.hb.trans.stn.count.2019 + pnc.ib.trans.stn.count.2019
  pnc.per.trans.stn.2019 = pnc.trans.stn.count.2019/trans_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of controllers per station
  trans_stn_pneu_proj[, `:=` (PnC.Station = pnc.per.trans.stn.2019)]
  
  # segment vintages into coarser regulatory bins
  trans_stn_pneu_proj[, Reg.Bin2 := fifelse(Reg.Bin %in% c("OOOOc", "OOOO"), "EG", "NSPS")]
  
  # calculate national total for controllers by coarse reg bin
  trans_stn_pneu_proj[, PnC.Natl.Reg.Bin2 := sum(pnc.per.trans.stn.2019 * Station.Count*(High.Bleed == "Has hbPnC"), na.rm = T), by = .(Year, Reg.Bin2)]
  
  # calculate high-bleed fraction of total controllers at EG stations in base year
  pnc.hb.trans.stn.frac.2019 = (pnc.hb.trans.stn.count.2019/pnc.trans.stn.count.2019)*
    trans_stn_pneu_proj[Year == base.yr & High.Bleed == "Has hbPnC", sum(pnc.per.trans.stn.2019 * Station.Count)]/
    trans_stn_pneu_proj[Year == base.yr & Reg.Bin %in% c("OOOOc", "OOOO") & High.Bleed == "Has hbPnC", sum(pnc.per.trans.stn.2019 * Station.Count)]
  
  # calculate national total for high-bleed controllers in all years
  trans_stn_pneu_proj[, PnC.HB.Natl := pnc.hb.trans.stn.frac.2019*PnC.Natl.Reg.Bin2]
  
  # calculate national number of sites by coarser reg bin
  trans_stn_pneu_proj[, Stn.Count.Natl.Reg.Bin2 := sum(Station.Count*(High.Bleed == "Has hbPnC")), by = .(Reg.Bin2, Year)]
  
  # distribute sites according to high-bleed controller status
  trans_stn_pneu_proj[, Station.Count := fifelse(Reg.Bin2 == "EG", pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                (High.Bleed == "Has hbPnC") + 
                                                (Station.Count - pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                   (High.Bleed == "No hbPnC"))*(!High.Bleed == "Has hbPnC"),
                                              Station.Count*(!High.Bleed == "Has hbPnC"))]
  
  # calculate controller breakdown for each site type
  trans_stn_pneu_proj[, HB.Count.Station := fifelse(High.Bleed == "Has hbPnC", pmax(1,PnC.HB.Natl/Stn.Count.Natl.Reg.Bin2), 0)]
  trans_stn_pneu_proj[, IB.Count.Station := (PnC.Station - HB.Count.Station)*pnc.ib.trans.stn.count.2019/(pnc.trans.stn.count.2019-pnc.hb.trans.stn.count.2019)]
  trans_stn_pneu_proj[, LB.Count.Station := PnC.Station - HB.Count.Station - IB.Count.Station]
  
  # remove rows without sites
  trans_stn_pneu_proj = trans_stn_pneu_proj[Station.Count > 0]
  
  trans_stn_pneu_proj[, .(sum(Station.Count), 
                       sum(Station.Count*HB.Count.Station), 
                       sum(Station.Count*IB.Count.Station), 
                       sum(Station.Count*LB.Count.Station)), by = .(Year)]
  
  ## storage compressor stations
  stor_stn_pneu_proj = stor_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # expand to create a distinction between sites with and without high-bleeds
  stor_stn_pneu_proj = CJ.table(stor_stn_pneu_proj, data.table(High.Bleed = c("Has hbPnC", "No hbPnC")))[order(Year, State, Reg.Bin, Vint.Trunc)]
  
  # calculate devices per station based on GHGI
  pnc.lb.stor.stn.count.2019 = gas_act_dta[Segment == "STOR" & Source == "(Low Bleed)" & Year == 2019, Count]
  pnc.hb.stor.stn.count.2019 = gas_act_dta[Segment == "STOR" & Source == "(High Bleed)" & Year == 2019, Count]
  pnc.ib.stor.stn.count.2019 = gas_act_dta[Segment == "STOR" & Source == "(Intermittent Bleed)" & Year == 2019, Count]
  pnc.stor.stn.count.2019 = pnc.lb.stor.stn.count.2019 + pnc.hb.stor.stn.count.2019 + pnc.ib.stor.stn.count.2019
  pnc.per.stor.stn.2019 = pnc.stor.stn.count.2019/stor_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of controllers per station
  stor_stn_pneu_proj[, `:=` (PnC.Station = pnc.per.stor.stn.2019)]
  
  # segment vintages into coarser regulatory bins
  stor_stn_pneu_proj[, Reg.Bin2 := fifelse(Reg.Bin %in% c("OOOOc", "OOOO"), "EG", "NSPS")]
  
  # calculate national total for controllers by coarse reg bin
  stor_stn_pneu_proj[, PnC.Natl.Reg.Bin2 := sum(pnc.per.stor.stn.2019 * Station.Count*(High.Bleed == "Has hbPnC"), na.rm = T), by = .(Year, Reg.Bin2)]
  
  # calculate high-bleed fraction of total controllers at EG stations in base year
  pnc.hb.stor.stn.frac.2019 = (pnc.hb.stor.stn.count.2019/pnc.stor.stn.count.2019)*
    stor_stn_pneu_proj[Year == base.yr & High.Bleed == "Has hbPnC", sum(pnc.per.stor.stn.2019 * Station.Count)]/
    stor_stn_pneu_proj[Year == base.yr & Reg.Bin %in% c("OOOOc", "OOOO") & High.Bleed == "Has hbPnC", sum(pnc.per.stor.stn.2019 * Station.Count)]
  
  # calculate national total for high-bleed controllers in all years
  stor_stn_pneu_proj[, PnC.HB.Natl := pnc.hb.stor.stn.frac.2019*PnC.Natl.Reg.Bin2]
  
  # calculate national number of sites by coarser reg bin
  stor_stn_pneu_proj[, Stn.Count.Natl.Reg.Bin2 := sum(Station.Count*(High.Bleed == "Has hbPnC")), by = .(Reg.Bin2, Year)]
  
  # distribute sites according to high-bleed controller status
  stor_stn_pneu_proj[, Station.Count := fifelse(Reg.Bin2 == "EG", pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                   (High.Bleed == "Has hbPnC") + 
                                                   (Station.Count - pmin(PnC.HB.Natl*(Station.Count/Stn.Count.Natl.Reg.Bin2), Station.Count)*
                                                      (High.Bleed == "No hbPnC"))*(!High.Bleed == "Has hbPnC"),
                                                 Station.Count*(!High.Bleed == "Has hbPnC"))]
  
  # calculate controller breakdown for each site type
  stor_stn_pneu_proj[, HB.Count.Station := fifelse(High.Bleed == "Has hbPnC", pmax(1,PnC.HB.Natl/Stn.Count.Natl.Reg.Bin2), 0)]
  stor_stn_pneu_proj[, IB.Count.Station := (PnC.Station - HB.Count.Station)*pnc.ib.stor.stn.count.2019/(pnc.stor.stn.count.2019-pnc.hb.stor.stn.count.2019)]
  stor_stn_pneu_proj[, LB.Count.Station := PnC.Station - HB.Count.Station - IB.Count.Station]
  
  # remove rows without sites
  stor_stn_pneu_proj = stor_stn_pneu_proj[Station.Count > 0]
  
  #------------------------------- -
  ## Reciprocating compressors -----
  #------------------------------- -
  
  ## processing plants
  # initialize projection table
  proc_recip_proj = proc_st_proj[, .(Plant.Count = sum(Plant.Count)), by = .(State, Reg.Bin, Vint.Trunc, Year)]
  
  # calculate reciprocating compressors per plant based on GHGI
  recip.proc.count.2019 = gas_act_dta[Segment == "PROC" & Source == "Reciprocating Compressors" & Year == 2019, Count]
  recip.per.proc.2019 = recip.proc.count.2019/proc_st_proj[Year == base.yr, sum(Plant.Count)]
  
  # apply number of reciprocating compressors per plant to get total compressors
  proc_recip_proj = proc_recip_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Recip.Count = recip.per.proc.2019*Plant.Count)]
  
  ## gathering and boosting stations
  # initialize projection table
  gb_stn_recip_proj = gb_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # calculate reciprocating compressors per station based on GHGI
  recip.gb.stn.count.2019 = gb.recip.frac*gas_act_dta[Segment == "G&B" & Source == "Compressors" & Year == 2019, Count]
  recip.per.gb.stn.2019 = recip.gb.stn.count.2019/gb_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of reciprocating compressors per station to get total compressors
  gb_stn_recip_proj = gb_stn_recip_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Recip.Count = recip.per.gb.stn.2019*Station.Count)]
  
  ## transmission compressor stations
  # initialize projection table
  trans_stn_recip_proj = trans_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # calculate reciprocating compressors per station based on GHGI
  recip.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "Reciprocating Compressor" & Year == 2019, Count]
  recip.per.trans.stn.2019 = recip.trans.stn.count.2019/trans_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of reciprocating compressors per station to get total compressors
  trans_stn_recip_proj = trans_stn_recip_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Recip.Count = recip.per.trans.stn.2019*Station.Count)]
  
  ## storage compressor stations
  # initialize projection table
  stor_stn_recip_proj = stor_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # calculate reciprocating compressors per station based on GHGI
  recip.stor.stn.count.2019 = gas_act_dta[Segment == "STOR" & Source == "Reciprocating Compressor" & Year == 2019, Count]
  recip.per.stor.stn.2019 = recip.stor.stn.count.2019/stor_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of reciprocating compressors per station to get total compressors
  stor_stn_recip_proj = stor_stn_recip_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Recip.Count = recip.per.stor.stn.2019*Station.Count)]
  
  #----------------------------------------- -
  ## Centrifugal compressors --------
  #----------------------------------------- -
  
  ## processing plants
  # initialize projection table
  proc_cntfgl_compr_proj = proc_st_proj[, .(Plant.Count = sum(Plant.Count)), by = .(State, Reg.Bin, Vint.Trunc, Year)]
  
  # calculate centrifugal compressors per plant based on GHGI
  wet.cntfgl.compr.proc.count.2019 = gas_act_dta[Segment == "PROC" & Source == "Centrifugal Compressors (wet seals)" & Year == base.yr, Count]
  dry.cntfgl.compr.proc.count.2019 = gas_act_dta[Segment == "PROC" & Source == "Centrifugal Compressors (dry seals)" & Year == base.yr, Count]
  wet.cntfgl.compr.per.proc.2019 = wet.cntfgl.compr.proc.count.2019/proc_st_proj[Year == base.yr, sum(Plant.Count)]
  dry.cntfgl.compr.per.proc.2019 = dry.cntfgl.compr.proc.count.2019/proc_st_proj[Year == base.yr, sum(Plant.Count)]
  cntfgl.compr.per.proc.2019 = (wet.cntfgl.compr.proc.count.2019+dry.cntfgl.compr.proc.count.2019)/proc_st_proj[Year == base.yr, sum(Plant.Count)]
  
  # apply number of centrifugal compressors per plant to get total compressors
  proc_cntfgl_compr_base = proc_cntfgl_compr_proj[Year == base.yr, .(Plant.Count = sum(Plant.Count),
                                                                     Cntfgl.Count = sum(cntfgl.compr.per.proc.2019*Plant.Count)),
                                                  by = .(Reg.Bin)][, `:=` (Cntfgl.Count.Tot = sum(Cntfgl.Count),
                                                                           Plants.Count.Tot = sum(Plant.Count))]
  
  # calculate number of wet and dry seal compressors per plant by regulatory vintage
  cntfgl_compr_per_proc_reg_bin = proc_cntfgl_compr_base[, .(Reg.Bin, 
                                                             dry.seal.per.proc = fifelse(Reg.Bin %in% c("OOOO", "OOOOa"), 
                                                                                         cntfgl.compr.per.proc.2019, 
                                                                                         (dry.cntfgl.compr.per.proc.2019*Plants.Count.Tot-
                                                                                            cntfgl.compr.per.proc.2019*(Plants.Count.Tot-Plant.Count))/Plant.Count),
                                                             wet.seal.per.proc = fifelse(Reg.Bin %in% c("OOOO", "OOOOa"), 0, 
                                                                                         wet.cntfgl.compr.per.proc.2019*Plants.Count.Tot/Plant.Count))]
  
  # create entry for OOOOb era
  cntfgl_compr_per_proc_reg_bin = rbindlist(list(cntfgl_compr_per_proc_reg_bin, cntfgl_compr_per_proc_reg_bin[Reg.Bin == "OOOOa"][, Reg.Bin := "OOOOb"]))
  
  # merge into main projection data table
  proc_cntfgl_compr_proj = merge.data.table(proc_cntfgl_compr_proj, cntfgl_compr_per_proc_reg_bin, by = c("Reg.Bin"))
  proc_wet_cntfgl_compr_proj = proc_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                          Cntfgl.Count = wet.seal.per.proc*Plant.Count)]
  proc_dry_cntfgl_compr_proj = proc_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                          Cntfgl.Count = dry.seal.per.proc*Plant.Count)]
  
  ## transmission compressor stations
  # initialize projection table
  trans_stn_cntfgl_compr_proj = trans_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # calculate centrifugal compressors per station based on GHGI
  wet.cntfgl.compr.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "Centrifugal Compressor (wet seals)" & Year == base.yr, Count]
  dry.cntfgl.compr.trans.stn.count.2019 = gas_act_dta[Segment == "TRANS" & Source == "Centrifugal Compressor (dry seals)" & Year == base.yr, Count]
  wet.cntfgl.compr.per.trans.stn.2019 = wet.cntfgl.compr.trans.stn.count.2019/trans_stn_st_proj[Year == base.yr, sum(Station.Count)]
  dry.cntfgl.compr.per.trans.stn.2019 = dry.cntfgl.compr.trans.stn.count.2019/trans_stn_st_proj[Year == base.yr, sum(Station.Count)]
  cntfgl.compr.per.trans.stn.2019 = (wet.cntfgl.compr.trans.stn.count.2019+dry.cntfgl.compr.trans.stn.count.2019)/trans_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of centrifugal compressors per plant to get total compressors
  trans_stn_cntfgl_compr_base = trans_stn_cntfgl_compr_proj[Year == base.yr, .(Station.Count = sum(Station.Count), 
                                                                               Cntfgl.Count = sum(cntfgl.compr.per.trans.stn.2019*Station.Count)),
                                                            by = .(Reg.Bin)][, Reg.Bin2 := fifelse(Reg.Bin %in% c("OOOOc", "OOOO"), "Unregulated", "Regulated")
                                                            ][, `:=` (Cntfgl.Count.2 = sum(Cntfgl.Count),
                                                                      Station.Count.2 = sum(Station.Count)), by = .(Reg.Bin2)
                                                            ][, `:=` (Cntfgl.Count.Tot = sum(Cntfgl.Count),
                                                                      Station.Count.Tot = sum(Station.Count))]
  
  # calculate number of wet and dry seal compressors per plant by regulatory vintage
  cntfgl_compr_per_trans_stn_reg_bin = trans_stn_cntfgl_compr_base[, .(Reg.Bin, 
                                                                       dry.seal.per.trans.stn = fifelse(Reg.Bin %in% c("OOOOa", "OOOOb"), 
                                                                                                        cntfgl.compr.per.trans.stn.2019, 
                                                                                                        (dry.cntfgl.compr.per.trans.stn.2019*Station.Count.Tot-
                                                                                                           cntfgl.compr.per.trans.stn.2019*(Station.Count.Tot-Station.Count.2))/
                                                                                                          Station.Count.2),
                                                                       wet.seal.per.trans.stn = fifelse(Reg.Bin %in% c("OOOOa", "OOOOb"), 0, 
                                                                                                        wet.cntfgl.compr.per.trans.stn.2019*Station.Count.Tot/Station.Count.2))]
  
  # create entry for OOOOb era
  cntfgl_compr_per_trans_stn_reg_bin = rbindlist(list(cntfgl_compr_per_trans_stn_reg_bin, cntfgl_compr_per_trans_stn_reg_bin[Reg.Bin == "OOOOa"][, Reg.Bin := "OOOOb"]))
  
  # merge into main projection data table
  trans_stn_cntfgl_compr_proj = merge.data.table(trans_stn_cntfgl_compr_proj, cntfgl_compr_per_trans_stn_reg_bin, by = c("Reg.Bin"))
  
  # apply number of centrifugal compressors per station to get total compressors
  trans_stn_wet_cntfgl_compr_proj = trans_stn_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                                    Cntfgl.Count = wet.seal.per.trans.stn*Station.Count)]
  trans_stn_dry_cntfgl_compr_proj = trans_stn_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                                    Cntfgl.Count = dry.seal.per.trans.stn*Station.Count)]
  
  ## gathering and boosting stations
  # initialize projection table
  gb_stn_cntfgl_compr_proj = gb_stn_st_proj[, .(State, Reg.Bin, Vint.Trunc, Year, Station.Count)]
  
  # calculate wet-seal centrifugal compressors per station based on GHGI (assume same dry/wet-seal split as in pre-OOOO transmission)
  wet.seal.cntfgl.compr.frac.2019 = cntfgl_compr_per_trans_stn_reg_bin[Reg.Bin == "OOOOc", wet.seal.per.trans.stn/cntfgl.compr.per.trans.stn.2019]
  cntfgl.compr.gb.stn.count.2019 = gb.cntfgl.frac*gas_act_dta[Segment == "G&B" & Source == "Compressors" & Year == 2019, Count]
  cntfgl.compr.per.gb.stn.2019 = cntfgl.compr.gb.stn.count.2019/gb_stn_st_proj[Year == base.yr, sum(Station.Count)]
  
  # apply number of wet-seal centrifugal compressors per station to get total compressors
  gb_stn_wet_cntfgl_compr_proj = gb_stn_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                              Cntfgl.Count = wet.seal.cntfgl.compr.frac.2019*cntfgl.compr.per.gb.stn.2019*Station.Count)]
  gb_stn_dry_cntfgl_compr_proj = gb_stn_cntfgl_compr_proj[, .(State, Reg.Bin, Vint.Trunc, Year, 
                                                              Cntfgl.Count = (1-wet.seal.cntfgl.compr.frac.2019)*cntfgl.compr.per.gb.stn.2019*Station.Count)]
  
  ##### Collect activity data projections --------------------------------
  
  # standardized AD format:
  # - Model.Plant
  # - Type
  # - Segment
  # - State
  # - Reg.Bin
  # - Vint.Trunc
  # - Year
  # - Fac.Count
  
  Reg.Bin.order <- c("OOOOc", "OOOO", "OOOOa", "OOOOb")
  Segment.order <- c("PROD", "G&B", "PROC", "TRANS", "STOR")
  
  # Processing Plants
  proc_st_voc_proj_std <- proc_st_proj[, .(
    Model.Plant = str_c("procplant_", str_to_lower(Size.Cat), "_voc"), Type = NA_character_,
    Segment = "PROC",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Plant.Count)]
  
  proc_st_nonvoc_proj_std <- proc_st_proj[, .(
    Model.Plant = str_c("procplant_", str_to_lower(Size.Cat), "_nonvoc"), Type = NA_character_,
    Segment = "PROC",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Plant.Count)]
  
  # Gathering and Boosting
  gb_stn_st_proj_std <- gb_stn_st_proj[, .(
    Model.Plant = "gbstation", Type = NA_character_,   Segment = "G&B",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count)]
  
  # Transmission
  trans_stn_st_proj_std <- trans_stn_st_proj[, .(
    Model.Plant = "transstation", Type = NA_character_, Segment = "TRANS",  
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count)]
  
  # Storage
  stor_stn_st_proj_std <- stor_stn_st_proj[, .(
    Model.Plant = "storstation", Type = NA_character_, Segment = "STOR",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count)]
  
  # Equipment
  
  # Reciprocating Compressors
  proc_recip_st_proj_std <- proc_recip_proj[, .(
    Model.Plant = "recip_comp", Type = NA_character_,
    Segment = "PROC",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Recip.Count)]
  gb_stn_recip_st_proj_std <- gb_stn_recip_proj[, .(
    Model.Plant = "recip_comp", Type = NA_character_,
    Segment = "G&B",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Recip.Count)]
  trans_stn_recip_st_proj_std <- trans_stn_recip_proj[, .(
    Model.Plant = "recip_comp", Type = NA_character_,
    Segment = "TRANS",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Recip.Count)]
  stor_stn_recip_st_proj_std <- stor_stn_recip_proj[, .(
    Model.Plant = "recip_comp", Type = NA_character_,
    Segment = "STOR",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Recip.Count)]
  
  # Centrifugal Compressors
  proc_wet_cntfgl_compr_st_proj_std <- proc_wet_cntfgl_compr_proj[, .(
    Model.Plant = "wet_cntfgl_comp", Type = NA_character_,
    Segment = "PROC",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  gb_stn_wet_cntfgl_compr_st_proj_std <- gb_stn_wet_cntfgl_compr_proj[, .(
    Model.Plant = "wet_cntfgl_comp", Type = NA_character_,
    Segment = "G&B",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  trans_stn_wet_cntfgl_compr_st_proj_std <- trans_stn_wet_cntfgl_compr_proj[, .(
    Model.Plant = "wet_cntfgl_comp", Type = NA_character_,
    Segment = "TRANS",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  proc_dry_cntfgl_compr_st_proj_std <- proc_dry_cntfgl_compr_proj[, .(
    Model.Plant = "dry_cntfgl_comp", Type = NA_character_,
    Segment = "PROC",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  gb_stn_dry_cntfgl_compr_st_proj_std <- gb_stn_dry_cntfgl_compr_proj[, .(
    Model.Plant = "dry_cntfgl_comp", Type = NA_character_,
    Segment = "G&B",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  trans_stn_dry_cntfgl_compr_st_proj_std <- trans_stn_dry_cntfgl_compr_proj[, .(
    Model.Plant = "dry_cntfgl_comp", Type = NA_character_,
    Segment = "TRANS",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Cntfgl.Count)]
  
  # Pneumatic devices
  gb_stn_pneu_st_proj_std <- gb_stn_pneu_proj[, .(
    Model.Plant = "pneu_dev", Type = NA_character_,
    Segment = "G&B",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count, 
    LB.Count.Station, IB.Count.Station, HB.Count.Station, Pump.Count.Station)]
  trans_stn_pneu_st_proj_std <- trans_stn_pneu_proj[, .(
    Model.Plant = "pneu_dev", Type = NA_character_,
    Segment = "TRANS",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count, 
    LB.Count.Station, IB.Count.Station, HB.Count.Station)]
  stor_stn_pneu_st_proj_std <- stor_stn_pneu_proj[, .(
    Model.Plant = "pneu_dev", Type = NA_character_,
    Segment = "STOR",
    State, Reg.Bin, Vint.Trunc, Year, Fac.Count = Station.Count, 
    LB.Count.Station, IB.Count.Station, HB.Count.Station)]
  
  
  # Liquids Unloading
  well_liq_unl_st_proj_std <- get_liquids_unloading_ad(wellsite_bins_proj, gas_act_dta)
  
  # Collect Equipment
  equip_st_proj <- rbindlist(
    list(
      proc_recip_st_proj_std,
      gb_stn_recip_st_proj_std,
      trans_stn_recip_st_proj_std,
      stor_stn_recip_st_proj_std,
      
      proc_wet_cntfgl_compr_st_proj_std,
      gb_stn_wet_cntfgl_compr_st_proj_std,
      trans_stn_wet_cntfgl_compr_st_proj_std,
      
      proc_dry_cntfgl_compr_st_proj_std,
      gb_stn_dry_cntfgl_compr_st_proj_std,
      trans_stn_dry_cntfgl_compr_st_proj_std,
      
      well_liq_unl_st_proj_std
    ),
    use.names = TRUE
  )
  
  # Collect State Projections
  ad_st_proj <- rbindlist(
    list(
      gb_stn_st_proj_std,
      proc_st_voc_proj_std,
      proc_st_nonvoc_proj_std,
      trans_stn_st_proj_std,
      stor_stn_st_proj_std,
      equip_st_proj
    ))
  
  ad_pneu_st_proj = rbindlist(
    list(
      gb_stn_pneu_st_proj_std,
      trans_stn_pneu_st_proj_std,
      stor_stn_pneu_st_proj_std
    ), fill = T)
  
  # Collect National Projections
  proc_ntnl_voc_proj_std <- proc_st_voc_proj_std[, .(
    Fac.Count=sum(Fac.Count)),
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year)]
  proc_ntnl_nonvoc_proj_std <- proc_st_nonvoc_proj_std[, .(
    Fac.Count=sum(Fac.Count)),
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year)]
  
  gb_stn_ntnl_proj_std <- gb_stn_st_proj[, .(
    Model.Plant = "gbstation", Type = NA_character_,
    Segment = "G&B",
    Fac.Count=sum(Station.Count)),
    by = .(Reg.Bin, Vint.Trunc, Year)]
  
  trans_stn_ntnl_proj_std <- trans_stn_st_proj[, .(
    Model.Plant = "transstation", Type = NA_character_,
    Segment = "TRANS",
    Fac.Count=sum(Station.Count)),
    by = .(Reg.Bin, Vint.Trunc, Year)]
  
  stor_stn_ntnl_proj_std <- stor_stn_st_proj[, .(
    Model.Plant = "storstation", Type = NA_character_,
    Segment = "STOR",
    Fac.Count=sum(Station.Count)),
    by = .(Reg.Bin, Vint.Trunc, Year)]
  
  #Equipment
  # sum from state totals, as most of these don't have independently calculated national proj
  equip_ntnl_proj <- equip_st_proj[, .(
    Fac.Count=sum(Fac.Count)),
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year)]
  
  # Pneumatic devices
  gb_stn_pneu_ntnl_proj_std <- gb_stn_pneu_st_proj_std[, .(
    Fac.Count = sum(Fac.Count)), 
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year,
           LB.Count.Station, IB.Count.Station, HB.Count.Station, Pump.Count.Station)]
  trans_stn_pneu_ntnl_proj_std <- trans_stn_pneu_st_proj_std[, .(
    Fac.Count = sum(Fac.Count)), 
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year,
           LB.Count.Station, IB.Count.Station, HB.Count.Station)]
  stor_stn_pneu_ntnl_proj_std <- stor_stn_pneu_st_proj_std[, .(
    Fac.Count = sum(Fac.Count)), 
    by = .(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year,
           LB.Count.Station, IB.Count.Station, HB.Count.Station)]
  
  ad_ntnl_proj <- rbindlist(
    list(
      # Site Types
      proc_ntnl_voc_proj_std,
      proc_ntnl_nonvoc_proj_std,
      gb_stn_ntnl_proj_std,
      trans_stn_ntnl_proj_std,
      stor_stn_ntnl_proj_std,
      
      # Regulated Facilities
      equip_ntnl_proj
    ),
    use.names = TRUE,
    fill = TRUE) %>% 
    as_tibble() %>%
    mutate(
      Segment = factor(Segment, levels = Segment.order),
      Reg.Bin = factor(Reg.Bin, levels = Reg.Bin.order)) %>%
    arrange(Model.Plant, Type, Segment, Reg.Bin, Year) %>%
    select(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year, Fac.Count)
  
  ad_pneu_ntnl_proj <- rbindlist(
    list(
      gb_stn_pneu_ntnl_proj_std,
      trans_stn_pneu_ntnl_proj_std,
      stor_stn_pneu_ntnl_proj_std
    ),
    use.names = TRUE,
    fill = TRUE) %>% 
    as_tibble() %>%
    mutate(
      Segment = factor(Segment, levels = Segment.order),
      Reg.Bin = factor(Reg.Bin, levels = Reg.Bin.order)) %>%
    arrange(Model.Plant, Type, Segment, Reg.Bin, Year) %>%
    select(Model.Plant, Type, Segment, Reg.Bin, Vint.Trunc, Year, Fac.Count, 
           LB.Count.Station, IB.Count.Station, HB.Count.Station, Pump.Count.Station)
  
  # Temporarily return list of both
  list(
    "National" = ad_ntnl_proj,
    "State" = ad_st_proj,
    "National Pneumatics" = ad_pneu_ntnl_proj,
    "State Pneumatics" = ad_pneu_st_proj
    
  )
}
