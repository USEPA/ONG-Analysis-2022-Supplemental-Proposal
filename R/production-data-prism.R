#' production-data-prism.R
#' These functions read production time series data from Enverus and calculate
#' average (median) decline rates.

# load data
load_wells_prod <- function(wells_prod_csv){
  
  # read production time series (2010--2020)
  prod_ts = fread(wells_prod_csv, check.names = T)
  
}

# calculate average decline rates
calc_wells_decl <- function(wells_prod_ts){
  
  # set default value for producing days to be a full year
  wells_prod_ts[, ProducingDays := fifelse(ProducingDays > 0, ProducingDays, 365)]
  
  # assign wells to production and vintage bins
  wells_prod_ts[, `:=` (
    FirstProdDate = parse_date_multi(FirstProdDate, formats = c("%m/%d/%Y", "%Y-%m-%d")),
    CompletionDate = parse_date_multi(FirstProdDate, formats = c("%m/%d/%Y", "%Y-%m-%d"))
  )]
  wells_prod_ts[, Modify.Date := as.IDate(pmax(FirstProdDate, CompletionDate, na.rm = TRUE))]
  wells_prod_ts[, Modify.Date := fifelse(is.na(Modify.Date), as.IDate("1980-01-01"), Modify.Date)]
  wells_prod_ts[, 
    Vintage.Bin := fcase(
    Modify.Date >  as.IDate("2015-09-18"), "2016-2019",
    Modify.Date >= as.IDate("2011-08-24"), "2012-2015",
    Modify.Date >= as.IDate("2000-01-01"), "2000-2011",
    Modify.Date < as.IDate("2000-01-01"), "Pre-2000",
    default = NA_character_)
  ]
  wells_prod_ts[, ProdRate.BOEpd := Prod_BOE / ProducingDays]
  wells_prod_ts[, ProdRate.Bin := fcase(
    ProdRate.BOEpd > 100, ">100 BOE/day/well",
    ProdRate.BOEpd > 15, "15-100 BOE/day/well",
    ProdRate.BOEpd > 3, "3-15 BOE/day/well",
    ProdRate.BOEpd <= 3 & ProdRate.BOEpd > 0, "<=3 BOE/day/well",
    default = NA_character_
  )]
  wells_prod_ts[, ProdRate.Bin.Coarse := fcase(
    ProdRate.BOEpd > 100, ">100 BOE/day/well",
    ProdRate.BOEpd > 15, "15-100 BOE/day/well",
    ProdRate.BOEpd <= 15 & ProdRate.BOEpd > 0, "<=15 BOE/day/well",
    default = NA_character_
  )]
  wells_prod_ts = wells_prod_ts[order(WellID_Prism, ProducingYear)]
  
  wells_prod_ts[, `:=` (LiquidsProd_BBL.Next = shift(LiquidsProd_BBL, type = "lead"),
                        GasProd_MCF.Next = shift(GasProd_MCF, type = "lead"),
                        ProdRate.BOEpd.Next = shift(ProdRate.BOEpd, type = "lead")),
                by = .(WellID_Prism)]
  
  wells_prod_ts[, `:=` (LiquidsProd_BBL.Next = fifelse(LiquidsProd_BBL.Next > 0 & ProducingYear > year(Modify.Date), LiquidsProd_BBL.Next, NA_real_),
                        GasProd_MCF.Next = fifelse(GasProd_MCF.Next > 0 & ProducingYear > year(Modify.Date), GasProd_MCF.Next, NA_real_),
                        ProdRate.BOEpd.Next = fifelse(ProdRate.BOEpd.Next > 0 & ProducingYear > year(Modify.Date), ProdRate.BOEpd.Next, NA_real_)), 
                by = .(WellID_Prism)]
  
  wells_prod_ts[, `:=` (LiquidsProd_BBL.Curr = fifelse(LiquidsProd_BBL > 0 & LiquidsProd_BBL.Next > 0 & ProducingYear > year(Modify.Date), LiquidsProd_BBL, NA_real_),
                        GasProd_MCF.Curr = fifelse(GasProd_MCF > 0 & GasProd_MCF.Next > 0 & ProducingYear > year(Modify.Date), GasProd_MCF, NA_real_),
                        ProdRate.BOEpd.Curr = fifelse(ProdRate.BOEpd > 0 & ProdRate.BOEpd.Next > 0 & ProducingYear > year(Modify.Date), ProdRate.BOEpd, NA_real_))]
  
  wells_prod_ts[, `:=` (LiquidsProd_BBL.Diff = fifelse(LiquidsProd_BBL > 0 & LiquidsProd_BBL.Next > 0 & ProducingYear > year(Modify.Date), 
                                                       LiquidsProd_BBL.Curr - LiquidsProd_BBL.Next, NA_real_),
                        GasProd_MCF.Diff = fifelse(GasProd_MCF > 0 & GasProd_MCF.Next > 0 & ProducingYear > year(Modify.Date), 
                                                   GasProd_MCF.Curr - GasProd_MCF.Next, NA_real_),
                        ProdRate.BOEpd.Diff = fifelse(ProdRate.BOEpd > 0 & ProdRate.BOEpd.Next > 0 & ProducingYear > year(Modify.Date), 
                                                      ProdRate.BOEpd.Curr - ProdRate.BOEpd.Next, NA_real_))]
  
  wells_prod_ts[, `:=` (LiquidsProd_BBL.Decl = LiquidsProd_BBL.Diff/LiquidsProd_BBL.Curr,
                        GasProd_MCF.Decl = GasProd_MCF.Diff/GasProd_MCF.Curr,
                        ProdRate.BOEpd.Decl = ProdRate.BOEpd.Diff/ProdRate.BOEpd.Curr)]
  
  decl_rates = wells_prod_ts[, .(Oil.Decl = median(LiquidsProd_BBL.Decl, na.rm = T),
                                 Gas.Decl = median(GasProd_MCF.Decl, na.rm = T),
                                 BOE.Decl = median(ProdRate.BOEpd.Decl, na.rm = T)), by = .(ProdRate.Bin)
                             ][!is.na(ProdRate.Bin), .(ProdRate.Bin, Oil.Decl, Gas.Decl)]
  
  decl_rates
  
}
