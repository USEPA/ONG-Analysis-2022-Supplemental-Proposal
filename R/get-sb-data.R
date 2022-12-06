#' get-sb-data.R
#' These functions read and process site and facility counts and cost data 
#' for the small business impacts analysis contained in the RIA.

# ==== Functions ==== 
## ==== Well Operator Data ==== 

get_operator_data <- function(
  f
) {
  
  # Read Enverus data, select variables
  wells19 = fread(f, check.names = T)[!(State %in% c("NGOM"))]
  
  well.prod.dta = wells19[, .(
    OperatorName,
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
  well.prod.dta[, Modify.Date.Entity := as.IDate(pmax(FirstProdDate, CompletionDate, na.rm = TRUE))]
  well.prod.dta[, Modify.Date.Entity := fifelse(is.na(Modify.Date.Entity), as.IDate("1980-01-01"), Modify.Date.Entity)]
  well.prod.dta[, GOR.Entity := Gas.Mcf.Entity/Oil.Bbl.Entity]
  well.prod.dta[, Type.Entity := fifelse(GOR.Entity > 100, "Gas", "Oil")]
  well.prod.dta[, Cond.Bbl.Entity := fifelse(Type.Entity == "Gas", Oil.Bbl.Entity, 0)]
  well.prod.dta[, BOE.Entity := Oil.Bbl.Entity + Gas.Mcf.Entity/6]
  well.prod.dta[, Gas.Well.Count := Well.Count*(Type.Entity == "Gas")]
  well.prod.dta[, Oil.Well.Count := Well.Count*(Type.Entity == "Oil")]
  well.prod.dta[, Completion_2019 := fifelse(year(Modify.Date.Entity) == 2019, TRUE, FALSE)]
  well.prod.dta[, ProdRate.BOEpd.Entity := BOE.Entity / 365 / Well.Count]
  well.prod.dta[, ProdRate.Class.Entity := fifelse(ProdRate.BOEpd.Entity > 15, "Non-low Production", "Low Production")]
  well.prod.dta[, Ultra.Low.Entity := ProdRate.BOEpd.Entity < 2.25]
  
  # Aggregate over operators
  well.prod.dta <- well.prod.dta[, .(
    Gas.Wells = sum(Gas.Well.Count),
    Oil.Wells = sum(Oil.Well.Count),
    Non.Low.Wells = sum(Well.Count * (ProdRate.Class.Entity == "Non-low Production")),
    CO.CA.NM.PA.Wells = sum(Well.Count * (State %in% c("CO", "CA", "NM", "PA"))),
    Non.Low.CO.CA.NM.PA.Wells = sum(Well.Count * (State %in% c("CO", "CA", "NM", "PA"))*(ProdRate.Class.Entity == "Non-low Production")), 
    Non.Ultra.Low.Wells = sum(Well.Count*(!Ultra.Low.Entity)),
    CO.CA.NM.PA.Non.Ultra.Low.Wells = sum(Well.Count*(!Ultra.Low.Entity)*(State %in% c("CO", "CA", "NM", "PA"))),
    Has.Completion.2019 = max(Completion_2019), 
    Total.Well.Completions.2019 = sum(Completion_2019*(Well.Count)),
    CO.CA.NM.PA.Well.Completions.2019 = sum(Completion_2019*(Well.Count)*(State %in% c("CO", "CA", "NM", "PA"))),
    Gas.Mcf = sum(Gas.Mcf.Entity), 
    Oil.Bbl = sum(Oil.Bbl.Entity), 
    Cond.Bbl = sum(Cond.Bbl.Entity),
    BOE = sum(BOE.Entity)
  ), by = .(OperatorName, State)]
  
  well.prod.dta %>% as_tibble() %>%
    mutate(
    Country = "US",
    City = "",
    Wells = Gas.Wells + Oil.Wells) %>%
    filter(!(OperatorName == "(N/A)")) %>%
    select(OperatorName, State, City, Country, 
           Gas.Wells, Oil.Wells, Wells, Gas.Mcf, Oil.Bbl, Cond.Bbl, BOE, Has.Completion.2019, 
           Total.Well.Completions.2019, CO.CA.NM.PA.Well.Completions.2019, 
           Non.Low.Wells,  CO.CA.NM.PA.Wells, Non.Low.CO.CA.NM.PA.Wells, Non.Ultra.Low.Wells, CO.CA.NM.PA.Non.Ultra.Low.Wells) %>%
    rename(operator_name = OperatorName, state = State)
  
}

## ==== Raw Data Processing ====
read_wellsites <- function(
  .f, 
  .sheet
) {
  
  read_excel(path = .f, sheet = .sheet) %>% mutate(facility = "Well Site") %>%
    rename(operator_name = OriginalOperatorName, state = s_State, final_source = finalDataSource,
           ultimate_parent = finalUltimateParentName, naics_code = finalNaicsCode,
           naics_desc = finalNaicsDescription, revenue = finalRevenueUsd, employees = finalEmployees,
           sb_status_strict = updatedSmallBusinessStatusStrictNsb,
           sb_status_loose = updatedSmallBusinessStatusLooseNsb) %>%
    select(facility, operator_name, state, ultimate_parent, naics_code, naics_desc,
           revenue, employees, sb_status_strict, sb_status_loose, final_source) %>%
    mutate(revenue = as.numeric(revenue), employees = as.numeric(employees), naics_code = as.character(naics_code)) %>% 
    filter(ultimate_parent != "NA")
  
}

read_processing_plants <- function(
  .f, 
  .sheet, 
  id_data
) {
  
  proc_counts <- get_processing_plant_counts(id_data)
  
  read_excel(path = .f, sheet = .sheet) %>% mutate(facility = "Processing Plant") %>%
    rename(operator_name = companyName, final_source = finalSource,
           ultimate_parent = finalUltimateParentName, naics_code = finalNaicsCode,
           naics_desc = finalNaicsDescription, revenue = finalRevenue, employees = finalEmployees,
           sb_status = smallBusinessStatus) %>%
    select(facility, operator_name, ultimate_parent, naics_code, naics_desc,
           revenue, employees, sb_status, final_source) %>%
    mutate(revenue = as.numeric(revenue), employees = as.numeric(employees), naics_code = as.character(naics_code)) %>% 
    filter(ultimate_parent != "NA") %>%
    left_join(proc_counts, by = "operator_name")
  
}

read_compressors <- function(
  .f, 
  .sheet, 
  id_data
) {
  
  comp_counts <- get_compressor_counts(id_data)
  
  read_excel(path = .f, sheet = .sheet) %>% mutate(facility = "Compressor Station") %>%
    rename(operator_name = companyName, final_source = finalSource,
           ultimate_parent = finalUltimateParentName, naics_code = finalNaicsCode,
           naics_desc = finalNaicsDescription, revenue = finalRevenue, employees = finalEmployees,
           sb_status = smallBusinessStatus) %>%
    select(facility, operator_name, ultimate_parent, naics_code, naics_desc,
           revenue, employees, sb_status, final_source) %>%
    mutate(revenue = as.numeric(revenue), employees = as.numeric(employees), naics_code = as.character(naics_code)) %>% 
    filter(ultimate_parent != "NA") %>%
    left_join(comp_counts, by = "operator_name")
  
}

get_compressor_counts <- function(
  .f
) {
  
  read_excel(.f, sheet = "Compressors") %>% 
    select(`Company Name`, ts_stations, gath_stations) %>%
    rename(operator_name = `Company Name`, gathering_stations = gath_stations)
  
}

get_processing_plant_counts <- function(
  .f
) {
  
  read_excel(.f, sheet = "Processing Plants") %>% 
    select(`Company Name`, oper_state_processors, State) %>%
    rename(operator_name = `Company Name`, proc_plants = oper_state_processors) %>% group_by(operator_name) %>%
    summarise(proc_plants = sum(proc_plants),
              proc_plants_co_ca = sum(proc_plants*(State %in% c("CO","CA"))))
  
}

# ==== Combining Operator Data with SB Data ====

get_sb_facility_data <- function(
  sb_data, 
  wells_data, 
  id_data
) {
  
  well_data <- get_operator_data(wells_data)
  
  wellsite_sheet <- "wellsiteFinal"
  proc_sheet <- "processorsFinal"
  comp_sheet <- "compressorsFinal"
  
  wellsite_data <- read_wellsites(sb_data, wellsite_sheet) %>%
    left_join(well_data, by = c("operator_name", "state"))
  proc_data <- read_processing_plants(sb_data, proc_sheet, id_data)
  comp_data <- read_compressors(sb_data, comp_sheet, id_data)
  
  facility_data <- bind_rows(wellsite_data, proc_data, comp_data) %>%
    filter((sb_status != "NA")|(sb_status_strict != "NA")|(sb_status_loose != "NA")) %>%
    mutate(across(.cols = Gas.Wells:gathering_stations, .fns = ~ifelse(is.na(.x), 0, .x), .names = "{.col}"))
  
  # Adjust revenue from 2021 to 2019 dollars using GDP deflator
  facility_data <- facility_data %>% mutate(revenue = revenue / (118.370/112.294))
}

get_sb_parent_data <- function(
  fac_data, 
  wellsite_bins_proj, 
  gas_act_dta, 
  ng_price
) {
  
  # AEO22 WTI spot for 2022 adjusted to 2019$
  oil_price <- 70 / (118.370/112.294)
  gas_price <- ng_price[year == 2022] %>% pull()
  
  # Columns for final datasets
  wellsite_cols <- c("Facility.Type", "Ultimate.Parent", "Naics.Code", "Naics.Desc", "SB.Status.Strict",
                     "SB.Status.Loose", "Revenue", "Product.Revenue", "Employees", "Wells", "Gas.Wells" , "Oil.Wells", "Has.Completion.2019", 
                     "Total.Well.Completions.2019", "Gas.Mcf", "Oil.Bbl", "Cond.Bbl",                   
                     "BOE", "Wellsites", "Liquids.Unloading.Events", "Non.Low.Wells", "Non.Low.Wellsites", "CO.CA.NM.PA.Wells",
                     "CO.CA.NM.PA.Wellsites", "Non.Low.CO.CA.NM.PA.Wells", "Non.Low.CO.CA.NM.PA.Wellsites", "Completed.Wellsites",
                     "CO.CA.NM.PA.Completed.Wellsites", "Non.Ultra.Low.Wellsites", "CO.CA.NM.PA.Non.Ultra.Low.Wellsites")
  
  proc_cols <- c("Facility.Type", "Ultimate.Parent", "Naics.Code", "Naics.Desc", "SB.Status", "SB.Status.Strict",
                 "SB.Status.Loose", "Revenue", "Employees", "Proc.Plants", "CO.CA.Proc.Plants")
  
  comp_cols <- c("Facility.Type", "Ultimate.Parent", "Naics.Code", "Naics.Desc", "SB.Status", "SB.Status.Strict",
                 "SB.Status.Loose", "Revenue", "Employees", "TS.Stations", "Gathering.Stations")
  
  # Per wellsite/well data
  wellsite_sb_data <- get_wellsite_sb_data(wellsite_bins_proj) 
  
  # Hardcode for now
  Wells.Per.Wellsite.All <- wellsite_sb_data$Wells.Per.Wellsite.All
  
  # liquids unloading
  # calculate "wellhead-only"
  who.frac <- sum(wellsite_bins_proj[str_detect(wellsite_bins_proj$equip_bin, "^[12]"), .(Gas.Well.Count)])/sum(wellsite_bins_proj[, .(Gas.Well.Count)])
  liq.unl.plunge.mnl.frac <- .76
  liq.unl.plunge.events.per.well <- 7.7
  liq.unl.wo.plunge.events.per.well <- 5.6
  
  # calculate fraction of wells that are venting
  well.gas.count.2019 = (1-who.frac)*gas_act_dta[Segment == "PROD" & Source == "Total Active Gas Wells" & Year == 2019, Count]
  liq.unl.plunge.count.2019 = gas_act_dta[Segment == "PROD" & Source == "Liquids Unloading with Plunger Lifts" & Year == 2019, Count]
  liq.unl.plunge.frac.2019 = liq.unl.plunge.count.2019/well.gas.count.2019
  liq.unl.wo.plunge.count.2019 = gas_act_dta[Segment == "PROD" & Source == "Liquids Unloading without Plunger Lifts" & Year == 2019, Count]
  liq.unl.wo.plunge.frac.2019 = liq.unl.wo.plunge.count.2019/well.gas.count.2019
  
  # Aggregate over parent companies
  parent_data <- fac_data %>% group_by(facility, ultimate_parent, naics_code, naics_desc, sb_status, sb_status_strict, sb_status_loose) %>%
    summarise(Revenue = max(revenue),
              Employees = max(employees),
              Wells = sum(Wells),
              Gas.Wells = sum(Gas.Wells),
              Oil.Wells = sum(Oil.Wells),
              Proc.Plants = sum(proc_plants),
              CO.CA.Proc.Plants = sum(proc_plants_co_ca),
              TS.Stations = sum(ts_stations),
              Gathering.Stations = sum(gathering_stations),
              Has.Completion.2019 = max(Has.Completion.2019),
              Total.Well.Completions.2019 = sum(Total.Well.Completions.2019),
              CO.CA.NM.PA.Well.Completions.2019 = sum(CO.CA.NM.PA.Well.Completions.2019),
              Gas.Mcf = sum(Gas.Mcf),
              Oil.Bbl = sum(Oil.Bbl),
              Cond.Bbl = sum(Cond.Bbl),
              BOE = sum(BOE),
              Non.Low.Wells = sum(Non.Low.Wells),
              CO.CA.NM.PA.Wells = sum(CO.CA.NM.PA.Wells),
              Non.Low.CO.CA.NM.PA.Wells = sum(Non.Low.CO.CA.NM.PA.Wells),
              Non.Ultra.Low.Wells = sum(Non.Ultra.Low.Wells),
              CO.CA.NM.PA.Non.Ultra.Low.Wells = sum(CO.CA.NM.PA.Non.Ultra.Low.Wells),
              .groups = "drop") %>%
    ungroup() %>%
    rename(Facility.Type = facility,
           Ultimate.Parent = ultimate_parent,
           Naics.Code = naics_code,
           Naics.Desc = naics_desc,
           SB.Status = sb_status,
           SB.Status.Strict = sb_status_strict,
           SB.Status.Loose = sb_status_loose) %>%
    mutate(SB.Status.Strict = ifelse(is.na(SB.Status.Strict), SB.Status, SB.Status.Strict),
           SB.Status.Loose = ifelse(is.na(SB.Status.Loose), SB.Status, SB.Status.Loose),
           Product.Revenue = oil_price*Oil.Bbl + gas_price*Gas.Mcf,
           Revenue = ifelse(Product.Revenue > Revenue, Product.Revenue, Revenue)) %>%
    mutate(Wellsites = Wells / Wells.Per.Wellsite.All,
           Non.Low.Wellsites = Non.Low.Wells/Wells.Per.Wellsite.All,
           CO.CA.NM.PA.Wellsites = CO.CA.NM.PA.Wells/Wells.Per.Wellsite.All,
           Non.Low.CO.CA.NM.PA.Wellsites = Non.Low.CO.CA.NM.PA.Wells/Wells.Per.Wellsite.All,
           Completed.Wellsites = Total.Well.Completions.2019/Wells.Per.Wellsite.All,
           CO.CA.NM.PA.Completed.Wellsites = CO.CA.NM.PA.Well.Completions.2019/Wells.Per.Wellsite.All,
           Non.Ultra.Low.Wellsites = Non.Ultra.Low.Wells/Wells.Per.Wellsite.All,
           CO.CA.NM.PA.Non.Ultra.Low.Wellsites = CO.CA.NM.PA.Non.Ultra.Low.Wells/Wells.Per.Wellsite.All,
           Liquids.Unloading.Events = liq.unl.plunge.events.per.well*liq.unl.plunge.mnl.frac*liq.unl.plunge.frac.2019*Gas.Wells + 
             liq.unl.wo.plunge.events.per.well*liq.unl.wo.plunge.frac.2019*Gas.Wells)
  
  list(wellsites = parent_data %>% filter(Facility.Type == "Well Site") %>% select(wellsite_cols),
       processors = parent_data %>% filter(Facility.Type == "Processing Plant") %>% select(proc_cols),
       compressors = parent_data %>% filter(Facility.Type == "Compressor Station") %>% select(comp_cols))
  
}

get_wellsite_sb_data <- function(
  wellsite_bins_proj
) {
  
  # Calculate overall wells per well site
  wellsite_aggregates_all <- wellsite_bins_proj %>%
    filter(Year == 2026) %>% select(Wellsite.Count, Well.Count) %>%
    summarise(
      Total.Wellsites = sum(Wellsite.Count),
      Total.Wells = sum(Well.Count)) %>% as_tibble()
  
  Wells.Per.Wellsite.All <- wellsite_aggregates_all$Total.Wells / wellsite_aggregates_all$Total.Wellsites
  
  # Include only data that has a fugitives monitoring plan
  wellsite_bins_proj <- wellsite_bins_proj %>% 
    filter(!(str_detect(equip_bin, "^[12]:") & (Site.Type.Bin == "Single")))
  
  # Calculate equipment counts per well/wellsite in 2026
  # These will be used to approximate costs for oil/gas wellsite operators
  wellsite_aggregates <- wellsite_bins_proj %>%
    filter(Year == 2026) %>%
    select(Year, contains(".Count"), Has.Tank.Battery, Has_Liq_Prod, contains("PnC_well"), Pumps_per_site) %>% 
    mutate(
      Pump.Count = Wellsite.Count*Pumps_per_site,
      Gas.Controllers = Gas.Well.Count*(PnC_well_gas_lb + PnC_well_gas_ib + PnC_well_gas_hb),
      Oil.Controllers = Oil.Well.Count*(PnC_well_oil_lb + PnC_well_oil_ib + PnC_well_oil_hb)
    ) %>%
    group_by(Year) %>%
    summarise(
      Total.Wellsites = sum(Wellsite.Count),
      Total.Wells = sum(Well.Count),
      Total.Gas.Wells = sum(Gas.Well.Count),
      Total.Oil.Wells = sum(Oil.Well.Count),
      Total.Tank.Wellsites = sum(Has.Tank.Battery*Wellsite.Count),
      Total.Gas.Controllers = sum(Gas.Controllers),
      Total.Oil.Controllers = sum(Oil.Controllers),
      Total.Pumps = sum(Pump.Count), .groups = "drop"
    ) %>%
    as_tibble()
  
  Controllers.Per.Gas.Well <- wellsite_aggregates$Total.Gas.Controllers / wellsite_aggregates$Total.Gas.Wells
  Controllers.Per.Oil.Well <- wellsite_aggregates$Total.Oil.Controllers / wellsite_aggregates$Total.Oil.Wells
  Controllers.Per.Wellsite <- (wellsite_aggregates$Total.Oil.Controllers + wellsite_aggregates$Total.Gas.Controllers) / wellsite_aggregates$Total.Wellsites
  Pumps.Per.Wellsite <- wellsite_aggregates$Total.Pumps / wellsite_aggregates$Total.Wellsites
  Tank.Batteries.Per.Wellsite <- wellsite_aggregates$Total.Tank.Wellsites / wellsite_aggregates$Total.Wellsites
  Wells.Per.Wellsite.With.Equip <- wellsite_aggregates$Total.Wells / wellsite_aggregates$Total.Wellsites
  
  list(
    Wells.Per.Wellsite.All = Wells.Per.Wellsite.All,
    Wells.Per.Wellsite.With.Equip = Wells.Per.Wellsite.With.Equip,
    Controllers.Per.Gas.Well = Controllers.Per.Gas.Well,
    Controllers.Per.Oil.Well = Controllers.Per.Oil.Well,
    Controllers.Per.Wellsite = Controllers.Per.Wellsite, 
    Pumps.Per.Wellsite = Pumps.Per.Wellsite,
    Tank.Batteries.Per.Wellsite = Tank.Batteries.Per.Wellsite
  )
  
}

# ==== Cost Functions ====

# Get cost tables for processing
get_sb_costs_raw <- function(
  scn_comp, 
  type, 
  mp_list_supp, 
  mp_attrs_list_supp
) {
  
  if(type == "All") {
    scn_comp_table(scn_comp,
                   var_list = list("fac_affected","ann_7", "ann_7_wgas", "capital_cost", "annual_cost"),
                   row_detail = "detail",
                   show_years = 2026,
                   mp_list = mp_list_supp,
                   mp_attrs_list = mp_attrs_list_supp)
  }
  
  else if (type == "NSPS") {
    scn_comp_NSPS <- scn_comp %>% filter(vintage_bin %in% c('OOOOb'))
    scn_comp_table(scn_comp_NSPS,
                   var_list = list("fac_affected","ann_7", "ann_7_wgas", "capital_cost", "annual_cost"),
                   row_detail = "detail",
                   show_years = 2023,
                   mp_list = mp_list_supp,
                   mp_attrs_list = mp_attrs_list_supp)
  }
  
  else if (type == "EG") {
    scn_comp_EG <- scn_comp %>% filter(vintage_bin %in% c('OOOO', 'OOOOa', 'OOOOc'))
    scn_comp_table(scn_comp_EG,
                   var_list = list("fac_affected","ann_7", "ann_7_wgas", "capital_cost", "annual_cost"),
                   row_detail = "detail",
                   show_years = 2026,
                   mp_list = mp_list_supp,
                   mp_attrs_list = mp_attrs_list_supp)
  }
}

get_sb_cost_source <- function(
  sb_cost_data
) {
  
  sb_cost_data %>%
    mutate(Cost.Source = case_when(
      str_detect(detail_2, "T&S") | str_detect(detail_3, "T&S") ~ "Transmission and Storage Station",
      str_detect(detail_2, "G&B") | str_detect(detail_3, "G&B") ~ "Gathering and Boosting Station",
      str_detect(detail_2, "Proc") | str_detect(detail_3, "Proc") ~ "Processing Plant",
      TRUE ~ "Well Site"
      
    )) %>% relocate(Cost.Source) %>% arrange(Cost.Source)
  
}

# ==== Costs Per facility ====

# Get average cost per wellsite
get_sb_cost_per_wellsite <- function(
  cost_dta, 
  wellsite_bins_proj
) {
  
  # Select rows that apply to well sites
  sb_wellsite_cost <- cost_dta %>% filter(Cost.Source == "Well Site")
  
  # Get well site data
  wellsite_dta <- get_wellsite_sb_data(wellsite_bins_proj)
  Wells.Per.Wellsite.With.Equip <- wellsite_dta$Wells.Per.Wellsite.With.Equip 
  Controllers.Per.Wellsite <- wellsite_dta$Controllers.Per.Wellsite  
  Pumps.Per.Wellsite <- wellsite_dta$Pumps.Per.Wellsite 
  Tank.Batteries.Per.Wellsite <- wellsite_dta$Tank.Batteries.Per.Wellsite 
  
  # Counts of affected plants/sources
  total_wellsites <- sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_pumps <- sb_wellsite_cost %>% filter(source == "pumps") %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  total_liq_unl <- sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  total_tanks <- sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  total_controllers <- sb_wellsite_cost %>% filter(source == "pneucontr") %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  # Costs per source
  
  # Annualized without recovery
  ann_7_cost_per_wellsite <- ifelse(total_wellsites > 0, (sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
                                                            summarise(ann_7 = sum(ann_7)) %>% pull()) / total_wellsites, 0)
  
  ann_7_cost_per_pump <- ifelse(total_pumps > 0, (sb_wellsite_cost %>% filter(source == "pumps") %>%
                                                    summarise(ann_7 = sum(ann_7)) %>% pull()) / total_pumps, 0)
  
  ann_7_cost_per_controller <- ifelse(total_controllers > 0, (sb_wellsite_cost %>% filter(source == "pneucontr") %>%
                                                                summarise(ann_7 = sum(ann_7)) %>% pull()) / total_controllers, 0)
  
  ann_7_cost_per_tank <- ifelse(total_tanks > 0, (sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
                                                    summarise(ann_7 = sum(ann_7)) %>% pull()) / total_tanks, 0)
  
  ann_7_cost_per_liq_unl <- ifelse(total_liq_unl > 0, (sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
                                                         summarise(ann_7 = sum(ann_7)) %>% pull()) /total_liq_unl, 0)
  
  
  # Annualized with recovery
  ann_7_wgas_cost_per_wellsite <- ifelse(total_wellsites > 0, (sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
                                                                 summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_wellsites, 0)
  
  ann_7_wgas_cost_per_pump <- ifelse(total_pumps > 0, (sb_wellsite_cost %>% filter(source == "pumps") %>%
                                                         summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_pumps, 0)
  
  ann_7_wgas_cost_per_controller <- ifelse(total_controllers > 0, (sb_wellsite_cost %>% filter(source == "pneucontr") %>%
                                                                     summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_controllers, 0)
  
  ann_7_wgas_cost_per_tank <- ifelse(total_tanks > 0, (sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
                                                         summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_tanks, 0)
  
  ann_7_wgas_cost_per_liq_unl <- ifelse(total_liq_unl > 0, (sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
                                                              summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) /total_liq_unl, 0)
  # Capital Cost + 1 year O/M
  onetime_cost_per_wellsite <- ifelse(total_wellsites > 0, (sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
                                                              summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_wellsites, 0)
  
  onetime_cost_per_pump <- ifelse(total_pumps > 0, (sb_wellsite_cost %>% filter(source == "pumps") %>%
                                                      summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_pumps, 0)
  
  onetime_cost_per_controller <- ifelse(total_controllers > 0, (sb_wellsite_cost %>% filter(source == "pneucontr") %>%
                                                                  summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_controllers, 0)
  
  onetime_cost_per_tank <- ifelse(total_tanks > 0, (sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
                                                      summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_tanks, 0)
  
  onetime_cost_per_liq_unl <- ifelse(total_liq_unl > 0, (sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
                                                           summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) /total_liq_unl, 0)
  
  # Capital Cost
  capital_cost_per_wellsite <- ifelse(total_wellsites > 0, (sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
                                                              summarise(capital_cost = sum(capital_cost)) %>% pull()) / total_wellsites, 0)
  
  capital_cost_per_pump <- ifelse(total_pumps > 0, (sb_wellsite_cost %>% filter(source == "pumps") %>%
                                                      summarise(capital = sum(capital_cost)) %>% pull()) / total_pumps, 0)
  
  capital_cost_per_controller <- ifelse(total_controllers > 0, (sb_wellsite_cost %>% filter(source == "pneucontr") %>%
                                                                  summarise(capital = sum(capital_cost)) %>% pull()) / total_controllers, 0)
  
  capital_cost_per_tank <- ifelse(total_tanks > 0, (sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
                                                      summarise(capital = sum(capital_cost)) %>% pull()) / total_tanks, 0)
  
  capital_cost_per_liq_unl <- ifelse(total_liq_unl > 0, (sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
                                                           summarise(capital = sum(capital_cost)) %>% pull()) /total_liq_unl, 0)
  
  # OM Cost
  om_cost_per_wellsite <- ifelse(total_wellsites > 0, (sb_wellsite_cost %>% filter(str_detect(source, "fugitives")) %>%
                                                              summarise(OM = sum(annual_cost)) %>% pull()) / total_wellsites, 0)
  
  om_cost_per_pump <- ifelse(total_pumps > 0, (sb_wellsite_cost %>% filter(source == "pumps") %>%
                                                      summarise(OM = sum(annual_cost)) %>% pull()) / total_pumps, 0)
  
  om_cost_per_controller <- ifelse(total_controllers > 0, (sb_wellsite_cost %>% filter(source == "pneucontr") %>%
                                                                  summarise(OM = sum(annual_cost)) %>% pull()) / total_controllers, 0)
  
  om_cost_per_tank <- ifelse(total_tanks > 0, (sb_wellsite_cost %>% filter(source == "Storage vessels") %>%
                                                      summarise(OM = sum(annual_cost)) %>% pull()) / total_tanks, 0)
  
  om_cost_per_liq_unl <- ifelse(total_liq_unl > 0, (sb_wellsite_cost %>% filter(source == "liquids_unloading") %>%
                                                           summarise(OM = sum(annual_cost)) %>% pull()) /total_liq_unl, 0)
  
  # Per site, doesn't include tanks and liquids unloading
  cost_per_site_ann_7 <- ann_7_cost_per_wellsite + ann_7_cost_per_pump*Pumps.Per.Wellsite + 
    ann_7_cost_per_controller*Controllers.Per.Wellsite 
  
  cost_per_site_ann_7_wgas <- ann_7_wgas_cost_per_wellsite + ann_7_wgas_cost_per_pump*Pumps.Per.Wellsite + 
    ann_7_wgas_cost_per_controller*Controllers.Per.Wellsite 
  
  cost_per_site_onetime <- onetime_cost_per_wellsite + onetime_cost_per_pump*Pumps.Per.Wellsite + 
    onetime_cost_per_controller*Controllers.Per.Wellsite
  
  cost_per_site_capital <- capital_cost_per_wellsite + capital_cost_per_pump*Pumps.Per.Wellsite + 
    capital_cost_per_controller*Controllers.Per.Wellsite
  
  cost_per_site_om <- om_cost_per_wellsite + om_cost_per_pump*Pumps.Per.Wellsite + 
    om_cost_per_controller*Controllers.Per.Wellsite
  
  list(wo_gas = cost_per_site_ann_7, w_gas = cost_per_site_ann_7_wgas, onetime = cost_per_site_onetime,
       wo_gas_liq_unl = ann_7_cost_per_liq_unl, w_gas_liq_unl = ann_7_wgas_cost_per_liq_unl, onetime_liq_unl = onetime_cost_per_liq_unl,
       wo_gas_tank = ann_7_cost_per_tank*Tank.Batteries.Per.Wellsite, w_gas_tank = ann_7_wgas_cost_per_tank*Tank.Batteries.Per.Wellsite,
       onetime_tank = onetime_cost_per_tank*Tank.Batteries.Per.Wellsite,
       capital = cost_per_site_capital, om = cost_per_site_om, capital_tank = capital_cost_per_tank*Tank.Batteries.Per.Wellsite,
       om_tank = om_cost_per_tank*Tank.Batteries.Per.Wellsite, 
       capital_liq_unl = capital_cost_per_liq_unl, om_liq_unl = om_cost_per_liq_unl
       )
  
}

#Get average cost per processing plant
get_sb_cost_per_processor <- function(
  cost_dta
) {
  
  # Select rows that apply to processing plants
  sb_proc_cost <- cost_dta %>% filter(Cost.Source == "Processing Plant")
  
  # Counts of affected plants/sources
  total_plants <- sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_recip_comp <- sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_wet_seal_cent_comp <- sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_dry_seal_cent_comp <- sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  # per plant counts
  recip_per_plant <- total_recip_comp / total_plants
  wet_seal_per_plant <- total_wet_seal_cent_comp / total_plants
  dry_seal_per_plant <- total_dry_seal_cent_comp / total_plants
  
  # Costs per plant/source
  # Plant
  ann_7_cost_per_plant <- ifelse(total_plants > 0, (sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
                             summarise(ann_7 = sum(ann_7)) %>% pull()) / total_plants, 0)
  
  ann_7_wgas_cost_per_plant <- ifelse(total_plants > 0, (sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
                             summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_plants, 0)
  
  onetime_cost_per_plant <- ifelse(total_plants > 0, (sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
                               summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_plants, 0)
  
  capital_cost_per_plant <- ifelse(total_plants > 0, (sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
                                                        summarise(capital = sum(capital_cost)) %>% pull()) / total_plants, 0)
  
  om_cost_per_plant <- ifelse(total_plants > 0, (sb_proc_cost %>% filter(str_detect(detail_3, "Plants")) %>%
                                                        summarise(om = sum(annual_cost)) %>% pull()) / total_plants, 0)
  
  # Recip Comps
  ann_7_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                      summarise(ann_7 = sum(ann_7)) %>% pull()) / total_recip_comp, 0)
  
  ann_7_wgas_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                           summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_recip_comp, 0)
  
  onetime_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                               summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_recip_comp, 0)
  
  capital_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(capital = sum(capital_cost)) %>% pull()) / total_recip_comp, 0)
  
  om_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(om = sum(annual_cost)) %>% pull()) / total_recip_comp, 0)
  
  # Wet Seal Centrifugal Compressors
  ann_7_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                          summarise(ann_7 = sum(ann_7)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  ann_7_wgas_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                               summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  onetime_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                            summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  capital_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(capital = sum(capital_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  om_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(om = sum(annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  # Dry Seal Centrifugal Compressors
  ann_7_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                summarise(ann_7 = sum(ann_7)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  ann_7_wgas_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                     summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  onetime_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  capital_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(capital = sum(capital_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  om_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_proc_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(om = sum(annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  # Costs per plant
  cost_per_plant_ann_7 <- ann_7_cost_per_plant + ann_7_cost_per_recip*recip_per_plant +
    ann_7_cost_per_wet*wet_seal_per_plant + ann_7_cost_per_dry*dry_seal_per_plant
  
  cost_per_plant_ann_7_wgas <- ann_7_wgas_cost_per_plant + ann_7_wgas_cost_per_recip*recip_per_plant +
    ann_7_wgas_cost_per_wet*wet_seal_per_plant + ann_7_wgas_cost_per_dry*dry_seal_per_plant
  
  cost_per_plant_onetime <- onetime_cost_per_plant + onetime_cost_per_recip*recip_per_plant +
    onetime_cost_per_wet*wet_seal_per_plant + onetime_cost_per_dry*dry_seal_per_plant
  
  cost_per_plant_capital <- capital_cost_per_plant + capital_cost_per_recip*recip_per_plant +
    capital_cost_per_wet*wet_seal_per_plant + capital_cost_per_dry*dry_seal_per_plant
  
  cost_per_plant_om <- om_cost_per_plant + om_cost_per_recip*recip_per_plant +
    om_cost_per_wet*wet_seal_per_plant + om_cost_per_dry*dry_seal_per_plant
  
  # Return results in a list 
  list(wo_gas = cost_per_plant_ann_7, w_gas = cost_per_plant_ann_7_wgas, onetime = cost_per_plant_onetime,
       capital = cost_per_plant_capital, om = cost_per_plant_om)

}

# Get average cost per T&S station
get_sb_cost_per_TS <- function(
  cost_dta
) {
  
  # Select rows that apply to T&S stations
  sb_ts_cost <- cost_dta %>% filter(Cost.Source == "Transmission and Storage Station")
  
  # Counts of affected ts/sources
  total_ts <- sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  if(total_ts == 0) {
    # NSPS has 0 affected facilities in 2023, so TS count is set equal to total T&S stations in operator data
    total_ts <- 2287
  }
  
  total_recip_comp <- sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_wet_seal_cent_comp <- sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_dry_seal_cent_comp <- sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  total_pneumatics <- sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
    summarise(fac_affected = sum(fac_affected)) %>% pull()
  
  # per ts counts
  recip_per_ts <- total_recip_comp / total_ts
  wet_seal_per_ts <- total_wet_seal_cent_comp / total_ts
  dry_seal_per_ts <- total_dry_seal_cent_comp / total_ts
  pneumatic_per_ts <- total_pneumatics / total_ts
  
  # Costs per plant/source
  # Plant
  ann_7_cost_per_ts <- ifelse(total_ts > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
                                               summarise(ann_7 = sum(ann_7)) %>% pull()) / total_ts, 0)
  
  ann_7_wgas_cost_per_ts <- ifelse(total_ts > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
                                                    summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_ts, 0)
  
  onetime_cost_per_ts <- ifelse(total_ts > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
                                                 summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_ts, 0)
  
  capital_cost_per_ts <- ifelse(total_ts > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
                                                 summarise(capital = sum(capital_cost)) %>% pull()) / total_ts, 0)
  
  om_cost_per_ts <- ifelse(total_ts > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "T&S Compressor Stations")) %>%
                                                 summarise(om = sum(annual_cost)) %>% pull()) / total_ts, 0)
  
  # Recip Comps
  ann_7_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                          summarise(ann_7 = sum(ann_7)) %>% pull()) / total_recip_comp, 0)
  
  ann_7_wgas_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                               summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_recip_comp, 0)
  
  onetime_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_recip_comp, 0)
  
  capital_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(capital = sum(capital_cost)) %>% pull()) / total_recip_comp, 0)
  
  om_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(om = sum(annual_cost)) %>% pull()) / total_recip_comp, 0)
  
  # Wet Seal Centrifugal Compressors
  ann_7_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                summarise(ann_7 = sum(ann_7)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  ann_7_wgas_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                     summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  onetime_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  capital_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(capital = sum(capital_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  om_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(om = sum(annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
  
  # Dry Seal Centrifugal Compressors
  ann_7_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                summarise(ann_7 = sum(ann_7)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  ann_7_wgas_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                     summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  onetime_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  capital_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(capital = sum(capital_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  om_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(om = sum(annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
  
  # Pneumatics
  ann_7_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                         summarise(ann_7 = sum(ann_7)) %>% pull()) / total_pneumatics, 0)
  
  ann_7_wgas_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                              summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_pneumatics, 0)
  
  onetime_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                           summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_pneumatics, 0)
  
  capital_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                           summarise(capital = sum(capital_cost)) %>% pull()) / total_pneumatics, 0)
  
  om_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_ts_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                           summarise(om = sum(annual_cost)) %>% pull()) / total_pneumatics, 0)
  
  # Costs per plant
  cost_per_ts_ann_7 <- ann_7_cost_per_ts + ann_7_cost_per_recip*recip_per_ts +
    ann_7_cost_per_wet*wet_seal_per_ts + ann_7_cost_per_dry*dry_seal_per_ts + ann_7_cost_per_pneu*pneumatic_per_ts
  
  cost_per_ts_ann_7_wgas <- ann_7_wgas_cost_per_ts + ann_7_wgas_cost_per_recip*recip_per_ts +
    ann_7_wgas_cost_per_wet*wet_seal_per_ts + ann_7_wgas_cost_per_dry*dry_seal_per_ts + ann_7_wgas_cost_per_pneu*pneumatic_per_ts
  
  cost_per_ts_onetime <- onetime_cost_per_ts + onetime_cost_per_recip*recip_per_ts +
    onetime_cost_per_wet*wet_seal_per_ts + onetime_cost_per_dry*dry_seal_per_ts + onetime_cost_per_pneu*pneumatic_per_ts
  
  cost_per_ts_capital <- capital_cost_per_ts + capital_cost_per_recip*recip_per_ts +
    capital_cost_per_wet*wet_seal_per_ts + capital_cost_per_dry*dry_seal_per_ts + capital_cost_per_pneu*pneumatic_per_ts
  
  cost_per_ts_om <- om_cost_per_ts + om_cost_per_recip*recip_per_ts +
    om_cost_per_wet*wet_seal_per_ts + om_cost_per_dry*dry_seal_per_ts + om_cost_per_pneu*pneumatic_per_ts

  # Return results in a list 
  list(wo_gas = cost_per_ts_ann_7, w_gas = cost_per_ts_ann_7_wgas, onetime = cost_per_ts_onetime,
       capital = cost_per_ts_capital, om = cost_per_ts_om)
  
}

#Get average cost per G&B station
get_sb_cost_per_GB <- function(
  cost_dta
) {

    sb_gb_cost <- cost_dta %>% filter(Cost.Source == "Gathering and Boosting Station")
    
    # Count of affected gb/sources
    total_gb <- sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
      summarise(fac_affected = sum(fac_affected)) %>% pull()
    
    if(total_gb == 0) {
      #If NSPS has 0 affected facilities in 2023, so gb count is set equal to total G&B stations in operator data
      total_gb <- 3272
    }
    
    total_recip_comp <- sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
      summarise(fac_affected = sum(fac_affected)) %>% pull()
    
    total_wet_seal_cent_comp <- sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
      summarise(fac_affected = sum(fac_affected)) %>% pull()
    
    total_dry_seal_cent_comp <- sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
      summarise(fac_affected = sum(fac_affected)) %>% pull()
    
    total_pneumatics <- sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
      summarise(fac_affected = sum(fac_affected)) %>% pull()
    
    #per gb counts
    recip_per_gb <- total_recip_comp / total_gb
    wet_seal_per_gb <- total_wet_seal_cent_comp / total_gb
    dry_seal_per_gb <- total_dry_seal_cent_comp / total_gb
    pneumatic_per_gb <- total_pneumatics / total_gb
    
    # Costs per plant/source
    # Plant
    ann_7_cost_per_gb <- ifelse(total_gb > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
                                                 summarise(ann_7 = sum(ann_7)) %>% pull()) / total_gb, 0)
    
    ann_7_wgas_cost_per_gb <- ifelse(total_gb > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
                                                      summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_gb, 0)
    
    onetime_cost_per_gb <- ifelse(total_gb > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
                                                   summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_gb, 0)
    
    capital_cost_per_gb <- ifelse(total_gb > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
                                                   summarise(capital = sum(capital_cost)) %>% pull()) / total_gb, 0)
    
    om_cost_per_gb <- ifelse(total_gb > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "G&B Stations")) %>%
                                                   summarise(om = sum(annual_cost)) %>% pull()) / total_gb, 0)
    
    # Recip Comps
    ann_7_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                            summarise(ann_7 = sum(ann_7)) %>% pull()) / total_recip_comp, 0)
    
    ann_7_wgas_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                                 summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_recip_comp, 0)
    
    onetime_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                              summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_recip_comp, 0)
    
    capital_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                              summarise(capital = sum(capital_cost)) %>% pull()) / total_recip_comp, 0)
    
    om_cost_per_recip <- ifelse(total_recip_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Recip")) %>%
                                                              summarise(om = sum(annual_cost)) %>% pull()) / total_recip_comp, 0)
    
    # Wet Seal Centrifugal Compressors
    ann_7_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                  summarise(ann_7 = sum(ann_7)) %>% pull()) / total_wet_seal_cent_comp, 0)
    
    ann_7_wgas_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                       summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_wet_seal_cent_comp, 0)
    
    onetime_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                    summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
    
    capital_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                    summarise(capital = sum(capital_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
    
    om_cost_per_wet <- ifelse(total_wet_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Wet Seal")) %>%
                                                                    summarise(om = sum(annual_cost)) %>% pull()) / total_wet_seal_cent_comp, 0)
    
    # Dry Seal Centrifugal Compressors
    ann_7_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                  summarise(ann_7 = sum(ann_7)) %>% pull()) / total_dry_seal_cent_comp, 0)
    
    ann_7_wgas_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                       summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_dry_seal_cent_comp, 0)
    
    onetime_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                    summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
    
    capital_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                    summarise(capital = sum(capital_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
    
    om_cost_per_dry <- ifelse(total_dry_seal_cent_comp > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Dry Seal")) %>%
                                                                    summarise(om = sum(annual_cost)) %>% pull()) / total_dry_seal_cent_comp, 0)
    
    # Pneumatics
    ann_7_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                           summarise(ann_7 = sum(ann_7)) %>% pull()) / total_pneumatics, 0)
    
    ann_7_wgas_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                                summarise(ann_7_wgas = sum(ann_7_wgas)) %>% pull()) / total_pneumatics, 0)
    
    onetime_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                             summarise(onetime = sum(capital_cost + annual_cost)) %>% pull()) / total_pneumatics, 0)
    
    capital_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                             summarise(capital = sum(capital_cost)) %>% pull()) / total_pneumatics, 0)
    
    om_cost_per_pneu <- ifelse(total_pneumatics > 0, (sb_gb_cost %>% filter(str_detect(detail_2, "Pneumatic")) %>%
                                                             summarise(om = sum(annual_cost)) %>% pull()) / total_pneumatics, 0)
    
    # Costs per plant
    cost_per_gb_ann_7 <- ann_7_cost_per_gb + ann_7_cost_per_recip*recip_per_gb +
      ann_7_cost_per_wet*wet_seal_per_gb + ann_7_cost_per_dry*dry_seal_per_gb + ann_7_cost_per_pneu*pneumatic_per_gb
    
    cost_per_gb_ann_7_wgas <- ann_7_wgas_cost_per_gb + ann_7_wgas_cost_per_recip*recip_per_gb +
      ann_7_wgas_cost_per_wet*wet_seal_per_gb + ann_7_wgas_cost_per_dry*dry_seal_per_gb + ann_7_wgas_cost_per_pneu*pneumatic_per_gb
    
    cost_per_gb_onetime <- onetime_cost_per_gb + onetime_cost_per_recip*recip_per_gb +
      onetime_cost_per_wet*wet_seal_per_gb + onetime_cost_per_dry*dry_seal_per_gb + onetime_cost_per_pneu*pneumatic_per_gb
    
    cost_per_gb_capital <- capital_cost_per_gb + capital_cost_per_recip*recip_per_gb +
      capital_cost_per_wet*wet_seal_per_gb + capital_cost_per_dry*dry_seal_per_gb + capital_cost_per_pneu*pneumatic_per_gb
    
    cost_per_gb_om <- om_cost_per_gb + om_cost_per_recip*recip_per_gb +
      om_cost_per_wet*wet_seal_per_gb + om_cost_per_dry*dry_seal_per_gb + om_cost_per_pneu*pneumatic_per_gb
    
    # Return results in a list 
    list(wo_gas = cost_per_gb_ann_7, w_gas = cost_per_gb_ann_7_wgas, onetime = cost_per_gb_onetime,
         capital = cost_per_gb_capital, om = cost_per_gb_om)
    
}

# Calculate proportions of model plants in CO/CA in 2026
get_CO_CA_props <- function(
  scn_proj
) {
  scn_proj <- scn_proj$baseline_2022 %>% filter(year == 2026) %>% as_tibble()
  
  proc_prop <- sum(scn_proj %>% filter(str_detect(mp, "procplant") & location %in% c("CO", "CA")) %>% select(fac_count) %>% pull()) / 
    sum(scn_proj %>% filter(str_detect(mp, "procplant")) %>% select(fac_count) %>% pull())
  
  ts_prop <- sum(scn_proj %>% filter(str_detect(mp, "(trans)|(stor)station") & location %in% c("CO", "CA")) %>% select(fac_count) %>% pull()) / 
    sum(scn_proj %>% filter(str_detect(mp, "(trans)|(stor)station")) %>% select(fac_count) %>% pull())
  
  gb_prop <- sum(scn_proj %>% filter(str_detect(mp, "(trans)|(stor)station") & location %in% c("CO", "CA")) %>% select(fac_count) %>% pull()) / 
    sum(scn_proj %>% filter(str_detect(mp, "gbstation")) %>% select(fac_count) %>% pull())
  
  list(proc_prop = proc_prop,
       ts_prop = ts_prop,
       gb_prop = gb_prop)
  
}

