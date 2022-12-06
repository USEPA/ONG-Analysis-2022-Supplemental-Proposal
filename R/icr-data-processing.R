#' icr-data-processing.R
#' These functions clean, process, and aggregate the 2016 Oil and Gas ICR data.

# Read ICR Part One Survey --- Well Site Data
get_icr_part_one_data <- function(
  icr_surv_xlsx = "data-raw/icr_part_one.xlsx"
) {
  
  # read raw data
  icr_raw_wells_web <- readxl::read_xlsx(path = icr_surv_xlsx, sheet = "Pt1WellsFromWebForms")
  icr_raw_wells_file <- readxl::read_xlsx(path = icr_surv_xlsx, sheet = "Pt1WellsFromFileUploads")
  icr_raw_wellsites_web <- readxl::read_xlsx(path = icr_surv_xlsx, sheet = "Pt1WellSiteFromWebForms")
  icr_raw_wellsites_file <- readxl::read_xlsx(path = icr_surv_xlsx, sheet = "Pt1WellSiteFromFileUploads")
  
  # combine submission types
  icr_raw_wells <- rbindlist(list(icr_raw_wells_web, icr_raw_wells_file), use.names = T, fill = T)
  icr_raw_wellsites <- rbindlist(list(icr_raw_wellsites_web, icr_raw_wellsites_file), use.names = T, fill = T)
  
  # create a combined site ID/name column
  icr_raw_wells[, well_site_id_name := paste0(well_site_id, " | ", well_site_name)]
  icr_raw_wellsites[, well_site_id_name := paste0(well_site_id, " | ", well_site_name)]
  
  # gather for output
  list(icr_raw_wells = icr_raw_wells, icr_raw_wellsites = icr_raw_wellsites)
  
}

# Remove duplicate entries
remove_dups_icr_data <- function(
  icr_surv_xlsx
){
  
  # load raw data
  icr_raw = get_icr_part_one_data(icr_surv_xlsx)
  
  # assign data tables from list
  icr_raw_wells = icr_raw$icr_raw_wells
  icr_raw_wellsites = icr_raw$icr_raw_wellsites
  
  # remove universal duplicates (all columns the same)
  icr_raw_wells = unique(icr_raw_wells)
  icr_raw_wellsites = unique(icr_raw_wellsites)
  
  # subset duplicate observations for the same well
  icr_raw_wells[, fD := .N, by = .(operator_name, well_site_id_name, well_id)]
  well_dups = icr_raw_wells[fD > 1]
  icr_raw_wellsites[, fD := .N, by = .(operator_name, well_site_id_name)]
  wellsite_dups = icr_raw_wellsites[fD > 1]
  
  # find observations with different file attachment IDs or record submission times
  well_dups[, fD1 := .N, by = .(operator_name, well_site_id_name, well_id, attachment_id, date_added)]
  wellsite_dups[, fD1 := .N, by = .(operator_name, well_site_id_name, attachment_id, date_added)]
  
  # for wells with multiple submissions, keep most recent
  well_dups_mult_sub = unique(well_dups[order(operator_name, well_site_id_name, well_id, attachment_id, date_added)][fD1 == 1],
                              by = c("operator_name", "well_site_id_name", "well_id"))
  wellsite_dups_mult_sub = unique(wellsite_dups[order(operator_name, well_site_id_name, attachment_id, date_added)][fD1 == 1],
                                  by = c("operator_name", "well_site_id_name"))
  
  # find observations with identical well types (assume we can't do choose between observations of the same well with different well types)
  # these observations differ only by their pt1_production_site_id; assume this distinction is meaningless
  well_dups[, fD2 := .N, by = .(operator_name, well_site_id_name, well_id, well_type, attachment_id, date_added)]
  well_dups_diff_pt1_psID = unique(well_dups[fD1 > 1 & fD2 > 1], by = setdiff(names(well_dups), "pt1_production_site_id"))
  
  # remove site observations that do not produce gas or oil (or are unknown) and 
  # determine sites that differ only by latitude/longitude
  wellsite_dups_lat_lon = unique(wellsite_dups[fD1 > 1 & (grepl("Y", produces_natural_gas_for_sales, ignore.case = T) | 
                                                            grepl("Y", produces_crude_oil_condensate, ignore.case = T))], 
                                 by = setdiff(names(wellsite_dups), c("longitude", "latitude")))
  
  # append well/well site lists
  icr_raw_wells =  rbindlist(list(icr_raw_wells[fD == 1], well_dups_mult_sub, well_dups_diff_pt1_psID), fill = T)[, `:=` (fD = NULL, fD1 = NULL, fD2 = NULL)]
  icr_raw_wellsites =  rbindlist(list(icr_raw_wellsites[fD == 1], wellsite_dups_mult_sub, wellsite_dups_lat_lon), fill = T)[, `:=` (fD = NULL, fD1 = NULL)]
  
  # rename column
  setnames(icr_raw_wells, "well_type", "well_type_raw")
  
  # gather for output
  list(icr_raw_wells = icr_raw_wells, icr_raw_wellsites = icr_raw_wellsites)
  
}

# Clean data
clean_icr_data <- function(
  icr_surv_xlsx
){
  
  # address duplicate observations from raw data
  icr_raw = remove_dups_icr_data(icr_surv_xlsx)
  
  # assign data tables from list
  icr_raw_wells = icr_raw$icr_raw_wells
  icr_raw_wellsites = icr_raw$icr_raw_wellsites
  
  ## WELLS
  # characterize well status, type, and production level
  icr_raw_wells[, well_status := case_when(grepl("^producing", well_type_raw, ignore.case = T) | grepl("^active", well_type_raw, ignore.case = T) ~ "Active",
                                           grepl("permanent", well_type_raw, ignore.case = T) ~ "Abandoned",
                                           grepl("temp", well_type_raw, ignore.case = T) ~ "Temporarily Shut-In",
                                           TRUE ~ "Unknown/Other")]
  icr_raw_wells[, well_type := case_when(grepl("dry gas", well_type_raw, ignore.case = T) ~ "Dry Gas",
                                         grepl("wet gas", well_type_raw, ignore.case = T) ~ "Wet Gas",
                                         grepl("gas", well_type_raw, ignore.case = T) & !grepl("storage", well_type_raw, ignore.case = T) ~ "Unknown Gas",
                                         grepl("coal", well_type_raw, ignore.case = T) | grepl("CBM", well_type_raw, ignore.case = T) ~ "Coal Bed Methane",
                                         grepl("light", well_type_raw, ignore.case = T) ~ "Light Oil",
                                         grepl("heavy", well_type_raw, ignore.case = T) | grepl("heaby", well_type_raw, ignore.case = T) ~ "Heavy Oil",
                                         grepl("oil", well_type_raw, ignore.case = T) ~ "Unknown Oil",
                                         TRUE ~ "Unknown/Other")]
  icr_raw_wells[, well_type2 := case_when(grepl("gas", well_type, ignore.case = T) | grepl("methane", well_type, ignore.case = T) ~ "Gas",
                                          grepl("oil", well_type, ignore.case = T) ~ "Oil",
                                          TRUE ~ "Unknown/Other")]
  icr_raw_wells[, well_prod_lvl := case_when(grepl("non", well_type_raw, ignore.case = T) & grepl("pper", well_type_raw, ignore.case = T) ~ "Non-low Production",
                                             grepl("pper", well_type_raw, ignore.case = T) ~ "Low Production",
                                             TRUE ~ "Unknown/Other")]
  
  ## SITES
  # clean production information
  icr_raw_wellsites[, `:=` (gas_prod_flag = case_when(grepl("y|si|t", produces_natural_gas_for_sales, ignore.case = T) ~ "Yes",
                                                      grepl("n|f", produces_natural_gas_for_sales, ignore.case = T) ~ "No",
                                                      TRUE ~ NA_character_),
                            liq_prod_flag = case_when(grepl("y|si|t|oil|cond", produces_crude_oil_condensate, ignore.case = T) ~ "Yes",
                                                      grepl("n|f", produces_crude_oil_condensate, ignore.case = T) ~ "No",
                                                      TRUE ~ NA_character_))]
  
  # clean flares
  icr_raw_wellsites[, has_flare := case_when(grepl("y|si|t", flare_at_site, ignore.case = T) ~ "Yes",
                                             grepl("m|n|0|f", flare_at_site, ignore.case = T) ~ "No",
                                             TRUE ~ NA_character_)]
  # clean vintage information
  icr_raw_wellsites[, vintage := case_when(grepl("y", subject_to_OOOOa, ignore.case = T) ~ "Post-OOOOa",
                                           grepl("^n|f", subject_to_OOOOa, ignore.case = T) ~ "Pre-OOOOa",
                                           TRUE ~ NA_character_)]
  
  # set uninformative equipment and tank entries to NA
  icr_raw_wellsites[, `:=` (num_recip_compr = fifelse(num_recip_compressors == well_site_id | num_recip_compressors == "Y", NA_character_, 
                                                      num_recip_compressors),
                            num_dry_seal_compr = fifelse(num_dry_seal_compressors == well_site_name, NA_character_, num_dry_seal_compressors),
                            num_small_tanks = fifelse(grepl("error", num_atmos_storage_tanks_less_10bbl_per_day, ignore.case = T), 
                                                      NA_character_, num_atmos_storage_tanks_less_10bbl_per_day),
                            num_large_tanks = fifelse(grepl("error", num_atmos_storage_tanks_greatequal_10bbl_per_day, ignore.case = T), 
                                                      NA_character_, num_atmos_storage_tanks_greatequal_10bbl_per_day))]
  
  # set text entries indicating absence of equipment equal to zero
  icr_raw_wellsites[, `:=` (num_sep = as.integer(fifelse(grepl("[a-z]", num_separators, ignore.case = T), "0", num_separators)),
                            num_deh = as.integer(fifelse(grepl("[a-z]", num_dehydrators, ignore.case = T), "0", num_dehydrators)),
                            num_recip_compr = as.integer(fifelse(grepl("[a-z]", num_recip_compr, ignore.case = T), "0", num_recip_compr)),
                            num_dry_seal_compr = as.integer(fifelse(grepl("[a-z]", num_dry_seal_compr, ignore.case = T), "0", num_dry_seal_compr)),
                            num_wet_seal_compr = as.integer(fifelse(grepl("[a-z]", num_wet_seal_compressors, ignore.case = T), "0", num_wet_seal_compressors)),
                            num_small_tanks = as.numeric(fifelse(grepl("[a-z]", num_small_tanks, ignore.case = T), "0", num_small_tanks)),
                            num_large_tanks = as.numeric(fifelse(grepl("[a-z]", num_large_tanks, ignore.case = T), "0", num_large_tanks)))]
  
  # identify sites with equipment inventories
  icr_raw_wellsites[, equip.inv := case_when(!(is.na(num_sep) | is.na(num_deh) | is.na(num_recip_compr) | is.na(num_dry_seal_compr) | 
                                                 is.na(num_wet_seal_compr) | is.na(num_small_tanks) | is.na(num_large_tanks)) ~ "Full",
                                             is.na(num_sep) & is.na(num_deh) & is.na(num_recip_compr) & is.na(num_dry_seal_compr) & 
                                               is.na(num_wet_seal_compr) & is.na(num_small_tanks) & is.na(num_large_tanks) ~ "None",
                                             TRUE ~ "Partial")][, equip.inv.cum := fifelse(equip.inv == "None", "No", "Yes")]
  
  # assign zero equipment counts to blank entries for sites with partial inventories
  icr_raw_wellsites[, `:=` (num_sep = fifelse(equip.inv == "Partial" & is.na(num_sep), 0, num_sep),
                            num_deh = fifelse(equip.inv == "Partial" & is.na(num_deh), 0, num_deh),
                            num_recip_compr = fifelse(equip.inv == "Partial" & is.na(num_recip_compr), 0, num_recip_compr),
                            num_dry_seal_compr = fifelse(equip.inv == "Partial" & is.na(num_dry_seal_compr), 0, num_dry_seal_compr),
                            num_wet_seal_compr = fifelse(equip.inv == "Partial" & is.na(num_wet_seal_compr), 0, num_wet_seal_compr),
                            num_small_tanks = fifelse(equip.inv == "Partial" & is.na(num_small_tanks), 0, num_small_tanks),
                            num_large_tanks = fifelse(equip.inv == "Partial" & is.na(num_large_tanks), 0, num_large_tanks))]
  
  # gather for output
  list(icr_clean_wells = icr_raw_wells, icr_clean_wellsites = icr_raw_wellsites)
  
}

# Merge well and well site data
comb_icr_well_and_site_data <- function(
  icr_surv_xlsx
){
  
  # load cleaned data
  icr_clean = clean_icr_data(icr_surv_xlsx)
  
  # assign data tables from list
  icr_clean_wells = icr_clean$icr_clean_wells
  icr_clean_wellsites = icr_clean$icr_clean_wellsites
  
  # aggregate well level variables to site level
  # restrict sample to producing or temporarily shut-in wells with known products and production levels, calculate number of oil/gas, LP/non-LP wells at each site
  icr_clean_well_site = icr_clean_wells[well_status %in% c("Active") & !(grepl("unknown", well_type2, ignore.case = T)) & !(well_prod_lvl == "Unknown/Other")
  ][, .(wells_at_site = .N,
        LP_gas_wells = sum(1*(well_type2 == "Gas" & well_prod_lvl == "Low Production")),
        NLP_gas_wells = sum(1*(well_type2 == "Gas" & well_prod_lvl == "Non-low Production")),
        LP_oil_wells = sum(1*(well_type2 == "Oil" & well_prod_lvl == "Low Production")),
        NLP_oil_wells = sum(1*(well_type2 == "Oil" & well_prod_lvl == "Non-low Production"))),
    by = .(operator_name, well_site_id_name)]
  
  # combine information from site- and well-level datasets
  icr_wellsites = merge.data.table(icr_clean_wellsites, icr_clean_well_site, by = c("operator_name", "well_site_id_name"), all.x = T)
  
  # characterize sites as single or multi
  icr_wellsites[, site_bin := case_when(wells_at_site == 1 ~ "Single", wells_at_site > 1 ~ "Multi", TRUE ~ NA_character_)]
  
  # characterize sites as oil or gas (determined by which type of well the site has more of, ties to oil)
  icr_wellsites[, site_type := case_when(LP_gas_wells + NLP_gas_wells > LP_oil_wells + NLP_oil_wells ~ "Gas", 
                                         LP_gas_wells + NLP_gas_wells <= LP_oil_wells + NLP_oil_wells ~ "Oil", 
                                         TRUE ~ NA_character_)]
  
  # characterize sites as low or non-low production (assign site as low production if all wells on site are low production, otherwise assign as non-low production)
  icr_wellsites[, site_prod_lvl := case_when(NLP_gas_wells | NLP_oil_wells > 0 ~ "Non-low Production", 
                                             NLP_gas_wells + NLP_oil_wells == 0 ~ "Low Production", 
                                             TRUE ~ NA_character_)]
  
  # assign each site to an equipment bin
  icr_wellsites[, equip_bin := fcase(num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr + 
                                           num_small_tanks + num_large_tanks == 0, "1: No equipment, no tanks",
                                         num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr == 0 & 
                                           num_small_tanks + num_large_tanks > 0, "2: Tanks, no equipment",
                                         num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr == 1 & 
                                           num_small_tanks + num_large_tanks == 0, "3: Equipment = 1, no tanks",
                                         num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr > 1 & 
                                           num_small_tanks + num_large_tanks == 0, "4: Equipment > 1, no tanks",
                                         num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr == 1 & 
                                           num_small_tanks + num_large_tanks > 0, "5: Equipment = 1 and tanks",
                                         num_sep + num_deh + num_recip_compr + num_dry_seal_compr + num_wet_seal_compr > 1 & 
                                           num_small_tanks + num_large_tanks > 0, "6: Equipment > 1 and tanks",
                                         default = NA_character_)]
  
  # gather for output
  icr_wellsites
  
}

# Calculate equipment bin proportions
get_icr_equip_props <- function(
  icr_wellsites
){
  
  # sum well level data into blocks
  icr_equip_prop = icr_wellsites[equip.inv.cum %in% c("Yes") & !is.na(site_type) & !is.na(site_prod_lvl),
                               .(N = .N), by = .(site_type, site_prod_lvl, site_bin, equip_bin)][order(site_type, site_prod_lvl, -site_bin, equip_bin)]
  
  # calculate proportions
  icr_equip_prop[, N.type.lvl := sum(N), by = .(site_type, site_prod_lvl, site_bin)][, Prop.type.lvl.tier := N/N.type.lvl]
  
  # gather for output
  icr_equip_prop[, .(site_type, site_prod_lvl, site_bin, equip_bin, prop = Prop.type.lvl.tier)]
  
}

# Calculate equipment averages
get_icr_equip_avg <- function(
  icr_wellsites
){
  
  # calculate average equipment counts
  icr_equip_avg = icr_wellsites[equip.inv.cum %in% c("Yes") & !is.na(wells_at_site)
  ][, .(wells_gas = sum(LP_gas_wells + NLP_gas_wells), wells_oil = sum(LP_oil_wells + NLP_oil_wells),
        sep_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*num_sep),
        sep_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*num_sep),
        deh_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*num_deh),
        deh_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*num_deh),
        recip_compr_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*num_recip_compr),
        recip_compr_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*num_recip_compr),
        cent_compr_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*(num_dry_seal_compr+num_wet_seal_compr)),
        cent_compr_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*(num_dry_seal_compr+num_wet_seal_compr)),
        small_tanks_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*num_small_tanks),
        small_tanks_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*num_small_tanks),
        large_tanks_gas = sum(((LP_gas_wells + NLP_gas_wells)/wells_at_site)*num_large_tanks),
        large_tanks_oil = sum(((LP_oil_wells + NLP_oil_wells)/wells_at_site)*num_large_tanks)),
    by = .(site_prod_lvl, site_bin, equip_bin)
  ][, .(site_prod_lvl, site_bin, equip_bin,
        sep_well_gas_icr = sep_gas/wells_gas,
        sep_well_oil_icr = sep_oil/wells_oil,
        deh_well_gas_icr = deh_gas/wells_gas,
        deh_well_oil_icr = deh_oil/wells_oil,
        recip_compr_well_gas_icr = recip_compr_gas/wells_gas,
        recip_compr_well_oil_icr = recip_compr_oil/wells_oil,
        cent_compr_well_gas_icr = cent_compr_gas/wells_gas,
        cent_compr_well_oil_icr = cent_compr_oil/wells_oil,
        small_tanks_well_gas_icr = small_tanks_gas/wells_gas,
        small_tanks_well_oil_icr = small_tanks_oil/wells_oil,
        large_tanks_well_gas_icr = large_tanks_gas/wells_gas,
        large_tanks_well_oil_icr = large_tanks_oil/wells_oil)
  ][order(site_prod_lvl, -site_bin, equip_bin)]
  
}