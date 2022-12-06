#' calc-funs-ong.R
#' These functions aid with impacts calculations.

# Takes two scenario projections and produces comparison results
compare_two_scenarios <- function(
  scn_bau, 
  scn_policy
) {
  
  # variables of interest (summing over fate...)
  mp_char_vars <- rlang::syms(list("mp", "attrs", "segment", "location", "vintage_bin"))
  
  var_list <- rlang::syms(list(#"fac_count", 
                        "Methane", "VOC", "HAP", "CH4_CO2e",
                        "flare_wholegas", "gas_capture", "gas_revenue",
                        "capital_cost", "annual_cost", 
                        "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas"))
  
  var_list_c <- as.character(var_list)
  
  
  # combine data from the 2 scenarios
  scn_both <- bind_rows(
    mutate(scn_bau, scn = "bau"),
    mutate(scn_policy, scn = "policy")
  )
    
  # to calculate change, negate the "bau" values, so a reduction would be negative
  scn_comp <- scn_both %>% 
    mutate(fac_diff = fac_count) %>%
    mutate(fac_count = if_else(scn == "bau", fac_count, 0)) %>%
    
    mutate(across(.cols = all_of(c("fac_diff", var_list_c)),
                  .fns = ~if_else(scn == "bau", .x * -1, .x))) %>%
    
  
  # sum over scenarios
    group_by(!!!mp_char_vars, fate, year) %>%
    summarize(across(.cols = all_of(c("fac_count", "fac_diff", var_list_c)),
                     .fns = ~sum(., na.rm = TRUE)),
              .groups = "drop") %>%

  # affected facilities are mp's w/ different fate in policy than bau
    mutate(fac_affected = pmax(fac_diff, 0)) %>%
  
  # sum over fates
    group_by(!!!mp_char_vars, year) %>%
    summarize(across(.cols = all_of(c("fac_count", "fac_diff", "fac_affected", var_list_c)),
                     .fns = ~sum(., na.rm = TRUE)),
              .groups = "drop") %>%
    
  # convert year variables to character
    mutate(year = as.character(year),
           #vintage = as.character(vintage) # since vintage -> vintage_bin, already character
           )
    
  scn_comp
}

# Calculate annualized costs for a vector of projections
annualized_cost <- function(
  contr_life, 
  r, 
  cap_cost, 
  annual_cost, 
  gas_rev = NA_real_
) {
  
  if (! is.numeric(contr_life) & 
      is.numeric(r) &
      is.numeric(cap_cost) & 
      is.numeric(annual_cost) & 
      is.numeric(gas_rev)
  ) stop("Arguments to `annualized cost` must be numeric")
  
  if (any(is.infinite(cap_cost)) |
      any(is.infinite(annual_cost)) |
      any(is.infinite(gas_rev))
  ) stop("Arguments to `annualized cost` must be finite.")
  
  if (isTRUE(min(contr_life) < 0)) stop("Argument control lifetime must be >= 0")
  
  gas_rev <- if_else(is.na(gas_rev), 0, gas_rev)
  
  result <- case_when(
    is.na(contr_life) ~ NA_real_,  
    contr_life == 0 ~ cap_cost + annual_cost - gas_rev,
    TRUE ~ 
      equiv_annualized_value(cap_cost + 
                               npv_fixed_pmts(annual_cost - gas_rev, contr_life, r), contr_life, r)
    
  )
  
  result
}