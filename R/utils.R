#' utils.R
#' These functions provide a series of data formatting utilities useful for 
#' O&G analyses.

CJ.table <- function(X,Y)
  setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]


parse_date_multi <- function(x, formats) {
  parse_formats_list <- map(formats, ~as.Date(x, format = .x))
  coalesce(!!! parse_formats_list)
}


transform_ad_proj <- function(.data, level) {
  
  if(level == "National") {
    .data %>% 
      as_tibble() %>%
      rename(mp = Model.Plant, segment = Segment, attrs = Type, year = Year,
             vintage_bin = Reg.Bin, vint_trunc = Vint.Trunc, fac_count = Fac.Count) %>%
      mutate(location = "USA") 
  }
  
  else if(level == "State") {
    .data %>%
      as_tibble() %>%
      rename(mp = Model.Plant, segment = Segment, attrs = Type, year = Year,
             vintage_bin = Reg.Bin, vint_trunc = Vint.Trunc, fac_count = Fac.Count, location = State) 
  }
  
  else stop("Level of ad projections must be State or National")
  
}

transform_ad_proj_pneu <- function(.data, level) {
  
  if(level == "National") {
    .data %>% 
      as_tibble() %>%
      rename(mp = Model.Plant, segment = Segment, attrs = Type, year = Year,
             vintage_bin = Reg.Bin, vint_trunc = Vint.Trunc, fac_count = Fac.Count,
             lb_count_stn = LB.Count.Station, ib_count_stn = IB.Count.Station, 
             hb_count_stn = HB.Count.Station, pump_count_stn = Pump.Count.Station) %>%
      mutate(location = "USA") 
  }
  
  else if(level == "State") {
    .data %>%
      as_tibble() %>%
      rename(mp = Model.Plant, segment = Segment, attrs = Type, year = Year,
             vintage_bin = Reg.Bin, vint_trunc = Vint.Trunc, fac_count = Fac.Count,
             lb_count_stn = LB.Count.Station, ib_count_stn = IB.Count.Station, 
             hb_count_stn = HB.Count.Station, pump_count_stn = Pump.Count.Station, 
             location = State) 
  }
  
  else stop("Level of ad projections must be State or National")
  
}


scn_summ_tab <- function(
  comp_dat, 
  mp_list = mp_list, 
  mp_attrs_list = mp_attrs_list
) {
  scn_comp_table(comp_dat,
                 var_list = list("Methane", "VOC", "HAP", "CH4_CO2e",
                                 "capital_cost", "annual_cost", "gas_revenue",
                                 "ann_7", "ann_7_wgas", "ann_3", "ann_3_wgas"),
                 row_detail = "total",
                 show_years = 2019:2035,
                 mp_list = mp_list,
                 mp_attrs_list = mp_attrs_list)
}

# Target factory to Make a Git-Stable Version of meta
tar_meta_stable <- function(
  name = meta_stable,
  deps = NULL,
  path_meta_out = fs::path(tar_config_get("store"), "meta", "meta-stable"),
  cue = targets::tar_cue("always")
) {
  
  # from targets::tar_target:
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_nzchar(name)
  
  # add deps into the command
  command <- rlang::expr({
    
    rlang::expr({{ deps }}) # TODO: better way to handle multiple deps?
    
    meta <- targets::tar_meta() %>%
      filter(name != "meta_stable") %>%
      select(-time, -seconds) %>%
      arrange(name) %>%
      mutate(across(everything(), ~format(.x)))
    
    meta_tbl <- bind_rows(
      set_names(names(meta), names(meta)),
      meta
    ) %>%
      mutate(across(everything(), ~format(.x)))
    
    lines <- reduce(as.list(meta_tbl), ~str_c(.x, .y, sep = "  "))
    
    write_lines(lines, !!path_meta_out)
      
  })
  
  targets::tar_target_raw(
    name = name, 
    command = command,
    cue = cue
  )
}

rnd_tbl <- function(data) mutate(data, across(where(is.numeric), ~signif(.x, digits = 3)))

# Write out a column-aligned text table, e.g., for convenient change tracking
write_text_tbl <- function(x, file) {
  x_data <- mutate(x, across(everything(), ~format(.x, big.mark = ",", justify = "right")))
  tbl <- bind_rows(
    set_names(names(x), names(x)),
    x_data
  ) %>%
    mutate(across(everything(), ~format(.x, justify = "right")))
  
  lines <- reduce(as.list(tbl), ~str_c(.x, .y, sep = "  "))
  write_lines(lines, file)
}


