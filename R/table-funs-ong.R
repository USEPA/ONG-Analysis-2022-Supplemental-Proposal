#' table-funs-ong.R
#' These functions facilitate table formatting for O&G analyses.

# Make scenario comparison tables
scn_comp_table <- function(
  scn_comp, 
  var_list = NULL,
  row_detail = NA_character_,
  show_years = c(2020, 2025),
  mp_list = NULL,
  mp_attrs_list = NULL
) {
  
  if(is.null(mp_list)) {mp_list <- rlang::caller_env()$mp_list}
  if(is.null(mp_attrs_list)) {mp_attrs_list <- rlang::caller_env()$mp_attrs_list}
  
  var_list <- syms(var_list)
  
  table_data <- scn_comp %>%
    
    filter(year %in% show_years) %>%
    
    left_join(mp_list, by = c("mp", "segment")) %>% 
    left_join(mp_attrs_list, by = c("mp", "segment", "attrs")) %>% {
      
      switch(row_detail,
             "total"    = row_table_total(., var_list, mp_list, mp_attrs_list),
             "overview" = row_table_overview(., var_list, mp_list, mp_attrs_list),
             "source"   = row_table_source(., var_list, mp_list, mp_attrs_list),
             "gascomp"  = row_table_gascomp(., var_list, mp_list, mp_attrs_list),
             "detail"   = row_table_detail(., var_list, mp_list, mp_attrs_list),
             "mp"       = .)
    }
  
  table_data
}


row_table_total <- function(
  raw_table, 
  var_list, 
  mp_list, 
  mp_attrs_list
) {
  
  result_table <- raw_table %>%
    group_by(year) %>%
    summarize_at(.vars = vars(!!!var_list),
                 .funs = ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(year) %>%
    select(year, !!!var_list)
  
  result_table  
}

row_table_overview <- function(
  raw_table, 
  var_list, 
  mp_list, 
  mp_attrs_list
) {
  
  # derived lists for consistent ordering
  src_order <- mp_list$source %>% unique()
  
  result_table <- raw_table %>%
    group_by(year, source) %>%
    summarize_at(.vars = vars(!!!var_list),
                 .funs = ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(source = factor(source, levels = src_order)) %>%
    arrange(year, source) %>%
    select(year, source, !!!var_list)
  
  result_table  
}

row_table_source <- function(
  raw_table, 
  var_list, 
  mp_list, 
  mp_attrs_list
) {
  
  # derived lists for consistent ordering
  src_order <- mp_list$source %>% unique()
  det2_order <- mp_attrs_list$detail_2 %>% unique()
  
  result_table <- raw_table %>%
    group_by(year, source, detail_2) %>%
    summarize_at(.vars = vars(!!!var_list),
                 .funs = ~sum(., na.rm = TRUE)) %>%
    ungroup()  %>%
    mutate(source = factor(source, levels = src_order)) %>%
    mutate(detail_2 = factor(detail_2, levels = det2_order)) %>%
    arrange(year, source, detail_2) %>%
    select(year, source, detail_2, !!!var_list)
  
  result_table  
}

row_table_gascomp <- function(
  raw_table, 
  var_list, 
  mp_list, 
  mp_attrs_list
) {
  
  result_table <- raw_table %>%
    group_by(year, gascomp_category) %>%
    summarize_at(.vars = vars(!!!var_list),
                 .funs = ~sum(., na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(year, gascomp_category) %>%
    select(year, gascomp_category, !!!var_list)
  
  result_table  
}

row_table_detail <- function(
  raw_table, 
  var_list, 
  mp_list, 
  mp_attrs_list
) {
  
  # derived lists for consistent ordering
  src_order <- mp_list$source %>% unique()
  det2_order <- mp_attrs_list$detail_2 %>% unique()
  det3_order <- mp_attrs_list$detail_3 %>% unique()
  
  result_table <- raw_table %>%
    group_by(year, source, detail_2, detail_3) %>%
    summarize_at(.vars = vars(!!!var_list),
                 .funs = ~sum(., na.rm = TRUE)) %>%
    ungroup()  %>%
    mutate(source = factor(source, levels = src_order)) %>%
    
    mutate(detail_2 = factor(detail_2, levels = det2_order)) %>%
    mutate(detail_3 = factor(detail_3, levels = det3_order)) %>%
    arrange(year, source, detail_2, detail_3) %>%
    select(year, source, detail_2, detail_3, !!!var_list)
  
  result_table  
}


#### Specific Tables --------

# Make summary cost table, with PV and EAV totals
table_cost_disc_pv_eav <- function(
  scn_comp, 
  r, 
  var_list, 
  show_years = 2021:2035, 
  disc_flag = TRUE, 
  base_year = 2021, 
  start_year = 2023, 
  end_year = 2035, 
  mp_list = NULL, 
  mp_attrs_list = NULL
) {
  
  if(is.null(mp_list)) {mp_list <- rlang::caller_env()$mp_list}
  if(is.null(mp_attrs_list)) {mp_attrs_list <- rlang::caller_env()$mp_attrs_list}
  
  ann_r_str      <- glue("ann_{r * 100}")
  ann_r_wgas_str <- glue("ann_{r * 100}_wgas")
  
  if ("ann_r" %in% var_list) {
    var_list <- str_replace(var_list, "ann_r", ann_r_str)}
  if ("ann_r_wgas" %in% var_list) {
    var_list <- str_replace(var_list, "ann_r_wgas", ann_r_wgas_str)}
  
  var_list_syms <- syms(var_list)
  
  main_undisc <- scn_comp_table(scn_comp,
                                var_list = var_list,
                                row_detail = "total", 
                                show_years = show_years)
  
  main_disc <- main_undisc %>%
    mutate_at(vars(!!!var_list),
              ~npv_xy_pmts(as.numeric(year), ., r, base_year))
  
  pv_r <- main_undisc %>%
    summarize_at(vars(!!!var_list),
                 ~sum(npv_xy_pmts(as.numeric(year), ., r, base_year)))
  
  eav_r <- mutate_all(pv_r, .funs = ~equiv_annualized_value(., n = end_year - start_year + 1, r, first_pmt_t = 1))
  
  if (disc_flag) {
    result <- bind_rows(
      main_disc,
      mutate(pv_r, year = "PV"),
      mutate(eav_r, year = "EAV")
    )
  } else {
    result <- bind_rows(
      main_undisc,
      mutate(pv_r, year = "PV"),
      mutate(eav_r, year = "EAV")
    )
  }
  
  result
}