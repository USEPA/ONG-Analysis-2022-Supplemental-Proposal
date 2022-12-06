#' fac-type.R
#' This function formats and categorizes affected facilities to facilitate
#' output reporting. 

add_fac_type <- function(.data) {
  
  stopifnot(all(c("source", "detail_2") %in% names(.data)))
  
  proc <- .data %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(fac_type = case_when(
      detail_2 == "Well Pad Fugitives" ~ "Well Sites",
      str_detect(detail_2, "Pneumatic Controller") ~ "Pneumatic Controllers",
      str_detect(detail_2, "Pumps") ~ "Pneumatic Pumps",
      str_detect(detail_2, "Centrifugal Comp") ~ "Centrifugal Compressors",
      str_detect(detail_2, "Recip Comp") ~ "Reciprocating Compressors",
      is.na(detail_2) ~ source,
      TRUE ~ detail_2 
    )) %>%
    mutate(fac_type = fct_relevel(fac_type, 
                                  c("Well Sites", "G&B Stations", "T&S Compressor Stations", "Processing Plants",
                                    "Pneumatic Pumps", "Pneumatic Controllers", "Reciprocating Compressors", 
                                    "Centrifugal Compressors", "liquids_unloading", "Storage vessels"))) %>%
    select(fac_type)
  
  # to avoid changes to factor columns, etc
  bind_cols(.data, proc)
  
}

