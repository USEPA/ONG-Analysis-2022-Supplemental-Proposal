#' wellsite-bins-table.R
#' These functions calculate and format summary statistics for well site
#' equipment bin output tables.  

group_wellsite_bins_data <- function(
  .dta
) {
  
  # create variable to group data into appropriate table rows
  res <- .dta %>%
    mutate(
      group_1 = case_when(
        Type == "Gas" ~ "Natural Gas",
        Type == "Oil" ~ "Oil"
      ),
      group_2 = case_when(
        Site.Type.Bin == "Single" ~ "Single-Wellhead",
        Site.Type.Bin == "Multi" ~ "Multi-Wellhead"
      ),
      group_3 = case_when(
        str_detect(equip_bin, "^1:") ~ "Wellhead Only",
        str_detect(equip_bin, "^[23]:") ~ "One piece of equipment, including tank batteries",
        str_detect(equip_bin, "^[456]:") ~ "More than one piece of equipment, including tank batteries"
      )
    ) %>%
    mutate(wellsite_group = paste(group_1, group_2, group_3, sep = ", ")) %>%
    select(!starts_with("group")) %>%
    relocate(wellsite_group)
  
}

sort_wellsite_bins_table <- function(
  .dta
) {
 
  # sort data into table sort-order
  res <- .dta %>%
    mutate(
      sort_1 = case_when(
        Type == "Gas" ~ 1,
        Type == "Oil" ~ 2
      ),
      sort_2 = case_when(
        str_detect(wellsite_group, "Single") ~ 1,
        str_detect(wellsite_group, "Multi") ~ 2
      ),
      sort_3 = case_when(
        str_detect(wellsite_group, "Wellhead Only") ~ 1,
        str_detect(wellsite_group, "More than") ~ 3,
        TRUE ~ 3
      )
    ) %>%
    arrange(sort_1, sort_2, sort_3) %>%
    select(!starts_with("sort"))
  
}

get_wellsite_bins_table_data <- function(
  .dta
) {
  
  # select data for table
  .dta %>%
    select(Block.ID, Year, Type, Site.Type.Bin, equip_bin, Wellsite.Count) %>% 
    group_wellsite_bins_data() %>%
    as_tibble()
  
}

calc_wellsite_bin_proportions <- function(
  .dta
) {
  
  get_wellsite_bins_table_data(.dta) %>% 
    mutate(pivot_group = ifelse(Year == 2019, "base", "projection")) %>%
    group_by(Type, pivot_group) %>%
    mutate(total_wellsites = sum(Wellsite.Count), .groups = "drop") %>%
    group_by(wellsite_group, Type, pivot_group) %>%
    summarise(group_wellsites = sum(Wellsite.Count), 
              total_wellsites = mean(total_wellsites),
              .groups = "drop") %>%
    mutate(group_prop = group_wellsites / total_wellsites) %>%
    select(wellsite_group, group_prop, pivot_group, Type) %>%
    pivot_wider(names_from = pivot_group,
                values_from = group_prop) %>%
    sort_wellsite_bins_table()
  
}

  
  
  
  
  
  
  
  
  
  
  
