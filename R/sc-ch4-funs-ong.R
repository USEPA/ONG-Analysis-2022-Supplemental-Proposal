#' sc-ch4-funs-ong.R
#' These functions perform calculations and format output tables for social
#' cost of methane.

# Returns methane emissions reductions with sc-ch4 in each year
# Methane reductions positive to facilitate PV calcs
# sc-ch4 in 2019$/short ton
get_sc_ch4_data <- function(
  scn_comp, 
  sc_ch4,
  mp_list,
  mp_attrs_list
) {
  
  scn_comp_table(scn_comp,
                 var_list = list("Methane"),
                 row_detail = "total",
                 show_years = 2023:2035, 
                 mp_list = mp_list,
                 mp_attrs_list = mp_attrs_list) %>%
    mutate(Methane = -Methane) %>%
    left_join(sc_ch4, by = c("year"))
  
}

# Table of undiscounted sc-ch4 reduction benefits
# Row for each year, column for each sc-ch4 value
get_undisc_cost_table <- function(
  scn_comp, 
  sc_ch4,
  mp_list,
  mp_attrs_list
) {
  
  dta <- get_sc_ch4_data(scn_comp, sc_ch4,
                         mp_list,
                         mp_attrs_list)
  
  dta %>%
    mutate(across(.cols = contains("CH4"), .fns = ~.x*Methane/1000000, .names = "{.col}_undisc")) %>%
    select(year, contains("undisc"))
  
}

# Table of discounted sc-ch4 reduction benefits
# Row for each year, column for each sc-ch4 value
get_disc_cost_table <- function(
  scn_comp, 
  sc_ch4,
  mp_list,
  mp_attrs_list
) {
  
  dta <- get_undisc_cost_table(scn_comp, sc_ch4,
                               mp_list,
                               mp_attrs_list)
  
  dta %>% mutate(t = as.numeric(year) - 2021,
                 `5.0%_CH4_disc` = `5.0%_CH4_undisc`*(1/(1.05^t)),
                 `3.0%_CH4_disc` = `3.0%_CH4_undisc`*(1/(1.03^t)),
                 `2.5%_CH4_disc` = `2.5%_CH4_undisc`*(1/(1.025^t)),
                 `3% 95th Pct._CH4_disc` = `3% 95th Pct._CH4_undisc`*(1/(1.03^t)),
                 `2.5%_NCEE_CH4_disc` = `2.5%_NCEE_CH4_undisc`*(1/(1.025^t)),
                 `2.0%_NCEE_CH4_disc` = `2.0%_NCEE_CH4_undisc`*(1/(1.020^t)),
                 `1.5%_NCEE_CH4_disc` = `1.5%_NCEE_CH4_undisc`*(1/(1.015^t)),
                 `3.0%_domestic_CH4_disc` = `3.0%_domestic_CH4_undisc`*(1/(1.03^t)),
                 `7.0%_domestic_CH4_disc` = `7.0%_domestic_CH4_undisc`*(1/(1.07^t)),
                 `2.5%_domestic_CH4_disc` = `2.5%_domestic_CH4_undisc`*(1/(1.025^t)),
                 `2.0%_domestic_CH4_disc` = `2.0%_domestic_CH4_undisc`*(1/(1.025^t))) %>%
    select(year, contains("_disc"))
  
}

# Calculates PV/EAV for each column of discounted benefits
get_pv_eav_sc_ch4 <- function(
  cost_disc_table
) {
  
  pv <- cost_disc_table %>% summarize(across(contains("_disc"), .fns = ~sum(.x), .names = "{.col}"))
  eav <- pv %>%  mutate(`5.0%_CH4_disc` = `5.0%_CH4_disc`*(.05 / (1 - 1.05^-(2035-2023+1))),
                        `3.0%_CH4_disc` = `3.0%_CH4_disc`*(.03 / (1 - 1.03^-(2035-2023+1))),
                        `2.5%_CH4_disc` = `2.5%_CH4_disc`*(.025 / (1 - 1.025^-(2035-2023+1))),
                        `3% 95th Pct._CH4_disc` = `3% 95th Pct._CH4_disc`*(.03 / (1 - 1.03^-(2035-2023+1))),
                        `2.5%_NCEE_CH4_disc` = `2.5%_NCEE_CH4_disc`*(.025 / (1 - 1.025^-(2035-2023+1))),
                        `2.0%_NCEE_CH4_disc` = `2.0%_NCEE_CH4_disc`*(.020 / (1 - 1.020^-(2035-2023+1))),
                        `1.5%_NCEE_CH4_disc` = `1.5%_NCEE_CH4_disc`*(.015 / (1 - 1.015^-(2035-2023+1))),
                        `3.0%_domestic_CH4_disc` = `3.0%_domestic_CH4_disc`*(.030 / (1 - 1.030^-(2035-2023+1))),
                        `7.0%_domestic_CH4_disc` = `7.0%_domestic_CH4_disc`*(.070 / (1 - 1.070^-(2035-2023+1))),
                        `2.5%_domestic_CH4_disc` = `2.5%_domestic_CH4_disc`*(.025 / (1 - 1.025^-(2035-2023+1))),
                        `2.0%_domestic_CH4_disc` = `2.0%_domestic_CH4_disc`*(.020 / (1 - 1.020^-(2035-2023+1))))
  rbind(pv, eav) %>% mutate(year = c("PV", "EAV")) %>% relocate(year)
  
}

# Returns table of discounted sc-ch4 reduction benefits along with a PV row and an EAV row
get_sc_ch4_table <- function(
  scn_comp, 
  sc_ch4, 
  mp_list, 
  mp_attrs_list
) {
  
  dta <- get_disc_cost_table(scn_comp, sc_ch4, mp_list, mp_attrs_list)
  
  rbind(dta, get_pv_eav_sc_ch4(dta))
  
}



