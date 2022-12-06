#' emissions-factors-bser-components.R
#' These functions estimate and output per-component fugitive emission rates
#' for the BSER model plants.

# Estimate methane emissions per component from BSER model plants
get_bser_component_ef <- function(fug_param, mp_comp,
                                  .fug_leak_rate = "0.50%", .fug_repair_rate = "30 day") {

  comp_dta <- unique(fug_param[(fug_leak_rate == .fug_leak_rate) & (fug_repair_rate == .fug_repair_rate) & (fug_mp != 4),
                                         .(fug_mp, fug_mp_emissions)]) %>% merge.data.table(mp_comp, by = "fug_mp")

  comp_lm <- lm(fug_mp_emissions ~ components, data = comp_dta)
  comp_ef <- coef(summary(comp_lm))["components", "Estimate"]

  comp_ef

}

get_bser_component_ef_all <- function(fug_param, mp_comp, leak_rates = c("1%", "0.50%"), repair_rates = c("30 day", "10 day")) {
 
 # get bser component emission factors for all leak/repair combos into table (repair rate doesn't matter)
 v.get <- Vectorize(get_bser_component_ef, vectorize.args = c(".fug_leak_rate", ".fug_repair_rate"))
 res <- expand.grid(fug_leak_rate = leak_rates, fug_repair_rate = repair_rates) %>%
   mutate(bser_component_ef = v.get(fug_param, mp_comp, fug_leak_rate, fug_repair_rate))
 as.data.table(res)

}
