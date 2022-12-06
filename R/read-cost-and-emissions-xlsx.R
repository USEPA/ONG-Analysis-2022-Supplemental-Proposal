#' read-cost-and-emissions-xlsx.R
#' This function reads cost and emissions data for control options.

read_cost_and_emissions_xlsx_nonwellsite <- function(
  cost_and_emissions_base_xlsx = "data-raw/cost_and_emissions_inputs.xlsx"
) {
  
  sheets = c("Fugitives", "Compressors", "Liquids unloading")
  
  # read all sheets
  xlsx_raw <- map(
    set_names(sheets, sheets),
    ~readxl::read_excel(cost_and_emissions_base_xlsx, .x)
  )
  
  # process non-wellsite + liquids unloading
  equip_cost_emiss <- map_dfr(
    xlsx_raw,
    ~.x
  ) 
  
}

