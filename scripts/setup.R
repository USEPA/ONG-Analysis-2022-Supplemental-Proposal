#' setup.R
#' This script loads libraries and source functions used in the project.

options(scipen = 999999999)

missing_packages <- setdiff(
  c("tidyverse", "readxl", "data.table", "dtplyr", "zoo", "targets", "tarchetypes", "kableExtra", 
    "extrafont", "ggplot2", 'ggthemes', "RColorBrewer", "here", "fs", "glue", "openxlsx"), installed.packages())

if (length(missing_packages) != 0) {
  warning("The following packages used in this project are missing: ", paste0(missing_packages, collapse = ", "))
}

# load libraries
library(tidyverse)
library(readxl)
library(data.table)
library(dtplyr)
library(zoo)

# targets pipeline
library(targets)
library(tarchetypes)

# reporting
library(kableExtra)

# figures
library(extrafont)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# utils
library(here)
library(fs)
library(glue)

if (!require(openxlsx)) {
  warning("Package openxlsx not available, cannot write output to xlsx.")
}

# emissions factor functions
source(here("R/inputs-ghgi.R"))
source(here("R/emissions-factors-subpartW.R"))
source(here("R/emissions-factors-rutherford.R"))
source(here("R/emissions-factors-bser-components.R"))

# decline rate calculation functions
source(here("R/production-data-prism.R"))

# 2016 ICR data processing functions
source(here("R/icr-data-processing.R"))

# activity data construction functions
source(here("R/activity-nonwellsites.R"))
source(here("R/activity-wellsites-step-1.R"))
source(here("R/activity-wellsites-step-2.R"))

# emissions calculations (for well sites) functions
source(here("R/emissions-wellsites.R"))

# model plant fate assignment functions
source(here("R/mpf-funs-supplemental.R"))

# control cost and emissions parameters
source(here("R/cost-emissions.R"))
source(here("R/read-cost-and-emissions-xlsx.R"))

# cost and emissions calculation functions
source(here("R/calc-funs-ong.R"))
source(here("R/npv-math-funs.R"))
source(here("R/scenario-calcs-wellsite-fugitives.R"))
source(here("R/scenario-calcs-wellsite-tanks.R"))
source(here("R/scenario-calcs-wellsite-pneumatics.R"))
source(here("R/scenario-calcs-nonwellsite.R"))
source(here("R/secondary-emissions-funs-ong.R"))

# small business calculation functions
source(here("R/get-sb-data.R"))
source(here("R/calc-sb-impacts.R"))

# output formatting functions
source(here("R/table-funs-ong.R"))
source(here("R/sc-ch4-funs-ong.R"))
source(here("R/fac-type.R"))
source(here("R/xlsx-output.R"))
source(here("R/format-funs.R"))
source(here("R/utils.R"))
source(here("R/wellsite-bins-table.R"))

# factor variable orders:
vintage_bin_order <- c("OOOOc", "OOOO", "OOOOa", "OOOOb")
segment_order <- c("PROD", "G&B", "PROC", "TRANS", "STOR")

# analysis bounds
base.yr = 2019
end.yr = 2035
prop.sign.yr = 2021 # signature year for proposal (determines initial vintage subject to NSPS OOOOb)
nsps.start.yr = 2023 # assumed year that NSPS takes effect
eg.start.yr = 2026 # assumed year that EG takes effect

effective.date.OOOO  <- as.Date("2011-08-24")
effective.date.OOOOa <- as.Date("2015-09-18")
effective.date.OOOOb <- as.Date("2022-09-01")

scenarios <- c("Proposal", "Less Strict", "More Strict")

# conversion factors
methane_per_whole_gas <- list(Gas = .788, Oil = .612)
gas_to_methane_prod <- .829
gas_to_methane_proc <- .87
gas_to_methane_trans <- .928
toc_to_methane <- .694769294934942
methane_to_voc_prod = .277974703
methane_to_voc_trans = 0.0276746887929933
voc_to_hap_prod = .03767
voc_to_hap_trans = 0.0297016948434322

gas_density <- 19.26
scf_to_kg <- gas_density/1000
metric_tons_per_short_ton <- 0.90718474
lbs_per_scf <- .04163
tons_per_lb <- 1/2000
lbs_per_kg <- 2.20462
methane_ton_per_mcf <- lbs_per_scf*tons_per_lb*1000
methane_mcf_per_ton <- 1/methane_ton_per_mcf

GWP_CH4 <- 25

hours_per_year <- 8760
days_per_year <- 365

# Plot Options

# Load extra fonts
# font_install("fontcm")
# loadfonts(device = "win")
loadfonts()

# plot specs
ppi = 300
textsize = 14

# plot themes
theme.main = theme_bw() + 
  theme(
    text = element_text(family = "CM Roman", size = textsize)
  )

# plot dimensions
single.w.mult = 10
single.h.mult = 6
