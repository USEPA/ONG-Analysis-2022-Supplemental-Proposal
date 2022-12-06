#' emissions-wellsites.R
#' This function calculates and assigns no-control emissions factors to well 
#' sites based on equipment and other characteristics.

# Characterize wellsites emissions without controls
wellsites_no_control_emissions <- function(
  wellsite_bins_proj, 
  ghgi_equip_data, 
  ef_equip_subpartw, 
  ef_equip_rutherford, 
  comp_counts, 
  bser_component_ef
) {
  
  # restrict GHGI equipment data to production segment only
  ghgi_equip_data = ghgi_equip_data[Segment == "PROD"][, `:=` (Segment = NULL, Count = NULL)]
  
  # emissions factors and proportions
  fug_frac_ib_pnc = 1-.3/8.8 # from Subpart W proposal
  tank_crude_tp_to_voc = 0.214 / 365 # see see "Summary Stats" tab in "data-raw/ep_tanks.xlsx"
  tank_crude_tp_to_methane = 0.0444 / 365 # see see "Summary Stats" tab in "data-raw/ep_tanks.xlsx"
  tank_cond_tp_to_voc = 2.09 / 365 # see see "Summary Stats" tab in "data-raw/ep_tanks.xlsx"
  tank_cond_tp_to_methane = 0.461 / 365 # see see "Summary Stats" tab in "data-raw/ep_tanks.xlsx"

  # Fugitive methane emissions
  wellsite_bins_proj[, `:=` (Fug_Comp_CH4_Site_GHGI = 
                                   Gas.Wells.per.site*(ghgi_equip_data[Source == "Wellheads" & Type == "Gas", CH4_TPY] +
                                                         sep_well_gas*ghgi_equip_data[Source == "Separators" & Type == "Gas", CH4_TPY] +
                                                         head_well_gas*ghgi_equip_data[Source == "Headers" & Type == "Gas", CH4_TPY] +
                                                         deh_well_gas*ghgi_equip_data[Source == "Dehydrators" & Type == "Gas", CH4_TPY] +
                                                         compr_well_gas*ghgi_equip_data[Source == "Compressors" & Type == "Gas", CH4_TPY] +
                                                         heat_well_gas*ghgi_equip_data[Source == "Heaters" & Type == "Gas", CH4_TPY] +
                                                         heat_treat_well_gas*ghgi_equip_data[Source == "Heater/Treaters" & Type == "Gas", CH4_TPY] +
                                                         meters_well_gas*ghgi_equip_data[Source == "Meters/Piping" & Type == "Gas", CH4_TPY]) +
                                   Oil.Wells.per.site*(ghgi_equip_data[Source == "Wellheads" & Type == "Oil", CH4_TPY] +
                                                         sep_well_oil*ghgi_equip_data[Source == "Separators" & Type == "Oil", CH4_TPY] +
                                                         head_well_oil*ghgi_equip_data[Source == "Headers" & Type == "Oil", CH4_TPY] +
                                                         deh_well_oil*ghgi_equip_data[Source == "Dehydrators" & Type == "Oil", CH4_TPY] +
                                                         compr_well_oil*ghgi_equip_data[Source == "Compressors" & Type == "Oil", CH4_TPY] +
                                                         heat_well_oil*ghgi_equip_data[Source == "Heaters" & Type == "Oil", CH4_TPY] +
                                                         heat_treat_well_oil*ghgi_equip_data[Source == "Heater/Treaters" & Type == "Oil", CH4_TPY] +
                                                         meters_well_oil*ghgi_equip_data[Source == "Meters/Piping" & Type == "Oil", CH4_TPY]),
                                 Fug_Comp_CH4_Site_GHGRP = 
                                   Gas.Wells.per.site*(ef_equip_subpartw[Source == "Wellhead" & Type == "Gas", CH4_TPY] +
                                                         sep_well_gas*ef_equip_subpartw[Source == "Separator" & Type == "Gas", CH4_TPY] +
                                                         deh_well_gas*ef_equip_subpartw[Source == "Dehydrator" & Type == "Gas", CH4_TPY] +
                                                         compr_well_gas*ef_equip_subpartw[Source == "Compressor" & Type == "Gas", CH4_TPY] +
                                                         heat_well_gas*ef_equip_subpartw[Source == "Heater/Treater" & Type == "Gas", CH4_TPY] +
                                                         meters_well_gas*ef_equip_subpartw[Source == "Meters/Piping" & Type == "Gas", CH4_TPY]) +
                                   Oil.Wells.per.site*(ef_equip_subpartw[Source == "Wellhead" & Type == "Oil", CH4_TPY] +
                                                         sep_well_oil*ef_equip_subpartw[Source == "Separator" & Type == "Oil", CH4_TPY] +
                                                         deh_well_oil*ef_equip_subpartw[Source == "Dehydrator" & Type == "Oil", CH4_TPY] +
                                                         compr_well_oil*ef_equip_subpartw[Source == "Compressor" & Type == "Oil", CH4_TPY] +
                                                         heat_treat_well_oil*ef_equip_subpartw[Source == "Heater/Treater" & Type == "Oil", CH4_TPY] +
                                                         meters_well_oil*ef_equip_subpartw[Source == "Meters/Piping" & Type == "Oil", CH4_TPY]),
                                 Fug_Comp_CH4_Site_Rutherford = 
                                   Gas.Wells.per.site*(ef_equip_rutherford[Source == "Wellhead" & Type == "Gas", CH4_TPY] +
                                                         sep_well_gas*ef_equip_rutherford[Source == "Separator" & Type == "Gas", CH4_TPY] +
                                                         deh_well_gas*ef_equip_rutherford[Source == "Dehydrator" & Type == "Gas", CH4_TPY] +
                                                         compr_well_gas*ef_equip_rutherford[Source == "Compressor" & Type == "Gas", CH4_TPY] +
                                                         heat_well_gas*ef_equip_rutherford[Source == "Heater/Treater" & Type == "Gas", CH4_TPY] +
                                                         meters_well_gas*ef_equip_rutherford[Source == "Meters/Piping" & Type == "Gas", CH4_TPY]) +
                                   Oil.Wells.per.site*(ef_equip_rutherford[Source == "Wellhead" & Type == "Oil", CH4_TPY] +
                                                         sep_well_oil*ef_equip_rutherford[Source == "Separator" & Type == "Oil", CH4_TPY] +
                                                         deh_well_oil*ef_equip_rutherford[Source == "Dehydrator" & Type == "Oil", CH4_TPY] +
                                                         compr_well_oil*ef_equip_rutherford[Source == "Compressor" & Type == "Oil", CH4_TPY] +
                                                         heat_treat_well_oil*ef_equip_rutherford[Source == "Heater/Treater" & Type == "Oil", CH4_TPY] +
                                                         meters_well_oil*ef_equip_rutherford[Source == "Meters/Piping" & Type == "Oil", CH4_TPY]),
                                 Fug_PnC_CH4_Site_GHGI = 
                                   PnC_per_site_ib*fug_frac_ib_pnc*ghgi_equip_data[Source == "IB Controllers" & Type == "Gas", CH4_TPY],
                                 Fug_PnC_CH4_Site_GHGRP = 
                                   PnC_per_site_ib*fug_frac_ib_pnc*ef_equip_subpartw[Source == "IB Controllers" & Type == "Gas", CH4_TPY]
                                 )
                         ]
  
  # Pneumatic methane emissions
  wellsite_bins_proj[, `:=` (PnC_LB_CH4_Site_GHGI = PnC_per_site_lb*ghgi_equip_data[Source == "LB Controllers" & Type == "Gas", CH4_TPY],
                             PnC_IB_CH4_Site_GHGI = PnC_per_site_ib*(1-fug_frac_ib_pnc)*ghgi_equip_data[Source == "IB Controllers" & Type == "Gas", CH4_TPY],
                             PnC_HB_CH4_Site_GHGI = PnC_per_site_hb*ghgi_equip_data[Source == "HB Controllers" & Type == "Gas", CH4_TPY],
                             PnC_LB_CH4_Site_GHGRP = PnC_per_site_lb*ef_equip_subpartw[Source == "LB Controllers" & Type == "Gas", CH4_TPY],
                             PnC_IB_CH4_Site_GHGRP = PnC_per_site_ib*(1-fug_frac_ib_pnc)*ef_equip_subpartw[Source == "IB Controllers" & Type == "Gas", CH4_TPY],
                             PnC_HB_CH4_Site_GHGRP = PnC_per_site_hb*ef_equip_subpartw[Source == "HB Controllers" & Type == "Gas", CH4_TPY],
                             Pumps_CH4_Site_GHGI = Pumps_per_site*ghgi_equip_data[Source == "Pumps" & Type == "Gas", CH4_TPY],
                             Pumps_CH4_Site_GHGRP = Pumps_per_site*ef_equip_subpartw[Source == "Pneumatic Pump" & Type == "Gas", CH4_TPY]
  )]
  
  # Tank methane emissions
  wellsite_bins_proj[, `:=` (Tank_CH4_Cond_Site_EPTanks = tank_cond_tp_to_methane*Tank.Cond.TP.Bbl/Wellsite.Count,
                             Tank_CH4_Crude_Site_EPTanks = tank_crude_tp_to_methane*Tank.Crude.TP.Bbl/Wellsite.Count,
                             Tank_CH4_Site_EPTanks_Start = tank_cond_tp_to_methane*Tank.Cond.TP.Bbl.Site.Start + tank_crude_tp_to_methane*Tank.Crude.TP.Bbl.Site.Start,
                             Tank_CH4_Site_EPTanks_EG = tank_cond_tp_to_methane*Tank.Cond.TP.Bbl.Site.EG + tank_crude_tp_to_methane*Tank.Crude.TP.Bbl.Site.EG,
                             Tank_CH4_Site_EPTanks_NSPS = tank_cond_tp_to_methane*Tank.Cond.TP.Bbl.Site.NSPS + tank_crude_tp_to_methane*Tank.Crude.TP.Bbl.Site.NSPS)]
  
  # VOC and HAP emissions
  wellsite_bins_proj[, `:=` (Fug_Comp_VOC_Site_GHGI = methane_to_voc_prod*Fug_Comp_CH4_Site_GHGI, 
                             Fug_Comp_VOC_Site_GHGRP = methane_to_voc_prod*Fug_Comp_CH4_Site_GHGRP, 
                             Fug_Comp_VOC_Site_Rutherford = methane_to_voc_prod*Fug_Comp_CH4_Site_Rutherford, 
                             Fug_PnC_VOC_Site_GHGI = methane_to_voc_prod*Fug_PnC_CH4_Site_GHGI, 
                             Fug_PnC_VOC_Site_GHGRP = methane_to_voc_prod*Fug_PnC_CH4_Site_GHGRP,
                             PnC_LB_VOC_Site_GHGI = methane_to_voc_prod*PnC_LB_CH4_Site_GHGI, 
                             PnC_IB_VOC_Site_GHGI = methane_to_voc_prod*PnC_IB_CH4_Site_GHGI, 
                             PnC_HB_VOC_Site_GHGI = methane_to_voc_prod*PnC_HB_CH4_Site_GHGI,
                             Pumps_VOC_Site_GHGI = methane_to_voc_prod*Pumps_CH4_Site_GHGI,  
                             PnC_LB_VOC_Site_GHGRP = methane_to_voc_prod*PnC_LB_CH4_Site_GHGRP, 
                             PnC_IB_VOC_Site_GHGRP = methane_to_voc_prod*PnC_IB_CH4_Site_GHGRP, 
                             PnC_HB_VOC_Site_GHGRP = methane_to_voc_prod*PnC_HB_CH4_Site_GHGRP,
                             Pumps_VOC_Site_GHGRP = methane_to_voc_prod*Pumps_CH4_Site_GHGRP,
                             Tank_VOC_Cond_Site_EPTanks = tank_cond_tp_to_voc*Tank.Cond.TP.Bbl/Wellsite.Count,
                             Tank_VOC_Crude_Site_EPTanks = tank_crude_tp_to_voc*Tank.Crude.TP.Bbl/Wellsite.Count,
                             Tank_VOC_Site_EPTanks_Start = tank_cond_tp_to_voc*Tank.Cond.TP.Bbl.Site.Start + tank_crude_tp_to_voc*Tank.Crude.TP.Bbl.Site.Start,
                             Tank_VOC_Site_EPTanks_EG = tank_cond_tp_to_voc*Tank.Cond.TP.Bbl.Site.EG + tank_crude_tp_to_voc*Tank.Crude.TP.Bbl.Site.EG,
                             Tank_VOC_Site_EPTanks_NSPS = tank_cond_tp_to_voc*Tank.Cond.TP.Bbl.Site.NSPS + tank_crude_tp_to_voc*Tank.Crude.TP.Bbl.Site.NSPS)
  ][, `:=` (Fug_Comp_HAP_Site_GHGI = voc_to_hap_prod*Fug_Comp_VOC_Site_GHGI, 
            Fug_Comp_HAP_Site_GHGRP = voc_to_hap_prod*Fug_Comp_VOC_Site_GHGRP, 
            Fug_Comp_HAP_Site_Rutherford = voc_to_hap_prod*Fug_Comp_VOC_Site_Rutherford, 
            Fug_PnC_HAP_Site_GHGI = voc_to_hap_prod*Fug_PnC_VOC_Site_GHGI, 
            Fug_PnC_HAP_Site_GHGRP = voc_to_hap_prod*Fug_PnC_VOC_Site_GHGRP,
            PnC_LB_HAP_Site_GHGI = voc_to_hap_prod*PnC_LB_VOC_Site_GHGI, 
            PnC_IB_HAP_Site_GHGI = voc_to_hap_prod*PnC_IB_VOC_Site_GHGI, 
            PnC_HB_HAP_Site_GHGI = voc_to_hap_prod*PnC_HB_VOC_Site_GHGI, 
            Pumps_HAP_Site_GHGI = voc_to_hap_prod*Pumps_VOC_Site_GHGI, 
            PnC_LB_HAP_Site_GHGRP = voc_to_hap_prod*PnC_LB_VOC_Site_GHGRP, 
            PnC_IB_HAP_Site_GHGRP = voc_to_hap_prod*PnC_IB_VOC_Site_GHGRP, 
            PnC_HB_HAP_Site_GHGRP = voc_to_hap_prod*PnC_HB_VOC_Site_GHGRP,
            Pumps_HAP_Site_GHGRP = voc_to_hap_prod*Pumps_VOC_Site_GHGRP)
  ]
  
  # component counts per equipment
  wellhead_comp_gas <- comp_counts[Type == "Gas" & Equipment == "Wellheads", .(Component.Count)] %>% pull()
  sep_comp_gas <- comp_counts[Type == "Gas" & Equipment == "Separators", .(Component.Count)] %>% pull()
  deh_comp_gas <- comp_counts[Type == "Gas" & Equipment == "Dehydrators", .(Component.Count)] %>% pull()
  compr_comp_gas <- comp_counts[Type == "Gas" & Equipment == "Compressors", .(Component.Count)] %>% pull()
  heat_comp_gas <- comp_counts[Type == "Gas" & Equipment == "In-line heaters", .(Component.Count)] %>% pull()
  meters_comp_gas <- comp_counts[Type == "Gas" & Equipment == "Meters/piping", .(Component.Count)] %>% pull()
  
  wellhead_comp_oil <- comp_counts[Type == "Oil" & Equipment == "Wellhead", .(Component.Count)] %>% pull()
  sep_comp_oil <- comp_counts[Type == "Oil" & Equipment == "Separator", .(Component.Count)] %>% pull()
  head_comp_oil <- comp_counts[Type == "Oil" & Equipment == "Header", .(Component.Count)] %>% pull()
  heat_treat_comp_oil <- comp_counts[Type == "Oil" & Equipment == "Heater-treater", .(Component.Count)] %>% pull()
  
  deh_comp_oil <- 0
  compr_comp_oil <- 0
  meters_comp_oil <- meters_comp_gas
  heat_comp_oil <- 0
  heat_treat_comp_gas <- 0
  head_comp_gas <- 0
  
  wellsite_bins_proj[, `:=` (
    gas_components_per_well = wellhead_comp_gas + sep_comp_gas*sep_well_gas  + deh_comp_gas*deh_well_gas + 
                              compr_comp_gas*compr_well_gas + heat_comp_gas*heat_well_gas +
                              heat_treat_comp_gas*heat_well_gas + head_comp_gas*head_well_gas + meters_comp_gas*meters_well_gas,
    oil_components_per_well = wellhead_comp_oil + sep_comp_oil*sep_well_oil + deh_comp_oil*deh_well_oil +
                              compr_comp_oil*compr_well_oil + heat_comp_oil*heat_well_oil + heat_treat_comp_oil*heat_well_oil + 
                              head_comp_oil*head_well_oil + meters_comp_oil*meters_well_oil
    )]
  
  bser_component_ef_1 <- bser_component_ef[fug_leak_rate == "1%" & fug_repair_rate == "30 day", bser_component_ef] 
  bser_component_ef_half <- bser_component_ef[fug_leak_rate == "0.50%" & fug_repair_rate == "30 day", bser_component_ef] 
  
  wellsite_bins_proj[, Fug_Comp_CH4_Site_BSER_1percentleak := (gas_components_per_well*Gas.Wells.per.site + oil_components_per_well*Oil.Wells.per.site)*bser_component_ef_1]
  wellsite_bins_proj[, Fug_Comp_CH4_Site_BSER_halfpercentleak := (gas_components_per_well*Gas.Wells.per.site + oil_components_per_well*Oil.Wells.per.site)*bser_component_ef_half]
  
  wellsite_bins_proj[, `:=` (Fug_Comp_VOC_Site_BSER_1percentleak = methane_to_voc_prod*Fug_Comp_CH4_Site_BSER_1percentleak,
                                 Fug_Comp_VOC_Site_BSER_halfpercentleak = methane_to_voc_prod*Fug_Comp_CH4_Site_BSER_halfpercentleak)
  ][, `:=` (Fug_Comp_HAP_Site_BSER_1percentleak = voc_to_hap_prod*Fug_Comp_VOC_Site_BSER_1percentleak,
          Fug_Comp_HAP_Site_BSER_halfpercentleak = voc_to_hap_prod*Fug_Comp_VOC_Site_BSER_halfpercentleak)
  ]

  
  wellsite_bins_proj
  
}
