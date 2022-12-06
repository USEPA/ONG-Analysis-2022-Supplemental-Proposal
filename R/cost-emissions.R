#' cost-emissions.R
#' These functions construct the cost and emissions tables for control measures
#' for pneumatic devices and storage vessels

# Pneumatic devices
get_pneumatic_parameters = function(
  ghgi_equip_data, 
  ef_equip_subpartw
)  {
  
  #---- 
  # list of fates
  #---- 
  fate_list = c("bau_new", "bau_retro",
                "high_to_low_bleed_new", "high_to_low_bleed_retro",
                "zero_emis_solar_new", "zero_emis_solar_retro",
                "zero_emis_elec_new", "zero_emis_elec_retro",
                "zero_emis_air_new", "zero_emis_air_retro")
  
  #---- 
  # cost and engineering parameters
  #---- 
  
  # parameter values are sourced primarily from Carbon Limits' Abatement Cost Tool (EPA-HQ-OAR-2021-0317-0845_attachment_4.xlsx at Regulations.gov);
  # others (i.e., inflation) are sourced from the 2022 Supplemental TSD workbook for pneumatic controllers
  
  # compressor cost parameters are estimated in Excel; see carbon_limits_compressor_cost_functions.xlsx in the data-raw folder
  
  ## electricity requirements
  amp_per_pnc = 0.08
  amp_per_pump = 0.40
  amp_other = 0.29
  
  ## capacity requirements
  amp_to_watt_pv = 24*24/0.85/4*1.5
  amp_adj_bat = 24*10/.8*1.35
  
  ## unit conversions
  amp_to_kwh_grid = 24*24*days_per_year/1000
  trans_tpy_ch4_to_scfh_gas = (1/tons_per_lb)/lbs_per_scf/gas_to_methane_trans/hours_per_year
  scfh_gas_to_hp_compr = (1/(1-0.17)+2)/60*0.20
  hp_to_kwh_grid = 0.0007457*1000*0.5*hours_per_year
  hp_compr_base = 4.24
  
  ## dollar year conversions
  infl_2016_2019 = 1.06
  infl_2012_2019 = 1.12
  
  ## capital and installation cost parameters
  
  # installation multipliers
  inst_mult_new = 1.5
  inst_mult_retro = 2
  
  # control panels
  ctrl_panel_cost = 3432
  
  # solar panels
  pv_cost_per_w = 343/140
  
  # batteries
  bat_cost_per_amp = 343/140
  
  # compressors
  compr_cost_init_quad = 120 
  compr_cost_init_lin = 200
  compr_cost_init_const = 18000
  compr_cost_repl_quad = 50
  compr_cost_repl_lin = -190
  compr_cost_repl_const = 6750
  
  # pneumatic devices
  lb_pnc_cost_new = 2698
  lb_pnc_cost_retro = 2698/2
  ib_pnc_cost_new = 2471
  hb_pnc_cost_new = 2471
  pump_cost_new = 1500
  ectrl_cost_new = 3432
  ectrl_cost_retro = 1716
  epump_cost_new = 6000/(708/607.5)
  epump_cost_retro = 6000/(708/607.5)
  pnc_instl_cost = 387
  pump_instl_cost = 387
  
  # other
  oth_supp_cap_cost_new = 1000
  oth_supp_cap_cost_retro = 1400
  
  ## O&M cost parameters
  # compressors
  compr_om_cost_pct = 0.04
  
  # pneumatic devices
  pnc_om_cost = 140
  pump_om_cost = 140
  ectrl_om_cost = 80
  epump_om_cost = 80
  
  # electricity (national average)
  elec_kwh_cost = 0.12
  
  #---- 
  # shortcut parameters that map from PnC tpy CH4 emissions to compressor HP
  #---- 
  
  # transmission
  alpha_trans_lb = ghgi_equip_data[Segment == "TRANS" & Source == "LB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  alpha_trans_ib = ghgi_equip_data[Segment == "TRANS" & Source == "IB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  alpha_trans_hb = ghgi_equip_data[Segment == "TRANS" & Source == "HB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  
  # storage
  alpha_stor_lb = ghgi_equip_data[Segment == "STOR" & Source == "LB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  alpha_stor_ib = ghgi_equip_data[Segment == "STOR" & Source == "IB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  alpha_stor_hb = ghgi_equip_data[Segment == "STOR" & Source == "HB Controllers" & Type == "Gas", CH4_TPY]*trans_tpy_ch4_to_scfh_gas*scfh_gas_to_hp_compr
  
  #---- 
  # define new data table
  #---- 
  
  pneumatic_parameters = data.table(fate =  rep(fate_list, length(segment_order)),
                                    segment = rep(segment_order, each = length(fate_list)))
  
  #---- 
  # add equipment lifetimes
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_ctrl_life = 15,
    pneu_pump_life = 15,
    pneu_pv_life = 10,
    pneu_bat_life = 4,
    pneu_compr_life = 6
  )]
  
  #---- 
  # add emissions reductions and gas capture
  #---- 
  
  pneumatic_parameters[, `:=` (
    pnc_er_pct_per_lb_GHGI = rep(c(rep(0, 4), rep(1, 6)), length(segment_order)),
    pnc_er_pct_per_ib_GHGI = rep(c(rep(0, 4), rep(1, 6)), length(segment_order)),
    pnc_er_pct_per_hb_GHGI = c(rep(0, 2), 
                               1 - ghgi_equip_data[Segment == "PROD" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "PROD" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               1 - ghgi_equip_data[Segment == "PROD" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "PROD" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               rep(1, 6),
                               rep(0, 2), 
                               1 - ghgi_equip_data[Segment == "G&B" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "G&B" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               1 - ghgi_equip_data[Segment == "G&B" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "G&B" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               rep(1, 6),
                               rep(0, 4),
                               rep(1, 6),
                               rep(0, 2), 
                               1 - ghgi_equip_data[Segment == "TRANS" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "TRANS" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               1 - ghgi_equip_data[Segment == "TRANS" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "TRANS" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               rep(1, 6),
                               rep(0, 2), 
                               1 - ghgi_equip_data[Segment == "STOR" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "STOR" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               1 - ghgi_equip_data[Segment == "STOR" & Source == "LB Controllers" & Type == "Gas", CH4_TPY] / 
                                 ghgi_equip_data[Segment == "STOR" & Source == "HB Controllers" & Type == "Gas", CH4_TPY], 
                               rep(1, 6)),
    pump_er_pct_GHGI = rep(c(rep(0, 4), rep(1, 6)), length(segment_order)),
    pneu_gas_cap_ind = rep(c(rep(0, 2), rep(1, 8)), length(segment_order))
  )]
  
  #---- 
  # add capital costs
  #---- 
  
  #---- 
  # control panel
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_panel_init_base = rep(c(rep(0, 4), 
                                        inst_mult_new*ctrl_panel_cost, 
                                        inst_mult_retro*ctrl_panel_cost, 
                                        inst_mult_new*ctrl_panel_cost, 
                                        inst_mult_retro*ctrl_panel_cost,
                                        rep(0, 2)), length(segment_order))
  )]

  #----   
  # solar PV
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_pv_init_base = rep(c(rep(0, 4), 
                                       inst_mult_new*amp_other*amp_to_watt_pv*pv_cost_per_w, 
                                       inst_mult_retro*amp_other*amp_to_watt_pv*pv_cost_per_w,
                                       rep(0, 4)), 
                                     length(segment_order)),
    pneu_cap_cost_pv_repl_base = rep(c(rep(0, 4), 
                                       amp_other*amp_to_watt_pv*pv_cost_per_w, 
                                       amp_other*amp_to_watt_pv*pv_cost_per_w,
                                       rep(0, 4)), 
                                     length(segment_order)),
    pneu_cap_cost_pv_init_per_pnc = rep(c(rep(0, 4),
                                          inst_mult_new*amp_per_pnc*amp_to_watt_pv*pv_cost_per_w,
                                          inst_mult_retro*amp_per_pnc*amp_to_watt_pv*pv_cost_per_w,
                                          rep(0, 4)), length(segment_order)),
    pneu_cap_cost_pv_repl_per_pnc = rep(c(rep(0, 4),
                                          amp_per_pnc*amp_to_watt_pv*pv_cost_per_w,
                                          amp_per_pnc*amp_to_watt_pv*pv_cost_per_w,
                                          rep(0, 4)), length(segment_order)),
    pneu_cap_cost_pv_init_per_pump = rep(c(rep(0, 4),
                                           inst_mult_new*amp_per_pump*amp_to_watt_pv*pv_cost_per_w,
                                           inst_mult_retro*amp_per_pump*amp_to_watt_pv*pv_cost_per_w,
                                           rep(0, 4)), length(segment_order)),
    pneu_cap_cost_pv_repl_per_pump = rep(c(rep(0, 4),
                                           amp_per_pump*amp_to_watt_pv*pv_cost_per_w,
                                           amp_per_pump*amp_to_watt_pv*pv_cost_per_w,
                                           rep(0, 4)), length(segment_order))
  )]
  
  #---- 
  # battery
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_bat_init_base = rep(c(rep(0, 4), 
                                        inst_mult_new*amp_other*amp_adj_bat*bat_cost_per_amp, 
                                        inst_mult_retro*amp_other*amp_adj_bat*bat_cost_per_amp,
                                        rep(0, 4)), 
                                      length(segment_order)),
    pneu_cap_cost_bat_repl_base = rep(c(rep(0, 4), 
                                        amp_other*amp_adj_bat*bat_cost_per_amp, 
                                        amp_other*amp_adj_bat*bat_cost_per_amp,
                                        rep(0, 4)), 
                                      length(segment_order)),
    pneu_cap_cost_bat_init_per_pnc = rep(c(rep(0, 4),
                                           inst_mult_new*amp_per_pnc*amp_adj_bat*bat_cost_per_amp,
                                           inst_mult_retro*amp_per_pnc*amp_adj_bat*bat_cost_per_amp,
                                           rep(0, 4)), length(segment_order)),
    pneu_cap_cost_bat_repl_per_pnc = rep(c(rep(0, 4),
                                           amp_per_pnc*amp_adj_bat*bat_cost_per_amp,
                                           amp_per_pnc*amp_adj_bat*bat_cost_per_amp,
                                           rep(0, 4)), length(segment_order)),
    pneu_cap_cost_bat_init_per_pump = rep(c(rep(0, 4),
                                            inst_mult_new*amp_per_pump*amp_adj_bat*bat_cost_per_amp,
                                            inst_mult_retro*amp_per_pump*amp_adj_bat*bat_cost_per_amp,
                                            rep(0, 4)), length(segment_order)),
    pneu_cap_cost_bat_repl_per_pump = rep(c(rep(0, 4),
                                            amp_per_pump*amp_adj_bat*bat_cost_per_amp,
                                            amp_per_pump*amp_adj_bat*bat_cost_per_amp,
                                            rep(0, 4)), length(segment_order))
  )]
  
  #---- 
  # compressor
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_compr_init_base = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                      rep(c(rep(0, 8),
                                      inst_mult_new*(compr_cost_init_const + compr_cost_init_lin*hp_compr_base + compr_cost_init_quad*hp_compr_base^2), 
                                      inst_mult_retro*(compr_cost_init_const + compr_cost_init_lin*hp_compr_base + compr_cost_init_quad*hp_compr_base^2)), 2)),
    pneu_cap_cost_compr_repl_base = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                      rep(c(rep(0, 8),
                                            compr_cost_repl_const + compr_cost_repl_lin*hp_compr_base + compr_cost_repl_quad*hp_compr_base^2, 
                                            compr_cost_repl_const + compr_cost_repl_lin*hp_compr_base + compr_cost_repl_quad*hp_compr_base^2), 2)),
    pneu_cap_cost_compr_init_per_lb_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_trans_lb^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_trans_lb^2,
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_stor_lb^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_stor_lb^2),
    pneu_cap_cost_compr_repl_per_lb_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_trans_lb^2,
                                             compr_cost_repl_quad*alpha_trans_lb^2,
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_stor_lb^2,
                                             compr_cost_repl_quad*alpha_stor_lb^2),
    pneu_cap_cost_compr_init_per_ib_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_trans_ib^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_trans_ib^2,
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_stor_ib^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_stor_ib^2),
    pneu_cap_cost_compr_repl_per_ib_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_trans_ib^2,
                                             compr_cost_repl_quad*alpha_trans_ib^2,
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_stor_ib^2,
                                             compr_cost_repl_quad*alpha_stor_ib^2),
    pneu_cap_cost_compr_init_per_hb_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_trans_hb^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_trans_hb^2,
                                             rep(0, 8),
                                             inst_mult_new*compr_cost_init_quad*alpha_stor_hb^2,
                                             inst_mult_retro*compr_cost_init_quad*alpha_stor_hb^2),
    pneu_cap_cost_compr_repl_per_hb_quad = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_trans_hb^2,
                                             compr_cost_repl_quad*alpha_trans_hb^2,
                                             rep(0, 8),
                                             compr_cost_repl_quad*alpha_stor_hb^2,
                                             compr_cost_repl_quad*alpha_stor_hb^2),
    pneu_cap_cost_compr_init_per_lbxib = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_trans_lb*alpha_trans_ib,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_trans_lb*alpha_trans_ib,
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_stor_lb*alpha_stor_ib,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_stor_lb*alpha_stor_ib),
    pneu_cap_cost_compr_repl_per_lbxib = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_trans_lb*alpha_trans_ib,
                                           compr_cost_repl_quad*2*alpha_trans_lb*alpha_trans_ib,
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_stor_lb*alpha_stor_ib,
                                           compr_cost_repl_quad*2*alpha_stor_lb*alpha_stor_ib),
    pneu_cap_cost_compr_init_per_lbxhb = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_trans_lb*alpha_trans_hb,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_trans_lb*alpha_trans_hb,
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_stor_lb*alpha_stor_hb,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_stor_lb*alpha_stor_hb),
    pneu_cap_cost_compr_repl_per_lbxhb = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_trans_lb*alpha_trans_hb,
                                           compr_cost_repl_quad*2*alpha_trans_lb*alpha_trans_hb,
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_stor_lb*alpha_stor_hb,
                                           compr_cost_repl_quad*2*alpha_stor_lb*alpha_stor_hb),
    pneu_cap_cost_compr_init_per_ibxhb = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_trans_ib*alpha_trans_hb,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_trans_ib*alpha_trans_hb,
                                           rep(0, 8),
                                           inst_mult_new*compr_cost_init_quad*2*alpha_stor_ib*alpha_stor_hb,
                                           inst_mult_retro*compr_cost_init_quad*2*alpha_stor_ib*alpha_stor_hb),
    pneu_cap_cost_compr_repl_per_ibxhb = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_trans_ib*alpha_trans_hb,
                                           compr_cost_repl_quad*2*alpha_trans_ib*alpha_trans_hb,
                                           rep(0, 8),
                                           compr_cost_repl_quad*2*alpha_stor_ib*alpha_stor_hb,
                                           compr_cost_repl_quad*2*alpha_stor_ib*alpha_stor_hb),
    pneu_cap_cost_compr_init_per_lb_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_lb,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_lb,
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_lb,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_lb),
    pneu_cap_cost_compr_repl_per_lb_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_lb,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_lb,
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_lb,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_lb),
    pneu_cap_cost_compr_init_per_ib_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_ib,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_ib,
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_ib,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_ib),
    pneu_cap_cost_compr_repl_per_ib_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_ib,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_ib,
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_ib,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_ib),
    pneu_cap_cost_compr_init_per_hb_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_hb,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_trans_hb,
                                            rep(0, 8),
                                            inst_mult_new*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_hb,
                                            inst_mult_retro*(2*compr_cost_init_quad*hp_compr_base+compr_cost_init_lin)*alpha_stor_hb),
    pneu_cap_cost_compr_repl_per_hb_lin = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_hb,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_trans_hb,
                                            rep(0, 8),
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_hb,
                                            (2*compr_cost_repl_quad*hp_compr_base+compr_cost_repl_lin)*alpha_stor_hb)
    
    )]
  
  #---- 
  # other supply
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_oth_per_pnc = c(rep(0, length(fate_list)*(length(segment_order)-2)),
                                  rep(c(rep(0, 8),
                                      inst_mult_new*oth_supp_cap_cost_new,
                                      inst_mult_retro*oth_supp_cap_cost_retro), 2))
  )]
  
  #---- 
  # devices
  #---- 
  
  pneumatic_parameters[, `:=` (
    pneu_cap_cost_dev_per_lb = c(rep(c(infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                      0, 
                                      infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                      0,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      rep(0,2)), length(segment_order)-2),
                                  rep(c(infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        0, 
                                        infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        0,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        0), 2)),
    pneu_cap_cost_dev_per_ib = c(rep(c(infl_2012_2019*ib_pnc_cost_new+pnc_instl_cost, 
                                      0, 
                                      infl_2012_2019*ib_pnc_cost_new+pnc_instl_cost, 
                                      0,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      rep(0,2)), length(segment_order)-2),
                                  rep(c(infl_2012_2019*ib_pnc_cost_new+pnc_instl_cost, 
                                        0, 
                                        infl_2012_2019*ib_pnc_cost_new+pnc_instl_cost, 
                                        0,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        0), 2)),
    pneu_cap_cost_dev_per_hb = c(rep(c(infl_2012_2019*hb_pnc_cost_new+pnc_instl_cost, 
                                      0, 
                                      infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                      infl_2012_2019*lb_pnc_cost_retro+pnc_instl_cost,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      inst_mult_new*ectrl_cost_new,
                                      inst_mult_retro*ectrl_cost_retro,
                                      rep(0,2)), length(segment_order)-2),
                                  rep(c(infl_2012_2019*hb_pnc_cost_new+pnc_instl_cost, 
                                        0, 
                                        infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        infl_2012_2019*lb_pnc_cost_retro+pnc_instl_cost,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        inst_mult_new*ectrl_cost_new,
                                        inst_mult_retro*ectrl_cost_retro,
                                        infl_2012_2019*lb_pnc_cost_new+pnc_instl_cost, 
                                        0), 2)),
    pneu_cap_cost_dev_per_pump = c(rep(c(infl_2016_2019*pump_cost_new+pump_instl_cost, 
                                        0, 
                                        infl_2016_2019*pump_cost_new+pump_instl_cost, 
                                        0,
                                        inst_mult_new*epump_cost_new,
                                        inst_mult_retro*epump_cost_retro,
                                        inst_mult_new*epump_cost_new,
                                        inst_mult_retro*epump_cost_retro,
                                        rep(0,2)), length(segment_order)-2),
                                    rep(0, 2*length(fate_list)))
  )]
  
  #---- 
  # add operating costs
  #---- 
  
  #---- 
  # compressor
  #----  
  
  # maintenance costs
  pneumatic_parameters[, `:=` (
    pneu_ann_cost_compr_base = compr_om_cost_pct*pneu_cap_cost_compr_init_base*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                     grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                     default = 0),
    pneu_ann_cost_compr_per_lb_quad = compr_om_cost_pct*pneu_cap_cost_compr_init_per_lb_quad*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                   grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                   default = 0),
    pneu_ann_cost_compr_per_ib_quad = compr_om_cost_pct*pneu_cap_cost_compr_init_per_ib_quad*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                   grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                   default = 0),
    pneu_ann_cost_compr_per_hb_quad = compr_om_cost_pct*pneu_cap_cost_compr_init_per_hb_quad*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                   grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                   default = 0),
    pneu_ann_cost_compr_per_lbxib = compr_om_cost_pct*pneu_cap_cost_compr_init_per_lbxib*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                               grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                               default = 0),
    pneu_ann_cost_compr_per_lbxhb = compr_om_cost_pct*pneu_cap_cost_compr_init_per_lbxhb*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                               grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                               default = 0),
    pneu_ann_cost_compr_per_ibxhb = compr_om_cost_pct*pneu_cap_cost_compr_init_per_ibxhb*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                               grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                               default = 0),
    pneu_ann_cost_compr_per_lb_lin = compr_om_cost_pct*pneu_cap_cost_compr_init_per_lb_lin*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                 grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                 default = 0),
    pneu_ann_cost_compr_per_ib_lin = compr_om_cost_pct*pneu_cap_cost_compr_init_per_ib_lin*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                 grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                 default = 0),
    pneu_ann_cost_compr_per_hb_lin = compr_om_cost_pct*pneu_cap_cost_compr_init_per_hb_lin*fcase(grepl("air_new", fate), 1/inst_mult_new,
                                                                                                 grepl("air_retro", fate), 1/inst_mult_retro,
                                                                                                 default = 0)
  )]
  
  # electricity costs
  pneumatic_parameters[, `:=` (
    pneu_ann_cost_compr_base = pneu_ann_cost_compr_base + fifelse(grepl("_air_", fate) & grepl("TRANS|STOR", segment), hp_compr_base*hp_to_kwh_grid*elec_kwh_cost, 0),
    pneu_ann_cost_compr_per_lb_lin = pneu_ann_cost_compr_per_lb_lin + fcase(grepl("_air_", fate) & grepl("TRANS", segment), alpha_trans_lb*hp_to_kwh_grid*elec_kwh_cost,
                                                                            grepl("_air_", fate) & grepl("STOR", segment), alpha_stor_lb*hp_to_kwh_grid*elec_kwh_cost,
                                                                            default = 0),
    pneu_ann_cost_compr_per_ib_lin = pneu_ann_cost_compr_per_ib_lin + fcase(grepl("_air_", fate) & grepl("TRANS", segment), alpha_trans_ib*hp_to_kwh_grid*elec_kwh_cost,
                                                                            grepl("_air_", fate) & grepl("STOR", segment), alpha_stor_ib*hp_to_kwh_grid*elec_kwh_cost,
                                                                            default = 0),
    pneu_ann_cost_compr_per_hb_lin = pneu_ann_cost_compr_per_hb_lin + fcase(grepl("_air_", fate) & grepl("TRANS", segment), alpha_trans_hb*hp_to_kwh_grid*elec_kwh_cost,
                                                                            grepl("_air_", fate) & grepl("STOR", segment), alpha_stor_hb*hp_to_kwh_grid*elec_kwh_cost,
                                                                            default = 0)
  )]
  
  #---- 
  # devices
  #----  
  
  pneumatic_parameters[, `:=` (
    pneu_ann_cost_dev_per_hb = rep(c(pnc_om_cost, pnc_om_cost, pnc_om_cost, pnc_om_cost, 
                                 ectrl_om_cost, ectrl_om_cost,
                                 amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, 
                                 rep(0, 2)), length(segment_order)),
    pneu_ann_cost_dev_per_ib = rep(c(pnc_om_cost, pnc_om_cost, pnc_om_cost, pnc_om_cost, 
                                 ectrl_om_cost, ectrl_om_cost,
                                 amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, 
                                 rep(0, 2)), length(segment_order)),
    pneu_ann_cost_dev_per_lb = rep(c(pnc_om_cost, pnc_om_cost, pnc_om_cost, pnc_om_cost, 
                                 ectrl_om_cost, ectrl_om_cost,
                                 amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, amp_per_pnc*amp_to_kwh_grid*elec_kwh_cost, 
                                 rep(0, 2)), length(segment_order)),
    pneu_ann_cost_dev_per_pump = rep(c(pump_om_cost, pump_om_cost, pump_om_cost, pump_om_cost, 
                                   epump_om_cost, epump_om_cost,
                                   amp_per_pump*amp_to_kwh_grid*elec_kwh_cost, amp_per_pump*amp_to_kwh_grid*elec_kwh_cost, 
                                   rep(0, 2)), length(segment_order))
  )]
  
  #---- 
  # other
  #----  
  
  pneumatic_parameters[, `:=` (
    pneu_ann_cost_other = rep(c(rep(0, 6), 
                               amp_other*amp_to_kwh_grid*elec_kwh_cost, 
                               amp_other*amp_to_kwh_grid*elec_kwh_cost,
                               rep(0, 2)), length(segment_order))
  )]
  
  pneumatic_parameters
  
}

# Storage vessels
get_tank_parameters = function(
  
)  {
  
  # cost parameters are estimated in Excel; see tank_control_cost_functions.xlsx in the data-raw folder
  tank_control_inputs = data.table(fate =  c("bau", "combustor_new", "combustor_retro"),
                                   control_lifetime = c(0, 15, 15),
                                   reduction_pct = c(0, .95, .95),
                                   gas_capture_pct = c(0, 0, 0),
                                   tank_cost_base_CH4_thrsh = c(0, 50, 50),
                                   tank_cap_cost_base = c(0, 79352, 103158),
                                   tank_cap_cost_per_ton_CH4 = c(0, 43.904, 57.075),
                                   tank_ann_cost_base = c(0, 22840, 22840),
                                   tank_ann_cost_per_ton_CH4_lin = c(0, 2.2436, 2.2436),
                                   tank_ann_cost_per_ton_CH4_sqr = c(0, 0.0033, 0.0033)
  )
  
  tank_control_inputs
  
}                                 
