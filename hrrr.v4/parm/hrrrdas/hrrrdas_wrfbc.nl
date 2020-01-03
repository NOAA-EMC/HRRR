 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = 2007,
 start_month                         = 04,
 start_day                           = 12,
 start_hour                          = 00,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = 2007,
 end_month                           = 04,
 end_day                             = 12,
 end_hour                            = 06,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 10800,
 input_from_file                     = .true., .true.,
 fine_input_stream                   = 0, 0,
 history_interval                    = 60, 60,
 frames_per_outfile                  = 1, 1,
 cycling                             = .true.,
 restart                             = .false.,
 restart_interval                    = 5000,
 WRITE_INPUT                         = .false.,
 INPUTOUT_INTERVAL                   = 60,
 input_outname                       = "wrfinput_out_d<domain>_<date>"
 io_form_history                     = 11
 io_form_restart                     = 11
 io_form_input                       = 11
 io_form_boundary                    = 2
 debug_level                         = 0
 diag_print                          = 1
 gsd_diagnostics                     = 1
 output_diagnostics                  = 0
 diag_int                            = 15
 wind_int                            = 5
 nocolons                            = .true.
 ncd_nofill                          = .true.
 /

&dfi_control
 dfi_opt                             = 0,
 dfi_nfilter                         = 7,
 dfi_write_filtered_input            = F,
 dfi_cutoff_seconds                  = 3600,
/

 &domains
 time_step                           = 60,
 time_step_dfi                       = 60,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 2,
 s_we                                = 1,     1,
 e_we                                = 400,   1801,
 s_sn                                = 1,     1,
 e_sn                                = 250,   1061,
 s_vert                              = 1,     1,
 e_vert                              = 51,    51,
 num_metgrid_levels                  = 50,
 num_metgrid_soil_levels             = 4,
 dx                                  = 15000.0, 3000.0,
 dy                                  = 15000.0, 3000.0,
 grid_id                             = 1,     2,
 parent_id                           = 1,     1,
 i_parent_start                      = 0,     21,
 j_parent_start                      = 0,     20,
 parent_grid_ratio                   = 1,     5,
 parent_time_step_ratio              = 1,     3,
 feedback                            = 1,
 smooth_option                       = 1,
 p_top_requested                     = 2000
 interp_type                         = 1
 rebalance                           = 1
 hypsometric_opt                     = 2
 lowest_lev_from_sfc                 = .false.
 lagrange_order                      = 1
 force_sfc_in_vinterp                = 1
 zap_close_levels                    = 500
 smooth_cg_topo                      = .true.
 sfcp_to_sfcp                        = .false.
 adjust_heights                      = .false.
 eta_levels   =   1.0000, 0.9980, 0.9940, 0.9870, 0.9750, 0.9590,
          0.9390, 0.9160, 0.8920, 0.8650, 0.8350, 0.8020, 0.7660,
          0.7270, 0.6850, 0.6400, 0.5920, 0.5420, 0.4970, 0.4565,
          0.4205, 0.3877, 0.3582, 0.3317, 0.3078, 0.2863, 0.2670,
          0.2496, 0.2329, 0.2188, 0.2047, 0.1906, 0.1765, 0.1624,
          0.1483, 0.1342, 0.1201, 0.1060, 0.0919, 0.0778, 0.0657,
          0.0568, 0.0486, 0.0409, 0.0337, 0.0271, 0.0209, 0.0151,
          0.0097, 0.0047, 0.0000,
 numtiles                            = 8
 /

 &physics
 mp_physics                          = 28,    28,
 mp_tend_radar                       = 0,
 do_radar_ref                        = 1,
 ra_lw_physics                       = 4,     4,
 ra_sw_physics                       = 4,     4,
 radt                                = 15,    15,
 swint_opt                           = 1,
 CO2TF                               = 1,
 sf_sfclay_physics                   = 5,     5,
 sf_surface_physics                  = 3,     3,
 sf_lake_physics                     = 0,     0,
 bl_pbl_physics                      = 5,     5,
 bldt                                = 0,     0,
 bl_mynn_tkebudget                   = 0,     0,
 bl_mynn_tkeadvect                   = .false., .false.,
 bl_mynn_cloudpdf                    = 2,
 bl_mynn_edmf                        = 1,     1,
 bl_mynn_edmf_mom                    = 1,     1,
 bl_mynn_edmf_tke                    = 0,     0,
 bl_mynn_mixlength                   = 2,
 grav_settling                       = 0,     0,
 scalar_pblmix                       = 0,     1,
 cu_physics                          = 3,     0,
 cu_diag                             = 1,     0,
 convtrans_avglen_m                  = 20,
 ishallow                            = 0,
 shcu_physics                        = 0,     0,
 cudt                                = 0,     0,
 isfflx                              = 1,
 ifsnow                              = 1,
 icloud                              = 1,
 icloud_bl                           = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 9,
 sf_urban_physics                    = 0,     0,
 mp_zero_out                         = 2,
 mp_zero_out_thresh                  = 1.e-12,
 use_aero_icbc                       = .true.
 use_rap_aero_icbc                   = .false.
 maxiens                             = 1,
 maxens                              = 3,
 maxens2                             = 3,
 maxens3                             = 16,
 ensdim                              = 144,
 usemonalb                           = .true.,
 rdlai2d                             = .true.,
 num_land_cat                        = 21,
 mosaic_lu                           = 1
 mosaic_soil                         = 1
 alb_sol                             = 0,     0,
 fractional_seaice                   = 1,
 seaice_threshold                    = 271.4
 cu_rad_feedback                     = .true., .false.,
 aer_opt                             = 0,
 prec_acc_dt                         = 60.
 tracer_pblmix                       = 0, 0,
 /

 &fdda
 /

 &dynamics
 zadvect_implicit                    = 1,
 w_damping                           = 0,
 diff_opt                            = 2,      2,
 km_opt                              = 4,      4,
 km_opt_dfi                          = 1,      1,
 diff_6th_opt                        = 2,      2,
 diff_6th_factor                     = 0.12,   0.12,
 diff_6th_factor2                    = 0.04,   0.04,
 diff_6th_slopeopt                   = 1,      1,
 diff_6th_thresh                     = 0.05,   0.05,
 moist_mix6_off                      = .false.,.false.,
 chem_mix6_off                       = .true., .true.,
 tracer_mix6_off                     = .true., .true.,
 scalar_mix6_off                     = .false.,.false.,
 tke_mix6_off                        = .true., .true.,
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,
 dampcoef                            = 0.2,    0.2,
 khdif                               = 0,      0,
 kvdif                               = 0,      0,
 SMDIV                               = 0.1,    0.1,
 EMDIV                               = 0.01,   0.01,
 EPSSM                               = 0.1,    0.1,
 non_hydrostatic                     = .true., .true.,
 moist_adv_opt                       = 1,      1,
 moist_adv_dfi_opt                   = 0,      0,
 scalar_adv_opt                      = 1,      1,
 TIME_STEP_SOUND                     = 4,      4,
 H_MOM_ADV_ORDER                     = 5,      5,
 V_MOM_ADV_ORDER                     = 5,      5,
 H_SCA_ADV_ORDER                     = 5,      5,
 V_SCA_ADV_ORDER                     = 5,      5,
 gwd_opt                             = 3,
 hybrid_opt                          = 2
 etac                                = 0.2
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,
 nested                              = .false., .true.,
 /

 &grib2
 /

 &logging
 compute_tasks_silent                = .true. ! all compute ranks except 0 are silent
 io_servers_silent                   = .true. ! all I/O server ranks are silent
 stderr_logging                      = 0      ! disable output to stderr except for error messages
 /

 &namelist_quilt
 nio_tasks_per_group = 0,
 nio_groups = 1,
 /
