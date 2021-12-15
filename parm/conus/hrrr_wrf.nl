 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = 2018,
 start_month                         = 08,
 start_day                           = 26,
 start_hour                          = 12,
 start_minute                        = 00,
 start_second                        = 00,
 end_year                            = 2018,
 end_month                           = 08,
 end_day                             = 26,
 end_hour                            = 18,
 end_minute                          = 00,
 end_second                          = 00,
 interval_seconds                    = 10800,
 input_from_file                     = .true.,
 history_interval                    = 15,
 history_interval2                   = 60,
 history_interval_change             = 1080,
 frames_per_outfile                  = 1,
 cycling                             = .true.,
 restart                             = .false.,
 restart_interval                    = 5000,
 WRITE_INPUT                         = .false.,
 INPUTOUT_INTERVAL                   = 60,
 input_outname                       = "wrfinput_out_d<domain>_<date>"
 io_form_history                     = 11 
 io_form_restart                     = 11
 io_form_input                       = 2
 io_form_boundary                    = 2
 debug_level                         = 0
 diag_print                          = 1
 gsd_diagnostics                     = 1
 output_diagnostics                  = 1
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
 time_step                           = 20, 
 time_step_dfi                       = 20,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 s_we                                = 1,     1,     1,
 e_we                                = 1800,    112,   94,
 s_sn                                = 1,     1,     1,
 e_sn                                = 1060,    97,    91,
 s_vert                              = 1,     1,     1,
 e_vert                              = 51,    28,    28,
 num_metgrid_levels                  = 51, 
 num_metgrid_soil_levels             = 9,
 dx                                  = 3000.0, 10000,  3333,
 dy                                  = 3000.0, 10000,  3333,
 grid_id                             = 1,     2,     3,
 parent_id                           = 1,     1,     2,
 i_parent_start                      = 0,     31,    30,
 j_parent_start                      = 0,     17,    30,
 parent_grid_ratio                   = 1,     3,     3,
 parent_time_step_ratio              = 1,     3,     3,
 feedback                            = 1,
 smooth_option                       = 0
 p_top_requested                     = 1500
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
 use_adaptive_time_step              = .true.
 step_to_output_time                 = .true.
 target_cfl                          = 1.2
 max_step_increase_pct               = 5
 starting_time_step                  = 20
 max_time_step                       = 20
 min_time_step                       = 20
 nproc_x                             = 16 ! will be replaced by script
 nproc_y                             = 64 ! will be replaced by script
 numtiles                            = 1 ! will be replaced by script
 /

 &physics
 mp_physics                          = 28,     3,     3,
 mp_tend_radar                       = 0,
 do_radar_ref                        = 1,
 ra_lw_physics                       = 4,     1,     1,
 ra_sw_physics                       = 4,     1,     1,
 ra_sw_eclipse                       = 1,
 radt                                = 15,    30,    30,
 swint_opt                           = 1,
 CO2TF                               = 1,
 sf_sfclay_physics                   = 5,     5,     1,
 sf_surface_physics                  = 3,     3,     1,
 sf_lake_physics                     = 1,     1,     1
 bl_pbl_physics                      = 5,     5,     1,
 bldt                                = 0,     0,     0,
 bl_mynn_tkebudget                   = 0,
 bl_mynn_tkeadvect                   = .false.,
 bl_mynn_cloudpdf                    = 2,
 bl_mynn_edmf                        = 1,
 bl_mynn_edmf_mom                    = 1,
 bl_mynn_edmf_tke                    = 0,
 bl_mynn_mixlength                   = 2,
 grav_settling                       = 0,
 scalar_pblmix                       = 1,
 cu_physics                          = 0,     1,     0,
 cudt                                = 0,     5,     5,
 isfflx                              = 1,
 ifsnow                              = 1,
 icloud                              = 1,
 icloud_bl                           = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 9,
 sf_urban_physics                    = 0,
 mp_zero_out                         = 2,
 mp_zero_out_thresh                  = 1.e-12,
 use_aero_icbc                       = .true.
 use_rap_aero_icbc                   = .true.
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
 alb_sol                             = 1
 fractional_seaice                   = 1,
 seaice_threshold                    = 271.4
 aer_opt                             = 3,
 prec_acc_dt                         = 60.
 prec_acc_dt1                        = 15.
 hailcast_opt                        = 1
 haildt                              = 60
 tracer_pblmix                       = 0,
 /

 &fdda
 /

 &dynamics
 zadvect_implicit                    = 1,
 w_damping                           = 0,
 diff_opt                            = 2,
 km_opt                              = 4,
 km_opt_dfi                          = 1,
 diff_6th_opt                        = 2,
 diff_6th_factor                     = 0.12,
 diff_6th_factor2                    = 0.04,
 diff_6th_slopeopt                   = 1,
 diff_6th_thresh                     = 0.05,
 moist_mix6_off                      = .false.,
 chem_mix6_off                       = .true.,
 tracer_mix6_off                     = .true.,
 scalar_mix6_off                     = .false.,
 tke_mix6_off                        = .true.,
 damp_opt                            = 3,
 zdamp                               = 5000.,  5000.,  5000.,
 dampcoef                            = 0.2,    0.01,   0.01
 khdif                               = 0,      0,      0,
 kvdif                               = 0,      0,      0,
 SMDIV                               = 0.1,    0.1,    0.1,
 EMDIV                               = 0.01,   0.01,   0.01,
 EPSSM                               = 0.1,    0.1,    0.1
 non_hydrostatic                     = .true., .true., .true.,
 moist_adv_opt                       = 1,      2,      2,
 moist_adv_dfi_opt                   = 0,      1,     1,
 scalar_adv_opt                      = 1,      2,      2,
 TIME_STEP_SOUND                     = 4,      4,      4,
 H_MOM_ADV_ORDER                     = 5,      5,      5,
 V_MOM_ADV_ORDER                     = 5,      3,      3,
 H_SCA_ADV_ORDER                     = 5,      5,      5,
 V_SCA_ADV_ORDER                     = 5,      3,      3,
 gwd_opt                             = 3,
 hybrid_opt                          = 2
 etac                                = 0.2

 chem_adv_opt                        = 2,
 tracer_opt                          = 0,
 /

 &bdy_control
 spec_bdy_width                      = 10,
 spec_zone                           = 1,
 relax_zone                          = 9,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 /

 &grib2
 /

 &logging
 compute_tasks_silent                = .true. ! all compute ranks except 0 are silent
 io_servers_silent                   = .true. ! all I/O server ranks are silent
 stderr_logging                      = 0      ! disable output to stderr except for error messages
 /

 &chem
 chem_opt                            = 18,       0,
 chemdt                              = 0,        0,
 chem_in_opt                         = 1,        0,
 have_bcs_chem                       = .true.,  .false.,

 vertmix_onoff                       = 1,        1,
 enh_vermix                          = .true.,
 gas_drydep_opt                      = 0,        1,
 aer_drydep_opt                      = 111,      1,
 wetscav_onoff                       = -1,       0,

 biomass_burn_opt                    = 4,        0,
 bb_dcycle                           = .true.,  .true.,
 flam_part                           = 0.75,
 plumerise_flag                      = 2,
 plumerisefire_frq                   = 60,       0,

 chem_conv_tr                        = 0,        1,
 conv_tr_wetscav                     = 0,        1,
 conv_tr_aqchem                      = 0,        1,

 simple_dir_fdb                      = 1,
 simple_ind_fdb                      = 0,

 debug_chem                          = .false.,
 chemdiag                            = 111

 io_style_emissions                  = 0,
 emiss_opt                           = 0,        3,
 kemit                               = 0,
 emiss_opt_vol                       = 0,
 aircraft_emiss_opt                  = 0,
 phot_opt                            = 0,        3,
 photdt                              = 0,       30,
 bio_emiss_opt                       = 0,        1,
 ne_area                             = 0,
 bioemdt                             = 0,       30,
 gas_bc_opt                          = 0,        1,
 gas_ic_opt                          = 0,        1,
 aer_bc_opt                          = 0,        1,
 aer_ic_opt                          = 0,        1,
 seas_opt                            = 0,
 dust_opt                            = 0,
 dmsemis_opt                         = 0,
 aer_ra_feedback                     = 0,        0,
 aer_op_opt                          = 0,
 opt_pars_out                        = 0,
 diagnostic_chem                     = 0,
 /

 &namelist_quilt
 ! script will add a poll_servers line here
 nio_tasks_per_group = 64, ! will be replaced by script
 nio_groups = 2, ! will be replaced by script
 /
