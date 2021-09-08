
cat <<EOF > gsiparm.anl

 &SETUP
   miter=${nummiter},niter(1)=50,niter(2)=50,
   write_diag(1)=.true.,write_diag(2)=.false.,write_diag(3)=.true.,
   gencode=78,qoption=2,
   factqmin=0.0,factqmax=0.0,
   iguess=-1,
   oneobtest=${if_oneobtest},retrieval=.false.,
   nhr_assimilation=2,l_foto=.false.,
   use_pbl=.false.,verbose=.true.,
   lread_obs_save=${if_read_obs_save},lread_obs_skip=${if_read_obs_skip},
   newpc4pred=.true.,adp_anglebc=.true.,angord=4,
   passive_bc=.true.,use_edges=.false.,emiss_bc=.true.,
   diag_precon=.true.,step_start=1.e-3,
   l4densvar=${if4d},nhr_obsbin=1,min_offset=60,
   use_gfs_nemsio=${if_gfs_nemsio},
 /
 &GRIDOPTS
   JCAP=62,JCAP_B=62,NLAT=60,NLON=60,nsig=60,regional=.true.,
   wrf_nmm_regional=${bk_core_nmm},wrf_mass_regional=${bk_core_arw},
   nems_nmmb_regional=${bk_core_nmmb},nmmb_reference_grid='H',diagnostic_reg=.false.,
   filled_grid=.false.,half_grid=.true.,netcdf=${bk_if_netcdf},
 /
 &BKGERR
   vs=${vs_op}
   hzscl=${hzscl_op}
   bw=0.,fstat=.true.,
 /
 &ANBKGERR
 /
 &JCOPTS
 /
 &STRONGOPTS
 /
 &OBSQC
   dfact=0.75,dfact1=3.0,noiqc=.false.,c_varqc=0.02,vadfile='prepbufr',
 /
 &OBS_INPUT
   dmesh(1)=120.0,dmesh(2)=60.0,dmesh(3)=30,time_window_max=1.5,ext_sonde=.true.,
 /
OBS_INPUT::
!  dfile          dtype       dplat     dsis                 dval    dthin dsfcalc
   prepbufr       ps          null      ps                   1.0     0     0
   prepbufr       t           null      t                    1.0     0     0
   prepbufr       q           null      q                    1.0     0     0
   prepbufr       pw          null      pw                   1.0     0     0
   satwndbufr     uv          null      uv                   1.0     0     0
   prepbufr       uv          null      uv                   1.0     0     0
   prepbufr       spd         null      spd                  1.0     0     0
   prepbufr       dw          null      dw                   1.0     0     0
   l2rwbufr       rw          null      rw                   1.0     0     0
   prepbufr       sst         null      sst                  1.0     0     0
   gpsrobufr      gps_ref     null      gps                  1.0     0     0
   ssmirrbufr     pcp_ssmi    dmsp      pcp_ssmi             1.0    -1     0
   tmirrbufr      pcp_tmi     trmm      pcp_tmi              1.0    -1     0
   sbuvbufr       sbuv2       n16       sbuv8_n16            1.0     0     0
   sbuvbufr       sbuv2       n17       sbuv8_n17            1.0     0     0
   sbuvbufr       sbuv2       n18       sbuv8_n18            1.0     0     0
   hirs3bufr      hirs3       n16       hirs3_n16            0.0     1     0
   hirs3bufr      hirs3       n17       hirs3_n17            6.0     1     0
   hirs4bufr      hirs4       metop-a   hirs4_metop-a        6.0     2     0
   hirs4bufr      hirs4       n18       hirs4_n18            0.0     1     0
   hirs4bufr      hirs4       n19       hirs4_n19            1.0     2     0
   hirs4bufr      hirs4       metop-b   hirs4_metop-b        1.0     1     0
   gimgrbufr      goes_img    g11       imgr_g11             0.0     1     0
   gimgrbufr      goes_img    g12       imgr_g12             0.0     1     0
   airsbufr       airs        aqua      airs281SUBSET_aqua  20.0     2     0
   amsuabufr      amsua       n15       amsua_n15           10.0     2     0
   amsuabufr      amsua       n18       amsua_n18           10.0     2     0
   amsuabufr      amsua       n19       amsua_n19           10.0     2     0
   amsuabufr      amsua       metop-a   amsua_metop-a       10.0     2     0
   amsuabufr      amsua       metop-b   amsua_metop-b       10.0     2     0
   airsbufr       amsua       aqua      amsua_aqua           5.0     2     0
   amsubbufr      amsub       n17       amsub_n17            1.0     1     0
   mhsbufr        mhs         n18       mhs_n18              3.0     2     0
   mhsbufr        mhs         n19       mhs_n19              3.0     2     0
   mhsbufr        mhs         metop-a   mhs_metop-a          3.0     2     0
   mhsbufr        mhs         metop-b   mhs_metop-b          3.0     2     0
   ssmitbufr      ssmi        f13       ssmi_f13             0.0     2     0
   ssmitbufr      ssmi        f14       ssmi_f14             0.0     2     0
   ssmitbufr      ssmi        f15       ssmi_f15             0.0     2     0
   amsrebufr      amsre_low   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_mid   aqua      amsre_aqua           0.0     2     0
   amsrebufr      amsre_hig   aqua      amsre_aqua           0.0     2     0
   ssmisbufr      ssmis_las   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_uas   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_img   f16       ssmis_f16            0.0     2     0
   ssmisbufr      ssmis_env   f16       ssmis_f16            0.0     2     0
   gsnd1bufr      sndrd1      g12       sndrD1_g12           1.5     1     0
   gsnd1bufr      sndrd2      g12       sndrD2_g12           1.5     1     0
   gsnd1bufr      sndrd3      g12       sndrD3_g12           1.5     1     0
   gsnd1bufr      sndrd4      g12       sndrD4_g12           1.5     1     0
   gsnd1bufr      sndrd1      g11       sndrD1_g11           1.5     1     0
   gsnd1bufr      sndrd2      g11       sndrD2_g11           1.5     1     0
   gsnd1bufr      sndrd3      g11       sndrD3_g11           1.5     1     0
   gsnd1bufr      sndrd4      g11       sndrD4_g11           1.5     1     0
   gsnd1bufr      sndrd1      g13       sndrD1_g13           1.5     1     0
   gsnd1bufr      sndrd2      g13       sndrD2_g13           1.5     1     0
   gsnd1bufr      sndrd3      g13       sndrD3_g13           1.5     1     0
   gsnd1bufr      sndrd4      g13       sndrD4_g13           1.5     1     0
   gsnd1bufr      sndrd1      g15       sndrD1_g15           1.5     2     0
   gsnd1bufr      sndrd2      g15       sndrD2_g15           1.5     2     0
   gsnd1bufr      sndrd3      g15       sndrD3_g15           1.5     2     0
   gsnd1bufr      sndrd4      g15       sndrD4_g15           1.5     2     0
   iasibufr       iasi        metop-a   iasi616_metop-a     20.0     1     0
   gomebufr       gome        metop-a   gome_metop-a         1.0     2     0
   omibufr        omi         aura      omi_aura             1.0     2     0
   sbuvbufr       sbuv2       n19       sbuv8_n19            1.0     0     0
   tcvitl         tcp         null      tcp                  1.0     0     0
   seviribufr     seviri      m08       seviri_m08           1.0     1     0
   seviribufr     seviri      m09       seviri_m09           1.0     1     0
   seviribufr     seviri      m10       seviri_m10           1.0     1     0
   iasibufr       iasi        metop-b   iasi616_metop-b      0.0     1     0
   gomebufr       gome        metop-b   gome_metop-b         0.0     2     0
   atmsbufr       atms        npp       atms_npp             0.0     1     0
   crisbufr       cris        npp       cris_npp             0.0     1     0
   mlsbufr        mls30       aura      mls30_aura           0.0     0     0
   oscatbufr      uv          null      uv                   0.0     0     0
   prepbufr       mta_cld     null      mta_cld              1.0     0     0
   prepbufr       gos_ctp     null      gos_ctp              1.0     0     0
   refInGSI       rad_ref     null      rad_ref              1.0     0     0
   lghtInGSI      lghtn       null      lghtn                1.0     0     0
   larcglb        larcglb     null      larcglb              1.0     0     0
   glmbufr        light       g16         light              0.0     0     0
::
 &SUPEROB_RADAR
   del_azimuth=5.,del_elev=.25,del_range=5000.,del_time=.5,elev_angle_max=5.,minnum=50,range_max=100000.,
   l2superob_only=.false.,
 /
 &LAG_DATA
 /
 &HYBRID_ENSEMBLE
   l_hyb_ens=${ifhyb},
   uv_hyb_ens=.true.,
   aniso_a_en=.false.,generate_ens=.false.,
   n_ens=${nummem},
   beta_s0=0.5,s_ens_h=110,s_ens_v=3,
   regional_ensemble_option=1,
   pseudo_hybens = .false.,
   grid_ratio_ens = 1,
   l_ens_in_diff_time=.true.,
   ensemble_path='',
 /
 &RAPIDREFRESH_CLDSURF
 /
 &CHEM
 /
 &NST
 /
 &SINGLEOB_TEST
   maginnov=1.0,magoberr=0.8,oneob_type='t',
   oblat=38.,oblon=279.,obpres=500.,obdattim=${ANAL_TIME},
   obhourset=0.,
 /
EOF
