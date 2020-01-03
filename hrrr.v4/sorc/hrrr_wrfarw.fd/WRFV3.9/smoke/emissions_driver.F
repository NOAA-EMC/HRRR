MODULE module_emissions_driver
   IMPLICIT NONE
CONTAINS

    subroutine emissions_driver(id,ktau,dtstep,                               &
                                adapt_step_flag,curr_secs,                    &
                                plumerisefire_frq,stepfirepl,                 &
                                !bioemdt,stepbioe,                             &
                                config_flags,                                 &
                                gmt,julday,t_phy,qvapor,                       &    ! RAR: scalar will be added for the qnwfa, qnifa
                                u_phy,v_phy,vvel,                             &
                                p_phy,rho_phy,dz8w,rel_hum,                 &
!         emis_vol,tsk,erod,erod_dri,lai_vegmask,                           &
!         g,emis_seas,emis_dust,tracer,                                     &
!         emis_seas2,                                                       &
                                ebu, emis_ant, chem,                &
!                                mean_fct_agtf,mean_fct_agef,                          &
!                                mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,          &
!                                firesize_agsv,firesize_aggr,                                      &

                                mean_frp,std_frp,mean_fsize,std_fsize,                    &    ! RAR:
                                coef_bb_dc,fire_hist,aod3d_smoke,                         &    ! RAR:
                                min_fplume, max_fplume,flam_frac,                        &
                                ebb_smoke,lu_fire1,peak_hr,                       &

!                                u10,v10,ivgtyp,isltyp,                                       &
                                xlat,xlong,luf_igbp,nlcat,                 &
                                z_at_w,zmid,                               &
!         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
!         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &
!         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
!         sebio_sesq,sebio_mbo,                                             &
!         noag_grow,noag_nongrow,nononag,slai,                              &
!         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
!         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
!         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
!         ebio_sesq, ebio_mbo,ebio_bpi,ebio_myrc,                           &
!         ebio_c10h16,ebio_tol,ebio_bigalk,ebio_ch3oh,ebio_acet,            &
!         ebio_nh3,ebio_no2,ebio_c2h5oh,ebio_ch3cooh,ebio_mek,              &
!         ebio_bigene,ebio_c2h6,ebio_c2h4,ebio_c3h6,ebio_c3h8,ebio_so2,     &
!         ebio_dms,                                                         &
!         ebio_alk3, ebio_alk4, ebio_alk5, ebio_ole1, ebio_ole2,            &
!         ebio_aro1, ebio_aro2, ebio_ccho, ebio_meoh,                       &
!         ebio_ethene, ebio_hcooh, ebio_terp, ebio_bald,                    &
!         ebio_cco_oh, ebio_rco_oh,                                         &
!         clayfrac,sandfrac,dust_alpha,dust_gamma,dust_smtune,dust_ustune,  &
!         clayfrac_nga,sandfrac_nga,                                        &
!         snowh,zs,afwa_dustloft,tot_dust,tot_edust,vis_dust,               &
!         soil_top_cat, ust_t, rough_cor, smois_cor,                        &
!         ebio_c5h8,ebio_apinene,ebio_bpinene,ebio_toluene,                 &
!         ebio_ch3cho,ebio_ch3co2h,ebio_tbut2ene,ebio_c2h5cho,              &
!         ebio_nc4h10,							   &
         ! stuff for MEGAN v2.04                                                 
         T2,swdown,rainc,rainnc,                                            &
!         nmegan,EFmegan,                                                   &
!         msebio_isop,                                                      &
!         mlai,                                                             &
!         pftp_bt, pftp_nt, pftp_sb, pftp_hb,                               &
!         mtsa,                                                             &
!         mswdown,                                                          &
!         mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                   &
!         mebio_acet, mebio_mbo, mebio_no,                                  &
         current_month,                                                    &
         ! end stuff for MEGAN v2.04
         ! stuff for LNOx emissions
!         ht, refl_10cm,                                                    &
!         ic_flashrate, cg_flashrate,                                       &
         ! end stuff for LNOx emissions
         ! stuff for aircraft emissions
!         emis_aircraft,                                                    &
         ! stuff for GHG fluxes
!         vprm_in,rad_vprm,lambda_vprm,alpha_vprm,resp_vprm,                &
!         xtime,tslb,wet_in,rainc,rainnc,potevp,sfcevp,lu_index,            &
!         biomt_par,emit_par,ebio_co2oce,eghg_bio,                          &
!         dust_flux, seas_flux,                                             &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
!  USE module_data_radm2
!  USE module_data_sorgam, only : mw_so4_aer,anthfac,factnumn,factnuma,factnumc
!  USE module_model_constants, only : mwdry
!  USE module_emissions_anthropogenics
!  USE module_bioemi_simple
!  USE module_bioemi_beis314
!  USE module_bioemi_megan2
!  USE module_aerosols_sorgam, only: sorgam_addemiss
!  USE module_cbmz_addemiss
!  USE module_cb05_addemiss
!  USE gocart_dust
!  USE gocart_dust_afwa
!  USE gocart_seasalt
!  USE uoc_dust
!  USE module_dms_emis
!  USE module_mosaic_addemiss
!  USE module_add_emis_cptec
  USE module_add_emiss_burn
  USE module_plumerise1
!  USE module_aerosols_sorgam_vbs, only: sorgam_vbs_addemiss
!  USE module_aerosols_soa_vbs, only: soa_vbs_addemiss
!  USE module_ghg_fluxes
!  USE module_lightning_nox_driver
!  USE module_cam_mam_addemiss, only: cam_mam_addemiss
  
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,julday,nlcat,                         &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,INTENT(IN) ::  ktau,stepfirepl
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                &
         INTENT(IN ) ::                                   qvapor

!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_scalar ),                &   ! RAR:
!         INTENT(INOUT ) ::                                   scalar

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_tracer ),               &
!         INTENT(INOUT ) ::                                   tracer
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ),                  &
         INTENT(INOUT ) ::                                   ebu
!   REAL, DIMENSION( ims:ime, 1, jms:jme, num_ebu_in ),                     &
!         INTENT(IN ) ::                                      ebu_in
! 2D smoke emissions
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) ::    ebb_smoke, peak_hr, lu_fire1

!   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                           &
!         INTENT(INOUT ) ::                               e_bio
   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant), INTENT(IN ) ::      emis_ant

   REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN ) ::  mean_frp,std_frp,mean_fsize,std_fsize

   REAL, DIMENSION(ims:ime,jms:jme ), INTENT(OUT )    ::   coef_bb_dc, flam_frac
   REAL, DIMENSION(ims:ime,jms:jme ), INTENT(INOUT )  ::   fire_hist      ! RAR
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme ), INTENT(OUT ) ::   aod3d_smoke      ! RAR
!
   REAL, DIMENSION(ims:ime,kms:kme,jms:jme ), INTENT(IN) ::  t_phy,p_phy,dz8w, rel_hum,    &
                                                             z_at_w , zmid ,                   &
                                                             u_phy,v_phy,vvel,rho_phy

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::  xlat,xlong,rainc,rainnc

   REAL, DIMENSION( ims:ime,1:nlcat,jms:jme ),INTENT(IN)  ::  luf_igbp

   INTEGER,DIMENSION( ims:ime,jms:jme ), INTENT(OUT) ::  min_fplume, max_fplume

      real, dimension (ims:ime, jms:jme), intent(in) :: T2, swdown
      integer, intent(in) :: current_month

      REAL(KIND=8), INTENT(IN   ) :: curr_secs

     ! integer :: curr_hours
      Integer :: endhr,endmin,beghr,begmin,ko,kk4,kl,k_initial,k_final
!      real :: emiss_ash_mass,emiss_ash_height,so2_mass,vert_mass_dist(kts:kte)
      real :: eh,area,x1,percen_mass_umbrel,base_umbrel,ashz_above_vent

      REAL, INTENT(IN   ) ::  dtstep, gmt 
      REAL :: smold_frac

      INTEGER, INTENT(IN   ) ::  plumerisefire_frq
      LOGICAL, INTENT(IN   ) ::  adapt_step_flag

! Local variables...
      INTEGER :: begday,endday,i, j, k  !, dust_emiss_active, seasalt_emiss_active,emiss_ash_hgt
      REAL :: conv,conv3,conv4,oconv3,oconv4
      CHARACTER (LEN=80) :: message
      LOGICAL :: do_plumerisefire  !,do_ex_volcanoe

! RAR:
    INTEGER, SAVE :: icall

    REAL, DIMENSION(ims:ime,jms:jme,4) :: plume_fre

    !plume_fre(:,:,1)= flam_frac
!    plume_fre(:,:,1)= 1.e+6*mean_frp    ! unit conversions for the smoke modeling, chem_opt=18
!    plume_fre(:,:,2)= 1.e+6*std_frp
!    plume_fre(:,:,3)= 1.e+6*mean_size
!    plume_fre(:,:,4)= 1.e+6*std_size

!smold_frac= 1.- config_flags%flam_part

 IF (ktau==1) THEN
    do j=jts,jte
       do i=its,ite
          ebu(i,kts,j,p_ebu_smoke)= ebb_smoke(i,j)
          do k=kts+1,kte
            ebu(i,k,j,p_ebu_smoke)= 0.
          enddo
       enddo
    enddo
 ENDIF
!--------------------------------------------------------------------------------------
! RAR: we start with 2nd time step as the coef_bb_dc=0 at ktau=1
      do_plumerisefire = .false.
      !IF ( config_flags ) THEN  ! option 4
        IF ( ktau==2 ) then
           do_plumerisefire = .true.
        ELSE IF ( adapt_step_flag ) THEN
           IF ( (plumerisefire_frq<=0)  .or.  ( curr_secs+real(dtstep,8)+0.01 >= &
                ( INT( curr_secs/real(plumerisefire_frq*60.,8)+1,8 )*real(plumerisefire_frq*60.,8) ) ) ) then
              do_plumerisefire = .true.
           ENDIF
        ELSE IF ( (MOD(ktau,stepfirepl)==0) .or. (stepfirepl==1) ) THEN
           do_plumerisefire = .true.
        ENDIF
      !ENDIF

!      do_bioemiss = .false.
!      IF ( ktau==1 ) then
!         do_bioemiss = .true.
!      ELSE IF ( adapt_step_flag ) THEN
!         IF ( (bioemdt<=0) .or. &
!              ( curr_secs+real(dtstep,8)+0.01 >= &
!              ( INT( curr_secs/real(bioemdt*60.,8)+1,8 )*real(bioemdt*60.,8) ) ) &
!              ) then
!            do_bioemiss = .true.
!         ENDIF
!      ELSE IF ( (MOD(ktau,stepbioe)==0) .or. (stepbioe==1) ) THEN
!         do_bioemiss = .true.
!      ENDIF
!
! we are doing the plumerise/fire emissions first, they may be needed for chem and tracer arrays
!
       if (icall<3000 .AND. config_flags%debug_chem) then
           WRITE(6,*) 'emissions_driver: time is ',curr_secs   
           WRITE(6,*) 'emissions_driver: do_plumerisefire,plumerisefire_frq: ',do_plumerisefire,plumerisefire_frq
       end if

! In case config_flags%plumerise_flag=0, then the emissions are emitted at kts level only
       IF ( do_plumerisefire ) THEN
          CALL wrf_debug(15,'fire emissions: calling biomassb')

          do j=jts,jte
            do i=its,ite
               plume_fre(i,j,1)= 1.e+6*coef_bb_dc(i,j)* mean_frp(i,j)
               plume_fre(i,j,2)= 1.e+6*coef_bb_dc(i,j)* std_frp(i,j)
               plume_fre(i,j,3)= 1.e+6*coef_bb_dc(i,j)* mean_fsize(i,j)
               plume_fre(i,j,4)= 1.e+6*coef_bb_dc(i,j)* std_fsize(i,j)
            enddo
          enddo

          IF (icall<5000 .AND. config_flags%debug_chem) then
              WRITE(6,*) 'emissions_driver: plumerise is called at'
              WRITE(6,*) 'curr_secs= ',curr_secs
              WRITE(6,*) 'emissions_driver: ktau ',ktau
              icall=icall+1
          END IF
   
          call plumerise_driver (id,                                           &
                      flam_frac,ebb_smoke,ebu,                                                  &
                      config_flags,                                                   &
                      t_phy,qvapor,                                                   &
                      rho_phy,vvel,u_phy,v_phy,p_phy,                                 &
                      z_at_w,zmid,ktau,                                               & 
                      plume_fre, min_fplume, max_fplume,                              &   ! new approach
                      ids,ide, jds,jde, kds,kde,                                      &
                      ims,ime, jms,jme, kms,kme,                                      &
                      its,ite, jts,jte, kts,kte                                       )
       ENDIF

!smoke_opt: IF (config_flags%chem_opt==CHEM_SMOKE) THEN
!            if (config_flags%biomass_burn_opt==biomassb_smoke) then
               call wrf_debug(15,'Add BB fluxes for the CHEM_SMOKE option')
               call add_emis_burn(id,dtstep,ktau,dz8w,rho_phy,rel_hum,chem,         &
                                  julday,gmt,xlat,xlong,luf_igbp,nlcat,             &
                                  lu_fire1,peak_hr,                                 &   ! RAR
                                  curr_secs,ebu,                                    &
                                  coef_bb_dc,fire_hist,aod3d_smoke,                &
        !                          scalar(ims,kms,jms,P_QNWFA),scalar(ims,kms,jms,P_QNIFA),     &
                                  rainc, rainnc,swdown,                                        &
                                  config_flags,                                                &
                                  ids,ide, jds,jde, kds,kde,                                   &
                                  ims,ime, jms,jme, kms,kme,                                   &
                                  its,ite, jts,jte, kts,kte                                    )
!            end if

!        ENDIF smoke_opt

    END subroutine emissions_driver

END module module_emissions_driver
