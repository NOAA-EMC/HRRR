 subroutine chem_driver ( grid , config_flags   &
 
#include "dummy_new_args.inc"
 
                 )
!----------------------------------------------------------------------
  USE module_domain !, only : domain
  USE module_configure
  USE module_driver_constants
  USE module_machine
  USE module_tiles
  USE module_dm
  USE module_model_constants
  USE module_state_description
!  USE module_data_radm2
!  USE module_data_sorgam
!  USE module_radm
  USE module_dep_simple
!  USE module_bioemi_simple
!  USE module_phot_mad
!  USE module_phot_tuv,    only : tuv_timestep_init
!  USE module_ftuv_driver, only : ftuv_timestep_init
!  USE module_aerosols_sorgam
!  USE module_chem_utilities
!  USE module_gocart_so2so4
!  USE module_aer_opt_out,only: aer_opt_out
!  USE module_ctrans_grell
!  USE module_data_soa_vbs, only: ldrog_vbs
!  USE module_dust_load
!  USE module_chem_cup, only: chem_cup_driver !BSINGH - For WRFCuP scheme
  USE module_dry_dep_driver
  USE module_emissions_driver
!  USE module_input_tracer, only: set_tracer
!  USE module_wetscav_driver, only: wetscav_driver
  USE module_wetdep_ls, only:wetdep_ls
! USE module_uoc_dustwd ! Claudia, 3 April 2014  [mklose 03082015]
  USE module_input_smoke_data, only: last_chem_time
!#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
!                                     chem_dbg,      &
!#endif
!                                     get_last_gas !,mozcart_lbc_set
!  USE module_upper_bc_driver, only: upper_bc_driver
!  USE module_tropopause,      only: tropopause_driver
!  USE modal_aero_data, only: ntot_amode_cam_mam => ntot_amode
!  USE module_cam_support, only: gas_pcnst => gas_pcnst_modal_aero,gas_pcnst_pos => gas_pcnst_modal_aero_pos, &
!       pcnst =>pcnst_runtime, numgas_mam, cam_mam_aerosols
!  USE module_cu_camzm_driver, only: zm_conv_tend_2
!  USE module_cam_mam_gas_wetdep_driver, only: cam_mam_gas_wetdep_driver
!  USE module_trajectory, only: trajectory_dchm_tstep_init, trajectory_dchm_tstep_set

  IMPLICIT NONE

   TYPE(domain) , TARGET          :: grid
   !
   !  Definitions of dummy arguments to solve
# include "dummy_new_decl.inc"
# define NO_I1_OLD

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER  ::  ids,ide, jds,jde, kds,kde,    &
                ims,ime, jms,jme, kms,kme,    &
                ips,ipe, jps,jpe, kps,kpe,    &
                its,ite, jts,jte, kts,kte
! .. Local Scalars ..
   INTEGER :: ij,i,j,k,l,numgas,nv,n, nr,ktau,k_start,k_end,idf,jdf,kdf, ijulian

! These arrays are calculated here   
   REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: p_phy,t_phy,dz8w,rho,rel_hum

! Variables for adaptive time steps...
      TYPE(WRFU_TimeInterval) :: tmpTimeInterval
      REAL(KIND=8) :: curr_secs
      REAL(KIND=8) :: real_time_r8                                 !ext. function in adapt_timestep_em.F
      LOGICAL      :: adapt_step_flag, do_chemstep !, do_photstep

 !     REAL :: DAYI,DPL,HOUR,PLYR,QI,QR,QW,RADT,TIMES,WC,TDUM,WMSK,RWMSK

 !     REAL :: dtstepc
      CHARACTER (LEN=1000) :: msg
      CHARACTER (LEN=256) :: current_date_char 
      integer :: current_month
      INTEGER, SAVE :: icall
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
      adapt_step_flag = .TRUE.
      ktau = grid%itimestep
      tmpTimeInterval = domain_get_time_since_sim_start(grid)
      curr_secs = real_time_r8(tmpTimeInterval)
      ijulian=ifix(grid%julian)

! RAR: This is for HRRR-Smoke only, to skip chem_driver every other time step
!  IF (MOD(ktau,2)==0)
!     RETURN
!  ENDIF
!      do_photstep = .false.

 ! if( ktau==1 ) then
 !    dtstepc = grid%dt
 ! else
 !    tmpTimeInterval = domain_get_current_time(grid) - last_chem_time(grid%id)
 !    dtstepc = real(real_time_r8(tmpTimeInterval),4)
 ! end if

      do_chemstep = .false.
      IF ( ktau==1 ) then
         do_chemstep = .true.
   !  grid%ktauc = 1
      ELSE IF ( adapt_step_flag ) THEN
        IF ( (grid%chemdt<=0) .or.  ( curr_secs+real(grid%dt,8)+0.01 >= &
           ( INT( curr_secs/real(grid%chemdt*60.,8)+1,8 )*real(grid%chemdt*60.,8) ) ) ) then
             do_chemstep = .true.
    !    grid%ktauc = grid%ktauc+1
            ! last_chem_time(grid%id) = domain_get_current_time( grid )
            ! call WRFU_TimeGet( last_chem_time(grid%id),         &
            !               YY = grid%last_chem_time_year,   &
            !               MM = grid%last_chem_time_month,  &
            !               DD = grid%last_chem_time_day,    &
            !               H  = grid%last_chem_time_hour,   &
            !               M  = grid%last_chem_time_minute, &
            !               S  = grid%last_chem_time_second  )
        ENDIF
      ELSE IF ( (MOD(ktau,grid%stepchem)==0) .or. (grid%stepchem==1) ) THEN
        do_chemstep = .true.
        !grid%ktauc=max(ktau/grid%stepchem,1)
      ENDIF

! RAR: diagnostics to check the time step etc. when adapt_time_step is True
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: ktau,grid%chemdt,grid%stepchem: ',ktau,grid%ktauc,grid%chemdt,grid%stepchem
        WRITE(*,*) 'chem_driver: adapt_step_flag,do_chemstep: ',adapt_step_flag,do_chemstep
        icall=icall+1
    end if

  CALL get_ijk_from_grid (  grid ,                          &
                            ids, ide, jds, jde, kds, kde,   &
                            ims, ime, jms, jme, kms, kme,   &
                            ips, ipe, jps, jpe, kps, kpe    )

! following two lines needed for MEGAN
  CALL domain_clock_get( grid, current_timestr=current_date_char )
  read(current_date_char(6:7),FMT='(I2)') current_month

!  if(config_flags%cu_diag == 0 ) grid%raincv_b(:,:) = grid%raincv(:,:)

  !num_3d_m        = num_moist
  !num_3d_c        = num_chem
  !num_3d_s        = num_scalar
  numgas          = 0               !get_last_gas(config_flags%chem_opt)

   !  Compute these starting and stopping locations for each tile and number of tiles.
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
  k_start         = kps
  k_end           = kpe

! RAR: diagnostics to check the time step etc. when adapt_time_step is True
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: ids, ide, jds, jde, kds, kde ', ids, ide, jds, jde, kds, kde
        WRITE(*,*) 'chem_driver: ims, ime, jms, jme, kms, kme ', ims, ime, jms, jme, kms, kme
        WRITE(*,*) 'chem_driver: ips, ipe, jps, jpe, kps, kpe ', ips, ipe, jps, jpe, kps, kpe
        WRITE(*,*) 'chem_driver: ktau,grid%chemdt,grid%stepchem:', ktau,grid%ktauc,grid%chemdt,grid%stepchem
        WRITE(*,*) 'chem_driver: adapt_step_flag,do_chemstep:', adapt_step_flag,do_chemstep
        icall=icall+1
    end if

!  ijds = min(ids, jds)
!  ijde = max(ide, jde)
!  chem_minval = epsilc !chem_minval can be case dependant and set below...

!------------------------------------------------------------------------
! Main chemistry tile loop
!------------------------------------------------------------------------
!RAR: Do we need to call chem_prep?!
chem_time: IF (do_chemstep) THEN

   chem_tile_loop_1: DO ij = 1, grid%num_tiles
       its = grid%i_start(ij) 
       ite = min(grid%i_end(ij),ide-1)
       jts = grid%j_start(ij)
       jte = min(grid%j_end(ij),jde-1) 
       kts = k_start
       kte = min(k_end,kde-1)
 
!         CALL wrf_debug ( 15 , ' call chem_prep' )
!         CALL chem_prep ( config_flags,                                               &
!                         grid%u_2, grid%v_2, grid%p, grid%pb,             &
!                         grid%alt,grid%ph_2, grid%phb, grid%t_2,          &
!                         moist, num_3d_m, rho,                                        &
!                         p_phy,  u_phy, v_phy,                                        &
!                         p8w, t_phy, t8w, grid%z, z_at_w,                          &
!                         dz8w, rel_hum, grid%fnm, grid%fnp,                              &
!                         ids, ide, jds, jde, kds, kde,                                &
!                         ims, ime, jms, jme, kms, kme,                                &
!                         its,ite,jts,jte,                                             &
!                         k_start, k_end                                               )
        do j = jts,jte
         do k = kts,kte
          do i = its,ite
             rho(i,k,j) = 1./grid%alt(i,k,j)    !*(1.+moist(i,k,j,P_QV))
             p_phy(i,k,j) = grid%p(i,k,j) + grid%pb(i,k,j)
             dz8w(i,k,j)  = grid%z_at_w(i,k+1,j) - grid%z_at_w(i,k,j)
             rel_hum(i,k,j)= max(.1,MIN( .95, moist(i,k,j,p_qv) / &
                                (3.80*exp(17.27*(grid%t_phy(i,k,j)-273.)/ &
                                (grid%t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j)))))
          enddo
         enddo
        enddo
  
    if (icall<2000 .AND. config_flags%debug_chem) then
        WRITE(*,*) 'chem_driver: num_tiles,grid%gmt,num_chem,numgas ', grid%num_tiles,grid%gmt,num_chem,numgas
        WRITE(*,*) 'chem_driver: ktau,grid%dt,curr_secs: ',ktau,grid%dt,curr_secs
        WRITE(*,*) 'chem_driver: its,ite,jts,jte,kts,kte ',its,ite,jts,jte,kts,kte
        WRITE(*,*) 'chem_driver: rho(its,kts,jts),p_phy(its,kts,jts),dz8w(its,kts,jts) ', rho(its,kts,jts),p_phy(its,kts,jts),dz8w(its,kts,jts)
        WRITE(*,*) 'chem_driver: rho(its,kte,jts),p_phy(its,kte,jts),dz8w(its,kte,jte) ', rho(its,kte,jts),p_phy(its,kte,jts),dz8w(its,kte,jte)
        WRITE(*,*) 'chem_driver: rel_hum(its,kts,jts), rel_hum(ite,kte,jte) ',rel_hum(its,kts,jts), rel_hum(ite,kte,jte)
        WRITE(*,*) 'chem_driver: grid%plumerisefire_frq,grid%stepfirepl: ',grid%plumerisefire_frq,grid%stepfirepl
        WRITE(*,*) 'chem_driver: dz8w(its,kts,jts),grid%z_at_w(ite,kte,jte),grid%z_at_w(ite,kte+1,jte): ',dz8w(its,kts,jts),grid%z_at_w(ite,kte,jte),grid%z_at_w(ite,kte+1,jte)
    end if

! RAR: BB emissions are added next
    if (config_flags%biomass_burn_opt==biomassb_smoke)then
!      call wrf_debug(15,'calling emissions driver')

      call emissions_driver(grid%id,ktau,grid%dt,                  &
              adapt_step_flag, curr_secs,                          &
              grid%plumerisefire_frq,grid%stepfirepl,              &
              !grid%bioemdt,grid%stepbioe,                          &
              config_flags,                                        &
              grid%gmt,ijulian,grid%t_phy,moist(:,:,:,p_qv),       &
              grid%u_phy,grid%v_phy,grid%w_2,                      &
              p_phy,rho,dz8w, rel_hum,                             &
              ebu, emis_ant, chem,                                  &
              grid%mean_frp,grid%std_frp,grid%mean_fsize,grid%std_fsize,      &    ! RAR
              grid%coef_bb_dc,grid%fire_hist,grid%aod3d_smoke,                &    ! RAR
              grid%min_fplume,grid%max_fplume,grid%flam_frac,                 &
              grid%ebb_smoke,grid%lu_fire1,grid%peak_hr,                      &
              grid%xlat,grid%xlong,grid%LANDUSEF,grid%num_land_cat,           &
              grid%z_at_w,grid%z,                                             &
              grid%T2,grid%swdown,grid%RAINC,grid%RAINNC,                      &
              current_month,                                                   &
              ids,ide, jds,jde, kds,kde,                                       &
              ims,ime, jms,jme, kms,kme,                                       &
              its,ite,jts,jte,kts,kte                                          )
     endif

     if (config_flags%vertmix_onoff>0) then
         if (ktau.gt.2) then
            call wrf_debug(15,'calling dry_deposition_driver')
            call dry_dep_driver( grid%id,ktau,grid%dt,config_flags,          &
                                 dz8w,rho,rel_hum,grid%exch_h,grid%hfx,       &
                                 grid%pblh,grid%rmol,grid%ust,                                    &
                                 grid%z,grid%z_at_w,                        &
                                 grid%LU_INDEX,grid%ddmass_smoke,                        &
                                 grid%mean_frp,grid%min_fplume,grid%coef_bb_dc,          &
                                 chem,         &
                              !   grid%depvelocity,                                                    &
                              !   grid%dep_vel,grid%num_vert_mix,                                      &
                                 ids,ide, jds,jde, kds,kde,                                           &
                                 ims,ime, jms,jme, kms,kme,                                           &
                                 its,ite,jts,jte,kts,kte                                              )
         end if
     end if

        if(config_flags%wetscav_onoff<0)then
           call wrf_debug(15,'calculate LS wet deposition')
           call wetdep_ls(grid%dt,chem,grid%rainncv,moist,rho,num_moist, &
                num_chem,numgas,dz8w,grid%w_2,grid%chem_opt,                 &
                ids,ide, jds,jde, kds,kde,                               &
                ims,ime, jms,jme, kms,kme,                               &
                its,ite, jts,jte, kts,kte                                )
        endif

! Fill top level to prevent spurious interpolation results (no extrapolation)
      do nv=2,num_chem
         do j=jts,jte
            do i=its,ite
                  chem(i,k_end,j,nv)=chem(i,kte,j,nv)
            enddo
         enddo
      enddo

   END DO chem_tile_loop_1
END IF chem_time  

   END subroutine chem_driver
