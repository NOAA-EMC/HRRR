!WRF:MODEL_LAYER:INITIALIZATION
!
   subroutine smoke_init (id,chem,                         &
                         config_flags,grid,                            &
!                         ttday,tcosz,                                  &
!                         last_chem_time_year,   last_chem_time_month,            &
!                         last_chem_time_day,    last_chem_time_hour,              &
!                         last_chem_time_minute, last_chem_time_second,         &
                         ids,ide, jds,jde, kds,kde,                            &
                         ims,ime, jms,jme, kms,kme,                            &
                         its,ite, jts,jte, kts,kte                             )

   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_dep_simple
   USE module_input_smoke_data, only:      last_chem_time

   IMPLICIT NONE

!   real  , intent(in) :: dt
!   INTEGER,      INTENT(IN   ) :: chem_in_opt
!   INTEGER,      INTENT(INOUT) :: num_vert_mix
   INTEGER,      INTENT(IN   ) :: id,                                &
                                  !last_chem_time_year,               &
                                  !last_chem_time_month,              &
                                  !last_chem_time_day,                &
                                  !last_chem_time_hour,               &
                                  !last_chem_time_minute,             &
                                  !last_chem_time_second,             &
                                  ids,ide, jds,jde, kds,kde,         &
                                  ims,ime, jms,jme, kms,kme,         &
                                  its,ite, jts,jte, kts,kte
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) ,       &
          INTENT(INOUT   ) ::    chem 
  
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags

!   integer :: stepbioe,stepphot,stepchem,stepfirepl
   TYPE(domain) ,             INTENT (inout) ::     grid
!
! local stuff
!
   CHARACTER*256 :: mminlu_loc
   CHARACTER*256 :: message_txt
   TYPE(WRFU_TimeInterval) :: tmpTimeInterval
   integer :: i,j,k,l,numgas,ixhour,n,ndystep,kk,nv
   real, DIMENSION (1,1) :: sza,cosszax
   real :: xtime,xhour,xmin,gmtp,xlonn,rlat
#include "version_decl"
#ifdef CHEM_DBG_I
    call print_chem_species_index( config_flags%chem_opt )
#endif
   program_name = "*             PROGRAM:WRF-Smoke " // TRIM(release_version) // " MODEL"

call wrf_message("*********************************************************************")
call wrf_message(program_name)
call wrf_message("*                                                                   *")
call wrf_message("*            PLEASE REPORT ANY BUGS TO                              *")
call wrf_message("*              ravan.ahmadov@noaa.gov                               *")
call wrf_message("*                                                                   *")
call wrf_message("*********************************************************************")

    numgas = 0  !get_last_gas(config_flags%chem_opt)

    IF ( config_flags%chem_opt /= 18 .OR. config_flags%tracer_opt>0) THEN
        call wrf_error_fatal(" ERROR: This version of WRF-Chem doesn't work with other chemistry or tracer options ")
    ENDIF

    IF ( config_flags%flam_part>1.) THEN
        call wrf_error_fatal(" ERROR: too high flam_part ")
    ENDIF

    IF (config_flags%plumerise_flag/=2 .AND. config_flags%flam_part>0.) THEN
       call wrf_error_fatal(" ERROR: check flam_part and plumerise_flag for consistency! ")
    ENDIF

    CALL nl_get_mminlu( 1, mminlu_loc )

!    IF (trim(mminlu_loc) /= 'USGS' .and. trim(mminlu_loc) /= 'MODIFIED_IGBP_MODIS_NOAH' ) THEN
    IF ( trim(mminlu_loc) /= 'MODIFIED_IGBP_MODIS_NOAH' ) THEN
         print*,mminlu_loc
         message_txt = " ERROR: HRRR-Smoke: The smoke routines require MODIS_NOAH land use maps. Need to change land use option."
         call wrf_error_fatal( trim(message_txt) )
    ELSEIF (trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' .and. grid%num_land_cat <= 19 ) THEN
            message_txt = " ERROR: CHEM_INIT: MODIS_NOAH land use map should have 20 or more catagories."
            call wrf_error_fatal( trim(message_txt) )
    ENDIF

   ! grid%stepbioe=nint(grid%bioemdt*60./dt)
   ! grid%stepphot=nint(grid%photdt*60./dt)
    
    grid%stepchem  = nint(grid%chemdt*60./grid%dt)
    grid%stepchem  = max(grid%stepchem,1)

    grid%stepfirepl= nint(grid%plumerisefire_frq*60/grid%dt)
    grid%stepfirepl= max(grid%stepfirepl,1)

    !grid%stepbioe=max(stepbioe,1)
    !grid%stepphot=max(stepphot,1)
    !grid%stepchem=max(stepchem,1)
    !grid%stepfirepl=max(stepfirepl,1)
    call wrf_debug( 15, 'in smoke_init' )
!   print *,'stepbioe,stepphot,stepchem,stepfirepl',stepbioe,stepphot,stepchem,stepfirepl

! Initialization of the 2D arrays for smoke modeling
!  IF ( config_flags%chem_opt==18 ) THEN
     do j=jts,jte
       do i=its,ite
          grid%lu_fire1(i,j)= grid%landusef(i,11,j) + grid%landusef(i,12,j) + grid%landusef(i,13,j) + grid%landusef(i,14,j) + grid%landusef(i,15,j) + &
                              grid%landusef(i,16,j) + grid%landusef(i,17,j) + grid%landusef(i,18,j) + grid%landusef(i,19,j) + grid%landusef(i,20,j)   

          if (grid%xlong(i,j)<-130.) then
              grid%peak_hr(i,j)= 0.0* 3600.    ! peak at 24 UTC, fires in Alaska
          elseif(grid%xlong(i,j)<-115.) then
              grid%peak_hr(i,j)= 23.0* 3600.
          elseif (grid%xlong(i,j)<-100.) then
              grid%peak_hr(i,j)= 22.0* 3600.    ! peak at 22 UTC, fires in the western US
          elseif (grid%xlong(i,j)<-85.) then
              grid%peak_hr(i,j)= 21.0* 3600. 
          elseif (grid%xlong(i,j)<-70.) then         ! peak at 20 UTC, fires in the eastern US
              grid%peak_hr(i,j)= 20.0* 3600.
          else
              grid%peak_hr(i,j)= 19.0* 3600.
          endif
       enddo
     enddo

   if( .NOT. config_flags%restart  ) then
         if(config_flags%chem_in_opt == 0 )then
               do j=jts,jte
                 do i=its,ite
!                    fire_hist(i,j)= 1.
                    do k=kts,kte
                      chem(i,k,j,:)= 1.e-16
                    enddo
                 enddo
               enddo
          endif
    endif

! Setup time info for chemistry
!
   if( config_flags%restart ) then
       call wrf_debug( 15, "Setting last_chem_time from restart file" )
! Do not know if I need the following nl_gets.
! domain_get_sim_start_time uses them for simulation_start_...
!          CALL nl_get_start_year   ( 1, last_chem_time_year   )
!          CALL nl_get_start_month  ( 1, last_chem_time_month  )
!          CALL nl_get_start_day    ( 1, last_chem_time_day    )
!          CALL nl_get_start_hour   ( 1, last_chem_time_hour   )
!          CALL nl_get_start_minute ( 1, last_chem_time_minute )
!          CALL nl_get_start_second ( 1, last_chem_time_second )
       call WRFU_TimeSet( last_chem_time(id),         &
                          YY = grid%last_chem_time_year,   &
                          MM = grid%last_chem_time_month,  &
                          DD = grid%last_chem_time_day,    &
                          H  = grid%last_chem_time_hour,   &
                          M  = grid%last_chem_time_minute, &
                          S  = grid%last_chem_time_second  )
   else
       call wrf_debug( 15, "Setting last_chem_time to model start time-dt" )
       call WRFU_TimeIntervalSet(tmpTimeInterval, s_=real(grid%dt,8))
       last_chem_time(id) = domain_get_current_time(grid) - tmpTimeInterval
   end if

   IF (config_flags%debug_chem) THEN
      WRITE(*,*) 'smoke_init: grid%xlong(its,jts),grid%xlat(its,jts),peak_hr(its,jts) ',grid%xlong(its,jts),grid%xlat(its,jts),grid%peak_hr(its,jts)
      WRITE(*,*) 'smoke_init: grid%xlong(ite,jte),grid%xlat(ite,jte),peak_hr(ite,jte) ',grid%xlong(ite,jte),grid%xlat(ite,jte),grid%peak_hr(ite,jte)
   END IF


!-- initialization upper boundary and tropopause modules
!
!    if( config_flags%have_bcs_upper ) then
!        CALL wrf_debug(00,'call upper boundary initialization')
!        call upper_bc_init( id, grid%xlat, dt, config_flags,  &
!                            ids,ide, jds,jde, kds,kde,   &
!                            ims,ime, jms,jme, kms,kme,   &
!                            its,ite, jts,jte, kts,kte    )

!        call tropopause_init( id, grid%xlat, grid%xlong, config_flags, &
!                            ids,ide, jds,jde, kds,kde,       &
!                            ims,ime, jms,jme, kms,kme,       &
!                            its,ite, jts,jte, kts,kte        )
!    end if
!    gas_pcnst_modal_aero_pos = max(1,gas_pcnst_modal_aero) !BSINGH - Fix for non-MAM simulations
    END SUBROUTINE smoke_init
