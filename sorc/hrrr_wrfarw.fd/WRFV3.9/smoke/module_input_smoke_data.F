!CPP directives to control ic/bc conditions...
!(The directive in module_mosaic_initmixrats also needs to be set.)
!  CASENAME = 0   Uses Texas August 2004 case values and profiles
!             1   Uses same concentrations as TX, but uses different
!                 profiles depending on the species. (NEAQS2004 case)
!#define CASENAME 0

MODULE module_input_smoke_data

    USE module_io_domain
    USE module_domain
!   USE module_data_sorgam, ONLY : conmin, rgasuniv, epsilc, grav
    USE module_get_file_names, ONLY : eligible_file_name, number_of_eligible_files, unix_ls

   IMPLICIT NONE

!  REAL :: grav = 9.8104
    REAL, PARAMETER :: conmin=1.E-16, epsilc=1.E-16

! Variables for adaptive time steps...
    TYPE(WRFU_Time), DIMENSION(max_domains) :: last_chem_time

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initial atmospheric chemistry profile data
    INTEGER :: k_loop          ! Used for loop index
    INTEGER :: lo              ! number of chemicals in inital profile
    INTEGER :: logg            ! number of final chemical species (nch-1)
    INTEGER :: kx              ! number of vertical levels in temp profile        
    INTEGER :: kxm1

    PARAMETER( kx=16, kxm1=kx-1, logg=350, lo=34) ! DL (6/2/2013) Changed value of logg from 200 to 350 for additional gas species
   
    INTEGER, DIMENSION(logg)                     :: iref
    REAL, DIMENSION(logg)                        :: fracref
    REAL, DIMENSION(kx)                          :: dens
    REAL, DIMENSION(kx+1)                        :: zfa
    REAL, DIMENSION(kx+1)                        :: zfa_bdy
    REAL, DIMENSION(lo,kx)                     :: xl
!    REAL                                         :: so4vaptoaer
!    DATA so4vaptoaer/.999/

!    CHARACTER (LEN=20), DIMENSION(logg) :: ggnam !BSINGH(12/04/13): changed length(LEN) from 4 to 20

!    type(lbc_concentration), allocatable :: fixed_lbc(:)

CONTAINS

SUBROUTINE vinterp_chem(nx1, nx2, ny1, ny2, nz1, nz_in, nz_out, nch, z_in, z_out, &
                 data_in, data_out, extrapolate)

    ! Interpolates columns of chemistry data from one set of height surfaces to
    ! another
 
    INTEGER, INTENT(IN)                :: nx1, nx2
    INTEGER, INTENT(IN)                :: ny1, ny2
    INTEGER, INTENT(IN)                :: nz1
    INTEGER, INTENT(IN)                :: nz_in
    INTEGER, INTENT(IN)                :: nz_out
    INTEGER, INTENT(IN)                :: nch
    REAL, INTENT(IN)                   :: z_in (nx1:nx2,nz1:nz_in ,ny1:ny2)
    REAL, INTENT(IN)                   :: z_out(nx1:nx2,nz1:nz_out,ny1:ny2)
    REAL, INTENT(IN)                   :: data_in (nx1:nx2,nz1:nz_in ,ny1:ny2,nch)
    REAL, INTENT(OUT)                  :: data_out(nx1:nx2,nz1:nz_out,ny1:ny2,nch)
    LOGICAL, INTENT(IN)                :: extrapolate
    INTEGER                            :: i,j,l
    INTEGER                            :: k,kk
    REAL                               :: desired_z
    REAL                               :: dvaldz
    REAL                               :: wgt0
  
!   Loop over the number of chemical species
    chem_loop: DO l = 2, nch

      data_out(:,:,:,l) = -99999.9

      DO j = ny1, ny2
        DO i = nx1, nx2

          output_loop: DO k = nz1, nz_out

            desired_z = z_out(i,k,j)
            IF (desired_z .LT. z_in(i,1,j)) THEN

              IF ((desired_z - z_in(i,1,j)).LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,1,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate downward because desired height level is below
                  ! the lowest level in our input data.  Extrapolate using simple
                  ! 1st derivative of value with respect to height for the bottom 2
                  ! input layers.
  
                  ! Add a check to make sure we are not using the gradient of 
                  ! a very thin layer
  
                  IF ( (z_in(i,1,j) - z_in(i,2,j)) .GT. 0.001) THEN
                    dvaldz = (data_in(i,1,j,l) - data_in(i,2,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,2,j) )
                  ELSE
                    dvaldz = (data_in(i,1,j,l) - data_in(i,3,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,3,j) )
                  ENDIF
                  data_out(i,k,j,l) = MAX( data_in(i,1,j,l) + &
                                dvaldz * (desired_z-z_in(i,1,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,1,j,l)
                ENDIF
              ENDIF
            ELSE IF (desired_z .GT. z_in(i,nz_in,j)) THEN
              IF ( (z_in(i,nz_in,j) - desired_z) .LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,nz_in,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate upward
                  IF ( (z_in(i,nz_in-1,j)-z_in(i,nz_in,j)) .GT. 0.0005) THEN
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-1,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-1,j))
                  ELSE
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-2,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-2,j)) 
                  ENDIF
                  data_out(i,k,j,l) =  MAX( data_in(i,nz_in,j,l) + &
                           dvaldz * (desired_z-z_in(i,nz_in,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,nz_in,j,l)
                ENDIF
              ENDIF
            ELSE
              ! We can trap between two levels and linearly interpolate
  
              input_loop:  DO kk = 1, nz_in-1
                IF (desired_z .EQ. z_in(i,kk,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk,j,l)
                  EXIT input_loop
                ELSE IF (desired_z .EQ. z_in(i,kk+1,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk+1,j,l)
                  EXIT input_loop
                ELSE IF ( (desired_z .GT. z_in(i,kk,j)) .AND. &
                          (desired_z .LT. z_in(i,kk+1,j)) ) THEN
                  wgt0 = (desired_z - z_in(i,kk+1,j)) / &
                         (z_in(i,kk,j)-z_in(i,kk+1,j))
                  data_out(i,k,j,l) = MAX( wgt0*data_in(i,kk,j,l) + &
                                    (1.-wgt0)*data_in(i,kk+1,j,l), 0.)
                  EXIT input_loop
                ENDIF        
              ENDDO input_loop

            ENDIF
          ENDDO output_loop
        ENDDO 
      ENDDO 
    ENDDO chem_loop

    RETURN
  END SUBROUTINE vinterp_chem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE input_chem_profile (si_grid)

   IMPLICIT NONE

   TYPE(domain)           ::  si_grid

   INTEGER :: i , j , k, &
              ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe    
   INTEGER :: fid, ierr, numgas
   INTEGER :: debug_level

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: si_zsigf, si_zsig


   CHARACTER (LEN=80) :: inpname, message

   write(message,'(A)') 'Subroutine input_chem_profile: '
   CALL  wrf_message ( TRIM(message) )

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1,debug_level )
   CALL set_wrf_debug_level ( debug_level )
   
   ! Get grid dimensions
   CALL get_ijk_from_grid (  si_grid ,                        &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   IF ( si_grid%chem_opt == CHEM_TRACER ) THEN
     si_grid%chem(ims:ime,kms:kme,jms:jme,:) = 0.0
   ENDIF

    RETURN

  END SUBROUTINE input_chem_profile

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE make_chem_profile ( nx1, nx2, ny1, ny2, nz1, nz2, nch, numgas, &
                                 chem_opt, zgrid, chem )
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nx1, ny1, nz1
    INTEGER, INTENT(IN) :: nx2, ny2, nz2
    INTEGER, INTENT(IN) :: nch, numgas, chem_opt
    !REAL, INTENT(IN), DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid

    CHARACTER (LEN=80) :: message
    INTEGER :: i, j, k, l, is

    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2,lo+1):: chprof
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2)     :: zprof

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,nch) :: chem
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,lo ) :: stor

    REAL :: hc358 !wig, 2-May-2007
    REAL :: olit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Check the number of species

     if( nch .NE. num_chem) then
       message = ' Input_chem_profile: wrong number of chemical species'
!       CALL WRF_ERROR_FATAL ( message )
     endif
       
      ! Vertically flip the chemistry data as it is given top down and
      ! heights are bottom up. Fill temp 3D chemical and profile array,
      ! keep chem slot 1 open as vinterp_chem assumes there is no data.
      DO j=ny1,ny2
      DO k=  1,kx 
      DO i=nx1,nx2 
         chprof(i,k,j,2:lo+1) = xl(1:lo,kx-k+1)
         zprof(i,k,j) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
      ENDDO
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
!     DO i=lo-6,lo               
!       xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
!     ENDDO

! Change number concentrations to mixing ratios for short-lived NALROM species
      do k=1,kx
         chprof(:,k,:,lo-5:lo+1) = chprof(:,k,:,lo-5:lo+1)/dens(k)
      end do

      ! Interpolate temp 3D chemical and profile array to WRF grid
      call vinterp_chem(nx1, nx2, ny1, ny2, nz1, kx, nz2, lo, zprof, zgrid, &
                          chprof, chem, .false.)

      ! place interpolated data into temp storage array
      stor(nx1:nx2,nz1:nz2,ny1:ny2,1:lo) = chem(nx1:nx2,nz1:nz2,ny1:ny2,2:lo+1)

      ! Here is where the chemistry profile is constructed
      !chem(:,:,:,1) = stor(:,:,:,1) * 0.
      chem(nx1:nx2,nz1:nz2,ny1:ny2,1) = -999.

!      DO l=2,nch
      DO l=2,numgas
        is=iref(l-1)
        DO j=ny1,ny2
        DO k=nz1,nz2
        DO i=nx1,nx2 
          chem(i,k,j,l)=fracref(l-1)*stor(i,k,j,is)*1.E6
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!
! For CBMZ, we need to construct PAR based on a combination of other
! species. This cannot be done with the looping construct above so
! we have to treat it separately. (wig, 2-May-2007)
!
      SELECT CASE(chem_opt)
!      CASE (CBMZ,CBMZ_BB,CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, &
!            CBMZ_MOSAIC_4BIN,CBMZ_MOSAIC_8BIN, &
!            CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ, &
!            CBMZSORG,CBMZSORG_AQ, CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, & 
!            CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
!            CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
!         do j = ny1,ny2
!         do k = nz1,nz2
!         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
!            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
!                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
!                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
!                    )*1.E6
!            chem(i,k,j,p_par) =                                    &
!                 0.4*chem(i,k,j,p_ald) + hc358                     &
!                 + 0.9*chem(i,k,j,p_ket) + 2.8*chem(i,k,j,p_oli)   &
!                 + 1.8*chem(i,k,j,p_olt) + 1.0*chem(i,k,j,p_ora2)
!         end do
!         end do
!         end do
      CASE (CBM4_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            olit = ( 0.9*fracref(numgas+4)*stor(i,k,j,iref(numgas+4)) &
                     +2.8*fracref(numgas+5)*stor(i,k,j,iref(numgas+5)) &
                     +1.8*fracref(numgas+6)*stor(i,k,j,iref(numgas+6)) &
                     +1.0*fracref(numgas+7)*stor(i,k,j,iref(numgas+7)) &
                    )*1.E6
            chem(i,k,j,p_par) =  0.4*chem(i,k,j,p_ald2) + hc358  + olit
         end do
         end do
         end do

      CASE (CB05_SORG_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) =                                    &
                 0.4*chem(i,k,j,p_ald2)  + hc358                     &
                 +0.4*chem(i,k,j,p_aldx)  + 2.8*chem(i,k,j,p_ole)    &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do

      CASE (CB05_SORG_VBS_AQ_KPP)
         do j = ny1,ny2
         do k = nz1,nz2
         do i = nx1,nx2
            !Construct the sum of the profiles for hc3, hc5, & hc8
            hc358 = ( 2.9*fracref(numgas+1)*stor(i,k,j,iref(numgas+1)) &
                     +4.8*fracref(numgas+2)*stor(i,k,j,iref(numgas+2)) &
                     +7.9*fracref(numgas+3)*stor(i,k,j,iref(numgas+3)) &
                    )*1.E6
            chem(i,k,j,p_par) =                                    &
                 0.4*chem(i,k,j,p_ald2)  + hc358                     &
                 +0.4*chem(i,k,j,p_aldx)  + 2.8*chem(i,k,j,p_ole)    &
                 + 1.8*chem(i,k,j,p_iole) + 1.0*chem(i,k,j,p_aacd)
         end do
         end do
         end do

      END SELECT

      RETURN
  END SUBROUTINE make_chem_profile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
  SUBROUTINE bdy_chem_value_tracer ( chem, nch )

! This subroutine is called to set the boundary values of chemistry
! species when chem_opt==CHEM_TRACER. Typically, the boundary values
! here should be set to match those in input_chem_profile so that the
! interior and boundary values are the same.
! William.Gustafson@pnl.gov; 16-Jun-2005

    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if( nch .ne. p_co  ) then
       chem = 0.0001
    else if( nch == p_co ) then
       chem = 0.08
    else
       chem = conmin
    end if
    if( nch .eq. p_tracer_1  ) then
       chem = 0.08
    endif

  END SUBROUTINE bdy_chem_value_tracer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE bdy_chem_value ( chem, z, nch, numgas )
                                  
    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
    INTEGER, intent(IN)   :: numgas     ! index number of last gas species

    INTEGER :: i, k, irefcur

    REAL, DIMENSION(kx):: cprof         ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL                 :: stor
    REAL                 :: wgt0

    CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Check the number of species
!     if((nch-1).gt.logg)return
! for radmkpp there is co2 and ch4 in the variable list
!
     if(p_co2.gt.1)then
     if (nch.eq.p_co2)then
       chem=370.
       return
     endif
     if (nch.eq.p_ch4)then
       chem=1.7
       return
     endif
     endif

!     if( nch .GT. numgas) then
!       message = ' Input_chem_profile: wrong number of chemical species'
!       return
!       CALL WRF_ERROR_FATAL ( message )
!     endif

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill 1D chemical profile array cprof
      ! Convert species 28-34 (lo-6:lo) from (molecules/cm3) to (mol/mol)
      irefcur = iref(nch-1)
      DO k = 1,kx 
        zprof(k) = 0.5*(zfa_bdy(k)+zfa_bdy(k+1))
        if (irefcur .lt. lo-6) then
          cprof(k) = xl(irefcur,kx+1-k)
        else
          cprof(k) = xl(irefcur,kx+1-k)/dens(kx+1-k)
        end if
      ENDDO

      ! Interpolate temp 3D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor = cprof(1) 
      ELSE IF (z .GT. zprof(kx)) THEN
        stor = cprof(kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor = cprof(k)
            EXIT input_loop
          ELSE IF (z .EQ. zprof(k+1) )THEN
            stor = cprof(k+1)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor = MAX( wgt0 *cprof(k  ) + &
                     (1.-wgt0)*cprof(k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chem = fracref(nch-1)*stor*1.E6

      ! special code for sulfate/h2so4
!      if(nch.eq.p_sulf.and.p_nu0.gt.1)then
!        chem=chem*(1.-so4vaptoaer)
!      endif

      RETURN
  END SUBROUTINE bdy_chem_value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE flow_dep_bdy_smoke (chem,                                       &
                              chem_bxs,chem_btxs,                                  &
                              chem_bxe,chem_btxe,                                  &
                              chem_bys,chem_btys,                                  &
                              chem_bye,chem_btye,                                  &
                              dtbdy,                                              &
                              !have_bcs_chem,                        & 
                              u, v, config_flags, alt, & 
                          !    t,pb,p,  &
                              ic,           &
                              ids,ide, jds,jde, kds,kde,  & ! domain dims
                              ims,ime, jms,jme, kms,kme,  & ! memory dims
                              ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                              its,ite, jts,jte, kts,kte )

! This subroutine is called regardless of have_bcs_chem value in the namelist.

!  This subroutine sets zero gradient conditions for outflow and a set profile value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

!      USE module_cam_mam_initmixrats, only:  bdy_chem_value_cam_mam
      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: ic
      REAL,         INTENT(IN   )    :: dtbdy

      TYPE( grid_config_rec_type )   :: config_flags

      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL,  DIMENSION( jms:jme , kds:kde , config_flags%spec_bdy_width), INTENT(IN   ) :: chem_bxs, chem_bxe, chem_btxs, chem_btxe
      REAL,  DIMENSION( ims:ime , kds:kde , config_flags%spec_bdy_width), INTENT(IN   ) :: chem_bys, chem_bye, chem_btys, chem_btye
!      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: z
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
  !    real, INTENT (IN) :: rcp,t0,p1000mb

      INTEGER    :: i, j, k, numgas
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      integer    :: itestbc, i_bdy_method, spec_zone, spec_bdy_width
!      real tempfac,convfac
      real       :: chem_bv_def
      logical    :: have_bcs_chem
      INTEGER, SAVE :: icall
  
      chem_bv_def = conmin
      numgas = 0    !get_last_gas(config_flags%chem_opt)
      itestbc=0
      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1
      
      have_bcs_chem = config_flags%have_bcs_chem   
      spec_bdy_width= config_flags%spec_bdy_width
      spec_zone     = config_flags%spec_zone

      i_bdy_method = 0
      if (have_bcs_chem) i_bdy_method =6
      if (ic .lt. param_first_scalar) i_bdy_method = 0

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary, first boundary in south-north direction, j index is for SN direction, i index is for WE direction
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
                 i_inner = max(i,ibs+spec_zone)
                 i_inner = min(i_inner,ibe-spec_zone)
                 IF(v(i,k,j) .lt. 0.)THEN
                    chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)         ! air parcel leaving the domain
                 ELSE
! RAR:
                 if (i_bdy_method .eq. 6) then      ! have_bcs_chem=.true.
                    CALL bdy_chem_value_rapsmoke( chem(i,k,j),chem_bys(i,k,1),chem_btys(i,k,1),alt(i,k,j),dtbdy )       ! air parcel entering the domain
                 else
                    chem(i,k,j) = chem_bv_def
                 endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
!                if (i_bdy_method .eq. 1) then
!                   CALL bdy_chem_value (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 9) then
!                   CALL bdy_chem_value_racm (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
!                else if (i_bdy_method .eq. 2) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_sorgam (   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
!                        alt(i,k,j),convfac,g)
!                else if (i_bdy_method .eq. 3) then
!                   CALL bdy_chem_value_cbmz (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 4) then
!                   CALL bdy_chem_value_mosaic (   &
!                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
!                else if (i_bdy_method .eq. 5) then
!                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )

! RAR:
                if (i_bdy_method .eq. 6) then      ! have_bcs_chem=.true.
                   CALL bdy_chem_value_rapsmoke ( chem(i,k,j),chem_bye(i,k,1),chem_btye(i,k,1),alt(i,k,j),dtbdy)

!                else if (i_bdy_method .eq. 7) then
!                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
!                else if (i_bdy_method .eq. 8) then
!                   chem(i,k,j) = 0.
!                else if (i_bdy_method .eq. 16) then
!                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic )  ! For GHGs
!                else if (i_bdy_method .eq. 15) then
!                   CALL bdy_chem_value_cb05 (   &
!                        2, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 17) then
!                   CALL bdy_chem_value_cb05_vbs (   &
!                        2, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 501) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_cam_mam(   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                else
                   chem(i,k,j) = chem_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
!                if (i_bdy_method .eq. 1) then
!                   CALL bdy_chem_value (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 9) then
!                   CALL bdy_chem_value_racm (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
!                else if (i_bdy_method .eq. 2) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_sorgam (   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
!                        alt(i,k,j),convfac,g)
!                else if (i_bdy_method .eq. 3) then
!                   CALL bdy_chem_value_cbmz (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 4) then
!                   CALL bdy_chem_value_mosaic (   &
!                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
!                else if (i_bdy_method .eq. 5) then
!                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )

!RAR: west boundary in WE direction
                if (i_bdy_method .eq. 6) then      ! have_bcs_chem=.true.
                   CALL bdy_chem_value_rapsmoke (chem(i,k,j),chem_bxs(j,k,1),chem_btxs(j,k,1),alt(i,k,j),dtbdy)

!               else if (i_bdy_method .eq. 7) then
!                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
!                else if (i_bdy_method .eq. 8) then
!                   chem(i,k,j) = 0.
!                else if (i_bdy_method .eq. 16) then
!                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic )  ! For GHGs
!                else if (i_bdy_method .eq. 15) then
!                   CALL bdy_chem_value_cb05 (   &
!                        3, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 17) then
!                   CALL bdy_chem_value_cb05_vbs (   &
!                        3, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 501) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_cam_mam(   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                 else
                    chem(i,k,j) = chem_bv_def
                 endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 

      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
!                if (i_bdy_method .eq. 1) then
!                   CALL bdy_chem_value (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 9) then
!                   CALL bdy_chem_value_racm (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas,p_co2 )
!                else if (i_bdy_method .eq. 2) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_sorgam (   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags,   &
!                        alt(i,k,j),convfac,g)
!                else if (i_bdy_method .eq. 3) then
!                   CALL bdy_chem_value_cbmz (   &
!                        chem(i,k,j), z(i,k,j), ic, numgas )
!                else if (i_bdy_method .eq. 4) then
!                   CALL bdy_chem_value_mosaic (   &
!                        chem(i,k,j), alt(i,k,j), z(i,k,j), ic, config_flags )
!                else if (i_bdy_method .eq. 5) then
!                   CALL bdy_chem_value_tracer ( chem(i,k,j), ic )

! RAR:
                if (i_bdy_method .eq. 6) then      ! have_bcs_chem=.true.
                   CALL bdy_chem_value_rapsmoke ( chem(i,k,j),chem_bxe(j,k,1),chem_btxe(j,k,1),alt(i,k,j),dtbdy)

!                else if (i_bdy_method .eq. 7) then
!                   CALL bdy_chem_value_gocart ( chem(i,k,j), ic )
!                else if (i_bdy_method .eq. 8) then
!                   chem(i,k,j) = 0.
!                else if (i_bdy_method .eq. 16) then
!                   CALL bdy_chem_value_ghg ( chem(i,k,j), ic ) ! For GHGs
!                else if (i_bdy_method .eq. 15) then
!                   CALL bdy_chem_value_cb05 (   &
!                        4, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 17) then
!                   CALL bdy_chem_value_cb05_vbs (   &
!                        4, chem(i,k,j), k, ic, config_flags, numgas )
!                else if (i_bdy_method .eq. 501) then
!                   tempfac=(t(i,k,j)+t0)*((p(i,k,j) + pb(i,k,j))/p1000mb)**rcp
!                   convfac=(p(i,k,j)+pb(i,k,j))/rgasuniv/tempfac
!                   CALL bdy_chem_value_cam_mam(   &
!                        chem(i,k,j), z(i,k,j), ic, config_flags, alt(i,k,j), convfac, g )
                 else
                   chem(i,k,j) = chem_bv_def
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF

      IF (config_flags%debug_chem .AND. icall<2000) THEN
         WRITE(*,*) 'flow_dep_bdy_smoke: dtbdy=dt_rk+dtbc ',dtbdy
         WRITE(*,*) 'flow_dep_bdy_smoke: its,ite,jts,jte,kts,kte ',its,ite,jts,jte,kts,kte   
         WRITE(*,*) 'flow_dep_bdy_smoke: alt(its,kts,jts),alt(ite,kte,jte) ',alt(its,kts,jts),alt(ite,kte,jte) 
         WRITE(*,*) 'flow_dep_bdy_smoke: chem(its,kts,jts),chem_bxe(jts,kts,1),chem_btxe(jts,kts,1) ',chem(its,kts,jts),chem_bxe(jts,kts,1),chem_btxe(jts,kts,1)
         icall=icall+1
      END IF 

   END SUBROUTINE flow_dep_bdy_smoke
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE bdy_chem_value_rapsmoke ( chem, chem_b, chem_bt, irho, dt)
                                  
    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: chem_b
    REAL,    intent(IN)   :: chem_bt, irho  ! inverse density
    REAL,    intent(IN)   :: dt
    CHARACTER (LEN=80) :: message

! This field is in ug/m3 in the met_em files generated from RAP output, therefore we divide it here by air den 

      chem=max(epsilc,chem_b + chem_bt * dt) * irho  

      RETURN
  END SUBROUTINE bdy_chem_value_rapsmoke
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE cv_mmdd_jday ( YY, MM, DD, JDAY)
!
!     Subroutine to compute the julian day given the month and day
!
!
      INTEGER,      INTENT(IN )    :: YY, MM, DD
      INTEGER,      INTENT(OUT)    :: JDAY

      INTEGER, DIMENSION(12) :: imon, imon_a
      INTEGER                :: i

      DATA imon_a /0,31,59,90,120,151,181,212,243,273,304,334/
!
!..... Check for leap year.
!
      do i=1,12
         imon(i) = imon_a(i)
      enddo 
      if(YY .eq. (YY/4)*4) then
         do i=3,12
            imon(i) = imon(i) + 1
         enddo 
      endif
!
!..... Convert month, day to julian day.
!
      jday = imon(mm) + dd


   END SUBROUTINE cv_mmdd_jday

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   integer FUNCTION get_last_gas(chem_opt)
     implicit none
     integer, intent(in) :: chem_opt

     ! Determine the index of the last gas species, which depends
     ! upon the gas mechanism.

     select case (chem_opt)
     case (0)
        get_last_gas = 0

!!! TUCCELLA
     case (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
           RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP,       &
           RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACMSORG_KPP, RACM_SOA_VBS_KPP,& 
           RACM_SOA_VBS_AQCHEM_KPP,GOCARTRACM_KPP,GOCARTRADM2)
        get_last_gas = p_ho2

!     case (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
!          SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP )!BSINGH(12/13/2013): Added SAPRC 8 bin AQ case and non-aq on 04/03/2014
!        get_last_gas = p_ch4


!     case (CBMZ,CBMZ_MOSAIC_DMS_4BIN,CBMZ_MOSAIC_DMS_8BIN,CBMZ_MOSAIC_DMS_4BIN_AQ,CBMZ_MOSAIC_DMS_8BIN_AQ)
!        get_last_gas = p_mtf

!     case (CBMZ_BB,CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_4BIN, &
!           CBMZ_MOSAIC_8BIN,CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ,CBMZSORG,CBMZSORG_AQ)
!        get_last_gas = p_isopo2

     case (CHEM_TRACER)
        get_last_gas = p_co

     case (CHEM_TRACE2)
        get_last_gas = p_tracer_1

     case (GOCART_SIMPLE)
        get_last_gas = p_msa

     case (CBM4_KPP)
        get_last_gas = p_ho2

     case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
        get_last_gas = p_nh3

     case (CHEM_VASH)
        get_last_gas = 0
     case (CHEM_VOLC)
        get_last_gas = p_sulf
     case (CHEM_VOLC_4BIN)
        get_last_gas = 0
     case (DUST)
        get_last_gas = 0
     case (MOZART_KPP)
        get_last_gas = p_meko2

!     case (CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
!        GET_LAST_GAS = p_ic3h7no3

!     case (MOZCART_KPP)
!        get_last_gas = p_meko2

!     case (MOZART_MOSAIC_4BIN_KPP)
!        get_last_gas = p_meko2

!     case (MOZART_MOSAIC_4BIN_AQ_KPP)
!        get_last_gas = p_meko2

     case (CO2_TRACER,GHG_TRACER) ! No gas chemistry or deposition for GHGs
        get_last_gas = 0
     
     case (CHEM_SMOKE)
        get_last_gas = 0

     case ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
        get_last_gas = p_soag
     case default
        call wrf_error_fatal("get_last_gas: could not decipher chem_opt value")

     end select

   END FUNCTION get_last_gas
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE sorgam_set_aer_bc_pnnl( chem, z, nch, config_flags )
   !   USE module_data_sorgam, ONLY : dginia, dginin, dginic, esn36, esc36, esa36, seasfac, no3fac, nh4fac, so4fac, soilfac, anthfac, orgfac

      implicit none

      INTEGER,INTENT(IN   ) :: nch
      real,intent(in      ) :: z
      REAL,INTENT(INOUT   ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags

      REAL :: mult,                       &
              m3acc, m3cor, m3nuc,        &
              bv_so4ai, bv_so4aj,         &
              bv_nh4ai, bv_nh4aj,         &
              bv_no3ai, bv_no3aj,         &
              bv_eci,   bv_ecj,           &
              bv_p25i,  bv_p25j,          &
              bv_orgpai,bv_orgpaj,        &
              bv_antha, bv_seas, bv_soila

!
! Determine height multiplier...
! This should mimic the calculation in sorgam_init_aer_ic_pnnl,
! mosaic_init_wrf_mixrats_opt2, and bdy_chem_value_mosaic
!!$!    Height(m)     Multiplier
!!$!    ---------     ----------
!!$!    <=2000        1.0
!!$!    2000<z<3000   linear transition zone to 0.5
!!$!    3000<z<5000   linear transision zone to 0.25
!!$!    >=5000        0.25
!!$!
!!$! which translates to:
!!$!    2000<z<3000   mult = 1.0 + (z-2000.)*(0.5-1.0)/(3000.-2000.)
!!$!    3000<z<5000   mult = 0.5 + (z-3000.)*(0.25-0.5)/(5000.-3000.)
!!$! or in reduced form:
!!$      if( z <= 2000. ) then
!!$         mult = 1.0
!!$      elseif( z > 2000. &
!!$           .and. z <= 3000. ) then
!!$         mult = 1.0 - 0.0005*(z-2000.)
!!$      elseif( z > 3000. &
!!$           .and. z <= 5000. ) then
!!$         mult = 0.5 - 1.25e-4*(z-3000.)
!!$      else
!!$         mult = 0.25
!!$      end if
! Updated aerosol profile multiplier 1-Apr-2005:
!    Height(m)     Multiplier
!    ---------     ----------
!    <=2000        1.0
!    2000<z<3000   linear transition zone to 0.25
!    3000<z<5000   linear transision zone to 0.125
!    >=5000        0.125
!
! which translates to:
!    2000<z<3000   mult = 1.00 + (z-2000.)*(0.25-1.0)/(3000.-2000.)
!    3000<z<5000   mult = 0.25 + (z-3000.)*(0.125-0.25)/(5000.-3000.)
! or in reduced form:
!       if( z <= 2000. ) then
!          mult = 1.0
!       elseif( z > 2000. &
!            .and. z <= 3000. ) then
!          mult = 1.0 - 0.00075*(z-2000.)
!       elseif( z > 3000. &
!            .and. z <= 5000. ) then
!          mult = 0.25 - 4.166666667e-5*(z-3000.)
!       else
!          mult = 0.125
!       end if
   !     if( z <= 500. ) then
   !        mult = 1.0
   !     elseif( z > 500. &
   !          .and. z <= 1000. ) then
   !        mult = 1.0 - 0.001074*(z-500.)
   !     elseif( z > 1000. &
   !          .and. z <= 5000. ) then
   !        mult = 0.463 - 0.000111*(z-1000.)
   !     else
   !        mult = 0.019
   !     end if

! These should match what is in sorgam_init_aer_ic_pnnl.
! Boundary values as of 2-Dec-2004:
!     bv_so4aj  = mult*2.375
!     bv_so4ai  = mult*0.179
!     bv_nh4aj  = mult*0.9604
!     bv_nh4ai  = mult*0.0196
!     bv_no3aj  = mult*0.0650
!     bv_no3ai  = mult*0.0050
!     bv_ecj    = mult*0.1630
!     bv_eci    = mult*0.0120
!     bv_p25j   = mult*0.6350
!     bv_p25i   = mult*0.0490
!     bv_orgpaj = mult*0.9300
!     bv_orgpai = mult*0.0700
!     bv_antha  = mult*2.2970
!     bv_seas   = mult*0.2290
!     bv_soila  = conmin

!#if (CASENAME == 4)
!        if( z <= 2000. ) then
!           mult = 1.0
!        elseif( z > 2000. &
!             .and. z <= 3000. ) then
!           mult = 1.0 - 0.00075*(z-2000.)
!        elseif( z > 3000. &
!             .and. z <= 5000. ) then
!           mult = 0.25 - 4.166666667e-5*(z-3000.)
!        else
!           mult = 0.125
!        end if
!      bv_so4aj = mult*(0.0004810001+0.7271175)*0.97
!      bv_so4ai = mult*(0.0004810001+0.7271175)*0.03
!      bv_nh4aj = mult*0.2133708*0.97
!      bv_nh4ai = mult*0.2133708*0.03
!      bv_no3aj = mult*0.01399485*0.97
!      bv_no3ai = mult*0.01399485*0.03
!      bv_ecj = mult*0.04612048*0.97
!      bv_eci = mult*0.04612048*0.03
!      bv_p25j = mult*1.890001e-05*0.97
!      bv_p25i = mult*1.890001e-05*0.03
!      bv_antha = conmin
!      bv_orgpaj = mult*0.5844942*0.97
!      bv_orgpai = mult*0.5844942*0.03
!      bv_seas = conmin
!      bv_soila = conmin

!#endif

! m3... calculations should match the very end of module_aerosols_sorgam.F
!... i-mode (note that the 8 SOA species have bv=conmin)
!      m3nuc = so4fac*bv_so4ai + nh4fac*bv_nh4ai + &
!        no3fac*bv_no3ai + &
!        orgfac*8.0*conmin + orgfac*bv_orgpai + &
!        anthfac*bv_p25i + anthfac*bv_eci

!... j-mode (note that the 8 SOA species have bv=conmin)
!      m3acc = so4fac*bv_so4aj + nh4fac*bv_nh4aj + &
!        no3fac*bv_no3aj + &
!        orgfac*8.0*conmin + orgfac*bv_orgpaj + &
!        anthfac*bv_p25j + anthfac*bv_ecj

!...c-mode
!      m3cor = soilfac*bv_soila + seasfac*bv_seas + &
!        anthfac*bv_antha

! Cannot set_sulf here because it is a "radm2" species whose bc value
! is set via bdy_chem_value. Instead, xl(iref(p_sulf-1),:) is set to
! the value conmin in subroutine gasprofile_init_pnnl
!      if( nch == p_sulf    ) chem = conmin !as per rce's 0 recommendation

    END SUBROUTINE sorgam_set_aer_bc_pnnl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE sorgam_vbs_set_aer_bc_pnnl( chem, z, nch, config_flags )
     ! USE module_data_sorgam_vbs, ONLY : dginia, dginin, dginic, esn36, esc36, esa36, seasfac, no3fac, nh4fac, so4fac, soilfac, anthfac, orgfac

      implicit none

      INTEGER,INTENT(IN   ) :: nch
      real,intent(in      ) :: z
      REAL,INTENT(INOUT   ) :: chem
      TYPE (grid_config_rec_type) , INTENT (in) :: config_flags

      REAL :: mult,                       &
              m3acc, m3cor, m3nuc,        &
              bv_so4ai, bv_so4aj,         &
              bv_nh4ai, bv_nh4aj,         &
              bv_no3ai, bv_no3aj,         &
              bv_eci,   bv_ecj,           &
              bv_p25i,  bv_p25j,          &
              bv_orgpai,bv_orgpaj,        &
              bv_antha, bv_seas, bv_soila
    END SUBROUTINE sorgam_vbs_set_aer_bc_pnnl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
END MODULE module_input_smoke_data
