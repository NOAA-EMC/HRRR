SUBROUTINE radar_ref2tten(nlon,nlat,nsig,ref_mos_3d,& 
                         p_bk,t_bk,ges_tten,dfi_rlhtp,krad_bot_in,pblh,&
                         l_tten_for_convection_only,convection_refl_threshold)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  radar_ref2tten      convert radar observation to temperature tedency
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-11-27
!
! ABSTRACT: 
!  This subroutine converts radar reflectivity (dBZ) to temperature tendency for DFI 
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!
!
!   input argument list:
!     mype         - processor ID
!     istat_radar  - radar data status: 0=no radar data; 1=use radar reflectivity
!     nlon         - no. of lons on subdomain (buffer points on ends)
!     nlat         - no. of lats on subdomain (buffer points on ends)
!     nsig         - no. of levels
!     ref_mos_3d   - 3D radar reflectivity  (dBZ)
!     cld_cover_3d - 3D cloud cover     (0-1)
!     p_bk         - 3D background pressure  (hPa)
!     t_bk         - 3D background potential temperature (K)
!     ges_tten     - 3D radar temperature tendency 
!     dfi_rlhtp    - dfi radar latent heat time period
!     krad_bot_in  - radar bottome height
!     pblh         - PBL height in grid unit
!     l_tten_for_convection_only - .true. = turn off radar-based tten for nonconvective precipitation
!
!   output argument list:
!     ges_tten     - 3D radar temperature tendency 
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!
  use constants, only: rd_over_cp, h1000
  use kinds, only: r_kind,i_kind,r_single
  implicit none

  INTEGER(i_kind),INTENT(IN) ::  nlon,nlat,nsig
  real(r_kind),INTENT(IN)    ::  dfi_rlhtp
  real(r_single),INTENT(IN)  ::  krad_bot_in
  real(r_single),INTENT(IN)  ::  pblh(nlon,nlat)

  real(r_single),INTENT(IN)    :: ref_mos_3d(nlon,nlat,nsig) ! reflectivity in grid
  real(r_single),INTENT(IN)  :: p_bk(nlon,nlat,nsig)
  real(r_single),INTENT(IN)  :: t_bk(nlon,nlat,nsig)
  real(r_single), INTENT(INOUT):: ges_tten(nlon,nlat,nsig)
  logical,INTENT(IN)         :: l_tten_for_convection_only
  REAL(r_kind),INTENT(IN) :: convection_refl_threshold     ! units dBZ

  real (r_single) :: tbk_k

  real(r_kind), allocatable :: tten_radar(:,:,:)         ! 
  real(r_kind), allocatable :: dummy(:,:)         ! 

  integer(i_kind) :: krad_bot             ! RUC bottom level for TTEN_RAD
                                          !  and for filling from above
!
!  convection suppression
!
  real(r_kind), allocatable :: radyn(:,:) 
  real(r_kind)    ::   radmax, dpint
  integer(i_kind) :: nrad
  real(r_kind)    ::   radmaxall, dpintmax

!
! DCD 2/22/13
! parameters for determining probable deep, moist convection
!
!  REAL(r_kind) :: convection_refl_threshold     ! units dBZ
!  PARAMETER(convection_refl_threshold=28.0_r_kind)
  LOGICAL, allocatable :: probable_convection(:,:)

! adopted from: METCON of RUC  (/ihome/rucdev/code/13km/hybfront_code)
! CONTAINS ATMOSPHERIC/METEOROLOGICAL/PHYSICAL CONSTANTS
!**  R_P               R  J/(MOL*K)    UNIVERSAL GAS CONSTANT
!**                                      R* = 8.31451
!**  MD_P              R  KG/MOL       MEAN MOLECULAR WEIGHT OF DRY AIR
!**                                      MD = 0.0289645
!jmb--Old value                          MD = 0.0289644
!**  RD_P              R  J/(KG*K)     SPECIFIC GAS CONSTANT FOR DRY AIR
!**                                      RD = R*>/<MD = 287.0586
!**  CPD_P             R  J/(KG*K)     SPECIFIC HEAT OF DRY AIR AT
!**                                      CONSTANT PRESSURE = 3.5*RD
!**                                      1004.6855
!**  LV_P              R  J/KG         LATENT HEAT OF VAPORIZATION
!**                                      AT 0 DEGREES C
!jmb                                     LV = 2.501E6 (SEVERAL SOURCES)
!jmb
!jmb LF0_P                J/KG         LATENT HEAT OF FUSION
!jmb                                     AT 0 DEGREES C
!jmb                                     LF0= .3335E6 [WEXLER (1977)]
!jmb
!**  CPOVR_P           R  ND           CPD/RD = 3.5

  REAL(r_kind)::     R_P
  PARAMETER ( R_P     =  8.31451_r_kind     )

  REAL(r_kind)::        MD_P
  REAL(r_kind)::        RD_P
  REAL(r_kind)::        CPD_P
  REAL(r_kind)::        CPOVR_P
  REAL(r_kind)::        LV_P
  REAL(r_kind)::        LF0_P
  PARAMETER ( MD_P     =  0.0289645_r_kind          )
  PARAMETER ( RD_P     =  R_P/MD_P           )
  PARAMETER ( CPD_P    =  3.5_r_kind* RD_P          )
  PARAMETER ( CPOVR_P  =  CPD_P/RD_P         )
  PARAMETER ( LV_P      = 2.501E6_r_kind           )
  PARAMETER ( LF0_P     = .3335E6_r_kind           )
!
  INTEGER(i_kind):: i,j, k, iskip
  REAL(r_kind):: tten, addsnow
  REAL(r_kind):: spval_p

!
  spval_p =99999.0_r_kind
  if( 1 == 1 ) then

! DCD 2/22/13
! For now, simply use a reflectivity threshold for determining when precipitation is
! probably associated with deep, moist convection.  A depth constraint is added for
! cold layers so that bright bands associated with melting snow aren't necessarily
! considered to be convective.
    write(6,*) 'l_tten_for_convection_only = ', l_tten_for_convection_only
    if (l_tten_for_convection_only) then
!      if (mype==0) write(6,*) 'determining locations of probable deep, moist convection'
      allocate(probable_convection(nlon,nlat))
      probable_convection(:,:) = .false.
      DO j=2,nlat-1
        DO i=2,nlon-1
          dpint = 0.0_r_kind
          DO k=2,nsig-1
            tbk_k=t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp       ! temperature (K)
            if (tbk_k>=277.15_r_kind .and. ref_mos_3d(i,j,k)>=convection_refl_threshold) then
              probable_convection(i,j) = .true.
            endif
            if (tbk_k<277.15_r_kind .and. ref_mos_3d(i,j,k)>=convection_refl_threshold) then
              dpint = dpint + 0.5_r_kind*(p_bk(i,j,k-1)-p_bk(i,j,k+1))
            endif
          ENDDO
          if (dpint>=200.0_r_kind) probable_convection(i,j) = .true.
        ENDDO
      ENDDO
    endif

    allocate(tten_radar(nlon,nlat,nsig))   
    allocate(dummy(nlon,2))   
    tten_radar=0
!
!-------Calculate tten_radar for LH specification in later model-DFI ------
!   NOTE: tten_radar should be temperature tendency (K/s).
!       This tendency is calculated using a "storm-lifetime" time period
!        over which condensate (from obs radar reflectivity)
!        was assumed to be formed, and therefore,
!        over which the corresponding latent heat was assumed to be released.
!         integration step instead of each second
!    So (60*cpd_p), where 60 is 60 steps for 40 min foreward integration with 40 second time step.
!
    DO k=2,nsig-1
      DO j=2,nlat-1
        DO i=2,nlon-1
          krad_bot= int( max(krad_bot_in,pblh(i,j)) + 0.5_r_single )  ! consider PBL height
          tbk_k=t_bk(i,j,k)*(p_bk(i,j,k)/h1000)**rd_over_cp  ! convert to temperature(K) 
          if (ref_mos_3d(i,j,k)<0.001_r_kind.and.ref_mos_3d(i,j,k)>-100) then     ! no echo
             tten_radar(i,j,k) = 0._r_kind
          else if (ref_mos_3d(i,j,k)>=0.001_r_kind) then  ! echo
            iskip=0
            if (tbk_k>277.15_r_kind .and. ref_mos_3d(i,j,k)<28._r_kind) then
            iskip=iskip+1
!           write (6,*)' t is over 277 ',i,j,k,ref_mos_3d(i,j,k)
!        ALSO, if T > 4C and refl < 28dBZ, again
!            tten_radar = 0.
            endif
            if(iskip == 0 ) then
!        tten_radar set as non-zero ONLY IF
!          - not contradicted by GOES clear, and
!          - ruc_refl > 28 dbZ for temp > 4K, and
!          - for temp < 4K, any ruc_refl dbZ is OK.
!          - cloudy and under GOES cloud top
              if (k>=krad_bot) then
! can not use cld_cover_3d because we don't use reflectivity to build cld_cover_3d
!                 if (abs(cld_cover_3d(i,j,k))<=0.5_r_kind) then
!                   addsnow=0.0_r_kind
!                 else
                   addsnow = 10**(ref_mos_3d(i,j,k)/17.8_r_kind)/264083._r_kind*1.5_r_kind
!                 endif
                 tten = ((1000.0_r_kind/p_bk(i,j,k))**(1./cpovr_p))    &
                     *(((LV_P+LF0_P)*addsnow)/                &
                      (dfi_rlhtp*60.0_r_kind*CPD_P))
!      60 = sec/min, and dfi_rlhtp is in minutes.
!      NOTE:  tten is in K/seconds
                 tten_radar(i,j,k)= min(0.01_r_kind,max(-0.01_r_kind,tten))
              end if
            end if
          end if  ! ref_mos_3d

        ENDDO
      ENDDO
    ENDDO

    DO k=1,nsig
      call smooth(tten_radar(1,1,k),dummy,nlon,nlat,0.5_r_kind)
      call smooth(tten_radar(1,1,k),dummy,nlon,nlat,0.5_r_kind)
    ENDDO

!  KEY element -- Set tten_radar to no-coverage AFTER smoothing
!      where ref_mos_3d had been previously set to no-coverage (-99.0 dbZ)

    DO k=1,nsig
      DO j=1,nlat
        DO i=1,nlon
           ges_tten(i,j,k)=tten_radar(i,j,k)
           if(ref_mos_3d(i,j,k)<=-200.0_r_kind ) ges_tten(i,j,k)=-spval_p   ! no obs
        ENDDO
      ENDDO
    ENDDO

! -- Whack (smooth) the tten_radar array some more.
!     for convection suppression in the radyn array.
    DO k=1,nsig
      call smooth(tten_radar(1,1,k),dummy,nlon,nlat,0.5_r_kind)
      call smooth(tten_radar(1,1,k),dummy,nlon,nlat,0.5_r_kind)
      call smooth(tten_radar(1,1,k),dummy,nlon,nlat,0.5_r_kind)
    ENDDO

    deallocate(dummy)   

!  RADYN array = convection suppression array
!  Definition of RADYN values
!     -10 -> no information 
!       0 -> no convection
!       1 -> there might be convection nearby
!  NOTE:  0,1 values are only possible if
!     deep radar coverage is available (i.e., > 300 hPa deep)

!  RADYN is read into RUC model as array PCPPREV,
!   where it is used to set the cap_depth (cap_max)
!   in the Grell-Devenyi convective scheme
!   to a near-zero value, effectively suppressing convection
!   during DFI and first 30 min of the forward integration.

    allocate(radyn(nlon,nlat))
    radyn = -10.

    radmaxall=-999
    dpintmax=-999
    DO j=1,nlat
      DO i=1,nlon

        nrad = 0
        radmax = 0._r_kind
        dpint = 0._r_kind
        DO k=2,nsig-1
          if ((ref_mos_3d(i,j,k))<=-200.0_r_kind) tten_radar(i,j,k) = -spval_p
          if (tten_radar(i,j,k)>-15._r_kind) then
            nrad=nrad+1
            dpint = dpint + 0.5_r_kind*(p_bk(i,j,k-1)-p_bk(i,j,k+1))
            radmax = max(radmax,tten_radar(i,j,k))
          end if
        ENDDO
        if (dpint>=300._r_kind ) then 
          radyn(i,j) = 0._r_kind
          if (radmax>0.00002_r_kind) radyn(i,j) = 1._r_kind
          if( abs(radyn(i,j)) < 0.00001_r_kind ) then
             krad_bot= int( max(krad_bot_in,pblh(i,j)) + 0.5_r_single )  ! consider PBL height
             do k=krad_bot,nsig-1
                 ges_tten(i,j,k) = 0._r_kind
             end do
          endif
        endif

!  2. Extend depth of no-echo zone from dpint zone down to PBL top, 
!   similarly to how lowest echo (with convection) is extended down to PBL top
!    5/27/2010 - Stan B.
!        if (dpint.ge.300. .and. radmax.le.0.00001) then
!           krad_bot= int( max(krad_bot_in,pblh(i,j)) + 0.5_r_single )  ! consider PBL height
!           do k=krad_bot,nsig-1
!               ges_tten(i,j,k) = 0.
!           end do
!        end if

        if(dpintmax < dpint ) dpintmax=dpint
        if(radmaxall< radmax) radmaxall=radmax
      ENDDO
    ENDDO

    DO j=1,nlat
      DO i=1,nlon
        ges_tten(i,j,nsig)=radyn(i,j)
      ENDDO
    ENDDO

! DCD 2/22/13
! Associate regions of non-convective precipitation with "no coverage" so that
! the temperature tendency comes from the model's microphysics scheme rather
! than being specified.
    if (l_tten_for_convection_only) then
      write(6,*) 'changing regions of non-convective precipitation to no coverage'
      DO j=2,nlat-1
        DO i=2,nlon-1
          if (radyn(i,j)>0.9_r_kind .and. .not. probable_convection(i,j)) then
            ges_tten(i,j,nsig)=-10.0_r_kind
            DO k=2,nsig-1
              ges_tten(i,j,k)=-spval_p
            ENDDO
          endif
        ENDDO
      ENDDO
      deallocate(probable_convection)
    endif

    deallocate(tten_radar)   
    deallocate(radyn)   

  else

     ges_tten=-spval_p
     ges_tten(:,:,nsig)=-10.0_r_kind

  endif

  DO k=1,nsig
    DO j=1,nlat
      DO i=1,nlon
         if(ges_tten(i,j,k) <= -200.0_r_kind ) ges_tten(i,j,k)=-20.0_r_kind   ! no obs
      ENDDO                  
    ENDDO
  ENDDO

END SUBROUTINE radar_ref2tten
