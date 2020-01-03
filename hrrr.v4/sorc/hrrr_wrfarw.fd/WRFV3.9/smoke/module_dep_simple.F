    MODULE module_dep_simple

      IMPLICIT NONE

      INTEGER, PARAMETER :: dep_seasons = 5
      INTEGER, PARAMETER :: nlu = 25
      REAL, parameter    :: small_value = 1.e-36
      REAL, parameter    :: large_value = 1.e36

!--------------------------------------------------
! following currently hardwired to USGS
!--------------------------------------------------
      integer, parameter :: isice_temp   = 24
      integer, parameter :: iswater_temp = 16
      integer, parameter :: wrf2mz_lt_map(nlu) = (/ 1, 2, 2, 2, 2, &
                                                    4, 3, 3, 3, 3, &
                                                    4, 5, 4, 5, 6, &
                                                    7, 9, 6, 8, 9, &
                                                    6, 6, 8, 0, 0 /)
      real, parameter    :: wh2o = 18.0153
      real, parameter    :: wpan = 121.04793
      character(len=4), parameter :: mminlu = 'USGS'

      INTEGER :: month = 0
      INTEGER :: ixxxlu(nlu)
!     include modis landuse 
      INTEGER, allocatable :: luse2usgs(:)
      REAL    :: kpart(nlu)
      REAL    :: rac(nlu,dep_seasons), rclo(nlu,dep_seasons), rcls(nlu,dep_seasons)
      REAL    :: rgso(nlu,dep_seasons), rgss(nlu,dep_seasons)
      REAL    :: ri(nlu,dep_seasons), rlu(nlu,dep_seasons)
      REAL    :: ri_pan(5,11)
      real    :: c0_pan(11) = (/ 0.000, 0.006, 0.002, 0.009, 0.015, &
                                 0.006, 0.000, 0.000, 0.000, 0.002, 0.002 /)
      real    :: k_pan (11) = (/ 0.000, 0.010, 0.005, 0.004, 0.003, &
                                 0.005, 0.000, 0.000, 0.000, 0.075, 0.002 /)
!--------------------------------------------------
! NO MORE THAN 1000 SPECIES FOR DEPOSITION
!--------------------------------------------------
      REAL    :: dratio(1000), hstar(1000), hstar4(1000)
      REAL    :: f0(1000), dhr(1000), scpr23(1000)

!      type wesely_pft
!        integer          :: npft
!        integer          :: months
!        INTEGER, pointer :: seasonal_wes(:,:,:,:)
!        logical          :: is_allocated
!      end type wesely_pft

!      type(wesely_pft), allocatable :: seasonal_pft(:)

!--------------------------------------------------
! .. Default Accessibility ..
!--------------------------------------------------
    PUBLIC

    logical, allocatable :: is_aerosol(:) ! true if field is aerosol (any phase)

    CONTAINS

SUBROUTINE wesely_driver( id, config_flags, current_month,  &
                          julday, rh,moist, p8w, t8w, raincv,     &
                          ddvel, aer_res_def, &
                          ivgtyp, tsk, gsw, vegfra, pbl,    &
                          rmol, ust, znt,                                 &
                          z, z_at_w, snowh, numgas,                       &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          its,ite, jts,jte, kts,kte                       )
!--------------------------------------------------
!  Wesely dry deposition driver
!--------------------------------------------------

  USE module_model_constants 
  USE module_configure
  USE module_state_description                       
!  USE module_data_sorgam
  USE module_state_description, only:  param_first_scalar        
   INTEGER,      INTENT(IN   ) :: id,julday,                              &
                                  numgas, current_month,                  &
                                  ids,ide, jds,jde, kds,kde,              &
                                  ims,ime, jms,jme, kms,kme,              &
                                  its,ite, jts,jte, kts,kte     
!   INTEGER,      INTENT(IN   ) :: ktau
!      REAL,      INTENT(IN   ) :: dtstep,gmt

!--------------------------------------------------
! advected moisture variables
!--------------------------------------------------
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), INTENT(IN ) :: &
                                                      moist  
!--------------------------------------------------
! advected chemical species
!--------------------------------------------------
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), INTENT(INOUT ) :: &
!                                                      chem
!--------------------------------------------------
! deposition velocities
!--------------------------------------------------
   REAL, DIMENSION( its:ite, jts:jte, num_chem ), INTENT(INOUT ) ::      &
                                                      ddvel                     
!--------------------------------------------------
! input from met model
!--------------------------------------------------
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) ::      &
     !                                                 t_phy,              &
     !                                                 p_phy,              &
      !                                                dz8w,               &
                                                      z,                  &
                                                      t8w,                &
                                                      p8w,                &
                                                      z_at_w, rh
                                          !            rho_phy
   INTEGER,DIMENSION (ims:ime , jms:jme ), INTENT(IN   ) ::               &
                                                     ivgtyp
   REAL, DIMENSION (ims:ime , jms:jme ), INTENT(INOUT   ) ::             &
                                                     tsk,                 &
                                                     gsw,                 &
                                                     vegfra,              &
                                                     pbl,                 &
                                                     rmol,                &
                                                     ust,                 &
                                                    ! xlat,                &
                                                    ! xlong,               &
                                                     raincv,              &
                                                     znt

   REAL, DIMENSION (ims:ime,jms:jme), INTENT(INOUT) :: aer_res_def !, aer_res_zcen

!   REAL, intent(inout) ::                            aer_res_def(its:ite,jts:jte)
!   REAL, intent(inout) ::                            aer_res_zcen(its:ite,jts:jte)

   REAL,  optional, INTENT(IN)  ::                   snowh(ims:ime,jms:jme)
   TYPE(grid_config_rec_type),  INTENT(IN) ::        config_flags

!--------------------------------------------------
! .. Local Scalars
!--------------------------------------------------
      REAL    ::  dvpart, pa, rad
      REAL    ::  rhchem, ta, ustar, vegfrac, z1, zntt
      INTEGER :: i, iland, iseason, j, jce, jcs, n, nr, ipr,jpr,nvr
      LOGICAL :: rainflag, vegflag, wetflag
!--------------------------------------------------
! .. Local Arrays
!--------------------------------------------------
      REAL :: p(kts:kte)
      REAL :: srfres(numgas)
      REAL :: ddvel0d(numgas)

!-----------------------------------------------------------
! necessary for aerosols (module dependent)         
!-----------------------------------------------------------
      real :: rcx(numgas)

!-----------------------------------------------------------
! .. Intrinsic Functions
!-----------------------------------------------------------
      INTRINSIC max, min                             

!      dep_vap= config_flags%depo_fact

      CALL wrf_debug(15,'in dry_dep_wesely')

 !     if( config_flags%chem_opt /= MOZART_KPP .and. &
 !         config_flags%chem_opt /= MOZCART_KPP .and. &
 !         config_flags%chem_opt /= MOZART_MOSAIC_4BIN_KPP .and. &
 !         config_flags%chem_opt /= MOZART_MOSAIC_4BIN_AQ_KPP) then
         if( julday < 90 .or. julday > 270 ) then
            iseason = 2
            CALL wrf_debug(15,'setting iseason to 2')
         else
            iseason = 1
         endif
 !     end if


tile_lat_loop : &
      do j = jts,jte 
tile_lon_loop : &
         do i = its,ite 
!            iprt  = 0

            iland = luse2usgs( ivgtyp(i,j) )
            ta    = tsk(i,j)
            rad   = gsw(i,j)
            vegfrac = vegfra(i,j)
!            pa      = .01*p_phy(i,kts,j)
!            clwchem = moist(i,kts,j,p_qc)
            ustar = ust(i,j)
            zntt  = znt(i,j)
            z1    = z_at_w(i,kts+1,j) - z_at_w(i,kts,j)
!-----------------------------------------------------------
!     Set logical default values
!-----------------------------------------------------------
            rainflag = .FALSE.
            wetflag  = .FALSE.
!            highnh3  = .FALSE.
            if(p_qr > 1) then
               if(moist(i,kts,j,p_qr) > 1.e-18 .or. raincv(i,j) > 0.) then
                  rainflag = .true.
               endif
            endif
!            rhchem = MIN( 100.,100. * moist(i,kts,j,p_qv) / &
!                     (3.80*exp(17.27*(t_phy(i,kts,j)-273.)/(t_phy(i,kts,j)-36.))/pa))
!            rhchem = MAX(5.,RHCHEM)
            rhchem= rh(i,kts,j)*100.
            if (rhchem >= 95.) wetflag = .true.

      !      if( p_nh3 > 1 .and. p_so2 > 1 ) then
      !         if( chem(i,kts,j,p_nh3) > 2.*chem(i,kts,j,p_so2) ) then
      !            highnh3 = .true.
      !         endif
      !      endif

!-----------------------------------------------------------
!--- deposition
!-----------------------------------------------------------
!     if(snowc(i,j).gt.0.)iseason=4
            CALL rc( rcx, ta, rad, rhchem, iland, &
                     iseason, numgas, wetflag, rainflag )
!           if( config_flags%chem_opt /= MOZART_KPP .and. &
 !               config_flags%chem_opt /= MOZCART_KPP .and. &
 !               config_flags%chem_opt /= MOZART_MOSAIC_4BIN_KPP .and. &
 !               config_flags%chem_opt /= MOZART_MOSAIC_4BIN_AQ_KPP)  then
              ! srfres(1:numgas-2) = rcx(1:numgas-2)
              ! srfres(numgas-1:numgas) = 0.
                srfres(:)= 0.
 !           else
 !              srfres(1:numgas) = rcx(1:numgas)
 !          end if
            CALL deppart( rmol(i,j), ustar, rhchem, iland, dvpart )
            ddvel0d(1:numgas) = 0.
            aer_res_def(i,j)  = 0.
!            aer_res_zcen(i,j) = 0.

            CALL landusevg( ddvel0d, ustar, rmol(i,j), zntt, z1, dvpart, iland,        &
                            numgas, srfres, aer_res_def(i,j) )

            ddvel(i,j,1:numgas) = ddvel0d(1:numgas)

         end do tile_lon_loop
      end do tile_lat_loop
     
END SUBROUTINE wesely_driver

      SUBROUTINE rc( rcx, t, rad, rh, iland, &
                     iseason, numgas, wetflag, rainflag )
!----------------------------------------------------------------------
!     THIS SUBROUTINE CALCULATES SURFACE RESISTENCES ACCORDING
!     TO THE MODEL OF
!     M. L. WESELY,
!     ATMOSPHERIC ENVIRONMENT 23 (1989), 1293-1304
!     WITH SOME ADDITIONS ACCORDING TO
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY  WINFRIED SEIDL, APRIL 1997
!     MODYFIED BY WINFRIED SEIDL, MARCH 2000
!                    FOR MM5 VERSION 3
!----------------------------------------------------------------------

  USE module_state_description                       

!----------------------------------------------------------------------
!	... dummy arguments
!----------------------------------------------------------------------
        INTEGER, intent(in) :: iland, iseason, numgas
 !       INTEGER, intent(in) :: iprt
!        INTEGER, intent(in) :: chem_opt
        REAL, intent(in)    :: rad, rh
        REAL, intent(in)    :: t                            ! surface temp (K)
!        REAL, intent(in)    :: p_srf                        ! surface pressure (Pa)
!        REAL, intent(in)    :: spec_hum                     ! surface specific humidity (kg/kg)
        real, intent(out)   :: rcx(numgas)
        LOGICAL, intent(in) :: rainflag, wetflag

!----------------------------------------------------------------------
! .. Local Scalars ..
!----------------------------------------------------------------------
        REAL, parameter :: t0    = 298.
        REAL, parameter :: tmelt = 273.16
        INTEGER :: lt, n
        REAL    :: rclx, rdc, resice, rgsx, rluo1, rluo2
        REAL    :: rlux, rmx, rs, rsmx, rdtheta, z, wrk
        REAL    :: qs, es, ws, dewm, dv_pan, drat
        REAL    :: crs, tc
        REAL    :: rs_pan, tc_pan
        LOGICAL :: has_dew
!----------------------------------------------------------------------
! .. Local Arrays ..
!----------------------------------------------------------------------
        REAL :: hstary(numgas)

!----------------------------------------------------------------------
! .. Intrinsic Functions ..
!----------------------------------------------------------------------
        INTRINSIC exp

        rcx(1:numgas) = 1.

        tc = t - 273.15
        rdtheta = 0.

        z = 200./(rad+0.1)

!!!  HARDWIRE VALUES FOR TESTING
!       z=0.4727409
!       tc=22.76083
!       t=tc+273.15
!       rad = 412.8426
!       rainflag=.false.
!       wetflag=.false.

        IF ( tc<=0. .OR. tc>=40. ) THEN
          rs = 9999.
        ELSE
          rs = ri(iland,iseason)*(1+z*z)*(400./(tc*(40.-tc)))
        END IF
        rdc   = 100.*(1. + 1000./(rad + 10.))/(1. + 1000.*rdtheta)
        rluo1 = 1./(1./3000. + 3./rlu(iland,iseason))
        rluo2 = 1./(1./1000. + 3./rlu(iland,iseason))
        resice = 1000.*exp( -(tc + 4.) )
        wrk    = (t0 - t)/(t0*t)

!       DRY SURFACE
!--------------------------------------------------
        rgsx = 1000.
!--------------------------------------------------
!       WET SURFACE
!--------------------------------------------------
        IF ((wetflag) .OR. (rainflag)) THEN
!          IF (highnh3) THEN
!            rgsx = 0.
!          ELSE
            rgsx = 500.
!          END IF
        END IF
!--------------------------------------------------
!       WATER
!--------------------------------------------------
        IF (iland==iswater_temp) THEN
          rgsx = 0.
        END IF
!--------------------------------------------------
!       SNOW
!--------------------------------------------------
        IF( iseason==4 .OR. iland==isice_temp ) THEN
          IF( tc > 2. ) THEN
            rgsx = 0.
          else IF ( tc >= -1. .AND. tc <= 2. ) THEN
            rgsx = 70.*(2. - tc)
          else IF ( tc < -1. ) THEN
            rgsx = 500.
          END IF
        END IF
!--------------------------------------------------
!       TOTAL SURFACE RESISTENCE
!--------------------------------------------------
 !       IF ((iseason/=4) .AND. (ixxxlu(iland)/=1) .AND. (iland/=iswater_temp) .AND. &
 !           (iland/=isice_temp)) THEN
 !         rcx(p_so2) = 1./(1./rsmx+1./rlux+1./(rclx+rdc+rgsx))
 !       ELSE
 !         rcx(p_so2) = rgsx
 !       END IF
 !       rcx(p_so2) = max( 1.,rcx(p_so2) )
 !    end if is_so2
!--------------------------------------------------
!     NH3 according to Erisman et al. 1994
!       R_STOM
!--------------------------------------------------
!is_nh3: if( p_nh3 > 1 ) then
!        rsmx = rs*dratio(p_nh3)
!--------------------------------------------------
!       GRASSLAND (PASTURE DURING GRAZING)
!--------------------------------------------------
!        IF (ixxxlu(iland)==3) THEN
!          IF (iseason==1) THEN
!--------------------------------------------------
!           SUMMER
!--------------------------------------------------
!            rcx(p_nh3) = 1000.
!          END IF
!          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
!            IF (tc>-1.) THEN
!              IF (rad/=0.) THEN
!                rcx(p_nh3) = 50.
!              ELSE
!                rcx(p_nh3) = 100.
!              END IF
!              IF ((wetflag) .OR. (rainflag)) THEN
!                rcx(p_nh3) = 20.
!              END IF
!            END IF
!            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
!              rcx(p_nh3) = 200.
!            END IF
!            IF (tc<(-5.)) THEN
!              rcx(p_nh3) = 500.
!            END IF
!          END IF
!        END IF
!--------------------------------------------------
!       AGRICULTURAL LAND (CROPS AND UNGRAZED PASTURE)
!--------------------------------------------------
!        IF (ixxxlu(iland)==2) THEN
!          IF (iseason==1) THEN
!--------------------------------------------------
!           SUMMER
!--------------------------------------------------
!            IF (rad/=0.) THEN
!              rcx(p_nh3) = rsmx
!            ELSE
!              rcx(p_nh3) = 200.
!            END IF
!            IF ((wetflag) .OR. (rainflag)) THEN
!              rcx(p_nh3) = 50.
!            END IF
!          END IF
!          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
!            IF (tc>-1.) THEN
!              IF (rad/=0.) THEN
!                rcx(p_nh3) = rsmx
!              ELSE
!                rcx(p_nh3) = 300.
!              END IF
!              IF ((wetflag) .OR. (rainflag)) THEN
!                rcx(p_nh3) = 100.
!              END IF
!            END IF
!            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
!              rcx(p_nh3) = 200.
!            END IF
!            IF (tc<(-5.)) THEN
!              rcx(p_nh3) = 500.
!            END IF
!          END IF
!        END IF
!--------------------------------------------------
!       SEMI-NATURAL ECOSYSTEMS AND FORESTS
!--------------------------------------------------
    !    IF ((ixxxlu(iland)==4) .OR. (ixxxlu(iland)==5) .OR. (ixxxlu( &
    !        iland)==6)) THEN
    !      IF (rad/=0.) THEN
    !        rcx(p_nh3) = 500.
    !      ELSE
    !        rcx(p_nh3) = 1000.
    !      END IF
    !      IF ((wetflag) .OR. (rainflag)) THEN
    !        IF (highnh3) THEN
    !          rcx(p_nh3) = 100.
    !        ELSE
    !          rcx(p_nh3) = 0.
    !        END IF
    !      END IF
         ! IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
        !    IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
        !      rcx(p_nh3) = 200.
        !    END IF
        !    IF (tc<(-5.)) THEN
         !     rcx(p_nh3) = 500.
         !   END IF
         ! END IF
        !END IF
!--------------------------------------------------
!       WATER
!--------------------------------------------------
        !IF (iland==iswater_temp) THEN
        !  rcx(p_nh3) = 0.
        !END IF
!--------------------------------------------------
!       URBAN AND DESERT (SOIL SURFACES)
!--------------------------------------------------
       ! IF (ixxxlu(iland)==1) THEN
       !   IF ( .NOT. wetflag) THEN
       !     rcx(p_nh3) = 50.
       !   ELSE
       !     rcx(p_nh3) = 0.
       !   END IF
       ! END IF
!--------------------------------------------------
!       SNOW COVERED SURFACES OR PERMANENT ICE
!--------------------------------------------------
       ! IF ((iseason==4) .OR. (iland==isice_temp)) THEN
       !   IF (tc>2.) THEN
       !     rcx(p_nh3) = 0.
       !   END IF
       !   IF ((tc>=(-1.)) .AND. (tc<=2.)) THEN
       !     rcx(p_nh3) = 70.*(2.-tc)
       !   END IF
       !   IF (tc<(-1.)) THEN
       !     rcx(p_nh3) = 500.
       !   END IF
       !   END IF
       ! rcx(p_nh3) = max( 1.,rcx(p_nh3) )
       !endif is_nh3
      END SUBROUTINE rc

      SUBROUTINE deppart( rmol, ustar, rh, iland, dvpart )
!--------------------------------------------------
!     THIS SUBROUTINE CALCULATES SURFACE DEPOSITION VELOCITIES
!     FOR FINE AEROSOL PARTICLES ACCORDING TO THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY WINFRIED SEIDL, APRIL 1997
!     MODIFIED BY WINFRIED SEIDL, MARCH 2000
!            FOR MM5 VERSION 3
!--------------------------------------------------

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: iland
        REAL, intent(in)    :: rh, rmol, ustar
        REAL, intent(out)   :: dvpart

!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC exp

        dvpart = ustar/kpart(iland)
        IF (rmol<0.) THEN
!--------------------------------------------------
!         UNSTABLE LAYERING CORRECTION
!--------------------------------------------------
          dvpart = dvpart*(1.+(-300.*rmol)**0.66667)
        END IF
        IF (rh>80.) THEN
!--------------------------------------------------
!         HIGH RELATIVE HUMIDITY CORRECTION
!         ACCORDING TO J. W. ERISMAN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 321-332
!--------------------------------------------------
          dvpart = dvpart*(1.+0.37*exp((rh-80.)/20.))
        END IF

!--------------------------------------------------
!       SEDIMENTATION VELOCITY OF FOG WATER ACCORDING TO
!       R. FORKEL, W. SEIDL, R. DLUGI AND E. DEIGELE
!       J. GEOPHYS. RES. 95D (1990), 18501-18515
!--------------------------------------------------
!        dvfog = 0.06*clw
        IF (ixxxlu(iland)==5) THEN
!--------------------------------------------------
!         TURBULENT DEPOSITION OF FOG WATER IN CONIFEROUS FOREST ACCORDI
!         A. T. VERMEULEN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 375-386
!--------------------------------------------------
!          dvfog = dvfog + 0.195*ustar*ustar
        END IF

      END SUBROUTINE deppart

      SUBROUTINE landusevg( vgs, ustar, rmol, z0, zz, &
                            dvparx, iland, numgas, srfres, aer_res_def)
                            !aer_res_zcen)
!--------------------------------------------------
!     This subroutine calculates the species specific deposition velocit
!     as a function of the local meteorology and land use.  The depositi
!     Velocity is also landuse specific.
!     Reference: Hsieh, C.M., Wesely, M.L. and Walcek, C.J. (1986)
!                A Dry Deposition Module for Regional Acid Deposition
!                EPA report under agreement DW89930060-01
!     Revised version by Darrell Winner (January 1991)
!        Environmental Engineering Science 138-78
!           California Institute of Technology
!              Pasadena, CA  91125
!     Modified by Winfried Seidl (August 1997)
!       Fraunhofer-Institut fuer Atmosphaerische Umweltforschung
!                    Garmisch-Partenkirchen, D-82467
!          for use of Wesely and Erisman surface resistances
!     Inputs:
!        Ustar  : The grid average friction velocity (m/s)
!        Rmol   : Reciprocal of the Monin-Obukhov length (1/m)
!        Z0     : Surface roughness height for the grid square (m)
!        SrfRes : Array of landuse/atmospheric/species resistances (s/m)
!        Slist  : Array of chemical species codes
!        Dvparx : Array of surface deposition velocity of fine aerosol p
!     Outputs:
!        Vgs    : Array of species and landuse specific deposition
!                 velocities (m/s)
!        Vg     : Cell-average deposition velocity by species (m/s)
!     Variables used:
!        SCPR23  : (Schmidt #/Prandtl #)**(2/3) Diffusion correction fac
!        Zr      : Reference Height (m)
!        Iatmo   : Parameter specifying the stabilty class (Function of
!        Z0      : Surface roughness height (m)
!        karman  : Von Karman constant (from module_model_constants)
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: iland, numgas    !, p_sulf
        REAL, intent(in)    :: dvparx, ustar, z0, zz
        REAL, intent(inout) :: rmol
        REAL, intent(inout) :: aer_res_def
!        REAL, intent(inout) :: aer_res_zcen
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(in)  :: srfres(numgas)
        REAL, intent(out) :: vgs(numgas)

!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: jspec
        REAL    :: vgp, vgpart, zr
        REAL    :: rmol_tmp
!--------------------------------------------------
! .. Local Arrays ..
!--------------------------------------------------
        REAL :: vgspec(numgas)

!--------------------------------------------------
!   Calculate aerodynamic resistance for reference
!   height = layer center
!--------------------------------------------------
        zr = zz*.5
        rmol_tmp = rmol
        CALL depvel( numgas, rmol_tmp, zr, z0, ustar, vgspec, vgpart, aer_res_def )
!--------------------------------------------------
!   Set the reference height (2.0 m)
!--------------------------------------------------
!       zr = 10.0
        zr = 2.0

!--------------------------------------------------
!   CALCULATE THE DEPOSITION VELOCITY without any surface
!   resistance term, i.e. 1 / (ra + rb)
!--------------------------------------------------
        CALL depvel( numgas, rmol, zr, z0, ustar, vgspec, vgpart, aer_res_def )

!--------------------------------------------------
!   Calculate the deposition velocity for each species
!   and grid cell by looping through all the possibile combinations
!   of the two
!--------------------------------------------------
        vgp = 1.0/((1.0/vgpart)+(1.0/dvparx))
!--------------------------------------------------
!   Loop through the various species
!--------------------------------------------------
        DO jspec = 1, numgas
!--------------------------------------------------
!   Add in the surface resistance term, rc (SrfRes)
!--------------------------------------------------
          vgs(jspec) = 1.0/(1.0/vgspec(jspec) + srfres(jspec))
        END DO
!        vgs(p_sulf) = vgp

        CALL cellvg( vgs, ustar, zz, zr, rmol, numgas )

      END SUBROUTINE landusevg

      SUBROUTINE cellvg( vgtemp, ustar, dz, zr, rmol, nspec )
!--------------------------------------------------
!     THIS PROGRAM HAS BEEN DESIGNED TO CALCULATE THE CELL AVERAGE
!     DEPOSITION VELOCITY GIVEN THE VALUE OF VG AT SOME REFERENCE
!     HEIGHT ZR WHICH IS MUCH SMALLER THAN THE CELL HEIGHT DZ.
!       PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner    (February 1991)
!.....PROGRAM VARIABLES...
!     VgTemp   - DEPOSITION VELOCITY AT THE REFERENCE HEIGHT
!     USTAR    - FRICTION VELOCITY
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     DZ       - CELL HEIGHT
!     CELLVG   - CELL AVERAGE DEPOSITION VELOCITY
!     VK       - VON KARMAN CONSTANT
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: nspec
        REAL, intent(in)    :: dz, rmol, ustar, zr
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(out) :: vgtemp(nspec)
!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: nss
        REAL    :: a, fac, pdz, pzr, vk
!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC alog, sqrt

!--------------------------------------------------
!     Set the von Karman constant
!--------------------------------------------------
        vk = karman

!--------------------------------------------------
!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE
!--------------------------------------------------
        DO nss = 1, nspec
          IF (rmol < 0.) THEN
            pdz = sqrt(1.0 - 9.0*dz*rmol)
            pzr = sqrt(1.0 - 9.0*zr*rmol)
            fac = ((pdz - 1.0)/(pzr - 1.0))*((pzr + 1.0)/(pdz + 1.0))
            a   = 0.74*dz*alog(fac) + (0.164/rmol)*(pdz-pzr)
          ELSE IF (rmol == 0.) THEN
            a = 0.74*(dz*alog(dz/zr) - dz + zr)
          ELSE
            a = 0.74*(dz*alog(dz/zr) - dz + zr) + (2.35*rmol)*(dz - zr)**2
          END IF
!--------------------------------------------------
!     CALCULATE THE DEPOSITION VELOCITIY
!--------------------------------------------------
          vgtemp(nss) = vgtemp(nss)/(1.0 + vgtemp(nss)*a/(vk*ustar*(dz - zr)))
        END DO

      END SUBROUTINE cellvg

      SUBROUTINE depvel( numgas, rmol, zr, z0, ustar, depv, vgpart, aer_res )
!--------------------------------------------------
!     THIS FUNCTION HAS BEEN DESIGNED TO EVALUATE AN UPPER LIMIT
!     FOR THE POLLUTANT DEPOSITION VELOCITY AS A FUNCTION OF THE
!     SURFACE ROUGHNESS AND METEOROLOGICAL CONDITIONS.
!     PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner  (Feb. 1991)
!                  by Winfried Seidl     (Aug. 1997)
!.....PROGRAM VARIABLES...
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     Z0       - SURFACE ROUGHNESS HEIGHT
!     SCPR23   - (Schmidt #/Prandtl #)**(2/3) Diffusion correction fact
!     UBAR     - ABSOLUTE VALUE OF SURFACE WIND SPEED
!     DEPVEL   - POLLUTANT DEPOSITION VELOCITY
!     Vk       - VON KARMAN CONSTANT
!     USTAR    - FRICTION VELOCITY U*
!     POLINT   - POLLUTANT INTEGRAL
!     AER_RES  - AERODYNAMIC RESISTANCE
!.....REFERENCES...
!     MCRAE, G.J. ET AL. (1983) MATHEMATICAL MODELING OF PHOTOCHEMICAL
!       AIR POLLUTION, ENVIRONMENTAL QUALITY LABORATORY REPORT 18,
!       CALIFORNIA INSTITUTE OF TECHNOLOGY, PASADENA, CALIFORNIA.
!.....RESTRICTIONS...
!     1. THE MODEL EDDY DIFFUSIVITIES ARE BASED ON MONIN-OBUKHOV
!        SIMILARITY THEORY AND SO ARE ONLY APPLICABLE IN THE
!        SURFACE LAYER, A HEIGHT OF O(30M).
!     2. ALL INPUT UNITS MUST BE CONSISTENT
!     3. THE PHI FUNCTIONS USED TO CALCULATE THE FRICTION
!        VELOCITY U* AND THE POLLUTANT INTEGRALS ARE BASED
!        ON THE WORK OF BUSINGER ET AL.(1971).
!     4. THE MOMENTUM AND POLLUTANT DIFFUSIVITIES ARE NOT
!        THE SAME FOR THE CASES L<0 AND L>0.
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: numgas
        REAL, intent(in)    :: ustar, z0, zr
        REAL, intent(out)   :: vgpart, aer_res
        REAL, intent(inout) :: rmol
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(out) :: depv(numgas)
!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: l
        REAL    :: ao, ar, polint, vk
!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC alog
!--------------------------------------------------
!     Set the von Karman constant
!--------------------------------------------------
        vk = karman

!--------------------------------------------------
!     Calculate the diffusion correction factor
!     SCPR23 is calculated as (Sc/Pr)**(2/3) using Sc= 1.15 and Pr= 1.0
!     SCPR23 = 1.10
!--------------------------------------------------
!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE
!--------------------------------------------------

        if(abs(rmol) < 1.E-6 ) rmol = 0.

        IF (rmol<0) THEN
          ar = ((1.0-9.0*zr*rmol)**(0.25)+0.001)**2
          ao = ((1.0-9.0*z0*rmol)**(0.25)+0.001)**2
          polint = 0.74*(alog((ar-1.0)/(ar+1.0))-alog((ao-1.0)/(ao+1.0)))
        ELSE IF (rmol==0.) THEN
          polint = 0.74*alog(zr/z0)
        ELSE
          polint = 0.74*alog(zr/z0) + 4.7*rmol*(zr-z0)
        END IF

!--------------------------------------------------
!     CALCULATE THE Maximum DEPOSITION VELOCITY
!--------------------------------------------------
        DO l = 1, numgas
          depv(l) = ustar*vk/(2.0*scpr23(l)+polint)
        END DO
        vgpart = ustar*vk/polint
        aer_res = polint/(karman*max(ustar,1.0e-4))

      END SUBROUTINE depvel

      SUBROUTINE dep_init( id, config_flags, numgas, mminlu_loc, &
                           ips, ipe, jps, jpe, ide, jde )
!--

!--------------------------------------------------
! .. Initialize simple deposition velocity routine
!--------------------------------------------------

  USE module_model_constants
  USE module_configure
  USE module_state_description                       

   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
        character(len=*), intent(in) :: mminlu_loc
!--

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        integer, intent(in) :: id, numgas
        integer, intent(in) :: ips, ipe, jps, jpe
        integer, intent(in) :: ide, jde

!--------------------------------------------------
! .. Local Scalars
!--------------------------------------------------
        INTEGER :: iland, iseason, l
!        integer :: iprt
        integer :: astat
        integer :: ncid
        integer :: dimid
        integer :: varid
        integer :: max_dom
!        integer :: cpos, slen
        integer :: lon_e, lat_e
        integer :: iend, jend
!        integer, allocatable :: input_wes_seasonal(:,:,:,:)
        REAL    :: sc
        character(len=128) :: err_msg
        character(len=128) :: filename
        character(len=3)   :: id_num
!--------------------------------------------------
! .. Local Arrays
!--------------------------------------------------
        REAL :: dat1(nlu,dep_seasons), dat2(nlu,dep_seasons),         &
                dat3(nlu,dep_seasons), dat4(nlu,dep_seasons),         &
                dat5(nlu,dep_seasons), dat6(nlu,dep_seasons),         &
                dat7(nlu,dep_seasons), dvj(numgas)

#ifdef NETCDF
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * IWORDSIZE )
include 'netcdf.inc'

!#else
!      if( config_flags%chem_opt == MOZART_KPP .or. &
!          config_flags%chem_opt == MOZCART_KPP .or. &
!          config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
!          config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
!         call wrf_message( 'dep_init: mozart,mozcart chem option requires netcdf' )
!         call wrf_abort
!      end if
#endif

!--------------------------------------------------
! .. Make sure that the model is being run with a soil model. Otherwise,
!    iland will be zero in deppart, which will try to pull non-exisant
!    array locations.
!--------------------------------------------------
        call nl_get_sf_surface_physics(id,l)
        if( l == 0 ) &
             call wrf_error_fatal("ERROR: Cannot use dry deposition without using a soil model.")

! ..
! .. Data Statements ..
!     RI for stomatal resistance
!      data ((ri(ILAND,ISEASON),ILAND=1,nlu),ISEASON=1,dep_seasons)/0.10E+11, &
        DATA ((dat1(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.60E+02, 0.60E+02, 0.60E+02, 0.60E+02, 0.70E+02, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.12E+03, 0.70E+02, 0.13E+03, 0.70E+02, &
          0.13E+03, 0.10E+03, 0.10E+11, 0.80E+02, 0.10E+03, 0.10E+11, &
          0.80E+02, 0.10E+03, 0.10E+03, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, 0.10E+11, &
          0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, &
          0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, &
          0.10E+11, 0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.70E+02, 0.40E+03, 0.80E+03, 0.10E+11, &
          0.10E+11, 0.80E+03, 0.10E+11, 0.10E+11, 0.80E+03, 0.80E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.14E+03, 0.24E+03, 0.24E+03, 0.24E+03, &
          0.12E+03, 0.14E+03, 0.25E+03, 0.70E+02, 0.25E+03, 0.19E+03, &
          0.10E+11, 0.16E+03, 0.19E+03, 0.10E+11, 0.16E+03, 0.19E+03, &
          0.19E+03, 0.10E+11, 0.10E+11, 0.10E+11/
! ..
        IF (nlu/=25) THEN
          call wrf_debug(0, 'number of land use classifications not correct ')
          CALL wrf_error_fatal ( "LAND USE CLASSIFICATIONS NOT 25")
        END IF
        IF (dep_seasons/=5) THEN
          call wrf_debug(0, 'number of dep_seasons not correct ')
          CALL wrf_error_fatal ( "DEP_SEASONS NOT 5")
        END IF

!     SURFACE RESISTANCE DATA FOR DEPOSITION MODEL OF
!     M. L. WESELY, ATMOSPHERIC ENVIRONMENT 23 (1989) 1293-1304

!     Seasonal categories:
!     1: midsummer with lush vegetation
!     2: autumn with unharvested cropland
!     3: late autumn with frost, no snow
!     4: winter, snow on ground and subfreezing
!     5: transitional spring with partially green short annuals

!     Land use types:
!     USGS type                                Wesely type
!      1: Urban and built-up land              1
!      2: Dryland cropland and pasture         2
!      3: Irrigated cropland and pasture       2
!      4: Mix. dry/irrg. cropland and pasture  2
!      5: Cropland/grassland mosaic            2
!      6: Cropland/woodland mosaic             4
!      7: Grassland                            3
!      8: Shrubland                            3
!      9: Mixed shrubland/grassland            3
!     10: Savanna                              3, always summer
!     11: Deciduous broadleaf forest           4
!     12: Deciduous needleleaf forest          5, autumn and winter modi
!     13: Evergreen broadleaf forest           4, always summer
!     14: Evergreen needleleaf forest          5
!     15: Mixed Forest                         6
!     16: Water Bodies                         7
!     17: Herbaceous wetland                   9
!     18: Wooded wetland                       6
!     19: Barren or sparsely vegetated         8
!     20: Herbaceous Tundra                    9
!     21: Wooded Tundra                        6
!     22: Mixed Tundra                         6
!     23: Bare Ground Tundra                   8
!     24: Snow or Ice                          -, always winter
!     25: No data                              8


!     Order of data:
!      |
!      |   seasonal category
!     \|/
!     ---> landuse type
!     1       2       3       4       5       6       7       8       9
!     RLU for outer surfaces in the upper canopy
        DO iseason = 1, dep_seasons
           ri(1:nlu,iseason) = dat1(1:nlu,iseason)
        END DO
!      data ((rlu(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat2(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, &
          0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, &
          0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.20E+04, 0.60E+04, 0.90E+04, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rlu(1:nlu,iseason) = dat2(1:nlu,iseason)
        END DO
!     RAC for transfer that depends on canopy height and density
!      data ((rac(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+03, &
        DATA ((dat3(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+04, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.00E+00, 0.30E+03, 0.20E+04, 0.00E+00, &
          0.30E+03, 0.20E+04, 0.20E+04, 0.00E+00, 0.00E+00, 0.00E+00, &
          0.10E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+04, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.15E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.17E+04, 0.00E+00, 0.20E+03, 0.17E+04, &
          0.00E+00, 0.20E+03, 0.17E+04, 0.17E+04, 0.00E+00, 0.00E+00, &
          0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, 0.10E+03, &
          0.15E+04, 0.00E+00, 0.10E+03, 0.15E+04, 0.15E+04, 0.00E+00, &
          0.00E+00, 0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+02, 0.10E+04, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, &
          0.50E+02, 0.15E+04, 0.00E+00, 0.50E+02, 0.15E+04, 0.15E+04, &
          0.00E+00, 0.00E+00, 0.00E+00, 0.10E+03, 0.50E+02, 0.50E+02, &
          0.50E+02, 0.50E+02, 0.12E+04, 0.80E+02, 0.80E+02, 0.80E+02, &
          0.10E+03, 0.12E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, &
          0.00E+00, 0.20E+03, 0.15E+04, 0.00E+00, 0.20E+03, 0.15E+04, &
          0.15E+04, 0.00E+00, 0.00E+00, 0.00E+00/
        DO iseason = 1, dep_seasons
           rac(1:nlu,iseason) = dat3(1:nlu,iseason)
        END DO
!     RGSS for ground surface  SO2
!      data ((rgss(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.40E+03, &
        DATA ((dat4(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.40E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, 0.10E+04, &
          0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+04, &
          0.40E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.50E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, &
          0.10E+04, 0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, &
          0.10E+04, 0.40E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, 0.10E+01, 0.10E+01, &
          0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, 0.20E+03, 0.10E+04, &
          0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.50E+03, 0.10E+03, 0.10E+03, 0.10E+01, &
          0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+04, 0.10E+03, 0.10E+04, 0.50E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, &
          0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, &
          0.10E+01, 0.10E+01, 0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, &
          0.20E+03, 0.10E+04, 0.10E+03, 0.10E+04/
        DO iseason = 1, dep_seasons
           rgss(1:nlu,iseason) = dat4(1:nlu,iseason)
        END DO
!     RGSO for ground surface  O3
!      data ((rgso(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.30E+03, &
        DATA ((dat5(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.30E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, &
          0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03, &
          0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.80E+03, 0.30E+03, &
          0.40E+03, 0.80E+03, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, &
          0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, &
          0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, &
          0.35E+04, 0.40E+03, 0.60E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.20E+03, 0.35E+04, 0.35E+04, 0.20E+04, &
          0.35E+04, 0.35E+04, 0.40E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.40E+03, 0.35E+04, 0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, &
          0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, &
          0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03/
        DO iseason = 1, dep_seasons
           rgso(1:nlu,iseason) = dat5(1:nlu,iseason)
        END DO
!     RCLS for exposed surfaces in the lower canopy  SO2
!      data ((rcls(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat6(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.20E+04, 0.40E+04, 0.10E+11, 0.90E+04, 0.40E+04, &
          0.10E+11, 0.90E+04, 0.40E+04, 0.40E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.30E+04, 0.60E+04, 0.10E+11, 0.90E+04, &
          0.60E+04, 0.10E+11, 0.90E+04, 0.60E+04, 0.60E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.20E+04, 0.20E+03, 0.40E+03, 0.10E+11, &
          0.90E+04, 0.40E+03, 0.10E+11, 0.90E+04, 0.40E+03, 0.40E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rcls(1:nlu,iseason) = dat6(1:nlu,iseason)
        END DO
!     RCLO for exposed surfaces in the lower canopy  O3
!      data ((rclo(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat7(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, 0.40E+03, &
          0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.40E+03, 0.60E+03, &
          0.10E+11, 0.40E+03, 0.60E+03, 0.60E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, &
          0.40E+03, 0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.80E+03, &
          0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.40E+03, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.10E+04, 0.15E+04, 0.60E+03, 0.10E+11, &
          0.80E+03, 0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.10E+04, 0.50E+03, 0.15E+04, 0.10E+04, 0.15E+04, 0.70E+03, &
          0.10E+11, 0.60E+03, 0.70E+03, 0.10E+11, 0.60E+03, 0.70E+03, &
          0.70E+03, 0.10E+11, 0.10E+11, 0.10E+11/

        DO iseason = 1, dep_seasons
           rclo(1:nlu,iseason) = dat7(1:nlu,iseason)
        END DO

!     data ((dat8(iseason,iland),iseason=1,5),iland=1,11) / &
!                          1.e36,  60., 120.,  70., 130., 100.,1.e36,1.e36,  80., 100., 150., &
!                          1.e36,1.e36,1.e36,1.e36, 250., 500.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36,1.e36,1.e36,1.e36, 250., 500.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36,1.e36,1.e36,1.e36, 400., 800.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36, 120., 240., 140., 250., 190.,1.e36,1.e36, 160., 200., 300. /
!       ri_pan(:,:) = dat8(:,:)

!--------------------------------------------------
!	Initialize parameters
!--------------------------------------------------
        hstar(1:numgas)  = 0.
        hstar4(1:numgas) = 0.
        dhr(1:numgas)    = 0.
        f0(1:numgas)     = 0.
        dvj(1:numgas)    = 99.

        if( id == 1 .or. id == 2 ) then
!       print*,"modis: num_land ",id,config_flags%num_land_cat
        if( allocated (luse2usgs) ) deallocate (luse2usgs)
        allocate( luse2usgs(config_flags%num_land_cat),stat=astat )
        if( astat /= 0 ) then
           CALL wrf_message( 'dep_init: failed to allocate luse2usgs array' )
           CALL wrf_abort
        end if
        if( trim(mminlu_loc) == 'USGS' ) then
           luse2usgs(:) = (/ (iland,iland=1,config_flags%num_land_cat) /)
         elseif( trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' ) then
           luse2usgs(:) = (/ 14,13,12,11,15,8,9,10,10,7, &
                              17,4,1,5,24,19,16,21,22,23 /)
         endif
        endif

!--------------------------------------------------
!     HENRY''S LAW COEFFICIENTS
!     Effective Henry''s law coefficient at pH 7
!     [KH298]=mole/(l atm)
!--------------------------------------------------
!     DATA FOR AEROSOL PARTICLE DEPOSITION FOR THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL AND P. WYERS
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607

!     vd = (u* / k) * CORRECTION FACTORS

!     CONSTANT K FOR LANDUSE TYPES:
! urban and built-up land                  
        kpart(1) = 500.
! dryland cropland and pasture             
        kpart(2) = 500.
! irrigated cropland and pasture           
        kpart(3) = 500.
! mixed dryland/irrigated cropland and past
        kpart(4) = 500.
! cropland/grassland mosaic                
        kpart(5) = 500.
! cropland/woodland mosaic                 
        kpart(6) = 100.
! grassland                                
        kpart(7) = 500.
! shrubland                                
        kpart(8) = 500.
! mixed shrubland/grassland                
        kpart(9) = 500.
! savanna                                  
        kpart(10) = 500.
! deciduous broadleaf forest               
        kpart(11) = 100.
! deciduous needleleaf forest              
        kpart(12) = 100.
! evergreen broadleaf forest               
        kpart(13) = 100.
! evergreen needleleaf forest              
        kpart(14) = 100.
! mixed forest                             
        kpart(15) = 100.
! water bodies                             
        kpart(16) = 500.
! herbaceous wetland                       
        kpart(17) = 500.
! wooded wetland                           
        kpart(18) = 500.
! barren or sparsely vegetated             
        kpart(19) = 500.
! herbaceous tundra                        
        kpart(20) = 500.
! wooded tundra                            
        kpart(21) = 100.
! mixed tundra                             
        kpart(22) = 500.
! bare ground tundra                       
        kpart(23) = 500.
! snow or ice                              
        kpart(24) = 500.
!     Comments:
        kpart(25) = 500.
!     Erisman et al. (1994) give
!     k = 500 for low vegetation and k = 100 for forests.

!     For desert k = 500 is taken according to measurements
!     on bare soil by
!     J. Fontan, A. Lopez, E. Lamaud and A. Druilhet (1997)
!     Vertical Flux Measurements of the Submicronic Aerosol Particles
!     and Parametrisation of the Dry Deposition Velocity
!     in: Biosphere-Atmosphere Exchange of Pollutants
!     and Trace Substances
!     Editor: S. Slanina. Springer-Verlag Berlin, Heidelberg, 1997
!     pp. 381-390

!     For coniferous forest the Erisman value of  k = 100 is taken.
!     Measurements of Erisman et al. (1997) in a coniferous forest
!     in the Netherlands, lead to values of k between 20 and 38
!     (Atmospheric Environment 31 (1997), 321-332).
!     However, these high values of vd may be reached during
!     instable cases. The eddy correlation measurements
!     of Gallagher et al. (1997) made during the same experiment
!     show for stable cases (L>0) values of k between 200 and 250
!     at minimum (Atmospheric Environment 31 (1997), 359-373).
!     Fontan et al. (1997) found k = 250 in a forest
!     of maritime pine in southwestern France.

!     For gras, model calculations of Davidson et al. support
!     the value of 500.
!     C. I. Davidson, J. M. Miller and M. A. Pleskov
!     The Influence of Surface Structure on Predicted Particles
!     Dry Deposition to Natural Gras Canopies
!     Water, Air, and Soil Pollution 18 (1982) 25-43

!     Snow covered surface: The experiment of Ibrahim et al. (1983)
!     gives k = 436 for 0.7 um diameter particles.
!     The deposition velocity of Milford and Davidson (1987)
!     gives k = 154 for continental sulfate aerosol.
!     M. Ibrahim, L. A. Barrie and F. Fanaki
!     Atmospheric Environment 17 (1983), 781-788

!     J. B. Milford and C. I. Davidson
!     The Sizes of Particulate Sulfate and Nitrate in the Atmosphere
!     - A Review
!     JAPCA 37 (1987), 125-134
! no data                                  
!       WRITE (0,*) ' return from rcread '
!     *********************************************************

!     Simplified landuse scheme for deposition and biogenic emission
!     subroutines
!     (ISWATER and ISICE are already defined elsewhere,
!     therefore water and ice are not considered here)

!     1 urban or bare soil
!     2 agricultural
!     3 grassland
!     4 deciduous forest
!     5 coniferous and mixed forest
!     6 other natural landuse categories


        IF (mminlu=='OLD ') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 3
          ixxxlu(4) = 4
          ixxxlu(5) = 5
          ixxxlu(6) = 5
          ixxxlu(7) = 0
          ixxxlu(8) = 6
          ixxxlu(9) = 1
          ixxxlu(10) = 6
          ixxxlu(11) = 0
          ixxxlu(12) = 4
          ixxxlu(13) = 6
        END IF
        IF (mminlu=='USGS') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 2
          ixxxlu(4) = 2
          ixxxlu(5) = 2
          ixxxlu(6) = 4
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 3
          ixxxlu(10) = 6
          ixxxlu(11) = 4
          ixxxlu(12) = 5
          ixxxlu(13) = 4
          ixxxlu(14) = 5
          ixxxlu(15) = 5
          ixxxlu(16) = 0
          ixxxlu(17) = 6
          ixxxlu(18) = 4
          ixxxlu(19) = 1
          ixxxlu(20) = 6
          ixxxlu(21) = 4
          ixxxlu(22) = 6
          ixxxlu(23) = 1
          ixxxlu(24) = 0
          ixxxlu(25) = 1
        END IF
        IF (mminlu=='SiB ') THEN
          ixxxlu(1) = 4
          ixxxlu(2) = 4
          ixxxlu(3) = 4
          ixxxlu(4) = 5
          ixxxlu(5) = 5
          ixxxlu(6) = 6
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 6
          ixxxlu(10) = 6
          ixxxlu(11) = 1
          ixxxlu(12) = 2
          ixxxlu(13) = 6
          ixxxlu(14) = 1
          ixxxlu(15) = 0
          ixxxlu(16) = 0
          ixxxlu(17) = 1
        END IF

      END SUBROUTINE dep_init

    END MODULE module_dep_simple
