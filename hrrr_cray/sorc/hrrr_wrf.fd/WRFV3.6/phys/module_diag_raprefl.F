!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_raprefl
CONTAINS


! DCD 10/25/11
! This subroutine uses dbzcalc2 to compute radar reflectivity and/or precipitation
! fall speed.  Currently, only the Thompson microphysics scheme is supported.
!
! A variable Thompson_scheme_version (not in the standard WRF release) must
! be defined in module_mp_thompson.F for the radar_diagnostic_calc and
! dbzcalc2 subroutines to work properly.
!
   SUBROUTINE radar_diagnostic_calc(                                    &
                      ims,ime, jms,jme, kms,kme, first_time_step        &
                     ,i_start,i_end,j_start,j_end,num_tiles             &
                     ,mp_physics, t, phb, ht, g, dt, rho_curr, raincv   &
                     ,qc_curr, qnr_curr, qr_curr, qs_curr, qg_curr      &
                     ,refl_10cm, refl_10cm_1km, refl_10cm_4km           &
                     ,composite_refl_10cm, vt_dbz_wt                    &
                                                                        )
!----------------------------------------------------------------------

  USE module_state_description, ONLY : THOMPSON,THOMPSONAERO
  USE module_mp_thompson, ONLY : Thompson_scheme_version

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
! Input:
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- num_tiles     number of tiles
!-- first_time_step  .true. if first time step in model integration
!-- mp_physics    index of precipitation microphysics scheme
!-- t             temperature (K)
!-- phb           base-state geopotential (m2 s-2)
!-- ht            terrain height (m)
!-- g             acceleration of gravity (m s-2)
!-- dt            time step (s)
!-- rho_curr      air density (kg m-3)
!-- raincv        cumulus scheme precipitation in one time step (mm)
!-- qc_curr       cloud water mixing ratio (kg kg-1)
!-- qnr_curr      rain number concentration (kg-1)
!-- qr_curr       rain water mixing ratio (kg kg-1)
!-- qs_curr       snow mixing ratio (kg kg-1)
!-- qg_curr       graupel mixing ratio (kg kg-1)
!
! Output:
!-- refl_10cm             radar reflectivity factor for 10-cm radar (dBZ)
!-- refl_10cm_1km         reflectivity at 1 km AGL (dBZ)
!-- refl_10cm_4km         reflectivity at 4 km AGL (dBZ)
!-- composite_refl_10cm   maximum reflectivity in column (dBZ)
!-- vt_dbz_wt             reflectivity-weighted precipitation fall speed (m s-1)
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ims,ime, jms,jme, kms,kme, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                  &
     &           i_start,i_end,j_start,j_end

   LOGICAL, INTENT(IN) :: first_time_step

   INTEGER, INTENT(IN) :: mp_physics

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                        qr_curr,qs_curr,qg_curr  &
                                              ,qc_curr,qnr_curr  &
                                                ,phb,t,rho_curr

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: ht
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(IN) :: raincv

   REAL, INTENT(IN) :: g, dt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL, INTENT(INOUT) :: refl_10cm

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), OPTIONAL, INTENT(INOUT) :: vt_dbz_wt
   REAL, DIMENSION( ims:ime , jms:jme ), OPTIONAL, INTENT(INOUT) ::  &
                                                composite_refl_10cm  &
                                               ,refl_10cm_1km        &
                                               ,refl_10cm_4km

   INTEGER :: i,j,k,ij

   REAL, PARAMETER :: dbzmin=-10.0

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: qra, qnr, qsn, qgr, qcw, tmk, rho, dbz, fallspd
   REAL  :: h_agl
   REAL  :: cuprate
   REAL  :: ze, ze_conv, dbz_sum
   INTEGER :: mi, mj, mk, k1, k2
   LOGICAL :: qnr_initialized, found_1km, found_4km


   IF ( mp_physics .EQ. THOMPSON .or. mp_physics .EQ. THOMPSONAERO ) THEN

     print *, 'microphysics scheme is Thompson version ', Thompson_scheme_version

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

     k1 = kms
     k2 = kme-1

     DO ij = 1 , num_tiles

       mi = i_end(ij) - i_start(ij) + 1
       mj = j_end(ij) - j_start(ij) + 1
       mk = k2 - k1 + 1

!       print *, 'ij, num_tiles, mi, mj, mk = ', ij, num_tiles, mi, mj, mk

       allocate (qra(mi+1,mj+1,mk))
       allocate (qnr(mi+1,mj+1,mk))
       allocate (qsn(mi+1,mj+1,mk))
       allocate (qgr(mi+1,mj+1,mk))
       allocate (qcw(mi+1,mj+1,mk))
       allocate (tmk(mi+1,mj+1,mk))
       allocate (rho(mi+1,mj+1,mk))
       allocate (dbz(mi+1,mj+1,mk))
       allocate (fallspd(mi+1,mj+1,mk))

       qnr_initialized = .true.

       ! Reorder indices and flip k index for arrays passed to dbzcalc2.
       DO j=j_start(ij),j_end(ij)
         DO k=k1,k2
           DO i=i_start(ij),i_end(ij)

             IF ( (qnr_curr(i,k,j) .le. 0.0) .and. (qr_curr(i,k,j) .ge. 1.e-12) ) THEN

!              Rain number concentration was not initialized properly, but we would still
!              like a reflectivity estimate.  Let's arbitrarily assume the drops have a
!              Marshall-Palmer distribution with intercept parameter 8000000 m**-4.
!              The desired units of qnr are kg**-1.

               qnr(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) =                          &
                 (8.e6 / rho_curr(i,k,j)) *                                             &
                 (rho_curr(i,k,j) * qr_curr(i,k,j) / (3.14159 * 1000.0 * 8.e6) )**0.25

               qnr_initialized = .false.

             ELSE
               qnr(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = qnr_curr(i,k,j)
             ENDIF

             qra(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = qr_curr(i,k,j)
             qsn(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = qs_curr(i,k,j)
             qgr(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = qg_curr(i,k,j)
             qcw(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = qc_curr(i,k,j)
             tmk(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = t(i,k,j)
             rho(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1) = rho_curr(i,k,j)

           ENDDO
         ENDDO
       ENDDO

       IF (.not. qnr_initialized) THEN
         print *, 'QNR not initialized.  Marshall-Palmer distribution will be assumed for reflectivity calculation.'
       ENDIF

       call dbzcalc2(Thompson_scheme_version,first_time_step,qra,qnr,qsn,qgr,qcw,tmk,rho,dbz,fallspd,mi+1,mj+1,mk)

       DO i=i_start(ij),i_end(ij)
         DO j=j_start(ij),j_end(ij)

           IF ( PRESENT(composite_refl_10cm) ) THEN
             composite_refl_10cm(i,j) = dbzmin
           ENDIF
           IF ( PRESENT(refl_10cm_1km) ) THEN
             refl_10cm_1km(i,j) = dbzmin
           ENDIF
           IF ( PRESENT(refl_10cm_4km) ) THEN
             refl_10cm_4km(i,j) = dbzmin
           ENDIF
           found_1km = .false.
           found_4km = .false.

!          reflectivity parameterization for parameterized convection (reference:  Unipost MDLFLD.f)
           ze_conv = 0.0
           IF ( PRESENT(raincv) ) THEN
             cuprate = raincv(i,j) * 3600.0 / dt          ! cu precip rate (mm/h)
             ze_conv = 300.0 * cuprate**1.4
           ENDIF

           DO k=k1,k2

!            approximation:  use base-state geopotential to determine height
             h_agl = phb(i,k,j) / g - ht(i,j)

             ze = 10.0 ** (0.1 * dbz(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1))
             dbz_sum = max(dbzmin, 10.0 * log10(ze + ze_conv))

             IF ( PRESENT(refl_10cm_1km) .and. (.not. found_1km) .and. (h_agl.gt.1000.0) ) THEN
               refl_10cm_1km(i,j) = dbz_sum
               found_1km = .true.
             ENDIF
             IF ( PRESENT(refl_10cm_4km) .and. (.not. found_4km) .and. (h_agl.gt.4000.0) ) THEN
               refl_10cm_4km(i,j) = dbz_sum
               found_4km = .true.
             ENDIF
             IF ( PRESENT(refl_10cm) ) THEN
               refl_10cm(i,k,j) = dbz_sum
             ENDIF
             IF ( PRESENT(composite_refl_10cm) ) THEN
               IF ( dbz_sum .GT. composite_refl_10cm(i,j) ) THEN
                 composite_refl_10cm(i,j) = dbz_sum
               ENDIF
             ENDIF
             IF ( PRESENT(vt_dbz_wt) ) THEN
               vt_dbz_wt(i,k,j) = fallspd(i-i_start(ij)+1, j-j_start(ij)+1, k2-k+1)
             ENDIF

           ENDDO
         ENDDO
       ENDDO

!       print *, 'max reflectivity = ', maxval(composite_refl_10cm)
!       print *, 'min reflectivity = ', minval(composite_refl_10cm)

       deallocate (qra)
       deallocate (qnr)
       deallocate (qsn)
       deallocate (qgr)
       deallocate (qcw)
       deallocate (tmk)
       deallocate (rho)
       deallocate (dbz)
       deallocate (fallspd)

     ENDDO  ! ij
!  !$OMP END PARALLEL DO

   ELSE

     print*, 'mp_physics = ', mp_physics
     CALL wrf_error_fatal('This microphysics scheme is not supported in radar_diagnostic_calc.')

   ENDIF   ! IF ( mp_physics .EQ. THOMPSON )

   END SUBROUTINE radar_diagnostic_calc

      subroutine dbzcalc2(Thompson_scheme_version,first_time_step,           &
                          qra,qnr,qsn,qgr,qcw,tmk,rho,dbz,vt_dbz,miy,mjx,mkzh)
!                         in0r,in0s,in0g,iliqskin)
!
!     This routine computes equivalent reflectivity factor (in dBZ) at
!     each model grid point.  In calculating Ze, RIP uses the same
!     assumptions as found in the new Thompson bulk microphysics
!     scheme of 2006.  The only difficult part is computing for
!     partially melted snow and graupel particles.  For this, the
!     code from Uli Blahak is simplified by assuming Rayleigh
!     scattering contribution only for 10 cm wavelength (NEXRAD) and
!     a simple function of meltwater coating the ice particles.

!+---+-----------------------------------------------------------------+
!.. Here are a few serious warnings.
!.. 1) Those users that modify parameters set at the top of my
!..  mp_thompson WRF routine MUST change to the same in this code or a
!..  mismatch will occur.  Big changes would likely result if users alter
!..  gamma shape parameters (mu_x) or intercept parameters.
!.. 2) This code is not intended for my mp_thompson WRF routine that was
!..  released prior to WRF-V3.1.  See preceeding subroutine for versions
!..  2.2, 2.2.1, 3.0 or 3.0.1.1 (before 2-moment rain).
!.. 3) The treatment of rain is simplest - just an integration of the
!..  explicit size distrib.  Snow & graupel without melting are also
!..  rather simple integrations if one assumes no water coats the
!..  particles when T<0C - which is what I did.  When snow/graupel exist
!..  above T=0C, it gets interesting (next item).
!.. 4) This code makes some rather simple assumptions about water
!..  coating on outside of frozen species (snow/graupel).  Fraction of
!..  meltwater is simply the ratio of mixing ratio below melting level
!..  divided by mixing ratio at level just above highest T>0C.  Also,
!..  immediately 70% of the melted water exists on the ice's surface
!..  and 30% is embedded within ice.  No water is "shed" at all in these
!..  assumptions.  The work-horse subroutines for reflectivity of melted
!..  particles comes from Ulrich Blahak (Univ. Karlsrhule in Germany):
!..  "rayleigh_soak_wetgraupel" (and related functions).  There are a
!..  large number of assumptions with his scheme that I simplified.
!.. 5) The code is quite slow because it does the reflectivity
!..  calculations based on 100 individual size bins of the distributions.
!.. 6) Ulrich has the code for treating Mie scattering as well as
!..  Rayleigh (which is all I'm using) so that other radar wavelengths
!..  could be considered in a future version.  Right now, only 10-cm
!..  (NEXRAD) is used herein.
!+---+-----------------------------------------------------------------+
!
      implicit none

! DCD 5/31/11, 2/12/12
      CHARACTER*8 Thompson_scheme_version
      logical first_time_step
      logical allow_wet_graupel
      logical allow_wet_snow
      logical calibrate_reflectivity
! DCD

      integer miy,mjx,mkzh
!      real in0r,in0s,in0g
!      integer iliqskin
      real qra(miy,mjx,mkzh), qnr(miy,mjx,mkzh), qcw(miy,mjx,mkzh), &
         qsn(miy,mjx,mkzh),qgr(miy,mjx,mkzh),tmk(miy,mjx,mkzh),     &
         rho(miy,mjx,mkzh),dbz(miy,mjx,mkzh)
      real vt_dbz(miy,mjx,mkzh)        ! reflectivity weighted precipitation fall speed (m/s)

      integer maxk
      parameter (maxk=100)
      logical L_qr(maxk)
      double precision ilamg(maxk), N0_g(maxk), &
           ilamr(maxk), N0_r(maxk)
      real mvd_r(maxk)
      real rs(maxk), rg(maxk), rr(maxk), nr(maxk), rc(maxk)
      real temp(maxk), smoz(maxk), smob(maxk), smoc(maxk)
      real ze_rain(maxk), ze_snow(maxk), ze_graupel(maxk)
      real rhof(maxk)
      real vts_dbz_wt, vtr_dbz_wt, vtg_dbz_wt

      real sa(10), sb(10), cse(12), csg(12), oams, obms
!      real WGAMMA
      real cre(13), crg(13), ore1, org1, org2, org3, oamr, obmr
      real cge(5), cgg(5), oge1, ogg1, ogg2, ogg3, oamg, obmg
      real ocms, ocmg
      real tc0, smo2, loga_, a_, b_
      real xDs, Mrat, Mrat2, ils1, ils2
      real t1_vts, t2_vts, t3_vts, t4_vts
      real xDg, Ds_m, Dg_m, r_mvd1, r_mvd2, xkrat
      double precision N0_exp, N0_min, lam_exp, lamg, lamr
      real z_e
      integer k_0, kbot, i, j, k, n
      logical melti

      REAL PI
      PARAMETER (PI = 3.1415926536)
      REAL R1
      PARAMETER (R1 = 1.e-12)
      REAL R2
      PARAMETER (R2 = 1.e-8)
      REAL rho_w
      PARAMETER (rho_w = 1000.0)
      REAL rho_i
      PARAMETER (rho_i = 890.0)
      REAL rho_g   ! DCD 3/20/13 value depends on Thompson microphysics scheme version
      REAL mu_r
      PARAMETER (mu_r = 0.0)
      REAL mu_g
      PARAMETER (mu_g = 0.0)
      REAL mu_s
      PARAMETER(mu_s = 0.6357)

      REAL gonv_min

      REAL gonv_max

      REAL am_r
      PARAMETER (am_r = PI*rho_w/6.0)
      REAL bm_r
      PARAMETER (bm_r = 3.0)
      REAL am_g   ! DCD 3/20/13 value depends on rho_g
      REAL bm_g
      PARAMETER (bm_g = 3.0)
      REAL am_s
      PARAMETER(am_s = 0.069)
      REAL bm_s
      PARAMETER(bm_s = 2.0)

      REAL Kap0
      PARAMETER(Kap0 = 490.6)
      REAL Kap1
      PARAMETER(Kap1 = 17.46)
      REAL Lam0
      PARAMETER(Lam0 = 20.78)
      REAL Lam1
      PARAMETER(Lam1 = 3.29)

!..Fallspeed power laws relations:  v = (av*D**bv)*exp(-fv*D)
!.. Rain from Ferrier (1994), ice, snow, and graupel from
!.. Thompson et al (2008). Coefficient fv is zero for graupel/ice.
      REAL, PARAMETER :: av_r = 4854.0
      REAL, PARAMETER :: bv_r = 1.0
      REAL, PARAMETER :: fv_r = 195.0
      REAL, PARAMETER :: av_s = 40.0
      REAL, PARAMETER :: bv_s = 0.55
      REAL fv_s   ! DCD 3/20/13 value depends on Thompson microphysics scheme version
      REAL, PARAMETER :: av_g = 442.0
      REAL, PARAMETER :: bv_g = 0.89
!     REAL, PARAMETER :: av_i = 1847.5
!     REAL, PARAMETER :: bv_i = 1.0

!..Rho_not used in fallspeed relations (rho_not/rho)**.5 adjustment.
      REAL, PARAMETER :: rho_not = 101325.0/(287.05*298.0)

!..Various radar related variables
      INTEGER nbins
      PARAMETER(nbins = 50)
      DOUBLE PRECISION xDx(nbins+1)
      DOUBLE PRECISION Ds(nbins), Dg(nbins), dts(nbins), dtg(nbins)
      DOUBLE PRECISION fmelt_s, fmelt_g, cback, xx, eta, f_d
      REAL oM3, M0, slam1, slam2
      REAL D0s, D0g
      PARAMETER (D0s = 200.E-6, D0g = 250.E-6)
      REAL xslw1, ygra1, zans1

      DOUBLE PRECISION lamda_radar
      PARAMETER(lamda_radar = 0.10)                                     ! in meters
      DOUBLE PRECISION K_w, PI5, lamda4
      COMPLEX*16 m_w_0, m_i_0
      DOUBLE PRECISION simpson(nbins+1)
      DOUBLE PRECISION basis(3)
      DOUBLE PRECISION melt_outside_s, melt_outside_g
      PARAMETER (melt_outside_s = 0.7d0, melt_outside_g = 0.7d0)
                    
      INTEGER slen
      PARAMETER(slen = 20)
      CHARACTER*(slen)  mixingrulestring_s,  matrixstring_s,       &
                        inclusionstring_s,  hoststring_s,          &
                        hostmatrixstring_s, hostinclusionstring_s, &
                        mixingrulestring_g, matrixstring_g,        &
                        inclusionstring_g,  hoststring_g,          &
                        hostmatrixstring_g, hostinclusionstring_g

!      COMPLEX*16 m_complex_water_ray, m_complex_ice_maetzler

!..For snow moments conversions (from Field et al. 2005)
      DATA sa / 5.065339, -0.062659, -3.032362, 0.029469, -0.000285,  &
                0.31255,   0.000204,  0.003199, 0.0,      -0.015952/
      DATA sb / 0.476221, -0.015896,  0.165977, 0.007468, -0.000141,  &
                0.060366,  0.000079,  0.000594, 0.0,      -0.003577/

      if (mkzh .gt. maxk) then
         print*, ' Cannot continue since mkzh is greater than maxk ', &
                 mkzh, maxk
         stop ' ABORT; Increase maxk'
      endif


! DCD 5/31/11, 2/18/12

      if (first_time_step) then
!       no bright banding, to be consistent with hydrometeor retrieval in GSI
        allow_wet_snow = .false.
      else
        allow_wet_snow = .true.
      endif

      allow_wet_graupel = .false.
!      allow_wet_graupel = .true.

      calibrate_reflectivity = .false.
!      calibrate_reflectivity = .true.

      if (Thompson_scheme_version .eq. '3.4.1') then
        rho_g = 500.0
        fv_s = 100.0
        print*,'rho_g, fv_s = ', rho_g, fv_s
      else
        rho_g = 400.0
        fv_s = 125.0
      endif
      am_g = PI*rho_g/6.0

      if ( (Thompson_scheme_version .eq. '3.2') .or. (Thompson_scheme_version .eq. '3.3') .or.   &
           (Thompson_scheme_version .eq. '3.3.1') .or. (Thompson_scheme_version .eq. '3.4.1') ) then
        gonv_min = 1.E4
      else if (Thompson_scheme_version .eq. '3.1.9') then
!       Version 3.1.9 is a non-official release but refers here to code provided by Greg Thompson
!       to Glen Romine on 19 October 2010 for STEP experiments.
        gonv_min = 1.E4
        allow_wet_graupel = .true.
      else
        print*, 'Thompson_scheme_version = ', Thompson_scheme_version
        print*, 'This version is not supported in dbzcalc2.'
!        return
        stop
      endif

      if ( (Thompson_scheme_version .eq. '3.2') .or. (Thompson_scheme_version .eq. '3.1.9') .or.   &
           (Thompson_scheme_version .eq. '3.3.1') .or. (Thompson_scheme_version .eq. '3.4.1') ) then
        gonv_max = 3.E6
      else if (Thompson_scheme_version .eq. '3.3') then
        gonv_max = 1.E6
      else
        print*, 'Thompson_scheme_version = ', Thompson_scheme_version
        print*, 'This version is not supported in dbzcalc2.'
        stop
      endif

      print*, 'allow_wet_snow = ', allow_wet_snow
      print*, 'allow_wet_graupel = ', allow_wet_graupel
      print*, 'calibrate_reflectivity (uniform) = ', calibrate_reflectivity

! DCD 5/31/11, 2/18/12

!..Precompute gamma values and exponents used later (rain, snow, graupel).
      cre(1) = bm_r + 1.
      cre(2) = mu_r + 1.
      cre(3) = bm_r + mu_r + 1.
      cre(4) = bm_r*2. + mu_r + 1.
      cre(5) = mu_r + bv_r + 1.
      cre(6) = bm_r + mu_r + bv_r + 1.
      cre(7) = bm_r*0.5 + mu_r + bv_r + 1.
      cre(8) = bm_r + mu_r + bv_r + 3.
      cre(9) = mu_r + bv_r + 3.
      cre(10) = mu_r + 2.
      cre(11) = 0.5*(bv_r + 5. + 2.*mu_r)
      cre(12) = bm_r*0.5 + mu_r + 1.
      cre(13) = bm_r*2. + mu_r + bv_r + 1
      do n = 1, 13
         crg(n) = WGAMMA(cre(n))
      enddo
      ore1 = 1./cre(1)
      org1 = 1./crg(1)
      org2 = 1./crg(2)
      org3 = 1./crg(3)
      oamr = 1./am_r
      obmr = 1./bm_r

      cse(1) = bm_s + 1.
      cse(2) = bm_s + 2.
      cse(3) = bm_s*2.
      cse(4) = bm_s + bv_s + 1.
      cse(5) = bm_s*2. + bv_s + 1.
      cse(6) = bm_s*2. + 1.
      cse(7) = bm_s + mu_s + 1.
      cse(8) = bm_s + mu_s + 2.
      cse(9) = bm_s + mu_s + 3.
      cse(10) = bm_s + mu_s + bv_s + 1.
      cse(11) = bm_s*2. + mu_s + bv_s + 1.
      cse(12) = bm_s*2. + mu_s + 1.
      do n = 1, 12
         csg(n) = WGAMMA(cse(n))
      enddo

      oams = 1./am_s
      obms = 1./bm_s
      ocms = oams**obms

      cge(1) = bm_g + 1.
      cge(2) = mu_g + 1.
      cge(3) = bm_g + mu_g + 1.
      cge(4) = bm_g*2. + mu_g + 1.
      cge(5) = bm_g*2. + mu_g + bv_g + 1.
      do n = 1, 5
         cgg(n) = WGAMMA(cge(n))
      enddo
      oge1 = 1./cge(1)
      ogg1 = 1./cgg(1)
      ogg2 = 1./cgg(2)
      ogg3 = 1./cgg(3)
      oamg = 1./am_g
      obmg = 1./bm_g
      ocmg = oamg**obmg

      basis(1) = 1.d0/3.d0
      basis(2) = 4.d0/3.d0
      basis(3) = 1.d0/3.d0

      PI5 = PI*PI*PI*PI*PI
      lamda4 = lamda_radar*lamda_radar*lamda_radar*lamda_radar
      m_w_0 = m_complex_water_ray (lamda_radar, 0.0d0)
      m_i_0 = m_complex_ice_maetzler (lamda_radar, 0.0d0)
      K_w = (ABS( (m_w_0*m_w_0 - 1.0) /(m_w_0*m_w_0 + 2.0) ))**2
      
      do n = 1, nbins+1
         simpson(n) = 0.0d0
      enddo
      do n = 1, nbins-1, 2
         simpson(n) = simpson(n) + basis(1)
         simpson(n+1) = simpson(n+1) + basis(2)
         simpson(n+2) = simpson(n+2) + basis(3)
      enddo

      do n = 1, slen
         mixingrulestring_s(n:n) = char(0)
         matrixstring_s(n:n) = char(0)
         inclusionstring_s(n:n) = char(0)
         hoststring_s(n:n) = char(0)
         hostmatrixstring_s(n:n) = char(0)
         hostinclusionstring_s(n:n) = char(0)
         mixingrulestring_g(n:n) = char(0)
         matrixstring_g(n:n) = char(0)
         inclusionstring_g(n:n) = char(0)
         hoststring_g(n:n) = char(0)
         hostmatrixstring_g(n:n) = char(0)
         hostinclusionstring_g(n:n) = char(0)
      enddo

      mixingrulestring_s = 'maxwellgarnett'
      hoststring_s = 'air'
      matrixstring_s = 'water'
      inclusionstring_s = 'spheroidal'
      hostmatrixstring_s = 'icewater'
      hostinclusionstring_s = 'spheroidal'

      mixingrulestring_g = 'maxwellgarnett'
      hoststring_g = 'air'
      matrixstring_g = 'water'
      inclusionstring_g = 'spheroidal'
      hostmatrixstring_g = 'icewater'
      hostinclusionstring_g = 'spheroidal'

      do k = 1, mkzh
      do j = 1, mjx-1
      do i = 1, miy-1
         dbz(i,j,k) = -35.0
      enddo
      enddo
      enddo

!     print*, ' DEBUG: ', tmk(140,112,1), tmk(140,112,mkzh)

      do 1000 j = 1, mjx-1
      do 1000 i = 1, miy-1

      do k = 1, mkzh

        rc(k) = MAX(R1, qcw(i,j,k)*rho(i,j,k))
        rr(k) = MAX(R1, qra(i,j,k)*rho(i,j,k))
        nr(k) = MAX(1., qnr(i,j,k)*rho(i,j,k))
        rg(k) = MAX(R1, qgr(i,j,k)*rho(i,j,k))
        rs(k) = MAX(R1, qsn(i,j,k)*rho(i,j,k))
        temp(k) = tmk(i,j,k)
        rhof(k) = sqrt(RHO_NOT/rho(i,j,k))
        ze_rain(k) = 1.e-22
        ze_snow(k) = 1.e-22
        ze_graupel(k) = 1.e-22

        if (qra(i,j,k) .gt. R1) then
          lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
          ilamr(k) = 1./lamr
          mvd_r(k) = (3.0 + mu_r + 0.672) * ilamr(k)
          L_qr(k) = .true.
        else
          mvd_r(k) = 50.E-6
          L_qr(k) = .false.
        endif

      enddo

!..Contribution to reflectivity from graupel.
!..The line below for N0_exp should change according to which version WRF
!.. and should match how the intercept parameter is set within
!.. module_mp_thompson.F

      N0_min = gonv_max
      do k = 1, mkzh

!..      N0_exp = 100.0*rho(i,j,k)/rg(k)

         if (Thompson_scheme_version .eq. '3.2') then

           N0_exp = (gonv_max-gonv_min)*0.5D0      &
                  * tanh((0.15E-3-rg(k))/0.15E-3)  &
                  + (gonv_max+gonv_min)*0.5D0

         else if (Thompson_scheme_version .eq. '3.1.9') then

           N0_exp = (gonv_max-gonv_min)*0.5D0                &
                  * tanh((0.01E-3-(rc(k)+rr(k)))/0.75E-3)    &
                  + (gonv_max+gonv_min)*0.5D0

         else if (Thompson_scheme_version .eq. '3.3') then

           if (temp(k).lt.273.15 .and. (rc(k)+rr(k)).gt.1.E-5) then
              xslw1 = 5. + alog10(max(1.E-5, min(1.E-2, (rc(k)+rr(k)))))
           else
              xslw1 = 0.
           endif

           ygra1 = 5. + alog10(max(1.E-5, min(1.E-2, rg(k))))
           zans1 = 3.324 + (3./(5.*xslw1*ygra1               &
                                / (5.*xslw1+1.+0.25*ygra1)+1.+0.25*ygra1))

           N0_exp = 10.**(zans1)
           N0_exp = DMAX1(DBLE(gonv_min), DMIN1(N0_exp, DBLE(gonv_max)))
           N0_min = DMIN1(N0_exp, N0_min)
           N0_exp = N0_min

         else if (Thompson_scheme_version .eq. '3.3.1') then

           if (temp(k).lt.273.15 .and. (rc(k)+rr(k)).gt.1.E-5) then
              xslw1 = 5. + alog10(max(1.E-5, min(1.E-2, (rc(k)+rr(k)))))
           else
              xslw1 = 0.0001
           endif

           ygra1 = 4.302 + alog10(max(5.E-5, min(5.E-2, rg(k))))
           zans1 = 3.565 + (90./(50.*xslw1*ygra1             &
                                 / (5./xslw1+1.+0.25*ygra1)+30.+15.*ygra1))

           N0_exp = 10.**(zans1)
           N0_exp = DMAX1(DBLE(gonv_min), DMIN1(N0_exp, DBLE(gonv_max)))
           N0_min = DMIN1(N0_exp, N0_min)
           N0_exp = N0_min

         else if (Thompson_scheme_version .eq. '3.4.1') then

           if (temp(k).lt.270.65 .and. L_qr(k) .and. mvd_r(k).gt.100.E-6) then
              xslw1 = 4.01 + alog10(mvd_r(k))
           else
              xslw1 = 0.01
           endif

           ygra1 = 4.31 + alog10(max(5.E-5, rg(k)))
           zans1 = 3.1 + (100./(300.*xslw1*ygra1             &
                                 / (10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
   
           N0_exp = 10.**(zans1)
           N0_exp = DMAX1(DBLE(gonv_min), DMIN1(N0_exp, DBLE(gonv_max)))
           N0_min = DMIN1(N0_exp, N0_min)
           N0_exp = N0_min

         else

           print*, 'Thompson_scheme_version = ', Thompson_scheme_version
           print*, 'This version is not supported in dbzcalc2.'
           stop

         endif

         lam_exp = (N0_exp*am_g*cgg(1)/rg(k))**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg(k) = 1./lamg
         N0_g(k) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      enddo
      do k = 1, mkzh
         if (rg(k).gt. R1)                                       &
               ze_graupel(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
                                    * (am_g/rho_i)*(am_g/rho_i)  &
                                    * N0_g(k)*cgg(4)*ilamg(k)**cge(4)
      enddo

!..Contribution to reflectivity from snow.
      do k = 1, mkzh
         if (.not. (rs(k).gt. R1) ) goto 992
         tc0 = MIN(-0.1, temp(k)-273.15)
         smob(k) = rs(k)*oams 
         
!..All other moments based on reference, 2nd moment.  If bm_s.ne.2,
!.. then we must compute actual 2nd moment and use as reference.
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2 = smob(k)
         else
            loga_ = sa(1) + sa(2)*tc0 + sa(3)*bm_s         &
               + sa(4)*tc0*bm_s + sa(5)*tc0*tc0            &
               + sa(6)*bm_s*bm_s + sa(7)*tc0*tc0*bm_s      &
               + sa(8)*tc0*bm_s*bm_s + sa(9)*tc0*tc0*tc0   &
               + sa(10)*bm_s*bm_s*bm_s
            a_ = 10.0**loga_  
            b_ = sb(1) + sb(2)*tc0 + sb(3)*bm_s            &
               + sb(4)*tc0*bm_s + sb(5)*tc0*tc0            &
               + sb(6)*bm_s*bm_s + sb(7)*tc0*tc0*bm_s      &
               + sb(8)*tc0*bm_s*bm_s + sb(9)*tc0*tc0*tc0   &
               + sb(10)*bm_s*bm_s*bm_s
            smo2 = (smob(k)/a_)**(1./b_)
         endif

!..Calculate bm_s+1 (th) moment.  Useful for diameter calcs.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(1)             &
               + sa(4)*tc0*cse(1) + sa(5)*tc0*tc0             &
               + sa(6)*cse(1)*cse(1) + sa(7)*tc0*tc0*cse(1)   &
               + sa(8)*tc0*cse(1)*cse(1) + sa(9)*tc0*tc0*tc0  &
               + sa(10)*cse(1)*cse(1)*cse(1) 
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(1) + sb(4)*tc0*cse(1)  &
              + sb(5)*tc0*tc0 + sb(6)*cse(1)*cse(1)               &
              + sb(7)*tc0*tc0*cse(1) + sb(8)*tc0*cse(1)*cse(1)    &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(1)*cse(1)*cse(1)
         smoc(k) = a_ * smo2**b_

!..Calculate bm_s*2 (th) moment.  Useful for reflectivity.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3)               &
               + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0               &
               + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3)     &
               + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0    &
               + sa(10)*cse(3)*cse(3)*cse(3) 
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3)   &
              + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3)                &
              + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3)     &
              + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)
         smoz(k) = a_ * smo2**b_

         ze_snow(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)             &
                                 * (am_s/rho_i)*(am_s/rho_i)*smoz(k)
 992  continue
      enddo

!..Contribution to reflectivity from rain.
      do k = 1, mkzh
         lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
         ilamr(k) = 1./lamr
         N0_r(k) = nr(k)*org2*lamr**cre(2)
      enddo

      do k = 1, mkzh
         if (rr(k).gt. R1)                                        &
                ze_rain(k) = N0_r(k)*crg(4)*ilamr(k)**cre(4)
      enddo


!+---+-----------------------------------------------------------------+
!..Locate K-level of start of melting (k_0 is level above).
!+---+-----------------------------------------------------------------+ 

      k_0 = mkzh
      melti = .false.
      do k = 2, mkzh
         if ( (temp(k).gt. 273.15) .and. (rr(k).gt. 0.0001e-3)    &
                   .and. ((rs(k-1)+rg(k-1)).gt. 0.01e-3) ) then
            k_0 = MIN(k-1, k_0)
            melti=.true.
            goto 135
         endif
      enddo
 135  continue

!+---+-----------------------------------------------------------------+
!..Special case of melting ice (snow/graupel) particles.  Assume the
!.. ice is surrounded by the liquid water.  Fraction of meltwater is
!.. extremely simple based on amount found above the melting level.
!.. Uses code from Uli Blahak (rayleigh_soak_wetgraupel and supporting
!.. routines).
!+---+-----------------------------------------------------------------+
      
      if ((.not. melti) .or. k_0.ge.mkzh) goto 991

!..Create bins of snow (from min diameter up to 2 cm).
      xDx(1) = D0s*1.0d0
      xDx(nbins+1) = 0.02d0
      do n = 2, nbins
         xDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nbins)               &
                  *DLOG(xDx(nbins+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbins
         Ds(n) = DSQRT(xDx(n)*xDx(n+1))
         dts(n) = xDx(n+1) - xDx(n)
      enddo

!..Create bins of graupel (from min diameter up to 5 cm).
      xDx(1) = D0g*1.0d0
      xDx(nbins+1) = 0.05d0
      do n = 2, nbins
         xDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nbins)              &
                  *DLOG(xDx(nbins+1)/xDx(1)) +DLOG(xDx(1)))
      enddo
      do n = 1, nbins
         Dg(n) = DSQRT(xDx(n)*xDx(n+1))
         dtg(n) = xDx(n+1) - xDx(n)
      enddo

      do k = k_0+1, mkzh
      
!..Reflectivity contributed by melting snow

         if (allow_wet_snow) then

           fmelt_s = DMIN1(1.0d0-rs(k)/rs(k_0), 1.0d0)
           if (fmelt_s.gt.0.01d0 .and. fmelt_s.lt.0.99d0 .and. &
                          rs(k).gt.R1) then
            eta = 0.d0
            oM3 = 1./smoc(k)
            M0 = (smob(k)*oM3)
            Mrat = smob(k)*M0*M0*M0
            slam1 = M0 * Lam0
            slam2 = M0 * Lam1
            do n = 1, nbins
               xx = am_s * Ds(n)**bm_s
               call rayleigh_soak_wetgraupel(xx, DBLE(ocms), DBLE(obms),  &
                     fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar,  &
                     CBACK, mixingrulestring_s, matrixstring_s,           &
                     inclusionstring_s, hoststring_s,                     &
                     hostmatrixstring_s, hostinclusionstring_s,           &
                     PI, rho_w, rho_i, PI5, lamda4)
               f_d = Mrat*(Kap0*DEXP(-slam1*Ds(n))                        &
                     + Kap1*(M0*Ds(n))**mu_s * DEXP(-slam2*Ds(n)))
               eta = eta + f_d * CBACK * simpson(n) * dts(n)
            enddo
            ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
           endif

         endif

!..Reflectivity contributed by melting graupel

! DCD 6/1/11  Wet versus dry graupel is a significant source of uncertainty
!              in reflectivity calculations.  Graupel with a wet surface
!              has much higher reflectivity than graupel with a dry surface.
!              The allow_wet_graupel parameter controls whether the possibility
!              of wet graupel is considered in the reflectivity calculation.

         if (allow_wet_graupel) then

           fmelt_g = DMIN1(1.0d0-rg(k)/rg(k_0), 1.0d0)
           if (fmelt_g.gt.0.01d0 .and. fmelt_g.lt.0.99d0 .and.            &
                          rg(k).gt.R1) then
            eta = 0.d0
            lamg = 1./ilamg(k)
            do n = 1, nbins
               xx = am_g * Dg(n)**bm_g
               call rayleigh_soak_wetgraupel(xx, DBLE(ocmg), DBLE(obmg),  &
                     fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar,  &
                     CBACK, mixingrulestring_g, matrixstring_g,           &
                     inclusionstring_g, hoststring_g,                     &
                     hostmatrixstring_g, hostinclusionstring_g,           &
                     PI, rho_w, rho_i, PI5, lamda4)
               f_d = N0_g(k)*Dg(n)**mu_g * DEXP(-lamg*Dg(n))
               eta = eta + f_d * CBACK * simpson(n) * dtg(n)
            enddo
            ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
           endif

         endif

      enddo

 991  continue

!..Scale answer to typical dBZ.

!     if (i.eq.140 .and. j.eq.112) print*, ' DEBUG: test column'

      do k = 1, mkzh

         dbz(i,j,k) = MAX(dbz(i,j,k),                                     &
                 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.e18))

        if (calibrate_reflectivity) then
            dbz(i,j,k) = MAX(-35.0, dbz(i,j,k) - 3.0)
        endif

!       if (i.eq.140 .and. j.eq.112) then
!        print*, 10.*log10(ze_rain(k)*1.e18),     &
!                10.*log10(ze_snow(k)*1.e18),     &
!                10.*log10(ze_graupel(k)*1.e18),  &
!                dbz(i,j,k)
!       endif

      enddo

!..Reflectivity-weighted terminal velocity (snow, rain, graupel, mix).

     do k = 1, mkzh
       vt_dbz(i,j,k) = 1.E-3
       if (rs(k).gt.R2) then
         xDs = smoc(k) / smob(k)
         Mrat = 1./xDs
         ils1 = 1./(Mrat*Lam0 + fv_s)
         ils2 = 1./(Mrat*Lam1 + fv_s)
!mass wgt           t1_vts = Kap0*csg(4)*ils1**cse(4)
!mass wgt           t2_vts = Kap1*Mrat**mu_s*csg(10)*ils2**cse(10)
         t1_vts = Kap0*csg(5)*ils1**cse(5)
         t2_vts = Kap1*Mrat**mu_s*csg(11)*ils2**cse(11)
         ils1 = 1./(Mrat*Lam0)
         ils2 = 1./(Mrat*Lam1)
         t3_vts = Kap0*csg(6)*ils1**cse(6)
         t4_vts = Kap1*Mrat**mu_s*csg(12)*ils2**cse(12)
!mass wgt           t3_vts = Kap0*csg(1)*ils1**cse(1)
!mass wgt           t4_vts = Kap1*Mrat**mu_s*csg(7)*ils2**cse(7)
         vts_dbz_wt = rhof(k)*av_s * (t1_vts+t2_vts)/(t3_vts+t4_vts)
       else
         vts_dbz_wt = 1.E-3
       endif

       if (rr(k).gt.R2) then
         lamr = 1./ilamr(k)
         vtr_dbz_wt = rhof(k)*av_r*crg(13)*(lamr+fv_r)**(-cre(13))      &
                    / (crg(4)*lamr**(-cre(4)))
       else
         vtr_dbz_wt = 1.E-3
       endif

       if (rg(k).gt.R2) then
         lamg = 1./ilamg(k)
         vtg_dbz_wt = rhof(k)*av_g*cgg(5)*lamg**(-cge(5))               &
                    / (cgg(4)*lamg**(-cge(4)))
       else
         vtg_dbz_wt = 1.E-3
       endif

       vt_dbz(i,j,k) = (vts_dbz_wt*ze_snow(k) + vtr_dbz_wt*ze_rain(k)      &
                        + vtg_dbz_wt*ze_graupel(k))                        &
                        / (ze_rain(k)+ze_snow(k)+ze_graupel(k))

      enddo

!
 1000 continue

      return
      end subroutine dbzcalc2

!+---+-----------------------------------------------------------------+

      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL XX
      DOUBLE PRECISION STP
      PARAMETER (STP = 2.5066282746310005D0)
      DOUBLE PRECISION SER,TMP,X,Y
      INTEGER J
      DOUBLE PRECISION COF(6)
      DATA COF /76.18009172947146D0, -86.50532032941677D0,          &
                       24.01409824083091D0, -1.231739572450155D0,   &
                      .1208650973866179D-2, -.5395239384953D-5/

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL y
!      REAL GAMMLN

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA
!+---+-----------------------------------------------------------------+

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION m_complex_water_ray(lambda,T)

!      Complex refractive Index of Water as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.001,1.0] m; T in [-10.0,30.0] deg C
!      after Ray (1972)

      IMPLICIT NONE
      DOUBLE PRECISION T,lambda
      DOUBLE PRECISION epsinf,epss,epsr,epsi
      DOUBLE PRECISION alpha,lambdas,sigma,nenner
      COMPLEX*16 i
      REAL PI
      PARAMETER (PI = 3.1415926536)

      i = (0d0,1d0)

      epsinf  = 5.27137d0 + 0.02164740d0 * T - 0.00131198d0 * T*T
      epss    = 78.54d+0 * (1.0 - 4.579d-3 * (T - 25.0)        &
              + 1.190d-5 * (T - 25.0)*(T - 25.0)               &
              - 2.800d-8 * (T - 25.0)*(T - 25.0)*(T - 25.0))
      alpha   = -16.8129d0/(T+273.16) + 0.0609265d0
      lambdas = 0.00033836d0 * exp(2513.98d0/(T+273.16)) * 1e-2

      nenner = 1.d0+2.d0*(lambdas/lambda)**(1d0-alpha)*sin(alpha*PI*0.5)  &
             + (lambdas/lambda)**(2d0-2d0*alpha)
      epsr = epsinf + ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)     &
           * sin(alpha*PI*0.5)+1d0)) / nenner
      epsi = ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha)              &
           * cos(alpha*PI*0.5)+0d0)) / nenner                             &
           + lambda*1.25664/1.88496
      
      m_complex_water_ray = SQRT(CMPLX(epsr,-epsi))
      
      return
      END FUNCTION m_complex_water_ray

!+---+-----------------------------------------------------------------+
      
      COMPLEX*16 FUNCTION m_complex_ice_maetzler(lambda,T)
      
!      complex refractive index of ice as function of Temperature T
!      [deg C] and radar wavelength lambda [m]; valid for
!      lambda in [0.0001,30] m; T in [-250.0,0.0] C
!      Original comment from the Matlab-routine of Prof. Maetzler:
!      Function for calculating the relative permittivity of pure ice in
!      the microwave region, according to C. Maetzler, "Microwave
!      properties of ice and snow", in B. Schmitt et al. (eds.) Solar
!      System Ices, Astrophys. and Space Sci. Library, Vol. 227, Kluwer
!      Academic Publishers, Dordrecht, pp. 241-257 (1998). Input:
!      TK = temperature (K), range 20 to 273.15
!      f = frequency in GHz, range 0.01 to 3000
         
      IMPLICIT NONE
      DOUBLE PRECISION T,lambda
      DOUBLE PRECISION f,c,TK,B1,B2,b,deltabeta,betam,beta,theta,alfa

      c = 2.99d8
      TK = T + 273.16
      f = c / lambda * 1d-9

      B1 = 0.0207
      B2 = 1.16d-11
      b = 335.0d0
      deltabeta = EXP(-10.02 + 0.0364*(TK-273.16))
      betam = (B1/TK) * ( EXP(b/TK) / ((EXP(b/TK)-1)**2) ) + B2*f*f
      beta = betam + deltabeta
      theta = 300. / TK - 1.
      alfa = (0.00504d0 + 0.0062d0*theta) * EXP(-22.1d0*theta)
      m_complex_ice_maetzler = 3.1884 + 9.1e-4*(TK-273.16)
      m_complex_ice_maetzler = m_complex_ice_maetzler            &
                             + CMPLX(0.0d0, (alfa/f + beta*f)) 
      m_complex_ice_maetzler = SQRT(CONJG(m_complex_ice_maetzler))
      
      return
      END FUNCTION m_complex_ice_maetzler

!+---+-----------------------------------------------------------------+

      subroutine rayleigh_soak_wetgraupel (x_g, a_geo, b_geo, fmelt,  &
                     meltratio_outside, m_w, m_i, lambda, C_back,     &
                     mixingrule,matrix,inclusion,                     &
                     host,hostmatrix,hostinclusion,                   &
                     PI, rho_w, rho_i, PI5, lamda4)

      IMPLICIT NONE

      DOUBLE PRECISION x_g, a_geo, b_geo, fmelt, lambda,  &
                                     meltratio_outside
      DOUBLE PRECISION C_back
      COMPLEX*16 m_w, m_i
      CHARACTER*(*)  mixingrule, matrix, inclusion,  &
                                     host, hostmatrix, hostinclusion
      REAL PI, rho_w, rho_i
      DOUBLE PRECISION PI5, lamda4

      COMPLEX*16 m_core, m_air
      DOUBLE PRECISION D_large, D_g, rhog, x_w, xw_a, fm, fmgrenz,  &
                       volg, vg, volair, volice, volwater,          &
                       meltratio_outside_grenz, mra
      INTEGER error
!      COMPLEX*16 get_m_mix_nested

!     refractive index of air:
      m_air = (1.0d0,0.0d0)

!     Limiting the degree of melting --- for safety: 
      fm = DMAX1(DMIN1(fmelt, 1.0d0), 0.0d0)
!     Limiting the ratio of (melting on outside)/(melting on inside):
      mra = DMAX1(DMIN1(meltratio_outside, 1.0d0), 0.0d0)

!    ! The relative portion of meltwater melting at outside should increase
!    ! from the given input value (between 0 and 1)
!    ! to 1 as the degree of melting approaches 1,
!    ! so that the melting particle "converges" to a water drop.
!    ! Simplest assumption is linear:
      mra = mra + (1.0d0-mra)*fm

      x_w = x_g * fm

      D_g = a_geo * x_g**b_geo

      if (D_g .ge. 1d-12) then

       vg = PI/6. * D_g**3
       rhog = DMAX1(DMIN1(x_g / vg, DBLE(rho_i)), 10.0d0)
       vg = x_g / rhog

       meltratio_outside_grenz = 1.0d0 - rhog / rho_w

       if (mra .le. meltratio_outside_grenz) then
!       !..In this case, it cannot happen that, during melting, all the
!       !.. air inclusions within the ice particle get filled with
!       !.. meltwater. This only happens at the end of all melting.
        volg = vg * (1.0d0 - mra * fm)
 
       else
!       !..In this case, at some melting degree fm, all the air
!       !.. inclusions get filled with meltwater.
        fmgrenz=(rho_i-rhog)/(mra*rho_i-rhog+rho_i*rhog/rho_w)

        if (fm .le. fmgrenz) then
!        !.. not all air pockets are filled:
         volg = (1.0 - mra * fm) * vg
        else
!        !..all air pockets are filled with meltwater, now the
!        !.. entire ice sceleton melts homogeneously:
         volg = (x_g - x_w) / rho_i + x_w / rho_w
        endif

       endif

       D_large  = (6.0 / PI * volg) ** (1./3.)
       volice = (x_g - x_w) / (volg * rho_i)
       volwater = x_w / (rho_w * volg)
       volair = 1.0 - volice - volwater
      
!      !..complex index of refraction for the ice-air-water mixture
!      !.. of the particle:
       m_core = get_m_mix_nested (m_air, m_i, m_w, volair, volice,       &
                         volwater, mixingrule, host, matrix, inclusion,  &
                         hostmatrix, hostinclusion, error)
       if (error .ne. 0) then
        C_back = 0.0d0
        return
       endif

!      !..Rayleigh-backscattering coefficient of melting particle: 
       C_back = (ABS((m_core**2-1.0d0)/(m_core**2+2.0d0)))**2  &
                * PI5 * D_large**6 / lamda4

      else
       C_back = 0.0d0
      endif

      return
      END subroutine rayleigh_soak_wetgraupel

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION get_m_mix_nested (m_a, m_i, m_w, volair,     &
                     volice, volwater, mixingrule, host, matrix,       &
                     inclusion, hostmatrix, hostinclusion, cumulerror)

      IMPLICIT NONE

      DOUBLE PRECISION volice, volair, volwater
      COMPLEX*16 m_a, m_i, m_w
      CHARACTER*(*) mixingrule, host, matrix,              &
                    inclusion, hostmatrix, hostinclusion
      INTEGER cumulerror

      DOUBLE PRECISION vol1, vol2
      COMPLEX*16 mtmp
      INTEGER error
!      COMPLEX*16 get_m_mix

!     !..Folded: ( (m1 + m2) + m3), where m1,m2,m3 could each be
!     !.. air, ice, or water

      cumulerror = 0
      get_m_mix_nested = CMPLX(1.0d0,0.0d0)

      if (host .eq. 'air') then

       if (matrix .eq. 'air') then
!       write(mp_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!       CALL wrf_debug(150, mp_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volice / MAX(volice+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, 0.0d0, vol1, vol2,       &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
          
        if (hostmatrix .eq. 'air') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,          &
                         volair, (1.0d0-volair), 0.0d0, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'icewater') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a,          &
                         volair, (1.0d0-volair), 0.0d0, mixingrule, &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error
        else
!        write(mp_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',    &
!                          hostmatrix
!        CALL wrf_debug(150, mp_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'ice') then

       if (matrix .eq. 'ice') then
!       write(mp_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!       CALL wrf_debug(150, mp_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volair+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, 0.0d0, vol2,         &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'ice') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,          &
                         (1.0d0-volice), volice, 0.0d0, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airwater') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a,          &
                         (1.0d0-volice), volice, 0.0d0, mixingrule, &
                         'air', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
!        write(mp_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',    &
!                          hostmatrix
!        CALL wrf_debug(150, mp_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'water') then

       if (matrix .eq. 'water') then
!       write(mp_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
!       CALL wrf_debug(150, mp_debug)
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volice+volair,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, vol2, 0.0d0,        &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error

        if (hostmatrix .eq. 'water') then
         get_m_mix_nested = get_m_mix (2.0d0*m_a, mtmp, m_w,             &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule,  &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airice') then
         get_m_mix_nested = get_m_mix (2.0d0*m_a, mtmp, m_w,             &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule,  &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error          
        else
!        write(mp_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ',         &
!                          hostmatrix
!        CALL wrf_debug(150, mp_debug)
         cumulerror = cumulerror + 1
        endif
       endif

      elseif (host .eq. 'none') then

       get_m_mix_nested = get_m_mix (m_a, m_i, m_w,                      &
                       volair, volice, volwater, mixingrule,             &
                       matrix, inclusion, error)
       cumulerror = cumulerror + error
        
      else
!      write(mp_debug,*) 'GET_M_MIX_NESTED: unknown matrix: ', host
!      CALL wrf_debug(150, mp_debug)
       cumulerror = cumulerror + 1
      endif

      IF (cumulerror .ne. 0) THEN
!      write(mp_debug,*) 'GET_M_MIX_NESTED: error encountered'
!      CALL wrf_debug(150, mp_debug)
       get_m_mix_nested = CMPLX(1.0d0,0.0d0)    
      endif

      return
      END FUNCTION get_m_mix_nested

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION get_m_mix (m_a, m_i, m_w, volair, volice,    &
                     volwater, mixingrule, matrix, inclusion, error)

      IMPLICIT NONE

      DOUBLE PRECISION volice, volair, volwater
      COMPLEX*16 m_a, m_i, m_w
      CHARACTER*(*) mixingrule, matrix, inclusion
      INTEGER error
!      COMPLEX*16 m_complex_maxwellgarnett

      error = 0
      get_m_mix = CMPLX(1.0d0,0.0d0)

      if (mixingrule .eq. 'maxwellgarnett') then
       if (matrix .eq. 'ice') then
        get_m_mix = m_complex_maxwellgarnett(volice, volair, volwater,   &
                           m_i, m_a, m_w, inclusion, error)
       elseif (matrix .eq. 'water') then
        get_m_mix = m_complex_maxwellgarnett(volwater, volair, volice,   &
                           m_w, m_a, m_i, inclusion, error)
       elseif (matrix .eq. 'air') then
        get_m_mix = m_complex_maxwellgarnett(volair, volwater, volice,   &
                           m_a, m_w, m_i, inclusion, error)
       else
!       write(mp_debug,*) 'GET_M_MIX: unknown matrix: ', matrix
!       CALL wrf_debug(150, mp_debug)
        error = 1
       endif

      else
!      write(mp_debug,*) 'GET_M_MIX: unknown mixingrule: ', mixingrule
!      CALL wrf_debug(150, mp_debug)
       error = 2
      endif

      if (error .ne. 0) then
!      write(mp_debug,*) 'GET_M_MIX: error encountered'
!      CALL wrf_debug(150, mp_debug)
      endif

      return
      END FUNCTION get_m_mix

!+---+-----------------------------------------------------------------+

      COMPLEX*16 FUNCTION m_complex_maxwellgarnett(vol1, vol2, vol3,    &
                     m1, m2, m3, inclusion, error)

      IMPLICIT NONE

      COMPLEX*16 m1, m2, m3
      DOUBLE PRECISION vol1, vol2, vol3
      CHARACTER*(*) inclusion

      COMPLEX*16 beta2, beta3, m1t, m2t, m3t
      INTEGER error

      error = 0

      if (DABS(vol1+vol2+vol3-1.0d0) .gt. 1d-6) then
!      write(mp_debug,*) 'M_COMPLEX_MAXWELLGARNETT: sum of the ',   &
!             'partial volume fractions is not 1...ERROR'
!      CALL wrf_debug(150, mp_debug)
       m_complex_maxwellgarnett=CMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m1t = m1**2
      m2t = m2**2
      m3t = m3**2

      if (inclusion .eq. 'spherical') then
       beta2 = 3.0d0*m1t/(m2t+2.0d0*m1t)
       beta3 = 3.0d0*m1t/(m3t+2.0d0*m1t)
      elseif (inclusion .eq. 'spheroidal') then
       beta2 = 2.0d0*m1t/(m2t-m1t) * (m2t/(m2t-m1t)*LOG(m2t/m1t)-1.0d0)
       beta3 = 2.0d0*m1t/(m3t-m1t) * (m3t/(m3t-m1t)*LOG(m3t/m1t)-1.0d0)
      else
!      write(mp_debug,*) 'M_COMPLEX_MAXWELLGARNETT: ',         &
!                        'unknown inclusion: ', inclusion
!      CALL wrf_debug(150, mp_debug)
       m_complex_maxwellgarnett=DCMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif

      m_complex_maxwellgarnett =                                          &
       SQRT(((1.0d0-vol2-vol3)*m1t + vol2*beta2*m2t + vol3*beta3*m3t) /   &
       (1.0d0-vol2-vol3+vol2*beta2+vol3*beta3))

      return
      END FUNCTION m_complex_maxwellgarnett

!+---+-----------------------------------------------------------------+
! end of code provided by Greg Thompson 4/4/11
!+---+-----------------------------------------------------------------+

END MODULE module_diag_raprefl
