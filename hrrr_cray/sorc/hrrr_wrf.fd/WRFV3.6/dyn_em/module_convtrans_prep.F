!------------------------------------------------------------------------
! Original routine by Georg Grell (NOAA-ESRL):
! This module does the following:
!   (1) averages the convective rain (rain_cv) over the time period set by
!       the namelist parameter "convtrans_avglen_m".
!   (2) averages the sub-grid scale clouds predicted by the deep- and
!       shallow-cu schemes (gd_cloud & gd_cloud2, for qc and qi, respectively).
!   (3) computes a cloud fraction, currently based off of Randall and Xu (1996),
!       from the time averaged clouds. This is really only necessary for WRF-Chem.
!   (4) This module is only called when cu_diag == 1
! Adaptive timestepping added by William Gustafson Jr. (PNNL).
! Cloud fraction added by Georg Grell (based on Stu McKeen's (NOAA-ESRL) previous 
! work with the so2so4 routine).
! Shallow-cu option added by Joseph Olson (NOAA-ESRL/CIRES). 
!   - this module is now called after both the deep- and shallow-cu calls.
!------------------------------------------------------------------------

Module module_convtrans_prep
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CONTAINS
subroutine convtrans_prep(gd_cloud,gd_cloud2,gd_cloud_a,         &
     qc_cu,raincv,raincv_a,raincv_b,                             &
     cldfra_dp,moist,p_QV,p_QC,p_qi,T_PHY,P_PHY,num_moist,       &
     gd_cloud2_a,qi_cu,convtrans_avglen_m,                       &
     adapt_step_flag,curr_secs,                                  &
     ktau,dt,cu_phys,shcu_physics,                               &
     ids,ide, jds,jde, kds,kde,                                  &
     ims,ime, jms,jme, kms,kme,                                  &
     its,ite, jts,jte, kts,kte                                   )

  !PARAMETERS FOR RANDALL AND XU (1996) CLOUD FRACTION 
  REAL, PARAMETER  ::  coef_p = 0.25, coef_gamm = 0.49, coef_alph = 100.

  INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,      &
                                 ims,ime, jms,jme, kms,kme,      &
                                 its,ite, jts,jte, kts,kte,      &
                                 p_QV,p_QC,p_qi,num_moist

  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(IN ) :: gd_cloud,gd_cloud2
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       INTENT(IN ) :: t_phy,p_phy
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),       &
       INTENT(IN ) :: moist
  REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                  &
       OPTIONAL,                                                 &
       INTENT(INOUT ) :: gd_cloud_a,gd_cloud2_a,                 &
                         cldfra_dp,                              &
                         qc_cu,qi_cu
  REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL,                 & !JOE: made optional
       INTENT(IN ) :: raincv
  REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL,                 & !JOE: made optional
       INTENT(INOUT ) :: raincv_a,raincv_b
  INTEGER, INTENT(IN) :: ktau,cu_phys,shcu_physics
  INTEGER  :: stepave
  INTEGER, SAVE  :: stepave_count
  REAL, INTENT(IN) :: curr_secs
  REAL, INTENT(IN) :: convtrans_avglen_m, dt
  LOGICAL, INTENT(IN) :: adapt_step_flag

  LOGICAL :: avg_end_flag, first_period_flag
  REAL :: satvp,rhgrid,h2oliq
  real :: pmax,pmin
!================================================================
!
! Determine where we are in relation to the averaging period...
!
!  convtrans_avglen_m = 30.  !Averaging time for convective transport in min.

  stepave=convtrans_avglen_m*60./dt
  avg_end_flag = .false.      !Initially assume we are not at the end
  first_period_flag = .false. !Nor at the beginning
  if( adapt_step_flag ) then
     !If end of period...
     if( curr_secs+real(dt,8)+0.01 >= &
          ( int( curr_secs/real(convtrans_avglen_m*60.,8) + 1_8, 8) &
            *real(convtrans_avglen_m*60.,8) ) ) &
          avg_end_flag = .true.
     if( curr_secs <= real(convtrans_avglen_m*60.,8) ) first_period_flag = .true.
  else
     if( mod(ktau,stepave)==0 ) avg_end_flag = .true.
     if( ktau <= stepave ) first_period_flag = .true.
  end if
!
! Initialize the averaging arrays at the simulation start, but only if the
! deep-cu (precipitating) scheme is used.
!
  IF(ktau.le.1)then
     stepave_count             = 0
  ENDIF
  IF(cu_phys.eq.3 .or. cu_phys.eq.5 .or. cu_phys.eq.93 )then  !JOE:add deep-cu dependency
     if(ktau.le.1)then
        raincv_a(its:ite,jts:jte) = 0.
        raincv_b(its:ite,jts:jte) = 0.
     end if
  ENDIF
  if(present(gd_cloud2_a))then
     if(ktau.le.1) gd_cloud2_a(its:ite,kts:kte,jts:jte)=0.
  end if

  if(present(cldfra_dp))then
     if(ktau.le.1) cldfra_dp(its:ite,kts:kte,jts:jte)=0.
  end if
!
! no time average available in first half hour
!
  IF(cu_phys.eq.3 .or. cu_phys.eq.5 .or. cu_phys.eq.93 )then  !JOE:add deep-cu dependency
     if( first_period_flag )then
        do j=jts,jte
           do i=its,ite
              raincv_b(i,j)=raincv(i,j)
           enddo
        enddo
     end if
  ENDIF
!
! build time average, and stored in raincv_b to be used by convective transport routine
!
  stepave_count = stepave_count+1
  IF(cu_phys.eq.3 .or. cu_phys.eq.5 .or. cu_phys.eq.93 )then  !JOE:add deep-cu dependency
     do j=jts,jte
        do i=its,ite
           raincv_a(i,j)=raincv_a(i,j)+raincv(i,j)
        enddo
     enddo
     if( avg_end_flag )then
        do j=jts,jte
           do i=its,ite
              raincv_b(i,j)=raincv_a(i,j)/real(stepave_count)
              raincv_a(i,j)=0.
           enddo
        enddo
     end if
  ENDIF
!
! do the same for convective parameterization cloud water mix ratio,
! currently only for cu_physics=3,5, used by both photolysis and atmospheric radiation
!
  if(cu_phys.eq.3 .or. cu_phys.eq.5 .or. &
     cu_phys.eq.93 .or. shcu_physics.eq.1)then
! if(config_flags%cu_physics == GDSCHEME  .OR. &
!    config_flags%cu_physics == GFSCHEME  .OR. &
!    config_flags%cu_physics == GFSCHEME ) THEN
!    pmax=maxval(gd_cloud)
!    pmin=maxval(gd_cloud2)
!    print *,pmax,pmin
     if( first_period_flag )then
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
!JOE                 gd_cloud_b(i,k,j)=gd_cloud(i,k,j)
!JOE                 gd_cloud2_b(i,k,j)=gd_cloud2(i,k,j)
                 qc_cu(i,k,j)=gd_cloud(i,k,j)
                 qi_cu(i,k,j)=gd_cloud2(i,k,j)
              enddo
           enddo
        enddo
     end if   !stepave/first period
!
!
     do j=jts,jte
        do k=kts,kte
           do i=its,ite
              gd_cloud_a(i,k,j)=gd_cloud_a(i,k,j)+gd_cloud(i,k,j)
              gd_cloud2_a(i,k,j)=gd_cloud2_a(i,k,j)+gd_cloud2(i,k,j)
           enddo
        enddo
     enddo
     if( avg_end_flag )then
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
!JOE                 gd_cloud_b(i,k,j)=gd_cloud_a(i,k,j)/real(stepave_count)
                 qc_cu(i,k,j)=gd_cloud_a(i,k,j)/real(stepave_count)
                 gd_cloud_a(i,k,j)=0.
!JOE                 gd_cloud2_b(i,k,j)=gd_cloud2_a(i,k,j)/real(stepave_count)
                 qi_cu(i,k,j)=gd_cloud2_a(i,k,j)/real(stepave_count)
                 gd_cloud2_a(i,k,j)=0.
              enddo
           enddo
        enddo
!    pmax=maxval(gd_cloud_b)
!    pmin=maxval(gd_cloud2_b)
!    print *,'avg_end_flag ',pmax,pmin
     end if !stepave/endflag
  end if ! cu_rad_feedback
!
! Clear the accumulator counter if we just finished an average...
!
if( avg_end_flag ) stepave_count = 0

! Siebesma et al., JAS, Vol. 60, no. 10, 1201-1219, 2003 (based on LES comparisons with 
! trade-wind cumulus from BOMEX)
! SAM: Note units of liquid water and saturation vapor pressure must be in g/kg
!      within the Siebesma et al. cloud fraction scheme
if( first_period_flag .or. avg_end_flag )then
   IF(cu_phys.eq.3 .or. cu_phys.eq.5 .or. cu_phys.eq.93 )then !JOE: only for deep-cu
      do j=jts,jte
         do k=kts,kte
            do i=its,ite
               cldfra_dp(i,k,j)=0.
!               if(gd_cloud_b(i,k,j).gt.0  .or. gd_cloud2_b(i,k,j).gt.0)then

               if(p_qc.gt.1 .and. p_qi.le.1)then
                  satvp = 3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
                         (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))
                  rhgrid = max(.1,MIN( .95, moist(i,k,j,p_qv) /satvp))
!JOE                  h2oliq=1000.*(gd_cloud_b(i,k,j) + moist(i,k,j,p_qc))
                  h2oliq=1000.*(qc_cu(i,k,j) + moist(i,k,j,p_qc))
                  satvp=1000.*satvp
                  cldfra_dp(i,k,j)=(1.-exp(-coef_alph*h2oliq/((1.-rhgrid)*satvp)**coef_gamm)) &
                                  *(rhgrid**coef_p)
                  cldfra_dp(i,k,j)=max(0.,MIN(1.,cldfra_dp(i,k,j)))
                  if(moist(i,k,j,p_qc).eq.0)cldfra_dp(i,k,j)=cldfra_dp(i,k,j)*.2
               endif
               if(p_qc.gt.1 .and. p_qi.gt.1)then
                  satvp = 3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
                         (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))
                  rhgrid = max(.1,MIN( .95, moist(i,k,j,p_qv) /satvp))
!JOE                  h2oliq=1000.*(gd_cloud_b(i,k,j) + moist(i,k,j,p_qc) + &
!JOE                                gd_cloud2_b(i,k,j) + moist(i,k,j,p_qi))
                  h2oliq=1000.*(qc_cu(i,k,j) + moist(i,k,j,p_qc) + &
                                qi_cu(i,k,j) + moist(i,k,j,p_qi))
                  satvp=1000.*satvp
                  cldfra_dp(i,k,j)=(1.-exp(-coef_alph*h2oliq/((1.-rhgrid)*satvp)**coef_gamm)) &
                                  *(rhgrid**coef_p)
                  cldfra_dp(i,k,j)=max(0.,MIN(1.,cldfra_dp(i,k,j)))
                  if(moist(i,k,j,p_qc).eq.0 .and. moist(i,k,j,p_qi).eq.0) &
                       cldfra_dp(i,k,j)=cldfra_dp(i,k,j)*.2
               endif

!               endif
            enddo
         enddo
      enddo
   endif
endif

END subroutine convtrans_prep

END MODULE MODULE_CONVTRANS_prep

