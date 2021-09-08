#if (NMM_CORE == 1)
MODULE module_diag_cl
CONTAINS
   SUBROUTINE diag_cl_stub
   END SUBROUTINE diag_cl_stub
END MODULE module_diag_cl
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_cl
CONTAINS

   SUBROUTINE clwrf_output_calc(                                      &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,is_restart                                      & ! CLWRF
                     ,clwrfH,wind_int,t2,q2,u10,v10, skintemp         & ! CLWRF
                     ,swrad,swnorm                                    &
                     ,t2clmin,t2clmax,tt2clmin,tt2clmax               & ! CLWRF
                     ,t2clmean,t2clstd                                & ! CLWRF
                     ,q2clmin,q2clmax,tq2clmin,tq2clmax               & ! CLWRF
                     ,q2clmean,q2clstd                                & ! CLWRF
                     ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & ! CLWRF
                     ,u10clmean,v10clmean,spduv10clmean               & ! CLWRF
                     ,u10clstd,v10clstd,spduv10clstd                  & ! CLWRF
                     ,raincclmax,rainncclmax,traincclmax,trainncclmax & ! CLWRF
                     ,raincclmean,rainncclmean,raincclstd,rainncclstd & ! CLWRF
                     ,skintempclmin,skintempclmax                     & ! CLWRF
                     ,tskintempclmin,tskintempclmax                   & ! CLWRF
                     ,skintempclmean,skintempclstd                    & ! CLWRF
                     ,swradmin,swradmax                               &
                     ,tswradmin,tswradmax                             &
                     ,swradmean,swradstd                              &
                     ,swnormmin,swnormmax                             &
                     ,tswnormmin,tswnormmax                           &
                     ,swnormmean,swnormstd                            &
                     ,raincv,rainncv                                  &
                     ,dt,xtime,curr_secs2                             &
                     ,nsteps,nsteps_wind                              &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- curr_secs2    current time in seconds since simulation restart
!
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
! CLWRF-UC May.09 definitions
!-----------
! is_restart: whether if simulation is a restart
! clwrfH: Interval (hour) of accumulation for computations 
! wind_int: Interval (hour) of accumulation for wind
! [var]cl[min/max]: [minimum/maximum] of variable [var] during interval
! t[var]cl[min/max]: Time (minutes) of [minimum/maximum] of variable 
!    [var] during interval 
! [var]clmean: mean of variable [var] during interval
! [var]clstd: standard dev. of variable [var] during interval
!    Variables are written on aux_hist_out7 (established
!    in Registry)
!
!======================================================================

   INTEGER,      INTENT(IN   )                     ::            &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
                                      i_end,j_start,j_end

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           & 
                                      RAINNCV, RAINCV,           &
                                      SKINTEMP, SWRAD, SWNORM 

   REAL,  INTENT(IN   )                            :: DT, XTIME
   REAL,  INTENT(IN   )                            :: curr_secs2

! LOCAL  VAR

   INTEGER                                   :: i,j,k,its,ite,jts,jte,ij
   INTEGER                                   :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER                                   :: prfreq

   REAL                                      :: xtimep
   LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor

!!-------------------
!! CLWRF-UC Nov.09

   CHARACTER (LEN=80)                        :: timestr

   REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
                          INTENT(IN)         :: t2, q2, u10, v10 
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
                          tt2clmax, t2clmean, t2clstd,                            & 
                          q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
                          u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
                          u10clmean, v10clmean, spduv10clmean,                    &
                          u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
                          skintempclmax, tskintempclmin, tskintempclmax,          &
                          skintempclmean, skintempclstd, swradmin, swradmax,      &
                          tswradmin, tswradmax, swradmean, swradstd,              &
                          swnormmin, swnormmax,                                   &
                          tswnormmin, tswnormmax, swnormmean, swnormstd
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: raincclmax, rainncclmax,          &
                          traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
                          raincclstd, rainncclstd 
   REAL, PARAMETER                           :: minimum0= 1000000.,               &
                          maximum0= -1000000. 
   REAL                                      :: value
   INTEGER, INTENT(IN)                       :: clwrfH
   INTEGER, INTENT(IN)                       :: wind_int
   CHARACTER (LEN=1024)                      :: message
   INTEGER, INTENT(INOUT)                    :: nsteps
   INTEGER, INTENT(INOUT)                    :: nsteps_wind
   LOGICAL                                   :: is_restart
! local vars
   REAL                                      :: t273

!-----------------------------------------------------------------

   t273  = 273.

! Initialize [var] values
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
!  IF ( MOD(NINT(XTIME), clwrfH) == 0 ) THEN
!  IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) .AND. (.NOT.is_restart)) THEN
!tgs - NINT in the MOD command was causing crashes with divide by 0.
  IF ((ABS( MOD(curr_secs2/dt,clwrfH*60./dt)) < 0.01) .AND. (.NOT.is_restart)) THEN
!    print *,'1 - initialization - xtime',xtime
    DO ij = 1 , num_tiles
      IF  ( wrf_dm_on_monitor() ) THEN
          CALL wrf_debug(100, 'Re-initializing accumulation arrays')
      ENDIF
      nsteps = 1
      DO j = j_start(ij), j_end(ij)
        DO i = i_start(ij), i_end(ij)
          t2clmin(i,j)=t2(i,j)
          t2clmax(i,j)=t2(i,j)
          t2clmean(i,j)=t2(i,j)
          t2clstd(i,j)=t2(i,j)*t2(i,j)
          q2clmin(i,j)=q2(i,j)
          q2clmax(i,j)=q2(i,j)
          q2clmean(i,j)=q2(i,j)
          q2clstd(i,j)=q2(i,j)*q2(i,j)
!          spduv10clmax(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
!          u10clmean(i,j)=u10(i,j)
!          v10clmean(i,j)=v10(i,j)
!          spduv10clmean(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
!          u10clstd(i,j)=u10(i,j)*u10(i,j)
!          v10clstd(i,j)=v10(i,j)*v10(i,j)
!          spduv10clstd(i,j)=u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)
          raincclmax(i,j)=raincv(i,j)/dt
          rainncclmax(i,j)=rainncv(i,j)/dt
          raincclmean(i,j)=raincv(i,j)/dt
          rainncclmean(i,j)=rainncv(i,j)/dt
          raincclstd(i,j)=(raincv(i,j)/dt)*(raincv(i,j)/dt)
          rainncclstd(i,j)=(rainncv(i,j)/dt)*(rainncv(i,j)/dt)
          skintempclmin(i,j)=skintemp(i,j)
          skintempclmax(i,j)=skintemp(i,j)
          skintempclmean(i,j)=skintemp(i,j)
          skintempclstd(i,j)=skintemp(i,j)*skintemp(i,j)
          swradmin(i,j)=swrad(i,j)
          swradmax(i,j)=swrad(i,j)
          swradmean(i,j)=swrad(i,j)
          swradstd(i,j)=swrad(i,j)*swrad(i,j)
          swnormmin(i,j)=swnorm(i,j)
          swnormmax(i,j)=swnorm(i,j)
          swnormmean(i,j)=swnorm(i,j)
          swnormstd(i,j)=swnorm(i,j)*swnorm(i,j)
        ENDDO
      ENDDO
  ENDDO
    nsteps=clwrfH*60./dt
  ELSE
!    print *,'2 - summation - xtime',xtime
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
!    nsteps=nsteps+1.
    nsteps=clwrfH*60./dt
!      DO j = j_start(ij), j_end(ij)
!        DO i = i_start(ij), i_end(ij)
!     DO j = jps, jpe
!       DO i = ips, ipe
! Temperature
          CALL varstatistics(t2,xtimep,ime-ims+1,jme-jms+1,t2clmin,t2clmax,   &
            tt2clmin,tt2clmax,t2clmean,t2clstd)
! Water vapor mixing ratio
          CALL varstatistics(q2,xtimep,ime-ims+1,jme-jms+1,q2clmin,q2clmax,   &
            tq2clmin,tq2clmax,q2clmean,q2clstd)
! Wind speed
!          CALL varstatisticsWIND(u10,v10,xtimep,ime-ims+1,jme-jms+1,u10clmax, &
!            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
!            spduv10clmean,u10clstd,v10clstd,spduv10clstd)
! Precipitation flux
          CALL varstatisticsMAX(raincv/dt,xtimep,ime-ims+1,jme-jms+1,         &
            raincclmax,traincclmax,raincclmean,raincclstd) 
          CALL varstatisticsMAX(rainncv/dt,xtimep,ime-ims+1,jme-jms+1,        &
            rainncclmax,trainncclmax,rainncclmean,rainncclstd)
! Skin Temperature 
          CALL varstatistics(skintemp,xtimep,ime-ims+1,jme-jms+1,skintempclmin,&
            skintempclmax, tskintempclmin,tskintempclmax,skintempclmean,      &
            skintempclstd)
! SWDOWN - incoming shortwave radiation
          CALL varstatistics(swrad,xtimep,ime-ims+1,jme-jms+1,swradmin,&
            swradmax, tswradmin,tswradmax,swradmean,      &
            swradstd)
! SWNORM - normal incoming shortwave radiation
          CALL varstatistics(swnorm,xtimep,ime-ims+1,jme-jms+1,swnormmin,&
            swnormmax, tswnormmin,tswnormmax,swnormmean,      &
            swnormstd)

!          IF (MOD(NINT(XTIME),clwrfH) == 0) THEN
!          IF (MOD(NINT(XTIME+dt/60.),clwrfH) == 0) THEN
!           IF ((MOD(NINT((curr_secs2+dt)/dt),NINT(clwrfH*60./dt)) == 0)) THEN
           IF (ABS(MOD((curr_secs2+dt)/dt,clwrfH*60./dt)) < 0.01) THEN
!    print *,'3 - averaging - xtime',xtime,curr_secs2,'nsteps=',nsteps
!             IF  ( wrf_dm_on_monitor() ) PRINT *,'nsteps=',nsteps,' xtime:',  &
!               xtime,' clwrfH:',clwrfH 
               t2clmean=t2clmean/nsteps
               t2clstd=SQRT(max(0.,t2clstd/nsteps-t2clmean**2.))
               q2clmean=q2clmean/nsteps
               q2clstd=SQRT(max(0.,q2clstd/nsteps-q2clmean**2.))
!               u10clmean=u10clmean/nsteps
!               v10clmean=v10clmean/nsteps
!               spduv10clmean=spduv10clmean/nsteps
!               u10clstd=SQRT(u10clstd/nsteps-u10clmean**2.)
!               v10clstd=SQRT(v10clstd/nsteps-v10clmean**2.)
!               spduv10clstd=SQRT(spduv10clstd/nsteps-                        &
!                 spduv10clmean**2)
               raincclmean=raincclmean/nsteps
               rainncclmean=rainncclmean/nsteps
               raincclstd=SQRT(max(0.,raincclstd/nsteps-raincclmean**2.))
               rainncclstd=SQRT(max(0.,rainncclstd/nsteps-rainncclmean**2.))
               skintempclmean=skintempclmean/nsteps
               skintempclstd=SQRT(max(0.,skintempclstd/nsteps-skintempclmean**2.))
               swradmean=swradmean/nsteps
               swradstd=SQRT(max(0.,swradstd/nsteps-swradmean**2.))
               swnormmean=swnormmean/nsteps
               swnormstd=SQRT(max(0.,swnormstd/nsteps-swnormmean**2.))
            END IF
!        ENDDO
!      ENDDO
  ENDIF

! wind is done separtely for wind_int
!  IF (( MOD(NINT(curr_secs2/dt),NINT(wind_int*60./dt)) == 0) .AND. (.NOT.is_restart)) THEN
!print*,"curr_secs2=",curr_secs2," dt=",dt,"wind_it*60=",wind_int*60.
!JOE- the second argument in the MOD function was resulting in a FPE - the REAL 
!     function stopped this behavior.
  IF ((ABS( MOD(curr_secs2/dt,REAL(wind_int)*60./dt)) < 0.01) .AND. (.NOT.is_restart)) THEN
!    print *,'1 - initialization wind - xtime',xtime
    DO ij = 1 , num_tiles
      !IF  ( wrf_dm_on_monitor() ) THEN
      !  WRITE(message, *)'Re-initializing accumulation arrays for wind'
      !ENDIF
      DO j = j_start(ij), j_end(ij)
        DO i = i_start(ij), i_end(ij)
          spduv10clmax(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clmean(i,j)=u10(i,j)
          v10clmean(i,j)=v10(i,j)
          spduv10clmean(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clstd(i,j)=u10(i,j)*u10(i,j)
          v10clstd(i,j)=v10(i,j)*v10(i,j)
          spduv10clstd(i,j)=u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)
        ENDDO
      ENDDO
  ENDDO
    nsteps_wind=wind_int*60./dt
  ELSE
!    print *,'2 - summation wind - xtime',xtime
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
    nsteps_wind=wind_int*60./dt
! Wind speed
          CALL varstatisticsWIND(u10,v10,xtimep,ime-ims+1,jme-jms+1,u10clmax, &
            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
            spduv10clmean,u10clstd,v10clstd,spduv10clstd)

!           IF ((MOD(NINT((curr_secs2+dt)/dt),NINT(wind_int*60./dt)) == 0)) THEN
           IF (ABS(MOD((curr_secs2+dt)/dt,wind_int*60./dt)) < 0.01) THEN
!    print *,'3 - averaging wind - xtime',xtime
!             IF  ( wrf_dm_on_monitor() ) PRINT *,'nsteps_wind=',nsteps_wind,' xtime:',  &
!               xtime,' wind_int:',wind_int
               u10clmean=u10clmean/nsteps_wind
               v10clmean=v10clmean/nsteps_wind
               spduv10clmean=spduv10clmean/nsteps_wind
               u10clstd=SQRT(max(0.,u10clstd/nsteps_wind-u10clmean**2.))
               v10clstd=SQRT(max(0.,v10clstd/nsteps_wind-v10clmean**2.))
               spduv10clstd=SQRT(max(0.,spduv10clstd/nsteps_wind-                        &
                 spduv10clmean**2))
           ENDIF
  ENDIF ! wind

!  !$OMP END PARALLEL DO

   END SUBROUTINE clwrf_output_calc

! UC.CLWRF Nov.09
SUBROUTINE varstatisticsWIND(varu, varv, tt, dx, dy, varumax, varvmax,       &
  varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd, varvstd,     & 
  varuvstd) 
! Subroutine to compute variable statistics for a wind somponents 

IMPLICIT NONE

INTEGER                                                        :: i, j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: varu, varv
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varumax,   &
  varvmax, varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd,      & 
  varvstd, varuvstd
REAL                                                           :: varuv

DO i=1,dx
  DO j=1,dy
    varuv=sqrt(varu(i,j)*varu(i,j)+varv(i,j)*varv(i,j))
      IF (varuv > varuvmax(i,j)) THEN
        varumax(i,j)=varu(i,j)
        varvmax(i,j)=varv(i,j)
        varuvmax(i,j)=varuv
        tvaruvmax(i,j)=tt
      END IF
    varuvmean(i,j)=varuvmean(i,j)+varuv
    varuvstd(i,j)=varuvstd(i,j)+varuv**2
  END DO
END DO
varumean=varumean+varu
varvmean=varvmean+varv
varustd=varustd+varu*varu
varvstd=varvstd+varv*varv

END SUBROUTINE varstatisticsWIND

SUBROUTINE varstatisticsMAX(var, tt, dx, dy, varmax, tvarmax, varmean,       &
   varstd)
! Subroutine to compute variable statistics for a max only variable values

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmax,    &
  tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatisticsMAX 

SUBROUTINE varstatistics(var, tt, dx, dy, varmin, varmax, tvarmin, tvarmax,  & 
  varmean, varstd) 
! Subroutine to compute variable statistics

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmin,    &
  varmax, tvarmin, tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) < varmin(i,j)) THEN
      varmin(i,j)=var(i,j)
      tvarmin(i,j)=tt
    END IF
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatistics

END MODULE module_diag_cl
#endif
