!WRF:MODEL_LAYER:PHYSICS
!

MODULE module_shcu_gfo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                  !
!     This shallow-convective parameterization is built to attempt !
!     a smooth transition to cloud resolving scales as resolution  !
!     is increased. Different scale-aware functions have been      !
!     implemented, taken from Honnert et al (2011) and Shin and    !
!     Hong (2014). Currently using Honnert et al.'s similarity     !
!     functions.                                                   !
!                                                                  !
!     The original form of this scheme is non-precipitating,       !
!     only mixing heat and water vapor. This scheme has            !
!     also been designed to parameterize the sub-grid scale        !
!     shallow-cumulus clouds, which can interact with the short-   !
!     and long-wave radiation when "cu_rad_feedback" is set to     !
!     true and cu_diag set to 2.                                   !
!                                                                  !
!     This scheme produces a cloud fraction, many are being tested !
!     for both land and water. A hidden parameter (below) called   !
!     "cldfra_sh_opt" chooses between two different forms.         !
!                                                                  !
!     Please excuse the messy code, as this scheme is in a state   !
!     rapid development and testing.                               !
!     Details will be described in a forthcoming paper by          !
!     Grell and Freitas (2015).                                    !
!                                                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CONTAINS

!-------------------------------------------------------------
   SUBROUTINE GFO_SHCU_DRV(                                     &
               DT,itimestep,DX                                  &
              ,rho                                              &
!JOE               ,RAINCV,PRATEC                                   &
              ,U,V,t                                            &
!JOE               ,W                                               &
              ,q,p,pi                                           &
              ,dz8w,p8w,XLV,G,r_v                               &
!JOE              ,htop,hbot                                        &
!JOE              ,CU_ACT_FLAG                                      &
!JOE          ,warm_rain                                            &
!JOE              ,MASS_FLUX
              ,HT,hfx,qfx,XLAND,gsw                             &
!JOE          ,edt_out                                              &
              ,QC_SHCU,QI_SHCU                                  &
              ,kpbl,k22_shallow,kbcon_shallow                   &
              ,ktop_shallow,xmb_shallow,Psig                    &
!JOE              ,imomentum                                        &
              ,ichoice,cu_physics                               &
              ,cldfra_sh                                        &
              ,ids,ide, jds,jde, kds,kde                        &
              ,ims,ime, jms,jme, kms,kme                        &
              ,its,ite, jts,jte, kts,kte                        &
              ,periodic_x,periodic_y                            &
              ,RQVSHTEN                                         &
!JOE              ,RQCSHTEN,RQISHTEN                                &
!JOE              ,RQVFTEN,RTHFTEN                                  &
              ,RTHSHTEN,RTHRATEN                                &
              ,rqvblten,rthblten                                &
              ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS         &
                                                                )
!-------------------------------------------------------------
   IMPLICIT NONE
! autoconv, 1=old c0, 2=berry c0
      integer, parameter :: autoconv=1
!aeroevap, 1=old,2=?, 3=average
      integer, parameter :: aeroevap=1
      integer, parameter :: training=0
      integer, parameter :: use_excess=0
      integer, parameter :: use_excess_sh=0
      integer, parameter :: maxiens=1
      integer, parameter :: maxens=1
      integer, parameter :: maxens2=1
      integer, parameter :: maxens3=16
      integer, parameter :: ensdim=16
      real, parameter :: ccnclean=250.
      real, parameter :: aodccn=0.1
      real, parameter :: beta=0.02
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                               &
                                  ids,ide, jds,jde, kds,kde,    & 
                                  ims,ime, jms,jme, kms,kme,    & 
                                  its,ite, jts,jte, kts,kte
   LOGICAL periodic_x,periodic_y
               integer, parameter  :: ens4_spread = 3 ! max(3,cugd_avedx)
               integer, parameter  :: ens4=ens4_spread*ens4_spread

   integer, intent (in   )              :: ichoice,cu_physics
  
   INTEGER,      INTENT(IN   ) :: ITIMESTEP !JOE, & !cugd_avedx, &
!JOE   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV, R_v, G
   REAL,         INTENT(IN   ) :: DT, DX
   REAL, PARAMETER :: CP=1004.6

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                          U,    &
                                                          V,    &
!JOE                                                          W,    &
                                                         pi,    &
                                                          t,    &
                                                          q,    &
                                                          p,    &
                                                       dz8w,    &
                                                        p8w,    &
                                                        rho
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          OPTIONAL, INTENT(INOUT   ) ::                         &
                                                    QC_SHCU,    &
                                                    QI_SHCU,    &
                                                   cldfra_sh

!JOE: subgrid scale cloud coefficient (moved here from convtrans_prep)
   REAL :: Cshcu  !made land/water dependent below

!JOE: add option to switch cloud fraction calculations:
   INTEGER, PARAMETER :: cldfra_sh_opt = 1  ! 0: KF-eta
                                            ! 1: Randal & Xu

   REAL, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: hfx,qfx,GSW,HT,XLAND
   INTEGER, DIMENSION( ims:ime , jms:jme ),INTENT(IN) :: KPBL
   INTEGER, DIMENSION( ims:ime , jms:jme ),INTENT(INOUT) ::     &
                                                    k22_shallow,& 
                                                  kbcon_shallow,&
                                                   ktop_shallow

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::       &
                                                    xmb_shallow,&
                                                           Psig

!
! Optionals
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         OPTIONAL,                                              &
         INTENT(INOUT) ::                                       &
                                                   RTHSHTEN,    &
                                                   RQVSHTEN,    &
                                                   RQVBLTEN,    &
                                                   RTHBLTEN,    &
                                                   RTHRATEN!,    &
!                                                   RQCSHTEN,    &
!                                                   RQISHTEN
!
! Flags relating to the optional tendency arrays declared above
! Models that carry the optional tendencies will provdide the
! optional arguments at compile time; these flags all the model
! to determine at run-time whether a particular tracer is in
! use or not.
!
   LOGICAL, OPTIONAL ::                                      &
                                                   F_QV      &
                                                  ,F_QC      &
                                                  ,F_QR      &
                                                  ,F_QI      &
                                                  ,F_QS

! LOCAL VARS
   real,    dimension (its:ite,kts:kte) ::                    &
        phh,subm,cupclws,dhdt,outts,outqs,outqcs,zagl
   real,    dimension (its:ite)         ::                    &
        ztexec,zqexec,pret, ter11, aa0,fp,xlandi,Psig1d,gsw1d
   integer, dimension (its:ite) ::                            &
        ierr,ierrs,kpbli,k22s,kbcons,ktops

   integer :: iens,ibeg,iend,jbeg,jend,n,nn,ens4n
   integer :: ibegh,iendh,jbegh,jendh
   integer :: ibegc,iendc,jbegc,jendc
   real :: temp
   real :: PTEN,PQEN,PAPH,ZRHO,PAHFS,PQHFL,ZKHVFL,ZWS,PGEOH

!JOE:add for cldfra_sh
   REAL :: xcldfra, UMF_new, dcf
   INTEGER :: ktop_dcf, kbcon_dcf

!
! basic environmental input includes windspeed (us,vs), and a flag (aaeq) 
! to turn off convection for this call only and at that particular gridpoint
!
     real,    dimension (its:ite,kts:kte) ::                    &
        zo,T2d,q2d,PO,P2d,US,VS,rhoi,tn,qo,tshall,qshall
     real, dimension (its:ite)            ::                    &
        ccn,Z1,PSUR,AAEQ,umean,vmean,pmean,xmbs

  !PARAMETERS FOR RANDALL AND XU (1996) CLOUD FRACTION
  REAL, PARAMETER  ::  coef_p = 0.25, coef_gamm = 0.49, coef_alph = 100.
  !Other variables needed for Randall & Xu
  REAL :: satvp,rhgrid,h2oliq

   INTEGER :: i,j,k,ICLDCK,ipr,jpr
   REAL    :: tcrit,tscl_KF,dp,dq,sub_spread,subcenter
   INTEGER :: itf,jtf,ktf,iss,jss,nbegin,nend
   INTEGER :: high_resolution
   REAL    :: rkbcon,rktop        !-lxz
   character*50 :: ierrc(its:ite)
   character*50 :: ierrcs(its:ite)

  ! A. Betts for shallow convection: suggestion for the KF timescale < DELTAX  / 25 m/s
   tscl_kf=dx/25.
   ccn(its:ite)=1500.
  !
   high_resolution=0
   subcenter=0.
   iens=1
   ipr=0 !639
   jpr=0 !141
   IF ( periodic_x ) THEN
      ibeg=max(its,ids)
      iend=min(ite,ide-1)
      ibegc=max(its,ids)
      iendc=min(ite,ide-1)
   ELSE
      ibeg=max(its,ids)
      iend=min(ite,ide-1)
      ibegc=max(its,ids+4)
      iendc=min(ite,ide-5)
   END IF
   IF ( periodic_y ) THEN
      jbeg=max(jts,jds)
      jend=min(jte,jde-1)
      jbegc=max(jts,jds)
      jendc=min(jte,jde-1)
   ELSE
      jbeg=max(jts,jds)
      jend=min(jte,jde-1)
      jbegc=max(jts,jds+4)
      jendc=min(jte,jde-5)
   END IF

   itf=MIN(ite,ide-1)
   ktf=MIN(kte,kde-1)
   jtf=MIN(jte,jde-1)

   do j=jts,jte
   do i=its,ite
     k22_shallow(i,j)=0
     kbcon_shallow(i,j)=0
     ktop_shallow(i,j)=0
     xmb_shallow(i,j)=0.
     Psig(i,j)=0.
   enddo
   enddo

   tcrit=258.

!----------------------
!  BEGIN J-LOOP
!----------------------
   DO 100 J = jts,jte

      DO I= its,ite
        ierrc(i)=" "
        ierrcs(i)=" "
        ierr(i)=0
        ierrs(i)=0
        xmbs(i)=0.
        Psig1d(i)=0.
        k22s(i)=0
        kbcons(i)=0
        ktops(i)=0
        xlandi(i)=xland(i,j)
      ENDDO

      !  put hydrostatic pressure on half levels
      DO K=kts,kte
        DO I=ITS,ITE
          phh(i,k) = p(i,k,j)
          cldfra_sh(i,k,j)=0.0
        ENDDO
      ENDDO

!ipr= 33 !78
!jpr= 17 !110
      DO I=ITS,ITE
         PSUR(I)=p8w(I,1,J)*.01
!        PSUR(I)=p(I,1,J)*.01
         TER11(I)=max(0.,HT(i,j))
         ZTEXEC(i) = 0.
         ZQEXEC(i) = 0.
         aaeq(i)=0.
         pret(i)=0.
         umean(i)=0.
         vmean(i)=0.
         pmean(i)=0.
         kpbli(i)=kpbl(i,j)
         GSW1D(i)=GSW(i,j)
         zo(i,kts)=ter11(i)+.5*dz8w(i,1,j)
         zagl(i,kts)=.5*dz8w(i,1,j)
         DO K=kts+1,kte  
            !height at half-sigma levels (middle of layer), including terrain height.
            zo(i,k)=zo(i,k-1)+.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
            !height at half-sigma levels (middle of layer), above ground level.
            zagl(i,k)=zagl(i,k-1)+.5*(dz8w(i,k-1,j)+dz8w(i,k,j))
         enddo
      ENDDO
!    if(j.eq.jpr .and. (ipr.gt.its .and. ipr.lt.itf))write(0,*)psur(ipr),ter11(ipr),kpbli(ipr)
      DO K=kts,kte
        DO I=ITS,ITE
          po(i,k)=phh(i,k)*.01
          subm(i,k)=0.
          P2d(I,K)=PO(i,k)
          rhoi(i,k)=rho(i,k,j)
          US(I,K) =u(i,k,j)
          VS(I,K) =v(i,k,j)
          T2d(I,K)=t(i,k,j)
          q2d(I,K)=q(i,k,j)
          IF(Q2d(I,K).LT.1.E-08)Q2d(I,K)=1.E-08
          OUTQCs(I,K)=0.
          OUTTS(I,K)=0.
          OUTQS(I,K)=0.
          cupclws(i,k)=0.
!JOE         TN(I,K)=t2d(i,k)+(RTHFTEN(i,k,j)+RTHRATEN(i,k,j)+RTHBLTEN(i,k,j)) &
!JOE                          *pi(i,k,j)*dt
!JOE         QO(I,K)=q2d(i,k)+(RQVFTEN(i,k,j)+RQVBLTEN(i,k,j))*dt
          TN(I,K)=t2d(i,k) + (RTHRATEN(i,k,j)+RTHBLTEN(i,k,j)) &
                          *pi(i,k,j)*dt
          QO(I,K)=q2d(i,k) + RQVBLTEN(i,k,j)*dt
          TSHALL(I,K)=t2d(i,k)+RTHBLTEN(i,k,j)*pi(i,k,j)*dt
          DHDT(I,K)=cp*RTHBLTEN(i,k,j)*pi(i,k,j)+ XLV*RQVBLTEN(i,k,j)
          QSHALL(I,K)=q2d(i,k)+RQVBLTEN(i,k,j)*dt
          IF(QO(I,K).LT.1.E-08)QO(I,K)=1.E-08
          IF(QSHALL(I,K).LT.1.E-09)QSHALL(I,K)=1.E-09
          QC_SHCU(i,k,j)=0.0
          QI_SHCU(i,k,j)=0.0
        ENDDO
      ENDDO
      if(use_excess.gt.0 .or. use_excess_sh.gt.0)then
        DO I=ITS,ITE
          ZRHO  = 100.*psur(i)/(287.04*(t2d(i,1)*(1.+0.608*q2d(i,1))))
          !- LE and H fluxes 
          PAHFS=-hfx(i,j) 
          PQHFL=-qfx(i,j)/xlv 
 
          !- buoyancy flux (H+LE)
          ZKHVFL= (PAHFS/1004.64+0.608*t2d(i,1)*PQHFL)/ZRHO
          !- height of the 1st level
          PGEOH = zo(i,1)-ht(i,j)
          !-convective-scale velocity w*
          ZWS = max(0.,0.001-1.5*0.41*ZKHVFL*PGEOH/T2D(i,1))

          if(ZWS > TINY(PGEOH)) then
            !-convective-scale velocity w*
            ZWS = 1.2*ZWS**.3333
            !- temperature excess 
            ZTEXEC(i)     = MAX(-1.5*PAHFS/(ZRHO*ZWS*1004.64),0.0)
            !- moisture  excess
            ZQEXEC(i)     = MAX(-1.5*PQHFL/(ZRHO*ZWS),0.)
          endif
        ENDDO
      endif  ! use_excess
      do k=  kts+1,kte-1
        DO I = its,ite
          if((p2d(i,1)-p2d(i,k)).gt.150.and.p2d(i,k).gt.300)then
            dp=-.5*(p2d(i,k+1)-p2d(i,k-1))
            umean(i)=umean(i)+us(i,k)*dp
            vmean(i)=vmean(i)+vs(i,k)*dp
            pmean(i)=pmean(i)+dp
          endif
        enddo
      enddo

! this turns off shallow convection when deep convection is active
! NOTE: pret is modified in the deep cu scheme. pret will have to be passed 
!       in to this module. Currently, pret = 0. (initialized above)
      do i=its,ite
        if(pret(i).gt.0.)then
          ierrs(i)=1
          aaeq(i)=-100.
        endif
      enddo
!
!---- CALL SHALLOW CUMULUS PARAMETERIZATION
!
   call gfo_shcu(xmbs,zo,OUTQCs,J,AAEQ,T2D,Q2D,TER11,                    &
              Tshall,Qshall,P2d,PRET,P2d,OUTTS,OUTQS,DT,itimestep,PSUR,US,VS,    &
              TCRIT,ztexec,zqexec,ccn,ccnclean,rhoi,dx,dhdt, &
              kpbli,kbcons,ktops,cupclws,k22s,               &   !-lxz
              xlandi,gsw1d,tscl_kf,Psig1d,                   &
              xlv,r_v,cp,g,ichoice,0,0,ierrs,ierrcs,         &
              autoconv,itf,jtf,ktf,                          &
              use_excess_sh,its,ite, jts,jte, kts,kte        &
                                                              )


!   if(j.lt.jbegc.or.j.gt.jendc)go to 100

        DO I=its,ite !JOE:ibegc,iendc
           xmb_shallow(i,j)=xmbs(i)
           Psig(i,j)=Psig1d(i)
           k22_shallow(i,j)=k22s(i)
           kbcon_shallow(i,j)=kbcons(i)
           if(kbcons(i)>= 35)print*,"kbcon=",kbcons(i)," xmbs=",xmbs(i)," k22=",k22s(i)
           ktop_shallow(i,j)=ktops(i)
           if(ktops(i)>= kte-5)print*,"ktops=",ktops(i)," xmbs=",xmbs(i)," k22=",k22s(i)
        ENDDO

        DO I=its,ite !JOE:ibegc,iendc
           DO K=kts,kte
              RTHSHTEN(I,K,J)=outts(i,k)/pi(i,k,j)
              RQVSHTEN(I,K,J)=outqs(i,k)
           ENDDO
        ENDDO

! Extra print-outs for 1D off-line testing
! shallow scheme needs t2d,q2d,p,z,tshall,qshall,dhdt!
!              if(i.eq.151 .and. j.eq.443)then
!                write(23,*)i,j,ter11(i),psur(i),xmb_shallow(i,j)
!                do k=kts,kte
!                   write(23,123)k,zo(i,k),p2d(i,k),t2d(i,k),q2d(i,k),dhdt(i,k),tshall(i,k),qshall(i,k)
!                enddo
!                do k=kts,kte
!                   write(23,124)k,outts(i,k)*86400.,outqs(i,k)*86400.*xlv/cp,cupclws(i,k)
!                enddo
! 123                        format(1x,i2,1x,f6.0,1x,f6.1,1x,5e13.5)
! 124                        format(1x,i2,1x,2(1x,f6.2),e13.5)

!JOE       IF(PRESENT(RQISHTEN).AND.PRESENT(RQCSHTEN))THEN
!JOE              IF (F_QI) THEN
           DO K=kts,kte
              DO I=its,ite !JOE:ibegc,iendc
                 IF(xland(i,j).lt.1.5) then     !LAND (xland == 1)
                    Cshcu=0.20
                 ELSE                           !WATER
                    Cshcu=0.10
                 ENDIF
                 if(t2d(i,k).lt.258.)then
                    !JOE-shcu currently only mixes heat & water vapor at the moment
                    !IF(PRESENT(RQISHTEN)) RQISHTEN(I,K,J)=outqcs(i,k)
                    !IF(PRESENT(RQCSHTEN)) RQCSHTEN(I,K,J)=0.
                    IF(PRESENT(QI_SHCU)) QI_SHCU(I,K,J)=Cshcu*cupclws(i,k)
                    IF(PRESENT(QC_SHCU)) QC_SHCU(I,K,J)=0.0
                 else
                    !IF(PRESENT(RQISHTEN)) RQISHTEN(I,K,J)=0.
                    !IF(PRESENT(RQCSHTEN)) RQCSHTEN(I,K,J)=outqcs(i,k)
                    IF(PRESENT(QC_SHCU)) QC_SHCU(I,K,J)=Cshcu*cupclws(i,k)
                    IF(PRESENT(QI_SHCU)) QI_SHCU(I,K,J)=0.0
                 endif
              ENDDO
           ENDDO

!JOE: ADD CLDFRA_SH
        DO I=its,ite
           kbcon_dcf=MAX(MIN(kbcon_shallow(i,j),kte),kts)
           ktop_dcf =MAX(MIN(ktop_shallow(i,j),kte),kts)
           DO K=kbcon_shallow(i,j),ktop_shallow(i,j)
              IF(k>=kts .AND. k<=kte)THEN
                IF (cldfra_sh_opt == 0) THEN
                  ! Taken from KF-ETA, but modified to have less variation
                  ! above 3 km grid spacing.
                  UMF_new = xmb_shallow(i,j)/(MIN(DX*3000.,9.0E6)/1.0E6)           !UMF(K)/DXSQ
                  !xcldfra = 0.21*alog(1.+(500.*UMF_new))
                  xcldfra = 0.30*alog(1.+(500.*UMF_new))
                  ! cldfra must taper off with height, like that in Zhang and Klein (2013)
                  dcf=1.0 - MIN((MAX(zagl(i,k)       -zagl(i,kbcon_dcf), 0.)) /&
                                (MAX(zagl(i,ktop_dcf)-zagl(i,kbcon_dcf), 1.)),1.0)**2.
                  dcf=MIN(MAX(dcf,0.2),1.0)
                  xcldfra = MAX(0.01,xcldfra*dcf)
                  IF(k>=kts .AND. k<=kte)cldfra_sh(i,k,j) = MIN(0.40,xcldfra)

                ELSEIF(cldfra_sh_opt == 1) THEN
                  !Randall and Xu
                  if(F_qc .and. .not. F_qi)then
                    satvp = 3.80*exp(17.27*(t2d(i,k)-273.)/ &
                         (t2d(i,k)-36.))/(.01*p2d(i,k))
                    rhgrid = max(.1,MIN( .95, q2d(i,k) /satvp))
                    h2oliq=1000.*qc_shcu(i,k,j)
                    satvp=1000.*satvp
                    cldfra_sh(i,k,j)=(1.-exp(-coef_alph*h2oliq/&
                          ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
                    cldfra_sh(i,k,j)=max(0.04,MIN(1.,cldfra_sh(i,k,j)))
                  elseif(F_qc .and. F_qi)then
                    satvp = 3.80*exp(17.27*(t2d(i,k)-273.)/ &
                        (t2d(i,k)-36.))/(.01*p2d(i,k))
                    rhgrid = max(.1,MIN( .95, q2d(i,k) /satvp))
                    h2oliq=1000.*(qc_shcu(i,k,j) + qi_shcu(i,k,j))
                    satvp=1000.*satvp
                    cldfra_sh(i,k,j)=(1.-exp(-coef_alph*h2oliq/&
                            ((1.-rhgrid)*satvp)**coef_gamm))*(rhgrid**coef_p)
                    cldfra_sh(i,k,j)=max(0.04,MIN(1.,cldfra_sh(i,k,j)))
                  endif
                ENDIF
              ENDIF
           ENDDO
         ENDDO

 100    continue !end j-loop

   END SUBROUTINE GFO_SHCU_DRV

!=======================================================================
   SUBROUTINE cup_kbcon(ierrc,cap_inc,iloop,k22,kbcon,he_cup,hes_cup, &
              hkb,ierr,kbmax,p_cup,cap_max,                           &
              xl,cp,ztexec,zqexec,use_excess,                         &
              itf,jtf,ktf,                                            &
              its,ite, jts,jte, kts,kte                        )

   IMPLICIT NONE
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,                                                   &
        its,ite, jts,jte, kts,kte 
  ! 
  ! 
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        he_cup,hes_cup,p_cup
     real,    dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        ztexec,zqexec,cap_max,cap_inc
     real,intent (in   )                  ::                           &
        xl,cp
     real,    dimension (its:ite)                                      &
        ,intent (inout   )                ::                           &
        hkb
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbmax
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        kbcon,k22,ierr
     integer                                                           &
        ,intent (in   )                   ::                           &
        iloop,use_excess
     character*50 :: ierrc(its:ite)

!
!  local variables in this routine
!

     integer                              ::                           &
        i,k,k1,k2
     real                                 ::                           &
        pbcdif,plus,hetest
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
       DO 27 i=its,ite !itf
      kbcon(i)=1
      IF(ierr(I).ne.0)GO TO 27
      KBCON(I)=K22(I)+1
      if(iloop.eq.5)KBCON(I)=K22(I)
      GO TO 32
 31   CONTINUE
      KBCON(I)=KBCON(I)+1
      IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        GO TO 27
      ENDIF
 32   CONTINUE
      hetest=hkb(i) !HE_cup(I,K22(I))
      if(iloop.eq.5)then
       hetest=HKB(I)
!      do k=1,k22(i)
!        hetest=max(hetest,he_cup(i,k))
!      enddo
      endif
      IF(HETEST.LT.HES_cup(I,KBCON(I)))then
!       write(0,*)'htest',k22(i),kbcon(i),HETEST,-P_cup(I,KBCON(I))+P_cup(I,K22(I))
        GO TO 31
      endif

!     cloud base pressure and max moist static energy pressure
!     i.e., the depth (in mb) of the layer of negative buoyancy
      if(KBCON(I)-K22(I).eq.1)go to 27
      if(iloop.eq.5 .and. (KBCON(I)-K22(I)).eq.0)go to 27
      PBCDIF=-P_cup(I,KBCON(I))+P_cup(I,K22(I))
      plus=max(25.,cap_max(i)-float(iloop-1)*cap_inc(i))
      if(iloop.eq.4)plus=cap_max(i)
!
! for shallow convection, if cap_max is greater than 25, it is the pressure at pbltop
      if(iloop.eq.5)plus=25.
      if(iloop.eq.5.and.cap_max(i).gt.25)pbcdif=-P_cup(I,KBCON(I))+cap_max(i)
      IF(PBCDIF.GT.plus)THEN
!       write(0,*)'htest',k22(i),kbcon(i),plus,-P_cup(I,KBCON(I))+P_cup(I,K22(I))
        K22(I)=K22(I)+1
        KBCON(I)=K22(I)+1
         if(use_excess == 2) then
             k1=max(1,k22(i)-1)
             k2=max(1,min(kbcon(i)-1,k22(i)+1))  !kbcon(i)-1
             k2=k22(i)+1
             hkb(i)=sum(he_cup(i,k1:k2))/float(k2-k1+1)+(xl*zqexec(i)+cp*ztexec(i))/float(k2-k1+1)
        else if(use_excess <= 1)then
             hkb(i)=he_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
        endif  ! excess

        if(iloop.eq.5)KBCON(I)=K22(I)
        IF(KBCON(I).GT.KBMAX(i)+2)THEN
         if(iloop.ne.4)then
                ierr(i)=3
                ierrc(i)="could not find reasonable kbcon in cup_kbcon"
         endif
        GO TO 27
      ENDIF
        GO TO 32
      ENDIF
 27   CONTINUE

   END SUBROUTINE cup_kbcon

!======================================================================
   SUBROUTINE cup_MAXIMI(ARRAY,KS,KE,MAXX,ierr,              &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input:
!

   ! only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
         itf,jtf,ktf,                                    &
         its,ite, jts,jte, kts,kte
  ! ARRAY input array
  ! MAXX output array with return values
  ! kt output array of levels
  ! ks & ke  check-range
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
         array
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
         ierr,ke
     integer                                                           &
        ,intent (in   )                   ::                           &
         ks
     integer, dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
         maxx
     real,    dimension (its:ite)         ::                           &
         x
     real                                 ::                           &
         xar
     integer                              ::                           &
         i,k

      DO 200 i=its,ite !itf
        MAXX(I)=KS
        if(ierr(i).eq.0)then
          X(I)=ARRAY(I,KS)
!
          DO 100 K=KS,KE(i)
            XAR=ARRAY(I,K)
            IF(XAR.GE.X(I)) THEN
              X(I)=XAR
              MAXX(I)=K
            ENDIF
 100      CONTINUE
        endif
 200  CONTINUE

   END SUBROUTINE cup_MAXIMI

!=====================================================================
   SUBROUTINE cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup,       &
              kbcon,ktop,ierr,                               &
              itf,jtf,ktf,                     &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE
!
!  on input
!
!  only local wrf dimensions are need as of now in this routine

     integer                                                           &
        ,intent (in   )                   ::                           &
        itf,jtf,ktf,                                     &
        its,ite, jts,jte, kts,kte
  ! aa0 cloud work function
  ! gamma_cup = gamma on model cloud levels
  ! t_cup = temperature (Kelvin) on model cloud levels
  ! dby = buoancy term
  ! zu= normalized updraft mass flux
  ! z = heights of model levels 
  ! ierr error value, maybe modified in this routine
  !
     real,    dimension (its:ite,kts:kte)                              &
        ,intent (in   )                   ::                           &
        z,zu,gamma_cup,t_cup,dby
     integer, dimension (its:ite)                                      &
        ,intent (in   )                   ::                           &
        kbcon,ktop
!
! input and output
!
     integer, dimension (its:ite)                                      &
        ,intent (inout)                   ::                           &
        ierr
     real,    dimension (its:ite)                                      &
        ,intent (out  )                   ::                           &
        aa0
!
!  local variables in this routine
!
     integer                              ::                           &
        i,k
     real                                 ::                           &
        dz,da
!
        do i=its,ite
          aa0(i)=0.
        enddo
        DO 100 k=kts+1,kte
        DO 100 i=its,ite
         IF(ierr(i).ne.0)GO TO 100
         IF(K.LE.KBCON(I))GO TO 100
         IF(K.Gt.KTOP(I))GO TO 100
         DZ=Z(I,K)-Z(I,K-1)
         da=zu(i,k)*DZ*(9.81/(1004.*( &
                (T_cup(I,K)))))*DBY(I,K-1)/ &
             (1.+GAMMA_CUP(I,K))
         IF(K.eq.KTOP(I).and.da.le.0.)go to 100
         AA0(I)=AA0(I)+da
         if(aa0(i).lt.0.)aa0(i)=0.
100     continue

   END SUBROUTINE cup_up_aa0

!=========================================================================
   SUBROUTINE gfo_shcu_init(RTHSHTEN,RQVSHTEN,                &
                        !JOE: RQCSHTEN,RQISHTEN,                   &
                        QC_SHCU,QI_SHCU,cldfra_sh,           &
                        restart,                             &
                        P_QC,P_QI,P_FIRST_SCALAR,            &
                        allowed_to_read,                     &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        its, ite, jts, jte, kts, kte         )
!--------------------------------------------------------------------
   IMPLICIT NONE
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_FIRST_SCALAR, P_QI, P_QC

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(OUT) :: &
                                                          RTHSHTEN, &
                                                          RQVSHTEN !, &
                                                          !RQCSHTEN, &
                                                          !RQISHTEN

   REAL,     DIMENSION( ims:ime , kms:kme , jms:jme ) , &
        OPTIONAL,    INTENT(INOUT) :: QC_SHCU,QI_SHCU,cldfra_sh

   INTEGER :: i, j, k, itf, jtf, ktf

   jtf=min0(jte,jde-1)
   ktf=min0(kte,kde-1)
   itf=min0(ite,ide-1)

   IF(.not.restart)THEN
      DO j=jts,jtf
      DO k=kts,ktf
      DO i=its,itf
         RTHSHTEN(i,k,j)=0.
         RQVSHTEN(i,k,j)=0.
         IF(PRESENT(cldfra_sh))cldfra_sh(i,k,j)=0.
         IF(PRESENT(qc_shcu))qc_shcu(i,k,j)=0.
         IF(PRESENT(qi_shcu))qi_shcu(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

!      IF (P_QC .ge. P_FIRST_SCALAR) THEN
!         DO j=jts,jtf
!         DO k=kts,ktf
!         DO i=its,itf
!            RQCSHTEN(i,k,j)=0.
!         ENDDO
!         ENDDO
!         ENDDO
!      ENDIF

!      IF (P_QI .ge. P_FIRST_SCALAR) THEN
!         DO j=jts,jtf
!         DO k=kts,ktf
!         DO i=its,itf
!            RQISHTEN(i,k,j)=0.
!         ENDDO
!         ENDDO
!         ENDDO
!      ENDIF

   ENDIF

   END SUBROUTINE gfo_shcu_init

!=============================================================================

   SUBROUTINE gfo_shcu(xmb_out,zo,OUTQC,J,AAEQ,T,Q,Z1,       &
              TN,QO,PO,PRE,P,OUTT,OUTQ,DTIME,ktau,PSUR,US,VS,&
              TCRIT,                                         &
              ztexec,zqexec,ccn,ccnclean,rho,dx,dhdt,        &
              kpbl,kbcon,ktop,cupclws,k22,                   &   !-lxz
              xland,gsw,tscl_kf,Psig,                        &
              xl,rv,cp,g,ichoice,ipr,jpr,ierr,ierrc,         &
              autoconv,itf,jtf,ktf,                          &
              use_excess,its,ite, jts,jte, kts,kte           &
                                                )

   IMPLICIT NONE

     integer,intent (in   )               ::                 &
        autoconv,itf,jtf,ktf,ktau,use_excess,                &
        its,ite, jts,jte, kts,kte,ipr,jpr
     integer, intent (in   )              ::                 &
        j,ichoice
  !
     real,    dimension (its:ite),intent (in   ) ::  gsw
  ! outtem = output temp tendency (per s)
  ! outq   = output q tendency (per s)
  ! outqc  = output qc tendency (per s)
  ! pre    = output precip
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (inout  )                   ::               &
        cupclws,OUTT,OUTQ,OUTQC
     real,    dimension (its:ite)                            &
        ,intent (out  )                   ::                 &
        pre,xmb_out,Psig
     integer,    dimension (its:ite)                         &
        ,intent (out  )                   ::                 &
        kbcon,ktop,k22
     integer,    dimension (its:ite)                         &
        ,intent (in  )                   ::                  &
        kpbl
  !
  ! basic environmental input includes moisture convergence (mconv)
  ! omega (omeg), windspeed (us,vs), and a flag (aaeq) to turn off
  ! convection for this call only and at that particular gridpoint
  !
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (in   )                   ::                 &
        rho,T,PO,P,US,VS,tn,dhdt
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (inout)                   ::                 &
         Q,QO
     real, dimension (its:ite)                               &
        ,intent (in   )                   ::                 &
        ztexec,zqexec,ccn,Z1,PSUR,AAEQ,xland
       
     real                                                    &
        ,intent (in   )                   ::                 &
        tscl_kf,dx,ccnclean,dtime,tcrit,xl,cp,rv,g

!
!***************** the following are your basic environmental
!                  variables. They carry a "_cup" if they are
!                  on model cloud levels (staggered). They carry
!                  an "o"-ending (z becomes zo), if they are the forced
!                  variables. They are preceded by x (z becomes xz)
!                  to indicate modification by some typ of cloud
!
  ! z           = heights of model levels
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! p           = environmental pressure
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! z_cup       = heights of model cloud levels
  ! q_cup       = environmental q on model cloud levels
  ! qes_cup     = saturation q on model cloud levels
  ! t_cup       = temperature (Kelvin) on model cloud levels
  ! p_cup       = environmental pressure
  ! he_cup = moist static energy on model cloud levels
  ! hes_cup = saturation moist static energy on model cloud levels
  ! gamma_cup = gamma on model cloud levels
!
!
  ! hcd = moist static energy in downdraft
  ! dby = buoancy term
  ! entr = entrainment rate
  ! hcd = h in model cloud
  ! bu = buoancy term
  ! gamma_cup = gamma on model cloud levels
  ! qcd = cloud q (including liquid water) after entrainment
  ! qrch = saturation q in cloud
  ! pwd = evaporate at that level
  ! pwev = total normalized integrated evaoprate (I2)
  ! entr= entrainment rate
  ! z1 = terrain elevation
  ! jmin = downdraft originating level
  ! kdet = level above ground where downdraft start detraining
  ! psur        = surface pressure
  ! pr_ens = precipitation ensemble
  ! xf_ens = mass flux ensembles
  ! massfln = downdraft mass flux ensembles used in next timestep
  ! omeg = omega from large scale model
  ! mconv = moisture convergence from large scale model
  ! zd      = downdraft normalized mass flux
  ! zu      = updraft normalized mass flux
  ! dir     = "storm motion"
  ! mbdt    = arbitrary numerical parameter
  ! dtime   = dt over which forcing is applied
  ! iact_gr_old = flag to tell where convection was active
  ! kbcon       = LFC of parcel from k22
  ! k22         = updraft originating level
  ! icoic       = flag if only want one closure (usually set to zero!)
  ! ktop = cloud top (output)
  ! xmb    = total base mass flux
  ! hc = cloud moist static energy
  ! hkb = moist static energy at originating level

     real,    dimension (its:ite,kts:kte) ::                           &
        entr_rate_2d,mentrd_rate_2d,he,hes,qes,z,                      &
        heo,heso,qeso,zo,                                              &
        xhe,xhes,xqes,xz,xt,xq,                                        &

        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,      &
        qeso_cup,qo_cup,heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,     &
        tn_cup,                                                        &
        xqes_cup,xq_cup,xhe_cup,xhes_cup,xz_cup,xp_cup,xgamma_cup,     &
        xt_cup,                                                        &

        xlamue,dby,qc,qrcd,pwd,pw,hcd,qcd,dbyd,hc,qrc,zu,zd,clw_all,   &
        dbyo,qco,qrcdo,pwdo,pwo,hcdo,qcdo,dbydo,hco,qrco,zuo,zdo,      &
        xdby,xqc,xqrcd,xpwd,xpw,xhcd,xqcd,xhc,xqrc,xzu,xzd,            &

  ! cd  = detrainment function for updraft
  ! cdd = detrainment function for downdraft
  ! dellat = change of temperature per unit mass flux of cloud ensemble
  ! dellaq = change of q per unit mass flux of cloud ensemble
  ! dellaqc = change of qc per unit mass flux of cloud ensemble

        cd,cdd,DELLAH,DELLAQ,DELLAT,DELLAQC,dsubt,dsubh,dsubq,subt,subq

  ! aa0 cloud work function for downdraft
  ! edt epsilon
  ! aa0     = cloud work function without forcing effects
  ! aa1     = cloud work function with forcing effects
  ! xaa0    = cloud work function with cloud effects (ensemble dependent)
  ! edt     = epsilon

     real,    dimension (its:ite) ::                                   &
       edt,edto,edtx,AA1,AA0,XAA0,HKB,                                 &
       HKBO,XHKB,QKB,QKBO,                                             &
       xmbmax,XMB,XPWAV,XPWEV,PWAV,PWEV,PWAVO,                         &
       PWEVO,BU,BUD,BUO,cap_max,xland1,                                &
       cap_max_increment,closure_n,psum,psumh,sig,zuhe
     integer,    dimension (its:ite) ::                                &
       kzdown,KDET,KB,JMIN,kstabi,kstabm,K22x,                         &   !-lxz
       KBCONx,KBx,KTOPx,ierr,ierr2,ierr3,KBMAX

     integer                              ::                           &
       nall,iedt,nens,nens3,ki,I,K,KK,iresult
     real                                 ::                           &
      day,dz,dzo,mbdt,entr_rate,radius,entrd_rate,mentrd_rate,         &
      zcutdown,edtmax,edtmin,depth_min,depth_max,zkbmax,z_detr,zktop,  &
      massfld,dh,cap_maxs,trash,frh,xlamdd,fsum

     real :: detdo1,detdo2,entdo,dp,subin,detdo,entup,                 &
      detup,subdown,entdoj,entupk,detupk,totmas
     real :: power_entr,zustart,zufinal,dzm1,dzp1

     integer :: tun_lim,jprnt,k1,k2,kbegzu,kfinalzu,kstart,jmini,      &
                levadj
!JOE:added for making tun_lim independent of model vertical resolution
     REAL :: tun_limz
!JOE: add scale-aware proprties
     REAL :: dxdh
!added above (out)     REAL,    dimension (its:ite) :: Psig

     logical :: keep_going
     real xff_shal(9),blqe,xkshal
     character*50 :: ierrc(its:ite)
     real,    dimension (its:ite,kts:kte) ::                           &
       up_massentr,up_massdetr,dd_massentr,dd_massdetr                 &
      ,up_massentro,up_massdetro,dd_massentro,dd_massdetro
     real,    dimension (kts:kte) :: smth
      zustart=.1
      zufinal=1.
      levadj=4
      power_entr=2.
      day=86400.
      do i=its,ite !itf
        xmb_out(i)=0.
        xland1(i)=1.
        if(xland(i).gt.1.5)xland1(i)=0.
        cap_max_increment(i)=25.
        ierrc(i)=" "
      enddo
!
!--- initial entrainment rate (these may be changed later on in the
!--- program
!
      entr_rate =.2/200.
      !tun_lim is now a function of the model grid layer depths, caluclated below
      !tun_lim=7
      tun_limz = 1500. !(meters)
      
!
!--- initial detrainmentrates
!
      do k=kts,kte !ktf
      do i=its,ite !itf
        up_massentro(i,k)=0.
        up_massdetro(i,k)=0.
        z(i,k)=zo(i,k)
        xz(i,k)=zo(i,k)
        qrco(i,k)=0.
        cd(i,k)=1.*entr_rate
        dellaqc(i,k)=0.
        cupclws(i,k)=0.
      enddo
      enddo
!
!--- max/min allowed value for epsilon (ratio downdraft base mass flux/updraft
!
!--- minimum depth (m), shcu clouds must have
!
      depth_min=50.
!
!--- maximum depth (m), shcu clouds can have
!
      depth_max=3000.
!
!--- maximum depth (mb) of capping 
!--- inversion (larger cap = no convection)
!
      cap_maxs=25.
      DO i=its,ite !itf
        kbmax(i)=1
        aa0(i)=0.
        aa1(i)=0.
      enddo
      do i=its,ite !itf
        cap_max(i)=cap_maxs
        iresult=0
      enddo
!
!--- max height(m) above PBL (top) where updraft air can originate
!
!      zkbmax=4000.
      zkbmax=500.
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(z,qes,he,hes,t,q,p,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env(zo,qeso,heo,heso,tn,qo,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)

!
!--- environmental values on cloud levels
!
      call cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,he_cup, &
           hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
      call cup_env_clev(tn,qeso,qo,heo,heso,zo,po,qeso_cup,qo_cup, &
           heo_cup,heso_cup,zo_cup,po_cup,gammao_cup,tn_cup,psur,  &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)

      do i=its,ite !itf
        if(ierr(i).eq.0)then
          if(aaeq(i).lt.-0.1)then
            ierr(i)=20
          endif
!
          do k=kts,kte !ktf
!JOE&GEORG - 20140522 - set max originating updraft level max = PBLH + 500 m
            if(zo_cup(i,k) .gt. zkbmax+z1(i)+zo_cup(i,MAX(kpbl(i),1)))then
              kbmax(i)=k
              go to 25
            endif
          enddo
 25   continue
!
          kbmax(i)=min(kbmax(i),kte-4)
        endif
      enddo
!
!------- DETERMINE LEVEL WITH HIGHEST MOIST STATIC ENERGY CONTENT - K22
!
      CALL cup_MAXIMI(HEO_CUP,3,KBMAX,K22,ierr, &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
       DO 36 i=its,ite !itf
         if(kpbl(i).gt.5)cap_max(i)=po_cup(i,MAX(kpbl(i),1))
         IF(ierr(I).eq.0.)THEN
           IF(K22(I).GT.KBMAX(i))then
             ierr(i)=2
             ierrc(i)="could not find k22"
           endif
           if(kpbl(i).gt.5)then !bring in PBLH to make this in meters
             k22(i)=kpbl(i)
             ierr(i)=0
             ierrc(i)="reset to zero because of kpbl"
           endif
         else
           ierrc(i)="why here? "
         endif
!      if(j.eq.jpr .and. i.eq.ipr)write(0,*)'initial k22 = ',k22(ipr),kpbl(i)
 36   CONTINUE
!
!--- DETERMINE THE LEVEL OF CONVECTIVE CLOUD BASE  - KBCON
!
      do i=its,ite !itf
        IF(ierr(I).eq.0.)THEN
          if(use_excess == 2) then
             k1=max(1,k22(i)-1)
             k2=k22(i)+1
             hkb(i) =sum(he_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
             hkbo(i)=sum(heo_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
             qkbo(i)=sum(qo_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)
!            write(0,*)sum(heo_cup(i,k1:k2))/float(k2-k1+1),heo_cup(i,k1),heo(i,k1:k2)
          else if(use_excess <= 1) then
             hkb(i)=he_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
             hkbo(i)=heo_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
             qkbo(i)=qo_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i))
          endif  ! excess
          do k=1,max(1,k22(i))
            hkb(i)=max(hkb(i),he_cup(i,k))
            hkbo(i)=max(hkbo(i),heo_cup(i,k))
            qkbo(i)=max(qkbo(i),qo_cup(i,k))
          enddo
        endif ! ierr
        !20140725 - possible closure fix?
        !if(use_excess >= 0) then
        !     hkbo(i)=hkbo(i)+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
        !     ! do not cause artifical cooling
        !     do k=1,k22(i)
        !       he_cup(i,k)=min(hes_cup(i,k),hkb(i))
        !       heo_cup(i,k)=min(heso_cup(i,k),hkbo(i))
        !       qo_cup(i,k)=min(qeso_cup(i,k),qkbo(i))
        !       ! do not go past saturation
        !       if(hkb(i).gt.hes_cup(i,k))hkb(i)=hes_cup(i,k)
        !       if(hkbo(i).gt.heso_cup(i,k))hkbo(i)=heso_cup(i,k)
        !       if(qkbo(i).gt.qes_cup(i,k))qkb(i)=qeso_cup(i,k)
        !     enddo
        !endif  ! excess
      enddo !i-loop
      DO i=its,ite
        !JOE-Georg and Saulo's new idea:
        do k=kts,kte
          dbyo(i,k)=hkb(i)-heso_cup(i,k)
        enddo
        do k=kts,kte
          !JOE-adding limit to depth of shcu clouds
          if(dbyo(i,k).lt.0 .OR. z(i,k) > depth_max + 1500.)then
            ktop(i)=k-1
            go to 441
          endif
        enddo
 441       continue
      enddo !i-loop

      call cup_kbcon(ierrc,cap_max_increment,5,k22,kbcon,heo_cup,heso_cup, &
           hkbo,ierr,kbmax,po_cup,cap_max, &
           xl,cp,ztexec,zqexec,use_excess,       &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- increase detrainment in stable layers
!
      DO 887 i=its,ite
         IF(ierr(I).eq.0.)THEN
            if(kbcon(i).gt.kte-4)then
               ierr(i)=231
               go to 887
            endif
            !JOE: initialize tun_lim before calculation
            tun_lim=0
            do k=kts,kte
               frh = min(qo_cup(i,k)/qeso_cup(i,k),1.)
               entr_rate_2d(i,k)=entr_rate*(1.3-frh)
               cd(i,k)=entr_rate_2d(i,k)
               !JOE: add height-dependent calculation of tun_lim
               IF(k > kbcon(i))then
                  !print*,"IN GF SHCU; k,kbcon=",k,kbcon(i)
                  IF(z(i,k)-z(i,kbcon(i)) < tun_limz) THEN
                     tun_lim=tun_lim+1
                     !print*,"IN GF SHCU; k,kbcon=",k,kbcon(i)
                     !print*,"IN GF SHCU; tun_lim=",tun_lim
                  ENDIF
               ENDIF
            enddo
            !tun_lim=MAX(4,MIN(tun_lim,ktf-kbcon(i)-4))
            tun_lim=MAX(3,min(tun_lim,ktop(i)-kbcon(i)))
            !print*,"IN GF SHCU; end tun_lim=",tun_lim

            zuhe(i)=zustart
            kstart=1
            frh=(zufinal-zustart)/((float(kbcon(i))**power_entr)-(float(kstart)**power_entr))
            dh=zuhe(i)-frh*(float(kstart)**power_entr)
            ! BELOW CLOUD
            do k=kstart,kbcon(i)-1
               dz=z_cup(i,k+1)-z_cup(i,k)
               cd(i,k)=0.
               entr_rate_2d(i,k)=((frh*(float((k+1))**power_entr)+dh)/zuhe(i)-1.+cd(i,k)*dz)/dz
               zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
!              if(i.eq.ipr.and.j.eq.jpr)write(0,*)'entr = ',k,entr_rate_2d(i,k),dh,frh,zuhe(i),dz
            enddo
            frh=-(0.1-zuhe(i))/((float(kbcon(i)+tun_lim)**power_entr)-(float(kbcon(i)-1)**power_entr))
            dh=zuhe(i)+frh*(float(kbcon(i))**power_entr)
            ! WITHIN THE CLOUD
            do k=kbcon(i),kbcon(i)+tun_lim
               dz=z_cup(i,k+1)-z_cup(i,k)
               cd(i,k)=-((-frh*(float((k+1))**power_entr)+dh)/zuhe(i)-1.-entr_rate_2d(i,k)*dz)/dz
               zuhe(i)=zuhe(i)+entr_rate_2d(i,k)*dz*zuhe(i)-cd(i,k)*dz*zuhe(i)
!              if(i.eq.ipr.and.j.eq.jpr)write(0,*)'entr = ',k,entr_rate_2d(i,k),cd(i,k),zuhe(i)
            enddo
            ! ABOVE THE CLOUD
!            do k=kbcon(i)+4+1,kte !ktf
            do k=kbcon(i)+tun_lim+1,kte !ktf
               entr_rate_2d(i,k)=0.
               cd(i,k)=0.
            enddo

        ENDIF
 887  enddo

!JOE-20140414
      ktop(its:ite)=0
!
! calculate mass entrainment and detrainment
!
      do k=kts,kte !ktf
      do i=its,ite !itf
         hc(i,k)=0.
         DBY(I,K)=0.
         hco(i,k)=0.
         DBYo(I,K)=0.
      enddo
      enddo
      do i=its,ite !itf
       IF(ierr(I).eq.0.)THEN
         do k=1,kbcon(i)-1
            hc(i,k)=hkb(i)
            hco(i,k)=hkbo(i)
            qco(i,k)=qkbo(i)
         enddo
         k=kbcon(i)
         hc(i,k)=hkb(i)
         qco(i,k)=qkbo(i)
         DBY(I,Kbcon(i))=Hkb(I)-HES_cup(I,K)
         hco(i,k)=hkbo(i)
         DBYo(I,Kbcon(i))=Hkbo(I)-HESo_cup(I,K)
         trash=QESo_cup(I,K)+(1./XL)*(GAMMAo_cup(i,k) &
              /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
         qrco(i,k)=max(0.,qco(i,k)-trash)
       endif ! ierr
      enddo
!
!
      do 42 i=its,ite !itf
         if(ierr(i).eq.0)then
         zu(i,1)=zustart
         zuo(i,1)=zustart
!    mass entrainment and detrinament is defined on model levels
         do k=2,kte-1 !kbcon(i)+4 ! ktf-1
           dz=zo_cup(i,k)-zo_cup(i,k-1)
           up_massentro(i,k-1)=entr_rate_2d(i,k-1)*dz*zuo(i,k-1)
           up_massdetro(i,k-1)=cd(i,k-1)*dz*zuo(i,k-1)
           zuo(i,k)=zuo(i,k-1)+up_massentro(i,k-1)-up_massdetro(i,k-1)
           if(zuo(i,k).lt.0.05)then
             zuo(i,k)=.05
             up_massdetro(i,k-1)=zuo(i,k-1)-.05  + up_massentro(i,k-1)
             cd(i,k-1)=up_massdetro(i,k-1)/dz/zuo(i,k-1)
           endif
           zu(i,k)=zuo(i,k)
           up_massentr(i,k-1)=up_massentro(i,k-1)
           up_massdetr(i,k-1)=up_massdetro(i,k-1)
!          zu(i,k)=max(0.01,zu(i,k-1)+up_massentr(i,k-1)-up_massdetr(i,k-1))
         enddo
         do k=kbcon(i)+1,kte-1
           hc(i,k)=(hc(i,k-1)*zu(i,k-1)-.5*up_massdetr(i,k-1)*hc(i,k-1)+ &
                         up_massentr(i,k-1)*he(i,k-1))   /            &
                         (zu(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
           dby(i,k)=hc(i,k)-hes_cup(i,k)
           hco(i,k)=(hco(i,k-1)*zuo(i,k-1)-.5*up_massdetro(i,k-1)*hco(i,k-1)+ &
                         up_massentro(i,k-1)*heo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
           dbyo(i,k)=hco(i,k)-heso_cup(i,k)
         enddo
         do k=kbcon(i)+1,kte !ktf
           !JOE-adding limit to depth of shcu clouds
           if(dbyo(i,k).lt.0 .OR. z(i,k)-z(i,kbcon(i)) > depth_max) then
              ktop(i)=k-1
              go to 41
           endif
         enddo
41       continue
         if(ktop(i).lt.kbcon(i)+1)then
            ierr(i)=5
            ierrc(i)='ktop is less than kbcon+1'
            go to 42
         endif
         if(ktop(i).gt.kte-2)then
            ierr(i)=5
            ierrc(i)="ktop is larger than kte-2"
            go to 42
         endif

!JOE: add scale-aware factor (Psig) here, taken from Honnert et al. (2011, JAS)
!                  and/or from Hyeyum Hailey Shin and Song-You Hong (2013, JAS)
         Psig(i)=1.0
         dxdh=MAX(dx,10.)/MAX(z(i,MAX(ktop(i),1)),500.)
         ! Honnert et al. 2011, TKE in PBL
         !Psig(i)= ((dxdh**2) + 0.07*(dxdh**0.667))/((dxdh**2) + (3./21.)*(dxdh**0.67) + (3./42.))
         ! Honnert et al. 2011, TKE in entrainment layer
         Psig(i)= ((dxdh**2) + (4./21.)*(dxdh**0.667))/((dxdh**2) + (3./20.)*(dxdh**0.67) + (7./21.))
         ! Honnert et al. 2011, TKE in cumulus
         !Psig(i)= ((dxdh**2) + 1.67*(dxdh**1.4))/((dxdh**2) +1.66*(dxdh**1.4) + 0.2)

         ! Honnert et al. 2011, w'q' in PBL
         !Psig(i)= 0.5 + 0.5*((dxdh**2) + 0.03*(dxdh**1.4) - (4./13.))/((dxdh**2) + 0.03*(dxdh**1.4) + (4./13.))
         ! Honnert et al. 2011, w'q' in cumulus
         !Psig(i)= ((dxdh**2) - 0.07*(dxdh**1.4))/((dxdh**2) -0.07*(dxdh**1.4) + 0.02)

         ! Honnert et al. 2011, q'q' in PBL
         !Psig(i)= 0.5 + 0.5*((dxdh**2) + 0.25*(dxdh**0.667) -0.73)/((dxdh**2) -0.03*(dxdh**0.667) + 0.73)
         ! Honnert et al. 2011, q'q' in cumulus
         !Psig(i)= ((dxdh**2) - 0.34*(dxdh**1.4))/((dxdh**2) - 0.35*(dxdh**1.4) + 0.37)

         ! Hyeyum Hailey Shin and Song-You Hong 2013, TKE in entrainment zone
         !Psig(i)= ((dxdh**2) + 0.145*(dxdh**0.667))/((dxdh**2) +0.172*(dxdh**0.667) + 0.170)
         ! Hyeyum Hailey Shin and Song-You Hong 2013, w'theta' in entrainment zone
         !Psig(i)= 0.5 + 0.5*((dxdh**2) - 0.112*(dxdh**0.25) -0.071)/((dxdh**2) + 0.054*(dxdh**0.25) + 0.10)

         !print*,"in shcu_gf; dx, dxdh, Psig(i)=",dx,dxdh,Psig(i)
         !If(Psig(i) < 0.000001 .OR. Psig(i) > 0.99999999)print*,"in shcu_gf; dx, dxdh, Psig(i)=",dx,dxdh,Psig(i) 
         If(Psig(i) > 1.0) Psig(i)=1.0
         If(Psig(i) < 0.0) Psig(i)=0.0
!JOE-end scale aware functions

         do k=kbcon(i)+1,ktop(i)
           trash=QESo_cup(I,K)+(1./XL)*(GAMMAo_cup(i,k) &
              /(1.+GAMMAo_cup(i,k)))*DBYo(I,K)
           qco(i,k)=   (qco(i,k-1)*zuo(i,k-1)-.5*up_massdetr(i,k-1)* qco(i,k-1)+ &
                         up_massentr(i,k-1)*qo(i,k-1))   /            &
                         (zuo(i,k-1)-.5*up_massdetr(i,k-1)+up_massentr(i,k-1))
           qrco(i,k)=max(0.,qco(i,k)-trash)
!05 DEC 2013:          cupclws(i,k)=qrco(i,k)*.1
           cupclws(i,k)=qrco(i,k)*.5
         enddo
         do k=ktop(i)+1,kte !ktf
           HC(i,K)=hes_cup(i,k)
           HCo(i,K)=heso_cup(i,k)
           DBY(I,K)=0.
           DBYo(I,K)=0.
           zu(i,k)=0.
           zuo(i,k)=0.
           cd(i,k)=0.
           entr_rate_2d(i,k)=0.
           up_massentr(i,k)=0.
           up_massdetr(i,k)=0.
           up_massentro(i,k)=0.
           up_massdetro(i,k)=0.
         enddo
!        if(i.eq.ipr.and.j.eq.jpr)then
!           write(0,*)'hcnew = '
!           do k=1,kte !ktf
!             write(0,*)k,hco(i,k),dbyo(i,k)
!           enddo
!        endif
      endif
42    continue
!     enddo
!
!--- calculate workfunctions for updrafts
!
      call cup_up_aa0(aa0,z,zu,dby,GAMMA_CUP,t_cup, &
           kbcon,ktop,ierr,                         &
           itf,jtf,ktf,                             &
           its,ite, jts,jte, kts,kte)
      call cup_up_aa0(aa1,zo,zuo,dbyo,GAMMAo_CUP,tn_cup, &
           kbcon,ktop,ierr,                              &
           itf,jtf,ktf,                                  &
           its,ite, jts,jte, kts,kte)
      do i=its,ite
         if(ierr(i).eq.0)then
           if(aa1(i).eq.0.)then
               ierr(i)=17
               ierrc(i)="cloud work function zero"
           endif
         endif
      enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!--- change per unit mass that a model cloud would modify the environment
!
!--- 1. in bottom layer
!
      do k=kts,kte
      do i=its,ite
        dellah(i,k)=0.
        dsubt(i,k)=0.
        dsubh(i,k)=0.
        dellaq(i,k)=0.
        dsubq(i,k)=0.
      enddo
      enddo
!
!----------------------------------------------  cloud level ktop
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level ktop-1
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level k+2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k+1
!
!----------------------------------------------  cloud level k+1
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level k
!
!----------------------------------------------  cloud level k
!
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!      .               .                 .
!
!----------------------------------------------  cloud level 3
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 2
!
!----------------------------------------------  cloud level 2
!
!- - - - - - - - - - - - - - - - - - - - - - - - model level 1

      do i=its,ite
        if(ierr(i).eq.0)then
         dp=100.*(po_cup(i,1)-po_cup(i,2))
             dsubt(i,1)=0. 
             dsubq(i,1)=0. 
         do k=kts+1,ktop(i)
               subin=0.
               subdown=0.
! these three are only used at or near mass detrainment and/or entrainment levels
            entupk=0.
            detupk=0.
! entrainment/detrainment for updraft
            entup=up_massentro(i,k)
            detup=up_massdetro(i,k)
!
!         SPECIAL LEVELS
!
            if(k.eq.ktop(i))then
               detupk=zuo(i,ktop(i))
               subin=0.
               subdown=0.
               entup=0.
               detup=0.
            endif
            totmas=subin-subdown+detup-entup  &
             -entupk+detupk+zuo(i,k+1)-zuo(i,k)
!               print *,'*********************',k,totmas
!              write(0,123)k,subin+zuo(i,k+1),subdown-zuo(i,k),detup,entup, &
!                          detdo,entdo,entupk,detupk
!             write(8,*)'totmas = ',k,totmas
            if(abs(totmas).gt.1.e-6)then
               write(0,*)'*********************',i,j,k,totmas
               print *,jmin(i),k22(i),kbcon(i),ktop(i)
               write(0,123)k,subin,subdown,detup,entup, &
                           entupk,detupk,zuo(i,k+1),zuo(i,k)
123     formAT(1X,i2,10E12.4)
!        call wrf_error_fatal ( 'totmas .gt.1.e-6' )
            endif
            dp=100.*(po_cup(i,k)-po_cup(i,k+1))
            dellah(i,k)=(detup*.5*(HCo(i,K+1)+HCo(i,K)) &
                    -entup*heo(i,k) &
                    +subin*heo_cup(i,k+1) &
                    -subdown*heo_cup(i,k) &
                    +detupk*(hco(i,ktop(i))-heo_cup(i,ktop(i)))    &
                    -entupk*heo_cup(i,k22(i)) &
                     )*g/dp
            dellaq(i,k)=(detup*.5*(qco(i,K+1)+qco(i,K)-qrco(i,k+1)-qrco(i,k)) &
                    -entup*qo(i,k) &
                    +subin*qo_cup(i,k+1) &
                    -subdown*qo_cup(i,k) &
                    +detupk*(qco(i,ktop(i))-qrco(i,ktop(i))-qo_cup(i,ktop(i)))    &
                    -entupk*qo_cup(i,k22(i)) &
                     )*g/dp
          
!
! updraft subsidence only
!
            if(k.lt.ktop(i))then
              dsubt(i,k)=(zuo(i,k+1)*heo_cup(i,k+1) &
                    -zuo(i,k)*heo_cup(i,k))*g/dp
              dsubq(i,k)=(zuo(i,k+1)*qo_cup(i,k+1) &
                    -zuo(i,k)*qo_cup(i,k))*g/dp
!           if(i.eq.ipr.and.j.eq.jpr)then
!            write(0,*)'dq3',k,zuo(i,k+1)*heo_cup(i,k+1),zuo(i,k)*heo_cup(i,k)
            endif
!
          enddo   ! k-loop

        endif !err-check
      enddo !i-loop
!
!-- take out cloud liquid water for detrainment
!
      do k=kts,kte-1
        do i=its,ite
          dellaqc(i,k)=0.
          if(ierr(i).eq.0)then
            if(k.eq.ktop(i)-0)dellaqc(i,k)= &
                      .01*zuo(i,ktop(i))*qrco(i,ktop(i))* &
                      9.81/(po_cup(i,k)-po_cup(i,k+1))
            if(k.lt.ktop(i).and.k.gt.kbcon(i))then
              dz=zo_cup(i,k+1)-zo_cup(i,k)
              dellaqc(i,k)=.01*9.81*up_massdetro(i,k)*.5*(qrco(i,k)+qrco(i,k+1))/ &
                        (po_cup(i,k)-po_cup(i,k+1))
            endif
            if(dellaqc(i,k).lt.0)write(0,*)'neg della',i,j,k,ktop(i),qrco(i,k), &
              qrco(i,k+1),up_massdetro(i,k),zuo(i,ktop(i))
            dellaqc(i,k)=max(0.,dellaqc(i,k))
          endif
        enddo
      enddo
!
!--- using dellas, calculate changed environmental profiles
!
      mbdt=3.e-4

      do k=kts,kte !ktf
        do i=its,ite !itf
          dellat(i,k)=0.
          if(ierr(i).eq.0)then
            dsubh(i,k)=dsubt(i,k)
            !05 Dec 2013
            dellaq(i,k)=dellaq(i,k)+dellaqc(i,k)
            dellaqc(i,k)=0.
            XHE(I,K)=(dsubt(i,k)+DELLAH(I,K))*MBDT+HEO(I,K)
            XQ(I,K)=(dsubq(i,k)+DELLAQ(I,K))*MBDT+QO(I,K)
            DELLAT(I,K)=(1./cp)*(DELLAH(I,K)-xl*DELLAQ(I,K))
            dSUBT(I,K)=(1./cp)*(dsubt(i,k)-xl*dsubq(i,k))
            XT(I,K)= (DELLAT(I,K)+dsubt(i,k))*MBDT+TN(I,K)
            IF(XQ(I,K).LE.0.)XQ(I,K)=1.E-08
          ENDIF
        enddo
      enddo
      do i=its,ite !itf
        if(ierr(i).eq.0)then
          xhkb(i)=hkbo(i)+(dsubh(i,k22(i))+DELLAH(I,K22(i)))*MBDT
          XHE(I,kte)=HEO(I,kte)
          XQ(I,kte)=QO(I,kte)
          XT(I,kte)=TN(I,kte)
          IF(XQ(I,kte).LE.0.)XQ(I,kte)=1.E-08
        endif
      enddo
!
!--- calculate moist static energy, heights, qes
!
      call cup_env(xz,xqes,xhe,xhes,xt,xq,po,z1, &
           psur,ierr,tcrit,-1,xl,cp,   &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!--- environmental values on cloud levels
!
      call cup_env_clev(xt,xqes,xq,xhe,xhes,xz,po,xqes_cup,xq_cup, &
           xhe_cup,xhes_cup,xz_cup,po_cup,gamma_cup,xt_cup,psur,   &
           ierr,z1,xl,rv,cp,          &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
!
!**************************** static control
!
!--- moist static energy inside cloud
!
!     do i=its,ite !itf
!       if(ierr(i).eq.0)then
!         xhkb(i)=xhe(i,k22(i))
!       endif
!     enddo
      do k=kts,kte
        do i=its,ite
          xhc(i,k)=0.
          xDBY(I,K)=0.
        enddo
      enddo
      do i=its,ite
        if(ierr(i).eq.0)then
!        if(use_excess == 2) then
!            k1=max(1,k22(i)-1)
!            k2=k22(i)+1
!            xhkb(i) =sum(xhe_cup(i,k1:k2))/float(k2-k1+1)+xl*zqexec(i)+cp*ztexec(i)
!        else if(use_excess <= 1) then
!            xhkb(i)=xhe_cup(i,k22(i))+float(use_excess)*(xl*zqexec(i)+cp*ztexec(i))
!        endif

          do k=1,kbcon(i)-1
             xhc(i,k)=xhkb(i)
          enddo
          k=kbcon(i)
          xhc(i,k)=xhkb(i)
          xDBY(I,Kbcon(i))=xHkb(I)-xHES_cup(I,K)
        endif !ierr
      enddo
!
!
      do i=its,ite
        if(ierr(i).eq.0)then
          xzu(i,kts:kte)=zuo(i,kts:kte)
          do k=kbcon(i)+1,ktop(i)
             xhc(i,k)=(xhc(i,k-1)*xzu(i,k-1)-.5*up_massdetro(i,k-1)*xhc(i,k-1)+ &
                         up_massentro(i,k-1)*xhe(i,k-1))   /            &
                         (xzu(i,k-1)-.5*up_massdetro(i,k-1)+up_massentro(i,k-1))
             xdby(i,k)=xhc(i,k)-xhes_cup(i,k)
          enddo
          do k=ktop(i)+1,kte
             xHC(i,K)=xhes_cup(i,k)
             xDBY(I,K)=0.
             xzu(i,k)=0.
          enddo
        endif
      enddo
!
!--- workfunctions for updraft
!
      call cup_up_aa0(xaa0,xz,xzu,xdby,GAMMA_CUP,xt_cup, &
           kbcon,ktop,ierr,           &
           itf,jtf,ktf, &
           its,ite, jts,jte, kts,kte)
!
! now for shallow forcing
!
      do i=its,ite
        xmb(i)=0.
        xff_shal(1:9)=0.
        if(ierr(i).eq.0)then
!JOE* add scale-awareness here; multiply mass flux by Psig 
          !xmbmax(i)=0.1
          xmbmax(i)=0.1*Psig(i)                                         !JOE
          xkshal=(xaa0(i)-aa1(i))/mbdt
          if(xkshal.ge.0.)xkshal=+1.e6
          if(xkshal.gt.-1.e-4 .and. xkshal.lt.0.)xkshal=-1.e-4
          xff_shal(1)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))*Psig(i)   !JOE
          xff_shal(1)=min(xmbmax(i),xff_shal(1))
          xff_shal(2)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))*Psig(i)   !JOE
          xff_shal(2)=min(xmbmax(i),xff_shal(2))
          xff_shal(3)=max(0.,-(aa1(i)-aa0(i))/(xkshal*dtime))*Psig(i)   !JOE
          xff_shal(3)=min(xmbmax(i),xff_shal(3))
          if(aa1(i).le.0)then
            xff_shal(1)=0.
            xff_shal(2)=0.
            xff_shal(3)=0.
          endif
          if(aa1(i)-aa0(i).le.0.)then
            xff_shal(1)=0.
            xff_shal(2)=0.
            xff_shal(3)=0.
          endif
! boundary layer QE (from Saulo Freitas)
          blqe=0.
          trash=0.
          if(k22(i).lt.kpbl(i)+1)then
             do k=1,kbcon(i)-1
                blqe=blqe+100.*dhdt(i,k)*(po_cup(i,k)-po_cup(i,k+1))/g
             enddo
             trash=max((hc(i,kbcon(i))-he_cup(i,kbcon(i))),1.e1)
             xff_shal(7)=max(0.,blqe/trash)*Psig(i)                     !JOE
             xff_shal(7)=min(xmbmax(i),xff_shal(7))
          else
             xff_shal(7)=0.
          endif
          if(xkshal.lt.-1.1e-04)then ! .and.  &
!            ((aa1(i)-aa0(i).gt.0.) .or. (xff_shal(7).gt.0)))then
            xff_shal(4)=max(0.,-aa0(i)/(xkshal*tscl_KF))*Psig(i)        !JOE
            xff_shal(4)=min(xmbmax(i),xff_shal(4))
            xff_shal(5)=xff_shal(4)
            xff_shal(6)=xff_shal(4)
          else
            xff_shal(4)=0.
            xff_shal(5)=0.
            xff_shal(6)=0.
          endif
!         write(0,888)'i0=',i,j,kpbl(i),blqe,xff_shal(7)
!888       format(a3,3(1x,i3),2e12.4)
          xff_shal(8)= xff_shal(7)
          xff_shal(9)= xff_shal(7)
! Sum & average the closures:
          fsum=0.
          do k=1,9
            xmb(i)=xmb(i)+xff_shal(k)
            fsum=fsum+1.
          enddo
          xmb(i)=min(xmbmax(i),xmb(i)/fsum)
!JOE-test1, closure1-3
!          xmb(i)=xff_shal(1)
!JOE-test2, closure4-6
!          xmb(i)=xff_shal(4)
!JOE-test3, closure7-9
!JOE-use only one closure for now until the others are fixed
          xmb(i)=xff_shal(7)

!         if(i.eq.ipr.and.j.eq.jpr)write(0,*)',ierr,xffs',ierr(i),xff_shal(1:9),xmb(i),xmbmax(i)
          if(xmb(i).eq.0.)ierr(i)=22
          if(xmb(i).eq.0.)ierrc(i)="22"
          if(xmb(i).lt.0.)then
             ierr(i)=21
             ierrc(i)="21"
!            write(0,*)'neg xmb,i,j,xmb for shallow = ',i,j,k22(i),ierr(i)
          endif
        endif !err-check
        if(ierr(i).ne.0)then
           k22(i)=0
           kbcon(i)=0
           ktop(i)=0
           xmb(i)=0
           do k=kts,kte
              outt(i,k)=0.
              outq(i,k)=0.
              outqc(i,k)=0.
           enddo
        else if(ierr(i).eq.0)then
!
! got the mass flux, sanity check, first for heating rates
!
          trash=0.
!         kmaxx=0
          do k=2,ktop(i)
            trash=max(trash,86400.*(dsubt(i,k)+dellat(i,k))*xmb(i))
          enddo
          if(trash.gt.100.)then
            xmb(i)=xmb(i)*100./trash
          endif
          trash=0.
          do k=2,ktop(i)
            trash=min(trash,86400.*(dsubt(i,k)+dellat(i,k))*xmb(i))
          enddo
          if(trash.lt.-100.)then
            xmb(i)=-xmb(i)*100./trash
          endif
!
! sanity check on moisture tendencies: do not allow anything that may allow neg
! tendencies
!
          do k=2,ktop(i)
            trash=q(i,k)+(dsubq(i,k)+dellaq(i,k))*xmb(i)*dtime
            if(trash.lt.1.e-12)then
              ! max allowable tendency over tendency that would lead to 
              ! too small mix ratios
              trash=(1.e-12 -q(i,k))/((dsubq(i,k)+dellaq(i,k))*dtime)
              xmb(i)=(1.e-12 -q(i,k))/((dsubq(i,k)+dellaq(i,k))*dtime)
            endif
          enddo
          xmb_out(i)=xmb(i)
! 
! final tendencies
!
          do k=2,ktop(i)
            outt(i,k)=(dsubt(i,k)+dellat(i,k))*xmb(i)
            outq(i,k)=(dsubq(i,k)+dellaq(i,k))*xmb(i)
          enddo
        endif
      enddo !i-loop
!      
! done shallow
!--------------------------done------------------------------
!

   END SUBROUTINE gfo_shcu

!=====================================================================
   SUBROUTINE cup_env(z,qes,he,hes,t,q,p,z1,                 &
              psur,ierr,tcrit,itest,xl,cp,                   &
              itf,jtf,ktf,                                   &
              its,ite, jts,jte, kts,kte                     )

   IMPLICIT NONE

     integer                                                 &
        ,intent (in   )                   ::                 &
        itf,jtf,ktf,                                         &
        its,ite, jts,jte, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! qes         = environmental saturation mixing ratio
  ! t           = environmental temp
  ! tv          = environmental virtual temp
  ! p           = environmental pressure
  ! z           = environmental heights
  ! he          = environmental moist static energy
  ! hes         = environmental saturation moist static energy
  ! psur        = surface pressure
  ! z1          = terrain elevation
  !
  !
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (in   )                   ::                 &
        p,t,q
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (out  )                   ::                 &
        he,hes,qes
     real,    dimension (its:ite,kts:kte)                    &
        ,intent (inout)                   ::                 &
        z
     real,    dimension (its:ite)                            &
        ,intent (in   )                   ::                 &
        psur,z1
     real                                                    &
        ,intent (in   )                   ::                 &
        xl,cp
     integer, dimension (its:ite)                            &
        ,intent (inout)                   ::                 &
        ierr
     integer                                                 &
        ,intent (in   )                   ::                 &
        itest
!
!  local variables in this routine
!
     integer                              ::                 &
       i,k,iph
      real, dimension (1:2) :: AE,BE,HT
      real, dimension (its:ite,kts:kte) :: tv
      real :: tcrit,e,tvbar
!      real, external :: satvap
!      real :: satvap

      HT(1)=XL/CP
      HT(2)=2.834E6/CP
      BE(1)=.622*HT(1)/.286
      AE(1)=BE(1)/273.+ALOG(610.71)
      BE(2)=.622*HT(2)/.286
      AE(2)=BE(2)/273.+ALOG(610.71)
!      print *, 'TCRIT = ', tcrit,its,ite
      DO k=kts,kte
        do i=its,ite
          if(ierr(i).eq.0)then
!Csgb - IPH is for phase, dependent on TCRIT (water or ice)
            IPH=1
            IF(T(I,K).LE.TCRIT)IPH=2
!           print *, 'AE(IPH),BE(IPH) = ',AE(IPH),BE(IPH),AE(IPH)-BE(IPH),T(i,k),i,k
!           E=EXP(AE(IPH)-BE(IPH)/T(I,K))
!           print *, 'P, E = ', P(I,K), E
!           QES(I,K)=.622*E/(100.*P(I,K)-E)
            e=satvap(t(i,k))
            qes(i,k)=0.622*e/max(1.e-8,(p(i,k)-e))
            IF(QES(I,K).LE.1.E-08)QES(I,K)=1.E-08
            IF(QES(I,K).LT.Q(I,K))QES(I,K)=Q(I,K)
!           IF(Q(I,K).GT.QES(I,K))Q(I,K)=QES(I,K)
            TV(I,K)=T(I,K)+.608*Q(I,K)*T(I,K)
          endif
        enddo
      enddo
!
!--- z's are calculated with changed h's and q's and t's
!--- if itest=2
!
      if(itest.eq.1 .or. itest.eq.0)then
         do i=its,ite
           if(ierr(i).eq.0)then
             Z(I,1)=max(0.,Z1(I))-(ALOG(P(I,1))- &
                 ALOG(PSUR(I)))*287.*TV(I,1)/9.81
           endif
         enddo

! --- calculate heights
         DO K=kts+1,kte
           do i=its,ite
             if(ierr(i).eq.0)then
               TVBAR=.5*TV(I,K)+.5*TV(I,K-1)
               Z(I,K)=Z(I,K-1)-(ALOG(P(I,K))- &
                ALOG(P(I,K-1)))*287.*TVBAR/9.81
             endif
           enddo
         enddo
      else if(itest.eq.2)then
         do k=kts,kte
           do i=its,its
             if(ierr(i).eq.0)then
               z(i,k)=(he(i,k)-1004.*t(i,k)-2.5e6*q(i,k))/9.81
               z(i,k)=max(1.e-3,z(i,k))
             endif
           enddo
         enddo
      else if(itest.eq.-1)then
      endif
!
!--- calculate moist static energy - HE
!    saturated moist static energy - HES
!
      DO k=kts,kte
        do i=its,ite
          if(ierr(i).eq.0)then
            if(itest.le.0)HE(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*Q(I,K)
            HES(I,K)=9.81*Z(I,K)+1004.*T(I,K)+2.5E06*QES(I,K)
            IF(HE(I,K).GE.HES(I,K))HE(I,K)=HES(I,K)
          endif
        enddo
      enddo

   END SUBROUTINE cup_env

!=========================================================================

   SUBROUTINE cup_env_clev(t,qes,q,he,hes,z,p,qes_cup,q_cup,   &
              he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup,psur, &
              ierr,z1,xl,rv,cp,                                &
              itf,jtf,ktf,                                     &
              its,ite, jts,jte, kts,kte                       )

   IMPLICIT NONE

     integer                                                   &
        ,intent (in   )                   ::                   &
        itf,jtf,ktf,           &
        its,ite, jts,jte, kts,kte
  !
  ! ierr error value, maybe modified in this routine
  ! q           = environmental mixing ratio
  ! q_cup       = environmental mixing ratio on cloud levels
  ! qes         = environmental saturation mixing ratio
  ! qes_cup     = environmental saturation mixing ratio on cloud levels
  ! t           = environmental temp
  ! t_cup       = environmental temp on cloud levels
  ! p           = environmental pressure
  ! p_cup       = environmental pressure on cloud levels
  ! z           = environmental heights
  ! z_cup       = environmental heights on cloud levels
  ! he          = environmental moist static energy
  ! he_cup      = environmental moist static energy on cloud levels
  ! hes         = environmental saturation moist static energy
  ! hes_cup     = environmental saturation moist static energy on cloud levels
  ! gamma_cup   = gamma on cloud levels
  ! psur        = surface pressure
  ! z1          = terrain elevation
  !
  !
     real,    dimension (its:ite,kts:kte)                      &
        ,intent (in   )                   ::                   &
        qes,q,he,hes,z,p,t
     real,    dimension (its:ite,kts:kte)                      &
        ,intent (out  )                   ::                   &
        qes_cup,q_cup,he_cup,hes_cup,z_cup,p_cup,gamma_cup,t_cup
     real,    dimension (its:ite)                              &
        ,intent (in   )                   ::                   &
        psur,z1
     real                                                      &
        ,intent (in   )                   ::                   &
        xl,rv,cp
     integer, dimension (its:ite)                              &
        ,intent (inout)                   ::                   &
        ierr
!
!  local variables in this routine
!
     integer                              ::       i,k

      do k=kts,kte
        do i=its,ite
          qes_cup(i,k)=0.
          q_cup(i,k)=0.
          hes_cup(i,k)=0.
          he_cup(i,k)=0.
          z_cup(i,k)=0.
          p_cup(i,k)=0.
          t_cup(i,k)=0.
          gamma_cup(i,k)=0.
        enddo
      enddo
      do k=kts+1,kte
        do i=its,ite
          if(ierr(i).eq.0)then
            qes_cup(i,k)=.5*(qes(i,k-1)+qes(i,k))
            q_cup(i,k)=.5*(q(i,k-1)+q(i,k))
            hes_cup(i,k)=.5*(hes(i,k-1)+hes(i,k))
            he_cup(i,k)=.5*(he(i,k-1)+he(i,k))
            if(he_cup(i,k).gt.hes_cup(i,k))he_cup(i,k)=hes_cup(i,k)
            z_cup(i,k)=.5*(z(i,k-1)+z(i,k))
            p_cup(i,k)=.5*(p(i,k-1)+p(i,k))
            t_cup(i,k)=.5*(t(i,k-1)+t(i,k))
            gamma_cup(i,k)=(xl/cp)*(xl/(rv*t_cup(i,k) &
                       *t_cup(i,k)))*qes_cup(i,k)
          endif
        enddo
      enddo
      do i=its,ite
        if(ierr(i).eq.0)then
          qes_cup(i,1)=qes(i,1)
          q_cup(i,1)=q(i,1)
!         hes_cup(i,1)=hes(i,1)
!         he_cup(i,1)=he(i,1)
          hes_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*qes(i,1)
          he_cup(i,1)=9.81*z1(i)+1004.*t(i,1)+2.5e6*q(i,1)
          z_cup(i,1)=.5*(z(i,1)+z1(i))
          p_cup(i,1)=.5*(p(i,1)+psur(i))
          z_cup(i,1)=z1(i)
          p_cup(i,1)=psur(i)
          t_cup(i,1)=t(i,1)
          gamma_cup(i,1)=xl/cp*(xl/(rv*t_cup(i,1) &
                       *t_cup(i,1)))*qes_cup(i,1)
        endif
      enddo

   END SUBROUTINE cup_env_clev

!===========================================================
      real function satvap(temp2)
      implicit none
      real :: temp2, temp, toot, toto, eilog, tsot,  &
     &        ewlog, ewlog2, ewlog3, ewlog4
 
      temp = temp2-273.155
      if (temp.lt.-20.) then   !!!! ice saturation
        toot = 273.16 / temp2
        toto = 1 / toot
        eilog = -9.09718 * (toot - 1) - 3.56654 * (log(toot) / &
     &    log(10.)) + .876793 * (1 - toto) + (log(6.1071) / log(10.))
        satvap = 10 ** eilog
      else
        tsot = 373.16 / temp2
        ewlog = -7.90298 * (tsot - 1) + 5.02808 * &
     &             (log(tsot) / log(10.))
        ewlog2 = ewlog - 1.3816e-07 * &
     &             (10 ** (11.344 * (1 - (1 / tsot))) - 1)
        ewlog3 = ewlog2 + .0081328 * &
     &             (10 ** (-3.49149 * (tsot - 1)) - 1)
        ewlog4 = ewlog3 + (log(1013.246) / log(10.))
        satvap = 10 ** ewlog4
      end if
      return
      end function
!===========================================================

END MODULE module_shcu_gfo
