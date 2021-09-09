      SUBROUTINE MEMSLP_NMM(TPRES,QPRES,FIPRES)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  MEMSLP      MEMBRANE SLP REDUCTION
C
C ABSTRACT:  THIS ROUTINE COMPUTES THE SEA LEVEL PRESSURE
C            REDUCTION USING THE MESINGER RELAXATION
C            METHOD FOR SIGMA COORDINATES.
C            A BY-PRODUCT IS THE
C            SET OF VALUES FOR THE UNDERGROUND TEMPERATURES
C            ON THE SPECIFIED PRESSURE LEVELS
C
C PROGRAM HISTORY LOG:
C   99-09-23  T BLACK - REWRITTEN FROM ROUTINE SLP (ETA
C                       COORDINATES)
C   02-07-26  H CHUANG - PARALLIZE AND MODIFIED FOR WRF A/C GRIDS
C                        ALSO REDUCE S.O.R. COEFF FROM 1.75 to 1.25 
C                        BECAUSE THERE WAS NUMERICAL INSTABILITY  
C   02-08-21  H CHUANG - MODIFIED TO ALWAYS USE OLD TTV FOR RELAXATION
C                        SO THAT THERE WAS BIT REPRODUCIBILITY BETWEEN
C                        USING ONE AND MULTIPLE TASKS	      
C
C USAGE:  CALL SLPSIG FROM SUBROUITNE ETA2P
C
C   INPUT ARGUMENT LIST:
C     PD   - SFC PRESSURE MINUS PTOP
C     FIS  - SURFACE GEOPOTENTIAL
C     T    - TEMPERATURE 
C     Q    - SPECIFIC HUMIDITY
C     FI   - GEOPOTENTIAL
C     PT   - TOP PRESSURE OF DOMAIN
C
C   OUTPUT ARGUMENT LIST:
C     PSLP - THE FINAL REDUCED SEA LEVEL PRESSURE ARRAY
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C             NONE
C
C-----------------------------------------------------------------------
      use vrbls3d
      use vrbls2d
      use masks
c      
      INCLUDE "params"
      INCLUDE "mpif.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!     & (LSMP1=LSM+1,IMJM=IM*JM)
                            P A R A M E T E R
     & (NFILL=0,NRLX1=500,NRLX2=100)
c
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
                            R E A L
     &TTV(IM,JSTA_2L:JEND_2U),TNEW(IM,JSTA_2L:JEND_2U)
     &,SLPX(IM,JSTA_2L:JEND_2U)
     &,P1(IM,JSTA_2L:JEND_2U),HTM2D(IM,JSTA_2L:JEND_2U)
      REAL TPRES(IM,JSTA_2L:JEND_2U,LSM)
     &,QPRES(IM,JSTA_2L:JEND_2U,LSM)
     &,FIPRES(IM,JSTA_2L:JEND_2U,LSM),HTMO(IM,JSTA_2L:JEND_2U,LSM)			    
                            I N T E G E R
     & KMNTM(LSM),IMNT(IM_JM,LSM),JMNT(IM_JM,LSM)
     &,LMHO(IM,JSTA_2L:JEND_2U)
                            I N T E G E R
     & IHE(JM),IHW(JM),IVE(JM),IVW(JM),IHS(JM),IHN(JM)
C-----------------------------------------------------------------------
                            L O G I C A L
     & SIGMA,STDRD,DONE(IM,JSTA_2L:JEND_2U)
C-----------------------------------------------------------------------
      STDRD=.FALSE.
C-----------------------------------------------------------------------
C***  CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C***
C
      ii=61
      jj=65
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
      ENDDO
      print*,'relaxation coeff= ',OVERRC
C-----------------------------------------------------------------------
C***  INITIALIZE ARRAYS.  LOAD SLP ARRAY WITH SURFACE PRESSURE.
C***
!$omp parallel do 
      DO J=JSTA,JEND
      DO I=1,IM
        LLMH=NINT(LMH(I,J))
        PSLP(I,J)=PINT(I,J,LLMH+1)
        if(i.eq.ii.and.j.eq.jj)print*,'Debug: FIS,IC for PSLP='
     +  ,FIS(i,j),PSLP(I,J)
        TTV(I,J)=0.
        LMHO(I,J)=0
      ENDDO
      ENDDO
C
C***  CALCULATE SEA LEVEL PRESSURE FOR PROFILES (AND POSSIBLY
C***  FOR POSTING BY POST PROCESSOR).
C
C***  "STDRD" REFERS TO THE "STANDARD" SLP REDUCTION SCHEME.
C
      IF(STDRD)GO TO 400
C--------------------------------------------------------------------
C***
C***  CREATE A 3-D "HEIGHT MASK" FOR THE SPECIFIED PRESSURE LEVELS
C***  (1 => ABOVE GROUND) AND A 2-D INDICATOR ARRAY THAT SAYS 
C***  WHICH PRESSURE LEVEL IS THE LOWEST ONE ABOVE THE GROUND
C***
      DO 100 L=1,LSM
      SPLL=SPL(L)
C       
      DO J=JSTA,JEND
      DO I=1,IM
        PSFC=PSLP(I,J)
        PCHK=PSFC
        IF(NFILL.GT.0)THEN
	 PCHK=PINT(I,J,NINT(LMH(I,J))+1-NFILL)
        ENDIF
        IF(FIS(I,J).LT.1.)PCHK=PSLP(I,J)
C
        IF(SPLL.LT.PCHK)THEN
          HTMO(I,J,L)=1.
        ELSE
          HTMO(I,J,L)=0.
          IF(L.GT.1.AND.HTMO(I,J,L-1).GT.0.5)LMHO(I,J)=L-1
        ENDIF
C
        IF(L.EQ.LSM.AND.HTMO(I,J,L).GT.0.5)LMHO(I,J)=LSM
        if(i.eq.ii.and.j.eq.jj)print*,'Debug: HTMO= ',HTMO(I,J,L)
      ENDDO
      ENDDO
C
  100 CONTINUE
      if(jj.ge.jsta.and.jj.le.jend)
     +print*,'Debug: LMHO=',LMHO(ii,jj)
C--------------------------------------------------------------------
C***
C***  WE REACH THIS LINE IF WE WANT THE MESINGER ETA SLP REDUCTION
C***  BASED ON RELAXATION TEMPERATURES.  THE FIRST STEP IS TO
C***  FIND THE HIGHEST LAYER CONTAINING MOUNTAINS.
C***
      DO 210 L=LSM,1,-1
C
      DO J=JSTA,JEND
      DO I=1,IM
        IF(HTMO(I,J,L).LT.0.5)GO TO 210
      ENDDO
      ENDDO
C
      LHMNT=L+1
      GO TO 220
  210 CONTINUE
C
  220 CONTINUE
      print*,'Debug in SLP: LHMNT=',LHMNT
      if ( num_procs .gt. 1 ) then
      CALL MPI_ALLREDUCE
     &  (LHMNT,LXXX,1,MPI_INTEGER,MPI_MIN,MPI_COMM_COMP,IERR)
      LHMNT = LXXX
      end if
      IF(LHMNT.EQ.LSMP1)THEN
        GO TO 430
      ENDIF
      print*,'Debug in SLP: LHMNT A ALLREDUCE=',LHMNT
C***
C***  NOW GATHER THE ADDRESSES OF ALL THE UNDERGROUND POINTS.
C***
!$omp parallel do private(kmn,kount)
      DO 250 L=LHMNT,LSM
      KMN=0
      KMNTM(L)=0
      KOUNT=0
      DO 240 J=JSTA_M2,JEND_M2
      DO 240 I=2,IM-1
      KOUNT=KOUNT+1
      IMNT(KOUNT,L)=0
      JMNT(KOUNT,L)=0
      IF(HTMO(I,J,L).GT.0.5)GO TO 240
      KMN=KMN+1
      IMNT(KMN,L)=I
      JMNT(KMN,L)=J
  240 CONTINUE
      KMNTM(L)=KMN
  250 CONTINUE
C
C
C***  CREATE A TEMPORARY TV ARRAY, AND FOLLOW BY SEQUENTIAL
C***  OVERRELAXATION, DOING NRLX PASSES.
C
c     IF(NTSD.EQ.1)THEN
        NRLX=NRLX1
c     ELSE
c       NRLX=NRLX2
c     ENDIF
C
!$omp parallel do private(i,j,tinit,ttv)
      DO 300 L=LHMNT,LSM
C
      DO 270 J=JSTA,JEND
      DO 270 I=1,IM
      TTV(I,J)=TPRES(I,J,L)
      IF(TTV(I,J).lt.150. .and. TTV(I,J).gt.325.0)print*
     1,'abnormal IC for T relaxation',i,j,TTV(I,J)
      HTM2D(I,J)=HTMO(I,J,L)
  270 CONTINUE
C
C***  FOR GRID BOXES NEXT TO MOUNTAINS, COMPUTE TV TO USE AS
C***  BOUNDARY CONDITIONS FOR THE RELAXATION UNDERGROUND
C
      CALL EXCH2(HTM2D(1,JSTA_2L))   !NEED TO EXCHANGE TWO ROW FOR E GRID
      DO J=JSTA_M2,JEND_M2
      DO I=2,IM-1
        IF(HTM2D(I,J).GT.0.5.AND.
     1     HTM2D(I+IHW(J),J-1)*HTM2D(I+IHE(J),J-1)
     2    *HTM2D(I+IHW(J),J+1)*HTM2D(I+IHE(J),J+1)
     3    *HTM2D(I-1     ,J  )*HTM2D(I+1     ,J  )
     4    *HTM2D(I       ,J-2)*HTM2D(I       ,J+2).LT.0.5)THEN
C     
          TTV(I,J)=TPRES(I,J,L)*(1.+0.608*QPRES(I,J,L))
        ENDIF
        if(i.eq.ii.and.j.eq.jj)print*,'Debug:L,TTV B SMOO= ',l,TTV(I,J) 
      ENDDO
      ENDDO
C
      KMM=KMNTM(L)
C
      DO 285 N=1,NRLX
      CALL EXCH2(TTV(1,JSTA_2L))
      DO 280 KM=1,KMM
      I=IMNT(KM,L)
      J=JMNT(KM,L)
      TINIT=TTV(I,J)
      TNEW(I,J)=AD05*(4.*(TTV(I+IHW(J),J-1)+TTV(I+IHE(J),J-1)
     1                  +TTV(I+IHW(J),J+1)+TTV(I+IHE(J),J+1))
     2                  +TTV(I-1,J)       +TTV(I+1,J)
     3                  +TTV(I,J-2)       +TTV(I,J+2))
     4                  -CFT0*TTV(I,J)
CHC MODIFICATION FOR C AND A GRIDS
C eight point relaxation using old TTV
CHC       TNEW(I,J)=AD05*(4.*(TTV(I-1,J)+TTV(I+1,J)
CHC     1                  +TTV(I,J-1)+TTV(I,J+1))
CHC     2                  +TTV(I-1,J-1)+TTV(I+1,J-1)
CHC     3                  +TTV(I-1,J+1)+TTV(I+1,J+1))
CHC     4                  -CFT0*TTV(I,J)
C
      if(i.eq.ii.and.j.eq.jj)print*,'Debug: L,TTV A S'
     1,l,TTV(I,J),N
c     1,l,TNEW(I,J),N
  280 CONTINUE
C
      DO KM=1,KMM
       I=IMNT(KM,L)
       J=JMNT(KM,L)
       TTV(I,J)=TNEW(I,J)
      END DO
  285 CONTINUE
C
      DO 290 KM=1,KMM
      I=IMNT(KM,L)
      J=JMNT(KM,L)
      TPRES(I,J,L)=TTV(I,J)
  290 CONTINUE
  300 CONTINUE
C----------------------------------------------------------------
C***
C***  CALCULATE THE SEA LEVEL PRESSURE AS PER THE NEW SCHEME.
C***  INTEGRATE THE HYDROSTATIC EQUATION DOWNWARD FROM THE
C***  GROUND THROUGH EACH OUTPUT PRESSURE LEVEL (WHERE TV
C***  IS NOW KNOWN) TO FIND GZ AT THE NEXT MIDPOINT BETWEEN
C***  PRESSURE LEVELS.  WHEN GZ=0 IS REACHED, SOLVE FOR THE
C***  PRESSURE.
C***
C
C***  COUNT THE POINTS WHERE SLP IS DONE BELOW EACH OUTPUT LEVEL
C
      KOUNT=0
      DO J=JSTA,JEND
      DO I=1,IM
        DONE(I,J)=.FALSE.
        IF(FIS(I,J).LT.1.)THEN
          PSLP(I,J)=PINT(I,J,NINT(LMH(I,J))+1)
          DONE(I,J)=.TRUE.
          KOUNT=KOUNT+1
          if(i.eq.ii.and.j.eq.jj)print*,'Debug:DONE,PSLP A S1='
     +,done(i,j),PSLP(I,J)
        ENDIF
      ENDDO
      ENDDO
C
      KMM=KMNTM(LSM)
!$omp parallel do private(gz1,gz2,i,j,lmap1,p1,p2),shared(pslp)
      DO 320 KM=1,KMM
      I=IMNT(KM,LSM)
      J=JMNT(KM,LSM)
      IF(DONE(I,J))GO TO 320
      LMHIJ=LMHO(I,J)
      GZ1=FIPRES(I,J,LMHIJ)
      P1(I,J)=SPL(LMHIJ)
C
      LMAP1=LMHIJ+1
      DO L=LMAP1,LSM
        P2=SPL(L)
        TLYR=0.5*(TPRES(I,J,L)+TPRES(I,J,L-1))
        GZ2=GZ1+RD*TLYR*ALOG(P1(I,J)/P2)
        FIPRES(I,J,L)=GZ2
        if(i.eq.ii.and.j.eq.jj)print*,'Debug:L,FI A S2=',L,GZ2
        IF(GZ2.LE.0.)THEN
          PSLP(I,J)=P1(I,J)/EXP(-GZ1/(RD*TPRES(I,J,L-1)))
          if(i.eq.ii.and.j.eq.jj)print*,'Debug:PSLP A S2=',PSLP(I,J)
          DONE(I,J)=.TRUE.
          KOUNT=KOUNT+1
          GO TO 320
        ENDIF
        P1(I,J)=P2
        GZ1=GZ2
      ENDDO
CHC EXPERIMENT
      LP=LSM
      SLOPE=-6.6E-4 
      TLYR=TPRES(I,J,LP)-0.5*FIPRES(I,J,LP)*SLOPE
      PSLP(I,J)=spl(lp)/EXP(-FIPRES(I,J,LP)/(RD*TLYR))
      DONE(I,J)=.TRUE.
      if(i.eq.ii.and.j.eq.jj)print*,'Debug:spl,FI,TLYR,PSLPA3='
     1,spl(lp),FIPRES(I,J,LP),TLYR,PSLP(I,J)       
CHC EXPERIMENT
  320 CONTINUE
C
C***  WHEN SEA LEVEL IS BELOW THE LOWEST OUTPUT PRESSURE LEVEL,
C***  SOLVE THE HYDROSTATIC EQUATION BY CHOOSING A TEMPERATURE
C***  AT THE MIDPOINT OF THE LAYER BETWEEN THAT LOWEST PRESSURE
C***  LEVEL AND THE GROUND BY EXTRAPOLATING DOWNWARD FROM T ON
C***  THE LOWEST PRESSURE LEVEL USING THE DT/DFI BETWEEN THE
C***  LOWEST PRESSURE LEVEL AND THE ONE ABOVE IT.
C
C      TOTAL=(IM-2)*(JM-4)
C
CHC      DO 340 LP=LSM,1,-1
c      IF(KOUNT.EQ.TOTAL)GO TO 350
CHC MODIFICATION FOR SMALL HILL HIGH PRESSURE SITUATION
CHC IF SURFACE PRESSURE IS CLOSER TO SEA LEVEL THAN LWOEST
CHC OUTPUT PRESSURE LEVEL, USE SURFACE PRESSURE TO DO EXTRAPOLATION
      LP=LSM
      DO 330 J=JSTA,JEND
      DO 330 I=1,IM
      if(i.eq.ii.and.j.eq.jj)print*,'Debug: with 330 loop'
      IF(DONE(I,J))GO TO 330
      if(i.eq.ii.and.j.eq.jj)print*,'Debug: still within 330 loop'
CHC Comment out the following line for situation with terrain 
CHC at boundary (ie FIPRES<0)
CHC because they were not counted as undergound point for 8 pt
CHC relaxation

      SLOPE=-6.6E-4
      IF(PINT(I,J,NINT(LMH(I,J))+1).GT.SPL(LP))THEN
       LLMH=NINT(LMH(I,J))
       TVRT=T(I,J,LLMH)*(H1+D608*Q(I,J,LLMH))
       DIS=ZINT(I,J,LLMH+1)-ZINT(I,J,LLMH)+0.5*ZINT(I,J,LLMH+1)
       TLYR=TVRT-DIS*G*SLOPE
       PSLP(I,J)=PINT(I,J,LLMH+1)*EXP(ZINT(I,J,LLMH+1)*G
     1 /(RD*TLYR))
      ELSE
       TLYR=TPRES(I,J,LP)-0.5*FIPRES(I,J,LP)*SLOPE
       PSLP(I,J)=spl(lp)/EXP(-FIPRES(I,J,LP)/(RD*TLYR))
       if(i.eq.ii.and.j.eq.jj)print*,'Debug:spl,FI,TLYR,PSLPA3='
     1,spl(lp),FIPRES(I,J,LP),TLYR,PSLP(I,J)
      END IF
      DONE(I,J)=.TRUE.
      KOUNT=KOUNT+1
  330 CONTINUE
C
  350 CONTINUE
C--------------------------------------------------------------------
C     SKIP THE STANDARD SCHEME.
C--------------------------------------------------------------------
      GO TO 430
C--------------------------------------------------------------------
C***
C***  IF YOU WANT THE "STANDARD" ETA/SIGMA REDUCTION
C***  THIS IS WHERE IT IS DONE.
C***
  400 CONTINUE
C
C****************************************************************
C     AT THIS POINT WE HAVE A SEA LEVEL PRESSURE FIELD BY
C     EITHER METHOD.  5-POINT AVERAGE THE FIELD ON THE E-GRID.
C****************************************************************
C
  430 CONTINUE
C
!$omp parallel do 
      DO 440 J=JSTA,JEND
      DO 440 I=1,IM
      SLPX(I,J)=PSLP(I,J)
  440 CONTINUE
C
      DO 480 KS=1,KSLPD
C
      CALL EXCH(PSLP(1,JSTA_2L))
!$omp parallel do private(ihh2)
c      DO 460 J=JSTA_M2,JEND_M2
      DO 460 J=JSTA_M,JEND_M
c      IHH2=IM-1-MOD(J+1,2)
      IHH2=IM-1
      DO 460 I=2,IHH2
C
C***  EXTRA AVERAGING UNDER MOUNTAINS TAKEN OUT, FM, MARCH 96
C
      SLPX(I,J)=0.125*(PSLP(I+IHW(J),J-1)+PSLP(I+IHE(J),J-1)
     1                +PSLP(I+IHW(J),J+1)+PSLP(I+IHE(J),J+1)
     2                +4.*PSLP(I,J))
  460 CONTINUE
C
!$omp parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        PSLP(I,J)=SLPX(I,J)
        if(pslp(i,j).gt.105000. .or. pslp(i,j).lt.93000.)print*,
     1'Debug:bad pslp,i,j,pslp= ',i,j,pslp(i,j)
      ENDDO
      ENDDO
C
  480 CONTINUE
C  
C THE FOLLOWING LINES ARE COMMENTED OUT SO THAT SMOOTHED
C UNDERGOUND TEMPERATURE DO NOT FEED BACK TO THE POST OUTPUT
      RETURN
      END
