       SUBROUTINE CALWXT_BOURG(T,Q,PINT,LMHK,LM,PPT,ZINT,PTYPE)
C
C     FILE: BOURG.f
C     WRITTEN: 06 JULY 1999, MICHAEL BALDWIN
C     REVISIONS:
C         20 SEP 1999 - BALDWIN: MAKE MORE CONSISTENT
C                                WITH BOURGOUIN (1992)
C         24 MAR 2006 - MANIKIN: ADDED TO WRF SOUNDING POST
C
C     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
C     APPROACH THAT USES THE SO-CALLED "ENERGY METHOD" OF
C     BOURGOUIN OF AES (CANADA) 1992
C
C     *** NOTE:  THIS SCHEME HAS SOME "IN-BETWEEN" SCENARIOS WHICH
C       IT ADDRESSES WITH RANDOM NUMBER GENERATION.  AS A RESULT,
C       RESULTS MAY NOT BE REPRODUCABLE.  ***
C
C  LIST OF VARIABLES NEEDED
C    INPUT:
C      T,TD,P,PINT,LMH,LM,PPT
C
C      T  - Mid layer temp (K)
C               pressures in log P)
C      Q  - Mid layer spec humidity
C      PINT - Interfacial pressure (Pa)
C      LMHK - Number of layers
C      LM - MAX Number of layers
C      PPT - Precip (m)
C      ZINT - Interfacial geopotential (m)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  NOTE: VERTICAL ORDER OF ARRAYS MUST BE LAYER 1 = TOP
C                                 ----          .
C                                               .
C                                               .
C                                         LAYER LMH = BOTTOM
C          (JUST LIKE IN THE ETA MODEL)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    INTERNAL:
C
C
C    OUTPUT:
C      PTYPE - INSTANTANEOUS WEATHER TYPE.
C        ACTS LIKE A 4 BIT BINARY
C          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
C          WHERE THE ONE'S DIGIT IS FOR SNOW
C                THE TWO'S DIGIT IS FOR ICE PELLETS
C                THE FOUR'S DIGIT IS FOR FREEZING RAIN
C            AND THE EIGHT'S DIGIT IS FOR RAIN
C
C
C-------------------------------------------------------------
C          IN OTHER WORDS...
C
C          PTYPE=1 SNOW
C          PTYPE=2 ICE PELLETS/MIX WITH ICE PELLETS
C          PTYPE=4 FREEZING RAIN/MIX WITH FREEZING RAIN
C          PTYPE=8 RAIN
C-------------------------------------------------------------
C
C     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
C     WE DO THIS SINCE WE WANT PTYPE TO REPRESENT THE
C     INSTANTANEOUS WEATHER TYPE ON RETURN.

      REAL T(LM),Q(LM),PINT(LM+1),ZINT(LM)
      INTEGER PTYPE 
      PARAMETER(PTHRES=0.02,G=9.80665)
C
      PTYPE=0
      IF (PPT.LE.PTHRES) RETURN

      T1=RTC()
      PSFCK=PINT(LMHK+1)
C
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
C
C     FIND THE DEPTH OF THE WARM LAYER BASED AT THE SURFACE
C     THIS WILL BE THE CUT OFF POINT BETWEEN COMPUTING
C     THE SURFACE BASED WARM AIR AND THE WARM AIR ALOFT
C
C
C     LOWEST LAYER T
C
      tlmhk = T(LMHK)
      iwrml = lmhk + 1
      IF (tlmhk.ge.273.15) THEN
        DO l = lmhk, 2, -1
         IF (t(l).ge.273.15.and.t(l-1).lt.273.15.and.
     +            iwrml.eq.lmhk+1) iwrml = l
          END DO
      END IF
C
C     NOW FIND THE HIGHEST ABOVE FREEZING LEVEL
C
      lhiwrm = lmhk + 1
      DO l = lmhk, 1, -1
          IF (t(l).ge.273.15) lhiwrm = l
      END DO

C     ENERGY VARIABLES
C     SURFW IS THE POSITIVE ENERGY BETWEEN THE GROUND
C     AND THE FIRST SUB-FREEZING LAYER ABOVE GROUND
C     AREANE IS THE NEGATIVE ENERGY BETWEEN THE GROUND
C     AND THE HIGHEST LAYER ABOVE GROUND
C     THAT IS ABOVE FREEZING
C     AREAPE IS THE POSITIVE ENERGY "ALOFT"
C     WHICH IS THE WARM ENERGY NOT BASED AT THE GROUND
C     (THE TOTAL WARM ENERGY = SURFW + AREAPE)
C
C     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
C     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
C     DZKL IS THE THICKNESS OF THE LAYER
C     IFRZL IS A FLAG THAT TELLS US IF WE HAVE HIT
C     A BELOW FREEZING LAYER
C
      pintk1 = psfck
      ifrzl = 0
      areane = 0.0
      areape = 0.0
      surfw = 0.0                                         

      DO 20 l = lmhk, 1, -1
          IF (ifrzl.eq.0.and.t(l).le.273.15) ifrzl = 1
          pintk2=pint(L)
          DZKL=ZINT(L)-ZINT(L+1)
c          dzkl = t(i,j,l) * (q(i,j,l)*0.608+1.0) * rog *
c     +              alog(pintk1/pintk2)
          area1 = alog(t(l)/273.15) * g * dzkl
          IF (t(l).ge.273.15) THEN
              IF (l.lt.iwrml) areape = areape + area1
              IF (l.ge.iwrml) surfw = surfw + area1
          ELSE
              IF (l.gt.lhiwrm) areane = areane + abs(area1)
          END IF
          pintk1 = pintk2
   20 CONTINUE
C
C     DECISION TREE TIME
C
      IF (areape.lt.2.0) THEN
C         VERY LITTLE OR NO POSITIVE ENERGY ALOFT, CHECK FOR
C         POSITIVE ENERGY JUST ABOVE THE SURFACE TO DETERMINE RAIN VS. SNOW
          IF (surfw.lt.5.6) THEN
C             NOT ENOUGH POSITIVE ENERGY JUST ABOVE THE SURFACE
C             SNOW = 1
              PTYPE = 1
              goto 800
          ELSE IF (surfw.gt.13.2) THEN
C             ENOUGH POSITIVE ENERGY JUST ABOVE THE SURFACE
C             RAIN = 8
              PTYPE = 8
              goto 800 
          ELSE
C             TRANSITION ZONE, ASSUME EQUALLY LIKELY RAIN/SNOW
C             PICKING A RANDOM NUMBER, IF <=0.5 SNOW
              t2=rtc() 
              ta=t2-t1
              call srand(ta)
              r1 = rand()
              IF (r1.le.0.5) THEN
C                 SNOW = 1
                  PTYPE = 1
                  goto 800
              ELSE
C                 RAIN = 8
                  PTYPE = 8
                  goto 800
              END IF
          END IF
      END IF
C
      IF (areape.ge.2.0) THEN
C         SOME POSITIVE ENERGY ALOFT, CHECK FOR ENOUGH NEGATIVE ENERGY
C         TO FREEZE AND MAKE ICE PELLETS TO DETERMINE IP VS. ZR
          IF (areane.gt.66.0+0.66*areape) THEN
C             ENOUGH NEGATIVE AREA TO MAKE IP,
C             NOW NEED TO CHECK IF THERE IS ENOUGH POSITIVE ENERGY
C             JUST ABOVE THE SURFACE TO MELT IP TO MAKE RAIN
              IF (surfw.lt.5.6) THEN
C                 NOT ENOUGH ENERGY AT THE SURFACE TO MELT IP
C                 ICE PELLETS = 2
                  PTYPE = 2
                  goto 800
              ELSE IF (surfw.gt.13.2) THEN
C                 ENOUGH ENERGY AT THE SURFACE TO MELT IP
C                 RAIN = 8
                  PTYPE = 8
                  goto 800
              ELSE
C                 TRANSITION ZONE, ASSUME EQUALLY LIKELY IP/RAIN
C                 PICKING A RANDOM NUMBER, IF <=0.5 IP
                  t2=rtc()
                  ta=t2-t1
                  call srand(ta)
                  r1 = rand()
                  IF (r1.le.0.5) THEN
C                     ICE PELLETS = 2
                      PTYPE = 2
                      goto 800
                  ELSE
C                     RAIN = 8
                      PTYPE = 8
                      goto 800 
                  END IF
              END IF
          ELSE IF (areane.lt.46.0+0.66*areape) THEN
C             NOT ENOUGH NEGATIVE ENERGY TO REFREEZE, CHECK SURFACE TEMP
C             TO DETERMINE RAIN VS. ZR
              IF (tlmhk.lt.273.15) THEN
C                 FREEZING RAIN = 4
                  PTYPE = 4
                   goto 800
              ELSE
C                 RAIN = 8
                  PTYPE = 8
                  goto 800
              END IF
          ELSE
C             TRANSITION ZONE, ASSUME EQUALLY LIKELY IP/ZR
C             PICKING A RANDOM NUMBER, IF <=0.5 IP
              t2=rtc()
              ta=t2-t1
              call srand(ta)
              r1 = rand()
              IF (r1.le.0.5) THEN
C                 STILL NEED TO CHECK POSITIVE ENERGY
C                 JUST ABOVE THE SURFACE TO MELT IP VS. RAIN
                  IF (surfw.lt.5.6) THEN
C                     ICE PELLETS = 2
                      PTYPE = 2
                      goto 800
                  ELSE IF (surfw.gt.13.2) THEN
C                     RAIN = 8
                      PTYPE = 8
                      goto 800
                  ELSE
C                     TRANSITION ZONE, ASSUME EQUALLY LIKELY IP/RAIN
C                     PICKING A RANDOM NUMBER, IF <=0.5 IP
                      t2=rtc()
                      ta=t2-t1
                      call srand(ta)
                      r2 = rand()
                      IF (r2.le.0.5) THEN
C                         ICE PELLETS = 2
                          PTYPE = 2
                          goto 800
                      ELSE
C                         RAIN = 8
                          PTYPE = 8
                          goto 800
                      END IF
                  END IF
              ELSE
C                 NOT ENOUGH NEGATIVE ENERGY TO REFREEZE, CHECK SURFACE TEMP
C                 TO DETERMINE RAIN VS. ZR
                  IF (tlmhk.lt.273.15) THEN
C                     FREEZING RAIN = 4
                      PTYPE = 4
                      goto 800
                  ELSE
C                     RAIN = 8
                      PTYPE = 8
                      goto 800 
                  END IF
              END IF
          END IF
      END IF
 800  CONTINUE
      RETURN
      END
C
C
