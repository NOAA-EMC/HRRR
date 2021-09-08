       SUBROUTINE CALWXT_DOMINANT(PREC,IWX1,IWX2,IWX3,IWX4,
     &         IWX5,DOMS,DOMIP,DOMZR,DOMR)
C
C     WRITTEN: 5 APRIL 2006, G MANIKIN 
C      
C     THIS ROUTINE TAKES THE PRECIP TYPE SOLUTIONS FROM DIFFERENT
C       ALGORITHMS AND DETERMINES THE MAJORITY TO GIVE A DOMINANT TYPE
C
      PARAMETER (NALG=5)
      PARAMETER (PTHRESH=0.02)
      REAL PREC, DOMS, DOMR, DOMZR, DOMIP
      INTEGER IWX,IWX1,IWX2,IWX3,IWX4,IWX5
      DIMENSION RAIN(NALG),SNOW(NALG),SLEET(NALG),FREEZR(NALG)
C
        DOMR = 0.
        DOMS = 0.
        DOMZR = 0.
        DOMIP = 0.
C
C   SKIP THIS POINT IF NO PRECIP THIS TIME STEP
       IF (PREC .LE. PTHRESH) RETURN 

       ISNO=MOD(IWX1,2)
       IIP=MOD(IWX1,4)/2
       IZR=MOD(IWX1,8)/4
       IRAIN=IWX1/8
       SNOW(1) = ISNO*1.0
       SLEET(1) = IIP*1.0
       FREEZR(1) = IZR*1.0
       RAIN(1) = IRAIN*1.0

       ISNO=MOD(IWX2,2)
       IIP=MOD(IWX2,4)/2
       IZR=MOD(IWX2,8)/4
       IRAIN=IWX2/8
       SNOW(2) = ISNO*1.0
       SLEET(2) = IIP*1.0
       FREEZR(2) = IZR*1.0
       RAIN(2) = IRAIN*1.0

       ISNO=MOD(IWX3,2)
       IIP=MOD(IWX3,4)/2
       IZR=MOD(IWX3,8)/4
       IRAIN=IWX3/8
       SNOW(3) = ISNO*1.0
       SLEET(3) = IIP*1.0
       FREEZR(3) = IZR*1.0
       RAIN(3) = IRAIN*1.0

       ISNO=MOD(IWX4,2)
       IIP=MOD(IWX4,4)/2
       IZR=MOD(IWX4,8)/4
       IRAIN=IWX4/8
       SNOW(4) = ISNO*1.0
       SLEET(4) = IIP*1.0
       FREEZR(4) = IZR*1.0
       RAIN(4) = IRAIN*1.0

       ISNO=MOD(IWX5,2)
       IIP=MOD(IWX5,4)/2
       IZR=MOD(IWX5,8)/4
       IRAIN=IWX5/8
       SNOW(5) = ISNO*1.0
       SLEET(5) = IIP*1.0
       FREEZR(5) = IZR*1.0
       RAIN(5) = IRAIN*1.0
       TOTSN = 0
       TOTIP = 0
       TOTR  = 0
       TOTZR = 0 

C   LOOP OVER THE NUMBER OF DIFFERENT ALGORITHMS THAT ARE USED
       DO 820 L = 1, NALG
        IF (RAIN(L).GT. 0) THEN
           TOTR = TOTR + 1
           GOTO 830
        ENDIF

        IF (SNOW(L).GT. 0) THEN
           TOTSN = TOTSN + 1
           GOTO 830
        ENDIF

        IF (SLEET(L).GT. 0) THEN
           TOTIP = TOTIP + 1
           GOTO 830
        ENDIF

        IF (FREEZR(L).GT. 0) THEN
           TOTZR = TOTZR + 1
           GOTO 830
        ENDIF
 830    CONTINUE
 820    CONTINUE

C   TIES ARE BROKEN TO FAVOR THE MOST DANGEROUS FORM OF PRECIP
C     FREEZING RAIN > SNOW > SLEET > RAIN 
        IF (TOTSN .GT. TOTIP) THEN
         IF (TOTSN .GT. TOTZR) THEN
          IF (TOTSN .GE. TOTR) THEN
           DOMS = 1.
           GOTO 800 
          ELSE
           DOMR = 1. 
           GOTO 800 
          ENDIF
         ELSE IF (TOTZR .GE. TOTR) THEN
          DOMZR = 1.
          GOTO 800 
         ELSE
          DOMR = 1.
          GOTO 800 
         ENDIF 
        ELSE IF (TOTIP .GT. TOTZR) THEN
         IF (TOTIP .GE. TOTR) THEN
          DOMIP = 1.
          GOTO 800 
         ELSE
          DOMR = 1.
          GOTO 800 
         ENDIF
        ELSE IF (TOTZR .GE. TOTR) THEN
         DOMZR = 1.
         GOTO 800 
         ELSE
          DOMR = 1.
          GOTO 800 
         ENDIF
 800   RETURN
      END
