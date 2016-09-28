      PROGRAM SNDPST
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM: ETA_SNDP
C   PRGMMR: ROGERS           ORG: NP22        DATE: 1999-09-24
C
C ABSTRACT:  THIS ROUTINE POSTS PROFILE DATA AND WRITES
C   OUTPUT IN BUFR FORMAT.  THIS REPLACES CODE THAT USED TO
C   RUN INSIDE OF CHKOUT IN THE ETA MODEL.
C     
C PROGRAM HISTORY LOG:
C   95-07-26  MIKE BALDWIN
C   96-05-07  MIKE BALDWIN - USE SMASK TO SET SOIL VARS TO MISSING
C   96-11-22  MIKE BALDWIN - ADD OPTION OF DOING MONOLITHIC FILE OR
C                            SPLIT OUT FILES OR BOTH
C   97-12-16  MIKE BALDWIN - NEW MULTI-LEVEL PARAMETERS (SUCH
C                            AS SOIL MOISTURE)
C   98-07-23  ERIC ROGERS  - MADE Y2K COMPLIANT
C   98-09-29  MIKE BALDWIN - SET ACC/AVE VARS TO MISSING AT T=0
C   99-04-01  GEOFF MANIKIN - MAJOR CHANGES FEATURING A REMOVAL OF
C                            THE DISTNICTION OF CLASS0 - ALL OUTPUT
C                            IS NOW CLASS1.  ALSO, THE FIELDS OF
C                            VISIBILITY AND CLOUD BASE PRESSURE 
C                            ARE ADDED.
C   99-09-03  JIM TUCCILLO - REDUCED MEMORY REQUIREMENTS BY CHANGING
C                            THE SIZE OF PRODAT AND INTRODUCING LPRO.
C                            ALSO, PRODAT'S STRUCTURE WAS CHANGED TO
C                            PROVIDE STRIDE-1 ACCESS.
C                            NOTE: THIS CODE CAN STILL BE MODIFIED TO
C                            REDUCE MEMORY. THE CHANGES TODAY WILL
C                            NOT AFFECT ITS FUNCTIONALITY BUT WILL
C                            ALLOW IT TO RUN ON A WINTERHAWK NODE
C                            WITHOUT PAGING. THE MEMORY REQUIREMENT
C                            IS NOW AT ABOUT 260 MBs.
C   00-03-10  GEOFF MANIKIN - CODE CHANGED TO BE READY FOR ETA EXTENSION
C                            TO 60 HOURS
C   00-5-15  ERIC ROGERS   - PUT NSOIL AND LM1 IN INCLUDED PARAMETER 
C                             FILE
C  
C   03-8-01  BINBIN ZHOU   -MODIFY TO ALSO WORK ON RSM 
C                           1.PUT NSTP and LCL1SL1 in INCLUDE file       
C                           2.USE SOIL LEVELS(4 for ETA and 2 for RSM TO 
C                             COMPUTE INDEX AND SOME PARAMETERS          
C  06-07-31  BINBIN ZHOU    -MODIFY TO WORK ON NCAR WRF
C                           The PROF output of NCAR-WRF(ARW) has no rain profile 
C                           while NCEP-WRF(NMM) has, so SNDPST.f for ARW is very different 
C                           from SNDPST for NMM.    
C  08-07-01  BINBIN ZHOU    -MODIFY TO WORK ON NCAR WRF  
C
C USAGE:   
C   INPUT ARGUMENT LIST:
C     NONE    
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       CALWXT
C       CALHEL
C       BFRIZE
C     LIBRARY:
C       BUFRLIB     
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C***
C***   7/26/95 M. BALDWIN
C*** 
C***     SOUNDING POST PROCESSOR
C***     PROGRAM TO READ THE PROFILE OUTPUT FILE ON THE CRAY
C***     AND PRODUCE DIAGNOSTIC QUANTITIES AND PACK INTO BUFR
C***
C--------------------------------------------------------------------
C
C    PARMS FOR HOURLY PROFILER OUTPUT
C      LM    - MAX NUMBER OF VERTICAL LEVELS
C      NPNT  - MAX NUMBER OF OUTPUT TIMES    
C      
C          TO HOLD ALL VARIABLES FOR ANY CLASS OF OUTPUT
C          (MAX NO MULTI-LAYER VARIABLES*LM + NO OF SINGLE LAYER VARS)
C      LCL1ML - MAX NUMBER OF MULTI-LAYER VARIABLES FOR CLASS 0 OR 1
C      LCL1SL - MAX NUMBER OF SINGLE LAYER VARIABLES FOR CLASS 0 OR 1
C      LCL1SOIL - MAX NUMBER OF SOIL LAYER VARIABLES FOR CLASS 0 OR 1
C      NSTAT - NUMBER OF STATIONS
C
C        NOTE: THESE NUMBERS WILL BE LARGER THAN THE NUMBERS
C              COMING OUT OF THE MODEL IN THE BINARY FILE SINCE
C              WE ARE COMPUTING SOME ADDITIONAL VARIABLES TO GO INTO
C              BUFR IN THIS PROGRAM.
C
C--------------------------------------------------------------------
      INCLUDE "parm.em"

      PARAMETER (LM=LM1,NPNT=NSTP                
     &, SPVAL=-99999.0,SMISS=1.E10                
     &, LCL1ML=13,LCL1SL=50,LCL1SOIL=4  
     &, LCL1ML1=13,LCL1SL1=58,ROG=287.04/9.8
     &, NWORD=(LCL1ML+1)*50+2*LCL1SL+NSOIL*LCL1SOIL)                               
c20080701     &, NWORD=964)               !14*60+2*58+4*2=964, Eta/RSM/NCAR_WRF use this size

      PARAMETER (SNOCON=1.4594E5,RAINCON=1.1787E4)
C
      LOGICAL LVLWSE,SEQFLG(8),NEED
      CHARACTER*16 SEQNM1(8), SBSET
      CHARACTER*80 CLIST1(8),FMTO,ASSIGN
      CHARACTER*8 CISTAT
cwas  DIMENSION FPACK(NWORD),PRODAT(NSTAT,NPNT,NWORD)
      DIMENSION FPACK(NWORD)
      REAL PRODAT(NWORD,NSTAT,NPNT)
C
C     THE PURPOSE OF LPRO IS TO HOLD THE VALUES OF RISTAT UNTIL
C     THEY ARE COPIED TO FRODAT. THIS ADDITION WILL ALLOW PRODAT
C     TO BE A REAL(4) ARRAY ( AND SAVE A CONSIDERABLE AMOUNT OF 
C     MEMORY) . PRODAT CAN BE FURTHER REDUCED WITH SOME MORE EFFORT.
C                    JIM TUCCILLO
C
      REAL(8) LPRO(NSTAT,NPNT)
      REAL(8) RISTAT 
cwas  REAL(8) PRODAT(NSTAT,NPNT,NWORD),RISTAT 
      REAL(8) FRODAT(NWORD),WORKK(NWORD)
      DIMENSION P(LM),T(LM),U(LM),V(LM),Q(LM),PINT(LM+1),ZINT(LM+1)
      REAL CWTR(LM),IMXR(LM)
      INTEGER IDATE(3),NP1(8),LLMH(NSTAT),NLVL(2),NSTAT_TRUE
      EQUIVALENCE (CISTAT,RISTAT)
C--------------------------------------------------------------------     
C
C     SET OUTPUT UNITS FOR CLASS 1 PROFILE FILE.
C      LCLAS1 - OUTPUT UNIT FOR CLASS 1 BINARY FILE
C      LTBCL1 - INPUT UNIT FOR CLASS 1 BUFR TABLE FILE
C      LUNCL1 - OUTPUT UNIT FOR CLASS 1 BUFR FILE
C
C--------------------------------------------------------------------     
                            I N T E G E R
     & LCLAS1,LTBCL1,LUNCL1,STDOUT
C--------------------------------------------------------------------     
                            L O G I C A L
     & MONOL,BRKOUT
C--------------------------------------------------------------------     
       NAMELIST /MODTOP/ ETOP
       NAMELIST /OPTION/ MONOL,BRKOUT
                            D A T A
     & LCLAS1 / 76 /
     &,LTBCL1 / 32 /
     &,LUNCL1 / 78 /
     &,STDOUT / 6 /
     &,SEQNM1 /'HEADR','PROFILE','SURF','FLUX',
     &         'HYDR','D10M','SLYR','XTRA'/
     &,SEQFLG /.FALSE.,.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &   .TRUE.,.FALSE./
     &,LVLWSE /.TRUE./
c      CALL W3TAGB('ETA_SNDP',1999,0267,0084,'NP22')


      PRODAT=0.
      FMTO='("ln -s ${DIRD}",I5.5,".",I4.4,3I2.2,'//
     &     '"  fort.",I2.2)'
C
C   GET MODEL TOP PRESSURE
C
       print *, 'into SNDPST'
       PTOP=25.0*100.0
       READ(5,MODTOP,END=12321)  !read from standard io (screen)
12321  CONTINUE
       PTOP=ETOP*100.0
       print *, 'PTOP ', PTOP
C
C   READ IN SWITCHES TO CONTROL WHETHER TO DO...
C     MONOL=.TRUE.   DO MONOLITHIC FILE
C     BRKOUT=.TRUE.  DO BREAKOUT FILES
C
       MONOL=.TRUE.
       BRKOUT=.FALSE.
       READ(11,OPTION,END=12322) !read from file eta_sndp.parm.mono
12322  CONTINUE
C
        IFCSTL=-99
        JHR=0
C----------------------------------------------------------------------
C---READ STATION DATA--------------------------------------------------
C----------------------------------------------------------------------
      LUNIT=66   !read from binary data file 
cBZHOU      LRECPR=4*(8+9+LCL1ML1*LM1+LCL1SL1)        !for RSM/ETA
      LRECPR=4*(8+9+LCL1ML*LM1+LCL1SL)                !for NCAR-WRF

      OPEN(UNIT=LUNIT,ACCESS='DIRECT',RECL=LRECPR,IOSTAT=IER)
      NREC=0
 33   CONTINUE
      NREC=NREC+1      

      print *, 'about to read profilm ', LRECPR
      READ(LUNIT,REC=NREC,IOSTAT=IRR,ERR=999) IHRST,IDATE,IFCST,
c     &   ISTAT,CISTAT,(FPACK(N),N=1,9),(FPACK(N),N=10,FPACK(7))
     &   ISTAT,CISTAT,(FPACK(N),N=1,9),(FPACK(N),N=10,709)

      IF(IRR.NE.0) THEN
       WRITE(*,*) NREC, '  read error, IRR=',IRR
      END IF

        IF (IFCST.GT.IFCSTL) THEN
         INUMS=1
         JHR=JHR+1
         IFCSTL=IFCST
        ELSE
         INUMS=INUMS+1
        ENDIF

c        INUMS=NST

        IYR=IDATE(3)
        IMON=IDATE(1)
        IDAY=IDATE(2)
        RLAT=FPACK(1)*DEG
        RLON=FPACK(2)*DEG
        ELEV=FPACK(3)
        LLMH(INUMS)=NINT(FPACK(4))
        LMH=NINT(FPACK(4))

        DO 25 L=1,LMH
C   REVERSE ORDER SO THAT P(1) IS THE TOP AND P(LMH) IS THE BOTTOM
         LV=LMH-L+1
         P(LV)=FPACK(L+9)
         T(LV)=FPACK(L+9+LMH)
         U(LV)=FPACK(L+9+LMH*2)
         V(LV)=FPACK(L+9+LMH*3)
         Q(LV)=FPACK(L+9+LMH*4)
C   IF THE CLOUD WATER IS NEGATIVE, IT IS ICE
         CWTR(LV)=FPACK(L+9+LMH*6)
         IF (CWTR(LV).LT.0.) THEN
           IMXR(LV)= -1. * CWTR(LV)
           CWTR(LV)= 0. 
           FPACK(L+9+LMH*6) = 0.
         ELSE
c           IMXR(LV) = 0.
C GSM  need to set ice to missing for now
           IMXR(LV) = SMISS
         ENDIF
 25     CONTINUE

C  USE SEA MASK TO SET SOIL/SFC VARIABLES TO MISSING VALUES
C    (IF SEA)
C
cbinbin  SM  =FPACK(13*LMH+54)              !this is for Eta
         SM  =FPACK(13*LMH+36+9+2*NSOIL+1)  !this is for both Eta(SM=54), RSM(SM=50), RWF-NCAR(54)
                                            !ie, #45, #41, #45 of sfc variables 
cZhou     IF (SM.GT.0.5) THEN                !Note all var following soil temperature needs modified
C   SMSTAV
CBZhou, WRF-NCAR has such value          FPACK(13*LMH+15)=SMISS            !soil moisture 
C
C   SUBSHX
C          FPACK(13*LMH+21)=SMISS            !sub-sfc heat flux
C   SNOPCX
C          FPACK(13*LMH+22)=SMISS            !flux  of snow phase change
C   ACSNOW, SMSTOT, SNO, ACSNOM, SSROFF, BGROFF, SOILTB
CBZhou, WRF-NCAR has ACSNOW, ACSNOM, SSROFF, BGROFF
C          DO LKJ=20,26
C            FPACK(13*LMH+LKJ+9)=SMISS
C          ENDDO
C   SFCEXC, VEGFRC, CMC, SMC(1:4), STC(1:4), (if RSM, SMC(1:2),STC(1:2) )
c Binbin modify following code:
c          DO LKJ=34,44
c            FPACK(13*LMH+LKJ+9)=SMISS
c          ENDDO
c as:
cZhou         DO LKJ=34,36
cZhou            FPACK(13*LMH+LKJ+9)=SMISS
cZhou         ENDDO
cZhou
cZhou         DO LKJ=37,37+2*NSOIL-1
cZhou            FPACK(13*LMH+LKJ+9)=SMISS
cZhou         ENDDO

ccZhou       ENDIF
C

C  GET PPT FOR CALWXT
C
         PPT  =FPACK(13*LMH+16)    !total rain
C     COMPUTE PINT,ZINT
C
        PINT(1)=PTOP
        DO L=1,LMH
          DP1=P(L)-PINT(L)
          PINT(L+1)=P(L)+DP1
        ENDDO
        ZINT(LMH+1)=FPACK(3)
        DO L=LMH,1,-1
         TV2=T(L)*(1.0+0.608*Q(L))
         ZZ=ROG*TV2*ALOG(PINT(L+1)/PINT(L))
         ZINT(L)=ZINT(L+1)+ZZ
        ENDDO
C
C     CALL PRECIP TYPE SUBROUTINE.
C
C      CALL CALWXT(T,Q,P,PINT,LMH,LM,PPT,IWX)
C     CALL PRECIP TYPE SUBROUTINES.
      CALL CALWXT(T,Q,P,PINT,LMH,LM,PPT,IWX1)
      CALL CALWXT_RAMER(T,Q,P,PINT,LMH,LM,PPT,IWX2)
      CALL CALWXT_BOURG(T,Q,PINT,LMH,LM,PPT,ZINT,IWX3)
      CALL CALWXT_REVISED(T,Q,P,PINT,LMH,LM,PPT,IWX4)
      CALL CALWXT_DOMINANT(PPT,IWX1,IWX2,IWX3,IWX4,IWX5,
     *        CSNO,CICE,CFZR,CRAI)

C     
C     DECOMPOSE IWX
C
C        CSNO=MOD(IWX,2)
C
C        CICE=MOD(IWX,4)/2
C
C        CFZR=MOD(IWX,8)/4
C
C        CRAI=IWX/8

C
C   COMPUTE HELICITY AND STORM MOTION
C
      CALL CALHEL(U,V,P,ZINT,PINT,LMH,LM,HELI,UST,VST)
C
C   COMPUTE VISIBILITY
C   FIRST, EXTRACT THE SNOW RATIO AND SEA LEVEL PRESSURE
      SR=FPACK(9+13*LMH+49) ! set to 0 in EM 

cBZhou      SLP=FPACK(13*LMH+10)  !WRF-NCAR has no sea level pressure output
       !use sfc pressure and elevation FPACK(3)to derive sea level pressure aproximately:
       !at present, just set it = 101300.0 Pa      
      SLP=101300.0
c       SLP=FPACK(13*LMH+10)

       if (PPT.LT.0.)then
         PPT=0.0
       endif

       SNORATE=(SR/100.)*PPT/3600.
       RAINRATE=(1-(SR/100.))*PPT/3600.
       TERM1=(T(LMH)/SLP)**0.4167
       TERM2=(T(LMH)/(P(LMH)))**0.5833
       TERM3=RAINRATE**0.8333
c       QRAIN=RAINCON*TERM1*TERM2*TERM3
       TERM4=(T(LMH)/SLP)**0.47
       TERM5=(T(LMH)/(P(LMH)))**0.53
       TERM6=SNORATE**0.94
c       QSNO=SNOCON*TERM4*TERM5*TERM6
       TT=T(LMH)
       QV=Q(LMH)
       QCD=CWTR(LMH)
       QICE=IMXR(LMH)
       PPP=P(LMH)

       QSNO=0.0
       QRAIN=0.0 
C    DO NOT HAVE ALL OF THE MIXING RATIOS YET
C       CALL CALVIS(QV,QCD,QRAIN,QICE,QSNO,TT,PPP,HOVI)  
       HOVI=SMISS

C   COMPUTE CLOUD BASE PRESSURE
C   FIRST, EXTRACT THE CONVECTIVE CLOUD BASE
       HBOT=FPACK(13*LMH+9+50)
       CLIMIT =1.0E-06
       NEED = .TRUE.
       CDBP = SMISS
       CBOT = 5000                       
 
       DO L=LMH,1,-1
C GSM
C START AT THE FIRST LAYER ABOVE GROUND, AND FIND THE
C   FIRST LAYER WITH A VALUE OF CLOUD WATER GREATER THAN
C   THE SIGNIFICANT LIMIT (VALUE DESIGNATED BY Q. ZHAO).
C   THIS LAYER WILL BE THE CLOUD BOTTOM UNLESS THE BOTTOM
C   OF THE CONVECTIVE CLOUD (HBOT) IS FOUND BELOW IN WHICH
C   CASE HBOT BECOMES THE CLOUD BASE LAYER.

        
        IF ((CWTR(L)+IMXR(L)).GT.CLIMIT.AND.NEED) THEN
            CBOT=L
            IF (HBOT.GT.CBOT) THEN
              CBOT = HBOT
            ENDIF
            NEED=.FALSE.
          ENDIF
       ENDDO

       IF (CBOT.GT.LMH) THEN  !cloud base
          CDBP=SMISS
       ELSE
          CDBP=P(INT(CBOT))
       ENDIF
       
c with cloud issues, need to set cloud base pressure to missing
       CDBP=SMISS
  
C
C
C   SET ACC/AVERAGED VARIABLES TO MISSING IF IFCST=0
C
      IF (IFCST.EQ.0) THEN
          DO L=1,LMH
           FPACK(L+9+LMH*7)=SMISS
           FPACK(L+9+LMH*8)=SMISS
          ENDDO
          DO JK=16,29
           FPACK(13*LMH+JK)=SMISS
          ENDDO
          DO JK=32,34
           FPACK(13*LMH+JK)=SMISS
          ENDDO
      ENDIF
C

C      ADD 9 SINGLE LEVEL VARIABLES TO THE OUTPUT
C      TACK THEM ON TO THE END;  WE DON'T NEED CONVECTIVE
C      CLOUD BASE, THOUGH, SO WRITE OVER THAT RECORD.
C      SO GO BACK 1 PLACE FOR STARTING POINT
C      WHICH MEANS WRITING OVER RECORD # NLENGTH - GSM
          NLEN = FPACK(7)         
          FPACK(NLEN+7) = CDBP     
          FPACK(NLEN) =   CSNO
          FPACK(NLEN+1) = CICE
          FPACK(NLEN+2) = CFZR
          FPACK(NLEN+3) = CRAI
          FPACK(NLEN+4) = UST
          FPACK(NLEN+5) = VST
          FPACK(NLEN+6) = HELI
          FPACK(NLEN+8) = HOVI
          FPACK(5) = FPACK(5) + 1  !add ice mixing ratio space 
          FPACK(6) = FPACK(6) + 6  !9 new variables but write over 3
          FPACK(7) = 9 + FPACK(5)*FPACK(4) + FPACK(6)

C
C           PLACE DATA INTO PRODAT IN PROPER LOCATIONS

          PRODAT (1,INUMS,JHR) = FLOAT(IFCST)
          PRODAT (2,INUMS,JHR) = FLOAT(ISTAT)

C         RISTAT is a REAL(8) variable by virtue of the fact that it 
C         is equivalenced to CISTAT. Everything else stored in PRODAT
C         is REAL(4). We have made PRODAT REAL(4) but need a REAL(8)
C         array for storing RISTAT - that is what LPRO is. Farther
C         down in the code, we will pull values out of LPRO and store
C         in FRODAT ( a REAL(8) array ).
cwas      PRODAT (3,INUMS,JHR) = RISTAT 


          LPRO   (  INUMS,JHR) = RISTAT
          PRODAT (4,INUMS,JHR) = FPACK (1)
          PRODAT (5,INUMS,JHR) = FPACK (2)
          PRODAT (6,INUMS,JHR) = FPACK (3)
          PRODAT (7,INUMS,JHR) = 1 

          DO IJ = 10, 13*LMH+9                              !13 profile variables
            PRODAT (IJ-2,INUMS,JHR) = FPACK (IJ)            !PRODAT(8,INUMS,JHR)=FPACK(10) 
          ENDDO                                             !PRODAT(9,INUMS,JHR)=FPACK(11)
C    TACK ON THE ICE WATER TO THE PROFILE SECTION           !..........
C    IT IS CURRENTLY WRITTEN IN REVERSE ORDER.
          DO L=1,LMH
            LV=LMH-L+1
            PRODAT(L+7+LMH*13,INUMS,JHR)=IMXR(LV)           !14th profile variable, ie. ice mixing ratio
          ENDDO
          DO IJ = 13*LMH+10,NLEN+8                          !all other surface variable
            PRODAT (IJ+LMH-2,INUMS,JHR) = FPACK (IJ)
          ENDDO

       IF (ISTAT .eq. 724030 .and. JHR .eq. 19) THEN
         DO M=1,765
          print *, 'prodat check ', M, PRODAT(M,INUMS,JHR)
         ENDDO
        ENDIF

        GOTO 33
 999    CONTINUE
 
        write(*,*) 'Write all of data into one big Bufr file ...'

       write(0,*) 'NREC-1: ', NREC-1
        write(0,*) 'estimated true NSTA: ', (NREC-1)/NFCST
        NSTAT_TRUE=(NREC-1)/NFCST
        write(0,*) 'NWORD is: ', NWORD
C
C  WRITE OUT INDIVIDUAL FILES FOR EACH STATION
C
        IF (BRKOUT) THEN
        DO I=1,NSTAT
         NLVL(1)=LLMH(I)
         NLVL(2)=NSOIL
C
         DO J=1,NFCST
          DO IJ = 1, NWORD
            FRODAT(IJ) = PRODAT (IJ,I,J)
          ENDDO
          FRODAT(3) = LPRO(I,J)
          ISTAT=NINT(FRODAT(2))

C
          IF (J.EQ.1) THEN
C
C     INITIALIZE BUFR LISTS SO BFRHDR WILL BE CALLED THE FIRST
C     TIME THROUGH.
C
            WRITE(ASSIGN,FMTO) ISTAT,IYR,IMON,IDAY,IHRST,LCLAS1
            CALL SYSTEM(ASSIGN)
            CLIST1(1)=' '
          ENDIF
C
C           CALL BUFR-IZING ROUTINE
C

           NSEQ = 8
           SBSET = 'ETACLS1'

          CALL BFRIZE(LTBCL1,LCLAS1,SBSET,IYR,IMON,IDAY,IHRST
     1,               SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,               WORKK,IER)
          IF(IER.NE.0)WRITE(6,1080)ISTAT,IER,FRODAT(1)
 1080   FORMAT(' SOME SORT OF ERROR ',2I8,F9.1)
C
C
         ENDDO
C
C   FINISHED, CLOSE UP BUFR FILES
C
        NSEQ = 8
        CALL BFRIZE(0,LCLAS1,SBSET,IYR,IMON,IDAY,IHRST
     1,             SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,             WORKK,IER)
         ENDDO
         ENDIF
         IF (MONOL) THEN
C
C  WRITE OUT ONE FILE FOR ALL STATIONS
C
C     INITIALIZE BUFR LISTS SO BFRHDR WILL BE CALLED THE FIRST
C     TIME THROUGH.
C
        CLIST1(1)=' '
        DO I=1,NSTAT_true
         NLVL(1)=LLMH(I)
         NLVL(2)=NSOIL
C
         DO J=1,NFCST
          DO IJ = 1, NWORD
            FRODAT(IJ) = PRODAT (IJ,I,J)
          ENDDO

          FRODAT(3) = LPRO(I,J)
          ISTAT=NINT(FRODAT(2))

C
C           CALL BUFR-IZING ROUTINE
C
           NSEQ = 8
           SBSET = 'ETACLS1'
           CALL BFRIZE(LTBCL1,LUNCL1,SBSET,IYR,IMON,IDAY,IHRST
     1,                SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,               WORKK,IER)
          IF(IER.NE.0)WRITE(6,1080)ISTAT,IER,FRODAT(1)

C
C
         ENDDO
         ENDDO
C
C   FINISHED, CLOSE UP BUFR FILES
C
        NSEQ = 8
        CALL BFRIZE(0,LUNCL1,SBSET,IYR,IMON,IDAY,IHRST
     1,             SEQNM1,SEQFLG,NSEQ,LVLWSE,FRODAT,NLVL,CLIST1,NP1
     2,             WORKK,IER)
         ENDIF
C
        WRITE(STDOUT,*) ' END OF SOUNDING POST '
C      CALL W3TAGE('ETA_SNDP')
        STOP
        END
