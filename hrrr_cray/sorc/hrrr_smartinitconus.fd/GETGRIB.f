      SUBROUTINE GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X   T2,Q2,D2,U10,V10,COAST,GUST,VSBY,TCLD,ZBASE,
     X   CEILING,PMSL,VALPT,DATE,IFHR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
C   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-09-30
C
C ABSTRACT:
C   .
C
C PROGRAM HISTORY LOG:
C   11-09-30  G MANIKIN  - ADAPT CODE TO HI-RES RAPID REFRESH 
C
C USAGE:    CALL SMARTINIT 
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE

      PARAMETER(ILIM=2145,JLIM=1377,MAXLEV=15)
      PARAMETER(ITOT=ILIM*JLIM)
      REAL, ALLOCATABLE :: GRID(:)
      DIMENSION DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5),YEAR,MON,DAY,IHR,DATE,IFHR
      LOGICAL*1 MASK(ITOT),VALPT(ILIM,JLIM)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      DIMENSION ZSFC(ILIM,JLIM),T(ILIM,JLIM,MAXLEV),PSFC(ILIM,JLIM),
     x  Q(ILIM,JLIM,MAXLEV),PMID(ILIM,JLIM,MAXLEV),VEG(ILIM,JLIM),
     x  UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV),
     x  T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM),COAST(ILIM,JLIM),
     x  U10(ILIM,JLIM),V10(ILIM,JLIM),HGHT(ILIM,JLIM,MAXLEV),
     x  VSBY(ILIM,JLIM),GUST(ILIM,JLIM),ZBASE(ILIM,JLIM),
     x  CEILING(ILIM,JLIM),TCLD(ILIM,JLIM)
      REAL PMSL(ILIM,JLIM)
C
      allocate(grid(itot))
      NUMLEV=MAXLEV
      SPVAL=9.9E10

C  ASSIGN UNIT NUMBERS 
C
C  FOR 12-hr TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
C   DATA FOR THE PREVIOUS 11 HOURS
       LUGB=11
       LUGI=12

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

C GSM  READ HRRR FILE 
C  READ INDEX FILE TO GET GRID SPECS
C
      IRGI = 1
      IRGS = 1
      KMAX = 0
      JR=0
      KSKIP = 0
      CALL BAOPEN(LUGB,'fort.11',IRETGB)
      CALL BAOPEN(LUGI,'fort.12',IRETGI)
      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
      write(6,*)' IRET FROM GETGI ',IRGI
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
        RETURN
      ENDIF
c      REWIND LUGI

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO

C    GET GRID NUMBER FROM PDS
C
      IGDNUM = KPDS(3)
C
C   PROCESS THE HRRR GRIB FILE
C
      IMAX = KGDS(2)
      JMAX = KGDS(3)
      NUMVAL = IMAX*JMAX
      KMAX = MAXLEV
      WRITE(6,280) IMAX,JMAX,NUMLEV,KMAX
  280 FORMAT(' IMAX,JMAX,NUMLEV,KMAX ',5I4)

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &               KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
        write(6,*)' IRET FROM GETGB1S ',IRGS
        IF(IRGI .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          ISTAT = IRGS
          RETURN
        ENDIF
C
      ENDDO

      print *, 'about to unpack the HRRR file'

C  UNPACK THE HRRR FILE

c   get the vertical profile of pressure
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 001
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
           PMID(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK PMID ', LL, IRET
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of height
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 007
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1,ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           HGHT(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK HEIGHT ', LL
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of temperature

      DO M=1,ILIM
      DO N=1,JLIM
        VALPT(M,N)=.TRUE.
      ENDDO
      ENDDO

      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 011
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           T(M,N,LL) = GRID(KK)
           IF (LL. EQ. 1) THEN
             IF (T(M,N,LL) .lt. 5.) THEN
               VALPT(M,N)=.FALSE.
             ENDIF
           ENDIF
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK TEMP ', LL
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of q
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 051
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           Q(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK SPEC HUM ', LL
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of u
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 033
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           UWND(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK U WIND ', LL
         ISTAT = IRET
       ENDIF
      ENDDO

c   get the vertical profile of v
      J=0
      DO LL=1,MAXLEV
       JPDS = -1
       JPDS(3) = IGDNUM
       JPDS(5) = 034
       JPDS(6) = 109
       CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
       J=K
       IF(IRET.EQ.0) THEN
         DO KK = 1, ITOT
           IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
           ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
           ENDIF
           VWND(M,N,LL) = GRID(KK)
         ENDDO
       ELSE
        WRITE(6,*)' COULD NOT UNPACK V WIND ', LL
         ISTAT = IRET
       ENDIF
      ENDDO

c  get sfc height 
C
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 007
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          ZSFC(M,N) = GRID(KK)
          IF (ZSFC(M,N).LT.0.0) ZSFC(M,N)=0.0
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK SFC HEIGHT ', IRET
         ISTAT = IRET
       RETURN
      ENDIF

c get surface pressure
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 001
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          PSFC(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK SFC P '
         ISTAT = IRET
       RETURN
      ENDIF

c get sea level pressure
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 129 
      JPDS(6) = 102 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          PMSL(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK SLP '
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m temp
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 11 
      JPDS(6) = 105 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          T2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 2-M TEMP '
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m spec hum
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 51 
      JPDS(6) = 105
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          Q2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 2-M Q'
         ISTAT = IRET
       RETURN
      ENDIF

c 2-m dew point 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 17 
      JPDS(6) = 105
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          D2(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 2-M DEW '
         ISTAT = IRET
       RETURN
      ENDIF

c 10-m U 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 33 
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          U10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 10-M U '
         ISTAT = IRET
       RETURN
      ENDIF

c 10-m V
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 34
      JPDS(6) = 105
      JPDS(7) = 10
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          V10(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK 10-M V '
         ISTAT = IRET
       RETURN
      ENDIF

c vegetation fraction
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 225
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          VEG(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK VEG '
         ISTAT = IRET
       RETURN
      ENDIF

c land mask
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 081 
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          COAST(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK LAND '
         ISTAT = IRET
       RETURN
      ENDIF

c visibility 
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 020
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          VSBY(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK VIS '
         ISTAT = IRET
       RETURN
      ENDIF

c sfc wind gust
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 180
      JPDS(6) = 001
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          GUST(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK GUST'
         ISTAT = IRET
       RETURN
      ENDIF

c total cloud fraction
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 71
      JPDS(6) = 200
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          TCLD(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK TOTAL CLD'
         ISTAT = IRET
       RETURN
      ENDIF

c cloud base height
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 7
      JPDS(6) = 2
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          ZBASE(M,N) = GRID(KK)
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK CLOUD BASE Z'
         ISTAT = IRET
       RETURN
      ENDIF

c ceiling
      J = 0
      JPDS = -1
      JPDS(3) = IGDNUM
      JPDS(5) = 7
      JPDS(6) = 215 
      CALL GETGB(LUGB,LUGI,NUMVAL,J,JPDS,JGDS,KF,K,
     X           KPDS,KGDS,MASK,GRID,IRET)
      IF(IRET.EQ.0) THEN
        DO KK = 1, ITOT
          IF(MOD(KK,ILIM).EQ.0) THEN
            M=ILIM
            N=INT(KK/ILIM)
          ELSE
            M=MOD(KK,ILIM)
            N=INT(KK/ILIM) + 1
          ENDIF
          CEILING(M,N) = GRID(KK)
          IF (CEILING(M,N) .EQ. 0.0) THEN
            CEILING(M,N)=SPVAL
          ENDIF 
        ENDDO
      ELSE
        WRITE(6,*)' COULD NOT UNPACK CEILING'
         ISTAT = IRET
       RETURN
      ENDIF

      print *, 'done with unpacking'
      RETURN
      END
