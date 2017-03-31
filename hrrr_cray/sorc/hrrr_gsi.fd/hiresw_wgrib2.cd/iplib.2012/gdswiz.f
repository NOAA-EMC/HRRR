C-----------------------------------------------------------------------
      SUBROUTINE GDSWIZ(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                  LROT,CROT,SROT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  GDSWIZ     GRID DESCRIPTION SECTION WIZARD
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT= 0) GRID AND EARTH COORDINATES OF ALL GRID POINTS
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=201) STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=203) E-STAGGERED ROTATED EQUIDISTANT CYLINDRICAL 2-D
C             (KGDS(1)=205) B-STAGGERED ROTATED EQUIDISTANT CYLINDRICAL 2-D
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.  ALSO IF IOPT=0,
C           IF THE NUMBER OF GRID POINTS EXCEEDS THE NUMBER ALLOTTED,
C           THEN ALL THE OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
C
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C   98-08-20  BALDWIN  ADD TYPE 203 STAGGERED 2-D ETA GRIDS
C   08-04-11  GAYNO    ADD TYPE 205 B-STAGGERED ROT LAT/LON GRIDS
C
C USAGE:    CALL GDSWIZ(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
C     &                 LROT,CROT,SROT)
C
C   INPUT ARGUMENT LIST:
C     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
C     IOPT     - INTEGER OPTION FLAG
C                ( 0 TO COMPUTE EARTH COORDS OF ALL THE GRID POINTS)
C                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
C                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
C     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
C     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
C                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
C                (ACCEPTABLE RANGE: -360. TO 360.)
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
C                (ACCEPTABLE RANGE: -90. TO 90.)
C     LROT     - INTEGER FLAG TO RETURN VECTOR ROTATIONS IF 1
C
C   OUTPUT ARGUMENT LIST:
C     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<=0
C     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<=0
C     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>=0
C     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>=0
C     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
C                (-1 IF PROJECTION UNRECOGNIZED)
C     CROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES IF LROT=1
C     SROT     - REAL (NPTS) CLOCKWISE VECTOR ROTATION SINES IF LROT=1
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C SUBPROGRAMS CALLED:
C   GDSWIZ00     GDS WIZARD FOR EQUIDISTANT CYLINDRICAL
C   GDSWIZ01     GDS WIZARD FOR MERCATOR CYLINDRICAL
C   GDSWIZ03     GDS WIZARD FOR LAMBERT CONFORMAL CONICAL
C   GDSWIZ04     GDS WIZARD FOR GAUSSIAN CYLINDRICAL
C   GDSWIZ05     GDS WIZARD FOR POLAR STEREOGRAPHIC AZIMUTHAL
C   GDSWIZC9     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL
C   GDSWIZCA     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL
C   GDSWIZCB     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL 2-D
C   GDSWIZCD     GDS WIZARD FOR ROTATED EQUIDISTANT CYLINDRICAL 2-D
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      INTEGER KGDS(200)
      REAL XPTS(NPTS),YPTS(NPTS),RLON(NPTS),RLAT(NPTS)
      REAL CROT(NPTS),SROT(NPTS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE GRID COORDINATES FOR ALL GRID POINTS
      IF(IOPT.EQ.0) THEN
        IF(KGDS(1).EQ.201) THEN
          IM=KGDS(7)*2-1
          JM=KGDS(8)
          KSCAN=MOD(KGDS(11)/256,2)
          IF(KSCAN.EQ.0) THEN
            IS1=(JM+1)/2
            NM=(IM/2+1)*JM-JM/2
          ELSE
            IS1=JM/2
            NM=IM/2*JM+JM/2
          ENDIF
        ELSEIF(KGDS(1).EQ.202) THEN
          IM=KGDS(7)
          JM=KGDS(8)
          NM=IM*JM
        ELSEIF(KGDS(1).EQ.203) THEN
          IM=KGDS(2)
          JM=KGDS(3)
          NM=IM*JM
          KSCAN=MOD(KGDS(11)/256,2)
          IF(KSCAN.EQ.0) THEN
            IS1=(JM+1)/2
          ELSE
            IS1=JM/2
          ENDIF
        ELSE
          IM=KGDS(2)
          JM=KGDS(3)
          NM=IM*JM
        ENDIF
        NSCAN=MOD(KGDS(11)/32,2)
        IF(NM.LE.NPTS) THEN
          IF(KGDS(1).EQ.201) THEN
            DO N=1,NM
              NN=2*N-1+KSCAN
              IF(NSCAN.EQ.0) THEN
                J=(NN-1)/IM+1
                I=NN-IM*(J-1)
              ELSE
                I=(NN-1)/JM+1
                J=NN-JM*(I-1)
              ENDIF
              XPTS(N)=IS1+(I-(J-KSCAN))/2
              YPTS(N)=(I+(J-KSCAN))/2
            ENDDO
          ELSEIF(KGDS(1).EQ.203) THEN
            DO N=1,NM
              IF(NSCAN.EQ.0) THEN
                J=(N-1)/IM+1
                I=(N-IM*(J-1))*2-MOD(J+KSCAN,2)
              ELSE
                I=(N-1)/JM+1
                J=(N-JM*(I-1))*2-MOD(I+KSCAN,2)
              ENDIF
              XPTS(N)=IS1+(I-(J-KSCAN))/2
              YPTS(N)=(I+(J-KSCAN))/2
            ENDDO
          ELSE
            DO N=1,NM
              IF(NSCAN.EQ.0) THEN
                J=(N-1)/IM+1
                I=N-IM*(J-1)
              ELSE
                I=(N-1)/JM+1
                J=N-JM*(I-1)
              ENDIF
              XPTS(N)=I
              YPTS(N)=J
            ENDDO
          ENDIF
          DO N=NM+1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ELSE
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
        IOPF=1
      ELSE
        IOPF=IOPT
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EQUIDISTANT CYLINDRICAL
      IF(KGDS(1).EQ.000) THEN
        CALL GDSWIZ00(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MERCATOR CYLINDRICAL
      ELSEIF(KGDS(1).EQ.001) THEN
        CALL GDSWIZ01(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LAMBERT CONFORMAL CONICAL
      ELSEIF(KGDS(1).EQ.003) THEN
        CALL GDSWIZ03(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GAUSSIAN CYLINDRICAL
      ELSEIF(KGDS(1).EQ.004) THEN
        CALL GDSWIZ04(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  POLAR STEREOGRAPHIC AZIMUTHAL
      ELSEIF(KGDS(1).EQ.005) THEN
        CALL GDSWIZ05(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
      ELSEIF(KGDS(1).EQ.201) THEN
        CALL GDSWIZC9(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROTATED EQUIDISTANT CYLINDRICAL
      ELSEIF(KGDS(1).EQ.202) THEN
        CALL GDSWIZCA(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  E-STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
      ELSEIF(KGDS(1).EQ.203) THEN
        CALL GDSWIZCB(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  B-STAGGERED ROTATED EQUIDISTANT CYLINDRICAL
      ELSEIF(KGDS(1).EQ.205) THEN
        CALL GDSWIZCD(KGDS,IOPF,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
     &                LROT,CROT,SROT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PROJECTION UNRECOGNIZED
      ELSE
        IRET=-1
        IF(IOPT.GE.0) THEN
          DO N=1,NPTS
            RLON(N)=FILL
            RLAT(N)=FILL
          ENDDO
        ENDIF
        IF(IOPT.LE.0) THEN
          DO N=1,NPTS
            XPTS(N)=FILL
            YPTS(N)=FILL
          ENDDO
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
