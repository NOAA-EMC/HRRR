 MODULE GDSWZD05_MOD
!$$$  MODULE DOCUMENTATION BLOCK
!
! MODULE:  GDSWZD05_MOD  GDS WIZARD MODULE FOR POLAR STEREOGRAPHIC
!                        AZIMUTHAL GRIDS.
!   PRGMMR: GAYNO     ORG: W/NMC23       DATE: 2015-01-21
!
! ABSTRACT: - CONVERT FROM EARTH TO GRID COORDINATES OR VICE VERSA
!             (SPHERICAL OR ELLIPTICAL EARTH).
!           - COMPUTE VECTOR ROTATION SINES AND COSINES.
!             (SPHERICAL OR ELLIPTICAL EARTH).
!           - COMPUTE MAP JACOBIANS (SPHERICAL EARTH).
!           - COMPUTE GRID BOX AREA (SPHERICAL EARTH).
!
! PROGRAM HISTORY LOG:
!   2015-01-21  GAYNO   INITIAL VERSION FROM A MERGER OF
!                       ROUTINES GDSWIZ05 AND GDSWZD05.
!
! USAGE:  "USE GDSWZD05_MOD"  THEN CALL THE PUBLIC DRIVER
!         ROUTINE "GDSWZD05".
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
 IMPLICIT NONE

 PRIVATE

 PUBLIC                          :: GDSWZD05

 REAL,             PARAMETER     :: RERTH=6.3712E6
 REAL,             PARAMETER     :: RERTH_WGS84=6.378137E6
 REAL,             PARAMETER     :: PI=3.14159265358979
 REAL,             PARAMETER     :: DPR=180./PI
 REAL,             PARAMETER     :: PI2=PI/2.0
 REAL,             PARAMETER     :: PI4=PI/4.0
 REAL,             PARAMETER     :: E2=.00669437999013  ! wgs84 datum
 REAL,             PARAMETER     :: SLAT=60.0  ! standard latitude according
                                               ! to grib 1 standard
 REAL,             PARAMETER     :: SLATR=SLAT/DPR

 INTEGER                         :: IROT

 REAL                            :: DE2, DR2, DXS, DYS, H, ORIENT

 CONTAINS

 SUBROUTINE GDSWZD05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET, &
                     CROT,SROT,XLON,XLAT,YLON,YLAT,AREA)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  GDSWZD05   GDS WIZARD FOR POLAR STEREOGRAPHIC AZIMUTHAL
!   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
!
! ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
!           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
!           AND RETURNS ONE OF THE FOLLOWING:
!             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
!             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
!           FOR POLAR STEREOGRAPHIC AZIMUTHAL PROJECTIONS.
!           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
!           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
!           OUTPUT ELEMENTS ARE SET TO FILL VALUES.
!           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.
!           OPTIONALLY, THE VECTOR ROTATIONS, MAP JACOBIANS, AND
!           GRID BOX AREAS MAY BE RETURNED AS WELL.  ROUTINE WORKS
!           FOR BOTH SPHERICAL AND ELLIPTICAL EARTHS WITH THE
!           EXCEPTION OF THE MAP JACOBIANS AND GRID BOX AREAS, WHICH
!           ARE ONLY COMPUTED FOR SPHERICAL EARTHS.  TO COMPUTE
!           THE VECTOR ROTATIONS, THE OPTIONAL ARGUMENTS 'SROT' AND 'CROT'
!           MUST BE PRESENT.  TO COMPUTE THE MAP JACOBIANS, THE
!           OPTIONAL ARGUMENTS 'XLON', 'XLAT', 'YLON', 'YLAT' MUST BE PRESENT.
!           TO COMPUTE THE GRID BOX AREAS, THE OPTIONAL ARGUMENT
!           'AREA' MUST BE PRESENT.
!
! PROGRAM HISTORY LOG:
!   96-04-10  IREDELL
!   97-10-20  IREDELL  INCLUDE MAP OPTIONS
!   09-05-13  GAYNO    ENSURE AREA ALWAYS POSITIVE
! 2015-01-21  GAYNO    MERGER OF GDSWIZ05 AND GDSWZD05.  MAKE
!                      CROT,SORT,XLON,XLAT,YLON,YLAT AND AREA
!                      OPTIONAL ARGUMENTS.  MAKE PART OF A MODULE.
!                      MOVE VECTOR ROTATION, MAP JACOBIAN AND GRID
!                      BOX AREA COMPUTATIONS TO SEPARATE SUBROUTINES.
!                      INCLUDE OPTION FOR ELLIPTICAL EARTHS.
!
! USAGE:    CALL GDSWZD05(KGDS,IOPT,NPTS,FILL,XPTS,YPTS,RLON,RLAT,NRET,
!    &                    CROT,SROT,XLON,XLAT,YLON,YLAT,AREA)
!
!   INPUT ARGUMENT LIST:
!     KGDS     - INTEGER (200) GDS PARAMETERS AS DECODED BY W3FI63
!     IOPT     - INTEGER OPTION FLAG
!                (+1 TO COMPUTE EARTH COORDS OF SELECTED GRID COORDS)
!                (-1 TO COMPUTE GRID COORDS OF SELECTED EARTH COORDS)
!     NPTS     - INTEGER MAXIMUM NUMBER OF COORDINATES
!     FILL     - REAL FILL VALUE TO SET INVALID OUTPUT DATA
!                (MUST BE IMPOSSIBLE VALUE; SUGGESTED VALUE: -9999.)
!     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT>0
!     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT>0
!     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT<0
!                (ACCEPTABLE RANGE: -360. TO 360.)
!     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT<0
!                (ACCEPTABLE RANGE: -90. TO 90.)
!
!   OUTPUT ARGUMENT LIST:
!     XPTS     - REAL (NPTS) GRID X POINT COORDINATES IF IOPT<0
!     YPTS     - REAL (NPTS) GRID Y POINT COORDINATES IF IOPT<0
!     RLON     - REAL (NPTS) EARTH LONGITUDES IN DEGREES E IF IOPT>0
!     RLAT     - REAL (NPTS) EARTH LATITUDES IN DEGREES N IF IOPT>0
!     NRET     - INTEGER NUMBER OF VALID POINTS COMPUTED
!     CROT     - REAL, OPTIONAL (NPTS) CLOCKWISE VECTOR ROTATION COSINES
!     SROT     - REAL, OPTIONAL (NPTS) CLOCKWISE VECTOR ROTATION SINES
!                (UGRID=CROT*UEARTH-SROT*VEARTH;
!                 VGRID=SROT*UEARTH+CROT*VEARTH)
!     XLON     - REAL, OPTIONAL (NPTS) DX/DLON IN 1/DEGREES
!     XLAT     - REAL, OPTIONAL (NPTS) DX/DLAT IN 1/DEGREES
!     YLON     - REAL, OPTIONAL (NPTS) DY/DLON IN 1/DEGREES
!     YLAT     - REAL, OPTIONAL (NPTS) DY/DLAT IN 1/DEGREES
!     AREA     - REAL, OPTIONAL (NPTS) AREA WEIGHTS IN M**2
!                (PROPORTIONAL TO THE SQUARE OF THE MAP FACTOR)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
 IMPLICIT NONE
!
 INTEGER,          INTENT(IN   ) :: IOPT, KGDS(200), NPTS
 INTEGER,          INTENT(  OUT) :: NRET
!
 REAL,             INTENT(IN   ) :: FILL
 REAL,             INTENT(INOUT) :: RLON(NPTS),RLAT(NPTS)
 REAL,             INTENT(INOUT) :: XPTS(NPTS),YPTS(NPTS)
 REAL, OPTIONAL,   INTENT(  OUT) :: CROT(NPTS),SROT(NPTS)
 REAL, OPTIONAL,   INTENT(  OUT) :: XLON(NPTS),XLAT(NPTS)
 REAL, OPTIONAL,   INTENT(  OUT) :: YLON(NPTS),YLAT(NPTS),AREA(NPTS)
!
 INTEGER                         :: IM, JM, IPROJ
 INTEGER                         :: ISCAN, JSCAN, ITER, N
!
 LOGICAL                         :: ELLIPTICAL, LROT, LMAP, LAREA
!
 REAL                            :: ALAT, ALAT1, ALONG, DIFF
 REAL                            :: DI, DJ, DE
 REAL                            :: DX, DY
 REAL                            :: DR, E, E_OVER_2, HI, HJ
 REAL                            :: MC
 REAL                            :: RLAT1, RLON1, RHO, T, TC
 REAL                            :: XMAX, XMIN, YMAX, YMIN
 REAL                            :: XP, YP
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 IF(PRESENT(CROT)) CROT=FILL
 IF(PRESENT(SROT)) SROT=FILL
 IF(PRESENT(XLON)) XLON=FILL
 IF(PRESENT(XLAT)) XLAT=FILL
 IF(PRESENT(YLON)) YLON=FILL
 IF(PRESENT(YLAT)) YLAT=FILL
 IF(PRESENT(AREA)) AREA=FILL
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 IF(KGDS(1).EQ.005) THEN
   ELLIPTICAL=MOD(KGDS(6)/64,2).EQ.1
   IM=KGDS(2)
   JM=KGDS(3)
   RLAT1=KGDS(4)*1.E-3
   RLON1=KGDS(5)*1.E-3
   IROT=MOD(KGDS(6)/8,2)
   ORIENT=KGDS(7)*1.E-3
   DX=KGDS(8)
   DY=KGDS(9)
   IPROJ=MOD(KGDS(10)/128,2)
   ISCAN=MOD(KGDS(11)/128,2)
   JSCAN=MOD(KGDS(11)/64,2)
   H=(-1.)**IPROJ
   HI=(-1.)**ISCAN
   HJ=(-1.)**(1-JSCAN)
   DXS=DX*HI
   DYS=DY*HJ
   IF(H.EQ.-1)ORIENT=ORIENT+180.
!
! FIND X/Y OF POLE
   IF (.NOT.ELLIPTICAL) THEN
     DE=(1.+SIN(SLATR))*RERTH
     DR=DE*COS(RLAT1/DPR)/(1+H*SIN(RLAT1/DPR))
     XP=1-H*SIN((RLON1-ORIENT)/DPR)*DR/DXS
     YP=1+COS((RLON1-ORIENT)/DPR)*DR/DYS
     DE2=DE**2
   ELSE
     E=SQRT(E2)
     E_OVER_2=E*0.5
     ALAT=H*RLAT1/DPR
     ALONG = (RLON1-ORIENT)/DPR
     T=TAN(PI4-ALAT/2.)/((1.-E*SIN(ALAT))/  &
       (1.+E*SIN(ALAT)))**(E_OVER_2)
     TC=TAN(PI4-SLATR/2.)/((1.-E*SIN(SLATR))/  &
       (1.+E*SIN(SLATR)))**(E_OVER_2)
     MC=COS(SLATR)/SQRT(1.0-E2*(SIN(SLATR)**2))
     RHO=RERTH_WGS84*MC*T/TC
     YP = 1.0 + RHO*COS(H*ALONG)/DYS
     XP = 1.0 - RHO*SIN(H*ALONG)/DXS
   ENDIF ! ELLIPTICAL
   XMIN=0
   XMAX=IM+1
   YMIN=0
   YMAX=JM+1
   NRET=0
   IF(PRESENT(CROT).AND.PRESENT(SROT))THEN
     LROT=.TRUE.
   ELSE
     LROT=.FALSE.
   ENDIF
   IF(PRESENT(XLON).AND.PRESENT(XLAT).AND.PRESENT(YLON).AND.PRESENT(YLAT))THEN
     LMAP=.TRUE.
   ELSE
     LMAP=.FALSE.
   ENDIF
   IF(PRESENT(AREA))THEN
     LAREA=.TRUE.
   ELSE
     LAREA=.FALSE.
   ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
   IF(IOPT.EQ.0.OR.IOPT.EQ.1) THEN
     IF(.NOT.ELLIPTICAL)THEN
       DO N=1,NPTS
         IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND. &
            YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
           DI=(XPTS(N)-XP)*DXS
           DJ=(YPTS(N)-YP)*DYS
           DR2=DI**2+DJ**2
           IF(DR2.LT.DE2*1.E-6) THEN
             RLON(N)=0.
             RLAT(N)=H*90.
           ELSE
             RLON(N)=MOD(ORIENT+H*DPR*ATAN2(DI,-DJ)+3600,360.)
             RLAT(N)=H*DPR*ASIN((DE2-DR2)/(DE2+DR2))
           ENDIF
           NRET=NRET+1
           IF(LROT) CALL GDSWZD05_VECT_ROT(RLON(N),CROT(N),SROT(N))
           IF(LMAP) CALL GDSWZD05_MAP_JACOB(RLON(N),RLAT(N), &
                                            XLON(N),XLAT(N),YLON(N),YLAT(N))
           IF(LAREA) CALL GDSWZD05_GRID_AREA(RLAT(N),AREA(N))
         ELSE
           RLON(N)=FILL
           RLAT(N)=FILL
         ENDIF
       ENDDO
     ELSE ! ELLIPTICAL
       DO N=1,NPTS
         IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.  &
            YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
           DI=(XPTS(N)-XP)*DXS
           DJ=(YPTS(N)-YP)*DYS
           RHO=SQRT(DI*DI+DJ*DJ)
           T=(RHO*TC)/(RERTH_WGS84*MC)
           IF(ABS(YPTS(N)-YP)<0.01)THEN
             IF(DI>0.0) ALONG=ORIENT+H*90.0
             IF(DI<=0.0) ALONG=ORIENT-H*90.0
           ELSE
             ALONG=ORIENT+H*ATAN(DI/(-DJ))*DPR
             IF(DJ>0) ALONG=ALONG+180.
           END IF
           ALAT1=PI2-2.0*ATAN(T)
           DO ITER=1,10
             ALAT = PI2 - 2.0*ATAN(T*(((1.0-E*SIN(ALAT1))/  &
                   (1.0+E*SIN(ALAT1)))**(E_OVER_2)))
             DIFF = ABS(ALAT-ALAT1)*DPR
             IF (DIFF < 0.000001) EXIT
             ALAT1=ALAT
           ENDDO
           RLAT(N)=H*ALAT*DPR
           RLON(N)=ALONG
           IF(RLON(N)<0.0) RLON(N)=RLON(N)+360.
           IF(RLON(N)>360.0) RLON(N)=RLON(N)-360.0
           NRET=NRET+1
           IF(LROT) CALL GDSWZD05_VECT_ROT(RLON(N),CROT(N),SROT(N))
         ELSE
           RLON(N)=FILL
           RLAT(N)=FILL
         ENDIF
       ENDDO
     ENDIF ! ELLIPTICAL
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  TRANSLATE EARTH COORDINATES TO GRID COORDINATES
   ELSEIF(IOPT.EQ.-1) THEN
     IF(.NOT.ELLIPTICAL)THEN
       DO N=1,NPTS
         IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND. &
                                       H*RLAT(N).NE.-90) THEN
           DR=DE*TAN((90-H*RLAT(N))/2/DPR)
           DR2=DR**2
           XPTS(N)=XP+H*SIN((RLON(N)-ORIENT)/DPR)*DR/DXS
           YPTS(N)=YP-COS((RLON(N)-ORIENT)/DPR)*DR/DYS
           IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND. &
              YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
             NRET=NRET+1
             IF(LROT) CALL GDSWZD05_VECT_ROT(RLON(N),CROT(N),SROT(N))
             IF(LMAP) CALL GDSWZD05_MAP_JACOB(RLON(N),RLAT(N), &
                                              XLON(N),XLAT(N),YLON(N),YLAT(N))
             IF(LAREA) CALL GDSWZD05_GRID_AREA(RLAT(N),AREA(N))
           ELSE
             XPTS(N)=FILL
             YPTS(N)=FILL
           ENDIF
         ELSE
           XPTS(N)=FILL
           YPTS(N)=FILL
         ENDIF
       ENDDO
     ELSE  ! ELLIPTICAL CASE
       DO N=1,NPTS
         IF(ABS(RLON(N)).LE.360.AND.ABS(RLAT(N)).LE.90.AND.  &
                                        H*RLAT(N).NE.-90) THEN
           ALAT = H*RLAT(N)/DPR
           ALONG = (RLON(N)-ORIENT)/DPR
           T=TAN(PI4-ALAT*0.5)/((1.-E*SIN(ALAT))/  &
             (1.+E*SIN(ALAT)))**(E_OVER_2)
           RHO=RERTH_WGS84*MC*T/TC
           XPTS(N)= XP + RHO*SIN(H*ALONG) / DXS
           YPTS(N)= YP - RHO*COS(H*ALONG) / DYS
           IF(XPTS(N).GE.XMIN.AND.XPTS(N).LE.XMAX.AND.  &
              YPTS(N).GE.YMIN.AND.YPTS(N).LE.YMAX) THEN
             NRET=NRET+1
             IF(LROT) CALL GDSWZD05_VECT_ROT(RLON(N),CROT(N),SROT(N))
           ELSE
             XPTS(N)=FILL
             YPTS(N)=FILL
           ENDIF
         ELSE
           XPTS(N)=FILL
           YPTS(N)=FILL
         ENDIF
       ENDDO
     ENDIF
   ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PROJECTION UNRECOGNIZED
 ELSE
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 END SUBROUTINE GDSWZD05
!
 SUBROUTINE GDSWZD05_VECT_ROT(RLON, CROT, SROT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  GDSWZD05_VECT_ROT   VECTOR ROTATION FIELDS FOR
!                                  POLAR STEREOGRAPHIC GRIDS.
!
!   PRGMMR: GAYNO     ORG: W/NMC23       DATE: 2015-01-21
!
! ABSTRACT: THIS SUBPROGRAM COMPUTES THE VECTOR ROTATION SINES AND
!           COSINES FOR A POLAR STEREOGRAPHIC AZIMUTHAL GRID.
!
! PROGRAM HISTORY LOG:
! 2015-01-21  GAYNO    INITIAL VERSION
!
! USAGE:    CALL GDSWZD05_VECT_ROT(RLON,CROT,SROT)
!
!   INPUT ARGUMENT LIST:
!     RLON     - GRID POINT LONGITUDE IN DEGREES (REAL)
!
!   OUTPUT ARGUMENT LIST:
!     CROT     - CLOCKWISE VECTOR ROTATION COSINES (REAL)
!     SROT     - CLOCKWISE VECTOR ROTATION SINES (REAL)
!                (UGRID=CROT*UEARTH-SROT*VEARTH;
!                 VGRID=SROT*UEARTH+CROT*VEARTH)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
 IMPLICIT NONE

 REAL,             INTENT(IN   ) :: RLON
 REAL,             INTENT(  OUT) :: CROT, SROT

 IF(IROT.EQ.1) THEN
   CROT=H*COS((RLON-ORIENT)/DPR)
   SROT=SIN((RLON-ORIENT)/DPR)
 ELSE
   CROT=1.
   SROT=0.
 ENDIF

 END SUBROUTINE GDSWZD05_VECT_ROT
!
 SUBROUTINE GDSWZD05_MAP_JACOB(RLON,RLAT,XLON,XLAT,YLON,YLAT)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  GDSWZD05_MAP_JACOB  MAP JACOBIANS FOR
!                                  POLAR STEREOGRAPHIC GRIDS.
!
!   PRGMMR: GAYNO     ORG: W/NMC23       DATE: 2015-01-21
!
! ABSTRACT: THIS SUBPROGRAM COMPUTES THE MAP JACOBIANS FOR
!           A POLAR STEREOGRAPHIC AZIMUTHAL GRID (SPHERICAL
!           EARTH).
!
! PROGRAM HISTORY LOG:
! 2015-01-21  GAYNO    INITIAL VERSION
!
! USAGE:  CALL GDSWZD05_MAP_JACOB(RLON,RLAT,XLON,XLAT,YLON,YLAT)
!
!   INPUT ARGUMENT LIST:
!     RLON     - LONGITUDE IN DEGREES (REAL)
!     RLAT     - LATITUDE IN DEGREES (REAL)
!
!   OUTPUT ARGUMENT LIST:
!     XLON     - DX/DLON IN 1/DEGREES (REAL)
!     XLAT     - DX/DLAT IN 1/DEGREES (REAL)
!     YLON     - DY/DLON IN 1/DEGREES (REAL)
!     YLAT     - DY/DLAT IN 1/DEGREES (REAL)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
 IMPLICIT NONE

 REAL,             INTENT(IN   ) :: RLON, RLAT
 REAL,             INTENT(  OUT) :: XLON, XLAT, YLON, YLAT

 REAL                            :: CLAT, DE, DR

 IF(DR2.LT.DE2*1.E-6) THEN
   DE=SQRT(DE2)
   XLON=0.
   XLAT=-SIN((RLON-ORIENT)/DPR)/DPR*DE/DXS/2
   YLON=0.
   YLAT=H*COS((RLON-ORIENT)/DPR)/DPR*DE/DYS/2
 ELSE
   DR=SQRT(DR2)
   CLAT=COS(RLAT/DPR)
   XLON=H*COS((RLON-ORIENT)/DPR)/DPR*DR/DXS
   XLAT=-SIN((RLON-ORIENT)/DPR)/DPR*DR/DXS/CLAT
   YLON=SIN((RLON-ORIENT)/DPR)/DPR*DR/DYS
   YLAT=H*COS((RLON-ORIENT)/DPR)/DPR*DR/DYS/CLAT
 ENDIF

 END SUBROUTINE GDSWZD05_MAP_JACOB
!
 SUBROUTINE GDSWZD05_GRID_AREA(RLAT, AREA)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:  GDSWZD05_GRID_AREA  GRID BOX AREA FOR
!                                  POLAR STEREOGRAPHIC GRIDS
!
!   PRGMMR: GAYNO     ORG: W/NMC23       DATE: 2015-01-21
!
! ABSTRACT: THIS SUBPROGRAM COMPUTES THE GRID BOX AREA FOR
!           A POLAR STEREOGRAPHIC AZIMUTHAL GRID (SPHERICAL
!           EARTH).
!
! PROGRAM HISTORY LOG:
! 2015-01-21  GAYNO    INITIAL VERSION
!
! USAGE:  CALL GDSWZD05_GRID_AREA(RLAT,AREA)
!
!   INPUT ARGUMENT LIST:
!     RLAT     - LATITUDE OF GRID POINT IN DEGREES (REAL)
!
!   OUTPUT ARGUMENT LIST:
!     AREA     - AREA WEIGHTS IN M**2 (REAL)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
!$$$
!
 IMPLICIT NONE

 REAL,             INTENT(IN   ) :: RLAT
 REAL,             INTENT(  OUT) :: AREA

 REAL                            :: CLAT

 IF(DR2.LT.DE2*1.E-6) THEN
   AREA=RERTH**2*ABS(DXS)*ABS(DYS)*4/DE2
 ELSE
   CLAT=COS(RLAT/DPR)
   AREA=RERTH**2*CLAT**2*ABS(DXS)*ABS(DYS)/DR2
 ENDIF

 END SUBROUTINE GDSWZD05_GRID_AREA

 END MODULE GDSWZD05_MOD
