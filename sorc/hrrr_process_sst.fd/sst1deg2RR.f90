!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& 
      SUBROUTINE SST1deg2RR (SSTLL,SST,GLAT,GLON,nx,ny, &
                           IMON,IDAY,istatus)
!
!
      implicit real (A-H, O-Z)
!
      character*80 input_file
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
!
      DIMENSION SSTLL(361,180),SALTLK(24),SALTLA(2),SALTLO(2)
!
      DIMENSION  SST(nx,ny), SM(nx,ny), GLAT(nx,ny), GLON(nx,ny)
!
      DATA   INSST/39/
      DATA   INDXST/0/
!     DATA  INDXST/40/


!     DATA SALTLK/273.38,274.27,278.50,283.01,287.33,293.41
!    1,           297.13,297.73,294.97,289.58,282.31,275.67/

      DATA SALTLK   &
         /0.5, 0.,-0.5, 3., 4., 7., 8., 12., 13., 16., 19., 21.,   &
         23.5, 25.,26.,24.,23.,20.5,18.,15., 11.5, 8.,  4.,  1./

!
!     CORNERS OF SALT LAKE LATITUDE/LONGITUDE BOX
!     in degrees---> 40.0     42.0            111.0    114.0
      data saltla   /40.0  ,  42.0/, saltlo/ -111.0 , -114.0/

!     DATA SALTLA/0.698132,0.733038/,SALTLO/1.937315,1.989675/
!
!
!----  INTERPOLATE 1-DEG GLOBAL SST TO ETA GRID  -------
!
!-CP NOTE:  THIS SUBROUTINE AND INTERPOLATION ALGORITHM ASSUME
!-CP A 1-DEG GLOBAL SST FIELD IN THE FOLLOWING FORMAT:  
!-CP
!-CP  I=1 AT 0.5 E,  I=2 AT 1.5 E, ... , I=360 at 0.5W
!-CP  J=1 AT 89.5S, J=2 AT 88.5 S, ..., J=180 at 89.5N
!-CP  
!-CP In the interpolation algorithm below, glon is positive westward,
!-CP from 0 to 360, with 0 at the greenwich meridian.  Elon is positive 
!-CP eastward, thus the need to subtract glon from 360 to get the index
!-CP of the correct oisst point.  If your input 1 deg SST field is in
!-CP a different indexing scheme, you will need to change the algorithm
!-CP below - see "grdeta.oldoi"
!-CP
      DO 185 j=1,ny
      DO 185 i=1,nx
        ELAT=H90+GLAT(i,j)
        ELON=H360+GLON(i,j)
        IF(ELON.GT.H360)ELON=ELON-H360
        ILON1=INT(ELON)
        DIF=ELON-ILON1
        IF(DIF.GT.D5)ILON1=ILON1+1
        IF(ILON1.EQ.D00)ILON1=360
        ILON2=ILON1+1
        ILAT1=INT(ELAT)
        DIF=ELAT-ILAT1
        IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
!     IF(ILAT1.EQ.180.OR.ILAT1.EQ.0)THEN
!       WRITE(6,6788)K,GLAT(K),GLON(K),ELAT,ELON
!6788   FORMAT(' K=',I4,' GLAT=',E12.5,' GLON=',E12.5,' ELAT=',E12.5,
!    1   ' ELON=',E12.5)
!       STOP 333
!     ENDIF
        ILAT2=ILAT1+1
        W1=ELON-ILON1+D5
        IF(W1.LT.D00)W1=W1+H360
        W2=ELAT-ILAT1+D5
        AR1=W1*W2
        AR2=W1*(H1-W2)
        AR3=(H1-W1)*(H1-W2)
        AR4=(H1-W1)*W2
        SST(i,j) = AR1*SSTLL(ILON2,ILAT2)+AR2*SSTLL(ILON2,ILAT1)+    &
                  AR3*SSTLL(ILON1,ILAT1)+AR4*SSTLL(ILON1,ILAT2)
 185  CONTINUE

!***
!***  INSERT TEMPERATURES FOR THE GREAT SALT LAKE
!***
!!!!!!!   goto 4500
! --- ID1 = biweekly period (2 per month)
!     ID2 = day within biweekly period

      ID1=2.*IMON -1 + iday/15
      ID2=mod(IDAY,15)

      MARG0=ID1-1
      IF (MARG0.LT.1)MARG0=24
      MNTH0=15
      MNTH1=15
      IF(ID2.LT.8)THEN
        NUMER=ID2+MNTH0-8
        DENOM=MNTH0
        IARG1=MARG0
        IARG2=ID1
      ELSE
        NUMER=ID2-8
        DENOM=MNTH1
        IARG1=ID1
        IARG2=ID1+1
        IF(IARG2.GT.24)IARG2=1
      ENDIF
      FRAC=NUMER/DENOM
      DO j=1,ny
      DO i=1,nx
        IF(GLAT(i,j).GT.SALTLA(1).AND.GLAT(i,j).LT.SALTLA(2).and. &
          GLON(i,j).GT.SALTLO(1).AND.GLON(i,j).LT.SALTLO(2))THEN  
              SST(i,j)=SALTLK(IARG1)+                             &
                    (SALTLK(IARG2)-SALTLK(IARG1))*FRAC
        ENDIF
      ENDDO
      ENDDO
!
      istatus = 0
 4500 CONTINUE
      RETURN
!
      WRITE (IOUTUPRT, 4550) INSST
 4550 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN SST        ',  &
                   'ON UNIT', I3, ' GRIB ' /                      &
              ' ', 'EXECUTION TERMINATING.')
!
      istatus = -1
      return
!
      END SUBROUTINE SST1deg2RR
