SUBROUTINE sstGlobal2RR(f,imaskSST,xland,nlon,nlat,xlon,ylat,sstRR)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine map NESDIS SNOW/ICE data to RR grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  imssnow
!
!   OUTPUT FILES:  RRimssnow
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________


  implicit real (A-H, O-Z)

  REAL, intent(in):: F(4320,2160)
  INTEGER, intent(in):: imaskSST(4320,2160)
!  grid
  integer, intent(in) :: nlon,nlat
  real, intent(in):: xlon(nlon,nlat)    !
  real, intent(in):: ylat(nlon,nlat)    !
  real, intent(in):: xland(nlon,nlat)    !
!
  real, intent(out):: sstRR(nlon,nlat)    !

!
  integer :: iland,ncount,KOUNT,IPOINT,JPOINT
  real    :: AR1,AR2,AR3,AR4,AREA
  integer :: i,j,k,ifound,LL,JPE,JPB,IPE,IPB,NK,MK
  integer :: jmask1, jmask2, mkflip
!
  PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
  PARAMETER  (DELX=0.083333, DELY=0.083333)

!--------------------------------------------------------------------
!
! ****** NOW BEGIN MAJOR LOOP OVER ALL GRID POINTS *******
!
     ncount=0

    DO j=1,nlat
    DO i=1,nlon

! RR land-water mask ILAND: 0 - water, 1 - land
      ILAND = 1
      IF( int(xland(i,j)) == 0 ) ILAND=0
!

!
!--------------- DETERMINE LAT/LON OF GRID POINT -------------
!                    (HERE LONG WILL BE EAST LONG)
       ELAT=89.958333-ylat(i,j)
!        ELAT=H90+ylat(i,j)
        ELAT=max(1.,ELAT/DELY)
        ELON=H360-0.041667+xlon(i,j)
        IF(ELON.GT.H360)ELON=ELON-H360

        ELON=ELON/DELX

        ILON1=INT(ELON)
        DIF=ELON-ILON1
        IF(DIF.GT.D5)ILON1=MIN(ILON1+1,4319)
        IF(ILON1.EQ.D00)ILON1=4319
        ILON2=ILON1+1

        ILAT1=INT(ELAT)
        DIF=ELAT-ILAT1
        IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,2159)
    IF(ILAT1.EQ.2160.OR.ILAT1.EQ.0)THEN
       WRITE(6,6788)i,j,ylat(i,j),xlon(i,j),ELAT,ELON,ILAT1,ILON1
6788   FORMAT(' i=',I4,' j=',I4,' ylat=',E12.5,' xlon=',E12.5,' ELAT=',E12.5, &
              ' ELON=',E12.5,' ILAT1=',I7,' ILON1=',I7)
!       STOP 333
     ENDIF
        ILAT2=ILAT1+1
        W1=ELON-ILON1+D5
        IF(W1.LT.D00)W1=W1+H360/DELX
        W2=ELAT-ILAT1+D5
        AR1=W1*W2
        AR2=W1*(H1-W2)
        AR3=(H1-W1)*(H1-W2)
        AR4=(H1-W1)*W2

   IF (ILAND == 0 ) THEN
! Water points in RR domain
! Land/water mask goes from the South Pole north, while SST goes from North Pole south
! Need to flip J index for land/water mask
      jmask1=2160-ILAT1+1
      jmask2=2160-ILAT2+1

      if(imaskSST(ILON2,jmask2) == 0 .AND. imaskSST(ILON2,jmask1) == 0 .AND.   &
         imaskSST(ILON1,jmask1) == 0 .AND. imaskSST(ILON1,jmask2) == 0) THEN
! All 4 points in RTG_SST around the water point in RR are also water
           sstRR(i,j) = AR1*F(ILON2,ILAT2)+AR2*F(ILON2,ILAT1)+    &
                  AR3*F(ILON1,ILAT1)+AR4*F(ILON1,ILAT2)

       else
! nearest neighbor
           KOUNT = 0

        IF (imaskSST(ILON1,jmask1) == 0) THEN
           KOUNT  = KOUNT + 1
           AREA   = AR3
           IPOINT = ILON1
           JPOINT = ILAT1
        END IF
!
        IF( imaskSST(ILON1, jmask2) == 0 ) THEN
           KOUNT = KOUNT +1
           IF (KOUNT .EQ. 1) THEN
              IPOINT = ILON1
              JPOINT = ILAT2
           ELSEIF (AR4 .GT. AREA) THEN
              AREA   = AR4
              IPOINT = ILON1
              JPOINT = ILAT2
           END IF
        END IF
!
        IF( imaskSST(ILON2, jmask1) == 0 ) THEN
           KOUNT = KOUNT + 1
           IF (KOUNT .EQ. 1) THEN
              AREA   = AR2
              IPOINT = ILON2
              JPOINT = ILAT1
           ELSEIF (AR2 .GT. AREA) THEN
              AREA   = AR2
              IPOINT = ILON2
              JPOINT = ILAT1
           END IF
        END IF
!
!
        IF( imaskSST(ILON2, jmask2) == 0  ) THEN
           KOUNT = KOUNT + 1
           IF (KOUNT .EQ. 1) THEN
              AREA   = AR1
              IPOINT = ILON2
              JPOINT = ILAT2
           ELSEIF (AR1.GT. AREA) THEN
              AREA   = AR1
              IPOINT = ILON2
              JPOINT = ILAT2
           END IF
        END IF
!
!     DETERMINE SST USING NEAREST NEIGHBOR 
!
        IF(KOUNT .GT. 0) THEN
            sstRR(i,j) = F(IPOINT,JPOINT)
        ELSE

     print *,' Expanded serach for water point'
!
!         EXPAND SEARCH RADIUS AND TAKE FIRST WATER TYPE MATCH
!
            IPOINT = ILON1
            JPOINT = ILAT1
!
!  Define the frame (no. of grid points) over which to search for
!    a matching land/water type from IMS data for the model gridpoint.
            ifound=0
            DO LL=1,16
              JPE = MIN (2160, JPOINT+LL)
              JPB = MAX (1 , JPOINT-LL)
              IPE = MIN (4320, IPOINT+LL)
              IPB = MAX (1 , IPOINT-LL)
!
              DO NK=IPB,IPE
              DO MK=JPB,JPE
                   mkflip=2160-mk+1
                 IF ( imaskSST(nk,mkflip) == 0 .and. ifound ==0 ) THEN
                    sstRR(i,j) = F(NK,MK)
                    ifound=1
                 ENDIF
              ENDDO  ! MK
              ENDDO  ! NK
            ENDDO  ! LL
            IF (ifound==0) THEN
              ncount=ncount+1
               sstRR(i,j) = F(IPOINT,JPOINT)
      print *,' Points with ifound=0, i,j ', I,J,sstRR(i,j)        
            ENDIF
        ENDIF   !  KOUNT .GT. 0
!
       endif
      
      ELSE
! Land
          sstRR(i,j) = F(ILON1,ILAT1)
      ENDIF
!
    ENDDO  ! nlon
    ENDDO  ! nlat

      print *,' Number of points with ifound=0 ', ncount
                                                                                                                       
end subroutine sstGlobal2RR
