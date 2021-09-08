      SUBROUTINE SST14K (SST,GLAT,GLON,vegtyp,iwater,ilake,nx,ny)
!
      IMPLICIT REAL (A-H, O-Z)
!
      parameter (imy=1041,jmy=441)
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
      PARAMETER  (RAD2D=57.29578E0,PI=3.141592654)
!
!
!
      DIMENSION    SST14   (IMY,JMY)
!
      DIMENSION  SST(nx,ny), GLAT(nx,ny), GLON(nx,ny)
      real       vegtyp(nx,ny)
      integer    iwater, ilake
      real       water, lake
      integer    ncount
!
!
      DATA   INSST/44/
!
!**************************  BEGIN EXECUTION ***********************
!
!
!  READ HI RESOLUTION 14 KM OPC SST ANALYSIS, CONVERT To KELVIN
!
!     *****change by schwartz to refer to file with a link 10-11-00
!
      water=float(iwater)
      lake =float(ilake)

      open(unit=insst,file='SST14km',convert='big_endian',  &
      status='old',form='unformatted',                       &
      err=20)
      go to 22
 20   write(6,21) 
 21   format('cannot find SST HIRES FILE')
      return
!    
!     ***************************
!  
 22   DO 25 I = 1, IMY
       READ (INSST,ERR=200,END=210) (SST14(I,J),J=1,JMY)
   25 CONTINUE

      do 30 j=1,jmy
      do 30 i=1,imy
       sst14(i,j) = sst14(i,j) + 273.15
   30 continue

!
!
!----  INTERPOLATE 1/8 DEG GLOBAL SATELLITE SST TO ETA GRID  -------
!
      ncount=0
      DO 100 j=1,ny
      DO 100 i=1,nx
        IF (GLAT(i,j).LT.65.0.AND.GLAT(i,j).GT.10.0 .AND.   &
        glon(i,j).LT.-35.0.AND.glon(i,j).GT.-160.0) then
! 2 lines below limit area to Great Lakes
!        IF (GLAT(i,j).LT.50.0.AND.GLAT(i,j).GT.40.0 .AND.   &
!        glon(i,j).LT.-74.0.AND.glon(i,j).GT.-94.0) then
! below - exclude GREAT LAKES
        IF (GLAT(i,j).GT.50.0.OR.GLAT(i,j).LT.40.0 .OR.   &
        glon(i,j).GT.-74.0.OR.glon(i,j).LT.-94.0) then


        ncount=ncount+1
!      --- skip to end if land point
!        if (vegtyp(i,j).ne.water .and. vegtyp(i,j).ne.lake) go to 100
        if (vegtyp(i,j).ne.lake) go to 100
! update only lake temperature, except for Great Lakes
        ii = int((glon(i,j) + 165.0) * 8.) + 1
        jj = int((glat(i,j) - 10.0 ) * 8.) + 1

        sst(i,j) = sst14(ii,jj)

        ELSE

!          print *,' sst14k, point outside of bounds',i,j
!          print *,' This should NOT happen -- check!'
         ENDIF

        END IF
  100 CONTINUE
!        print *,'number of replaced points for Great Lakes =', ncount

!
!   REACHING HERE MEANS 14KM SST READ OK
!
      RETURN
!
!   REACHING HERE MEANS SOMETHING IS WRONG
!
  200 CONTINUE      !  SOME KIND OF ERROR READING FILE
      WRITE(6,555) INSST
  210 CONTINUE      !  HIT UNEXPECTED END O' FILE
      WRITE(6,556) INSST
  555 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN 14 KM SST        ', &
                   'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
  556 FORMAT ('0', 'HIT UNEXPECTED END OF FILE READING 14K SST',       & 
                   'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
      RETURN
      END SUBROUTINE
