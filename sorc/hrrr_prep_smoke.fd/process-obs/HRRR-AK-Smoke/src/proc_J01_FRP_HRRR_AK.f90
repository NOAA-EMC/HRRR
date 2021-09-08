PROGRAM rviirs_j01_AK
IMPLICIT NONE

! The preprocessor for the VIIRS FRP data, originally from G.Pereira, 2017
! USAGE: pgf90 -tp amd64 -mcmodel=medium -Mlarge_arrays -o procviirs rviirs.f90
! RAR: updated in 2018

! Variables declaration
  CHARACTER(len=250)  :: input_viirs
  CHARACTER(len=250)  :: output
  CHARACTER(len=250)  :: lulcmap
  CHARACTER(len=80)   :: cab(1)
  CHARACTER(len=255)  :: line
  INTEGER             :: hour, minute

  INTEGER :: stat
  INTEGER :: yy, mm, dd, bowtie,per_anomaly
  INTEGER :: mask, confi
  INTEGER :: posy, posx, posxx
  INTEGER :: i, julday

  REAL :: lon_vi, lat_vi
  REAL :: t13
  REAL :: frp_v
  REAL :: parea
  REAL :: juldaydec,hourf   ! hourf is hour with fraction

! Variables to cross fire location with land use and land cover
! Dimensions are the size of product
! In this case MCD12Q1 2013 with 500 meters resolution
! Could be a better map to USA, but here we use 3km res. LU map
! This is a example
  INTEGER, PARAMETER ::  n_cols= 6040
  INTEGER, PARAMETER ::  n_rows= 4000

! Spatial Resolution of LULC map
!! DO NOT FORGET TO CHANGE IT IF MODIFICATIONS IN LULC PRODUCT WAS MADE
  REAL  :: resx, resy
! Position in cartesian matrix
  INTEGER  :: posix, posiy

! Test to lower data
!	INTEGER, PARAMETER ::  n_cols= 7200
!	INTEGER, PARAMETER ::  n_rows= 3600

! Defined lulc variable
  REAL, ALLOCATABLE, DIMENSION(:,:) :: lulc
  REAL :: igbp
  REAL :: fsize_v

! Variables to group fire locations in a specific grid
! declaration values in lon and lat to globe and resolution of 3 km
! Sould be changed acccording to resolution
! 360 / resolution of data in degrees
! 180 / resolution of data in degrees

   INTEGER, PARAMETER ::  a_cols= 13335
   INTEGER, PARAMETER ::  a_rows= 6667

   INTEGER :: dyear, leap_y, jday0
   INTEGER :: cont(a_cols,a_rows)          ! Total FRP counts within 24 hour time period
   INTEGER :: x, y, read_lulc

   REAL    :: ac_frp(a_cols,a_rows), ac_parea(a_cols,a_rows), ac_fsize(a_cols,a_rows), ac_day(a_cols,a_rows) ! accumulated quantities for 3x3km pixels
   REAL    :: delta    ! The model resolution in degrees
   REAL    :: lon, lat
   REAL(4) :: dday
   LOGICAL, PARAMETER :: dbg=.true.

! Input VIIRS data
   CALL GETARG(1,input_viirs)

!Output ascii file -- after process
   CALL GETARG(2,output)

! MAP file in binary and float point
   CALL GETARG(3,lulcmap)

!  Error if number of described files are lower then need to process
   IF (input_viirs .EQ. ' ' .AND. output .EQ. ' ' .AND. lulcmap .EQ. ' ') THEN
      WRITE(*,*) 'use: program <input_viirs> <output> <lulc map>'
      STOP
   ENDIF

! VIIRS data
   OPEN(21,file=trim(input_viirs),status='old', IOSTAT=stat, ACTION='read')
   READ(21,'(a80)') (cab(i),i=1,1)

   resx = 0.025    ! IGBP land-use data spatial resolution
   resy = 0.025

! DELTA resolution
! COnvert km to degrees
! In near future this variable will be direct assessed in prep_chem.inp

   delta = 3. / 111.12  ! the model resolution is hard wired for the HRRR res.
   ac_frp(:,:) = 0.
   ac_parea(:,:) = 0.
   cont(:,:) = 0

! A sample J01 file
! year,month,day,hh,mm,lon,lat,mask,confidence,bright_t13,frp,line,sample,bowtie;    nfire = 9
! 2018, 09, 18, 00, 07,   19.609308,   30.400427,   9, 100,  353.013733,   70.738800,    13,  1087,   0

! This is the number
   read_lulc=1

!!! Write a subroutine for reading all the VIIRS text files, so we don't read the LULC map for each file!!
line_loop: DO
              READ(21, *, iostat=stat, end=999) yy,mm,dd,hour,minute,lon_vi,lat_vi,mask,confi,t13,frp_v,posy,posx,bowtie,per_anomaly
              IF (stat /= 0) EXIT
              IF (per_anomaly>0) cycle      ! The FRP detections for volcanoes and other non-BB sources

              IF ((frp_v<1.) .OR. (frp_v>10000.)) cycle    ! I suggest we put low/high end limits on the FRP data to remove the false detections or bad data

   check_loc: IF ( (lat_vi .GT. 45.0) .AND. (lat_vi .LT. 75.0) .AND. (lon_vi .GT. -180.0) .AND. (lon_vi .LT. -120.0) ) THEN

    check_lulc:  IF (read_lulc==1) THEN
                    ! Input MAP of Land Use and Land cover
                    OPEN(23, file=lulcmap, FORM='UNFORMATTED', ACCESS="STREAM", IOSTAT=stat)

                    ! Allocate MAP file
                    ALLOCATE(lulc(n_cols,n_rows))

                    ! Read the MAP with land use and land cover IGBP
                    READ (23) lulc
                    read_lulc=read_lulc+1   ! The LULC data won't be read again

                    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                    print *, '!!!!!  Read of LULC MAP DONE  !!!!!'
                    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                    print *, ' Processing JPSS-1 FRP data now', input_viirs

                    ! Output file to write the processed data
                    OPEN(22,file=trim(output), status='replace', IOSTAT=stat, ACTION='write')
                 ENDIF     check_lulc

                 dday = REAL(dd)

jday:            IF ( MOD(yy,4)==0 )  THEN      ! leap year
                    SELECT CASE (mm)
                      CASE ( 1 )
                        juldaydec = dday
                      CASE ( 2 )
                        juldaydec = 31.+dday
                      CASE ( 3 )
                        juldaydec = 60.+dday
                      CASE ( 4 )
                        juldaydec = 91.+dday
                      CASE ( 5 )
                        juldaydec = 121.+dday
                      CASE ( 6 )
                        juldaydec = 152.+dday
                      CASE ( 7 )
                        juldaydec = 182.+dday
                      CASE ( 8 )
                        juldaydec = 213.+dday
                      CASE ( 9 )
                        juldaydec = 244.+dday
                      CASE ( 10 )
                        juldaydec = 274.+dday
                      CASE ( 11 )
                        juldaydec = 305.+dday
                      CASE ( 12 )
                        juldaydec = 335.+dday
                      END SELECT
                 ELSE

                    SELECT CASE (mm)
                      CASE ( 1 )
                        juldaydec = dday
                      CASE ( 2 )
                        juldaydec = 31.+dday
                      CASE ( 3 )
                        juldaydec = 59.+dday
                      CASE ( 4 )
                        juldaydec = 90.+dday
                      CASE ( 5 )
                        juldaydec = 120.+dday
                      CASE ( 6 )
                        juldaydec = 151.+dday
                      CASE ( 7 )
                        juldaydec = 181.+dday
                      CASE ( 8 )
                        juldaydec = 212.+dday
                      CASE ( 9 )
                        juldaydec = 243.+dday
                      CASE ( 10 )
                        juldaydec = 273.+dday
                      CASE ( 11 )
                        juldaydec = 304.+dday
                      CASE ( 12 )
                        juldaydec = 334.+dday
                     END SELECT

                 ENDIF  jday

                 IF (yy<2010) THEN
                    write(0,*) 'wrong year!!!'
                    stop 3
                 ENDIF

                 dyear= yy - 2010
                 leap_y= floor(REAL(dyear)/4.)
                 jday0= leap_y*366 + (dyear-leap_y)*365      ! days counted since Jan-1, 2010 until end of previous year

                 hourf = REAL(hour) + REAL(minute)/60.
                 juldaydec= REAL(jday0) + juldaydec + hourf/24.   ! total # of days since Jan-1, 2010

                 IF (dbg) THEN
                    WRITE(*,*) 'yy, dday= ',yy, dday
                    WRITE(*,*) 'dyear, leap_y,jday0= ', dyear, leap_y, jday0
                    WRITE(*,*) 'hourf,juldaydec= ', hourf,juldaydec
                 ENDIF

! Estimate the pixel parea (parea) according to Ivan's output table; We do this for each FRP point
! Regression by polinomial aproximation to  B1 A1 A2 B2
! And Power Regression to C1 and C2
! VIIRS have 3 distinct arrays = SO C1 B1 A1 A2 B2 C2
! A = Near NADIR
    ! A1 estimation
            IF ((posx .ge. 1008) .and. (posx .lt. 1600)) then
               posxx=1600-posx
               parea=(2E-9*(posxx**3))+(5E-8*(posxx**2))+(0.0001*posxx)+0.5494
            ENDIF
    ! A2 estimation
            IF ((posx .ge. 1600) .and. (posx .lt. 2191)) then
               posxx=posx-1600
               parea=(2E-9*(posxx**3))+(5E-8*(posxx**2))+(0.0001*posxx)+0.5494
            ENDIF
    ! B1 estimation
            IF ((posx .ge. 640) .and. (posx .lt. 1008)) then
               posxx=1600-posx
               parea=(5E-9*(posxx**3))-(9E-6*(posxx**2))+(0.0062*posxx)-0.9383
            ENDIF
    ! B2 estimation
            IF ((posx .ge. 2191) .and. (posx .lt. 2560)) then
               posxx=posx-1600
               parea=(5E-9*(posxx**3))-(9E-6*(posxx**2))+(0.0062*posxx)-0.9383
            ENDIF
    ! C1 estimation
            IF ((posx .ge. 0) .and. (posx .lt. 640)) then
               posxx=1600-posx
               parea=(3.56E-8*(posxx**2.4454))
            ENDIF
     ! C2 estimation
            IF ((posx .ge. 2560) .and. (posx .lt. 4200)) then
               posxx=posx-1600
               parea=(3.56E-8*(posxx**2.4454))
            ENDIF

! GET the position in matrix to determine the LULC
            posix =   (nint((lon_vi-(-180.0))/resx)) + 1
            posiy =   (nint((90.0-(lat_vi))/resy)) + 1

            igbp = lulc(posix,posiy)

            IF (dbg) THEN
                WRITE(*,*), 'resx,resy,posix,posiy,igbp ',resx,resy,posix,posiy,igbp
            ENDIF

! Estimation of active fire size for the MODIS LU classes
! Climatologial values to South America; In future DOzier method could be adopted?
! Using diurnal cycle information to estimate single fire detections in FRE integration
! the time of emission will be an average of pixel fires

            if (igbp .eq. 0.) then
               fsize_v = 0.00021*frp_v   ! near water
            endif

            if (igbp .eq. 1.) then
               fsize_v = 0.00021*frp_v   ! Evergreen needleleaf 
            endif
            
            if (igbp .eq. 2.) then
               fsize_v = 0.00021*frp_v   ! Evergreen Broadleaf
            endif

            if (igbp .eq. 3.) then
               fsize_v = 0.00021*frp_v   ! Deciduous Needleleaf 
            endif

            if (igbp .eq. 4.) then
               fsize_v = 0.00021*frp_v   ! Deciduous Broadleaf
            endif

            if (igbp .eq. 5.) then       ! Mixed forest
               fsize_v = 0.00023*frp_v
            endif

            if (igbp .eq. 6.) then       ! Closed shrublands
               fsize_v = 0.00022*frp_v
            endif

            if (igbp .eq. 7.) then       ! Open shrublands
               fsize_v = 0.00022*frp_v
            endif

            if (igbp .eq. 8.) then       ! Woody savannas
               fsize_v = 0.00022*frp_v
            endif

            if (igbp .eq. 9.) then       ! Savannas
               fsize_v = 0.00029*frp_v
            endif

            if (igbp .eq. 10.) then      ! Grassland
               fsize_v = 0.00029*frp_v
            endif

            if (igbp .eq. 11.) then      ! Permanent wetlands
               fsize_v = 0.00021*frp_v
            endif

            if (igbp .eq. 12.) then         ! cropland
               fsize_v = 0.00026*frp_v
            endif

            if (igbp .eq. 13.) then         ! 'Urban and Built-Up'
               fsize_v = 0.00021*frp_v
            endif

            if (igbp .eq. 14.) then         ! cropland/natural vegetation mosaic
               fsize_v = 0.00026*frp_v
            endif

            if (igbp .eq. 15.) then        ! Snow and ice
               fsize_v = 0.00021*frp_v
            endif

            if (igbp .eq. 16.) then        ! Barren or sparsely vegetated
               fsize_v = 0.00021*frp_v
            endif

! Aggregation process
            x = nint( (lon_vi - (-180.0))/delta ) + 1
            y = nint( (  90.0 - (lat_vi))/delta ) + 1

            cont(x,y)    = cont(x,y)     + 1            ! fire counts falling in the domain of interest
            ac_frp(x,y)  = ac_frp(x,y)   + frp_v        ! Here we add all FRP points within 15 min time window and 3x3km grid box
            ac_parea(x,y)= ac_parea(x,y) + parea        ! We add satellite pixels
            ac_fsize(x,y)= ac_fsize(x,y) + fsize_v      ! We sum up the estimated fire/burnt areas
            ac_day(x,y)  = juldaydec

            IF (dbg) THEN
               WRITE(*,*), 'lat_vi,lon_vi ',lat_vi,lon_vi
               WRITE(*,*), 'x,y,ac_frp,ac_fsize,ac_day ',x,y,ac_frp(x,y),ac_fsize(x,y),ac_day(x,y)
            ENDIF

       ENDIF  check_loc
     ENDDO  line_loop

999 CONTINUE

    IF (read_lulc==1) THEN
        print *, '!!!!!  This file does NOT contain data for our domain       !!!!!'
        STOP
    ENDIF

loop_lat:  DO y=1,a_rows
loop_lon:   DO x=1,a_cols
              IF (cont(x,y) > 0) THEN
                  lon = -180.0 + (x-1)*delta
                  lat =   90.0 - (y-1)*delta
                  WRITE(22,700) lon, lat, ac_frp(x,y), ac_fsize(x,y), ac_parea(x,y), ac_day(x,y), cont(x,y)     !, julday
                  write(0,*) '****OUTPUT TO: ',trim(output)
              ENDIF
            ENDDO    loop_lon
           ENDDO    loop_lat

 CLOSE(21)
 CLOSE(22)
 CLOSE(23)

        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print *, '!!!!!         FINISHED        !!!!!'
        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

400 FORMAT (1x, 2a)
500 FORMAT (1x, 3a)
600 FORMAT (8x, 1a, 1i7)

700 FORMAT (6f20.8,1I10)
!700	FORMAT (5i6,2f15.6,2i6,2f15.6,2i4)

800 FORMAT (3f15.6,A10,1f15.6)
!999 CONTINUE

IF (read_lulc>1) THEN
    DEALLOCATE(lulc)
ENDIF

END PROGRAM rviirs_j01_AK
