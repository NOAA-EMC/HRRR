PROGRAM rmodis_AK
IMPLICIT NONE

! USAGE: pgf90 -tp amd64 -mcmodel=medium -Mlarge_arrays -o procviirs rviirs.f90
! The original file from Gabriel Pereira, 02/15/2017
! This program is to preprocess the MODIS FRP data in text file from FIRMS: ftp://nrt1.modaps.eosdis.nasa.gov/FIRMS/Global/

! The text files contain these columns: latitude,longitude,brightness,scan,track,acq_date,acq_time,satellite,confidence,version,bright_t31,frp,daynight

! The highest resolution of thes∂e data is ~1km2
! This program uses the IGBP-MODIS land use map on 3km resolution to estimate the fire size
! In future we can adapt reg. coefficients for North America or adopt another methodolody

! Variables declaration
  CHARACTER(len=250)  :: input_modis
  CHARACTER(len=250)  :: output
  CHARACTER(len=250)  :: lulcmap
  CHARACTER(len=180)  :: cab(1)
  CHARACTER(len=255)  :: line
  CHARACTER(len=2)    :: hour
  CHARACTER(len=2)    :: minute
  CHARACTER(len=4)    :: yearc
  CHARACTER(len=2)    :: monc
  CHARACTER(len=2)    :: dayc
  CHARACTER(len=10)   :: chour

  INTEGER :: stat

! Date variables to convert in decimal julian day
  REAL    :: juldaydec

! time to be processed inside the file  // Will be called from create emissions
  CHARACTER(len=2)  :: qtime
  REAL              :: ftime
  REAL              :: itime
  INTEGER           :: qtimei
  CHARACTER(len=2)  :: cftime
  CHARACTER(len=2)  :: citime
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  INTEGER :: posy
!  INTEGER :: posx
  INTEGER :: i

! MODIS variables in file
   REAL :: lat_m,lon_m,brightness,scan,track, parea
   CHARACTER(len=10)  :: acq_date
   CHARACTER(len=5)   :: acq_time
   CHARACTER(len=1)   :: satellite,dani
   INTEGER            :: confidence
   REAL               :: bright_t31,frp_v
   CHARACTER(len=6)   :: version

! Variables to tabulate fire location with land use and land cover
! Dimensions are the size of product
! In this case MCD12Q1 2013 with 3 km resolution  to CONUS + ALASKA
   INTEGER, PARAMETER ::  n_cols= 6040
   INTEGER, PARAMETER ::  n_rows= 4000

! Spatioal Resolution of LULC map based o
!! DO NOT FORGET TO CHANGE IT IF MODIFICATIONS IN LULC PRODUCT ARE MADE
    REAL  :: resx, resy

! Position in cartesian matrix
    INTEGER  :: posix, posiy

! Defined lulc variable
    REAL, ALLOCATABLE, DIMENSION(:,:) :: lulc
    REAL :: igbp
    REAL :: fsize_v

! Variables to group fire locations in a specific grid
! declaration values in lon and lat to globe and resolution of 3 km
! Sould be changed acccording to resolution
    INTEGER, PARAMETER ::  a_cols= 13335    ! 360 / resolution of data in degrees
    INTEGER, PARAMETER ::  a_rows= 6667     ! 180 / resolution of data in degrees

    INTEGER :: cont(a_cols,a_rows)
    REAL    :: ac_frp(a_cols,a_rows), ac_area(a_cols,a_rows), ac_fsize(a_cols,a_rows), ac_day(a_cols,a_rows)    ! accumulated quantities
    REAL    :: delta
    REAL    :: lon, lat
    INTEGER :: x, y

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  Time determination !!!!!!!!!!!!!!!!!!!!!!!!!!!
    REAL, PARAMETER :: time_step= 0.25 ! separate files for every 15 minutes

    REAL :: hourf,dhh,dmm,dday
    INTEGER :: index,ihh,mm, yy, dyear, leap_y, jday0
    REAL :: iloop,floop,a
    CHARACTER(len=2) :: cmm,chh,omm,odd,preinthour
    CHARACTER(len=7) :: julday,prehour

    LOGICAL, PARAMETER :: dbg=.true.    ! for debugging
    integer :: islash
        
! Call the file data
! VIIRS data
        CALL GETARG(1,input_modis)
        islash=len_trim(input_modis)
        do while(islash>0)
           if(input_modis(islash:islash)=='/') exit
           islash=islash-1
        enddo
        
        julday = input_modis(29+islash:35+islash)
! MAP file in binary and float point
        CALL GETARG(2,lulcmap)

! Call for increment (avoid to process all data again)
        CALL GETARG(3,citime)   ! Command line arguments
        read(citime(1:2),*) itime
       ! read(3,*) qtime

        CALL GETARG(4,cftime)
        read(cftime(1:2),*) ftime
       !read(4,*) ftime

        print *, ' PROCESSING INITIAL TIME: 0  // START TIME : ', itime
        print *, ' PROCESSING END TIME: 0  // END TIME : ', ftime

!  Error if number of described files are lower then need to process
!       IF (input_modis .EQ. ' ' .AND. output .EQ. ' ' .AND. lulcmap .EQ. ' ') THEN
        IF (input_modis .EQ. ' ' .OR. lulcmap .EQ. ' ' .OR. julday .EQ. ' ') THEN
	    WRITE(*,*) 'MISSING ARGUMENTS TO PROCESS MODIS FILES ::: STOPPED'
	    WRITE(*,*) 'NEED: INPUT / LANDUSE MAP /  TIME INIC'
	  STOP
	ENDIF

! Input MAP of Land Use and Land cover
        OPEN(23, file=lulcmap, FORM='UNFORMATTED', ACCESS="STREAM", IOSTAT=stat)

! Allocate MAP file
        ALLOCATE(lulc(n_cols,n_rows))

! Read the MAP with land use and land cover IGBP
        READ (23) lulc

        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print *, '!!!!!  Read of LULC MAP DONE  !!!!!'
        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

        print *, ' Processing MODIS data', input_modis

! LULC MAP Resolution
        resx = 0.025
        resy = 0.025

! DELTA resolution
! Convert km to degrees;    In near future this variable will be entered in prep_chem.inp
        delta = 3. / 111.12     ! this needs to be updated in future to account for the latitude
        ac_frp(:,:) = 0.
        ac_area(:,:) = 0.
        cont(:,:) = 0
        ac_fsize(:,:) = 0.
        ac_day(:,:) = 0.

! Start to create the loop in time with 15 minutes (time_step=0.25)
! Changed to process only increment
! INCREMENT MUST BE IN DECIMAL HOUR varing from 0 to 23.75
! Optimized to start in FULL hours 00, 01 , 02 , 03 ....
        index = -1

! Variable to start delta T
! Reseting
        floop = 0.0

time_loop:  DO iloop=itime,ftime,time_step   ! time_step is fixed =15min
                floop=iloop+time_step
                index = index + 1
        
                SELECT CASE (index)
                CASE ( 0,4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96)     ! cmm is needed for the processed file names
                   cmm = '00'
                CASE ( 1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97)
                   cmm = '15'
                CASE ( 2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,66,70,74,78,82,86,90,94,98)
                   cmm = '30'
                CASE ( 3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,67,71,75,79,83,87,91,95,99)
                   cmm = '45'
                END SELECT

! HOUR in Decimal parameter
                ihh = FLOOR(iloop)

! Convert hour to Character to ouput file; We need this for the file names
                IF (ihh .LE. 9) then
                    write(chh,'(i1)') ihh
                    chh = '0' // chh
                ELSE
                    write(chh,'(i2)') ihh
                ENDIF

                IF (dbg) THEN
                    WRITE(*,*) 'itime,ftime,floop ', itime,ftime,floop
                    WRITE(*,*) 'index,cmm,ihh= ',index,cmm,ihh
                ENDIF

! Start READ FILE
! In future read in array file ! Without time now; Read Header (cab)
                OPEN(21,file=trim(input_modis),status='old', IOSTAT=stat, ACTION='read')      ! TRIM returns the argument with trailing blanks removed.
                READ(21,'(a80)') (cab(i),i=1,1)

! Create the name of the output file
! FORMAT >>  YYYYDDDHHMM_mod_3km.txt
                output = julday // chh // cmm // '_mod_AK.txt'
                print *, 'PROCESSING FILE:    ',output

                ! Output file
                OPEN(22,file=trim(output), status='replace', IOSTAT=stat, ACTION='write')

! START READING ALL LINES AND SEPARATE IN FILES BY 15 MINUTES; latitude,longitude,brightness,scan,track,acq_date,acq_time,satellite,confidence,version,bright_t31,frp,daynight
! Sample line from these files:
! 12.146,6.255,312.3,1,1,2017-12-01,01:15,A,59,6.0NRT,292.6,11.2,N

line_loop:  DO
               READ(21, *, iostat=stat, end=999) lat_m,lon_m,brightness,scan,track,acq_date,acq_time,satellite,confidence,version,bright_t31,frp_v,dani

! You can assume MODIS 1km FRP to peak around 13,000MW, and VIIRS 750m FRP to peak at 16,000MW
               IF ((frp_v<1.) .OR. (frp_v>10000.)) cycle
 check_loc:    IF ( (lat_m .GT. 45.0) .AND. (lat_m .LT. 75.0) .AND. (lon_m .GT. -180.0) .AND. (lon_m .LT. -120.0 ) ) THEN

! CONVERT the scan time (ACQ_TIME) in HH:MM FORMAT TO DECIMAL UTM HOUR
                hour =  acq_time(1:2)
                minute= acq_time(4:5)
                read(hour(1:2),*) dhh
                read(minute(1:2),*) dmm

! CONVERT ACQ_DATE in YYYY-MM-DD FORMAT TO YYYYY MM DD
                yearc = acq_date(1:4)
                monc  = acq_date(6:7)
                dayc  = acq_date(9:10)

                read(yearc(1:4),*) yy
                read(monc(1:2),*)  mm
                read(dayc(1:2),*)  dday
            
jday:           IF ( MOD(yy,4)==0 )  THEN      ! leap year
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

               ENDIF    jday

               IF (yy<2010) THEN
                  write(0,*) 'wrong year!!!'
                  stop 3
               ENDIF

               dyear= yy - 2010
               leap_y= floor(REAL(dyear)/4.)
               jday0= leap_y*366 + (dyear-leap_y)*365      ! days counted since Jan-1, 2010 until end of previous year

               hourf = dhh + dmm/60.
               juldaydec= REAL(jday0) + juldaydec + hourf/24.   ! total # of days since Jan-1, 2010

               IF (dbg) THEN
                   WRITE(*,*) 'yy, dday= ',yy, dday
                   WRITE(*,*) 'dyear, leap_y,jday0= ', dyear, leap_y, jday0
                   WRITE(*,*) 'hourf,juldaydec= ', hourf,juldaydec
               ENDIF

check_time:   IF ((hourf >= iloop) .AND. (hourf < floop)) THEN   ! within 15 min intervals
                    parea = scan*track    ! satellite data pixel size, needs to be checked later
                    posix = nint((lon_m - (-180.0))/resx) + 1
                    posiy = nint((90.0  - (lat_m))/resy) + 1
                    igbp  = lulc(posix,posiy)

! Fire size in km^2
                    if (igbp .eq. 0.) then       ! water 
                       fsize_v = 0.00021*frp_v   ! Forest fire near water
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
                    
                    if (igbp .eq. 5.) then
                       fsize_v = 0.00023*frp_v   ! Mixed forest
                    endif
                    
                    if (igbp .eq. 6.) then
                       fsize_v = 0.00022*frp_v   ! Closed shrublands
                    endif
                    
                    if (igbp .eq. 7.) then
                       fsize_v = 0.00022*frp_v   ! Open shrublands
                    endif
                    
                    if (igbp .eq. 8.) then
                       fsize_v = 0.00022*frp_v   ! Woody savannas
                    endif
                    
                    if (igbp .eq. 9.) then
                       fsize_v = 0.00029*frp_v   ! Savannas
                    endif
                    
                    if (igbp .eq. 10.) then
                       fsize_v = 0.00029*frp_v   ! Grassland
                    endif
                    
                    if (igbp .eq. 11.) then
                       fsize_v = 0.00021*frp_v   ! Permanent wetlands
                    endif
                    
                    if (igbp .eq. 12.) then       ! cropland
                       fsize_v = 0.00026*frp_v
                    endif
                    
                    if (igbp .eq. 13.) then       ! Urban and Built-Up
                       fsize_v = 0.00021*frp_v
                    endif
                    
                    if (igbp .eq. 14.) then       ! cropland/natural vegetation mosaic
                       fsize_v = 0.00026*frp_v
                    endif
                    
                    if (igbp .eq. 15.) then       ! Snow and ice
                       fsize_v = 0.00021*frp_v
                    endif
                    
                    if (igbp .eq. 16.) then       ! Barren or sparsely vegetated
                       fsize_v = 0.00021*frp_v
                    endif

!         ! Aggregation process
                    x = nint((lon_m - (-180.0))/delta) + 1
                    y = nint((90.0  - (lat_m))/delta) + 1

                    cont(x,y)    = cont(x,y) + 1
                    ac_frp(x,y)  = ac_frp(x,y) + frp_v           ! Here we add all FRP points within 15 min time window and 3x3km grid box
                    ac_area(x,y) = ac_area(x,y) + parea          ! We add satellite pixels
                    ac_fsize(x,y)= ac_fsize(x,y) + fsize_v       ! We also add fire areas
                    ac_day(x,y)  = juldaydec

                    IF (dbg) THEN
                        WRITE(*,*), 'x,y,lat_m,lon_m ',x,y,lat_m,lon_m
                        WRITE(*,*), 'posix,posiy,igbp ',posix,posiy,igbp
                        WRITE(*,*), 'ac_frp(x,y),ac_fsize(x,y),ac_day(x,y) ',ac_frp(x,y),ac_fsize(x,y),ac_day(x,y)
                    ENDIF

               ENDIF  check_time
            ENDIF   check_loc
         ENDDO  line_loop

         999 CONTINUE
         CLOSE(21)

loop_lat:  DO y=1,a_rows
loop_lon:   DO x=1,a_cols
              IF (cont(x,y) > 0) THEN        ! # of points in a given 3x3km grid cell
                  lon = -180.0 + (x-1)*delta
                  lat =   90.0 - (y-1)*delta
                  WRITE(22,700) lon, lat, ac_frp(x,y), ac_fsize(x,y), ac_area(x,y), ac_day(x,y), cont(x,y)    !, julday_out    ! writing text files
              ENDIF
            ENDDO  loop_lon
           ENDDO  loop_lat

        CLOSE(22)

        cont(:,:) = 0
        ac_frp(:,:) = 0.
        ac_area(:,:) = 0.
        ac_fsize(:,:) = 0.
        ac_day(:,:) = 0
        
     ENDDO  time_loop

     CLOSE(21)
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

DEALLOCATE(lulc)
END PROGRAM rmodis_AK
