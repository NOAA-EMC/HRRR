PROGRAM FRE_BBM_RAP      ! Total biomass that burnt in kg
    IMPLICIT NONE
!!!!! DEFINE DELTA EQUAL TO FRP GRID ESTIMATION in KM
! Last update, January, 2019

! USAGE: pgf90 -tp amd64 -mcmodel=medium -Mlarge_arrays -o fre cfre.f90
! Size of the grid
! Should be equal to frp fortran program
! Use is "./fre.exe input_fire output_text LULC_map Biome_map" (Ecosystem map); It covers the entire NA, 13.5km res.

    INTEGER :: m, q, k, n   ! n- # of detections
    REAL(4) :: jdayf(180)    !,jdayi(180)          ! fractional day of the FRP detection
    REAL :: frpa(180), sizea(180), mareaa(180)      ! FRP , Fire size and pixel area from input file, ! we can reduce for VIIRS, MODIS
    REAL :: lat,lon     ! coordinates of the FRP points
    REAL :: fre, fre_t  !, fsize_t       ! FRE (2 detections), total FRE and accumulated fire size within 24 hours for a single grid cell, fsize_t total fire size
    REAL :: totsec, bbm   ! duration of fire in seconds and biomass burnt mass
    REAL :: sec1,sec2, dsec, acdsec, acfrp_t, tails, taile
    REAL :: mean_frp, mean_fsize, std_frp, std_fsize      ! statistics for FRP data
    REAL :: frp1,frp2, fsize1,fsize2  ! temporary vars
    REAL :: diur_dtime(2), diur_sdev(2), avg_fdt, std_fdt, bbcoef    ! average duration and sigma for fires, FRE->BBM coefficient
    REAL :: p1,p2,d1,d2,dt1,dt2
!    REAL :: frpskew,frppskew,fsizeskew,fsizepskew,skewfrp,skewfsize

    CHARACTER :: iha*4,ima*4,hora*2,min*2,horap*2,minp*2,ihap*4
    CHARACTER :: input*250, output*250

!! IGBP LAND USE data
    INTEGER, PARAMETER ::  lulc_cols= 2951
    INTEGER, PARAMETER ::  lulc_rows= 1475

! Biome map
    INTEGER, PARAMETER ::  biom_cols= 1238
    INTEGER, PARAMETER ::  biom_rows= 820

! Position in cartesian matrix
    INTEGER :: posix_lulc, posiy_lulc, posix_biom, posiy_biom, stat

! Spatial Resolution of LULC map ~3km
!! DO NOT FORGET TO CHANGE IT IF MODIFICATIONS IN LULC PRODUCT WAS MADE
    REAL :: resx_lulc, resy_lulc, resx_biom, resy_biom

! Defined lulc variable
    REAL, ALLOCATABLE, DIMENSION(:,:) :: lulc
    REAL :: igbp
    CHARACTER(len=250) :: lulcmap

! Biome variables
    REAL, ALLOCATABLE, DIMENSION(:,:) :: lbiome
    REAL :: rbiome
    CHARACTER(len=250)  :: cbiome

    LOGICAL, PARAMETER :: dbg=.false.

! OPEN DAILY FRP FIRE DATA PROCESSED BY PREVIOUS STEP
    CALL getarg(1,input)
    IF(input.EQ.' ')THEN
    WRITE(*,*) 'Use: FRE_BBM_RAP.exe <input frp daily file> <output file> <LULC> <BIOME>'
          STOP
    ENDIF
    OPEN (21,file=input,status='old')

! OUTput FILE TO BE SAVED IN PROCESS
    CALL getarg(2,output)
    IF (output.EQ.' ') THEN
        WRITE(*,*) 'FRE_BBM.exe <input frp daily file> <output file> <LULC> <BIOME>'
        STOP
    ENDIF
    OPEN(22,file=output,status='replace')

    !WRITE(22,*) '        Long               Lat            Average_FRP         Average_fsize       Std_FRP           Std_fsize          BBM        NOBS      Duration      IGBP   '
    WRITE(22,"(2A12,8A20)")'Long','Lat','Average_FRP','Average_fsize','Std_FRP','Std_fsize','BBM','NOBS','Duration','IGBP'

! LAND USE MAP (SAME AS READ MODIS AND VIIRS)
    ! MAP file in binary and float point
    CALL GETARG(3,lulcmap)
    IF (lulcmap.EQ.' ') THEN
       WRITE(*,*) 'Use: FRE <input frp daily file> <output file> <LULC> <BIOME>'
       STOP
    ENDIF

! LAND USE MAP (SAME AS READ MODIS AND VIIRS)
    ! MAP file in binary and float point
    CALL GETARG(4,cbiome)
    IF (cbiome.EQ.' ') THEN
       WRITE(*,*) 'Use: fre <input frp daily file> <output file> <LULC> <BIOME>'
       STOP
    ENDIF

! Input MAP of Land Use and Land cover
    OPEN(24, file=lulcmap, FORM='UNFORMATTED', ACCESS="STREAM", IOSTAT=stat)

! READ LULC MAP
! Allocate LU index array
    ALLOCATE (lulc(lulc_cols,lulc_rows))

! Read the MAP with land use and land cover IGBP
    READ (24) lulc

    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print *, '!!!!!  LULC MAP IS READ!!!!!  !!!!!'
    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    
    resx_lulc = 0.122   ! resolution of the Land-use map
    resy_lulc = 0.122

! Input the BIOME MAP
    OPEN(25, file=cbiome, FORM='UNFORMATTED', ACCESS="STREAM", IOSTAT=stat)     ! RAR: add checks to check if a file is read

! READ LULC MAP
! Allocate MAP file
    ALLOCATE (lbiome(biom_cols,biom_rows))

! Read the MAP with land use and land cover IGBP
    READ (25) lbiome

    resx_biom = 0.122   ! resolution of the biome map
    resy_biom = 0.122

    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print *, '!!!!  Reading the BIOME MAP DONE  !!!!'
    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

! New estimation
!    meanfrp=0.
!    meanfsize=0.
!    sdfrp=0.
!    sdfsize=0.
!    p1=0.
!    p2=0.
!    d1=0.
!    d2=0.
!    frpskew = 0.
!    frppskew = 0.
!    fsizeskew = 0.
!    fsizepskew = 0.
!    skewfrp  = 0.
!    skewfsize = 0.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

loop_frp:  DO WHILE (.TRUE.)
              READ(21,*,END=100) n                                                                          ! # of detections
              READ(21,*,END=100) lon,lat,(frpa(k),k=1,n),(sizea(k),k=1,n),(mareaa(k),k=1,n),(jdayf(k),k=1,n)     !, (jdayi(k),k=1,n)   !

!              print *,jdayi(n)
! GET the position in matrix to determine the LULC
              posix_lulc = (nint((lon-(-180.0))/resx_lulc)) + 1       ! WE index of the grid cell
              posiy_lulc = (nint((90.0-(lat))/resy_lulc)) + 1         ! SN index of the grid cell

! BIOME map
              posix_biom = (nint((lon-(-180.0))/resx_biom)) + 1       ! WE index of the grid cell
              posiy_biom = (nint((90.0-(lat))/resy_biom)) + 1         ! SN index of the grid cell

              igbp   = lulc(posix_lulc,posiy_lulc)

              rbiome=0.
              IF (posix_biom<biom_cols .AND. posiy_biom<biom_rows) THEN
                  rbiome = lbiome(posix_biom,posiy_biom)
              END IF

              !WRITE(*,*) 'lat,lon,igbp,rbiome ',lat,lon,igbp,rbiome

!RAR: new development is the separation of the Eastern temperate forest (array 2) and the rest of the North America (1)
lu_sel:   SELECT CASE (INT(igbp))
    CASE (0)                          ! water, here we assume fire near coastline e.g., needs further improvement
    diur_dtime(1) = 7.66
    diur_sdev(1)  = 5.05
    diur_dtime(2) = 4.87
    diur_sdev(2)  = 3.62

    bbcoef        = 1.2
    CASE (1)                          ! Evergreen needleleaf
    diur_dtime(1) = 7.71
    diur_sdev(1)  = 4.73
    diur_dtime(2) = 7.77
    diur_sdev(2)  = 5.87

    bbcoef        = 1.55
    CASE (2)                          ! Evergreen Broadleaf
    diur_dtime(1) = 8.94
    diur_sdev(1)  = 5.15
    diur_dtime(2) = 6.32
    diur_sdev(2)  = 4.89

    bbcoef        = 0.96
    CASE (3)                          ! Deciduous Needleleaf
    diur_dtime(1) = 6.13
    diur_sdev(1)  = 3.94
    diur_dtime(2) = 8.10
    diur_sdev(2)  = 6.25

    bbcoef        = 1.55
    CASE (4)                          ! Deciduous Broadleaf
    diur_dtime(1) = 5.77
    diur_sdev(1)  = 2.62
    diur_dtime(2) = 3.94
    diur_sdev(2)  = 1.57

    bbcoef        = 1.55
    CASE (5)                          ! Mixed forest
    diur_dtime(1) = 6.48
    diur_sdev(1)  = 4.18
    diur_dtime(2) = 4.57
    diur_sdev(2)  = 2.96

    bbcoef        = 1.55
    CASE (6)                          ! Closed shrublands
    diur_dtime(1) = 8.14
    diur_sdev(1)  = 5.28
    diur_dtime(2) = 5.56
    diur_sdev(2)  = 4.63

    bbcoef        = 0.78
    CASE (7)                          ! Open shrublands
    diur_dtime(1) = 9.10
    diur_sdev(1)  = 6.28
    diur_dtime(2) = 6.17
    diur_sdev(2)  = 4.30

    bbcoef     = 1.55
    CASE (8)                          ! Woody savannas
    diur_dtime(1) = 9.81
    diur_sdev(1)  = 6.12
    diur_dtime(2) = 4.63
    diur_sdev(2)  = 3.27

    bbcoef        = 1.55
    CASE (9)                          ! Savannas
    diur_dtime(1) = 8.70
    diur_sdev(1)  = 5.74
    diur_dtime(2) = 5.28
    diur_sdev(2)  = 4.12

    bbcoef        = 0.78
    CASE (10)                         ! Grassland
    diur_dtime(1) = 7.26
    diur_sdev(1)  = 4.82
    diur_dtime(2) = 4.07
    diur_sdev(2)  = 1.34

    bbcoef        = 0.78
    CASE (11)                         ! Permanent wetlands
    diur_dtime(1) = 8.24
    diur_sdev(1)  = 5.26
    diur_dtime(2) = 5.38
    diur_sdev(2)  = 4.01

    bbcoef        = 1.55
    CASE (12)                         ! cropland
    diur_dtime(1) = 3.00   !5.89
    diur_sdev(1)  = 3.78
    diur_dtime(2) = 3.00   !7.17
    diur_sdev(2)  = 5.52

    bbcoef        = 0.29
    CASE (13)                         ! Urban and Built-Up
    diur_dtime(1) = 6.31
    diur_sdev(1)  = 4.42
    diur_dtime(2) = 4.54
    diur_sdev(2)  = 2.67

    bbcoef        = 0.29
    CASE (14)                         ! cropland/natural vegetation mosaic
    diur_dtime(1) = 5.23
    diur_sdev(1)  = 2.93
    diur_dtime(2) = 4.15
    diur_sdev(2)  = 1.64

    bbcoef        = 0.78
    CASE (15)                         ! Snow and ice
    diur_dtime(1) = 6.42
    diur_sdev(1)  = 3.94
    diur_dtime(2) = 4.77
    diur_sdev(2)  = 3.09

    bbcoef        = 0.49
    CASE (16)                         ! Barren or sparsely vegetated
    diur_dtime(1) = 8.57
    diur_sdev(1)  = 5.27
    diur_dtime(2) = 4.77
    diur_sdev(2)  = 4.70

    bbcoef        = 0.78

END SELECT  lu_sel

!    lu_sel:   SELECT CASE (INT(igbp))
!                  CASE (0)                          ! water, here we assume fire near coastline e.g., needs further improvement
!                     diur_dtime = 7.09
!                     diur_sdev  = 4.94
!                     bbcoef     = 1.2
!                  CASE (1)                          ! Evergreen needleleaf
!                     diur_dtime = 7.70
!                     diur_sdev  = 4.7
!                     bbcoef     = 1.55
!                  CASE (2)                          ! Evergreen Broadleaf
!                     diur_dtime = 8.66
!                     diur_sdev  = 5.15
!                     bbcoef     = 0.96
!                  CASE (3)                          ! Deciduous Needleleaf
!                     diur_dtime = 6.69
!                     diur_sdev  = 4.79
!                     bbcoef     = 1.55
!                  CASE (4)                          ! Deciduous Broadleaf
!                     diur_dtime = 5.28
!                     diur_sdev  = 2.52
!                     bbcoef     = 1.55
!                  CASE (5)                          ! Mixed forest
!                     diur_dtime = 3.75
!                     diur_sdev  = 3.74
!                     bbcoef     = 1.55
!                  CASE (6)                          ! Closed shrublands
!                     diur_dtime = 6.78
!                     diur_sdev  = 5.11
!                     bbcoef     = 0.78
!                  CASE (7)                          ! Open shrublands
!                     diur_dtime = 9.00
!                     diur_sdev  = 6.24
!                     bbcoef     = 1.55
!                  CASE (8)                          ! Woody savannas
!                     diur_dtime = 9.38
!                     diur_sdev  = 6.1
!                     bbcoef     = 1.55
!                  CASE (9)                          ! Savannas
!                     diur_dtime = 7.56
!                     diur_sdev  = 5.49
!                     bbcoef     = 0.78
!                  CASE (10)                         ! Grassland
!                     diur_dtime = 7.19
!                     diur_sdev  = 4.79
!                     bbcoef     = 0.78
!                  CASE (11)                         ! Permanent wetlands
!                     diur_dtime = 8.24
!                     diur_sdev  = 5.26
!                     bbcoef     = 1.55
!                  CASE (12)                         ! cropland
!                     diur_dtime = 2.87
!                     diur_sdev  = 4.00
!                     bbcoef     = 0.29
!                  CASE (13)                         ! Urban and Built-Up
!                     diur_dtime = 5.73
!                     diur_sdev  = 4.04
!                     bbcoef     = 0.29
!                  CASE (14)                         ! cropland/natural vegetation mosaic
!                     diur_dtime = 3.44
!                     diur_sdev  = 4.7
!                     bbcoef     = 0.78
!                  CASE (15)                         ! Snow and ice
!                     diur_dtime = 5.82
!                     diur_sdev  = 3.74
!                     bbcoef     = 0.49
!                  CASE (16)                         ! Barren or sparsely vegetated
!                     diur_dtime = 8.57
!                     diur_sdev  = 5.39
!                     bbcoef     = 0.78

!              END SELECT  lu_sel

              IF (dbg) THEN
                WRITE(*,*) '******************************************'
                WRITE(*,*) 'lat,lon,frpa(1:n)= ',lat,lon,frpa(1:n)
                WRITE(*,*) 'jdayf(1:n)= ',jdayf(1:n)
                WRITE(*,*) 'rbiome,INT(igbp)= ',rbiome,INT(igbp)
                WRITE(*,*) 'diur_dtime,diur_sdev,bbcoef = ', diur_dtime,diur_sdev,bbcoef
              ENDIF

! Calculation of the std and skewness for single FRP points
! These data are based on South America, 2000-2015 GOES data analysis. This needs to be improved in the future
single_det:   IF (n == 1) THEN        ! For single FRP detections, this needs to be improved in the future
                  mean_frp= frpa(n)
                  mean_fsize= sizea(n)

                  std_frp=   0.25* frpa(1)      ! RAR: for single FRP detections 25% sigma is assumed, the FRP uncertainty is ~50%
                  std_fsize= 0.25* sizea(1)

                !  IF (frpa(1) .LE. 10.) THEN
                !      sdfrp   = 1.3
                !      sdfsize = 0.0012
                !      skewfrp = -1.671
                !  ENDIF

                !  IF ((frpa(1) .GT. 10.0) .AND. (frpa(1) .LT. 20.0)) THEN
                !      sdfrp   = 2.97
                !      sdfsize = 0.0027
                !      skewfrp = 0.20
                !  ENDIF

                !  IF ((frpa(1) .GT. 20.0) .AND. (frpa(1) .LT. 40.0)) THEN
                !      sdfrp   = 5.8
                !      sdfsize = 0.0052
                !      skewfrp = 0.20
                !  ENDIF

                !  IF ((frpa(1) .GT. 40.0) .AND. (frpa(1) .LT. 80.0)) THEN
                !      sdfrp   = 11.11
                !      sdfsize = 0.01
                !      skewfrp = -0.18
                !  ENDIF

                !  IF ((frpa(1) .GT. 80.0) .AND. (frpa(1) .LT. 160.0)) THEN
                !      sdfrp   = 22.25
                !      sdfsize = 0.02
                !      skewfrp = 0.009
                !  ENDIF

                !  IF ((frpa(1) .GT. 160.0) .AND. (frpa(1) .LT. 320.0)) THEN
                !      sdfrp   = 45.32
                !      sdfsize = 0.05
                !      skewfrp = 0.42
                !  ENDIF

                !  IF ((frpa(1) .GT. 320.0) .AND. (frpa(1) .LT. 640.0)) THEN
                !      sdfrp   = 89.26
                !      sdfsize = 0.08
                !      skewfrp = 0.78
                !  ENDIF

                !  IF ((frpa(1) .GT. 640.0) .AND. (frpa(1) .LT. 1280.0)) THEN
                !      sdfrp   = 162.32
                !      sdfsize = 0.14
                !      skewfrp = 0.78
                !  ENDIF

                !  IF (frpa(1) .GT. 1280.0) THEN     ! FRP<=5000.
                !      sdfrp   = 895.36
                !      sdfsize = 0.967
                !      skewfrp = 4.5
                !  ENDIF

                  IF (dbg) THEN
                    WRITE(*,*) 'n,frpa(n),mean_frp= ',n,frpa(n),mean_frp
                    WRITE(*,*) 'std_frp,std_fsize = ', std_frp,std_fsize
                  ENDIF
              ENDIF     single_det

mult_det:     IF (n > 1) THEN
                  mean_frp=0.
                  mean_fsize=0.
                  p1=0.
                  p2=0.
                  d1=0.
                  d2=0.
           !       frpskew = 0.
           !       frppskew = 0.
           !       fsizeskew = 0.
          !        fsizepskew = 0.
          !        skewfrp  = 0.
          !        skewfsize = 0.

                  ! Calculate mean FRP and fire size within 24 hour time frame
                  DO m=1,n
                     mean_frp   = frpa(m) + mean_frp
                     mean_fsize = sizea(m) + mean_fsize
                  ENDDO
                  mean_frp  = mean_frp/n
                  mean_fsize= mean_fsize/n

                  DO m=1,n
                    p1 = (frpa(m) - mean_frp)**2
                    d1 = (sizea(m)- mean_fsize)**2
                    p2 = p2 + p1
                    d2 = d2 + d1
                  ENDDO

                  std_frp=(p2/n)**0.5
                  std_fsize=(d2/n)**0.5

                  ! We need to have some variability in the FRP
                  std_frp=   MAX(std_frp,0.25*mean_frp)
                  std_fsize= MAX(std_fsize,0.25*mean_fsize)

!                  DO m=1,n
!                     frppskew   = frpa(m)   - meanfrp
!                     fsizepskew = sizea(m)  - meanfsize
!                     frpskew    = frpskew   + ((frppskew/sdfrp)**3)
!                     fsizeskew  = fsizeskew + ((fsizepskew/sdfsize)**3)
!                  ENDDO
!                     skewfrp    = frpskew/n
!                     skewfsize  = fsizeskew/n

                    IF (dbg) THEN
                        WRITE(*,*) 'n,frpa(n)= ',n,frpa(n)
                        WRITE(*,*) 'n,sizea(n)= ',n,sizea(n)
                        WRITE(*,*) 'mean_frp,mean_fsize = ', mean_frp,mean_fsize
!                        WRITE(*,*) 'skewfrp,skewfsize = ', skewfrp,skewfsize
                    ENDIF

              ENDIF  mult_det

! FRE estimation; Locations with only one fire detection within 24 hours
single_frp: IF (n == 1) THEN

                IF (rbiome == 9.) THEN              ! 9 is Eastern Temperate Forest ecosystem, which covers the eastern US
                    avg_fdt = 3.44   ! hours        ! For cropland the avg. diurnal duration (2.87 hours) is used
                ENDIF

                ! All the IGBP classes except the Eastern Temperate Forest ecosystem, all permanent wetlands, cropland, urban, cropland/natural vegetation mosaic, snow/ice, barren LU classes
                ! Savannas, grassland etc. are included too; In the future check cropland/natural vegetation mosaic (14), maybe it needs to be excluded
                IF ( (rbiome == 9.) .AND. (igbp/=12.) ) THEN       ! .OR. (igbp == 0.) .OR. (igbp>8.) ) THEN
                    totsec = avg_fdt*3600.0  ! seconds, duration of a fire when there is a single FRP point based on diurnal cycle
                ELSE
                    totsec = diur_dtime(1)*3600.   !MIN(diur_dtime(1)*(diur_sdev/2.),24.) *3600.
                ENDIF
                bbm= frpa(n) *totsec *bbcoef     ! Total burned biomass, n=1 here anyways

                ! For all the rest, the avg. fire duration is used
                !fsize_t = sizea(n)

    ! Output file
                WRITE(22,700)lon,lat,mean_frp,mean_fsize,std_frp,std_fsize,bbm,n,totsec,igbp

                IF (dbg) THEN
                    WRITE(*,*) 'n= ',n
                    WRITE(*,*) 'rbiome,igbp= ',rbiome,igbp
                    WRITE(*,*) 'diur_dtime,diur_sdev,totsec = ', diur_dtime,diur_sdev,totsec
                    WRITE(*,*) 'mean_frp,mean_fsize = ', mean_frp,mean_fsize
!                    WRITE(*,*) 'skewfrp,skewfsize = ', skewfrp,skewfsize
                    WRITE(*,*) 'bbm = ', bbm
                ENDIF

            ENDIF  single_frp

! Start of FRE process to n-observations
mult_frp: IF (n > 1) THEN     ! More than one FRP detections in 24 hours

! Variable reset
             acdsec = 0.0
             fre_t  = 0.0
             !fsize_t= 0.0
             acfrp_t= 0.0

             ! added tails= 1.5 hours to each end of the integration
             tails= frpa(1)* (1.5*3600.)/2.
             taile= frpa(n)* (1.5*3600.)/2.
             acfrp_t =  frpa(1)
             !fsize_t =  sizea(1)

loop_det:    DO m=1,n-1
                dsec = (jdayf(m+1) - jdayf(m)) *24. *3600.

!                IF (dhour .GT. 24.00) THEN
!                    dt1 = jday(m+1) - jdayi(m+1)*100
!                    dt2 = jday(m)   - jdayi(m)*100
!                    dt2 = 24.- dt2
!                    dsec= (dt1+dt2) *3600
!                ELSE
!                    dsec = dhour*3600
!                ENDIF

                frp1 = frpa(m)
                frp2 = frpa(m+1)        ! if n=2 then m=1
                fsize2 = sizea(m+1)     ! this is the sat. pixel size

                ! Avoid non-sequencial hour in time integration

! Avoid error in process more than 24hr files from MODIS or VIIRS
! Should not occur -- only if using other day files
                IF (dsec > 86400.) THEN
                    dsec= 86400.
                ENDIF

! Simultaneous observations - assuming 1 minute delay
                IF (dsec == 0.0) THEN
                    dsec= 60.0
                ENDIF
                 
                acdsec  = acdsec + dsec          ! Total time in seconds for all FRP detections within 24 hours for a single grid cell
                acfrp_t = acfrp_t + frp2         ! Accumulated FRP within 24 hours for a single grid cell
                !fsize_t = fsize_t + fsize2       ! Do we need to add ifsizep here??

                fre = (frp1 + frp2)* dsec/2.
                fre_t = fre_t + fre              ! FRP*time

                IF (dbg) THEN
                    WRITE(*,*) 'n,= ',n
                    WRITE(*,*) 'igbp,rbiome,diur_dtime= ',igbp,rbiome,diur_dtime
                    WRITE(*,*) 'acdsec,acfrp_t= ',acdsec,acfrp_t    !,fsize_t
                ENDIF

             ENDDO  loop_det

             ! Two tails (each assuming +1.5 hr) added to the integration
             fre_t = fre_t + tails + taile

            IF (rbiome == 9.) THEN     ! eastern US
                avg_fdt= diur_dtime(2)
                std_fdt= diur_sdev(2)
            ELSE
                avg_fdt= diur_dtime(1)
                std_fdt= diur_sdev(1)
            ENDIF

            ! bbm is the total biomass burnt in kg, it's equal to total biomass x combustion efficiency (e.g. for grassland it's 100%)
            IF (acdsec < avg_fdt*3600.) THEN     ! short lasting fire
                IF ( (acdsec<10800.) .OR. (rbiome == 9.) .OR. (igbp == 0.) .OR. (igbp > 8.) ) THEN     ! If the detected duration is shorter than the diurnal cycle and it its igbp>10 or in the eastern US
                    totsec= avg_fdt*3600.
                ELSE
                    totsec= MIN( avg_fdt*(std_fdt/2.), 24. ) *3600.     ! All igbp classes 1-8 in the western US will fall here
                ENDIF
            bbm= bbcoef* (acfrp_t/n)* totsec    ! no "tails" are added to the integration

            ELSE    ! Otherwise do integration of FRP*dt, long lasting fires > diurnal cycle
            ! For forest fires we assume 24 hours for duration; This will be improved later when geostat. data will be utilized. 8-Woody savannas
                IF ( (rbiome /= 9.) .AND. (igbp > 0.) .AND. (igbp <= 8.) ) THEN  ! forest and other classes in the western US, !!!grassland isn't included here!!!
                     totsec= 24.0*3600.0
                     bbm= bbcoef* (acfrp_t/n)* totsec           ! it's assumed that the forest and other fires last 24 hours
                ELSE
                     totsec= acdsec + 10800.    ! In future we should process this differently
                     bbm= bbcoef* fre_t
                ENDIF
            ENDIF

            IF (dbg) THEN
                WRITE(*,*) 'totsec,fre_t,bbcoef,bbm= ',totsec,fre_t,bbcoef,bbm
            ENDIF

            WRITE(22,700)lon,lat,mean_frp,mean_fsize,std_frp,std_fsize,bbm,n,totsec,igbp !,rbiome

          ENDIF   mult_frp

700     format (06f18.6,1f20.3,1i4,2f12.2)

        ENDDO   loop_frp

100 CONTINUE

        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
        print *, '!!!!! FINISHED TOTAL BB EMISSION ESTIMATION !!!!'
        print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'

        CLOSE(21)
        CLOSE(22)
        CLOSE(24)
        CLOSE(25)

        DEALLOCATE(lbiome,lulc)
END PROGRAM FRE_BBM_RAP
