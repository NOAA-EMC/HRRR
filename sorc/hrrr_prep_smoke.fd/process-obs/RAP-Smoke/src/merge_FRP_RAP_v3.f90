! This program processes files with all the FRP data (MODIS, VIIRS), aggregates to the ~3x3km grid cells

PROGRAM frp_agg_rap
	IMPLICIT NONE
! USAGE: pgf90 -tp amd64 -mcmodel=medium -Mlarge_arrays -o frp frp.f90
! Variables declaration

! Variables to determine the fire locations in a specific grid and the hour
! declaration values in lon and lat to globe and resolution of 3 km
! It has to be changed acccording to resolution
! 360 / resolution of data in degrees
! 180 / resolution of data in degrees

! NA domain: 180W - 50W and 90N-10N
    INTEGER, PARAMETER ::  n_cols= 2951
    INTEGER, PARAMETER ::  n_rows= 1475

! Gridded variables to aggregation process
! Variables are in X,Y and in X,Y,TIME
	INTEGER, ALLOCATABLE, DIMENSION(:,:) :: detc
 	REAL(4), ALLOCATABLE, DIMENSION(:,:,:) :: jdayf3   ! 3D fractional julian day array
	REAL, ALLOCATABLE, DIMENSION(:,:,:) :: frp3, fsize3, parea3   ! 3D arrays for FRP, pixel area and fire size

    REAL :: lat, lon, lon_s, lat_s, frp_s, delta, parea, fsize, jdayf_s
	INTEGER ::  x, y, k, ll, n,w,t,count, lns
 	INTEGER :: ih, im, imt, mh, mm,deltax   !,julday_ag
	CHARACTER :: input*172, fmt*20, lixo*80, output*172
    REAL :: diur_dtime, igbp

    LOGICAL, PARAMETER :: dbg=.false.

! OPEN daily FIRE DATA PROCESSED IN STEP BEFORE
	CALL getarg(1,input)
	IF(input.EQ.' ')THEN
       WRITE(*,*) 'Use: frp <input daily file> <output file>'
       STOP
    ENDIF
	OPEN(21,file=input,status='old')

! Output file TO BE SAVED IN PROCESS
    CALL getarg(2,output)
    IF(output.EQ.' ')THEN
       WRITE(*,*) 'Use: frp <input daily file> <output file>'
       STOP
    ENDIF
	OPEN(22,file=output,status='replace')
	
! Few time observations --- reduce time in Z variable to get faster
	ALLOCATE(detc(n_cols,n_rows))
    ALLOCATE(jdayf3(n_cols,n_rows,20), frp3(n_cols,n_rows,20))      !hh(n_cols,n_rows,50),, frpa(n_cols,n_rows,50))
    ALLOCATE(fsize3(n_cols,n_rows,20), parea3(n_cols,n_rows,20))

! Grid resolution
	delta = 13.545 / 111.12

	detc(:,:)     = 0       ! # of detections
    jdayf3(:,:,:) = 0.
    frp3(:,:,:)   = 0.      ! 3D array
	fsize3(:,:,:) = 0.      ! 3D array
	parea3(:,:,:) = 0.      ! 3D array

! RAR: I rewrote these loops to make them more compact
llr:  DO WHILE (.TRUE.)
         READ(21,*, END=200) lon_s,lat_s,frp_s,fsize,parea,jdayf_s,count   !,julday_ag
         x= nint((lon_s - (-180.0))/delta) + 1    ! WE direction
         y= nint(( 90.0 - lat_s)/delta) + 1       ! SN direction

         IF ((x > 0) .and. (y > 0)) THEN   ! falls within the domain of interest, cell with x,y indices
            detc(x,y) = detc(x,y) + 1
            ll= detc(x,y)

! RAR: this was added as a precaution. This part needs further improvement
            IF ( ll>1 .and. jdayf_s < jdayf3(x,y,ll-1) )  THEN
                jdayf3(x,y,ll)= jdayf3(x,y,ll-1)
                frp3(x,y,ll)  = frp3(x,y,ll-1)
                fsize3(x,y,ll)= fsize3(x,y,ll-1)
                parea3(x,y,ll)= parea3(x,y,ll-1)

                jdayf3(x,y,ll-1)= jdayf_s
                frp3(x,y,ll-1)  = frp_s
                fsize3(x,y,ll-1)= fsize
                parea3(x,y,ll-1)= parea
            ELSE
                jdayf3(x,y,ll)= jdayf_s
                frp3(x,y,ll)  = frp_s
                fsize3(x,y,ll)= fsize
                parea3(x,y,ll)= parea
            ENDIF
         ENDIF

         IF (dbg) THEN
            WRITE(*,*) 'x,y,ll= ',ll
            WRITE(*,*) 'jdayf3(x,y,ll)= ',jdayf3(x,y,ll)
            WRITE(*,*) 'frp3(x,y,ll)= ',  frp3(x,y,ll)
            WRITE(*,*) 'fsize3(x,y,ll)= ',fsize3(x,y,ll)
            WRITE(*,*) 'parea3(x,y,ll)= ',parea3(x,y,ll)
         ENDIF

      ENDDO llr

200 CONTINUE

y_l:  DO y=1,n_rows
x_l:     DO x=1,n_cols
d_c:      IF (detc(x,y) > 0) THEN

            lon = (-180.0 + (x-1)*delta)
            lat = (  90.0 - (y-1)*delta)
            n   = detc(x,y)

            write(22,*) n
            write(22,*) lon,lat,(frp3(x,y,k),k=1,n),(fsize3(x,y,k),k=1,n),(parea3(x,y,k),k=1,n),(jdayf3(x,y,k),k=1,n)       !,(julday_ar(x,y,k),k=1,n)

          ENDIF d_c
         ENDDO x_l
      ENDDO y_l

	CLOSE(21)
	CLOSE(22)

    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    print *, '!!!!!  FINISHED INTEGRATION   !!!!!'
    print *, '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	
	DEALLOCATE(detc,jdayf3,frp3,fsize3,parea3)
	
END PROGRAM frp_agg_rap
	
