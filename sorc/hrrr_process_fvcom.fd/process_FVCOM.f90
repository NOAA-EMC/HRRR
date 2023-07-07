! process_FVCOM.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 12 Dec 2018
!
! This is the code to grab lake surface temperature and aerial ice
! concentration from GLERL-provided FVCOM forecast files (which have
! already been mapped to HRRR grid).  The code uses new object-oriented
! coding to handle the data, performing some minimal QC.
!
! The executable is now created via cmake.

program process_FVCOM

   use mpi
   use module_map_utils, only: map_util
   use kinds, only: r_kind, i_kind, r_single
   use module_ncio, only: ncio
   use module_nwp, only: fcst_nwp

   implicit none

! MPI variables
  integer :: npe, mype, mypeLocal,ierror
!

!  New object-oriented declarations

   type(ncio) :: geo
   type(fcst_nwp) :: fcst
   type(map_util) :: map

!  Grid variables

   character*180 :: geofile
   character*2 :: workPath
   character*1 :: char1

   integer :: MAP_PROJ, NLON, NLAT
   integer :: hrrrlon, hrrrlat, hrrrtimes
   integer :: fvlon, fvlat, fvtimes
   integer :: i, j, t1, t2

   real :: rad2deg = 180.0/3.1415926
   real :: userDX, userDY, CEN_LAT, CEN_LON
   real :: userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON
   real :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
   real :: knowni, knownj, dx
   real :: one, pi, deg2rad

!  For lightning data

   character*180 :: hrrrfile
   character*180 :: fvcomfile

   real(r_single), allocatable :: hrrrice(:,:), hrrrsst(:,:)
   real(r_single), allocatable :: hrrrsfcT(:,:), hrrrmask(:,:)
   real(r_single), allocatable :: fvice(:,:), fvsst(:,:)
   real(r_single), allocatable :: fvsfcT(:,:), fvmask(:,:)
!
   real(r_single)    :: xice_threshold
   integer(i_kind)   :: num_seaice2water, num_water2seaice
   real(r_single),allocatable::xland_rr(:,:)  ! XLAND
   real(r_single),allocatable::lu_index(:,:)  ! LU_INDEX
   integer,allocatable:: ivgtyp(:,:)     ! IVGTYP
   integer,allocatable:: isltyp(:,:)     ! ISLTYP

!  Declare namelists
!  SETUP (general control namelist) :

   integer :: update_type

   namelist/setup/update_type, t2

! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  xice_threshold = 0.02
!
! NCEP LSF has to use all cores allocated to run this application 
! but this if check can make sure only one core run through the real code.
if(mype==0) then
!

   read(5,setup)
   write(*,*) 'From namelist: update type is ', update_type
   write(*,*) 'From namelist: t2 is ', t2

!  Set geogrid fle name and obtain grid parameters

   workPath='./'
   write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'
   write(*,*) 'geofile', trim(geofile)
   call geo%open(trim(geofile),'r',200)
   call geo%get_dim("west_east",NLON)
   call geo%get_dim("south_north",NLAT)
   write(*,*) 'NLON,NLAT:', NLON, NLAT
   call geo%get_att("DX",userDX)
   call geo%get_att("DY",userDY)
   call geo%get_att("CEN_LAT",CEN_LAT)
   call geo%get_att("CEN_LON",CEN_LON)
   call geo%get_att("TRUELAT1",userTRUELAT1)
   call geo%get_att("TRUELAT2",userTRUELAT2)
   call geo%get_att("MOAD_CEN_LAT",MOAD_CEN_LAT)
   call geo%get_att("STAND_LON",STAND_LON)
   call geo%get_att("MAP_PROJ",MAP_PROJ)
   call geo%close
   write(*,*) userDX, userDY, CEN_LAT, CEN_LON
   write(*,*) userTRUELAT1, userTRUELAT2, MOAD_CEN_LAT, STAND_LON, MAP_PROJ

   write(*,*) 'Finished reading geofile.'
   write(*,*) ' '

!  Allocate variables for I/O

   allocate(hrrrice(nlon,nlat))
   allocate(hrrrsfcT(nlon,nlat))
   allocate(hrrrsst(nlon,nlat))
   allocate(hrrrmask(nlon,nlat))  ! LANDMASK

   allocate(xland_rr(nlon,nlat))
   allocate(lu_index(nlon,nlat))
   allocate(ivgtyp(nlon,nlat))
   allocate(isltyp(nlon,nlat))
   
   allocate(fvice(nlon,nlat))
   allocate(fvsfcT(nlon,nlat))
   allocate(fvsst(nlon,nlat))
   allocate(fvmask(nlon,nlat))

!  Read HRRR input datasets

   hrrrfile='wrf_inout'
   t1=1

   call fcst%initial(' HRRR')
   call fcst%list_initial
   call fcst%read_n(trim(hrrrfile),'HRRR',hrrrlon,hrrrlat,hrrrtimes,t1,hrrrmask,hrrrsst,hrrrice,hrrrsfcT)
   call fcst%finish

   call geo%open(trim(hrrrfile),'r',300)
   call geo%get_var("XLAND",NLON,NLAT,xland_rr)
   call geo%get_var("LU_INDEX",NLON,NLAT,lu_index)
   call geo%get_var("IVGTYP",NLON,NLAT,ivgtyp)
   call geo%get_var("ISLTYP",NLON,NLAT,isltyp)
   call geo%close
!  Check that the dimensions match

   if (hrrrlon .ne. nlon .or. hrrrlat .ne. nlat ) then
      write(*,*) 'ERROR: HRRR/geofile dimensions do not match:'
      write(*,*) 'hrrrlon: ', hrrrlon
      write(*,*) 'nlon: ', nlon
      write(*,*) 'hrrrlat: ', hrrrlat
      write(*,*) 'nlat: ', nlat
      stop 123
   endif

   write(*,*) 'hrrrtimes: ', hrrrtimes
   write(*,*) 'time to use: ', t1

!  Read FVCOM input datasets

   fvcomfile='fvcom.nc'

   call fcst%initial('FVCOM')
   call fcst%list_initial
   call fcst%read_n(trim(fvcomfile),'FVCOM',fvlon,fvlat,fvtimes,t2,fvmask,fvsst,fvice,fvsfcT)
   call fcst%finish

!  Check that the dimensions match

   if (fvlon .ne. nlon .or. fvlat .ne. nlat) then
      write(*,*) 'ERROR: FVCOM/geofile dimensions do not match:'
     write(*,*) 'fvlon: ', fvlon
     write(*,*) 'nlon: ', nlon
     write(*,*) 'fvlat: ', fvlat
     write(*,*) 'nlat: ', nlat
     stop 135
   endif

   write(*,*) 'fvtimes: ', fvtimes
   write(*,*) 'time to use: ', t2

!  Update with FVCOM fields

   num_seaice2water=0
   num_water2seaice=0
   do j=1,nlat
      do i=1,nlon
         if (fvmask(i,j) > 0. .and. fvsst(i,j) .ge. -90.0) then
            hrrrsst(i,j) = fvsst(i,j) + 273.15
            hrrrsfcT(i,j) = fvsst(i,j) + 273.15

            hrrrice(i,j) = fvice(i,j)
            !-- Consistency check
	    if(hrrrice(i,j) < xice_threshold .and. hrrrmask(i,j) == 1.) num_seaice2water = num_seaice2water + 1
            if(hrrrice(i,j) < xice_threshold) then
            !-- turn old seaice into water
                ivgtyp(i,j)=17
                lu_index(i,j)=17
                hrrrmask(i,j)=0.
                xland_rr(i,j)=2.
                isltyp(i,j)=14
                hrrrice(i,j)=0.
            endif
            if(hrrrice(i,j) >= xice_threshold .and. hrrrmask(i,j) == 0.) num_water2seaice=num_water2seaice + 1
            if(hrrrice(i,j) >= xice_threshold) then  
            !-- turn old water into seaice
                ivgtyp(i,j)=15
                lu_index(i,j)=15
                hrrrmask(i,j)=1.
                xland_rr(i,j)=1.
                isltyp(i,j)=16
            endif
         endif
      enddo
   enddo
!
  write(*,*) 'SUMMARY on seaice:'
  write(*,*) 'grid point from old seaice into water: ', num_seaice2water
  write(*,*) 'grid point from old water  into seaice: ', num_water2seaice

! Write out HRRR file again

   call geo%open(trim(hrrrfile),'w',300)
   if (update_type .eq. 1) then
      call geo%replace_var("SST",NLON,NLAT,hrrrsst)
      call geo%replace_var("TSK",NLON,NLAT,hrrrsfcT)
   else
      call geo%replace_var("SEAICE",NLON,NLAT,hrrrice)
      call geo%replace_var("LANDMASK",NLON,NLAT,hrrrmask)
      call geo%replace_var("XLAND",NLON,NLAT,xland_rr)
      call geo%replace_var("LU_INDEX",NLON,NLAT,lu_index)
      call geo%replace_var("IVGTYP",NLON,NLAT,ivgtyp)
      call geo%replace_var("ISLTYP",NLON,NLAT,isltyp)

   endif
   call geo%close

   write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

endif ! mype==0

call MPI_FINALIZE(ierror)


end program process_FVCOM 
