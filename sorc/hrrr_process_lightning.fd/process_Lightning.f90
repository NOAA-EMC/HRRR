program process_Lightning
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2008-01-02
!
! ABSTRACT: 
!     This routine read in lightning data and 
!     map them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  NCEP BUFR lightning data
!
!   OUTPUT FILES:
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
!
  use mpi
  use map_utils
  use misc_definitions_module , only : PROJ_LC
  use constants_module ,only : EARTH_RADIUS_M
  use constants ,only : init_constants_derived, deg2rad
  use gridmod_gsimap ,only : nlon,nlat,init_general_transform,tll2xy,txy2ll
  use kinds, only: r_kind,i_kind

  implicit none
  INCLUDE 'netcdf.inc'
!
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid and map
  CHARACTER*180   geofile

!
!  For lightning data
!
  INTEGER,parameter ::    max_numStrike=1000000
  INTEGER ::    numStrike
  CHARACTER*180   lightsngle
  real,allocatable:: llon(:)    !
  real,allocatable:: llat(:)    !
  integer,allocatable:: ltime(:)   !
  integer,allocatable:: lstrike(:) !
  character*21,allocatable:: ctime(:)   !
  real :: rtmp
  integer,allocatable:: lquality(:) !

  REAL, allocatable :: lightning(:,:)   ! lightning  strakes
  REAL(r_kind), allocatable :: lightning_out(:,:)   ! lightning  strakes

  integer :: numNLDN_all, numNLDN_used
  integer :: numAlaska_all, numAlask_used
!
!! Declare namelists 
!
! SETUP (general control namelist) :
!
  character*10 :: analysis_time
  real :: trange_start,trange_end
  integer :: minute
  namelist/setup/analysis_time,minute,trange_start,trange_end
!
!  ** misc
      
  CHARACTER*180   workpath

  real :: LAT_LL_P,LON_LL_P
  real :: user_known_x, user_known_y
  real(r_kind) :: XC,YC
  real(r_kind)        :: rlon  ! earth longitude (radians)
  real(r_kind)        :: rlat  ! earth latitude  (radians)

  integer i,j,igrid,jgrid,nt

  integer :: NCID, istatus
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  real(r_kind),allocatable:: rxlon(:,:)    !
  real(r_kind),allocatable:: rylat(:,:)    !

  integer :: numlightning,idate
  real,allocatable:: savelon(:)    !
  real,allocatable:: savelat(:)    !



!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if(mype==0) then  ! only us one core mype==0
  numNLDN_all=0
  numNLDN_used=0
  minute=0

  open(15, file='lightning_bufr.namelist')
    read(15,setup)
  close(15)

  call init_constants_derived
!
! set geogrid fle name
!
  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT
!
!   setup  map
!
  allocate(rxlon(nlon,nlat))
  allocate(rylat(nlon,nlat))
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)

  rylat=ylat*deg2rad
  rxlon=xlon*deg2rad
  call init_general_transform(rylat,rxlon,mype)

  allocate(lightning(nlon,nlat))
  lightning=0

!
!  process NLDN data
!
    allocate(llon(max_numStrike))
    allocate(llat(max_numStrike))
    allocate(ltime(max_numStrike))
    allocate(lStrike(max_numStrike))

    lightsngle='lghtngbufr'
    read(analysis_time,'(I10)') idate
    call read_lightning_bufr(lightsngle,max_numStrike,analysis_time,minute,trange_start,trange_end,&
                             numStrike,llon,llat,ltime,lStrike)
    numNLDN_all=numStrike

    allocate(savelon(numStrike))
    allocate(savelat(numStrike))
    do i=1,numStrike
!        write(*,*) i, llon(i),llat(i),ltime(i),lStrike(i)
       savelon(i)=llon(i)
       savelat(i)=llat(i)
    enddo
!
!  check quality
!
    allocate(lquality(numStrike))
    lquality = 0    ! 0 good data,  > 0 bad data
    call Check_Lightning_QC(numStrike,llon,llat,ltime,lstrike,lquality)

    do i=1,numStrike

      if(lquality(i) == 0 ) then
        rlon=llon(i)*deg2rad
        rlat=llat(i)*deg2rad
        call tll2xy(rlon,rlat,xc,yc)

        igrid = int(XC+0.5)
        jgrid = int(YC+0.5)
        if( (igrid > 0 .and. igrid< nlon).and.  &
            (jgrid > 0 .and. jgrid< nlat)) then 
            lightning(igrid,jgrid) = lightning(igrid,jgrid) + lStrike(i)
            numNLDN_used=numNLDN_used+1
        endif
      endif

    enddo

    deallocate(llon)
    deallocate(llat)
    deallocate(ltime)
    deallocate(lStrike)
    deallocate(lquality)
!
!  statistic
!
  write(*,*) ' The total number of NLDN data is:', numStrike
  write(*,*) ' The number of NLDN data used is:', numNLDN_used

!
!     Find max reflectivity in each column
!
   allocate(lightning_out(4,nlon*nlat))
   numlightning=0
   DO j=1,nlat
   DO i=1,nlon
     if(lightning(i,j) > 0 ) then
       numlightning=numlightning+1
       lightning_out(1,numlightning)=float(i)
       lightning_out(2,numlightning)=float(j)
       lightning_out(3,numlightning)=float(minute)/60.0
       lightning_out(4,numlightning)=lightning(i,j)
       if(lightning_out(4,numlightning) > 1000.0 ) then
          lightning_out(4,numlightning)=1000.0
          write(6,*) 'high lightning strokes=',lightning(i,j),i,j
       endif
     endif
   ENDDO
   ENDDO
     write(*,*) 'Dump out results',numlightning,'out of',nlon*nlat
     OPEN(10,file=trim(workPath)//'LightningInGSI.dat',form='unformatted')
      write(10) 3,nlon,nlat,numlightning,1,2
      write(10) ((real(lightning_out(i,j)),i=1,3),j=1,numlightning)
      write(10) lightning
      write(10) numNLDN_all
      write(10) savelon(1:numNLDN_all)
      write(10) savelat(1:numNLDN_all)
     close(10)
!
!  
   write(6,*) 'cycle time is :', idate, ' minute is:',minute

   write(6,*) ' write lightning in BUFR'
   call write_bufr_lightning(1,nlon,nlat,numlightning,lightning_out,idate)

   write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

   endif   ! only us one core mype==0
   call MPI_FINALIZE(ierror)
!
end program process_Lightning 
!
