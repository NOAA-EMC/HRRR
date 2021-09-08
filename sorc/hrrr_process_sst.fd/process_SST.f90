PROGRAM process_SST
!
!   PRGMMR: Ming Hu  nd Tanya Smirnova   ORG: GSD        DATE: 2010-09-25
!
! ABSTRACT: 
!     This routine reads in SST
!
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

  use mpi

  implicit none
!
  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! RR grid
  integer :: nlon,nlat
! RR in LC
!  parameter (nlon=648,nlat=647)
! RR in RLL
!  parameter (nlon=758,nlat=567)
  real,allocatable  :: xlon(:,:)    !
  real,allocatable  :: ylat(:,:)    !
  real,allocatable  :: xland(:,:)   !
  real,allocatable  :: vegtyp(:,:)   !

  real, allocatable :: sstRR(:,:)    ! sst in RR 
  real, allocatable :: sstGlobal(:,:)  ! sst from global dataset
  integer, allocatable :: imaskSST(:,:)

!
  character*100 sst_file
  integer :: istatus
  integer :: i,j,iwater,ilake,iice
!
  INTEGER :: iyear, imonth, iday, ihr

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!
  if(mype==0) then

     call GET_DIM_ATT_geo('./geo_em.d01.nc',nlon,nlat)
     write(*,*) 'grid dimension =',nlon,nlat

     allocate(xlon(nlon,nlat))
     allocate(ylat(nlon,nlat)) 
     allocate(xland(nlon,nlat))
     allocate(vegtyp(nlon,nlat))

     call GET_RR_GRID(xlon,ylat,nlon,nlat,xland,vegtyp)
!
! read in global 0.083333 degre SST
     allocate(sstGlobal(4320,2160))
!     call read_sstGlobal(sstGlobal,iyear,imonth,iday,ihr)
!     write(6,*)' read in global sst data', iyear, imonth, iday, ihr
!     write(*,*) 'sst from grib1 file=',maxval(sstGlobal),minval(sstGlobal)
!
     sst_file='latest.SST'
     call read_sstGlobal_grib2(sst_file,sstGlobal,iyear,imonth,iday,ihr)
     write(6,*)' read in global sst data', iyear, imonth, iday, ihr
     write(*,*) 'sst from grib2 file=',maxval(sstGlobal),minval(sstGlobal)

     allocate(imaskSST(4320,2160))
     OPEN (11,FILE='RTG_SST_landmask.dat')
! 
! Read in land sea tags (0 for ocean; 3 for land) 
!
        READ (11,'(80I1)') imaskSST
        CLOSE (11)
        print *,'imaskSST(600,210) ', imaskSST(600,210)
     close(11)
!
! interpolate to RAP/HRRR grid
!
     allocate(sstRR(nlon,nlat))
     sstRR=0
     call sstGlobal2RR (sstGlobal,imaskSST,xland,nlon,nlat,xlon,ylat,sstRR)
     write(6,*)'from global  data ylat/xlon/sstRR(516,258)',   &
                ylat(516,258),xlon(516,258),sstRR(516,258)

! High-resolution SST plus Great Lakes data
!      iwater=16 ! USGS
     iwater=17 ! MODIS
     ilake =21 ! MODIS
     iice = 15 ! MODIS

!     write(6,*)' read 14km sst data'
!     call sst14k (sstRR, ylat, xlon, vegtyp, iwater, ilake, nlon, nlat)
!     write(6,*)'after sst14k ylat/xlon/sstRR(516,258)',                 &
!                 ylat(516,258),xlon(516,258),sstRR(516,258)
!
! update model sst and tsk
!
     call update_SST_netcdf_mass(sstRR, ylat, xlon, nlon, nlat,xland,vegtyp,ilake,iice)
!
     write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

  endif ! mype==0

  call MPI_FINALIZE(ierror)
!
END PROGRAM process_SST

SUBROUTINE read_sstGlobal(sst,iyear,imonth,iday,ihr)
!
!   PRGMMR: Ming Hu, Tanya Smirnova    ORG: GSD        DATE: 2010-09-27
!
! ABSTRACT:
!     This routine read in GLOBAL SST data from a grib file
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT 
!   OUTPUT 
!      sst :  sea surface temperature
!      iyear: Year
!      imonth: month
!      iday:   day
!      ihr:  hour
!   INPUT FILES:  sst_1degree
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


!  USE GRIB_MOD
  implicit none

  INTEGER JF, MBUF
!  PARAMETER (JF=360*180)
  PARAMETER (JF=4320*2160)
  INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
  LOGICAL*1 LB(JF)
  REAL, intent(out)::  SST(JF)
  PARAMETER(MBUF=256*2160)
  CHARACTER CBUF(MBUF)

  INTEGER :: LUGB,LUGI, J, IRET, K, KF
  character(200):: FNAME1

  INTEGER :: I, iyear, imonth, iday, ihr

!-----------------------------------------------------------------------
  LUGB = 11
  LUGI = 0
  J = 0

!  FNAME1='./sst_1degree'
  FNAME1='SSTRTG'
  call baopen(LUGB,trim(fname1),IRET)
!grib2  call baopenr(LUGB,trim(fname1),IRET)
  if ( IRET .ne. 0) then
      write(6,*) 'bad baopen!!! ', IRET
      STOP
  endif

  JPDS=-1
  JGDS=-1
  JPDS(5) = 11
  call GETGB(LUGB,LUGI,JF,J,JPDS,JGDS,    &
             KF,K,KPDS,KGDS,LB,SST,IRET)

  if (IRET .ne. 0) then
    write(6,*) 'bad getgb ', IRET
    STOP
  endif
  write(*,*)  'sst=', maxval(sst), minval(sst),k

!  iyear=2000 + KPDS(8)
!  imonth=KPDS(9)
!  iday=KPDS(10)
!  ihr=KPDS(11)
!       POLAR STEREOGRAPHIC GRIDS
!      KGDS(i)
!          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
!          (3)   - N(J) NR POINTS ALONG LON CIRCLE
!          (4)   - LA(1) LATITUDE OF ORIGIN
!          (5)   - LO(1) LONGITUDE OF ORIGIN
!          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!          (7)   - LOV GRID ORIENTATION
!          (8)   - DX - X DIRECTION INCREMENT
!          (9)   - DY - Y DIRECTION INCREMENT
!          (10)  - PROJECTION CENTER FLAG
!          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
!  do j=1,1024,5
!    write(*,'(1024i1)') (int(F((j-1)*1024 + i)+0.2),i=1,1024,4)
!  enddo
end subroutine 

SUBROUTINE GET_RR_GRID(xlon,ylat,nlon,nlat,xland,vegtyp)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine read in Rapid Refresh grid and land mask
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT:
!
!   OUTPUT:
!      xlon:  longitude in each grid
!      ylat:  latitude in each grid
!      xland: land mask
!
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


!
  implicit none
!
  INCLUDE 'netcdf.inc'

!  grid
  integer, intent(in) :: nlon,nlat
  real, intent(out):: xlon(nlon,nlat)    !
  real, intent(out):: ylat(nlon,nlat)    !
  real, intent(out):: xland(nlon,nlat)   !
  real, intent(out):: vegtyp(nlon,nlat)  !
!
  integer :: tnlon,tnlat
!
  integer :: NCID
  CHARACTER*180   geofile
  CHARACTER*180   workPath

!
! set geogrid fle name
!
  workPath=''
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile ', trim(geofile)
  call GET_DIM_ATT_geo(geofile,TNLON,TNLAT)
  if( (TNLON.ne.NLON) .or. (TNLAT.ne.NLAT)) then
    write(6,*) ' unmatched dimension'
    write(*,*) 'NLON,NLAT',NLON,NLAT, 'TNLON,TNLAT',TNLON,TNLAT
    stop 123
  endif 
!
!  get GSI horizontal grid in latitude and longitude
!

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon,xland,vegtyp)
  call CLOSE_geo(NCID)


END SUBROUTINE GET_RR_GRID

SUBROUTINE read_sstGlobal_grib2(filename,sst,idatayr,idatamon,idataday,idatahh)
!
!   PRGMMR: Ming Hu,    ORG: GSD        DATE: 2017-06-11
!
! ABSTRACT:
!     This routine read in GLOBAL SST data from a grib2 file
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT 
!   OUTPUT 
!      sst :  sea surface temperature
!      idatayr: Year
!      idatamon: month
!      idataday:   day
!      idatahh:  hour
!   INPUT FILES:  fiename
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


  use grib2_read_mod, only : read_grib2_head_dim,read_grib2_head_time
  use grib2_read_mod, only : read_grib2_sngle

!  USE GRIB_MOD
  implicit none

  INTEGER JF
  PARAMETER (JF=4320*2160)
  character*100,intent(in) :: filename
  REAL, intent(out)::  SST(JF)
  INTEGER,intent(out) :: idatayr,idatamon,idataday,idatahh

  real :: rlatmin,rlonmin
  real*8  :: rdx,rdy
  integer :: nx,ny
  
  integer :: idatamm

  logical :: fileexist
  integer :: ntot

!-----------------------------------------------------------------------

     inquire(file=trim(filename),exist=fileexist)
     if( .not. fileexist) then
        write(*,*) 'file is not exist: ',trim(filename)
        write(*,*) 'stop update SST'
        stop
     endif
     call read_grib2_head_dim(filename,nx,ny,rlonmin,rlatmin,rdx,rdy)
     write(*,*) nx,ny,rlonmin,rlatmin,rdx,rdy
     call read_grib2_head_time(filename,idatayr,idatamon,idataday,idatahh,idatamm)   
     write(*,'(a,5I5)') 'data: idatayr,idatamon,idataday,idatahh,idatamm=',&
                      idatayr,idatamon,idataday,idatahh,idatamm

     ntot = nx*ny
     call read_grib2_sngle(filename,ntot,sst)

end subroutine 
