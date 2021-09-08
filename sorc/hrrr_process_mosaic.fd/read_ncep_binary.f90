  subroutine read_ncep_binary_head(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)

!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
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
  use kinds, only: r_kind,i_kind

  implicit none
!
!
  integer,intent(in) :: mype
!
  CHARACTER*256,intent(in) ::  mosaicfile

  INTEGER,intent(out) ::   mscNlon   ! number of longitude of mosaic data
  INTEGER,intent(out) ::   mscNlat   ! number of latitude of mosaic data
  INTEGER,intent(out) ::   mscNlev   ! number of vertical levels of mosaic data
  REAL,intent(out) :: lonMin,latMin,lonMax,latMax,dlon,dlat

  type mosaic_head
            integer(4):: nx
            integer(4):: ny
            integer(4):: nz
            real(4):: dx
            real(4):: dy
            real(4):: ctrl_lat, ctrl_lon
            real(4):: zp(31)
            real(4):: missing_value
            real(4):: no_radar_cover
  end type mosaic_head

  type(mosaic_head):: mhead
  integer(4) bufint
  real(4) bufreal

  integer(4) :: flsize(8), this_flsize

  real t_lat_s(8)
  real t_lat_n(8)
  real t_lon_w(8)
  real t_lon_e(8)
  data t_lat_s /40.0,40.0,40.0,40.0,  &
                20.0,20.0,20.0,20.0/
  data t_lon_w /-130.0,-110.0,-90.0,-80.0,  &
                -130.0,-110.0,-90.0,-80.0/
  data t_lat_n /55.0,55.0,55.0,55.0,  &
                40.0,40.0,40.0,40.0/
  data t_lon_e /-110.0,-90.0,-80.0,-60.0,  &
                -110.0,-90.0,-80.0,-60.0/

!  ** misc

  integer i,j,k,itype,iymdh,ier,jret,ifn

  INTEGER(i_kind)  ::  maxlvl, tversion
  INTEGER(i_kind)  ::  numlvl,numref

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
  flsize(1)=522609334; flsize(2)=522609334
  flsize(3)=261435334; flsize(4)=522609334
  flsize(5)=696696334; flsize(6)=696696334
  flsize(7)=348522334; flsize(8)=696696334
!
!   deal with certain tile
!
   this_flsize=flsize(mype+1)
!
   open(1,file=mosaicfile,access='stream', status='old',err=800)
      read(1,err=800) mhead
   close(1)
   bufint=mhead%nx
   mscNlon=bufint
   bufint=mhead%ny
   mscNlat=bufint
   bufint=mhead%nz
   mscNlev=bufint

   latMin=t_lat_s(mype+1)
   latMax=t_lat_n(mype+1)
   lonMin=t_lon_w(mype+1)
   lonMax=t_lon_e(mype+1)

   bufreal=mhead%dx
   dlon=bufreal
   bufreal=mhead%dy
   dlat=bufreal
   write(*,*) mscNlon,mscNlat,mscNlev
   write(*,*) 'Area of tile=',lonMin,latMin,lonMax,latMax,dlon,dlat

   return

800 continue
    write(*,*) 'There is a problem in processing tile:',trim(mosaicfile)
    stop 1234

end subroutine read_ncep_binary_head

subroutine read_ncep_binary_value(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   mscValue)

!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
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
  use kinds, only: r_kind,i_kind

  implicit none
!
!  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer,intent(in) :: mype
!
  CHARACTER*256,intent(in)::  mosaicfile

  INTEGER,intent(in) ::   mscNlon   ! number of longitude of mosaic data
  INTEGER,intent(in) ::   mscNlat   ! number of latitude of mosaic data
  INTEGER,intent(in) ::   mscNlev   ! number of vertical levels of mosaic data
  REAL,intent(out)   ::   mscValue(mscNlon,mscNlat,mscNlev)    ! reflectivity

  type mosaic_head
            integer(4):: nx
            integer(4):: ny
            integer(4):: nz
            real(4):: dx
            real(4):: dy
            real(4):: ctrl_lat, ctrl_lon
            real(4):: zp(31)
            real(4):: missing_value
            real(4):: no_radar_cover
  end type mosaic_head

  type(mosaic_head):: mhead
  real(4), allocatable :: ref(:,:,:)

  integer i,j,k

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
!   deal with certain tile
!
   allocate(ref(mscNlev,mscNlat,mscNlon))
!
   open(21,file=mosaicfile,access='stream', status='old')
      read(21) mhead,                 &
             (((ref(k,j,i),i=1,mscNlon),j=mscNlat,1,-1),k=1,mscNlev)
   close(21)

   print *, mype,mscNlev,mscNlat,mscNlon
   DO k=1, mscNlev
!     write(*,*) 'mype=',mype+1,k, maxval(ref(k,:,:)),minval(ref(k,:,:))

     Do j=1,mscNlat
     Do i=1,mscNlon
          mscValue(i,j,k)=ref(k,j,i)
     enddo
     enddo
   enddo
   
   deallocate(ref)
   return

800 continue
    write(*,*) 'There is a problem in read tile:',trim(mosaicfile)
    stop 1234

end subroutine read_ncep_binary_value
