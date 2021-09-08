module nc_readwrt_mod
!
!  module: functions to read and write netcdf files
!
!  Ming Hu
!
! program history log:
!   2017-04-10 Hu           initial build
! 
! Subroutines Included:
!   sub drefresh_cldsurf  - initialize RR related variables to default
!

  use netcdf
  implicit none

! set default to private
  private
! set subroutines to public
  public :: get_dim
  public :: get_time
  public :: get_field
  public :: replace_field
  public :: handle_err

contains

subroutine get_time(filename,iyear,imonth,iday,ihh,imm)
!
! find dimensions in wrf netcdf file
!
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2017-04-10
!
! abstract: 
!

  implicit none
!
  character*100,intent(in) :: filename
  integer,intent(out) :: iyear,imonth,iday,ihh,imm

  integer :: ncid, status
  integer :: DimId
  character(len=80) :: datestring

! open existing netCDF dataset
  status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
  if (status /= nf90_noerr) call handle_err(status)

!  get date from exisiting NC file
  status = nf90_get_att(ncid, NF90_GLOBAL, "SIMULATION_START_DATE", datestring)
  if (status /= nf90_noerr) call handle_err(status)
  read(datestring,'(I4,1x,I2,1x,I2,1x,I2,1x,I2)') iyear,imonth,iday,ihh,imm
!
! close netCDF dataset
  status = nf90_close(ncid)
  if (status /= nf90_noerr) call handle_err(status)

end subroutine get_time

subroutine get_dim(filename,nwe,nsn,nbt)
!
! find dimensions in wrf netcdf file
!
! prgmmr:  Ming Hu             org: GSD/AMB           date: 2017-04-10
!
! abstract: 
!

  implicit none
!
  character*100,intent(in) :: filename
  integer,intent(out) :: nwe,nsn,nbt

  integer :: ncid, status
  integer :: DimId

! open existing netCDF dataset
  status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
  if (status /= nf90_noerr) call handle_err(status)

!  get dimension from exisiting NC file
  status = nf90_inq_dimid(ncid,"west_east", DimId)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_Inquire_Dimension(ncid, DimId, len = nwe)
  if (status /= nf90_noerr) call handle_err(status)
!  write(*,*) 'west_east=',nwe
  status = nf90_inq_dimid(ncid,"south_north", DimID)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_Inquire_Dimension(ncid, DimID, len = nsn)
  if (status /= nf90_noerr) call handle_err(status)
!  write(*,*) 'south_north=',nsn
  status = nf90_inq_dimid(ncid,"bottom_top", DimId)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_Inquire_Dimension(ncid, DimId, len = nbt)
  if (status /= nf90_noerr) call handle_err(status)
!  write(*,*) 'bottom_top=',nbt
!
! close netCDF dataset
  status = nf90_close(ncid)
  if (status /= nf90_noerr) call handle_err(status)

end subroutine get_dim

subroutine get_field(ncid,varname,nx,ny,nz,value)
!
  use netcdf
!
  implicit none
!
  character*10,intent(in) :: varname
  integer,intent(in)  :: ncid
  integer,intent(in)  :: nx,ny,nz
  real,intent(out) :: value(nx,ny,nz,1)
! 
  integer :: status
  integer :: varid
  integer :: ends(4),start(4)

  integer :: nDims,ndim
  integer :: dimids(4)
  character*20 :: dimname

!
  integer :: i,k
!
  write(*,*) 'get variable ', trim(varname)
! get variable IDs
  status = nf90_inq_varid(ncid, trim(varname), VarId)
  if(status /= nf90_NoErr) call handle_err(status)

!  get dimensions
  ends=1
  start=1
  status = nf90_inquire_variable(ncid, VarId, ndims = nDims)
  if(status /= nf90_NoErr) call handle_err(status)
  status = nf90_inquire_variable(ncid, VarId, dimids = dimids(1:nDims))
  if(status /= nf90_NoErr) call handle_err(status)
  do i=1,nDims
    status = nf90_inquire_dimension(ncid, dimids(i), dimname, len = ndim)
    if (status /= nf90_noerr) call handle_err(status)
    ends(i)=ndim
!    write(*,*) i,dimname,ends(i)
  enddo
!  do i=1,4
!    write(*,*) 'dim=',i,start(i),ends(i)
!  enddo
! get values of variables
   if(ends(1) == nx .and. ends(2) == ny .and. ends(3) == nz) then
      status = nf90_get_var(ncid, VarId, Value, &
                            start = start(1:4) , &
                            count = ends(1:4))
      if(status /= nf90_NoErr) call handle_err(status)
      do k=1,nz
         write(*,*) k,maxval(Value(:,:,k,:)),minval(Value(:,:,k,:))
      enddo
   else
      write(*,*) 'missing dimension',nx,ny,nz
   endif

end subroutine get_field

subroutine replace_field(ncid,varname,nx,ny,nz,value)
!
  use netcdf
!
  implicit none
!
  character*10,intent(in) :: varname
  integer,intent(in)  :: ncid
  integer,intent(in)  :: nx,ny,nz
  real,intent(in) :: value(nx,ny,nz,1)
! 
  integer :: status
  integer :: varid
  integer :: ends(4),start(4)

  integer :: nDims,ndim
  integer :: dimids(4)
  character*20 :: dimname

!
  integer :: i,k
!
  write(*,*) 'replace variable ', trim(varname)
! get variable IDs
  status = nf90_inq_varid(ncid, trim(varname), VarId)
  if(status /= nf90_NoErr) call handle_err(status)

!  get dimensions
  ends=1
  start=1
  status = nf90_inquire_variable(ncid, VarId, ndims = nDims)
  if(status /= nf90_NoErr) call handle_err(status)
  status = nf90_inquire_variable(ncid, VarId, dimids = dimids(1:nDims))
  if(status /= nf90_NoErr) call handle_err(status)
  do i=1,nDims
    status = nf90_inquire_dimension(ncid, dimids(i), dimname, len = ndim)
    if (status /= nf90_noerr) call handle_err(status)
    ends(i)=ndim
!    write(*,*) i,dimname,ends(i)
  enddo
!  do i=1,4
!    write(*,*) 'dim=',i,start(i),ends(i)
!  enddo
! get values of variables
   if(ends(1) == nx .and. ends(2) == ny .and. ends(3) == nz) then
      do k=1,nz
         write(*,*) k,maxval(Value(:,:,k,:)),minval(Value(:,:,k,:))
      enddo
      status = nf90_put_var(ncid, VarId, value, &
                 start=start(1:3), count = ends(1:3) )
      if(status /= nf90_NoErr) call handle_err(status)
   else
      write(*,*) 'missing dimension',nx,ny,nz
   endif

end subroutine replace_field

subroutine handle_err(status)
  use netcdf
  implicit none
!
  integer, intent ( in) :: status
  if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      stop "Stopped"
  end if
end subroutine handle_err

end module nc_readwrt_mod
