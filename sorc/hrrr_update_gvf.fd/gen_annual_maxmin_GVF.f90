PROGRAM gen_annual_maxmin_GVF
!
! read in annual maximum and minimum vegetation fraction and 
!    write out
!
!   Ming Hu, 2017-04-10
!
!
  use netcdf
  use nc_readwrt_mod, only : handle_err
  use nc_readwrt_mod, only : get_dim
  use nc_readwrt_mod, only : get_field
!
  implicit none
!
  character*100 :: filename
!
  integer :: nx,ny,nz
!
  integer :: ncid, status
  character*20 :: varname
!  from wrf netcdf 
  real, allocatable :: field2d(:,:)
!
  integer :: i,j
!
!========
!  
  open(12,file='annual_maxmin_vegfrc.bin',form='unformatted')

  filename='wrfinput_d01'
!
  call get_dim(filename,nx,ny,nz)
  write(*,*) 'dimension =',nx,ny,nz
  write(12) nx,ny
!
  allocate(field2d(nx,ny))
!
!
  write(*,*) 'open file =',trim(filename)
! open existing netCDF dataset
  status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
  if (status /= nf90_noerr) call handle_err(status)

! read latlonn and ver frc from wrf inpput
  varname='SHDMAX'
  call get_field(ncid,varname,nx,ny,1,field2d)
  do j=1,ny
     do i=1,nx
        field2d(i,j)=max(0.0,min(100.0,field2d(i,j)))
     enddo
  enddo
  write(12) field2d
!
  varname='SHDMIN'
  call get_field(ncid,varname,nx,ny,1,field2d)
  do j=1,ny
     do i=1,nx
        field2d(i,j)=max(0.0,min(100.0,field2d(i,j)))
     enddo
  enddo
  write(12) field2d

  status = nf90_close(ncid)
  if (status /= nf90_noerr) call handle_err(status)
!
  close(12)
  deallocate(field2d)

end program
