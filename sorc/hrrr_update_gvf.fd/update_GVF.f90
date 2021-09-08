PROGRAM update_GVF
!
! read in real-time vegetation fraction and 
!    update monthly vegetation fracction in wrfinput
!
!   Ming Hu, 2017-04-10
!
!
  use mpi
  use netcdf
  use nc_readwrt_mod, only : handle_err
  use nc_readwrt_mod, only : get_dim,get_time
  use nc_readwrt_mod, only : get_field,replace_field
  use grib2_read_mod, only : read_grib2_head_dim,read_grib2_head_time
  use grib2_read_mod, only : read_grib2_sngle
!
  implicit none

! MPI variables
  integer :: npe, mype, mypeLocal,ierror
!
  character*100 :: filename
!
  integer :: nx,ny,nz
  integer :: nxa,nya
  integer :: src_maxmin
  integer :: src_gvf
!
  integer :: ncid, status
  character*20 :: varname
!  from wrf netcdf 
  real, allocatable :: xlon(:,:),ylat(:,:)
  real, allocatable :: vegfrc_wrf(:,:)
  real, allocatable :: vegfrc(:,:)
  real, allocatable :: vegfrc_max(:,:)
  real, allocatable :: vegfrc_min(:,:)
  real, allocatable :: field2d(:,:)
!
!  for grib2
!
  real :: rlatmin,rlonmin
  real*8  :: rdx,rdy
  integer :: nxobs,nyobs
  logical :: fileexist
  integer :: ntot
  real,allocatable :: vegfrc_obs(:,:)
!
! for binary
  character, allocatable :: gvf4km_char(:)
!
!  GVF grib 2 has start date in file
  integer,parameter :: maxday=10+7
  integer :: i,j,idate5(5)
  integer :: minbk,mindata
  integer :: ibkyr,ibkmon,ibkday,ibkhh,ibkmm
  integer :: idatayr,idatamon,idataday,idatahh,idatamm
  real :: vegfrcsmax,vegfrcsmin
!
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!
! NCEP LSF has to use all cores allocated to run this application 
! but this if check can make sure only one core run through the real code.
if(mype==0) then
!
!========
!  
! src_maxmin=1 : calculate from raw data
!            2 : read in from processed data
!           <=0 : no max/min
!
  src_maxmin=1
!
!  src_gvf=1   : grib2
!          2   : geogrid binary
  src_gvf=1
  filename='wrf_inout'
!
! get date from wrf inout file 
  call get_time(filename,ibkyr,ibkmon,ibkday,ibkhh,ibkmm)
  write(*,'(a,5I5)') 'background: ibkyr,ibkmon,ibkday,ibkhh,ibkmm =', &
                      ibkyr,ibkmon,ibkday,ibkhh,ibkmm
  idate5(1)=ibkyr
  idate5(2)=ibkmon
  idate5(3)=ibkday
  idate5(4)=ibkhh
  idate5(5)=ibkmm
  call w3fs21(idate5,minbk)
!
! get dimension from wrf inout file
  call get_dim(filename,nx,ny,nz)
  write(*,*) 'dimension =',nx,ny,nz
!
  allocate(xlon(nx,ny),ylat(nx,ny))
  allocate(vegfrc_wrf(nx,ny))
!
!
  write(*,*) 'open file =',trim(filename)
! open existing netCDF dataset
  status = nf90_open(path = trim(filename), mode = nf90_nowrite, ncid = ncid)
  if (status /= nf90_noerr) call handle_err(status)

! read latlonn and ver frc from wrf inout
  varname='XLAT'
  call get_field(ncid,varname,nx,ny,1,ylat)
  varname='XLONG'
  call get_field(ncid,varname,nx,ny,1,xlon)
  varname='VEGFRA'
  call get_field(ncid,varname,nx,ny,1,vegfrc_wrf)

! close netCDF dataset
  status = nf90_close(ncid)
  if (status /= nf90_noerr) call handle_err(status)
!
! ====
!
  if(src_gvf==1) then
!
!   read veg fraction from grib2
     filename='GVF-WKL-GLB.grib2'
     inquire(file=trim(filename),exist=fileexist)
     if( .not. fileexist) then
        write(*,*) 'file is not exist: ',trim(filename)
        write(*,*) 'stop update veg frc'
        stop
     endif
     call read_grib2_head_dim(filename,nxobs,nyobs,rlonmin,rlatmin,rdx,rdy)
     write(*,*) nxobs,nyobs,rlonmin,rlatmin,rdx,rdy
     call read_grib2_head_time(filename,idatayr,idatamon,idataday,idatahh,idatamm)
     write(*,'(a,5I5)') 'data: idatayr,idatamon,idataday,idatahh,idatamm=',&
                      idatayr,idatamon,idataday,idatahh,idatamm
     idate5(1)=idatayr
     idate5(2)=idatamon
     idate5(3)=idataday
     idate5(4)=idatahh
     idate5(5)=idatamm
     call w3fs21(idate5,mindata)
! time differenece between background and GVF data larger than 8 days
     if(abs(minbk-mindata) > maxday*24*60) then
        write(*,*) 'real-time GVF is ',maxday-7,' days older than background'
        stop
     endif

     allocate(vegfrc_obs(nxobs,nyobs))
     ntot = nxobs*nyobs
     call read_grib2_sngle(filename,ntot,vegfrc_obs)
     write(*,*) 'gvf from grib2:  read in=',maxval(vegfrc_obs),minval(vegfrc_obs)
  elseif(src_gvf==2) then
!
!   read veg fraction from geogrid binary format file
     filename='GVF-WKL-GLB_char.bin'
     nxobs=10000
     nyobs=5000
     rlonmin=180.0000
     rlatmin=-89.96400
     rdx=3.600000000E-002
     rdy=3.600000000E-002
     allocate(vegfrc_obs(nxobs,nyobs))
     allocate(gvf4km_char(nxobs))

     inquire(file=trim(filename),exist=fileexist)
     if(fileexist) then
        open(55, file=trim(filename), form='unformatted', &
                            access='direct', recl=nxobs/4)
        do j=1,nyobs
           read(55,rec=2*nyobs+j) gvf4km_char(1:nxobs)
           do i=1,nxobs
              if(gvf4km_char(i)=='0') then
                 vegfrc_obs(i,j)=0.0
               else
                 vegfrc_obs(i,j)=float(iachar(gvf4km_char(i)))
               endif
           enddo
        enddo
     else
        write(*,*) 'file is not exist: ',trim(filename)
        write(*,*) 'stop update veg frc'
        stop
     endif
  else
     write(*,*) 'unknow gvf source, stop'
     stop
  endif
!
! ====
!  interpolate real time VGF to wrf grid
!
  allocate(vegfrc(nx,ny))
  allocate(vegfrc_max(nx,ny))
  allocate(vegfrc_min(nx,ny))
  call interp_bili_2d(rlonmin,rlatmin,rdx,rdy,nx,ny,xlon,ylat,vegfrc,nxobs,nyobs,vegfrc_obs)
!
!  get max and min GVF
!
  if(src_maxmin==1) then
     write(*,*) 'calculate max GVF',nxobs,nyobs
     filename='gvf_VIIRS_4KM.MAX.1gd4r.new'
     inquire(file=trim(filename),exist=fileexist)
     if(fileexist) then
        open(56, file=trim(filename),form='unformatted', access='direct',recl=nxobs,convert='big_endian')
        do j=1,nyobs
           read(56,rec=j) vegfrc_obs(1:nxobs,j)
        enddo
        close (56)
        vegfrc_obs=vegfrc_obs*100.0
        call interp_bili_2d(rlonmin,rlatmin,rdx,rdy,nx,ny,xlon,ylat,vegfrc_max,nxobs,nyobs,vegfrc_obs)
     else
        write(*,*) 'min GVF file is not exist:',trim(filename)
        src_maxmin=-1
     endif

     write(*,*) 'calculate min GVF'
     filename='gvf_VIIRS_4KM.MIN.1gd4r.new'
     inquire(file=trim(filename),exist=fileexist)
     if(fileexist) then
        open(57, file=trim(filename), form='unformatted', access='direct',recl=nxobs,convert='big_endian')
        do j=1,nyobs
           read(57,rec=j) vegfrc_obs(1:nxobs,j)
        enddo
        close (57)
        vegfrc_obs=vegfrc_obs*100.0
        call interp_bili_2d(rlonmin,rlatmin,rdx,rdy,nx,ny,xlon,ylat,vegfrc_min,nxobs,nyobs,vegfrc_obs)
     else
        write(*,*) 'min GVF file is not exist:',trim(filename)
        src_maxmin=-1
     endif

  elseif(src_maxmin==2) then  ! read in max min GVF from a file over the
                              ! analysis grid
     filename='annual_maxmin_vegfrc.bin'
     inquire(file=trim(filename),exist=fileexist)
     if(fileexist) then
        open(12,file=trim(filename),form='unformatted')
        read(12) nxa,nya
        if(nxa .ne. nx .or. nya.ne.ny) then
           write(*,*) 'Annual maxmin veg frc has mismtached dimension to wrfinput' 
           stop 1234
        endif  
        read(12) vegfrc_max
        read(12) vegfrc_min
        close(12)
     else
        write(*,*) 'file is not exist: ',trim(filename)
        write(*,*) 'no update to annual max and min veg frc'
        src_maxmin=-2
     endif
  endif
!
! Now write new Veg frc to WRF inout file
!
  filename='wrf_inout'
  write(*,*) 'open file =',trim(filename)
! open existing netCDF dataset
  status = nf90_open(path = trim(filename), mode = nf90_write, ncid = ncid)
  if (status /= nf90_noerr) call handle_err(status)

  if(src_maxmin>0) then
     do j=1,ny
        do i=1,nx
           vegfrc_max(i,j)=max(0.0,min(100.0,vegfrc_max(i,j)))
        enddo
     enddo
     varname='SHDMAX'
     call replace_field(ncid,varname,nx,ny,1,vegfrc_max)

     do j=1,ny
        do i=1,nx
           vegfrc_min(i,j)=max(0.0,min(100.0,vegfrc_min(i,j)))
        enddo
     enddo
     varname='SHDMIN'
     call replace_field(ncid,varname,nx,ny,1,vegfrc_min)
  endif
! bound the real-time value with the climate and maxmin value.
  do j=1,ny
     do i=1,nx
        vegfrcsmax=min(100.0,vegfrc_wrf(i,j)+30.0)
        vegfrcsmin=max(  0.0,vegfrc_wrf(i,j)-30.0)
        if(src_maxmin>0) then
           if((vegfrcsmin <=vegfrc_max(i,j)) .and. (vegfrc_min(i,j)<=vegfrcsmax)) then
              vegfrcsmax=min(100.0,min(vegfrcsmax,vegfrc_max(i,j)))
              vegfrcsmin=max(  0.0,max(vegfrcsmin,vegfrc_min(i,j)))
           endif
        endif
        vegfrc(i,j)=max(vegfrcsmin,min(vegfrcsmax,vegfrc(i,j)))
     enddo
  enddo
  varname='VEGFRA'
  call replace_field(ncid,varname,nx,ny,1,vegfrc)

! close netCDF dataset
  status = nf90_close(ncid)
  if (status /= nf90_noerr) call handle_err(status)
!
!
  deallocate(vegfrc_wrf)
  deallocate(vegfrc,vegfrc_max,vegfrc_min)
  deallocate(xlon,ylat)
  deallocate(vegfrc_obs)
 
  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

endif ! mype==0

call MPI_FINALIZE(ierror)
!

end program

subroutine interp_bili_2d(rlonmin_in,rlatmin,rdx,rdy,nx,ny,xlon,ylat,vegfrc,nxobs,nyobs,vegfrc_obs)
!
!  interpolation for GVF from VIIRS
!

  implicit none

  integer,intent(in) :: nx,ny,nxobs,nyobs
  real,intent(in) :: rlatmin,rlonmin_in
  real,intent(in) :: vegfrc_obs(nxobs,nyobs)
  real,intent(in) :: xlon(nx,ny)
  real,intent(in) :: ylat(nx,ny)
  real*8,intent(in)  :: rdx,rdy
  real,intent(out):: vegfrc(nx,ny)

!
  real    :: rlonmin
!
  integer :: i,j,ip,jp
  real    :: rlat,rlon,rip,rjp,dx,dy
!

  rlonmin=rlonmin_in-180.0_4
  do j=1,ny
     do i=1,nx
       rlon=xlon(i,j)
       rlat=ylat(i,j)
       rip=((rlon+180.0_4)-rlonmin)/rdx + 1.0_4
       rjp=(rlat-rlatmin)/rdy + 1.0_4
       ip=int(rip)
       jp=int(rjp)
       dx=rip-ip
       dy=rjp-jp
       if(ip >=1 .and. ip <=nxobs .and. &
          jp >=1 .and. jp <=nyobs) then
          vegfrc(i,j)=vegfrc_obs(ip,jp)* (1.0-dx)*(1.0-dy) + &
                      vegfrc_obs(min(ip+1,nxobs),jp)* dx*(1.0-dy) + &
                      vegfrc_obs(ip,min(jp+1,nyobs))* (1.0-dx)*dy + &
                      vegfrc_obs(min(nxobs,ip+1),min(nyobs,jp+1))*dx*dy
       else
          vegfrc(i,j)=0.0
       endif
     enddo
  enddo
  write(*,*) 'obs veg frc in wrf grid=',maxval(vegfrc),minval(vegfrc)
!
end subroutine interp_bili_2d

