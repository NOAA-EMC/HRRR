program initial_arw_ens_multidom
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    initial_arw_ens 
!   prgmmr: Hu         org: GSD                date: 2015-03-24
!
! abstract: read pertubations on ARW A grid and generate initial files for ARW
!            ensembles
!
!
! program history log:
!   2015-03-23  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  zeus
!
!$$$ end documentation block

  use mpimod, only: npe,mpi_comm_world,ierror,mype
  use mpeu_util, only: die
  use kinds, only: r_kind,i_kind,r_single
  use constants, only : half
!  use constants, only : max_varname_length
  implicit none
  INCLUDE 'netcdf.inc'

  integer(i_kind),parameter :: max_varname_length=32
  integer :: n_ens
  integer :: nlon,nlat,nsig,num_fields
  integer(i_kind) :: nc2d,nc3d
  character(len=max_varname_length),allocatable,dimension(:) :: cvars2d 
  character(len=max_varname_length),allocatable,dimension(:) :: cvars3d  

  real(r_single),allocatable,dimension(:,:,:)::en_perts
  real(r_kind),dimension(:,:),allocatable:: workh

  real(r_single),allocatable,dimension(:,:,:):: field3

! Declare netcdf parameters

  character(len=120) :: flnm1
  character(len=120) :: flnm_new
  character(len=19)  :: DateStr1
  integer(i_kind)    :: dh1
  integer(i_kind)    :: dh2

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var
!
! inflation factor
!
  integer, parameter :: infltlvl=100
  real(r_single) :: infltnfct_t(infltlvl),infltnfct_uv(infltlvl),infltnfct_q(infltlvl),infltnfct_ps
  real(r_single) :: infltnfct(100)
!
! Declare variables.
!
  logical :: ifinflt_column
  integer :: num_dom
  namelist/setup/ n_ens,ifinflt_column,num_dom
!
!
!
  integer(i_kind) i,j,k,n
  integer(i_kind) ic2,ic3
  character(255) filename
  integer(i_kind),dimension(4):: idate4
  integer(i_kind) im,i0

  integer(i_kind) :: iunit,iout
  integer :: ios
  integer :: domainid,mymemid
  character(len=80) :: myname_
  character(len=80) :: filestdout
  logical :: lexist
!
!
!  MPI
  call MPI_INIT(ierror)
  call mpi_comm_size(mpi_comm_world,npe,ierror)
  call mpi_comm_rank(mpi_comm_world,mype,ierror)
!
  ifinflt_column=.false.
  n_ens=0
  num_dom=1
!
!
  iout=13
  write(filestdout,'(a,I4.4)') 'stdout_initens_pe',mype+1
  open(iout,file=trim(filestdout))
  open(11,file='namelist.initens')
    read(11,setup,iostat=ios)
    if(ios/=0) call die(myname_,'read(setup)',ios)
  close(11)
  if(n_ens > 0) then
     write(iout,*) 'the ensemble member number==',n_ens
     write(iout,*) 'if turn on vertical inflation factor is ==',ifinflt_column
  else
     write(iout,*) 'wrong ensemble member number==',n_ens
     stop
  endif
!
  infltnfct=1.0_r_single
!
  if(ifinflt_column) then
    inquire(file='vertical_inflate_factor.txt',exist=lexist)
    if(lexist) then
       i=1
       open(10,file='vertical_inflate_factor.txt')
       read(10,*)
       do while(i <= infltlvl)
          read(10,'(I10,f10.4)',end=110) k,infltnfct(i)
          i=i+1
       enddo
110    continue
    close(10)
    endif
  endif
  infltnfct_t=infltnfct
  infltnfct_uv=infltnfct
  infltnfct_q=infltnfct
  infltnfct_ps=infltnfct(1)

!  check inflate factor
   write(iout,*) 'inflate factor'
   write(iout,'(4a10)') 'level','T', 'UV','q'
   do k=1, i
      write(iout,'(I10,3f10.4)') k,infltnfct_t(k),infltnfct_uv(k),infltnfct_q(k)
   enddo
   write(iout,'(a,f10.4)') 'surface pressure inflate factor=',infltnfct_ps
!
! figure out the begin and end of member for each core
!
  if(npe < num_dom*n_ens) then
     write(*,*) 'Error, must run this job with more cores!!!'
     write(*,*) 'The minimum core number is ',num_dom*n_ens
     call mpi_finalize(ierror)
     stop 123
  endif
  mymemid=mype+1
  if(mymemid <= num_dom*n_ens) then
     domainid=(mymemid-1)/n_ens +1   
     mymemid=mymemid-(domainid-1)*n_ens
  else
     domainid=0
     mymemid=0
  endif
  write(iout,*) 'domainid=',domainid,mymemid
!           open netcdf file to read
  call ext_ncd_ioinit(sysdepinfo,status)
!
  iunit=20
  if( domainid > 0 .and. mymemid > 0) then
     write(flnm1,'(a,I2.2)') 'wrfinput_d',domainid
     call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
     if ( Status /= 0 )then
        write(iout,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
             trim(flnm1),', Status = ', Status
        stop 74
     endif
!
!-------------  get date info  from file read in
!
     call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
     read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)')   &
                       iyear,imonth,iday,ihour,iminute,isecond
     write(iout,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                       ,iyear,imonth,iday,ihour,iminute,isecond

!   get dimensions
     write(filename,'(a,I2.2,a,I4.4)') './d',domainid,'/en_perts4arw.mem',mymemid
     write(iout,*) 'read dimension from ', trim(filename)
     open(iunit,file=trim(filename),form='unformatted')
        read(iunit) nc3d,nc2d
        write(iout,*) 'dimension is =',nc3d,nc2d
        allocate(cvars3d(nc3d),cvars2d(nc2d))
        rewind(iunit)
        read(iunit) nc3d,nc2d,cvars3d,cvars2d
        read(iunit) nlat,nlon,nsig

        write(iout,*) 'nlat,nlon,nsig=',nlat,nlon,nsig
        write(iout,'(I5,A10,10A6)') nc3d,'cvars3d=',(trim(cvars3d(ic3)),ic3=1,nc3d)
        write(iout,'(I5,A10,10A6)') nc2d,'cvars2d=',(trim(cvars2d(ic2)),ic2=1,nc2d)

        num_fields=nc3d*nsig+nc2d
        allocate(workh(nlat,nlon))
        allocate(en_perts(nlat,nlon,num_fields))
!
!  read perturbations
!
        write(iout,*) 
        write(iout,*) 'read perturbations for ', trim(filename)

        do k=1,num_fields

            read(iunit) workh
!            write(iout,*) k,maxval(workh),minval(workh)
            do j=1,nlon
               do i=1,nlat
                  en_perts(i,j,k)=workh(i,j)
               end do
            end do

        end do

        deallocate(workh)

     close(iunit)

     write(flnm_new,'(a,I2.2,a,I4.4)') 'wrfinput_d',domainid,'.mem',mymemid
     call ext_ncd_open_for_update( trim(flnm_new), 0, 0, "", dh2, Status)
     if ( Status /= 0 )then
        write(iout,*)'gen_initial_ensemble:  cannot open flnm = ',&
             trim(flnm_new),', Status = ', Status
        stop 74
     endif
     write(iout,*) 'generate intial file for =',trim(flnm_new)

     rmse_var='T' 
     allocate(field3(nlon,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,nsig,iout)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+2*nsig)*infltnfct_t(k)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,nsig,iout)
     deallocate(field3)

     rmse_var='U' 
     allocate(field3(nlon+1,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon+1,nlat,nsig,iout)
     do k=1,nsig
        do j=1,nlon+1
           do i=1,nlat
              im=max(1,j-1)
              i0=min(nlon,j)
              field3(j,i,k)=field3(j,i,k)+&
                            half*(en_perts(i,im,k)+en_perts(i,i0,k))*infltnfct_uv(k)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon+1,nlat,nsig,iout)
     deallocate(field3)

     rmse_var='V' 
     allocate(field3(nlon,nlat+1,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat+1,nsig,iout)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat+1
              im=max(1,i-1)
              i0=min(nlon,i)
              field3(j,i,k)=field3(j,i,k)+&
                            half*(en_perts(im,j,k+nsig)+en_perts(i0,j,k+nsig))*infltnfct_uv(k)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat+1,nsig,iout)
     deallocate(field3)

     rmse_var='QVAPOR' 
     allocate(field3(nlon,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,nsig,iout)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+3*nsig)*infltnfct_q(k)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,nsig,iout)
     deallocate(field3)

     rmse_var='MU' 
     allocate(field3(nlon,nlat,1))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,1,iout)
     do k=1,1
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+6*nsig)*infltnfct_ps
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,1,iout)
     deallocate(field3)

     call ext_ncd_ioclose(dh2, Status)
     call ext_ncd_ioclose(dh1, Status)

  endif ! if this core has a member to process
!

  call mpi_finalize(ierror) 

end program initial_arw_ens_multidom

SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  write(*,*) 'wrf_debug called !'
  RETURN
END SUBROUTINE wrf_debug

