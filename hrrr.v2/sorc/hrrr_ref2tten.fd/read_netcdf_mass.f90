subroutine read_netcdf_mass(dh1,DateStr1,nlon,nlat,nsig,   &
                            pt_ll,eta1_ll,aeta1_ll,zh,ps_bk,q_bk,t_bk) 
!$$$  documentation block
!                .      .    .                                       .
!   read_netcdf_mass: read variables from netcdf file and 
!
!   prgmmr: Ming Hu                 date: 2014-03-28
!
! program history log:
!
!   input argument list:
!      dh1 :    handle of file read in
!      nlon,nlat,nsig: 3 dimensions
!
!   output argument list:
!      DateStr1 : time string of file read in 
!      pt_ll,eta1_ll,aeta1_ll,zh,ps_bk,q_bk,t_bk
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind, r_kind
  use constants, only: h300
!  use constants, only: grav, h300
  implicit none

!
  integer(i_kind), intent(in)  :: dh1
  integer(i_kind),intent(in)   :: nlon,nlat,nsig
  character(len=19),intent(in)  :: DateStr1
!
  real(r_single),intent(out) :: t_bk(nlon,nlat,nsig)
  real(r_single),intent(out) :: q_bk(nlon,nlat,nsig)
  real(r_single),intent(out) :: ps_bk(nlon,nlat)
  real(r_single),intent(out) :: zh(nlon,nlat)
!
  real(r_kind),intent(out) :: pt_ll
  real(r_kind),intent(out) :: eta1_ll(nsig+1) 
  real(r_kind),intent(out) :: aeta1_ll(nsig)

  character (len=31)  :: rmse_var
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  
  integer(i_kind) :: k,i,j
  integer(i_kind) :: ierr
  real(r_single),allocatable:: field1(:),field3(:,:,:)
  real(r_single) pt_regional
  real(r_kind) grav
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' read_netcdf_mass '
  write(6,*) ' ================== '

  grav=9.81_r_kind
  wrf_real=104
  t_bk=0.0
  q_bk=0.0
  ps_bk=0.0
  zh=0.0
  eta1_ll=0.0
  aeta1_ll=0.0
  pt_ll=0.0

!-------------  get grid info
  
  rmse_var='P_TOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
  write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index,' end_index = ',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          pt_regional,WRF_REAL,0,0,0,ordering,          &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          ierr                                 )
  pt_ll=pt_regional/100.0
  write(6,*)' p_top=',pt_ll

  rmse_var='ZNU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
  write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index,' end_index = ',end_index
  allocate(field1(nsig))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          field1,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,                    &
          start_index,end_index,                   & !dom
          start_index,end_index,                   & !mem
          start_index,end_index,                   & !pat
          ierr                                 )
  aeta1_ll=field1
  deallocate(field1)
  do k=1,nsig
     write(6,*)' k,znu(k)=',k,aeta1_ll(k)
  end do

  rmse_var='ZNW'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
  write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index,' end_index = ',end_index
  allocate(field1(nsig+1))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          field1,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,                    &
          start_index,end_index,                   & !dom
          start_index,end_index,                   & !mem
          start_index,end_index,                   & !pat
          ierr                                 )
  eta1_ll=field1
  deallocate(field1)
  do k=1,nsig+1
     write(6,*)' k,znw(k)=',k,eta1_ll(k)
  end do

  rmse_var='MUB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
  write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index,' end_index = ',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          zh,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          ierr                                 )
  write(6,*)' max,min MUB=',maxval(zh),minval(zh)

  rmse_var='MU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1=',ndim1
  write(6,*)' WrfType = ',WrfType,' WRF_REAL=',WRF_REAL,'ierr  = ',ierr
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index,' end_index = ',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
          ps_bk,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index,               & !dom
          start_index,end_index,               & !mem
          start_index,end_index,               & !pat
          ierr                                 )
  write(6,*)' max,min MU=',maxval(ps_bk),minval(ps_bk)
  ps_bk=(zh+ps_bk+pt_regional)/100.0
  write(6,*)' max,min psfc0=',maxval(ps_bk),minval(ps_bk)

  rmse_var='PHB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index  =',end_index
  write(6,*)'ierr  = ',ierr   !DEDE

  allocate(field3(end_index(1),end_index(2),end_index(3)))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  if(ierr == 0 ) then
     zh=field3(:,:,1)/grav
     write(6,*)' max,min zh =',maxval(zh),minval(zh)
  else
     write(*,*) 'error in read from ',trim(rmse_var),' error=',ierr
  endif
  deallocate(field3)

  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index  =',end_index
  write(6,*)'ierr  = ',ierr   !DEDE

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       t_bk,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  if(ierr == 0 ) then
     t_bk=t_bk+h300
     DO k=1,end_index(3)
        write(6,*)' max,min T =',maxval(t_bk(:,:,k)),minval(t_bk(:,:,k))
     enddo
  else
     write(*,*) 'error in read from ',trim(rmse_var),' error=',ierr
  endif

  rmse_var='QVAPOR'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index  =',end_index
  write(6,*)'ierr  = ',ierr   !DEDE

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       q_bk,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  if(ierr == 0 ) then
     DO k=1,end_index(3)
        write(6,*)' max,min Q =',maxval(q_bk(:,:,k)),minval(q_bk(:,:,k))
     enddo
  else
     write(*,*) 'error in read from ',trim(rmse_var),' error=',ierr
  endif

end subroutine read_netcdf_mass

