subroutine update_netcdf_mass_snowT(dh1,dh2,DateStr1,DateStr2)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!  ------ Read SNOWC, TSK, SOILT1 and update TSK, SOILT1 on snow point
!
!   prgmmr: Ming Hu                 date: 2011-03-1
!
! program history log:
!
!   input argument list:
!      dh1 :    handle of file read in
!      dh2 :    handle of file write out
!      DateStr1 : time string of file read in 
!      DateStr2 : time string of file write out
!      rmse_var :  variable updated
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind, r_kind
  implicit none

!
  integer(i_kind), intent(in)      :: dh1
  integer(i_kind), intent(in)      :: dh2
  character(len=19),intent(in)  :: DateStr1
  character(len=19),intent(in)  :: DateStr2
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  character (len=31)   :: rmse_var
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)

  real(r_single),allocatable::snowc(:,:)
  real(r_single),allocatable::tsk(:,:)
  real(r_single),allocatable::soilt1(:,:)
  real(r_single),allocatable::tslb(:,:,:)
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Partially update variable '
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get snow cover
  
  rmse_var='SNOWC'
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(snowc(nlon_regional,nlat_regional))
  if( ndim1 == 2 ) then
      allocate(field3(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    write(6,*)' max,min snowc=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  snowc=field3(:,:,1)
  deallocate(field3)
  
!=========================================================

  rmse_var='TSK'
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(tsk(nlon_regional,nlat_regional))
  if( ndim1 == 2 ) then
      allocate(field3(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    write(6,*)' max,min TSK=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  tsk=field3(:,:,1)

  field3=0
  call ext_ncd_read_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,end_index(3)
    write(6,*)' max,min TSK in BK=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
! cycle snow grid point
  DO j=1,nlat_regional
  DO i=1,nlon_regional
     if(snowc(i,j) > 0.0001 ) then
       field3(i,j,1) = tsk(i,j)   
     endif
  ENDDO
  ENDDO
  deallocate(tsk)
  
!-------------  update
   
  end_index=0
  call ext_ncd_get_var_info (dh2,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2)) .or.    &
      (nsig_regional .ne. end_index(3)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      stop 123
  endif

  write(6,*)' >>>>>>>>>>>  write out data to dh2  = ',dh2       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  
  deallocate(field3)

!=========================================================
  rmse_var='SOILT1'
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(soilt1(nlon_regional,nlat_regional))
  if( ndim1 == 2 ) then
      allocate(field3(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    write(6,*)' max,min SOILT1=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  soilt1=field3(:,:,1)

  field3=0
  call ext_ncd_read_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  DO k=1,end_index(3)
    write(6,*)' max,min SOILT1 in BK=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
       
! cycle snow grid point
  DO j=1,nlat_regional
  DO i=1,nlon_regional
     if(snowc(i,j) > 0.0001 ) then
       field3(i,j,1) = soilt1(i,j)
     endif
  ENDDO
  ENDDO
  deallocate(soilt1)

!-------------  update

  end_index=0
  call ext_ncd_get_var_info (dh2,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2)) .or.    &
      (nsig_regional .ne. end_index(3)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      stop 123
  endif

  write(6,*)' >>>>>>>>>>>  write out data to dh2  = ',dh2
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  deallocate(field3)

end subroutine update_netcdf_mass_snowT
  
