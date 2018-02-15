subroutine update_netcdf_mass_TSK(dh1,dh2,DateStr1,DateStr2,rmse_var)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!           For TSK, we need to sync SST from GFS into it.
!
!   prgmmr: Ming Hu                 date: 2010-05-16
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
  character (len=31), intent(in)   :: rmse_var
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
  character (len=31)   :: rmse_var_local
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::field2(:,:)
  real(r_single),allocatable::tsk_1h(:,:),xland(:,:)
  
  integer(i_kind) :: k,i,j
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  
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
    write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  allocate(tsk_1h(nlon_regional,nlat_regional))
  tsk_1h=field3(:,:,1)
  
!
! sync TSK on land and seaice only
!
  allocate(field2(nlon_regional,nlat_regional))
  allocate(xland(nlon_regional,nlat_regional))
  write(6,*) '================================================='
  rmse_var_local='XLAND'
  call ext_ncd_get_var_info (dh1,trim(rmse_var_local),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var_local)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var_local),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min XLAND=',maxval(field2),minval(field2)
  xland=field2
  deallocate(field2)

  write(6,*) '================================================='
  rmse_var_local='TSK'
  call ext_ncd_get_var_info (dh2,trim(rmse_var_local),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var_local)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL 
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh2,DateStr1,TRIM(rmse_var_local),              &
       field3,WRF_REAL,0,0,0,ordering,           &              
       staggering, dimnames ,               &        
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 ) 
  write(6,*)' max,min TSK in GFS =',maxval(field3(:,:,1)),minval(field3(:,:,1))
!
!
  DO J=1,nlat_regional
  DO I=1,nlon_regional
    if( int(xland(i,j)+0.01) == 1 ) then    ! land and seaice
            field3(i,j,1)=tsk_1h(i,j)
    endif
  ENDDO
  ENDDO
  deallocate(tsk_1h)
  deallocate(xland)
!-------------  get grid info
   
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
  
end subroutine update_netcdf_mass_TSK

subroutine update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var,if_integer)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!   prgmmr: Ming Hu                 date: 2009-01-16
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
  character (len=31), intent(in)   :: rmse_var
  character(len=19),intent(in)  :: DateStr1
  character(len=19),intent(in)  :: DateStr2
  logical,intent(in)  :: if_integer
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)
  integer,allocatable::ifield3(:,:,:)
  
  integer(i_kind) :: k
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  
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
  if(ndim1 == 2) nsig_regional=1
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  if( ndim1 == 2 .or. ndim1 == 3) then
      if(if_integer) then
         allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
      else
         allocate(field3(nlon_regional,nlat_regional,nsig_regional))
      endif
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  if(if_integer) then
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield3,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  else
    call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  endif

  DO k=1,nsig_regional
    if(if_integer) then
      write(6,*)' max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
    else
      write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
    endif
  enddo
  
!-------------  get grid info
   
  end_index=0
  call ext_ncd_get_var_info (dh2,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2)) .or.    &
      (ndim1 == 3 .and. (nsig_regional .ne. end_index(3)))) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      write(6,*)' end_index 1,2,3=',end_index(1),end_index(2),end_index(3)
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
  if(if_integer) then
    call ext_ncd_write_field(dh2,DateStr2,TRIM(rmse_var),              &
       ifield3,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  else
    call ext_ncd_write_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  endif
  
  if(if_integer) then
     deallocate(ifield3)
  else
     deallocate(field3)
  endif
  
end subroutine update_netcdf_mass

SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
!  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
!      CALL wrf_message( str )
  ENDIF
  write(*,*) 'wrf_debug called !'
  RETURN
END SUBROUTINE wrf_debug

subroutine update_netcdf_mass_partial(dh1,dh2,DateStr1,DateStr2)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!  ------ partial update TSK, SOILT1, TSLB
!
!   prgmmr: Ming Hu                 date: 2009-01-16
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
     if(snowc(i,j) > 0.9 ) then
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
     if(snowc(i,j) > 0.9 ) then
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

!=========================================================
  rmse_var='TSLB'
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
  allocate(tslb(nlon_regional,nlat_regional,nsig_regional))
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
    write(6,*)' max,min TSLB=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  tslb=field3

  field3=0
  call ext_ncd_read_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,end_index(3)
    write(6,*)' max,min TSLB in BK=',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
! cycle snow grid point
  DO k=3,nsig_regional
     field3(:,:,k)=tslb(:,:,k)
  ENDDO
  DO j=1,nlat_regional
  DO i=1,nlon_regional
     if(snowc(i,j) > 0.9 ) then
       field3(i,j,1) = tslb(i,j,1)   
       field3(i,j,2) = tslb(i,j,2)   
     endif
  ENDDO
  ENDDO
  deallocate(tslb)
  
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
  
end subroutine update_netcdf_mass_partial
  
subroutine update_netcdf_mass_sfc629(dh1,dh2,DateStr1,DateStr2,rmse_var,if_integer)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!   prgmmr: Ming Hu                 date: 2009-01-16
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
  character (len=31), intent(in)   :: rmse_var
  character(len=19),intent(in)  :: DateStr1
  character(len=19),intent(in)  :: DateStr2
  logical,intent(in)  :: if_integer
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::field3_9(:,:,:)
  integer, parameter :: nsoilold=6,nsoilnew=9
  real(r_single) :: zsold(nsoilold),zsnew(nsoilnew)
  real(r_single) :: wk,wkp1
  
  integer(i_kind) :: k,k9,k6,nsig_regional9
  integer(i_kind) :: ierr
  logical :: ifsamelevel
!
  zsold = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
  zsnew = (/ 0.00 , 0.01 , 0.04 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  
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
      write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  
!-------------  get grid info
   
  end_index=0
  call ext_ncd_get_var_info (dh2,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2))) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat=',nlon_regional,nlat_regional
      stop 123
  endif
  nsig_regional9=end_index(3)
  if( ndim1 == 2 ) then
         allocate(field3_9(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
         allocate(field3_9(nlon_regional,nlat_regional,nsig_regional9))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif
  if( (nsig_regional .ne. nsig_regional9) ) then
      write(6,*) ' swith Dimensions: ',nsig_regional,nsig_regional9
      do k9=1,nsoilnew
         ifsamelevel=.false.
         do k6=1,nsoilold
            if(abs(zsold(k6)-zsnew(k9)) < 1.0e-5) then
                field3_9(:,:,k9)=field3(:,:,k6)
                write(*,*) 'assign old ',k6,zsold(k6), 'to new ', k9,zsnew(k9)
                ifsamelevel=.true.
            endif
         enddo
         if(ifsamelevel) then
            cycle
         else
            do k6=1,nsoilold-1
               if((zsold(k6)<zsnew(k9)) .and. (zsold(k6+1) > zsnew(k9)) ) k=k6
            enddo
            wkp1=(zsnew(k9)-zsold(k))/(zsold(k+1)-zsold(k))
            wk=1.0-wkp1
            field3_9(:,:,k9)=field3(:,:,k)*wk + field3(:,:,k+1)*wkp1 
            write(*,*) k9, 'is interpolated between',k,' and ',k+1, 'with ', &
                     wk, wkp1
         endif
      enddo
  else
      write(6,*) ' Keep same vertical Dimensions: ',nsig_regional,nsig_regional9
      field3_9=field3
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
       field3_9,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  
     deallocate(field3)
  
end subroutine update_netcdf_mass_sfc629
