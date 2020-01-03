
subroutine update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file, but read dh3 to replace the
!           part of dh1
!
!   prgmmr: Ming Hu                 date: 2019-10-12
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
  integer(i_kind), intent(in)      :: dh3
  character (len=31), intent(in)   :: rmse_var
  character(len=19),intent(in)  :: DateStr1
  character(len=19),intent(in)  :: DateStr2
  character(len=19),intent(in)  :: DateStr3
  logical,intent(in)  :: if_integer
  integer,intent(in)  :: mype
  
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
  integer(i_kind) nlon_regionalb,nlat_regionalb,nsig_regionalb
  real(r_single),allocatable::field3(:,:,:)
  integer,allocatable::ifield3(:,:,:)
  real(r_single),allocatable::field3b(:,:,:)
  integer,allocatable::ifield3b(:,:,:)
  
  integer(i_kind) :: k,i,j
  integer(i_kind) :: ierr
!
!  
!
  if(mype==0) then
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '
  endif

  wrf_real=104_i_kind
!-------------  get grid info
  
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  if(mype==0) then
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  endif
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  if(ndim1 == 2) nsig_regional=1
  if(mype==0) write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
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

!
!-------------  get grid info

  end_index=0
  call ext_ncd_get_var_info (dh3,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  if(mype==0) then
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh3  = ',dh3
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE
  endif
  nlon_regionalb=end_index(1)
  nlat_regionalb=end_index(2)
  nsig_regionalb=end_index(3)
  if(ndim1 == 2) nsig_regionalb=1
  if(mype==0)  write(6,*)' nlon,lat,sig_regional=',nlon_regionalb,nlat_regionalb,nsig_regionalb
  if( ndim1 == 2 .or. ndim1 == 3) then
      if(if_integer) then
         allocate(ifield3b(nlon_regionalb,nlat_regionalb,nsig_regionalb))
      else
         allocate(field3b(nlon_regionalb,nlat_regionalb,nsig_regionalb))
      endif
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  if(if_integer) then
    call ext_ncd_read_field(dh3,DateStr3,TRIM(rmse_var),              &
       ifield3b,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  else
    call ext_ncd_read_field(dh3,DateStr3,TRIM(rmse_var),              &
       field3b,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  endif

  if( (nlon_regional .ne. nlon_regionalb+1) .or.    &
      (nlat_regional .ne. nlat_regionalb+1) .or.    &
      (ndim1 == 3 .and. (nsig_regional .ne. nsig_regionalb)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      write(6,*)' end_index 1,2,3=',nlon_regionalb,nlat_regionalb,nsig_regionalb
      stop 123
  endif
!
  DO k=1,nsig_regional
    if(if_integer) then
      if(mype==0)  write(6,*)' max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
      if(mype==0)  write(6,*)'b max,min =',maxval(ifield3b(:,:,k)),minval(ifield3b(:,:,k))
      do j=1,nlat_regionalb
        do i=1,nlon_regionalb
          ifield3(i,j,k)=ifield3b(i,j,k)
        enddo
      enddo
    else
      if(mype==0)  write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
      if(mype==0)  write(6,*)'b max,min =',maxval(field3b(:,:,k)),minval(field3b(:,:,k))
      do j=1,nlat_regionalb
        do i=1,nlon_regionalb
          field3(i,j,k)=field3b(i,j,k)
        enddo
      enddo
    endif
  enddo
  if(trim(rmse_var)=='TSK' .or. trim(rmse_var)=='SOILT1' .or.   &
     trim(rmse_var)=='TSLB') then
     if(mype==0)  write(6,*) 'update_netcdf_mass: Safety check for variable ',trim(rmse_var)
     field3=min(400.0,max(200.0,field3))
     
     if(mype==0)  then
     DO k=1,nsig_regional
       if(if_integer) then
         write(6,*)'Again max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
       else
         write(6,*)'Again max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
       endif
     enddo
     endif
  endif
  
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

  if(mype==0) then
  write(6,*)' >>>>>>>>>>>  write out data to dh2  = ',dh2       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  endif
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
     deallocate(ifield3b)
  else
     deallocate(field3)
     deallocate(field3b)
  endif
  
end subroutine update_netcdf_mass

SUBROUTINE wrf_debug( level , str )
!  USE module_wrf_error
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

