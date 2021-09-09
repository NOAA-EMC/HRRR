program surface_consistent_check_netcdf_mass
!$$$  documentation block
!                .      .    .                                       .
!   Security check for consistency of all land surface parameters on water/ice

!   prgmmr: Ming Hu                 date: 2019-11-18
!
! program history log:
!
!
! attributes:
!   language: f90
!
!$$$

  use mpi
  use kinds, only: r_single,i_kind
  implicit none

  INCLUDE 'netcdf.inc'

! MPI variables
  integer :: npe, mype, mypeLocal,ierror
!
  integer, parameter :: WRF_INTEGER = 106
!
  integer :: nlon, nlat
!
! Declare local parameters

  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  integer(i_kind)            :: dh1
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering
  character (len=31) :: name,name1,name2,name3,name4,name5
  
  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo
  
  character (len=5) :: lutype
  integer(i_kind) :: l, n
  
  integer(i_kind) :: ierr, ier, Status, Status_next_time

! rmse stuff
  
  character (len=31) :: rmse_var
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  integer,allocatable::ifield2(:,:)
  real(r_single),allocatable::field2(:,:)

  real(r_single),allocatable::seaice(:,:)
  integer(i_kind) wrf_real

!  surface parameters
  real(r_single),allocatable::landmask(:,:)
  real(r_single),allocatable::xland_rr(:,:)
  real(r_single),allocatable::lu_index(:,:)
  integer,allocatable:: ivgtyp(:,:)
  integer,allocatable:: isltyp(:,:)
!
! surface parameters from geogrid
  integer :: ncid
  real,allocatable :: xlonRR(:,:)    !
  real,allocatable :: ylatRR(:,:)    !
  real,allocatable :: luseRR(:,:)   !
  real,allocatable :: xland(:,:)
!
  real(r_single)    :: xice_threshold
  integer(i_kind)   :: fractional_seaice
!
!
  integer :: iii,jjj,num,numh,i4,j4
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!
  if(mype==0) then

  fractional_seaice=1
  if ( fractional_seaice == 0 ) then
    xice_threshold = 0.5
    write(*,*) ' do not use fraction sea ice'
  else if ( fractional_seaice == 1 ) then
    xice_threshold = 0.02
    write(*,*) ' use fraction sea ice'
  endif
!  

  call GET_DIM_ATT_geo('./geo_em.d01.nc',nlon,nlat)
  write(*,*) 'grid dimension =',nlon,nlat
  allocate(xlonRR(nlon,nlat))
  allocate(ylatRR(nlon,nlat))
  allocate(xland(nlon,nlat))
  allocate(luseRR(nlon,nlat))
  call OPEN_geo('./geo_em.d01.nc', NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylatRR,xlonRR,xland,luseRR)
  call CLOSE_geo(NCID)
  deallocate(xlonRR,ylatRR,luseRR)
  write(*,*) 'xland from geogrid=',maxval(xland),minval(xland)

  wrf_real=104

!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

  call ext_ncd_ioinit(sysdepinfo,status)
  
  flnm1='wrf_inout'        ! for full cycle
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'CONVERT_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74 
  endif

!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' precipiation and snow data from background file at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

  rmse_var='T'

  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE

  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE

  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  if( (nlon .ne. end_index(1)) .or.    &
      (nlat .ne. end_index(2)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat=',nlon,nlat
      stop 123
  endif

  allocate(seaice(nlon_regional,nlat_regional))
  allocate(landmask(nlon_regional,nlat_regional))
  allocate(xland_rr(nlon_regional,nlat_regional))
  allocate(lu_index(nlon_regional,nlat_regional))
  allocate(ivgtyp(nlon_regional,nlat_regional))
  allocate(isltyp(nlon_regional,nlat_regional))

  allocate(field2(nlon_regional,nlat_regional))
  
!
  write(6,*) '================================================='
  rmse_var='SEAICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min SEAICE=',maxval(field2),minval(field2)
  seaice=field2

  write(6,*) '================================================='
  rmse_var='XLAND'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering       
  write(6,*)' start_index=',start_index     
  write(6,*)' end_index=',end_index         
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min XLAND=',maxval(field2),minval(field2)
  xland_rr=field2
  write(6,*) '================================================='
  rmse_var='LANDMASK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering  
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index         
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min LANDMASK=',maxval(field2),minval(field2)
  landmask=field2
  write(6,*) '================================================='
  rmse_var='LU_INDEX'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering  
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index         
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min LU_INDEX=',maxval(field2),minval(field2)
  lu_index=field2
  write(6,*) '================================================='
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*) ierr
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_INTEGER =',WrfType, WRF_INTEGER
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering  
  write(6,*)' start_index=',start_index
  end_index(3) =1
  write(6,*)' end_index=',end_index         
  allocate(ifield2(nlon_regional,nlat_regional))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WRF_INTEGER,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)
  ivgtyp=ifield2
  write(6,*) '================================================='
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_INTEGER=',WrfType, WRF_INTEGER
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WRF_INTEGER,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
  isltyp=ifield2
  deallocate(ifield2)
  write(6,*) '================================================='

  call ext_ncd_ioclose(dh1, Status)
!
! save the RR background snow in snowRRbk
!
!!! Security check for consistency of all land surface parameters on water/ice:
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      if(seaice(i,j) < xice_threshold) then
!water
! for MODIS water category is 17
            ivgtyp(i,j)=17
            lu_index(i,j)=17
            landmask(i,j)=0.
            xland_rr(i,j)=2.
            isltyp(i,j)=14 ! STASGO water
      else
!ice
!for MODIS ice category is 15
             ivgtyp(i,j)=15
             lu_index(i,j)=15
             landmask(i,j)=1.
             xland_rr(i,j)=1.
             isltyp(i,j)=16 ! STASGO ice
      endif
    endif  ! water
  ENDDO
  ENDDO
!
!           update mass core netcdf file with snow,snowh,snowc
!
  write(6,*) ' ================== '
  write(6,*) ' check for consistency of all land surface parameters '
  write(6,*) ' ================== '
  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
    write(6,*)'UPDATE_NETCDF_MASS:  problem with flnm1 = ',&
         trim(flnm1),', Status = ', Status
    stop 75
  endif
     
!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*) ' Update sruface parameters in background at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index, WrfType, ierr    )
  if( (nlon .ne. end_index(1)) .or.    &
      (nlat .ne. end_index(2)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat=',nlon,nlat
      stop 123
  endif
   
  write(6,*) '================================================='
  field2=landmask
  write(6,*)' max,min landmask=',maxval(field2),minval(field2)
  rmse_var='LANDMASK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=xland_rr
  write(6,*)' max,min xland=',maxval(field2),minval(field2)
  rmse_var='XLAND'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=lu_index
  write(6,*)' max,min lu_index=',maxval(field2),minval(field2)
  rmse_var='LU_INDEX'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  allocate(ifield2(nlon_regional,nlat_regional))
  ifield2=isltyp
  write(6,*)' max,min isltyp=',maxval(ifield2),minval(ifield2)
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  ifield2=ivgtyp
  write(6,*)' max,min ivgtyp=',maxval(ifield2),minval(ifield2)
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  deallocate(ifield2)
  deallocate(field2)
  call ext_ncd_ioclose(dh1, Status)
  deallocate(seaice)
  deallocate(landmask)
  deallocate(xland_rr)
  deallocate(lu_index)
  deallocate(ivgtyp)
  deallocate(isltyp)
  deallocate(xland)
  
  write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

  endif ! mype==0

  call MPI_FINALIZE(ierror)

end program surface_consistent_check_netcdf_mass

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

