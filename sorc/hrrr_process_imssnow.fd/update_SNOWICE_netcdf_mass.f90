subroutine update_SNOWICE_netcdf_mass(snowiceRR, xland, luse, nlon, nlat,xlandIMS)
!$$$  documentation block
!                .      .    .                                       .
!   update_SNOWICE_netcdf_mass: read SNOW from wrf mass netcdf old background file
!           and update SST in wrf mass background file
!   prgmmr: Ming Hu                 date: 2008-02-25
!
! program history log:
!
! 2009-07-27: make consistent of all land surface parameters and using fraction seac ice
! if ( fractional_seaice == 0 ) then
!    xice_threshold = 0.5
! else if ( fractional_seaice == 1 ) then
!    xice_threshold = 0.02
! endif
!
!
! For sea-ice:
!                     vegcat(i,j)=24     not used
!                     ivgtyp(i,j)=24     int IVGTYP / =15 for MODIS
!                     lu_index(i,j)=24   float LU_INDEX / =15 for MODIS
!                     landmask(i,j)=1.   float LANDMASK
!                     xland_rr(i,j)=1.      float XLAND
!                     isltyp(i,j)=16.    int ISLTYP
! 
! For water:
!                     vegcat(i,j)=16   / =17 and 21 (inland) for MODIS
!                     ivgtyp(i,j)=16  / =17 and 21 for MODIS
!                     lu_index(i,j)=16  / =17 and 21 for MODIS
!                     landmask(i,j)=0.
!                     xland_rr(i,j)=2.
!                     isltyp(i,j)=14.
! 

!
!   input argument list:
!       snowRR: snow  cover
!       iceRR:  seaice cover
!       xland:    land and sea mask
!       luse:    land use category
!       nlon:  x dimension
!       nlat:  Y dimension
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind
  implicit none

  INCLUDE 'netcdf.inc'

  integer, parameter :: WRF_INTEGER = 106
!
  integer :: nlon, nlat
  real  :: snowiceRR(nlon,nlat)
  real  :: xland(nlon,nlat)
  real  :: luse(nlon,nlat)
  real  :: xlandIMS(nlon,nlat)

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
  integer(i_kind) nsig_soil_regional
  integer,allocatable::ifield2(:,:)
  real(r_single),allocatable::field2(:,:)
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::precip(:,:)
  real(r_single),allocatable::surftemp(:,:)
  real(r_single),allocatable::tskin(:,:)
  real(r_single),allocatable::tsnow(:,:)
  real(r_single),allocatable::landmask_soilmoisture1(:,:)
  real(r_single),allocatable::soilmoisture(:,:,:)
  real(r_single),allocatable::soiltemp(:,:,:)

  real(r_single),allocatable::snow(:,:)
  real(r_single),allocatable::snowh(:,:)
  real(r_single),allocatable::snowc(:,:)
  real(r_single),allocatable::seaice(:,:)
  integer(i_kind) wrf_real

  real(r_single),allocatable::snowRRbk(:,:)
  real(r_single),allocatable::snowhRRbk(:,:)
  real(r_single),allocatable::snowcRRbk(:,:)
  real(r_single),allocatable::tskinRRbk(:,:)
  real(r_single),allocatable::tsnowRRbk(:,:)
  real(r_single),allocatable::soiltempRRbk(:,:,:)
  real(r_single),allocatable::surftempRRbk(:,:)
!  surface parameters
!                     ivgtyp(i,j)=24     int IVGTYP
!                     isltyp(i,j)=16.    int ISLTYP
  real(r_single),allocatable::landmask(:,:)
  real(r_single),allocatable::xland_rr(:,:)
  real(r_single),allocatable::lu_index(:,:)
!  integer(i_kind),allocatable:: ivgtyp(:,:)
!  integer(i_kind),allocatable:: isltyp(:,:)
  integer,allocatable:: ivgtyp(:,:)
  integer,allocatable:: isltyp(:,:)
!
!
  real(r_single)    :: xice_threshold
  integer(i_kind)   :: fractional_seaice
!
  real(r_single)    :: time, time1, time2
  real(r_single)    :: a, b
  integer(i_kind), dimension(4)  :: start_index1,  end_index1

  real(r_single)    :: R, Cp, RCP, P0
  integer(i_kind)   :: num_seaice2water, num_water2seaice
  integer(i_kind)   :: numtrimsnow, numbuildsnow, numusetrim
  integer   :: MSLANDID
!
  real snowtr,snowhtr,snowctr,snowav,snowhav,snowcav,tskinav,tsnowav,surftempav,soilt1av,soilt2av,soilt3av
  real snowsum,snowhsum,snowcsum,tskinsum,tsnowsum,surftempsum,soilt1sum,soilt2sum,soilt3sum
  real rhosn, snowtrimsum, snowbuiltsum
  integer nsoil,ii,jj,itr,jtr,ist,iend,jst,jend,numnb, numbuildmin
!
  integer :: iii,jjj,num,numh,i4,j4
  real    :: newvalue, newvalueh

  R = 287.06  ! gas constant JKg-1K-1
  Cp= 1003.5  ! Specific heat capacity  JKg-1K-1
  RCP = R/Cp
  P0=100000.0 ! Pa
!
  fractional_seaice=1
  if ( fractional_seaice == 0 ) then
    xice_threshold = 0.5
    write(*,*) ' do not use fraction sea ice'
  else if ( fractional_seaice == 1 ) then
    xice_threshold = 0.02
    write(*,*) ' use fraction sea ice'
  endif
!  
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

!-------------  get grid info
  rmse_var='SMOIS'

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
  nsig_soil_regional=end_index(3)


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
  allocate(snow(nlon_regional,nlat_regional))
  allocate(snowh(nlon_regional,nlat_regional))
  allocate(snowc(nlon_regional,nlat_regional))
  allocate(seaice(nlon_regional,nlat_regional))
  allocate(surftemp(nlon_regional,nlat_regional))
  allocate(tskin(nlon_regional,nlat_regional))
  allocate(tsnow(nlon_regional,nlat_regional))
  allocate(landmask_soilmoisture1(nlon_regional,nlat_regional))

  allocate(snowRRbk(nlon_regional,nlat_regional))
  allocate(snowhRRbk(nlon_regional,nlat_regional))
  allocate(snowcRRbk(nlon_regional,nlat_regional))
  allocate(tskinRRbk(nlon_regional,nlat_regional))
  allocate(tsnowRRbk(nlon_regional,nlat_regional))
  allocate(surftempRRbk(nlon_regional,nlat_regional))

!  allocate(soilmoisture(nlon_regional,nlat_regional,nsig_soil_regional))
!  allocate(soiltemp(nlon_regional,nlat_regional,nsig_soil_regional))

  allocate(landmask(nlon_regional,nlat_regional))
  allocate(xland_rr(nlon_regional,nlat_regional))
  allocate(lu_index(nlon_regional,nlat_regional))
  allocate(ivgtyp(nlon_regional,nlat_regional))
  allocate(isltyp(nlon_regional,nlat_regional))

  allocate(field2(nlon_regional,nlat_regional))
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  allocate(precip(nlon_regional,nlat_regional))
  precip=0.0
  
if(1==1) then   ! use 1st level atmosphere temperature
  write(6,*) '================================================='
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
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min temp=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
  surftemp=field3(:,:,1) + 300.0
  write(6,*)' max,min temp=',maxval(surftemp),minval(surftemp)

  write(6,*) '================================================='
  rmse_var='P'
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
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min P =',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
  precip=field3(:,:,1)

  write(6,*) '================================================='
  rmse_var='PB'
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
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min PB=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
  precip=precip+field3(:,:,1)
  surftemp=surftemp*(precip/P0)**RCP
  write(6,*)' max,min surface temp (K)=',maxval(surftemp),minval(surftemp)
endif
!
  write(6,*) '================================================='
  rmse_var='TSK'
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
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  tskin=field2(:,:)
  write(6,*)' max,min skin temp (K)=',maxval(tskin),minval(tskin)
!
  write(6,*) '================================================='
  rmse_var='SOILT1'
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
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  tsnow=field2(:,:)
  write(6,*)' max,min snow temp (K)=',maxval(tsnow),minval(tsnow)
!
  write(6,*) '================================================='
  precip=0
  rmse_var='QRAIN'
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
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min QRAIN=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
!tgs 10 mar 2013 - remove liquid precip for snow trimming to work correctly
!  precip=field3(:,:,1)
  write(6,*) '================================================='
  rmse_var='QSNOW'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' ordering=',ordering 
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min QSNOW=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
  precip=precip+field3(:,:,1)
  write(6,*) '================================================='
  rmse_var='QGRAUP'
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
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  DO k=1,nsig_regional
    write(6,*)' max,min QGRAUP=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  ENDDO
  precip=precip+field3(:,:,1)
  write(6,*)' max,min precip=',maxval(precip(:,:)),minval(precip(:,:))

  write(6,*) '================================================='
  rmse_var='SNOW'
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
  write(6,*)' max,min SNOW=',maxval(field2),minval(field2)
  snow=field2
  write(6,*) '================================================='
  rmse_var='SNOWH'
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
  write(6,*)' max,min SNOWH=',maxval(field2),minval(field2)
  snowh=field2
  write(6,*) '================================================='
  rmse_var='SNOWC'
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
  write(6,*)' max,min SNOWC=',maxval(field2),minval(field2)
  snowc=field2
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
  allocate(soilmoisture(nlon_regional,nlat_regional,nsig_soil_regional))
  rmse_var='SMOIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1 
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index    
  deallocate(field3)
  nsig_regional=end_index(3)
  nsoil=nsig_regional
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
    write(6,*)' max,min SMOIS=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  landmask_soilmoisture1=field3(:,:,1)  ! use soil mositure to find water =1 water
  do k=1,nsig_regional
  soilmoisture(:,:,k)=field3(:,:,k)
  enddo
!
  write(6,*) '================================================='
  allocate(soiltemp(nlon_regional,nlat_regional,nsig_soil_regional))
  allocate(soiltempRRbk(nlon_regional,nlat_regional,nsig_soil_regional))
  rmse_var='TSLB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  deallocate(field3)
  nsig_regional=end_index(3)
  nsoil=nsig_regional
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
    write(6,*)' max,min TSLB=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  do k=1,nsig_regional
  soiltemp(:,:,k)=field3(:,:,k)
  enddo
!
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
  snowRRbk=snow
  snowhRRbk=snowh
  snowcRRbk=snowc
  tskinRRbk=tskin
  tsnowRRbk=tsnow
  soiltempRRbk=soiltemp
  surftempRRbk=surftemp
!
!  trim snow
!

  snowtrimsum=0.
  snowbuiltsum=0.

  numtrimsnow=0
  numbuildsnow=0
  numusetrim=0
  numbuildmin=0
  DO J=1,nlat
  DO I=1,nlon
!  if(i.eq.427.and.j.eq.204) print*,'i,j,surftemp(i,j),precip(i,j)',i,j,surftemp(i,j),precip(i,j)
! xland is the RR land/water mask from the geo* file - no effect from sea ice, =1 for land, 0 - water.
    if(int(xland(i,j)+0.01) == 1 .and. int(seaice(i,j)+0.01) == 0  ) then  ! on land
      if(snowiceRR(i,j) < 1.0e-12 .and. snow(i,j) > 0.0 ) then   ! over forecast snow ?
!tgs may be increase 274K to 276K? Sometimes 100-200mm of snow trimmed with 274.
!tgs 13 April 2012 - change 276K to 280K
!tgs 10 March 2013 - not enough snow trimming in KS - turn temp threshold back
!                    to 276 K. 
!      if(precip(i,j) < 1.0e-12 .and. surftemp(i,j) > 280.0 ) then   ! make sure 
!      if(precip(i,j) < 1.0e-12 .and. surftemp(i,j) > 276.0 ) then   ! make sure 
        if(precip(i,j) < 1.0e-12) then   ! make sure 
          write(6,*) 'trim snow',i,j,snow(i,j),precip(i,j),surftemp(i,j),snowiceRR(i,j) 
          numtrimsnow=numtrimsnow+1
          itr=i
          jtr=j

! save values of snow to be trimmed
          snowtr=snow(i,j)
          snowhtr=snowh(i,j)
          snowctr=snowc(i,j)
          snowtrimsum=snowtrimsum+snow(i,j)
! trim snow
          snow(i,j) = 0.0
          snowh(i,j) = 0.0
          snowc(i,j) = 0.0
        endif
      endif

!tgs snow building
      if(snowiceRR(i,j) > 1.0e-12 .and. snow(i,j) == 0.0 ) then   !  underforecasted snow
        if(surftemp(i,j) < 278.0 ) then   
           write(6,*) 'build snow at i,j',i,j,'precip,surftemp,snowiceRR',precip(i,j),surftemp(i,j),snowiceRR(i,j)

           snowsum = 0.
           snowhsum = 0.
           snowcsum = 0.
           tskinsum = 0.
           tsnowsum = 0.
           soilt1sum = 0.
           soilt2sum = 0.
           soilt3sum = 0.
           surftempsum = 0.

           numnb=0
           ist=max(1,i-2)
           iend=min(nlon,i+2)
           jst=max(1,j-2)
           jend=min(nlat,j+2)
           do ii=ist,iend
           do jj=jst,jend
             if(int(xland(ii,jj)+0.01) == 1) then  ! land
               if(ii.eq.itr.and.jj.eq.jtr) then 
! snow trimmed at the neighbor point
                 numnb=100
               endif
               if( numnb== 100) exit

               if(snowRRbk(ii,jj) > 1.) then
                 numnb = numnb + 1
                 snowsum = snowsum + snowRRbk(ii,jj)
                 snowhsum = snowhsum + snowhRRbk(ii,jj)
                 snowcsum = snowcsum + snowcRRbk(ii,jj) 
                 tskinsum = tskinsum + tskinRRbk(ii,jj)
                 tsnowsum = tsnowsum + tsnowRRbk(ii,jj)
                 soilt1sum = soilt1sum + soiltempRRbk(ii,jj,1)
                 soilt2sum = soilt2sum + soiltempRRbk(ii,jj,2)
                 soilt3sum = soilt3sum + soiltempRRbk(ii,jj,3)
                 surftempsum = surftempsum + surftempRRbk(ii,jj)
               endif
             endif
           enddo
             if( numnb == 100) exit
           enddo

! compute averages for all neighbor land points
           if( (numnb.ge.1) .and. (numnb .ne. 100)) then
             snowav=snowsum/numnb
             snowhav=snowhsum/numnb
             snowcav=snowcsum/numnb
             tskinav=tskinsum/numnb
             tsnowav=tsnowsum/numnb
             soilt1av=soilt1sum/numnb
             soilt2av=soilt2sum/numnb
             soilt3av=soilt3sum/numnb
             surftempav=surftempsum/numnb
             print *,'snow neighbors found, numnb =',numnb, &
               'snowsum,snowav,snowhav,snowcav,tskinav,tsnowav,soilt1av,soilt2av,soilt3av,surftempav', &
                snowsum,snowav,snowhav,snowcav,tskinav,tsnowav,soilt1av,soilt2av,soilt3av,surftempav
           endif

           numbuildsnow=numbuildsnow+1
           if(numnb == 100) then ! use point with trimmed snow
             numusetrim=numusetrim+1
             print *,'trimmed snow at itr,jtr',itr,jtr,'is used to build snow at point i,j',i,j
             print *,'snowtr, snowhtr, snowctr, tskin(itr,jtr), tsnow(itr,jtr)', &
                  snowtr, snowhtr, snowctr,tskin(itr,jtr),tsnow(itr,jtr)
             if(snowhtr > 1.e-12) then
!                rhosn=max(76.9,min(500.,snowtr/snowhtr))
!tgs 26jun18 - consistency with the changed limits of snow density in RUC LSM.
! bug fix 12mar2019                rhosn=max(58.8,min(500.,snowav/snowhav))
               rhosn=max(58.8,min(500.,snowtr/snowhtr))
               snow(i,j) = max(1.,snowtr) ! not less than 1 mm SWE
               snowh(i,j) = snow(i,j)/rhosn
               snowc(i,j) = min(1.,snow(i,j)/32.)
               tskin(i,j) = tskin(itr,jtr)
               tsnow(i,j) = min(tsnow(itr,jtr),272.)
               soiltemp(i,j,1) = min(soiltemp(itr,jtr,1),272.)
               soiltemp(i,j,2) = min(soiltemp(itr,jtr,2),272.5)
               soiltemp(i,j,3) = min(soiltemp(itr,jtr,3),273.)
             else
!tgs 22apr15 - this warning is OK if the GFS background snow is getting trimmed (cold-start).
! This warning in the cycled RAP and HRRR indicates a problem.
               print *,'WARNING in snow build from the neighbor-point trimmed snow '
               print *,'Set snow to min value,j,snowhtr',i,j,snowhtr
               numbuildmin=numbuildmin+1
               snow(i,j) = 1.0
               snowh(i,j) = 1.0/250. ! rhosn=250.,snowh[m]=snow[mm]/rhosn
               snowc(i,j) = min(1.,snow(i,j)/32.) ! snowc=1 if snow=32mm 
               tskin(i,j) = min(tskin(i,j),272.)
               tsnow(i,j) = min(tsnow(i,j),272.)
               soiltemp(i,j,1) = min(soiltemp(i,j,1),272.)
               soiltemp(i,j,2) = min(soiltemp(i,j,2),272.5)
               soiltemp(i,j,3) = min(soiltemp(i,j,3),273.)
             endif
           else

             if(numnb.ge.1) then
               if(snowhav > 1.e-12 .and. snowav > 1.e-12) then
                 print *,'build snow based on neighbor points ',numnb
!                  rhosn=max(76.9,min(500.,snowav/snowhav))
!tgs 26jun18 - consistency with the changed limits of snow density in RUC LSM.
                 rhosn=max(58.8,min(500.,snowav/snowhav))
                 snow(i,j) = max(1.,snowav)
                 snowh(i,j) = snow(i,j)/rhosn
                 snowc(i,j) = min(1.,snow(i,j)/32.)
                 tskin(i,j) = min(min(tskinav,tskin(i,j)),272.)
                 tsnow(i,j) = min(min(tsnowav,tsnow(i,j)),272.)
                 soiltemp(i,j,1) = min(min(soilt1av,soiltemp(i,j,1)),272.)
                 soiltemp(i,j,2) = min(min(soilt2av,soiltemp(i,j,2)),272.5)
                 soiltemp(i,j,3) = min(min(soilt3av,soiltemp(i,j,3)),273.)
               else
!tgs 22apr15 - this warning is OK if the GFS background snow is getting trimmed (cold-start).
! This warning in the cycled RAP and HRRR indicates a problem.
                 print *,' WARNING in snow build from the neighbors average '
                 print *,'Set snow to min value - i,j,snowhav,rhosn',i,j,snowhav,rhosn
                 numbuildmin=numbuildmin+1
                 snow(i,j) = 1.0
                 snowh(i,j) = 1.0/250. ! rhosn=250.,snowh[m]=snow[mm]/rhosn
                 snowc(i,j) = min(1.,snow(i,j)/32.) ! snowc=1 if snow=32mm 
                 tskin(i,j) = min(tskin(i,j),272.)
                 tsnow(i,j) = min(tsnow(i,j),272.)
                 soiltemp(i,j,1) = min(soiltemp(i,j,1),272.)
                 soiltemp(i,j,2) = min(soiltemp(i,j,2),272.5)
                 soiltemp(i,j,3) = min(soiltemp(i,j,3),273.)
               endif
             else
               print *,'set snow to min value'
               numbuildmin=numbuildmin+1
               snow(i,j) = 1.0  
               snowh(i,j) = 1.0/250. ! rhosn=250.,snowh[m]=snow[mm]/rhosn
               snowc(i,j) = min(1.,snow(i,j)/32.) ! snowc=1 if snow=32mm 
               tskin(i,j) = min(tskin(i,j),272.)
               tsnow(i,j) = min(tsnow(i,j),272.)
               soiltemp(i,j,1) = min(soiltemp(i,j,1),272.)
               soiltemp(i,j,2) = min(soiltemp(i,j,2),272.5)
               soiltemp(i,j,3) = min(soiltemp(i,j,3),273.)
             endif
           endif  !  if(numnb == 100) then
           snowbuiltsum=snowbuiltsum+snow(i,j)
           print *,'BUILD - snow,snowh,snowc,tskin,tsnow,soiltemp1,soiltemp2,soiltemp3', &
              i,j,snow(i,j),snowh(i,j),snowc(i,j),tskin(i,j),tsnow(i,j),soiltemp(i,j,1),soiltemp(i,j,2),soiltemp(i,j,3)       
        endif
      endif
    endif

! limit snow depth not to exceed 50 m
    if((snowh(i,j) >= 0. .and. snowh(i,j) <=50.0) .and. (snow(i,j)  <=20000. .and. snow(i,j)  >=0.) ) then
    elseif(snowh(i,j) < 0. .or. snow(i,j)  < 0.) then
      snowh(i,j)=0.
      snow(i,j) = 0.
    elseif(snowh(i,j) > 50. .or. snow(i,j)  > 20000.) then
      print *,'Huge snow value i,j,snowh(i,j),snow(i,j)',i,j,snowh(i,j),snow(i,j)
      newvalue=0.0
      newvalueh=0.0
      num=0
      numh=0
      do jjj=j-1,j+1
        do iii=i-1,i+1
          write(*,*) iii,jjj,snowh(iii,jjj),snow(iii,jjj)
          if(iii .ne. i .and. jjj .ne. j) then
            i4=min(max(iii,1),nlon)
            j4=min(max(jjj,1),nlat)
            newvalue=newvalue+snow(i4,j4)
            newvalueh=newvalueh+snowh(i4,j4)
            num=num+1
          endif
        enddo
      enddo
      if(num > 0 .and. newvalue < 100000.0 .and. newvalueh < 200.0) then
        snow(i,j)=newvalue/num
        snowh(i,j)=newvalueh/num
      else
        snow(i,j)=snow(i-1,j-1)
        snowh(i,j)=snowh(i-1,j-1)
      endif

      print *,'Corrected snow value i,j,snowh(i,j),snow(i,j)',i,j,snowh(i,j),snow(i,j)
    else
      print *,'===>Error<===: strange point i,j,snowh(i,j),snow(i,j)',i,j,snowh(i,j),snow(i,j)
      snowh(i,j) = 0.0
      snow(i,j)  = 0.0
      snowc(i,j) = 0.0
    endif
! check consistency of snow variables after snow trim
    if((snow(i,j) <= 0..and.snowh(i,j) > 0.) .or. (snowh(i,j) <=0..and.snow(i,j) > 0.)) then
      print *,'Inconsistency of snow and snowh AFTER snow trim at i,j,snow,snowh', i,j,snow(i,j),snowh(i,j)
      snow(i,j)  = 0.
      snowh(i,j) = 0.
      snowc(i,j) = 0.
      print *,'Corrected snow and snowh at i,j,snow,snowh',i,j,snow(i,j),snowh(i,j)
    endif 
  ENDDO
  ENDDO

  write(*,*) 'SUMMARY on snow trim/build:'
  write(*,*) 'grid point with trimmed snow: ', numtrimsnow
  write(*,*) 'grid point with built snow: ', numbuildsnow
  write(*,*) 'grid point with built snow from the "trimmed" neighbor: ', numusetrim
  write(*,*) 'grid point with built min (=1 mm) snow: ', numbuildmin
  write(*,*) 'total SWE trimmed:',snowtrimsum,'[mm]'
  write(*,*) 'total SWE built:',snowbuiltsum, '[mm]'
!
!  replace seaice and xland
!
if(1==2) then  ! turn off , use GFS sea ice
  num_seaice2water=0
  num_water2seaice=0
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      if(seaice(i,j) >= xice_threshold .and. snowiceRR(i,j) < xice_threshold ) then  ! turn old seaice into water
! For water:
!for MODIS
!            ivgtyp(i,j)=luse(i,j)
!            lu_index(i,j)=luse(i,j)
        ivgtyp(i,j)=17
        lu_index(i,j)=17
        landmask(i,j)=0.
        xland_rr(i,j)=2.
        isltyp(i,j)=14
        seaice(i,j)=snowiceRR(i,j)
        num_seaice2water = num_seaice2water + 1
      elseif(seaice(i,j) < xice_threshold .and. snowiceRR(i,j) >= xice_threshold .and. surftemp(i,j) < 280.) then  ! turn old water into seaice
! for sea ice
!for MODIS
        ivgtyp(i,j)=15
        lu_index(i,j)=15
        landmask(i,j)=1.   
        xland_rr(i,j)=1.     
        isltyp(i,j)=16 
        seaice(i,j)=snowiceRR(i,j)
        num_water2seaice=num_water2seaice+1
       else

!     if(i.eq.471.and.j.eq.297) print *,'set seaice to snowiceRR, seaice(i,j),snowiceRR(i,j)', &
!                                  seaice(i,j),snowiceRR(i,j)
!             seaice(i,j)=snowiceRR(i,j)
      endif
    else
!land - nothing to do here
!switch to MODIS for land
!             ivgtyp(i,j)=luse(i,j)
!             lu_index(i,j)=luse(i,j)
!        if(i.eq.350.and.j.eq.250)print *,'land',i,j,landmask(i,j),xland_rr(i,j)
! make sure landmask and xland are consistent for land
!             landmask(i,j)=1.
!             xland_rr(i,j)=1.
!        if(i.eq.350.and.j.eq.250)print *,'land after check',i,j,landmask(i,j),xland_rr(i,j)
    endif
  ENDDO
  ENDDO
  write(*,*) 'SUMMARY on seaice:'
  write(*,*) 'grid point from old seaice into water: ', num_seaice2water
  write(*,*) 'grid point from old water  into seaice: ', num_water2seaice 
endif ! 1==2

!!! Security check for consistency of all land surface parameters on water/ice:
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      if(seaice(i,j) < xice_threshold) then
!       if(i.eq.120.and.j.eq.410) print *,'in security check, water, no ice',i,j,landmask(i,j),xland_rr(i,j)
!water
! for MODIS water category is 17
            ivgtyp(i,j)=17
            lu_index(i,j)=17
            landmask(i,j)=0.
            xland_rr(i,j)=2.
            isltyp(i,j)=14 ! STASGO water
      else
!      if(i.eq.275.and.j.eq.530)print *,'in security check, water with ice',i,j,landmask(i,j),xland_rr(i,j)
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
!  get rid of snow on water
!
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      do k=1,nsoil
         soilmoisture(i,j,k)=1.
      enddo
!    if( abs(landmask_soilmoisture1(i,j) -1.0) < 0.00001 ) then    ! water
      if( seaice(i,j) < 0.001 .and. snow(i,j) > 0.0 ) then  ! snow on water
        snow(i,j) = 0.0
        snowh(i,j) = 0.0
        snowc(i,j) = 0.0
      endif
    endif
  ENDDO
  ENDDO

!
!           update mass core netcdf file with snow,snowh,snowc
!
  write(6,*) ' ================== '
  write(6,*) ' trim snow and replace ice '
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
  write(6,*) ' Update SST in background at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
  if( (nlon_regional .ne. end_index1(1)) .or.    &
      (nlat_regional .ne. end_index1(2)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat=',nlon_regional,nlat_regional
      stop 123
  endif
   
  write(6,*) '================================================='
  rmse_var='SMOIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  deallocate(field3)
  nsig_regional=end_index(3)
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  do k=1,nsig_regional
  field3(:,:,k)=soilmoisture(:,:,k)
  enddo
  do k=1,nsig_regional
    write(6,*)' max,min SMOIS=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  write(6,*) '================================================='
  rmse_var='TSLB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  deallocate(field3)
  nsig_regional=end_index(3)
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  do k=1,nsig_regional
  field3(:,:,k)=soiltemp(:,:,k)
  enddo
  do k=1,nsig_regional
    write(6,*)' max,min TSLB=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  write(6,*) '================================================='
!  field2=seaice
!  write(6,*)' max,min sea ice=',maxval(field2),minval(field2)
!  rmse_var='SEAICE'
!  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
!       start_index,end_index1, WrfType, ierr    )
!  write(6,*)' rmse_var=',trim(rmse_var)
!!  write(6,*)' ordering=',ordering
!  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
!  write(6,*)' ndim1=',ndim1
!!  write(6,*)' staggering=',staggering
!  write(6,*)' start_index=',start_index
!  write(6,*)' end_index1=',end_index1
!!  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
!       field2,WRF_REAL,0,0,0,ordering,           &
!       staggering, dimnames ,               &
!       start_index,end_index1,               & !dom
!!       start_index,end_index1,               & !mem
!       start_index,end_index1,               & !pat
!       ierr                                 )
!!
!  write(6,*) '================================================='
  field2=snowc
  write(6,*)' max,min snowc=',maxval(field2),minval(field2)
  rmse_var='SNOWC'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           & 
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  write(6,*) '================================================='
  field2=snowh
  do j=1,nlat_regional
  do i=1,nlon_regional
  enddo
  enddo
  write(6,*)' max,min snowh=',maxval(field2),minval(field2)
  rmse_var='SNOWH'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           & 
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  write(6,*) '================================================='
  field2=snow
  do j=1,nlat_regional
  do i=1,nlon_regional
  enddo
  enddo
  write(6,*)' max,min snow=',maxval(field2),minval(field2)
  rmse_var='SNOW'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           & 
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=tskin
  write(6,*)' max,min tsk=',maxval(field2),minval(field2)
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  write(6,*) '================================================='
  field2=tsnow
  write(6,*)' max,min soilt1=',maxval(field2),minval(field2)
  rmse_var='SOILT1'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=landmask
  write(6,*)' max,min landmask=',maxval(field2),minval(field2)
  rmse_var='LANDMASK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=xland_rr
  write(6,*)' max,min xland=',maxval(field2),minval(field2)
  rmse_var='XLAND'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  field2=lu_index
  write(6,*)' max,min lu_index=',maxval(field2),minval(field2)
  rmse_var='LU_INDEX'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  allocate(ifield2(nlon_regional,nlat_regional))
  ifield2=isltyp
  write(6,*)' max,min isltyp=',maxval(ifield2),minval(ifield2)
  rmse_var='ISLTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  ifield2=ivgtyp
  write(6,*)' max,min ivgtyp=',maxval(ifield2),minval(ifield2)
  rmse_var='IVGTYP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       ifield2,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*) '================================================='
  deallocate(ifield2)
  deallocate(field2)
  call ext_ncd_ioclose(dh1, Status)
  
end subroutine update_SNOWICE_netcdf_mass

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

