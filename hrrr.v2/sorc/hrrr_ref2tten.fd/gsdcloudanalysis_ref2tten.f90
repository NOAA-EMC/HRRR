program gsdcloudanalysis_ref2tten
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! program:  gsdcloudanalysis_ref2tten   driver for radar tten
! calculation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2014-03-28
!
! ABSTRACT: 
!
! PROGRAM HISTORY LOG:
!    2014-03-28  Hu  Add NCO document block
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (ZEUS)
!
!$$$
!
!_____________________________________________________________________
!

  use mpi
  use kinds,   only: r_single,i_kind, r_kind
  use constants, only: init_constants,init_constants_derived

  implicit none
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror
!
! dimension
!
!  NETCDF
!
  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  integer(i_kind)    :: dh1

  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind) :: wrf_real
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  character (len=80) :: SysDepInfo

  integer(i_kind) :: ierr,Status,Status_next_time
  character (len=31) :: rmse_var
  character (len=80), dimension(3)  ::  dimnames
!
! background
!
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single)  pt_regional
  
  integer(i_kind) :: iunit
  data iunit / 15 /

  real(r_single),allocatable:: t_bk(:,:,:)
  real(r_single),allocatable:: h_bk(:,:,:)
  real(r_single),allocatable:: p_bk(:,:,:)
  real(r_single),allocatable:: ps_bk(:,:)
  real(r_single),allocatable:: zh(:,:)
  real(r_single),allocatable:: q_bk(:,:,:)

  real(r_single),allocatable:: pblh(:,:)         ! PBL height (grid coordinate)

  real(r_kind) :: pt_ll
  real(r_kind),allocatable :: eta1_ll(:)  !
  real(r_kind),allocatable :: aeta1_ll(:)   !

!
!  radar observation : 3D reflectvity in RR grid
!
  character (len=80) :: radarfile
  real(r_single),allocatable :: ref_mos_3d(:,:,:)
  real(r_single),allocatable :: ref_mosaic31(:,:,:)
  INTEGER(i_kind)          :: Nmsclvl_radar,nlon_radar,nlat_radar
  real(r_single)  ::  krad_bot          ! radar bottom level
  INTEGER(i_kind)          :: iunit_radar
!
!
!
  real(r_single),allocatable :: ges_tten(:,:,:)
  real(r_kind) :: dfi_lhtp
  logical         :: l_tten_for_convection_only
  real(r_single) :: dfi_radar_latent_heat_time_period
  REAL(r_kind) :: convection_refl_threshold     ! units dBZ
!
  real(r_single),allocatable :: field1(:)
!
  INTEGER(i_kind) :: miss_obs_int
  REAL(r_kind)    :: miss_obs_real
  PARAMETER ( miss_obs_int = -99999  )
  PARAMETER ( miss_obs_real = -99999.0_r_single )

!  real(r_single), allocatable :: sat_ctp(:,:)
!
!  misc.
!
  INTEGER(i_kind) :: i,j,k,n

!
! ===============================================================================
!
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  write(*,*) mype, 'deal with tten'
  mypeLocal=mype+1

! 2.0  open and read background dimesion
!          
  call init_constants(.true.)
  call init_constants_derived
  end_index=0
  start_index=0
  krad_bot=7.0_r_single
  dfi_radar_latent_heat_time_period=20.0_r_single
  convection_refl_threshold=28.0_r_kind
  l_tten_for_convection_only=.true.
  Nmsclvl_radar = -999
  iunit_radar=25

  call ext_ncd_ioinit(sysdepinfo,status)

  flnm1='wrf_inout'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'problem with flnm1 = ',&
                trim(flnm1),', Status = ', Status
      stop 1234
  endif
  
!-------------  get date info

     call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
     read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)')  &
                                iyear,imonth,iday,ihour,iminute,isecond
     write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'

  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index, WrfType, ierr    )   

  write(6,*)' rmse_var = ',trim(rmse_var),' ndim1 = ',ndim1,' dh1 = ',dh1
  write(6,*)' WrfType = ',WrfType,'ierr  = ',ierr 
  write(6,*)' ordering = ',trim(ordering),' staggering = ',trim(staggering)
  write(6,*)' start_index = ',start_index
  write(6,*)' end_index   = ',end_index

  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)'nlon,lat,sig_regional=', &
             nlon_regional,nlat_regional,nsig_regional
!
!
! 2.2   allocate background and analysis fields
!
  allocate(t_bk(nlon_regional,nlat_regional,nsig_regional))
  allocate(q_bk(nlon_regional,nlat_regional,nsig_regional))
  allocate(ps_bk(nlon_regional,nlat_regional))
  allocate(zh(nlon_regional,nlat_regional))
  allocate(eta1_ll(nsig_regional+1))
  allocate(aeta1_ll(nsig_regional))
!
! 2.4  read in background fields
!          
  call read_netcdf_mass(dh1,DateStr1,nlon_regional,nlat_regional,nsig_regional, &
                   pt_ll,eta1_ll,aeta1_ll,zh,ps_bk,q_bk,t_bk)

  call ext_ncd_ioclose(dh1, Status)
!          
!        zh(i,j)       !  terrain in meter
!        ps_bk(i,j)    !  surace pressure in mb
!        q_bk(i,j,k)   !  humidity in mixing ratio
!        t_bk(i,j,k)   !  T in potentional
!

  allocate(h_bk(nlon_regional,nlat_regional,nsig_regional))
  allocate(p_bk(nlon_regional,nlat_regional,nsig_regional))
  call BackgroundCld(nlon_regional,nlat_regional,nsig_regional, &
                     t_bk,p_bk,ps_bk,q_bk,h_bk,zh,              &
                     pt_ll,eta1_ll,aeta1_ll)

  do k=1,nsig_regional
     write(*,*) 'max.min, h_bk=',k,maxval(h_bk(:,:,k)), minval(h_bk(:,:,k))
  enddo
  do k=1,nsig_regional
     write(*,*) 'max.min, p_bk=',k,maxval(p_bk(:,:,k)), minval(p_bk(:,:,k))
  enddo

! 
! 2.5 calculate PBL height
! 
  allocate(pblh(nlon_regional,nlat_regional))
  call calc_pbl_height(nlon_regional,nlat_regional,nsig_regional,q_bk,t_bk,p_bk,pblh)
  write(*,*) 'max.min, pblh=',maxval(pblh), minval(pblh)

  deallocate(q_bk)
  deallocate(ps_bk)

!  allocate(sat_ctp(nlon_regional,nlat_regional))
!  sat_ctp=miss_obs_real

  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'problem with flnm1 = ',&
                trim(flnm1),', Status = ', Status
      stop 1234
  endif
!-------------  get date info
  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)')  &
                                iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!  DO n=1,4
  n=mypeLocal

     write(radarfile,'(a,I2.2)') 'RefInGSI3D.dat_',n
     write(*,*)
     write(*,*) 'processing ',trim(radarfile)
     open(iunit_radar,file=trim(radarfile),form='unformatted')
        read(iunit_radar) Nmsclvl_radar,nlon_radar,nlat_radar
        allocate(ref_mosaic31(nlon_regional,nlat_regional,Nmsclvl_radar))
        read(iunit_radar) ref_mosaic31
     close(iunit_radar)
     write(*,*) 'Nmsclvl_radar,nlon_radar,nlat_radar',  &
                 Nmsclvl_radar,nlon_radar,nlat_radar
     do k=1,Nmsclvl_radar
        write(*,*) 'ref_mosaic31=',k,maxval(ref_mosaic31(:,:,k)), &
                                     minval(ref_mosaic31(:,:,k))
     enddo
!
!  2.6 vertical interpolation of radar reflectivity
!
     allocate(ref_mos_3d(nlon_regional,nlat_regional,nsig_regional))
     ref_mos_3d=miss_obs_real
     call vinterp_radar_ref(nlon_regional,nlat_regional,nsig_regional,Nmsclvl_radar, &
                         ref_mos_3d,ref_mosaic31,h_bk,zh)
     do k=1,nsig_regional
        write(*,*) 'vinterp ref_mos_3d=',k,maxval(ref_mos_3d(:,:,k)), &
                                           minval(ref_mos_3d(:,:,k))
     enddo
     deallocate( ref_mosaic31 )
     call build_missing_REFcone(nlon_regional,nlat_regional,nsig_regional, &
                             krad_bot,ref_mos_3d,h_bk,pblh)
     do k=1,nsig_regional
        write(*,*) 'refcon ref_mos_3d=',k,maxval(ref_mos_3d(:,:,k)), &
                                          minval(ref_mos_3d(:,:,k))
     enddo

!
! 4.10 radar temperature tendency for DFI
!
     allocate(ges_tten(nlon_regional,nlat_regional,nsig_regional))
     ges_tten=-20.0_r_kind
     ges_tten(:,:,nsig_regional)=-10.0_r_kind

     dfi_lhtp=dfi_radar_latent_heat_time_period
     call radar_ref2tten(nlon_regional,nlat_regional,nsig_regional,ref_mos_3d,& 
                     p_bk,t_bk,ges_tten,dfi_lhtp,krad_bot,pblh,  &
                     l_tten_for_convection_only,convection_refl_threshold)
     deallocate(ref_mos_3d)
     do k=1,nsig_regional
        write(*,*) 'ges_tten=',k,maxval(ges_tten(:,:,k)), &
                                 minval(ges_tten(:,:,k))
     enddo

!
! 5.10 update
!
     WRF_REAL=104
     write(rmse_var,'(a,I1)') 'RAD_TTEN_DFI_',n
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
                                start_index,end_index, WrfType, ierr    )

     if( (nlon_regional .ne. end_index(1)) .or.    &
         (nlat_regional .ne. end_index(2))) then
          write(6,*) ' Dimensions do not match!!!'
          write(6,*)' nlon,lat=',nlon_regional,nlat_regional
          stop 123
      endif

      write(6,*)' >>>>>>>>>>>  write out data to dh1  = ',dh1
      write(6,*)' rmse_var=',trim(rmse_var)
      write(6,*)' ordering=',ordering
      write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
      write(6,*)' ndim1=',ndim1
      write(6,*)' staggering=',staggering
      write(6,*)' start_index=',start_index
      write(6,*)' end_index=',end_index
      call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
           ges_tten,WRF_REAL,0,0,0,ordering,           &
           staggering, dimnames ,               &
           start_index,end_index,               & !dom
           start_index,end_index,               & !mem
           start_index,end_index,               & !pat
           ierr                                 )

      deallocate(ges_tten)

!  ENDDO ! n
!
  if(mype == 0) then

      allocate(field1(4))
      field1(1)=15.0_r_single
      field1(2)=30.0_r_single
      field1(3)=45.0_r_single
      field1(4)=60.0_r_single
      end_index=1
      rmse_var='TTEN_TIMES'
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
           field1,WRF_REAL,0,0,0,ordering,           &  
           staggering, dimnames ,               &  
           start_index,end_index,               & !dom 
           start_index,end_index,               & !mem 
           start_index,end_index,               & !pat 
           ierr     )
      deallocate(field1)
  endif

  call ext_ncd_ioclose(dh1, Status)
  write(*,*) 'finisd, now release memory'
!
!  release memory
!
!  deallocate(sat_ctp)
  deallocate(t_bk)
  deallocate(p_bk)
  deallocate(pblh)
  deallocate(zh)
  deallocate(eta1_ll,aeta1_ll)

  write(*,*) 'End of release memory'

  call MPI_FINALIZE(ierror)

end program gsdcloudanalysis_ref2tten

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

