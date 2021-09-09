program full_cycle_surface_netcdf_mass
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_netcdf_mass: read surface variables from latest wrf mass
!            netcdf 0 h forecast 
!           and update them in wrf mass background file
!  List of variables updated:
!	SMOIS
!	SNOW
!	SNOWH
!       SNOWC
!       SST
!    if surface data are in the same time with background data
!	CANWAT
!       SOILT1
!	TSLB
!	TSK
!    if surface data are older than background data
!       TSLB(3,4,5,6)
!         for snow grid point
!           SOILT1
!           TSK
!           TSLB(1,2)
!
!    Now, for the consistence, we add the following surface index fields
!       LANDMASK
!       XLAND
!       LU_INDEX
!       ISLTYP
!       IVGTYP
!
!    Add lake model fields
!       float SNOWDP2D 
!       float H2OSNO2D
!       float T_GRND2D
!       float T_LAKE3D
!       float LAKE_ICEFRAC3D
!       float T_SOISNO3D
!       float H2OSOI_ICE3D
!       float H2OSOI_LIQ3D
!       float H2OSOI_VOL3D
!
!   prgmmr: Ming Hu                 date: 2008-11-21
!
! program history log:
!
!    2009-11-09 Ming Hu : add snowc
!
!   input argument list:
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
! Declare local parameters

  character(len=120) :: flnmbas
  character(len=120) :: flnm1
  character(len=120) :: flnm2
  character(len=120) :: flnm3
  character(len=19)  :: DateStr1
  character(len=19)  :: DateStr2
  character(len=19)  :: DateStr3
  integer(i_kind)    :: dh1
  integer(i_kind)    :: dh2
  integer(i_kind)    :: dh3

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var

  logical :: if_integer
  logical :: if_lake_physics
  integer :: NCID,in_SF_LAKE_PHYSICS,out_SF_LAKE_PHYSICS
  integer :: numens
  character(len=3) cnumens
!
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  numens=0
  call getarg(1,cnumens)
  read(cnumens,'(i3)') numens
!
  write(*,*) 'ensemble member number is ',numens
  if(npe < numens .or. numens==0) then
    write(*,*) 'Error:core number is smaller than ensemble number!'
    call MPI_FINALIZE(ierror)
    stop 124
  endif
!
! NCEP LSF has to use all cores allocated to run this application 
! but this if check can make sure only one core run through the real code.
if(mype+1 <= numens) then
!

  if_lake_physics=.true.
  in_SF_LAKE_PHYSICS=0
  out_SF_LAKE_PHYSICS=0
!
  flnmbas="wrfinput_d02.mem"
  if( numens == 1) then
     flnm1='wrfinput_d02'
     flnm2='wrfinput_d02'
  else
     write(flnm1,'(a,I4.4)') trim(flnmbas),mype+1
     write(flnm2,'(a,I4.4)') trim(flnmbas),mype+1
  endif
  flnm3='wrfout_d01_save'
!
  STATUS=NF_OPEN(trim(flnm3),0,NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'SF_LAKE_PHYSICS',in_SF_LAKE_PHYSICS)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS=NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  if(mype==0) write(*,*) 'in_SF_LAKE_PHYSICS=',in_SF_LAKE_PHYSICS
  STATUS=NF_OPEN(trim(flnm2),0,NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'SF_LAKE_PHYSICS',out_SF_LAKE_PHYSICS)  
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS=NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  if(mype==0) write(*,*) 'out_SF_LAKE_PHYSICS=',out_SF_LAKE_PHYSICS
!
  call ext_ncd_ioinit(sysdepinfo,status)

!
!           open netcdf file to read
!
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!           open netcdf file to read
!
  call ext_ncd_open_for_read( trim(flnm3), 0, 0, "", dh3, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm3 = ',&
          trim(flnm3),', Status = ', Status
     stop 74
  endif
!
!          open netcdf file to write
!
  call ext_ncd_open_for_update( trim(flnm2), 0, 0, "", dh2, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm2 = ',&
          trim(flnm2),', Status = ', Status
     stop 75
  endif

!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  if(mype==0) write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond
  JDATE=0
  JDATE(1)=iyear
  JDATE(2)=imonth
  JDATE(3)=iday
  JDATE(5)=ihour
  JDATE(6)=iminute

  call ext_ncd_get_next_time(dh2, DateStr2, Status_next_time)

!-------------  get date info from file write out

  call ext_ncd_get_next_time(dh3, DateStr3, Status_next_time)
  read(DateStr2,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  if(mype==0) write(6,'(a,6I5)') ' write data to file at time  (y,m,d,h,m,s):'        &
              ,iyear,imonth,iday,ihour,iminute,isecond
  IDATE=0
  IDATE(1)=iyear
  IDATE(2)=imonth
  IDATE(3)=iday
  IDATE(5)=ihour
  IDATE(6)=iminute
!
! find time difference
  rinc(1)=iw3jdn(jdate(1),jdate(2),jdate(3))-  &
          iw3jdn(idate(1),idate(2),idate(3))
  rinc(2:5)=jdate(5:8)-idate(5:8)
  if(mype==0) write(*,*) 'time difference=',RINC   ! DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS
  timediff=rinc(1)*24+rinc(2)   ! hours
  if(timediff < -49 .or. timediff > 0.1 ) then
    write(*,*) 'surface data file is too old, NO CYCLE'
    call exit(1)
    stop 99
  endif
!
!  ------ update SMOIS
  if_integer=.false.
  rmse_var='SMOIS'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update SNOW
  if_integer=.false.
  rmse_var='SNOW'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update SNOWH
  if_integer=.false.
  rmse_var='SNOWH'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update SNOWC
  if_integer=.false.
  rmse_var='SNOWC'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update QVG
  if_integer=.false.
  rmse_var='QVG'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update QCG
  if_integer=.false.
  rmse_var='QCG'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update QFX
  if_integer=.false.
  rmse_var='QFX'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update HFX
  if_integer=.false.
  rmse_var='HFX'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update UST
  if_integer=.false.
  rmse_var='UST'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update PBLH
  if_integer=.false.
  rmse_var='PBLH'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

  if( abs(timediff) < 100 ) then
!  ------ update SST
      if_integer=.false.
      rmse_var='SST'
      call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update CANWAT
      if_integer=.false.
      rmse_var='CANWAT'
      call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update TSLB
      if_integer=.false.
      rmse_var='TSLB'
      call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update SOILT1
      if_integer=.false.
      rmse_var='SOILT1'
      call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update SEAICE 
!      if_integer=.false.
!      rmse_var='SEAICE'
!      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var,if_integer)


!  ------ update TSK
      if_integer=.false.
      rmse_var='TSK'
      call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

   else

!  ------ partial update TSK, SOILT1, TSLB
!      call update_netcdf_mass_partial(dh1,dh2,DateStr1,DateStr2)
      write(*,*) 'surface data file is too old, NO partial CYCLE'

   endif

!  ------ update LANDMASK
  if_integer=.false.
  rmse_var='LANDMASK'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update XLAND
  if_integer=.false.
  rmse_var='XLAND'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update LU_INDEX
  if_integer=.false.
  rmse_var='LU_INDEX'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update ISLTYP
  if_integer=.true.
  rmse_var='ISLTYP'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!  ------ update IVGTYP 
  if_integer=.true.
  rmse_var='IVGTYP'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

!
!  update lake model fields if SF_LAKE_PHYSICS = 1
!
  if(if_lake_physics .and. ( in_SF_LAKE_PHYSICS==1) .and.  &
                           (out_SF_LAKE_PHYSICS==1)       ) then
!  ------ update LAKEMASK 
  if_integer=.true.
  rmse_var='LAKEMASK'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)

     if_integer=.false.
!  ------ update LAKEDEPTH2D
     rmse_var='LAKEDEPTH2D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update SAVETKE12D
     rmse_var='SAVEDTKE12D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update SNOWDP2D
     rmse_var='SNOWDP2D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update H2OSNO2D
     rmse_var='H2OSNO2D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update T_GRND2D
     rmse_var='T_GRND2D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update T_LAKE3D
     rmse_var='T_LAKE3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update LAKE_ICEFRAC3D
     rmse_var='LAKE_ICEFRAC3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update T_SOISNO3D
     rmse_var='T_SOISNO3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update H2OSOI_ICE3D
     rmse_var='H2OSOI_ICE3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update H2OSOI_LIQ3D
     rmse_var='H2OSOI_LIQ3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
!  ------ update H2OSOI_VOL3D
     rmse_var='H2OSOI_VOL3D'
     call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
  endif
!
!  ------ update MF_VX_INV
! This is a temporary solution to avoid the block in MF_VX_INV from real.
!
  if_integer=.false.
  rmse_var='MF_VX_INV'
  call update_netcdf_mass(dh1,dh2,dh3,DateStr1,DateStr2,DateStr3,rmse_var,if_integer,mype)
  if(mype==0) write(*,*) 'replace MF_VX_INV for safty'
!
!-------------  close files ----------------
  call ext_ncd_ioclose(dh1, Status)
  call ext_ncd_ioclose(dh2, Status)
  call ext_ncd_ioclose(dh3, Status)

  if(mype==0) write(6,*) "=== RAPHRRR PREPROCCESS SUCCESS ==="

endif ! mype==0

call MPI_FINALIZE(ierror)
!

end program full_cycle_surface_netcdf_mass


!
SUBROUTINE HANDLE_ERR_M(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_M

