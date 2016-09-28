program full_cycle_netcdf_mass
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_netcdf_mass: read some variables from one wrf mass
!            netcdf file and write to another mass netcdf file 
!  List of variables updated:
!	SNOW
!	SNOWH
!
!   prgmmr: Ming Hu                 date: 2011-02-24
!
! program history log:
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind
  implicit none

! Declare local parameters

  character(len=120) :: flnm1
  character(len=120) :: flnm2
  character(len=19)  :: DateStr1
  character(len=19)  :: DateStr2
  integer(i_kind)    :: dh1
  integer(i_kind)    :: dh2

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var

  call ext_ncd_ioinit(sysdepinfo,status)

!
!           open netcdf file to read
!
  flnm1='wrf_4read'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!
!          open netcdf file to write
!
  flnm2='wrf_4writ'
  call ext_ncd_open_for_update( trim(flnm2), 0, 0, "", dh2, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm2 = ',&
          trim(flnm2),', Status = ', Status
     stop 75
  endif

!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond
  JDATE=0
  JDATE(1)=iyear
  JDATE(2)=imonth
  JDATE(3)=iday
  JDATE(5)=ihour
  JDATE(6)=iminute

!-------------  get date info from file write out

  call ext_ncd_get_next_time(dh2, DateStr2, Status_next_time)
  read(DateStr2,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)') ' write data to file at time  (y,m,d,h,m,s):'        &
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
  write(*,*) RINC   ! DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS
  timediff=rinc(1)*24+rinc(2)   ! hours
!
!  ------ update SNOW
  rmse_var='SNOW'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SNOWH
  rmse_var='SNOWH'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)
 
!  ------ update SNOW T
  call update_netcdf_mass_snowT(dh1,dh2,DateStr1,DateStr2)

!-------------  close files ----------------
  call ext_ncd_ioclose(dh1, Status)
  call ext_ncd_ioclose(dh2, Status)

end program full_cycle_netcdf_mass

