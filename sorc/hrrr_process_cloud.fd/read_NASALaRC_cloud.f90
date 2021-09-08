subroutine read_NASALaRC_cloud(satfile,atime,nxp,nyp,ptop, teff, phase, lwp_iwp,lat, lon)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-09-04
!
! ABSTRACT: 
!     This routine read in NASA LaRC cloud products and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,i_kind

  implicit none
!
  INCLUDE 'netcdf.inc'
!
!  For NASA LaRC 
!
  CHARACTER*40   satfile

  INTEGER ::   nxp, nyp  ! dimension
  INTEGER     FAILURE_P 
  PARAMETER ( FAILURE_P  =  1       )
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  CHARACTER*24 :: cbase_time
  INTEGER(i_kind) ::  base_time
  INTEGER(i_kind) ::  ibase_year,ibase_month,ibase_day,ibase_hour,ihour
  INTEGER(i_kind) ::  icycle_year,icycle_month,icycle_day,icycle_hour
  REAL*8      time_offset
  REAL*4      lat                            (  nxp,  nyp)
  REAL*4      lon                            (  nxp,  nyp)
  integer     phase                          (  nxp,  nyp)
  REAL*4      lwp_iwp                        (  nxp,  nyp)
  REAL*4      teff                           (  nxp,  nyp)
  REAL*4      ptop                           (  nxp,  nyp)
!
!
!  ** misc
      
  integer i,j,k
  Integer nf_status,nf_fid,nx,ny,nf_vid

  integer :: NCID

  integer :: status
  character*10  atime

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  status = failure_p
  write(6,*) 'read in satellite data from: ',trim(satfile)
  nf_status = NF_OPEN(trim(satfile), NF_NOWRITE,nf_fid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'NF_OPEN '//atime(1:7)//trim(satfile)
        status=failure_p
        return
  endif
! -------------------------------------------------
!    statements to fill base_time
!
  nf_status = NF_INQ_VARID(nf_fid,'base_time',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var base_time'
  endif
  nf_status = NF_GET_VAR_INT(nf_fid,nf_vid,base_time)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ base_time '
  end if
!mhu crash in new netcdf (Dec.2, 2010)
if(1==2) then
  nf_status = NF_GET_ATT_TEXT(nf_fid,nf_vid,'string',cbase_time)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_ATT_TEXT_ cbase_time '
  end if
  read(cbase_time(1:4),'(I4)') ibase_year
  read(cbase_time(6:7),'(I2)') ibase_month
  read(cbase_time(9:10),'(I2)') ibase_day
  read(cbase_time(12:13),'(I2)') ibase_hour
  write(6,*) 'observation base time is ',cbase_time
endif
! -------------------------------------------------
!    statements to fill time_offset
!
  nf_status = NF_INQ_VARID(nf_fid,'time_offset',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var time_offset'
  endif
  nf_status = NF_GET_VAR_DOUBLE(nf_fid,nf_vid,time_offset)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ time_offset '
  end if
!mhu  ihour=ibase_hour + int(time_offset/3600)
!mhu  write(*,*) 'obs hour:', ihour

!*** Check time of obs to make sure valid for this analysis
  read(atime(1:4),'(I4)') icycle_year
  read(atime(5:6),'(I2)') icycle_month
  read(atime(7:8),'(I2)') icycle_day
  read(atime(9:10),'(I2)') icycle_hour
!  if( (icycle_year.ne.ibase_year) .or. (icycle_month.ne.ibase_month) .or.  &
!      (icycle_day.ne.ibase_day) .or. (icycle_hour.ne.ihour) ) then
!     write(6,*) 'Observation time does not match cycle time'
!     write(6,*) 'obs time  ',ibase_year,ibase_month,ibase_day,ihour
!     write(6,*) 'cycle time:',icycle_year,icycle_month,icycle_day,icycle_hour
!     stop 1234
!  endif

! -------------------------------------------------
!    statements to fill lat
!
  nf_status = NF_INQ_VARID(nf_fid,'latitude',nf_vid)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var lat'
  endif
  nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lat)
  if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Lat '
  end if
!  print*, 'lat900',lat(900,350)
! -------------------------------------------------
!    statements to fill lon
!
   nf_status = NF_INQ_VARID(nf_fid,'longitude',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var lon'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lon)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ Lon '
   end if
!   print*, 'lon900',lon(900,350)
! -------------------------------------------------
!    statements to fill ptop (cloud top pressure)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_top_pressure',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var ptop'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,ptop)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ ptop '
   end if
!   print*, 'ptop900',ptop(900,350)
! -------------------------------------------------
!    statements to fill teff (cloud effective temperature)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_effective_temperature',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var cloud_effective_temperature'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,teff)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ teff '
   end if
!   print*, 'teff900',teff(900,350)
! -------------------------------------------------
!    statements to fill phase (cloud phase)
!
   nf_status = NF_INQ_VARID(nf_fid,'cloud_phase',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var cloud_phase'
   endif
   nf_status = NF_GET_VAR_int(nf_fid,nf_vid,phase)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ cloud_phase '
   end if
!   print*, 'cloud_phase900',phase(900,350)
! -------------------------------------------------
!    statements to fill LWP-IWP (liquid water path)
!
   nf_status = NF_INQ_VARID(nf_fid,'liquid_water_path',nf_vid)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in var liquid_water_path'
   endif
   nf_status = NF_GET_VAR_REAL(nf_fid,nf_vid,lwp_iwp)
   if(nf_status.ne.NF_NOERR) then
        write(6,*)  NF_STRERROR(nf_status)
        write(6,*) 'in NF_GET_VAR_ lwp_iwp '
   end if
!   print*, 'lwp_iwp900',lwp_iwp(900,350)
! -------------------------------------------------

!
end subroutine read_NASALaRC_cloud
