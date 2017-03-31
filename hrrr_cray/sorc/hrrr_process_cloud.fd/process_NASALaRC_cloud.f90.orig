program  process_NASALaRC_cloud
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
  use kinds, only: r_kind,i_kind,r_single
  use map_utils
  use misc_definitions_module , only : PROJ_LC, PROJ_ROTLL
  use constants_module ,only : EARTH_RADIUS_M
  use constants, only: init_constants_derived, deg2rad
  use gridmod_gsimap ,only : nlon,nlat,init_general_transform,tll2xy,txy2ll

  implicit none
!
  INCLUDE 'netcdf.inc'
!
  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
!  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  real(r_kind),allocatable:: rxlon(:,:)    !
  real(r_kind),allocatable:: rylat(:,:)    !

  real ::  userDX, userDY, CEN_LAT, CEN_LON
  real ::  userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON
  integer :: MAP_PROJ

  type (proj_info) :: proj_stack
  REAL :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
  REAL :: knowni, knownj, dx
  REAL :: user_known_x,user_known_y

  CHARACTER*180   geofile
!
!  For NASA LaRC 
!
  CHARACTER*180   workPath
  CHARACTER*80   satfile
  INTEGER ::   nxp, nyp  ! dimension
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  CHARACTER*24 :: cbase_time
  INTEGER(i_kind) ::  base_time
  INTEGER(i_kind) ::  ibase_year,ibase_month,ibase_day,ibase_hour,ihour
  INTEGER(i_kind) ::  icycle_year,icycle_month,icycle_day,icycle_hour
  REAL*8      time_offset
  REAL(r_single), allocatable ::   lat_l(:,:)
  REAL(r_single), allocatable ::   lon_l(:,:)
  REAL(r_single), allocatable ::   lwp_l(:,:)
  REAL(r_single), allocatable ::   teff_l(:,:)
  REAL(r_single), allocatable ::   ptop_l(:,:)
  integer(i_kind), allocatable ::   phase_l(:,:)
!
!  array for RR
!
  REAL(r_single), allocatable ::   w_pcld(:,:)
  REAL(r_single), allocatable ::   w_tcld(:,:)
  REAL(r_single), allocatable ::   w_frac(:,:)
  REAL(r_single), allocatable ::   w_lwp (:,:)
  integer(i_kind),allocatable ::   nlev_cld(:,:)

!
! Working
  integer  nfov
  parameter (nfov=160)
  real, allocatable ::     Pxx(:,:,:),Txx(:,:,:),WPxx(:,:,:)
  real,allocatable  ::     xdist(:,:,:), xxxdist(:)
  real     fr,sqrt, qc, type
  integer,allocatable  ::  PHxx(:,:,:),index(:,:), jndex(:)
  integer  ioption
  integer  ixx,ii,jj,med_pt,igrid,jgrid  &
               ,ncount,ncount1,ncount2,ii1,jj1,nobs,n

  integer(i_kind) :: npts_rad
!
!
!  ** misc
      
  real(r_kind)        :: xc  ! x-grid coordinate (grid units)
  real(r_kind)        :: yc  ! y-grid coordinate (grid units)
  real(r_kind)        :: rlon  ! earth longitude (radians)
  real(r_kind)        :: rlat  ! earth latitude  (radians)

  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

  integer i,j,k,ipt,jpt,cfov
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID
  integer :: isat

  integer :: status,mype
  character*10  atime

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  npts_rad=1

  call init_constants_derived

  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  call GET_MAP_ATT_geo(geofile, userDX, userDY, CEN_LAT, CEN_LON, &
                userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ)
  write(*,*) userDX, userDY, CEN_LAT, CEN_LON
  write(*,*) userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat),rxlon(nlon,nlat))
  allocate(ylat(nlon,nlat),rylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)
!
!   setup  map
!
!  if (MAP_PROJ == PROJ_LC) then
!     user_known_x = (NLON+1)/2.0
!     user_known_y = (NLAT+1)/2.0
!     call map_init(proj_stack)
!
!     call map_set(MAP_PROJ, proj_stack, &
!                  truelat1=userTRUELAT1, &
!                  truelat2=userTRUELAT2, &
!                  stdlon=STAND_LON, &
!                  lat1=CEN_LAT, &
!                  lon1=CEN_LON, &
!                  knowni=user_known_x, &
!                  knownj=user_known_y, &
!                  dx=userDX, &
!                  r_earth=earth_radius_m)
!  else
     mype=0
     rylat=ylat*deg2rad
     rxlon=xlon*deg2rad
     call init_general_transform(rylat,rxlon,mype)
!  endif
!
  allocate (Pxx(nlon,nlat,nfov),Txx(nlon,nlat,nfov),WPxx(nlon,nlat,nfov))
  allocate (xdist(nlon,nlat,nfov), xxxdist(nfov))
  allocate (PHxx(nlon,nlat,nfov),index(nlon,nlat), jndex(nfov))
  index=0
!
!  read in the NASA LaRC cloud data
  atime='2009091616'
  DO isat=1,2
    if(isat==1) then
       satfile='nasaLaRC_gesoWest.nc'
       nxp=1800
       nyp=700
       allocate(lat_l(nxp,nyp))
       allocate(lon_l(nxp,nyp))
       allocate(ptop_l(nxp,nyp))
       allocate(teff_l(nxp,nyp))
       allocate(phase_l(nxp,nyp))
       allocate(lwp_l(nxp,nyp))
     elseif(isat==2) then
       satfile='nasaLaRC_gesoEast.nc'
       nxp=1500
       nyp=850
       allocate(lat_l(nxp,nyp))
       allocate(lon_l(nxp,nyp))
       allocate(ptop_l(nxp,nyp))
       allocate(teff_l(nxp,nyp))
       allocate(phase_l(nxp,nyp))
       allocate(lwp_l(nxp,nyp))
     else
       write(6,*) 'No satellite information '
       stop 123
     endif
     ptop_l=-9.
     teff_l=-9.
     lat_l =-9.
     lon_l =-9.
     lwp_l =-9.
     phase_l=-9
     call read_NASALaRC_cloud(satfile,atime,nxp,nyp,ptop_l, teff_l, phase_l, lwp_l,lat_l, lon_l)

     write(6,*)'LaRC isat/nxl/nyl =',isat,nxp, nyp

!     write(6,*)'LaRC ptop =', (ptop_l(500,j),j=1,nyp,50)
!     write(6,*)'LaRC teff =', (teff_l(500,j),j=1,nyp,50)
!     write(6,*)'LaRC lat  =', (lat_l(500,j),j=1,nyp,50)
!     write(6,*)'LaRC lon  =', (lon_l(500,j),j=1,nyp,50)
!     write(6,*)'LaRC lwp  =', (lwp_l(500,j),j=1,nyp,50)
!     write(6,*)'LaRC phase  =', (phase_l(500,j),j=1,nyp,50)

     do j=1,nyp
     do i=1,nxp
        if (phase_l(i,j).eq.4) phase_l(i,j) = 0   ! clear
        if (phase_l(i,j).eq.5) phase_l(i,j) = -9  ! bad data
!                                     equivalent to "no data"
!        if (phase_l(i,j).eq.-9) ptop_l(i,j) = -9.
        if (phase_l(i,j).eq.0) ptop_l(i,j) = -20.
     enddo
     enddo

! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RR grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
     if(userDX < 7000.0) then
        npts_rad=3
     else
        npts_rad=1
     endif
     write(*,*) 'The number of impact point is=',npts_rad
     do jpt=1,nyp
     do ipt=1,nxp
       if (phase_l(ipt,jpt).ge.0) then
!  Indicates there is some data (not missing)
!         if (MAP_PROJ == PROJ_LC) then
!           call latlon_to_ij(proj_stack, 90.0-lat_l(ipt,jpt), lon_l(ipt,jpt), xc, yc)
!         else
           rlon=lon_l(ipt,jpt)*deg2rad
!mhu           rlat=(90.0-lat_l(ipt,jpt))*deg2rad
           rlat=lat_l(ipt,jpt)*deg2rad
           call tll2xy(rlon,rlat,xc,yc)
!           call txy2ll(xc,yc,rlon,rlat)
!         endif
! * Compute RR grid x/y at lat/lon of cloud data
! -----------------------------------------------------------
! * XC,YC should be within RR boundary, i.e., XC,YC >0

         ii1 = int(xc+0.5)
         jj1 = int(yc+0.5)
         do jj = max(1,jj1-npts_rad), min(nlat,jj1+npts_rad)
         if (jj1-1.ge.1 .and. jj1+1.le.nlat) then
         do ii = max(1,ii1-npts_rad), min(nlon,ii1+npts_rad)
         if (ii1-1.ge.1 .and. ii1+1.le.nlon) then
!         if(XC .ge. 1. .and. XC .lt. nlon .and.        &
!            YC .ge. 1. .and. YC .lt. nlat) then
!             ii1 = int(xc+0.5)
!             jj1 = int(yc+0.5)
!             ii=ii1
!             jj=jj1

! * We check multiple data within gridbox

                 if (index(ii,jj).lt.nfov) then
                   index(ii,jj) = index(ii,jj) + 1

                   Pxx(ii,jj,index(ii,jj)) = Ptop_l(ipt,jpt)
                   Txx(ii,jj,index(ii,jj)) = Teff_l(ipt,jpt)
                   PHxx(ii,jj,index(ii,jj)) = phase_l(ipt,jpt)
                   WPxx(ii,jj,index(ii,jj)) = lwp_l(ipt,jpt)
                   xdist(ii,jj,index(ii,jj)) = sqrt(      &
                      (XC+1-ii)**2 + (YC+1-jj)**2)
                 else
                   write(6,*) ' too many data in one grid, increase nfov'
                   write(6,*) nfov, ii,jj
                   stop 1234
                 end if
         endif
         enddo ! ii
         endif
         enddo  ! jj
!         endif   ! observation is in the domain

       endif   ! phase_l >= 0
     enddo   ! ipt
     enddo   ! jpt

     deallocate(lat_l,lon_l,ptop_l,teff_l,phase_l,lwp_l)

  ENDDO  ! isat 

!
  write(6,*) 'The max index number is: ', maxval(index)

! * ioption = 1 is nearest neighrhood
! * ioption = 2 is median of cloudy fov
  ioption = 2
  allocate(w_pcld(nlon,nlat))
  allocate(w_tcld(nlon,nlat))
  allocate(w_frac(nlon,nlat))
  allocate(w_lwp(nlon,nlat))
  allocate(nlev_cld(nlon,nlat))
  w_pcld=99999.
  w_tcld=99999.
  w_frac=99999.
  w_lwp=99999.
  nlev_cld = 99999

  do jj = 1,nlat
  do ii = 1,nlon
    if ((index(ii,jj) .ge. 1 .and. index(ii,jj) .lt. 3) .and. userDX < 7000.0 ) then
          w_pcld(ii,jj) = Pxx(ii,jj,1) ! hPa
          w_tcld(ii,jj) = Txx(ii,jj,1) ! K
          w_lwp(ii,jj) = WPxx(ii,jj,1) !  g/m^2
          w_frac(ii,jj) = 1
          nlev_cld(ii,jj) = 1
          if (w_pcld(ii,jj).eq.-20) then
               w_pcld(ii,jj) = 1013. ! hPa - no cloud
               w_frac(ii,jj)=0.0
               nlev_cld(ii,jj) = 0
          end if
    elseif(index(ii,jj) .ge. 3) then

! * We decided to use nearest neighborhood for ECA values,
! *     a kind of convective signal from GOES platform...
!
! * Sort to find median value 
      if(ioption .eq. 2) then    !pick median 
          do i=1,index(ii,jj)
              jndex(i) = i
              xxxdist(i) = Pxx(ii,jj,i)
          enddo
          call sortmed(xxxdist,index(ii,jj),jndex,fr)
          med_pt = index(ii,jj)/2  + 1
          w_pcld(ii,jj) = Pxx(ii,jj,jndex(med_pt)) ! hPa
          w_tcld(ii,jj) = Txx(ii,jj,jndex(med_pt)) ! K
          w_lwp(ii,jj) = WPxx(ii,jj,jndex(med_pt)) !  g/m^2
          if (w_pcld(ii,jj).eq.-20) then
               w_pcld(ii,jj) = 1013. ! hPa - no cloud
               w_frac(ii,jj)=0.0
               nlev_cld(ii,jj) = 0
          end if
      endif   ! pick median
!
! cloud fraction based on phase (0 are clear), what about -9 ????
      if( w_pcld(ii,jj) < 1012.99) then
        cfov = 0
        do i=1,index(ii,jj)
          if(PHxx(ii,jj,i) .gt. 0.1) cfov = cfov + 1
        enddo
        w_frac(ii,jj) = float(cfov)/(max(1,index(ii,jj)))     !  fraction
        if( w_frac(ii,jj) > 0.01 ) nlev_cld(ii,jj) = 1
      endif

    endif   ! index > 3
  enddo  !ii
  enddo  !jj

      write (6,*) 'index'
      write (6,'(10i10)') (index(300,jj),jj=1,nlat,10)
      write (6,*) 'w_pcld'
      write (6,'(10f10.2)') (w_pcld(300,jj),jj=1,nlat,10)
      write (6,*) 'w_tcld'
      write (6,'(10f10.2)') (w_tcld(300,jj),jj=1,nlat,10)
      write (6,*) 'w_frac'
      write (6,'(10f10.2)') (w_frac(300,jj),jj=1,nlat,10)
      write (6,*) 'w_lwp '
      write (6,'(10f10.2)') (w_lwp (300,jj),jj=1,nlat,10)
      write (6,*) 'nlev_cld'
      write (6,'(10I10)') (nlev_cld(300,jj),jj=1,nlat,10)
  OPEN(15, file='NASALaRC_cloud.bin',form='unformatted')
    write(15)  nlon,nlat
    write(15)  xlon
    write(15)  ylat
    write(15)  index
    write(15)  w_pcld
    write(15)  w_tcld
    write(15)  w_frac
    write(15)  w_lwp
    write(15)  nlev_cld
  CLOSE(15)
!
!  write out results
!
  call write_bufr_NASALaRC(nlon,nlat,userDX,index,w_pcld,w_tcld,w_frac,w_lwp,nlev_cld)
!

!  DO j=1,nlat
!  DO I=1,nlon
!    if( w_lwp(i,j) > 99998.0 ) w_lwp(i,j) = 0.0
!  ENDDO
!  ENDDO
!
end program process_NASALaRC_cloud

subroutine sortmed(p,n,is)
      real p(n)
      integer is(n)
! * count cloudy fov
      real    f
      integer cfov
      cfov = 0
      do i=1,n
! - changed for NASA LaRC, p set = -9 for clear FOVs
         if(p(i) .gt. 0.) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1
      ip1 = i+1 
        do 10 j=ip1,n
        if(p(i).le.p(j)) goto 10
          temp = p(i) 
          p(i) = p(j)
          p(j) = temp
          iold  = is(i)
          is(i) = is(j)
          is(j) = iold
   10 continue
      return
end subroutine sortmed
                                                                                 
