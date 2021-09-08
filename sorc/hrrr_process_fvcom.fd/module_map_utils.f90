!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
module module_map_utils

  use kinds, only: i_byte,r_kind,r_single,i_kind
  implicit none

  public :: map_util
!
!-------------------------------------------------------------------------

! set default to private
  private
  type :: map_util
      integer :: nlat,nlon
    contains
      procedure :: init_general_transform
      procedure :: destory_general_transform
      procedure :: tll2xy
      procedure :: txy2ll
      procedure :: rotate_wind_xy2ll
      procedure :: rotate_wind_ll2xy
  end type map_util

      real(r_kind) pihalf,sign_pole,rlambda0
      real(r_kind) atilde_x,btilde_x,atilde_y,btilde_y
      real(r_kind) btilde_xinv,btilde_yinv
      integer(i_kind) nxtilde,nytilde
      real(r_kind),allocatable::xtilde0(:,:),ytilde0(:,:)
      real(r_kind),allocatable::beta_ref(:,:),cos_beta_ref(:,:),sin_beta_ref(:,:)
      integer(i_kind),allocatable::i0_tilde(:,:),j0_tilde(:,:)
      integer(i_byte),allocatable::ip_tilde(:,:),jp_tilde(:,:)
!
! constants
!
  integer(i_kind),parameter::  izero    = 0_i_kind
  integer(i_kind),parameter::  ione     = 1_i_kind
  real(r_kind),   parameter::  zero     = 0.0_r_kind
  real(r_kind),   parameter::  one      = 1.0_r_kind
  real(r_kind),   parameter::  two      = 2.0_r_kind
  real(r_kind),   parameter::  half     = 0.5_r_kind
  real(r_kind),   parameter::  one_tenth= 0.10_r_kind
  real(r_kind),   parameter::  pi       = acos(-one)
  real(r_kind),   parameter::  deg2rad  = pi/180.0_r_kind
  real(r_kind),   parameter::  rad2deg  = one/deg2rad
!
contains
   
 subroutine init_general_transform(this,nlon,nlat,glatsin,glonsin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_transform
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    glons,glats - lons,lats of input grid points of dimesion nlon,nlat(degree)
!    mype        - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  class(map_util) :: this
  integer,intent(in) :: nlon,nlat
  real(r_single)   ,intent(in   ) :: glatsin(nlon,nlat),glonsin(nlon,nlat)
  real(r_kind) :: glats(nlon,nlat),glons(nlon,nlat)

  real(r_kind),parameter:: r0_01=0.01_r_kind
  real(r_kind),parameter:: rbig =1.0e30_r_kind
  real(r_kind) xbar_min,xbar_max,ybar_min,ybar_max
  real(r_kind) clon,slon,r_of_lat,xbar,ybar
  integer(i_kind) i,j,istart0,iend,iinc,itemp,ilast,jlast
  real(r_kind) cosalpha,sinalpha,denom,epslon,r0,r1,x0,x1,x2,y0,y1,y2
  integer(i_kind) ip

  pihalf=half*pi
  this%nlon=nlon
  this%nlat=nlat

  glats=glatsin*deg2rad
  glons=glonsin*deg2rad
!  define xtilde, ytilde grid, transform

!      glons,glats are lons, lats of input grid points of dimension nlon,nlat
  call get_xytilde_domain(nlon,nlat,glons,glats,nxtilde,nytilde, &
                   xbar_min,xbar_max,ybar_min,ybar_max)
  allocate(i0_tilde(nxtilde,nytilde),j0_tilde(nxtilde,nytilde))
  allocate(ip_tilde(nxtilde,nytilde),jp_tilde(nxtilde,nytilde))
  allocate(xtilde0(nlon,nlat),ytilde0(nlon,nlat))

! define atilde_x, btilde_x, atilde_y, btilde_y

  btilde_x   =(nxtilde -one     )/(xbar_max-xbar_min)
  btilde_xinv=(xbar_max-xbar_min)/(nxtilde -one     )
  atilde_x   =one-btilde_x*xbar_min
  btilde_y   =(nytilde -one     )/(ybar_max-ybar_min)
  btilde_yinv=(ybar_max-ybar_min)/(nytilde -one     )
  atilde_y   =one-btilde_y*ybar_min

! define xtilde0,ytilde0
  do j=1,nlat
     do i=1,nlon
        r_of_lat=pihalf+sign_pole*glats(i,j)
        clon=cos(glons(i,j)+rlambda0)
        slon=sin(glons(i,j)+rlambda0)
        xbar=r_of_lat*clon
        ybar=r_of_lat*slon
        xtilde0(i,j)=atilde_x+btilde_x*xbar
        ytilde0(i,j)=atilde_y+btilde_y*ybar
     end do
  end do

!  now get i0_tilde, j0_tilde, ip_tilde,jp_tilde
  ilast=ione ; jlast=ione
  istart0=nxtilde
  iend=ione
  iinc=-ione
  do j=1,nytilde
     itemp=istart0
     istart0=iend
     iend=itemp
     iinc=-iinc
     ybar=j
     do i=istart0,iend,iinc
        xbar=i
        call nearest_3(ilast,jlast,i0_tilde(i,j),j0_tilde(i,j), &
                       ip_tilde(i,j),jp_tilde(i,j),xbar,ybar,nlon,nlat,xtilde0,ytilde0)
     end do
  end do

!  now compute beta_ref, used in alpha = beta_ref + sign_pole*earth_lon, and alpha is 
!   angle between earth positive east and grid positive x.  This is needed
!   for rotation of u,v from earth to grid coordinate.
  allocate(beta_ref(nlon,nlat),cos_beta_ref(nlon,nlat),sin_beta_ref(nlon,nlat))
  epslon=r0_01*deg2rad
  do j=1,nlat
     do i=1,nlon-ione
        ip=i+ione
        r0=two*cos(glats(i,j ))/(one-sign_pole*sin(glats(i,j )))
        x0=r0 *cos(glons(i,j ))
        y0=r0 *sin(glons(i,j ))
        r1=two*cos(glats(ip,j))/(one-sign_pole*sin(glats(ip,j)))
        x1=r1 *cos(glons(ip,j))
        y1=r1 *sin(glons(ip,j))
        x2=r0 *cos(glons(i,j)+epslon)
        y2=r0 *sin(glons(i,j)+epslon)
        denom=one/sqrt(((x1-x0)**2+(y1-y0)**2)*((x2-x0)**2+(y2-y0)**2))
        cosalpha=((x2-x0)*(x1-x0)+(y2-y0)*(y1-y0))*denom
        sinalpha=((x2-x0)*(y1-y0)-(y2-y0)*(x1-x0))*denom
        beta_ref(i,j)=atan2(sinalpha,cosalpha)-sign_pole*glons(i,j)
        cos_beta_ref(i,j)=cos(beta_ref(i,j))
        sin_beta_ref(i,j)=sin(beta_ref(i,j))
     end do
     i=nlon
     ip=nlon-ione
     r0=two*cos(glats(i,j ))/(one-sign_pole*sin(glats(i,j )))
     x0=r0 *cos(glons(i,j ))
     y0=r0 *sin(glons(i,j ))
     r1=two*cos(glats(ip,j))/(one-sign_pole*sin(glats(ip,j)))
     x1=r1 *cos(glons(ip,j))
     y1=r1 *sin(glons(ip,j))
     x2=r0 *cos(glons(i,j)-epslon)
     y2=r0 *sin(glons(i,j)-epslon)
     denom=one/sqrt(((x1-x0)**2+(y1-y0)**2)*((x2-x0)**2+(y2-y0)**2))
     cosalpha=((x2-x0)*(x1-x0)+(y2-y0)*(y1-y0))*denom
     sinalpha=((x2-x0)*(y1-y0)-(y2-y0)*(x1-x0))*denom
     beta_ref(i,j)=atan2(sinalpha,cosalpha)-sign_pole*glons(i,j)
     cos_beta_ref(i,j)=cos(beta_ref(i,j))
     sin_beta_ref(i,j)=sin(beta_ref(i,j))
  end do
!mhu  beta_diff_max=-rbig
!mhu  beta_diff_max_gt_20=-rbig
!mhu  beta_diff_min= rbig
!mhu  beta_diff_rms=zero
!mhu  count_beta_diff=zero
!mhu  count_beta_diff_gt_20=zero

end subroutine init_general_transform

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  tll2xy --- convert earth lon-lat to x-y grid coordinates
!
! !INTERFACE:
!
  subroutine tll2xy(this,rlonin,rlatin,xout,yout)

! !USES:

    implicit none
    class(map_util) :: this

    real(r_single),intent(in   ) :: rlonin  ! earth longitude (degree)
    real(r_single),intent(in   ) :: rlatin  ! earth latitude  (degree)

    real(r_kind) :: rlon  ! earth longitude (radians)
    real(r_kind) :: rlat  ! earth latitude  (radians)

! !OUTPUT PARAMETERS:

    real(r_single),intent(  out) :: xout  ! x-grid coordinate (grid units)
    real(r_single),intent(  out) :: yout  ! y-grid coordinate (grid units)

    real(r_kind) :: x  ! x-grid coordinate (grid units)
    real(r_kind) :: y  ! y-grid coordinate (grid units)

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a 
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) clon,slon,r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde,detinv
    integer(i_kind) itilde,jtilde
    integer(i_kind) i0,j0,ip,jp

!   first compute xtilde, ytilde
    rlon=rlonin*deg2rad
    rlat=rlatin*deg2rad

    clon=cos(rlon+rlambda0)
    slon=sin(rlon+rlambda0)
    r_of_lat=pihalf+sign_pole*rlat

    xtilde=atilde_x+btilde_x*r_of_lat*clon
    ytilde=atilde_y+btilde_y*r_of_lat*slon

!  next get interpolation information

    itilde=max(ione,min(nint(xtilde),nxtilde))
    jtilde=max(ione,min(nint(ytilde),nytilde))

    i0     =   i0_tilde(itilde,jtilde)
    j0     =   j0_tilde(itilde,jtilde)
    ip     =i0+ip_tilde(itilde,jtilde)
    jp     =j0+jp_tilde(itilde,jtilde)
    dtilde =xtilde-xtilde0(i0,j0)
    etilde =ytilde-ytilde0(i0,j0)
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    detinv =one/(d1tilde*e2tilde-d2tilde*e1tilde)
    x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde)
    y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde)

    xout=x
    yout=y

 end subroutine tll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  txy2ll ---  convert x-y grid units to earth lat-lon coordinates
!
! !INTERFACE:
!
  subroutine txy2ll(this,xin,yin,rlonout,rlatout)

! !USES:

    implicit none
    class(map_util) :: this

! !INPUT PARAMETERS:

    real(r_single),intent(in   ) :: xin      ! x-grid coordinate (grid units)
    real(r_single),intent(in   ) :: yin      ! y_grid coordinate (grid units)

    real(r_kind) :: x      ! x-grid coordinate (grid units)
    real(r_kind) :: y      ! y_grid coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_single),intent(  out) :: rlonout   ! earth longitude (degree)
    real(r_single),intent(  out) :: rlatout   ! earth latitude  (degree)

    real(r_kind) :: rlon   ! earth longitude (radians)
    real(r_kind) :: rlat   ! earth latitude  (radians)

! !DESCRIPTION: to convert earth lon-lat to x-y grid units of a
!           general regional rectangular domain.  Also, decide if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! !REVISION HISTORY:
!   2003-08-28  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2004-07-23  parrish - new routine
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-08-28
!
!EOP
!-------------------------------------------------------------------------

    real(r_kind) r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde,xbar,ybar
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde
    integer(i_kind) i0,j0,ip,jp
    integer(i_kind) nlon,nlat

    nlon=this%nlon
    nlat=this%nlat
    x=xin
    y=yin

    i0=nint(x)
    j0=nint(y)
    i0=max(ione,min(i0,nlon))
    j0=max(ione,min(j0,nlat))
    ip=i0+nint(sign(one,x-i0))
    jp=j0+nint(sign(one,y-j0))
    if(ip<ione) then
       i0=2_i_kind
       ip=ione
    end if
    if(jp<ione) then
       j0=2_i_kind
       jp=ione
    end if
    if(ip>nlon) then
       i0=nlon-ione
       ip=nlon
    end if
    if(jp>nlat) then
       j0=nlat-ione
       jp=nlat
    end if
    d1tilde=(xtilde0(ip,j0)-xtilde0(i0,j0))*(ip-i0)
    d2tilde=(xtilde0(i0,jp)-xtilde0(i0,j0))*(jp-j0)
    e1tilde=(ytilde0(ip,j0)-ytilde0(i0,j0))*(ip-i0)
    e2tilde=(ytilde0(i0,jp)-ytilde0(i0,j0))*(jp-j0)
    dtilde =d1tilde*(x-i0) +d2tilde*(y-j0)
    etilde =e1tilde*(x-i0) +e2tilde*(y-j0)
    xtilde =dtilde         +xtilde0(i0,j0)
    ytilde =etilde         +ytilde0(i0,j0)

    xbar=(xtilde-atilde_x)*btilde_xinv
    ybar=(ytilde-atilde_y)*btilde_yinv
    r_of_lat=sqrt(xbar**2+ybar**2)
    rlat=(r_of_lat-pihalf)*sign_pole
    rlon=atan2(ybar,xbar)-rlambda0

    rlatout=rlat*rad2deg
    rlonout=rlon*rad2deg

 end subroutine txy2ll

 subroutine nearest_3(ilast,jlast,i0,j0,ip,jp,x,y,nx0,ny0,x0,y0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nearest_3
!   prgmmr:
!
! abstract: find closest 3 points to (x,y) on grid defined by x0,y0
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ilast,jlast
!    nx0,ny0
!    x,y
!    x0,y0
!
!   output argument list:
!    ilast,jlast
!    i0,j0
!    ip,jp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(inout) :: ilast,jlast
  integer(i_kind),intent(  out) :: i0,j0
  integer(i_byte),intent(  out) :: ip,jp
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: x,y
  real(r_kind)   ,intent(in   ) :: x0(nx0,ny0),y0(nx0,ny0)
 
  real(r_kind) dista,distb,dist2,dist2min
  integer(i_kind) i,inext,j,jnext

  do
     i0=ilast
     j0=jlast
     dist2min=huge(dist2min)
     inext=izero
     jnext=izero
     do j=max(j0-ione,ione),min(j0+ione,ny0)
        do i=max(i0-ione,ione),min(i0+ione,nx0)
           dist2=(x-x0(i,j))**2+(y-y0(i,j))**2
           if(dist2<dist2min) then
              dist2min=dist2
              inext=i
              jnext=j
           end if
        end do
     end do
     if(inext==i0.and.jnext==j0) exit
     ilast=inext
     jlast=jnext
  end do

!  now find which way to go in x for second point

  ip=izero
  if(i0==nx0)  ip=-ione
  if(i0==ione) ip=ione
  if(ip==izero) then
     dista=(x-x0(i0-ione,j0))**2+(y-y0(i0-ione,j0))**2
     distb=(x-x0(i0+ione,j0))**2+(y-y0(i0+ione,j0))**2
     if(distb<dista) then
        ip=ione
     else
        ip=-ione
     end if
  end if

!  repeat for y for 3rd point

  jp=izero
  if(j0==ny0  ) jp=-ione
  if(j0==ione ) jp=ione
  if(jp==izero) then
     dista=(x-x0(i0,j0-ione))**2+(y-y0(i0,j0-ione))**2
     distb=(x-x0(i0,j0+ione))**2+(y-y0(i0,j0+ione))**2
     if(distb<dista) then
        jp=ione
     else
        jp=-ione
     end if
  end if

  ilast=i0
  jlast=j0
    
 end subroutine nearest_3

 subroutine get_xytilde_domain(nx0,ny0,rlons0,rlats0, &
                                  nx,ny,xminout,xmaxout,yminout,ymaxout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_xytilde_domain
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx0,ny0
!    rlons0,rlats0
!
!   output argument list:
!    nx,ny
!    xminout,xmaxout,yminout,ymaxout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!  define parameters for xy domain which optimally overlays input grid

  implicit none

  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: rlons0(nx0,ny0),rlats0(nx0,ny0)

  integer(i_kind),intent(  out) :: nx,ny
  real(r_kind)   ,intent(  out) :: xminout,xmaxout,yminout,ymaxout

  real(r_kind),parameter:: r10=10.0_r_kind
  real(r_kind),parameter:: r37=37.0_r_kind

  real(r_kind) area,areamax,areamin,extra,rlats0max,rlats0min,testlambda
  real(r_kind) xthis,ythis
  integer(i_kind) i,ip1,j,jp1,m

  real(r_kind) coslon0(nx0,ny0),sinlon0(nx0,ny0)
  real(r_kind) coslat0(nx0,ny0),sinlat0(nx0,ny0)
  real(r_kind) count,delbar
  real(r_kind) dx,dy,disti,distj,distmin,distmax
  real(r_kind) xmin,xmax,ymin,ymax

!  get range of lats for input grid

  rlats0max=maxval(rlats0) ; rlats0min=minval(rlats0)

!   assign hemisphere ( parameter sign_pole )

  if(rlats0min>-r37*deg2rad) sign_pole=-one   !  northern hemisphere xy domain
  if(rlats0max< r37*deg2rad) sign_pole= one   !  southern hemisphere xy domain

!   get optimum rotation angle rlambda0

  areamin= huge(areamin)
  areamax=-huge(areamax)
  do m=0,359
     testlambda=m*deg2rad
     xmax=-huge(xmax)
     xmin= huge(xmin)
     ymax=-huge(ymax)
     ymin= huge(ymin)
     do j=1,ny0,ny0-ione
        do i=1,nx0
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     do j=1,ny0
        do i=1,nx0,nx0-ione
           xthis=(pihalf+sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(pihalf+sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     area=(xmax-xmin)*(ymax-ymin)
     areamax=max(area,areamax)
     if(area<areamin) then
        areamin =area
        rlambda0=testlambda
        xmaxout =xmax
        xminout =xmin
        ymaxout =ymax
        yminout =ymin
     end if
  end do


!   now determine resolution of input grid and choose nx,ny of xy grid accordingly
!                 (currently hard-wired at 1/2 the average input grid increment)

  do j=1,ny0
     do i=1,nx0
        coslon0(i,j)=cos(one*rlons0(i,j)) ; sinlon0(i,j)=sin(one*rlons0(i,j))
        coslat0(i,j)=cos(one*rlats0(i,j)) ; sinlat0(i,j)=sin(one*rlats0(i,j))
     end do
  end do

  delbar=zero
  count =zero
  do j=1,ny0-ione
     jp1=j+ione
     do i=1,nx0-ione
        ip1=i+ione
        disti=acos(sinlat0(i,j)*sinlat0(ip1,j)+coslat0(i,j)*coslat0(ip1,j)* &
                  (sinlon0(i,j)*sinlon0(ip1,j)+coslon0(i,j)*coslon0(ip1,j)))
        distj=acos(sinlat0(i,j)*sinlat0(i,jp1)+coslat0(i,j)*coslat0(i,jp1)* &
                  (sinlon0(i,j)*sinlon0(i,jp1)+coslon0(i,j)*coslon0(i,jp1)))
        distmax=max(disti,distj)
        distmin=min(disti,distj)
        delbar=delbar+distmax
        count=count+one
     end do
  end do
  delbar=delbar/count
  dx=half*delbar
  dy=dx

!   add extra space to computational grid to push any boundary problems away from
!     area of interest

  extra=r10*dx
  xmaxout=xmaxout+extra
  xminout=xminout-extra
  ymaxout=ymaxout+extra
  yminout=yminout-extra
  nx=ione+(xmaxout-xminout)/dx
  ny=ione+(ymaxout-yminout)/dy
 
 end subroutine get_xytilde_domain

 subroutine destory_general_transform(this)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_general_transform
!   prgmmr:
!
! abstract:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  class(map_util) :: this

  deallocate(i0_tilde,j0_tilde)
  deallocate(ip_tilde,jp_tilde)
  deallocate(xtilde0,ytilde0)
  deallocate(beta_ref,cos_beta_ref,sin_beta_ref)
end subroutine destory_general_transform

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_ll2xy ---  Rotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_ll2xy(this,u0in,v0in,uout,vout,rlon0in,xin,yin)

! !USES:

    implicit none
    class(map_util) :: this

! !INPUT PARAMETERS:

    real(r_single),intent(in   ) :: u0in,v0in        ! earth wind component
    real(r_single),intent(in   ) :: rlon0in        ! earth   lon (degree)
    real(r_single),intent(in   ) :: xin,yin          ! local x,y coordinate (grid units) 

    real(r_kind) :: u0,v0        ! earth wind component
    real(r_kind) :: rlon0        ! earth   lon (radians)
    real(r_kind) :: x,y          ! local x,y coordinate (grid units) 

! !OUTPUT PARAMETERS:

    real(r_single),intent(  out) :: uout,vout          ! rotated coordinate of winds
    real(r_kind) :: u,v          ! rotated coordinate of winds

! !DESCRIPTION: to convert earth vector wind components to corresponding
!           local x,y coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to
!   more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy
  integer(i_kind) nlon,nlat

!  interpolate departure from longitude part of angle between earth positive
!  east and local positive x

  u0=u0in
  v0=v0in
  x=xin
  y=yin
  rlon0=rlon0in*deg2rad

  nlon=this%nlon
  nlat=this%nlat

  ix=x
  iy=y
  ix=max(1,min(ix,nlon-1))
  iy=max(1,min(iy,nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=cos_beta_ref(ix  ,iy  )*delxp*delyp+cos_beta_ref(ix+1,iy  )*delx*delyp+ &
           cos_beta_ref(ix  ,iy+1)*delxp*dely +cos_beta_ref(ix+1,iy+1)*delx*dely  
  sin_beta=sin_beta_ref(ix  ,iy  )*delxp*delyp+sin_beta_ref(ix+1,iy  )*delx*delyp+ &
           sin_beta_ref(ix  ,iy+1)*delxp*dely +sin_beta_ref(ix+1,iy+1)*delx*dely  
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u= u0*cos(beta-rlon0)+v0*sin(beta-rlon0)
  v=-u0*sin(beta-rlon0)+v0*cos(beta-rlon0)

  uout=u
  vout=v

 end subroutine rotate_wind_ll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_xy2ll ---  Unrotate earth vector wind
!
! !INTERFACE:
!
  subroutine rotate_wind_xy2ll(this,uin,vin,u0out,v0out,rlon0in,xin,yin)

! !USES:

    implicit none
    class(map_util) :: this

! !INPUT PARAMETERS:

    real(r_single),intent(in   ) :: uin,vin         ! rotated coordinate winds
    real(r_single),intent(in   ) :: rlon0in       ! earth   lon     (degree)
    real(r_single),intent(in   ) :: xin,yin         ! rotated lon/lat (radians)

    real(r_kind) :: u,v         ! rotated coordinate winds
    real(r_kind) :: rlon0       ! earth   lon     (radians)
    real(r_kind) :: x,y         ! rotated lon/lat (radians)

! !OUTPUT PARAMETERS:

    real(r_single),intent(  out) :: u0out,v0out       ! earth winds

    real(r_kind) :: u0,v0       ! earth winds

! !DESCRIPTION: rotate u,v in local x,y coordinate to u0,v0 in earth 
!           lat, lon coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to
!   more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------
  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy
  integer(i_kind) nlon,nlat

!  interpolate departure from longitude part of angle between earth 
!  positive east and local positive x

  u=uin
  v=vin
  x=xin
  y=yin
  rlon0=rlon0in*deg2rad

  nlon=this%nlon
  nlat=this%nlat

  ix=x
  iy=y
  ix=max(1,min(ix,nlon-1))
  iy=max(1,min(iy,nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=cos_beta_ref(ix  ,iy  )*delxp*delyp+cos_beta_ref(ix+1,iy  )*delx*delyp+ & 
           cos_beta_ref(ix  ,iy+1)*delxp*dely +cos_beta_ref(ix+1,iy+1)*delx*dely     
  sin_beta=sin_beta_ref(ix  ,iy  )*delxp*delyp+sin_beta_ref(ix+1,iy  )*delx*delyp+ & 
           sin_beta_ref(ix  ,iy+1)*delxp*dely +sin_beta_ref(ix+1,iy+1)*delx*dely     
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u0= u*cos(beta-rlon0)-v*sin(beta-rlon0)
  v0= u*sin(beta-rlon0)+v*cos(beta-rlon0)

  u0out=u0
  v0out=v0

 end subroutine rotate_wind_xy2ll

end module module_map_utils
