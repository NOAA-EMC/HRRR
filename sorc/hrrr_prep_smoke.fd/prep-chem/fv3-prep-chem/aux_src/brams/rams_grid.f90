!
! Copyright (C) 1991-2004  ; All Rights Reserved ; Colorado State University
! Colorado State University Research Foundation ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################

subroutine grid_setup(num)

use mem_grid

implicit none

integer :: num

integer :: ifm,ifileok

if (num == 1) then

   call gridinit
   call gridset(1)
   
else  

   do ifm = 1,ngrids
      call newgrid(ifm)

      if (ihtran <= 1) then

         call polarst(nnxp(ifm),nnyp(ifm)                      &
            ,grid_g(ifm)%glat  (1,1) ,grid_g(ifm)%glon  (1,1)  &
            ,grid_g(ifm)%fmapu (1,1) ,grid_g(ifm)%fmapv (1,1)  &
            ,grid_g(ifm)%fmapt (1,1) ,grid_g(ifm)%fmapm (1,1)  &
            ,grid_g(ifm)%fmapui(1,1) ,grid_g(ifm)%fmapvi(1,1)  &
            ,grid_g(ifm)%fmapti(1,1) ,grid_g(ifm)%fmapmi(1,1)  )

         call grdspc(nnxp(ifm),nnyp(ifm)                      &
            ,grid_g(ifm)%dxu  (1,1)  ,grid_g(ifm)%dxv  (1,1)  &
            ,grid_g(ifm)%dxt  (1,1)  ,grid_g(ifm)%dxm  (1,1)  &
            ,grid_g(ifm)%dyu  (1,1)  ,grid_g(ifm)%dyv  (1,1)  &
            ,grid_g(ifm)%dyt  (1,1)  ,grid_g(ifm)%dym  (1,1)  &
            ,grid_g(ifm)%fmapu(1,1)  ,grid_g(ifm)%fmapv(1,1)  &
            ,grid_g(ifm)%fmapt(1,1)  ,grid_g(ifm)%fmapm(1,1)  )
      
      elseif (ihtran == 2) then
     
         call grdspc_lc(nnxp(ifm),nnyp(ifm)                    &
            ,grid_g(ifm)%dxu   (1,1) ,grid_g(ifm)%dxv   (1,1)  &
            ,grid_g(ifm)%dxt   (1,1) ,grid_g(ifm)%dxm   (1,1)  &
            ,grid_g(ifm)%dyu   (1,1) ,grid_g(ifm)%dyv   (1,1)  &
            ,grid_g(ifm)%dyt   (1,1) ,grid_g(ifm)%dym   (1,1)  &
            ,grid_g(ifm)%fmapu (1,1) ,grid_g(ifm)%fmapv (1,1)  &
            ,grid_g(ifm)%fmapt (1,1) ,grid_g(ifm)%fmapm (1,1)  &
            ,grid_g(ifm)%fmapui(1,1) ,grid_g(ifm)%fmapvi(1,1)  &
            ,grid_g(ifm)%fmapti(1,1) ,grid_g(ifm)%fmapmi(1,1)  &
            ,grid_g(ifm)%glat  (1,1) ,grid_g(ifm)%glon  (1,1)  )

      elseif (ihtran == 3) then 
         call grdspc_mc(nnxp(ifm),nnyp(ifm)                    &
            ,grid_g(ifm)%glat(1,1) ,grid_g(ifm)%glon(1,1)      &
	    ,grid_g(ifm)%dxt (1,1) ,grid_g(ifm)%dyt (1,1)      )
      
      
      else
         print*,'Unknown IHTRAN value:',ihtran
         stop 'grid_setup: bad IHTRAN'
        
      endif

     if (ihtran == 3) goto 1000   ! no transformation for Mercator projection
! Define transformation Jacobians for all grids

      call fill_toptuvm(nnxp(ifm),nnyp(ifm)               &
         ,grid_g(ifm)%topt (1,1) ,grid_g(ifm)%topu (1,1)  &
         ,grid_g(ifm)%topv (1,1) ,grid_g(ifm)%topm (1,1)  &
         ,grid_g(ifm)%topta(1,1) ,grid_g(ifm)%topma(1,1)  )

      call transfm(nnxp(ifm),nnyp(ifm)                  &
         ,grid_g(ifm)%topt(1,1) ,grid_g(ifm)%topu(1,1)  &
         ,grid_g(ifm)%topv(1,1) ,grid_g(ifm)%topm(1,1)  &
         ,grid_g(ifm)%rtgt(1,1) ,grid_g(ifm)%rtgu(1,1)  &
         ,grid_g(ifm)%rtgv(1,1) ,grid_g(ifm)%rtgm(1,1)  &
         ,grid_g(ifm)%f13u(1,1) ,grid_g(ifm)%f13v(1,1)  &
         ,grid_g(ifm)%f13t(1,1) ,grid_g(ifm)%f13m(1,1)  &
         ,grid_g(ifm)%f23u(1,1) ,grid_g(ifm)%f23v(1,1)  &
         ,grid_g(ifm)%f23t(1,1) ,grid_g(ifm)%f23m(1,1)  &
         ,grid_g(ifm)%dxu (1,1) ,grid_g(ifm)%dxv (1,1)  &
         ,grid_g(ifm)%dxt (1,1) ,grid_g(ifm)%dxm (1,1)  &
         ,grid_g(ifm)%dyu (1,1) ,grid_g(ifm)%dyv (1,1)  &
         ,grid_g(ifm)%dyt (1,1) ,grid_g(ifm)%dym (1,1)  )

      call lpuvw_init(nnxp(ifm),nnyp(ifm),grid_g(ifm)%lpu(1,1)  &
                    ,grid_g(ifm)%lpv(1,1),grid_g(ifm)%lpw(1,1)  )

      if (if_adap == 1) then
         call ctrlvols (nnzp(ifm),nnxp(ifm),nnyp(ifm),nnstbot(ifm)  &
            ,dztn(1,ifm),xmn(1,ifm),ymn(1,ifm),zmn(1,ifm)           &
            ,platn(ifm),plonn(ifm)                                  &
            ,grid_g(ifm)%aru  (1,1,1) ,grid_g(ifm)%arv (1,1,1)      &
            ,grid_g(ifm)%arw  (1,1,1) ,grid_g(ifm)%volt(1,1,1)      &
            ,grid_g(ifm)%volu (1,1,1) ,grid_g(ifm)%volv(1,1,1)      &
            ,grid_g(ifm)%volw (1,1,1) ,grid_g(ifm)%lpu (1,1)        &
            ,grid_g(ifm)%lpv  (1,1)   ,grid_g(ifm)%lpw (1,1)        &
            ,grid_g(ifm)%dxu  (1,1)   ,grid_g(ifm)%dxv (1,1)        &
            ,grid_g(ifm)%dxt  (1,1)   ,grid_g(ifm)%dyu (1,1)        &
            ,grid_g(ifm)%dyv  (1,1)   ,grid_g(ifm)%dyt (1,1)        &
            ,grid_g(ifm)%topma(1,1)   ,grid_g(ifm)%topm(1,1)        &
            ,ifm, nzg, npatch, stdlat2  )
      endif
      
1000 continue

   enddo
endif
return
end

!     ******************************************************************

subroutine gridinit

use mem_grid

implicit none

!     +----------------------------------------------------------------
!     !    initialize the domain sizes for all nest levels.
!     +----------------------------------------------------------------
!
!          grid sizes:  the user has specified all nnxp, nnyp, nnzp,
!                       nzg, nzs, and npatch.  fill other arrays that are a
!                       function of these.

jdim = 1

if (nnyp(1) == 1) jdim = 0
do ngrid = 1,ngrids

!         x - direction

   nnx(ngrid) = nnxp(ngrid) - 1
   nnx1(ngrid) = nnxp(ngrid) - 2
   nnx2(ngrid) = nnxp(ngrid) - 3

!         y - direction

  if (jdim == 1) then
    nny(ngrid) = nnyp(ngrid) - 1
    nny1(ngrid) = nnyp(ngrid) - 2
    nny2(ngrid) = nnyp(ngrid) - 3
  else
    nny(ngrid) = 1
    nny1(ngrid) = 1
    nny2(ngrid) = 1
  endif

!         z - direction

  nnz(ngrid) = nnzp(ngrid) - 1
  nnz1(ngrid) = nnzp(ngrid) - 2

!  print *,'nnxp(ngrid),nnyp(ngrid), nnzp(ngrid)',nnxp(ngrid),nnyp(ngrid), nnzp(ngrid)
enddo

do ngrid = 1,ngrids
  nnxyzp(ngrid) = nnxp(ngrid) * nnyp(ngrid) * nnzp(ngrid)
  nnxysp(ngrid) = nnxp(ngrid) * nnyp(ngrid) * (nzg+nzs+3) * npatch
  nnxyp(ngrid) = nnxp(ngrid) * nnyp(ngrid)
!  print *,'nnxyzp(ngrid),nnxysp(ngrid), nnxyp(ngrid), ngrid, ngrids', nnxyzp(ngrid),nnxysp(ngrid), nnxyp(ngrid), ngrid, ngrids
enddo

return
end


!     *****************************************************************

subroutine polarst(n2,n3,glat,glon,fmapu,fmapv,fmapt,fmapm  &
                  ,fmapui,fmapvi,fmapti,fmapmi)

use mem_grid
use rconstants

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: glat,glon,fmapu,fmapv,fmapt,fmapm  &
         ,fmapui,fmapvi,fmapti,fmapmi

integer :: i,j
real :: c1,xm2,xt2,ym2,yt2
!  Calculates map factors and inverse map factors at u,v,t,m-points and
!  geographical lat/lon at t-points for a given polar stereographic grid

c1 = (2. * erad) ** 2
do j = 1,n3
   do i = 1,n2
      xm2 = xm(i) * xm(i)
      xt2 = xt(i) * xt(i)
      ym2 = ym(j) * ym(j)
      yt2 = yt(j) * yt(j)

      fmapt(i,j) = 1. + (xt2 + yt2) / c1
      fmapu(i,j) = 1. + (xm2 + yt2) / c1
      fmapv(i,j) = 1. + (xt2 + ym2) / c1
      fmapm(i,j) = 1. + (xm2 + ym2) / c1

      fmapui(i,j) = 1.0 / fmapu(i,j)
      fmapvi(i,j) = 1.0 / fmapv(i,j)
      fmapti(i,j) = 1.0 / fmapt(i,j)
      fmapmi(i,j) = 1.0 / fmapm(i,j)

      call xy_ll(glat(i,j),glon(i,j),platn(ngrid),plonn(ngrid)  &
         ,stdlat2,xt(i),yt(j))

!       write(6,344)i,j,fmapt(i,j),fmapm(i,j),glat(i,j),glon(i,j)
! 344   format('polst:i,j,fmt,fmm,glt,gln',2i4,4e12.3)

   enddo
enddo

if (ihtran == 0) then
   call ae0(n2*n3,fmapu,1.)
   call ae0(n2*n3,fmapv,1.)
   call ae0(n2*n3,fmapt,1.)
   call ae0(n2*n3,fmapm,1.)
   call ae0(n2*n3,fmapui,1.)
   call ae0(n2*n3,fmapvi,1.)
   call ae0(n2*n3,fmapti,1.)
   call ae0(n2*n3,fmapmi,1.)
endif
return
end

!     *****************************************************************

subroutine grdspc(n2,n3,dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym  &
                 ,fmapu,fmapv,fmapt,fmapm)

use mem_grid

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym  &
                         ,fmapu,fmapv,fmapt,fmapm

integer :: i,j

do j = 1,n3
   do i = 1,n2-1
      dxu(i,j) = fmapu(i,j) / (xtn(i+1,ngrid)-xtn(i,ngrid))
      dxm(i,j) = fmapm(i,j) / (xtn(i+1,ngrid)-xtn(i,ngrid))
   enddo
   dxu(nxp,j)=dxu(nx,j)*fmapu(nxp,j)/fmapu(nx,j)
   dxm(nxp,j)=dxm(nx,j)*fmapm(nxp,j)/fmapm(nx,j)
   do i = 2,n2
      dxv(i,j)=fmapv(i,j)/(xmn(i,ngrid)-xmn(i-1,ngrid))
      dxt(i,j)=fmapt(i,j)/(xmn(i,ngrid)-xmn(i-1,ngrid))
   enddo
   dxv(1,j)=dxv(2,j)*fmapv(1,j)/fmapv(2,j)
   dxt(1,j)=dxt(2,j)*fmapt(1,j)/fmapt(2,j)
enddo

if (jdim == 1) then
   do i = 1,n2
      do j = 1,n3-1
         dyv(i,j)=fmapv(i,j)/(ytn(j+1,ngrid)-ytn(j,ngrid))
         dym(i,j)=fmapm(i,j)/(ytn(j+1,ngrid)-ytn(j,ngrid))
      enddo
      dyv(i,nyp)=dyv(i,ny)*fmapv(i,nyp)/fmapv(i,ny)
      dym(i,nyp)=dym(i,ny)*fmapm(i,nyp)/fmapm(i,ny)
      do j = 2,n3
         dyu(i,j)=fmapu(i,j)/(ymn(j,ngrid)-ymn(j-1,ngrid))
         dyt(i,j)=fmapt(i,j)/(ymn(j,ngrid)-ymn(j-1,ngrid))
      enddo
      dyu(i,1)=dyu(i,2)*fmapu(i,1)/fmapu(i,2)
      dyt(i,1)=dyt(i,2)*fmapt(i,1)/fmapt(i,2)
   enddo
else
   do i=1,n2
      do j=1,n3
         dyu(i,j)=1./deltayn(ngrid)
         dyv(i,j)=1./deltayn(ngrid)
         dyt(i,j)=1./deltayn(ngrid)
         dym(i,j)=1./deltayn(ngrid)
      enddo
   enddo
endif
return
end

!**********************************************************************

subroutine grdspc_lc(n2,n3,dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym  &
   ,fmapu,fmapv,fmapt,fmapm,fmapui,fmapvi,fmapti,fmapmi,glat,glon)

use mem_grid

! *****Subroutine to compute inverse earth distances and map factors 
! on RAMS grid for Lambert conformal Conic projection with 2 different 
! or identical standard parallels

!     ***** Input global variables ******

!     ngrid   : current grid number
!     nnxp    : array dimension in x direction
!     nnyp    : array dimension in y direction
!     xmn     : RAMS grid x coordinate array for M points
!     xtn     : RAMS grid x coordinate array for T points
!     xmn     : RAMS grid y coordinate array for M points
!     ytn     : RAMS grid y coordinate array for T points
!     polelat : geographic latitude of pole or origin point
!     polelon : geographic longitude of pole or origin point
!     stdlat1 : first (southern) lcc standard latitude (deg)
!     stdlat2 : second (northern) lcc standard latitude (deg)

!     ***** Input variables ********

!     n2      : array dimension in x direction
!     n3      : array dimension in y direction

!     ***** Output variables ********

!     dxu     : 1/dx_earth centered at U point
!     dxv     : 1/dx_earth centered at V point
!     dxt     : 1/dx_earth centered at T point
!     dxm     : 1/dx_earth centered at M point
!     dyu     : 1/dy_earth centered at U point
!     dyv     : 1/dy_earth centered at V point
!     dyt     : 1/dy_earth centered at T point
!     dym     : 1/dy_earth centered at M point
!     fmapu   : map factor centered at U point
!     fmapv   : map factor centered at V point
!     fmapt   : map factor centered at T point
!     fmapm   : map factor centered at M point
!     fmapui  : inverse map factor centered at U point
!     fmapvi  : inverse map factor centered at V point
!     fmapti  : inverse map factor centered at T point
!     fmapmi  : inverse map factor centered at M point
!     glat    : latitude of T point (deg)
!     glon    : longitude of T point (deg)


!     ***** Local variables *********

!     i,j     : loop indices for x,y directions

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym  &
   ,fmapu,fmapv,fmapt,fmapm,fmapui,fmapvi,fmapti,fmapmi,glat,glon

integer :: i,j
integer :: im1,ip1,jm1,jp1
real, external :: earthdist_lc

!  Loop over all horizontal points (same for all 4 stagger locations)

do j = 1,nnyp(ngrid)
     jm1 = max(j-1,1)
     jp1 = min(j+1,nnyp(ngrid))
   do i = 1,nnxp(ngrid)
     im1 = max(i-1,1)
     ip1 = min(i+1,nnxp(ngrid))
   
   !print*,'^^^^^^^^^:',i,j,stdlat1,stdlat2,polelat,polelon

!  Call function earthdist_lc to evaluate earth distance between two
!  points whose x and y coordinates are the first 4 arguments.

!  Fill RAMS inverse distance arrays.

      dxm(i,j) = 1. / earthdist_lc(xtn(ip1,ngrid),xtn(i,ngrid)  &
         ,ymn(j,ngrid),ymn(j,ngrid),polelat,polelon,stdlat1,stdlat2)
      dxt(i,j) = 1. / earthdist_lc(xmn(i,ngrid),xmn(im1,ngrid)  &
         ,ytn(j,ngrid),ytn(j,ngrid),polelat,polelon,stdlat1,stdlat2)
      dxu(i,j) = 1. / earthdist_lc(xtn(ip1,ngrid),xtn(i,ngrid)  &
         ,ytn(j,ngrid),ytn(j,ngrid),polelat,polelon,stdlat1,stdlat2)
      dxv(i,j) = 1. / earthdist_lc(xmn(i,ngrid),xmn(im1,ngrid)  &
         ,ymn(j,ngrid),ymn(j,ngrid),polelat,polelon,stdlat1,stdlat2)
      dym(i,j) = 1. / earthdist_lc(xmn(i,ngrid),xmn(i,ngrid)  &
         ,ytn(jp1,ngrid),ytn(j,ngrid),polelat,polelon,stdlat1,stdlat2)
      dyt(i,j) = 1. / earthdist_lc(xtn(i,ngrid),xtn(i,ngrid)  &
         ,ymn(j,ngrid),ymn(jm1,ngrid),polelat,polelon,stdlat1,stdlat2)
      dyu(i,j) = 1. / earthdist_lc(xmn(i,ngrid),xmn(i,ngrid)  &
         ,ymn(j,ngrid),ymn(jm1,ngrid),polelat,polelon,stdlat1,stdlat2)
      dyv(i,j) = 1. / earthdist_lc(xtn(i,ngrid),xtn(i,ngrid)  &
         ,ytn(jp1,ngrid),ytn(j,ngrid),polelat,polelon,stdlat1,stdlat2)

!  Evaluate map factors which are Lambert conformal projection distance
!  divided by earth distance
      
      fmapm(i,j) = dxm(i,j) * (xtn(ip1,ngrid) - xtn(i,ngrid))
      fmapt(i,j) = dxt(i,j) * (xmn(i,ngrid) - xmn(im1,ngrid))
      fmapu(i,j) = dxu(i,j) * (xtn(ip1,ngrid) - xtn(i,ngrid))
      fmapv(i,j) = dxv(i,j) * (xmn(i,ngrid) - xmn(im1,ngrid))

   enddo      !tks
enddo      !tks

!tks  in the above do loops, earthdist_lc returns zero on edges of the grid (a different edge 
!depending on whether the code is using im1,ip1,jm1,jp1) causing a divide by zero  
!The following fills in these rows/columns with values from the adjacent row/column

do j=1,nnyp(ngrid)
dxt(1,j)=dxt(2,j)
dxv(1,j)=dxv(2,j)
fmapt(1,j)=fmapt(2,j)
fmapv(1,j)=fmapv(2,j)
enddo
do j=1,nnyp(ngrid)
dxm(nnxp(ngrid),j)=dxm(nnxp(ngrid)-1,j)
dxu(nnxp(ngrid),j)=dxu(nnxp(ngrid)-1,j)
fmapm(nnxp(ngrid),j)=fmapm(nnxp(ngrid)-1,j)
fmapu(nnxp(ngrid),j)=fmapu(nnxp(ngrid)-1,j)
enddo

do i=1,nnxp(ngrid)
dyu(i,1)=dyu(i,2)
dyt(i,1)=dyt(i,2)
enddo
do i=1,nnxp(ngrid)
dym(i,nnyp(ngrid))=dym(i,nnyp(ngrid)-1)
dyt(i,nnyp(ngrid))=dyt(i,nnyp(ngrid)-1)
enddo

!print*,'dxt ',maxval(dxt),minval(dxt)
!print*,'dyt ',maxval(dyt),minval(dyt)

do j = 1,nnyp(ngrid)
   do i = 1,nnxp(ngrid)
!  Evaluate inverse map factors
      
      fmapmi(i,j) = 1. / fmapm(i,j)
      fmapti(i,j) = 1. / fmapt(i,j)
      fmapui(i,j) = 1. / fmapu(i,j)
      fmapvi(i,j) = 1. / fmapv(i,j)

!  Call subroutine lc_ll to obtain latitude and longitude of T points

      call lc_ll2(glat(i,j),glon(i,j),polelat,polelon,stdlat1,stdlat2  &
         ,xtn(i,ngrid),ytn(j,ngrid))
!      print*,'lambert=> glat glon=',glat(i,j), glon(i,j)!, mapfac_arr_y
      
   enddo
enddo
!tks

return
end

!**********************************************************************

subroutine grdspc_mc(n2,n3,glat,glon,dxt,dyt)

use mem_grid, only: ngrid,stdlat1,deltax,polelat,polelon,nnxp,nnyp, &
                    deltaxn,deltayn

! *****Subroutine to compute grid map for Mercator projection 

!     ***** Input global variables ******

!     ngrid   : current grid number
!     nnxp    : array dimension in x direction
!     nnyp    : array dimension in y direction
!     polelat : geographic latitude of pole or origin point (center point)
!     polelon : geographic longitude of pole or origin point
!     stdlat1 : trule lat1 

!     ***** Input variables ********

!     n2      : array dimension in x direction
!     n3      : array dimension in y direction

!     ***** Output variables ********
!     glat    : latitude of T point (deg)
!     glon    : longitude of T point (deg)


!     ***** Local variables *********

!     i,j     : loop indices for x,y directions

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: glat,glon,dxt,dyt
real      :: clain
real, parameter :: PI = 3.141592653589793
real,parameter :: rad_per_deg = pi/180 
real, parameter :: DEG_PER_RAD = 180./PI
real, parameter :: EARTH_RADIUS_M = 6370000.   ! meter 
real dlon,rsw
real :: colat0,colat,mapfac_arr_x,mapfac_arr_y

integer :: i,j,knowni,knownj
      knowni=nnxp(ngrid)/2.0
      knownj=nnyp(ngrid)/2.0
! stdlat1= First true latitude
! polelat= Latitude of grid origin point (i,j)=(1,1)
! polelon= Longitude of grid origin
print*,'mercator parameters ',stdlat1,deltax,polelat,polelon
do j = 1,nnyp(ngrid)
   do i = 1,nnxp(ngrid)

      !  Preliminary variables
      clain = COS(rad_per_deg*stdlat1)
      dlon = deltaxn(ngrid) / (EARTH_RADIUS_M * clain)

      rsw = 0.
      IF (polelat .NE. 0.) THEN
         rsw = (ALOG(TAN(0.5*((polelat+90.)*rad_per_deg))))/dlon
      ENDIF

      glat(i,j) = 2.0*ATAN(EXP(dlon*(rsw + j-knownj)))*deg_per_rad - 90.
      glon(i,j) = (i-knowni)*dlon*deg_per_rad + polelon
      IF (glon(i,j).GT.180.) glon(i,j) = glon(i,j) - 360.
      IF (glon(i,j).LT.-180.) glon(i,j) = glon(i,j) + 360.

      colat0 = rad_per_deg*(90.0 - stdlat1)
      colat  = rad_per_deg*(90.0 - glat(i,j))
      mapfac_arr_x = sin(colat0) / sin(colat) 
      mapfac_arr_y = mapfac_arr_x 

      dxt(i,j) = 1./(deltaxn(ngrid)/mapfac_arr_x) ! actually dxt=1/deltax
      dyt(i,j) = 1./(deltayn(ngrid)/mapfac_arr_y) ! actually dyt=1/deltay
!      print*,'mercator=> dxt dyt mapfac_arr_y=',dxt(i,j), dyt(i,j), mapfac_arr_y
!      print*,'mercator=> glat glon=',glat(i,j), glon(i,j)!, mapfac_arr_y
   enddo
enddo
return
end
!***************************************************************************
   SUBROUTINE llij_merc(lat, lon,polelat,polelon,stdlat1, i, j)
      use mem_grid, only: deltaxn,deltayn,ngrid,nnxp,nnyp

      ! Compute i/j coordinate from lat lon for mercator projection
    
      IMPLICIT NONE
      REAL, INTENT(IN)              :: lat,polelat,polelon,stdlat1
      REAL, INTENT(IN)              :: lon
!
      REAL,INTENT(OUT)              :: i
      REAL,INTENT(OUT)              :: j
      REAL                          :: deltalon,rsw,clain,dlon
      real, parameter :: PI = 3.141592653589793
      real,parameter :: rad_per_deg = pi/180 
      real, parameter :: DEG_PER_RAD = 180./PI
      real, parameter :: EARTH_RADIUS_M = 6370000.   ! meter 
      integer :: knowni,knownj

      !  Preliminary variables
      knowni=nnxp(ngrid)/2.0
      knownj=nnyp(ngrid)/2.0
      clain = COS(rad_per_deg*stdlat1)
      dlon = deltaxn(ngrid) / (EARTH_RADIUS_M * clain)
      rsw = 0.
      IF (polelat .NE. 0.) THEN
         rsw = (ALOG(TAN(0.5*((polelat+90.)*rad_per_deg))))/dlon
      ENDIF
      deltalon = lon - polelon
      IF (deltalon .LT. -180.) deltalon = deltalon + 360.
      IF (deltalon .GT. 180.) deltalon = deltalon - 360.
      
      i = knowni + (deltalon/(dlon*deg_per_rad))
      
      j = knownj + (ALOG(TAN(0.5*((lat + 90.) * rad_per_deg)))) / &
             dlon - rsw
  
      i=(i-knowni)*deltaxn(ngrid)
      j=(j-knownj)*deltayn(ngrid)
  
   END SUBROUTINE llij_merc
! **************************************************************************
!   SUBROUTINE ijll_merc(i, j, proj, lat, lon)
! 
!      ! Compute the lat/lon from i/j for mercator projection
!  
!      IMPLICIT NONE
!      REAL,INTENT(IN)               :: i
!      REAL,INTENT(IN)               :: j    
!      TYPE(proj_info),INTENT(IN)    :: proj
!      REAL, INTENT(OUT)             :: lat
!      REAL, INTENT(OUT)             :: lon 
!  
!  
!      lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + j-proj%knownj)))*deg_per_rad - 90.
!      lon = (i-proj%knowni)*proj%dlon*deg_per_rad + proj%lon1
!      IF (lon.GT.180.) lon = lon - 360.
!      IF (lon.LT.-180.) lon = lon + 360.
!      RETURN
!
!   END SUBROUTINE ijll_merc
!***************************************************************************
! **************************************************************************

subroutine fill_toptuvm(n2,n3,topt,topu,topv,topm,topta,topma)

use mem_grid

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: topt,topu,topv,topm,topta,topma

integer :: i,j
real :: terdev

do j = 1,n3
   do i = 1,n2
      topt(i,j) = topta(i,j)
   enddo
enddo

terdev = 0.
do j = 1,n3
   do i = 1,n2-1
      topu(i,j) = topt(i,j) + (topt(i+1,j) - topt(i,j))  &
                * (xm(i) - xt(i)) / (xt(i+1) - xt(i))
      terdev = max(terdev,abs(topt(i,j)))
   enddo
   topu(n2,j) = topt(n2,j) + (topt(n2,j) - topt(n2-1,j))  &
               * (xm(n2) - xt(n2)) / (xt(n2) - xt(n2-1))
enddo

if (terdev < 1.e-6) then
   itopo = 0
else
   itopo = 1
endif

if (jdim == 1) then
   do i = 1,n2
      do j = 1,n3-1
         topv(i,j) = topt(i,j) + (topt(i,j+1) - topt(i,j))  &
                   * (ym(j) - yt(j)) / (yt(j+1) - yt(j))
         topm(i,j) = topu(i,j) + (topu(i,j+1) - topu(i,j))  &
                   * (ym(j) - yt(j)) / (yt(j+1) - yt(j))
      enddo
      topv(i,n3) = topt(i,n3) + (topt(i,n3) - topt(i,n3-1))  &
                  * (ym(n3) - yt(n3)) / (yt(n3) - yt(n3-1))
      topm(i,n3) = topu(i,n3) + (topu(i,n3) - topu(i,n3-1))  &
                  * (ym(n3) - yt(n3)) / (yt(n3) - yt(n3-1))
   enddo
else
   do j = 1,n3
      do i = 1,n2
         topv(i,j) = topt(i,j)
         topm(i,j) = topu(i,j)
      enddo
   enddo
endif

do j = 1,n3
   do i = 1,n2
      topma(i,j) = topm(i,j)
   enddo
enddo

if (if_adap == 1) then
   do j = 1,n3
      do i = 1,n2
         topt(i,j) = 0.
         topu(i,j) = 0.
         topv(i,j) = 0.
         topm(i,j) = 0.
      enddo
   enddo
endif

return
end

! ************************************************************************

subroutine transfm(n2,n3,topt,topu,topv,topm,rtgt,rtgu,rtgv,rtgm  &
                  ,f13u,f13v,f13t,f13m,f23u,f23v,f23t,f23m  &
                  ,dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym)

use mem_grid

implicit none

integer :: n2,n3
real, dimension(n2,n3) :: topt,topu,topv,topm,rtgt,rtgu,rtgv,rtgm  &
         ,f13u,f13v,f13t,f13m,f23u,f23v,f23t,f23m  &
         ,dxu,dxv,dxt,dxm,dyu,dyv,dyt,dym

integer :: iztflag=0,i,j,k

!     this routine computes the coordinate transformation constants
!     based on the topographical values of TOPT.

ztop =max(1., zmn(max(1,nnzp(1)-1),1))
do k = 1,nzp
  htn(k,ngrid) = zt(k) / ztop - 1.
  hwn(k,ngrid) = zm(k) / ztop - 1.
enddo
do k = 1,nzp
  ht2n(k,ngrid) = .5 * htn(k,ngrid)
  ht4n(k,ngrid) = .25 * htn(k,ngrid)
  hw2n(k,ngrid) = .5 * hwn(k,ngrid)
  hw4n(k,ngrid) = .25 * hwn(k,ngrid)
enddo
do k = 1,nzp
  ht(k)  = htn(k,ngrid)
  hw(k)  = hwn(k,ngrid)
  ht2(k) = ht2n(k,ngrid)
  ht4(k) = ht4n(k,ngrid)
  hw2(k) = hw2n(k,ngrid)
  hw4(k) = hw4n(k,ngrid)
enddo

do j = 1,n3
   do i = 1,n2
      rtgt(i,j) = 1. - topt(i,j) / ztop
      rtgu(i,j) = 1. - topu(i,j) / ztop
      rtgv(i,j) = 1. - topv(i,j) / ztop
      rtgm(i,j) = 1. - topm(i,j) / ztop
      if (topt(i,j) > .5 * ztop) then
         print*, 'Terrain height is over half the model domain'
         print*, 'height.  Model will stop here to avoid this.'
         print*, 'ngrid, i, j, topt, ztop = ',ngrid,i,j,topt(i,j),ztop
         iztflag = 1
      endif
   enddo
enddo

if (iztflag == 1) stop 'topt/ztop'

do j = 1,n3
   do i = 2,n2
      f13t(i,j) = (topu(i,j) - topu(i-1,j)) * dxt(i,j) / rtgt(i,j)
      f13v(i,j) = (topm(i,j) - topm(i-1,j)) * dxv(i,j) / rtgv(i,j)
   enddo
   do i = 1,n2-1
      f13u(i,j) = (topt(i+1,j) - topt(i,j)) * dxu(i,j) / rtgu(i,j)
      f13m(i,j) = (topv(i+1,j) - topv(i,j)) * dxm(i,j) / rtgm(i,j)
   enddo
   f13t(1,j)  = f13u(1,j)
   f13v(1,j)  = f13m(1,j)
   f13u(n2,j) = f13t(n2,j)
   f13m(n2,j) = f13v(n2,j)
enddo

do i = 1,n2
   do j = 2,n3
      f23t(i,j) = (topv(i,j) - topv(i,j-jdim)) * dyt(i,j) / rtgt(i,j)
      f23u(i,j) = (topm(i,j) - topm(i,j-jdim)) * dyu(i,j) / rtgu(i,j)
   enddo
   do j = 1,n3-1
      f23v(i,j) = (topt(i,j+jdim) - topt(i,j)) * dyv(i,j) / rtgv(i,j)
      f23m(i,j) = (topu(i,j+jdim) - topu(i,j)) * dym(i,j) / rtgm(i,j)
   enddo
   if (jdim == 1) then
      f23t(i,1)  = f23v(i,1)
      f23u(i,1)  = f23m(i,1)
      f23v(i,n3) = f23t(i,n3)
      f23m(i,n3) = f23u(i,n3)
   endif
enddo

return
end


!------------------------------------------------------------------------

subroutine newgrid(ngr)

use mem_grid
use node_mod

implicit none
integer :: ngr

integer :: i,j,k

!     +----------------------------------------------------------------
!     !    Fill the single and 1D variables that the rest of the model
!     !      uses from the nest arrays and change grid level in the I/O.
!     +----------------------------------------------------------------



ngrid = ngr

!         grid point references

!         x - direction

nxp=nnxp(ngr)
nx=nnx(ngr)
nx1=nnx1(ngr)
nx2=nnx2(ngr)
!
!         y - direction
!
nyp=nnyp(ngr)
ny=nny(ngr)
ny1=nny1(ngr)
ny2=nny2(ngr)
!
!         z - direction
!
nzp=nnzp(ngr)
nzpp=nzp+1
nz=nnz(ngr)
nz1=nnz1(ngr)


nxyzp=nnxyzp(ngr)
nxysp=nnxysp(ngr)
nxyp=nnxyp(ngr)

!          grid spacings

deltax=deltaxn(ngr)
do i=1,nxp
  xt(i)=xtn(i,ngr)
  xm(i)=xmn(i,ngr)
enddo
!
deltay=deltayn(ngr)
do j=1,nyp
  yt(j)=ytn(j,ngr)
  ym(j)=ymn(j,ngr)
enddo
!
deltaz=zmn(2,ngr)-zmn(1,ngr)
do k=1,nzp
  zt(k)=ztn(k,ngr)
  zm(k)=zmn(k,ngr)
  dzm(k)=dzmn(k,ngr)
  dzt(k)=dztn(k,ngr)
  dzm2(k)=dzm2n(k,ngr)
  dzt2(k)=dzt2n(k,ngr)
  ht(k)=htn(k,ngr)
  ht2(k)=ht2n(k,ngr)
  ht4(k)=ht4n(k,ngr)
  hw(k)=hwn(k,ngr)
  hw2(k)=hw2n(k,ngr)
  hw4(k)=hw4n(k,ngr)
enddo

!         option flags

nsttop=nnsttop(ngr)
nstbot=nnstbot(ngr)

!         timesteps

dtlt=dtlongn(ngr)
dtlv=2.*dtlt

!        node gridpoint info

mxp=mmxp(ngr)
myp=mmyp(ngr)
mzp=mmzp(ngr)
ia=mia(ngr)
iz=miz(ngr)
ja=mja(ngr)
jz=mjz(ngr)
i0=mi0(ngr)
j0=mj0(ngr)
ibcon=mibcon(ngr)

return
end
