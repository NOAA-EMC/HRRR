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
      
real function earthdist_lc(x1,x2,y1,y2,polelat,polelon,stdlat1,stdlat2)

!  Function to compute earth distance (m) between two points (x1,y1) and 
!  (x2,y2) that are defined on a Lambert conformal conic projection with
!  standard latitudes stdlat1 and stdlat2 and with origin located at 
!  geographic latitude polelat and longitude polelon.

! Input Variables:

!    x1       :  x-coordinate of first point
!    y1       :  y-coordinate of first point
!    x2       :  x-coordinate of second point
!    y2       :  y-coordinate of second point
!    polelat  :  latitude of origin in Lcc projection
!    polelon  :  longitude of origin in Lcc projection
!    stdlat1  :  southern standard latitude of Lcc projection
!    stdlat2  :  northern standard latitude of Lcc projection

! Output Variable:

!    earthdist_lc :  Earth distance (m) between first and second point

! Local Parameters:

!    nsegs     :  number of segments along line between first and second 
!                    points for numerical integration
!    radearth  :  Earth radius (m)

! Local Variables:

!    iseg      :  current segment number
!    xseg1     :  x-coordinate of beginning of current segment
!    xseg2     :  x-coordinate of end of current segment
!    yseg1     :  y-coordinate of beginning of current segment
!    yseg2     :  y-coordinate of end of current segment
!    qlat1     :  latitude of point at (xseg1,yseg1)
!    qlon1     :  longitude of point at (xseg1,yseg1)
!    qlat2     :  latitude of point at (xseg2,yseg2)
!    qlon2     :  longitude of point at (xseg2,yseg2)
!    dlat      :  difference between qlat1 and qlat2
!    dlon      :  difference between qlon1 and qlon2
!    avglat    :  average of qlat1 and qlat2


! Subroutines called:

!    lc_ll     Subroutine to compute latitude and longitude from x,y
!              coordinates in Lambert conformal conic projection

implicit none

integer, parameter :: nsegs = 10
real, parameter :: radearth = 6370000.   ! (USGS Paper 1395)

real :: x1,y1,x2,y2,polelat,polelon,stdlat1,stdlat2

integer :: iseg
real :: xseg1,xseg2,yseg1,yseg2,qlat1,qlat2,qlon1,qlon2,dlat,dlon,avglat

!  Initialize earthdist_lc to zero

earthdist_lc = 0.

!  Loop over all line segments

do iseg = 1,nsegs

!  Compute beginning and ending coordinates of current line segment

   xseg1 = x1 + (x2 - x1) * float(iseg-1) / float(nsegs)
   xseg2 = x1 + (x2 - x1) * float(iseg)   / float(nsegs)
   yseg1 = y1 + (y2 - y1) * float(iseg-1) / float(nsegs)
   yseg2 = y1 + (y2 - y1) * float(iseg)   / float(nsegs)

!  Compute latitude and longitude of both ends of current line segment

   call lc_ll2(qlat1,qlon1,polelat,polelon,stdlat1,stdlat2,xseg1,yseg1)
   call lc_ll2(qlat2,qlon2,polelat,polelon,stdlat1,stdlat2,xseg2,yseg2)

!  Compute length of current line segment and add to earthdist_lc

   dlat = qlat2 - qlat1
   dlon = qlon2 - qlon1
   avglat = .5 * (qlat1 + qlat2)
   earthdist_lc = earthdist_lc   &
      + radearth * sqrt(dlat ** 2 + (cos(avglat*3.14159/180.) * dlon) ** 2)*3.14159/180.

enddo

return
end

!*****************************************************************************

subroutine ll_lc2(qlat,qlon,polelat,polelon,stdlat1,stdlat2,x,y)

! *****Subroutine to convert latitude,longitude to x,y (relative to polelat/polelon)
! on Lambert conformal Conic grid with 2 different or identical standard parallels

!     ***** Passed variables ********

!     qlat    : latitude of input point (deg)
!     qlon    : longitude of input point (deg)
!     polelat : latitude of origin on lcc grid (deg)
!     polelon : longitude of origin on lcc grid (deg)
!     stdlat1 : first (southern) standard latitude (deg)
!     stdlat2 : second (northern) standard latitude (deg)
!     x       : x-coordinate of point on Lambert Conformal grid (m)
!     y       : y-coordinate of point on Lambert Conformal grid (m)

!     ***** Local variables *********

!     cone: cone constant
!     Ffactor: another constant
!     radearth: earth radius
!     pio180: degrees-to-radians conversion factor
!     pio2: pi/2
!     pio4: pi/4
!     f
!     others are scratch variables (see code)

implicit none

real, parameter :: radearth = 6370000.           ! (USGS Paper 1395)
real, parameter :: pio180 = 3.141592653589793 / 180.
real, parameter :: pio2 = 3.141592653589793 / 2.
real, parameter :: pio4 = 3.141592653589793 / 4.

real :: qlat,qlon,polelat,polelon,stdlat1,stdlat2,x,y
real :: rpolelat,rpolelon,rstdlat1,rstdlat2,rqlat,rqlon,cone  &
       ,Ffactor,rho,rho0,rho8999,theta,xr,yr

if (stdlat2 < stdlat1) then
   print*, 'Error: Subroutine ll_lc2sp was called with stdlat2 < stdlat1.'
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'll_lc2sp_1'
!   return
endif

if (abs(stdlat1 + stdlat2) < .1) then
   print*, 'Error: Subroutine ll_lc2sp was called with standard latitudes'
   print*, '   that indicate a Mercator projection.'
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'll_lc2sp_2'
!   return
endif

if (stdlat2 > 89.9 .or. stdlat1 < -89.9) then
   print*, 'Error: Subroutine ll_lc2sp was called with standard latitude'
   print*, '   out of bounds:  stdlat1 = ',stdlat1,' stdlat2 = ',stdlat2
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'll_lc2sp_3'
!   return
endif

rpolelat = polelat * pio180
rpolelon = polelon * pio180
rqlat = qlat * pio180
rqlon = qlon * pio180
rstdlat1 = stdlat1 * pio180
rstdlat2 = stdlat2 * pio180

if (stdlat2 - stdlat1 > .1) then
   cone = log(cos(rstdlat1) / cos(rstdlat2))  &
        / log(tan(pio4 + .5 * rstdlat2) / tan(pio4 + .5 * rstdlat1))
else
   cone = sin(rstdlat1)
endif
     
Ffactor = cos(rstdlat1) * tan(pio4 + .5 * rstdlat1) ** cone / cone

rho0 = Ffactor / tan(pio4 + .5 * rpolelat) ** cone

if (qlat <= 89.99) then
   rho = Ffactor / tan(pio4 + .5 * rqlat) ** cone
else
   rho8999 = Ffactor/tan(pio4 + .5 * 89.99 * pio180) ** cone
   rho = 100. * (90. - qlat) * rho8999
endif
   
theta = cone * (rqlon - rpolelon)

xr = rho * sin(theta)
yr = rho0 - rho * cos(theta)

!write(6,55)  stdlat1,stdlat2,cone, Ffactor, rho0, theta, x, y
!55 format(10f10.5)

x = xr * radearth
y = yr * radearth

return
end

!***************************************************************************

! *****Subroutine to convert x,y (relative to polelon,polelat) to qlat,qlon
! on Lambert conformal Conic grid with 2 different or identical standard parallels

subroutine lc_ll2(qlat,qlon,polelat,polelon,stdlat1,stdlat2,x,y)

!     ***** Passed variables ********

!     qlat    : latitude of input point (deg)
!     qlon    : longitude of input point (deg)
!     polelat : latitude of origin on lcc grid (deg)
!     polelon : longitude of origin on lcc grid (deg)
!     stdlat1 : first (southern) standard latitude (deg)
!     stdlat2 : second (northern) standard latitude (deg)
!     x       : x-coordinate of point on Lambert Conformal grid (m)
!     y       : y-coordinate of point on Lambert Conformal grid (m)

!     ***** Local variables *********

!     cone: cone constant
!     Ffactor: another constant
!     radearth: earth radius
!     pio180: degrees-to-radians conversion factor
!     pio2: pi/2
!     pio4: pi/4
!     f
!     others are scratch variables (see code)

implicit none

real, parameter :: radearth = 6370000.           ! (USGS Paper 1395)
real, parameter :: pio180 = 3.14159265359 / 180.
real, parameter :: pio2 = 3.14159265359 / 2.
real, parameter :: pio4 = 3.14159265359 / 4.

real :: qlat,qlon,polelat,polelon,stdlat1,stdlat2,x,y
real :: rpolelat,rpolelon,rstdlat1,rstdlat2,rqlat,rqlon,cone  &
       ,Ffactor,rho,rho0,rho8999,theta,xr,yr

if (stdlat2 < stdlat1) then
   print*, 'Error: Subroutine lc2sp_ll was called with stdlat2 < stdlat1.'
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'lc2sp_ll_1'
!   return
endif

if (abs(stdlat1 + stdlat2) < .1) then
   print*, 'Error: Subroutine lc2sp_ll was called with standard latitudes'
   print*, '   that indicate a Mercator projection.'
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'lc2sp_ll_2'
!   return
endif

if (stdlat2 > 89.9 .or. stdlat1 < -89.9) then
   print*, 'Error: Subroutine lc2sp_ll was called with standard latitude'
   print*, '   out of bounds:  stdlat1 = ',stdlat1,' stdlat2 = ',stdlat2
   print*, '   stdlat1 = ',stdlat1,'   stdlat2 = ',stdlat2
   stop 'lc2sp_ll_3'
!   return
endif

rpolelat = polelat * pio180
rpolelon = polelon * pio180
rstdlat1 = stdlat1 * pio180
rstdlat2 = stdlat2 * pio180

if (stdlat2 - stdlat1 > .1) then
   cone = log(cos(rstdlat1) / cos(rstdlat2))  &
        / log(tan(pio4 + .5 * rstdlat2) / tan(pio4 + .5 * rstdlat1))
else
   cone = sin(rstdlat1)
endif
     
Ffactor = cos(rstdlat1) * tan(pio4 + .5 * rstdlat1) ** cone / cone

rho0 = Ffactor / (tan(pio4 + .5 * rpolelat)) ** cone

xr = x / radearth
yr = y / radearth

if (cone >= 0.) then
   rho = sqrt(xr ** 2 + (rho0 - yr) ** 2)
else
   rho = -sqrt(xr ** 2 + (rho0 - yr) ** 2)
endif

theta = atan(xr / (rho0 - yr))

rqlat = 2. * atan((Ffactor/rho) ** (1./cone)) - pio2
rqlon = theta / cone + rpolelon

!print*, 'reverse: cone,Ffactor,rho0,rho,theta,rqlat,rqlon'
!print*,           cone,Ffactor,rho0,rho,theta,rqlat,rqlon

qlat = rqlat / pio180
qlon = rqlon / pio180

return
end

!***************************************************************************

subroutine uvll_uvlc2(ulc,vlc,ull,vll,qlat,qlon,polelat,polelon,stdlat1,stdlat2)

!    Rotate earth-relative winds to lambert-conformal-relative components

implicit none
real :: ulc,vlc,ull,vll,qlat,qlon,polelat,polelon &
       ,x0,y0,x1,y1,angle,stdlat1,stdlat2

call ll_lc(qlat,qlon   ,polelat,polelon,stdlat1,stdlat2,x0,y0)
call ll_lc(qlat,qlon+.1,polelat,polelon,stdlat1,stdlat2,x1,y1)

angle = atan2(y1-y0,x1-x0)
ulc = ull * cos(angle) - vll * sin(angle)
vlc = ull * sin(angle) + vll * cos(angle)

return
end

!***************************************************************************

subroutine uvlc_uvll2(ulc,vlc,ull,vll,qlat,qlon,polelat,polelon,stdlat1,stdlat2)

!    Rotate lambert-conformal-relative winds to earth-relative components

implicit none
real :: ulc,vlc,ull,vll,qlat,qlon,polelat,polelon &
       ,x0,y0,x1,y1,angle,stdlat1,stdlat2

call ll_lc(qlat,qlon   ,polelat,polelon,stdlat1,stdlat2,x0,y0)
call ll_lc(qlat,qlon+.1,polelat,polelon,stdlat1,stdlat2,x1,y1)

angle = -atan2(y1-y0,x1-x0)
ull = ulc * cos(angle) - vlc * sin(angle)
vll = ulc * sin(angle) + vlc * cos(angle)

return
end

