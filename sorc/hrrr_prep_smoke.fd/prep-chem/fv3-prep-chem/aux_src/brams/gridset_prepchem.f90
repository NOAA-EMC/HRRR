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

subroutine gridset(ngra)

use mem_grid
use rconstants

implicit none

integer :: ngra,nhemgrds,ifm,icm,k,nestza1,nestza2,iinc,icnt,if1,jinc  &
   ,jcnt,jf,kinc,kcnt,kf,nrat,i,j,ngrb,kcy,kcw,kk
real :: centx1,centy1,centx,centy,dzr,dsum,dzrcm,dzrfm,tsum
real :: zmnvc(-1:nzpmax+1,maxgrds)

! If ngra = 1, the model is being initialized.  If ngra > 1, this routine
! is being called for moving a nested grid.  Begin this routine with several
! operations done only for model initialization.

if (ngra == 1) then

! Check to see whether this is a global simulation.  If it is, set nhemgrds
! (the number of hemispheric grids) to 2 and set nhemgrd2 to the number of
! the second hemispheric grid.  Grid 1 is always the first hemispheric grid.

   nhemgrds = 0
   do ifm = 1,ngrids
      icm = nxtnest(ifm)
      if (icm == 0) then
         nhemgrds = nhemgrds + 1
         nhemgrd2 = ifm
      endif
   enddo
   if (nhemgrd2 == 1) nhemgrd2 = 0

! Fill platn and plonn arrays, the value of polelat and polelon for each
! grid.

   do ifm = 1,ngrids
      icm = nxtnest(ifm)
      if (ifm == 1) then
         platn(ifm) = polelat
         plonn(ifm) = polelon
      elseif (ifm == nhemgrd2) then
         platn(ifm) = -polelat
         if (polelon <= 0.) plonn(ifm) = polelon + 180.
         if (polelon > 0.) plonn(ifm) = polelon - 180.
      else
         platn(ifm) = platn(icm)
         plonn(ifm) = plonn(icm)
      endif
   enddo

! If this is a global simulation, set ihtran to 1 and redefine DELTAX and
! DELTAY so that grids
! 1 and nhemgrd2 are hemispheric, based on nnxp and nnyp.  Also, override
! the namelist settings of CENTLAT and CENTLON for the two hemispheric grids
! to the platn and plonn values for those grids.

   if (nhemgrd2 > 1) then
      ihtran = 1
      deltax = 4. * erad / float(nnxp(1) - 3)
      deltay = deltax
      centlat(1) = platn(1)
      centlon(1) = plonn(1)
      centlat(nhemgrd2) = platn(nhemgrd2)
      centlon(nhemgrd2) = plonn(nhemgrd2)
   endif

! Set NRZFLG to the nested grids (one maximum for each hemisphere) that
! influences the CM vertical spacing.
! Fill NRZ with the variable nest ratios.

   do ifm = 1,ngrids
      do k = 1,nzpmax
         nrz(k,ifm) = 1
      enddo
   enddo

   nestza1 = abs(nestz1)
   nestza2 = abs(nestz2)

   if (nestza1 > 1 .and. nestza1 <= ngrids) then
      do k = 2,nnzp(1)
         nrz(k,nestza1) = max(1,nstratz1(k))
      enddo
      nrz(1,nestza1) = nstratz1(2)
   endif

   if (nestza2 > 1 .and. nestza2 <= ngrids .and. nhemgrd2 > 1) then
      do k = 2,nnzp(1)
         nrz(k,nestza2) = max(1,nstratz2(k))
      enddo
      nrz(1,nestza2) = nstratz2(2)
   endif

!     FILL ALL DELTAXN AND DELTAYN VALUES, AND FIND NINEST AND NJNEST VALUES
!     IF SET TO ZERO IN NAMELIST

   deltaxn(1) = deltax
   deltayn(1) = deltay

   call ll_xy(centlat(1),centlon(1),platn(1),plonn(1),stdlat2,centx1,centy1)

   xmn(1,1) = centx1 - 0.5 * float(nnxp(1)-2) * deltaxn(1)
   ymn(1,1) = centy1 - 0.5 * float(nnyp(1)-2) * deltayn(1)
   if (nnyp(1) == 1) ymn(1,1) = centy1
   if (nhemgrd2 > 1) then
      deltaxn(nhemgrd2) = deltaxn(1)
      deltayn(nhemgrd2) = deltayn(1)
      xmn(1,nhemgrd2)  = xmn(1,1)
      ymn(1,nhemgrd2) = ymn(1,1)
   endif

   do ifm = 1,ngrids
      icm = nxtnest(ifm)
      if (icm >= 1) then
         deltaxn(ifm) = deltaxn(icm) / float(nstratx(ifm))
         deltayn(ifm) = deltayn(icm) / float(nstraty(ifm))
         if (ninest(ifm) <= 0 .or. njnest(ifm) <= 0)  &
            call ll_xy(centlat(ifm),centlon(ifm),platn(ifm)  &
               ,plonn(ifm),stdlat2,centx,centy)
         if (ninest(ifm) <= 0) then
            xmn(1,ifm) = centx - 0.5 * float(nnxp(ifm)-2) * deltaxn(ifm)
            ninest(ifm) = int((xmn(1,ifm) - xmn(1,icm)) / deltaxn(icm) + 1.5)
         endif
         xmn(1,ifm) = xmn(1,icm) + deltaxn(icm) * float(ninest(ifm)-1)
         if (njnest(ifm) <= 0) then
            ymn(1,ifm) = centy - 0.5 * float(nnyp(ifm)-2) * deltayn(ifm)
            njnest(ifm) = int((ymn(1,ifm) - ymn(1,icm)) / deltayn(icm) + 1.5)
         endif
         ymn(1,ifm) = ymn(1,icm) + deltayn(icm) * float(njnest(ifm)-1)
      endif
   enddo
endif

!     Fill IPM, JPM and KPM arrays with parent grid index values for
!     all fine grids.

do ifm = 2,ngrids
   icm = nxtnest(ifm)
   if (icm >= 1) then
      ipm(1,ifm) = ninest(ifm)
      iinc = 1
      icnt = 0
      do if1 = 2,nnxp(ifm)
         ipm(if1,ifm) = ipm(if1-1,ifm) + iinc
         icnt = icnt + 1
         if (icnt >= nstratx(ifm)) then
            icnt = 0
            iinc = 1
         else
            iinc = 0
         endif
      enddo

      jpm(1,ifm) = njnest(ifm)
      jinc = 1
      jcnt = 0
      do jf = 2,nnyp(ifm)
         jpm(jf,ifm) = jpm(jf-1,ifm) + jinc
         jcnt = jcnt + 1
         if (jcnt >= nstraty(ifm)) then
            jcnt = 0
            jinc = 1
         else
            jinc = 0
         endif
      enddo

      if (nknest(ifm) == 1) then
         kpm(1,ifm) = 2
         kinc = 0
      else
         kpm(1,ifm) = nknest(ifm)
         kinc = 1
      endif
      kcnt = 0
      do kf = 2,nnzp(ifm)
         kpm(kf,ifm) = kpm(kf-1,ifm) + kinc
         nrat = nrz(kpm(kf,ifm),ifm)
         kcnt = kcnt + 1
         if (kcnt >= nrat .and. (kf < nnzp(ifm)-1 .or.  &
            kpm(kf,ifm) < nnzp(icm)-1)) then
            kcnt = 0
            kinc = 1
         else
            kinc = 0
         endif
      enddo
   endif
enddo

! Calculate xmn and ymn for the coarse grid(s) for an initial start.

if (ngra == 1) then

   do i = 2,nnxp(1)
      xmn(i,1) = xmn(i-1,1) + deltaxn(1)
      !print*,'xx=',xmn(i,1)
   enddo

   do j = 2,nnyp(1)
      ymn(j,1) = ymn(j-1,1) + deltayn(1)
   enddo

   if (nhemgrd2 > 1) then
      do i = 1,nnxp(nhemgrd2)
         xmn(i,nhemgrd2) = xmn(i,1)
      enddo
      do j = 1,nnyp(nhemgrd2)
         ymn(j,nhemgrd2) = ymn(j,1)
      enddo
   endif

endif

! compute xmn and ymn for any required nested grids, and xtn and ytn for
! any required grids.

ngrb = ngrids
if (ngra > 1) ngrb = ngra

do ifm = ngra,ngrb
   icm = nxtnest(ifm)
   if (icm >= 1) then
      xmn(1,ifm) = xmn(ninest(ifm),icm)
      ymn(1,ifm) = ymn(njnest(ifm),icm)
   endif
   do i = 2,nnxp(ifm)
      xmn(i,ifm) = xmn(i-1,ifm) + deltaxn(ifm)
      xtn(i,ifm) = .5 * (xmn(i,ifm) + xmn(i-1,ifm))
   enddo
   xtn(1,ifm) = 1.5 * xmn(1,ifm) - .5 * xmn(2,ifm)

   if (jdim == 1) then
      do j = 2,nnyp(ifm)
         ymn(j,ifm) = ymn(j-1,ifm) + deltayn(ifm)
         ytn(j,ifm) = .5 * (ymn(j,ifm) + ymn(j-1,ifm))
      enddo
      ytn(1,ifm) = 1.5 * ymn(1,ifm) - .5 * ymn(2,ifm)
   else
      ytn(1,ifm) = ymn(1,ifm)
   endif
enddo

!-24-01-2012-srf: return since we are not using any vertical information
 return 
!-24-01-2012-srf

! return if ngra > 1 since all remaining operations in this routine are
! required for initialization only.

if (ngra > 1) return

! calculate zmn for the coarse grid(s).

if (deltaz == 0.) then
   do k = 1,nnzp(1)
      zmn(k,1) = zz(k)
   enddo
else
   zmn(1,1) = 0.
   zmn(2,1) = deltaz
   do k = 3,nnzp(1)
   dzr = dzrat
   if (nestz1 < -1 ) then
      if( k > kpm(2,nestza1) .and.  &
          k <= kpm(nnzp(nestza1)-1,nestza1) .and.  &
          nrz(k,nestza1) /= nrz(k-1,nestza1)) then
         if (max(nrz(k,nestza1),nrz(k-1,nestza1)) == 2)  &
            dzr = 1.325 ** (nrz(k,nestza1) - nrz(k-1,nestza1))
         if (max(nrz(k,nestza1),nrz(k-1,nestza1)) == 3)  &
            dzr = 1.124 ** (nrz(k,nestza1) - nrz(k-1,nestza1))
         if (max(nrz(k,nestza1),nrz(k-1,nestza1)) == 4)  &
            dzr = 1.066 ** (nrz(k,nestza1) - nrz(k-1,nestza1))
         if (max(nrz(k,nestza1),nrz(k-1,nestza1)) == 5)  &
            dzr = 1.042 ** (nrz(k,nestza1) - nrz(k-1,nestza1))
      endif
   endif
   zmn(k,1) = zmn(k-1,1) + min(dzr * (zmn(k-1,1) - zmn(k-2,1)),dzmax)
   enddo
   deltazn(1) = deltaz
endif

if (nhemgrd2 > 1) then
   do k = 1,nnzp(nhemgrd2)
      zmn(k,nhemgrd2) = zmn(k,1)
   enddo
   deltazn(nhemgrd2) = deltazn(1)
endif

! fill scratch array znmvc with zmn for coarse grid(s)

ztop = zmn(max(nnzp(1)-1,1),1)

do k = 1,nnzp(1)
   zmnvc(k,1) = zmn(k,1)
enddo
zmnvc(0,1) = -(zmnvc(2,1) - zmnvc(1,1)) ** 2 / (zmnvc(3,1) - zmnvc(2,1))
zmnvc(-1,1) = zmnvc(0,1) - (zmnvc(1,1) - zmnvc(0,1)) ** 2  &
   / (zmnvc(2,1) - zmnvc(1,1))
zmnvc(nnzp(1)+1,1) = zmnvc(nnzp(1),1)  &
   + (zmnvc(nnzp(1),1) - zmnvc(nnzp(1)-1,1)) ** 2  &
   / (zmnvc(nnzp(1)-1,1) - zmnvc(nnzp(1)-2,1))

if (nhemgrd2 > 1) then
   do k = -1,nnzp(nhemgrd2)+1
      zmnvc(k,nhemgrd2) = zmnvc(k,1)
   enddo
endif

! get zmn coordinates for all nested grids.

do ifm = 2,ngrids
   icm = nxtnest(ifm)
   if (icm >= 1) then
      kcy = max(-1,nrz(kpm(2,ifm),ifm)-3)
      do k = -1,nnzp(ifm) + 1
         if (k <= 0) then
            nrat = nrz(kpm(2,ifm),ifm)
            kcw = nknest(ifm)-1
            if (k == -1 .and. nrat == 1) kcw = nknest(ifm) - 2
         elseif (k == nnzp(ifm)+1) then
            nrat = nrz(kpm(nnzp(ifm)-1,ifm),ifm)
         else
            nrat = nrz(kpm(k,ifm),ifm)
         endif
         kcy = mod(kcy+1,nrat)
!
         if (kcy == 0) then
            if (k >= 1) kcw = kcw + 1
            zmnvc(k,ifm) = zmnvc(kcw,icm)
         else
            if (kcy == 1 .or. k == -1) then
               dsum = 0.
               dzrcm = sqrt((zmnvc(kcw+2,icm) - zmnvc(kcw+1,icm))  &
                  * nrz(max(1,kcw),ifm)  &
                  / ((zmnvc(kcw,icm) - zmnvc(kcw-1,icm)) * nrz(kcw+2,ifm)))
               dzrfm = dzrcm ** (1. / float(nrat))
               tsum = 0.
               do kk = 1,nrat
                  tsum = tsum + dzrfm ** (kk-1)
               enddo
            endif
            if (k == -1) then
               do kk = 1,kcy-1
                  dsum = dsum + ((zmnvc(kcw+1,icm)-zmnvc(kcw,icm))  &
                     / tsum) * dzrfm ** (kk-1)
               enddo
            endif
            dsum = dsum + (zmnvc(kcw+1,icm) - zmnvc(kcw,icm))  &
               / tsum * dzrfm ** (kcy-1)
            zmnvc(k,ifm) = zmnvc(kcw,icm) + dsum
         endif
         if (k >= 1 .and. k <= nnzp(ifm)) zmn(k,ifm) = zmnvc(k,ifm)
      enddo
   endif
enddo

!     compute ztn values for all grids by geometric interpolation.

do ifm = 1,ngrids
   do k = 1,nnzp(ifm)
      dzrfm = sqrt(sqrt((zmnvc(k+1,ifm) - zmnvc(k,ifm))  &
         / (zmnvc(k-1,ifm) - zmnvc(k-2,ifm))))
      ztn(k,ifm) = zmnvc(k-1,ifm)  &
         + (zmnvc(k,ifm) - zmnvc(k-1,ifm)) / (1. + dzrfm)
   enddo

!     compute other arrays based on the vertical grid.

   do k = 1,nnzp(ifm)-1
      dzmn(k,ifm) = 1. / (ztn(k+1,ifm) - ztn(k,ifm))
   enddo
   do k = 2,nnzp(ifm)
      dztn(k,ifm) = 1. / (zmn(k,ifm) - zmn(k-1,ifm))
   enddo
   do k = 2,nnzp(ifm)-1
      dzm2n(k,ifm) = 1. / (zmn(k+1,ifm) - zmn(k-1,ifm))
      dzt2n(k,ifm) = 1. / (ztn(k+1,ifm) - ztn(k-1,ifm))
   enddo
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   if(nnzp(ifm) >=3 ) then
     dzmn(nnzp(ifm),ifm) = dzmn(nnzp(ifm)-1,ifm)  &
        * dzmn(nnzp(ifm)-1,ifm) / dzmn(nnzp(ifm)-2,ifm)
   else
     dzmn(nnzp(ifm),ifm) = 0.
   endif
   dztn(1,ifm) = dztn(2,ifm) * dztn(2,ifm) / dztn(3,ifm)
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   dzm2n(1,ifm) = dzm2n(2,ifm)
   dzm2n(nnzp(ifm),ifm) = dzm2n(max(nnzp(ifm)-1,1),ifm)
   dzt2n(1,ifm) = dzt2n(2,ifm)
   dzt2n(nnzp(ifm),ifm) = dzt2n(max(nnzp(ifm)-1,1),ifm)

   deltazn(ifm) = zmn(2,ifm) - zmn(1,ifm)

enddo

!srf - call cofnest

return
end
