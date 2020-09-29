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


Module io_params

use grid_dims

integer, parameter :: maxlite=50

character (len=32) :: lite_vars(maxlite)
character(len=80)  :: hfilin,afilepref,pastfn
character(len=20)  :: xlite,ylite,zlite
integer :: ipastin
!-------------------------------------------------------------------------------
integer :: ioutput,iinput,iopunt,iclobber,nlite_vars
real    :: frqstate(maxgrds),avgtim,frqlite,frqmean,frqboth,frqstate_keep  


!-------------------------------------------------------------------------------

integer, dimension(maxgrds) :: itoptflg,isstflg,ivegtflg,isoilflg  &
                              ,ndviflg,nofilflg,itopsflg,iz0flg
real                        :: z0fact
integer                     :: ntopsmth,izflat
real, dimension(maxgrds)    :: z0max,toptenh,toptwvl
!-------------------------------------------------------------------------------
character(len=80), dimension(maxgrds) :: itoptfn,isstfn,ivegtfn,isoilfn  &
                                        ,ndvifn
!-------------------------------------------------------------------------------

integer, parameter :: maxsstfiles=2000
character(len=80)                             :: sstfpfx,sfcfiles,topfiles
character(len=80), dimension(maxsstfiles,maxgrds)     :: fnames_sst
character(len=14), dimension(maxsstfiles,maxgrds) :: itotdate_sst
!-------------------------------------------------------------------------------
integer                             :: iupdsst,isstcyclic,isstcycdata
integer,dimension(maxgrds)          :: nsstfiles,isstflp,isstflf
real,dimension(maxgrds)             :: ssttime1,ssttime2
!-------------------------------------------------------------------------------
integer, parameter :: maxndvifiles=2000
character(len=80)                                     :: ndvifpfx
character(len=80), dimension(maxndvifiles,maxgrds)     :: fnames_ndvi
character(len=14), dimension(maxndvifiles,maxgrds)     :: itotdate_ndvi
!-------------------------------------------------------------------------------
integer                             :: iupdndvi,indvicyclic,indvicycdata
integer,dimension(maxgrds)          :: nndvifiles,indviflp,indviflf
real,dimension(maxgrds)             :: ndvitime1,ndvitime2

!-------------------------------------------------------------------------------
character(len=8), dimension(50) :: plfmt,pltit
character(len=16), dimension(50) :: iplfld
!-------------------------------------------------------------------------------
integer                :: nplt,initfld
integer, dimension(50) :: ixsctn,iplvect,isbval,iaa,iab,joa,job,naavg,noavg
real                   :: frqprt
real, dimension(50)    :: plconlo,plconhi,plconin


end Module io_params
