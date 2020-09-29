!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################


Module grid_dims

!  This module contains very basic specification of grid dimensions and other 
!     parameters that will be used to dimension arrays and allocate memory.

!==============================================================================
!        Set maximum values of parameters:
!          MAXGRDS - Maximum number of grids
!          NXPMAX  - Maximum number of points in x-direction
!          NYPMAX  - Maximum number of points in y-direction
!          NZPMAX  - Maximum number of points in z-direction
!          NZGMAX  - Maximum number of soil levels
!          MAXSCLR - Maximum number of additional scalars
!          MAXHP   - Maximum number of u, v, OR t points in a single vertical
!                       level interpolated from opposite hemispheric grid

   integer, parameter :: maxgrds=8
   integer, parameter :: nxpmax=2903, nypmax=2903, nzpmax=132, nzgmax=20
   integer, parameter :: maxsclr=150, maxhp=1000

!        Set MAXDIM to the largest of NXPMAX,NYPMAX,NZPMAX+10,NZGMAX

   integer, parameter :: maxdim=903

!        maxmach is the maximum number of processors that can be used in a 
!          parallel run

   integer, parameter :: maxmach=64
!==============================================================================

!        Computed parameters

   integer, parameter :: maxdimp=maxdim+2
   integer, parameter :: nxyzpm=nzpmax*nxpmax*nypmax
   
   real, parameter ::                    &
   	pi180    = 3.141592653589793 / 180.   &
   ,   	r_earth  = 6367000.

End Module grid_dims
