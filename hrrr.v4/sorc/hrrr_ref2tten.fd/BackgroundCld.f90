SUBROUTINE  BackgroundCld(lon2,lat2,nsig,tbk,pbk,psbk,q,hbk, &
             zh,pt_ll,eta1_ll,aeta1_ll)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BackgroundCld  Ingest background fields for cloud analysis
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine reads in background hydrometeor fields for cloud analysis
!
! PROGRAM HISTORY LOG:
!    2009-01-02  Hu  Add NCO document block
!    2010-04-26  Hu  delete the module gridmod and guess_grids.
!                    transfer information subroutine dummy variables
!
!
!   input argument list:
!     lon2        - no. of lons on subdomain (buffer points on ends)
!     lat2        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of vertical levels
!     tbk         - 3D background potential temperature (K)
!     psbk        - 2D background surface pressure (hPa)
!     q           - 3D moisture (water vapor mixing ratio kg/kg)
!     zh          - terrain
!     pt_ll       -  vertical coordinate 
!     eta1_ll     -  vertical coordinate 
!     aeta1_ll    -  vertical coordinate 
!
!   output argument list:
!     pbk         - 3D background pressure  (hPa)
!     hbk         - 3D height above the ground (not the sea level)
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use kinds, only: r_single,i_kind,r_kind
  use constants, only: rd,h1000,rd_over_cp,grav,half

  implicit none

  integer(i_kind),intent(in):: lon2
  integer(i_kind),intent(in):: lat2
  integer(i_kind),intent(in):: nsig

  real(r_kind), intent(in) :: pt_ll
  real(r_kind), intent(in) :: eta1_ll(nsig+1)  !
  real(r_kind), intent(in) :: aeta1_ll(nsig)   !


! background
!
! read in from WRF
!
  real(r_single),intent(in) :: tbk(lon2,lat2,nsig)        ! potential temperature
  real(r_single),intent(inout) :: psbk(lon2,lat2)         ! surface pressure
  real(r_single),intent(in)    :: zh(lon2,lat2)           ! terrain elevation
  real(r_single),intent(inout) :: q(lon2,lat2,nsig)       ! moisture
!
! derived fields
!
  real(r_single),intent(out) :: hbk(lon2,lat2,nsig)! height
  real(r_single),intent(out) :: pbk(lon2,lat2,nsig)! pressure  hPa

!
!  misc.
!
  INTEGER :: i,j,k

  REAL(r_single) :: rdog, h, dz, rl
  REAL(r_single) :: height(nsig+1)
  real(r_single) :: q_integral(lon2,lat2)   
  real(r_single) :: deltasigma, psfc_this
  
  real(r_single) :: tbk_k(nsig)
!
!================================================================
!
  q_integral=1
  do k=1,nsig
    deltasigma=eta1_ll(k)-eta1_ll(k+1)
    do j=1,lat2
      do i=1,lon2
         q(i,j,k) = q(i,j,k)/(1.0_r_kind-q(i,j,k))   ! water vapor mixing ratio (kg/kg)
         q_integral(i,j)=q_integral(i,j)+deltasigma*q(i,j,k)
      enddo
    enddo
  enddo
  do j=1,lat2
    do i=1,lon2
       psfc_this=pt_ll+(psbk(i,j)-pt_ll)/q_integral(i,j)
       psbk(i,j)= psfc_this
    enddo
  enddo
!
! now get pressure (pbk) and height (hbk) at each grid point
!

    do k=1,nsig
      do j=1,lat2
        do i=1,lon2
           pbk(i,j,k)=aeta1_ll(k)*(psbk(i,j)-pt_ll)+pt_ll
        end do
      end do
    end do

!   Compute geopotential height at midpoint of each layer
    rdog = rd/grav
    do j=1,lat2
      do i=1,lon2
        do k=1,nsig
           tbk_k(k)=tbk(i,j,k)*(pbk(i,j,k)/h1000)**rd_over_cp
        enddo
        k  = 1
        h  = rdog * tbk_k(k)
        dz = h * log(psbk(i,j)/pbk(i,j,k))
        height(k) = zh(i,j) + dz
      
        do k=2,nsig
          h  = rdog * half * (tbk_k(k-1)+tbk_k(k))
          dz = h * log(pbk(i,j,k-1)/pbk(i,j,k))
          height(k) = height(k-1) + dz
        end do
      
        do k=1,nsig
          hbk(i,j,k)=height(k) - zh(i,j)
        end do
      end do
    end do

END SUBROUTINE BackgroundCld
