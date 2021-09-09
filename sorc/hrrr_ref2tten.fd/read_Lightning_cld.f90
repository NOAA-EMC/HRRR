SUBROUTINE read_Lightning2cld(nlon,nlat,numlight,light_in,lightning)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NESDIS     read in lightning flash rate  
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-11-30
!
! ABSTRACT: 
!  This subroutine read in lightning flash rate
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     numlight    - number of observation
!
!   output argument list:
!     lightning   - lightning flash rate in analysis grid
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

  use kinds, only: r_kind,i_kind, r_single
  implicit none

  INTEGER(i_kind),intent(in) :: nlon,nlat
  INTEGER(i_kind),intent(in) :: numlight 
  real(r_single),intent(in)    :: light_in(3,numlight)
  real(r_single), intent(out):: lightning(nlon,nlat)
!
!  local
!
  integer(i_kind):: ilat1s,ilon1s
  INTEGER(i_kind) :: i,j,ii,jj
!
  ilon1s=1
  ilat1s=2

!  DO i=1,numlight
!    write(*,*) 'lat lon values ',i,light_in(ilat1s,i)+0.001_r_kind,light_in(ilon1s,i)+0.001_r_kind
!  ENDDO
  DO i=1,numlight
    ii=int(light_in(ilon1s,i)+0.001_r_single)
    jj=int(light_in(ilat1s,i)+0.001_r_single)
    
    if( ii < 1 .or. ii > nlon ) write(6,*) 'read_Lightning_cld: ', &
                                'Error in read in lightning ii:',ii,jj,i
    if( jj < 1 .or. jj > nlat ) write(6,*) 'read_Lightning_cld:', &
                                'Error in read in lightning jj:',ii,jj,i
    lightning(ii,jj)=light_in(3,i)
  ENDDO

END SUBROUTINE read_Lightning2cld
