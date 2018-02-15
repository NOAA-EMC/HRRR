!-------------------------------------------------------------------------
!    NOAA/GSD,
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  initial ---  initialization 
!
! !INTERFACE:
!
module initial

! !USES:

  use kinds, only: r_kind,r_single

  implicit none

! !DESCRIPTION: module containing initialization
!
! !REVISION HISTORY:
!   2014-12-18  Hu 
!
!
! !AUTHOR: 
!   HU               org: GDS                 date: 2014-12-16
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: init_namelist

! set passed variables to public
  public :: miterrr

  integer miterrr ! test

contains
   
!-------------------------------------------------------------------------
!    NOAA/GSD 
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  init_namelist --- Initialize defaults for namelist variables
!
! !INTERFACE:
!
  subroutine init_namelist

!
!
! !REVISION HISTORY:
!   2014-12-18  Hu 
!
! !AUTHOR:
!   Hu               org: GDS                date: 2014-12-18
!
!EOP
!-------------------------------------------------------------------------
    use constants, only: two
    implicit none

    miterrr=3

    return
  end subroutine init_namelist
  

end module initial

