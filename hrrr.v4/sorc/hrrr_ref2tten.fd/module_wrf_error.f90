MODULE module_wrf_error
!$$$   module documentation block
!
! module:  module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add module and subroutine doc blocks
!
! subroutines included:
!   wrf_at_debug_level           ---
!   init_module_wrf_error        ---
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: wrf_at_debug_level
  public :: init_module_wrf_error
! set passed variables to public
  public :: wrf_debug_level

  INTEGER(i_kind) :: wrf_debug_level = 0

CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  wrf_at_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    IMPLICIT NONE

    INTEGER(i_kind) , INTENT(IN   ) :: level

    wrf_at_debug_level = ( level <= wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
!$$$   subprogram documentation block
!
! subprogram:  init_module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    IMPLICIT NONE
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error
