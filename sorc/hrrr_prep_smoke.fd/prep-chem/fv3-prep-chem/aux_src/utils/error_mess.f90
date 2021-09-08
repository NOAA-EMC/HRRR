!############################# Change Log ##################################
! 2.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

subroutine error_mess(msg)

  ! error_mess: dumps a message on stdout

  implicit none
  character(len=*), intent(in) :: msg
  write(*,"(a)") msg
end subroutine error_mess



subroutine fatal_error(msg)

  ! fatal_error: exception handling, to be invoked
  !   whenever a fatal error occurs.
  !   dumps a message at stdout and halts execution

  implicit none
  character(len=*), intent(in) :: msg
  write(*,"(a)") "**(ERROR)**"//msg
  write(*,"(a)") "*** BRAMS execution halts (see previous error msg) ***"
  stop
end subroutine fatal_error
