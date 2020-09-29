module module_debug

#ifdef _GEOGRID 
   use parallel_module
#else
#ifdef _METGRID
   use parallel_module
#else
   integer, parameter :: IO_NODE = 0 
   integer :: my_proc_id = 0 
#endif
#endif

   integer, parameter :: QUIET=-100, LOGFILE=-2, DEBUG=0, INFORM=1, WARN=2, ERROR=3, STDOUT=100

   integer :: the_debug_level = DEBUG

   logical :: have_set_logname = .false.

   logical :: continuing_line_logfile = .false.
   logical :: continuing_line_debug   = .false.
   logical :: continuing_line_inform  = .false.
   logical :: continuing_line_warn    = .false.
   logical :: continuing_line_error   = .false.
   logical :: continuing_line_stdout  = .false.


   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: set_debug_level
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine set_debug_level(ilev)

      implicit none
     
      ! Arguments
      integer, intent(in) :: ilev

      the_debug_level = ilev

   end subroutine set_debug_level


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: mprintf
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine mprintf(assertion, level, fmtstring, &
                      newline, &
                      i1, i2, i3, i4, i5, i6, &
                      f1, f2, f3, f4, f5, f6, &
                      s1, s2, s3, s4, s5, s6, &
                      l1, l2, l3, l4, l5, l6)

      implicit none

      ! Arguments
      integer, intent(in) :: level
      logical, intent(in) :: assertion
      character (len=*), intent(in) :: fmtstring
      logical, intent(in), optional :: newline
      integer, intent(in), optional :: i1, i2, i3, i4, i5, i6
      real, intent(in), optional :: f1, f2, f3, f4, f5, f6
      logical, intent(in), optional :: l1, l2, l3, l4, l5, l6
      character (len=*), intent(in), optional :: s1, s2, s3, s4, s5, s6

      ! Local variables 
      integer :: idxi, idxf, idxs, idxl, istart, i, iend, ia
      real :: fa
      logical :: continuing_line, la
      character (len=8) :: cur_date
      character (len=10) :: cur_time
      character (len=10) :: print_date
      character (len=12) :: print_time
!BUG: sa should be as long as the largest string length used anywhere in WPS
      character (len=1024) :: sa
      character (len=1024) :: ctemp
      STOP " stoping at mprintf routine" 
   end subroutine mprintf

end module module_debug
