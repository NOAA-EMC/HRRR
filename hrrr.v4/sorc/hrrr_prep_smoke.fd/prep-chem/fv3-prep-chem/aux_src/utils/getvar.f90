!############################# Change Log ##################################
! 2.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

integer function RAMS_getvar(string, ngrd, a, b, flnm)

  use an_header

  implicit none
  ! Arguments:
  character(len=*), intent(in) :: string
  integer, intent(in)          :: ngrd
  real, intent(inout)          :: a(*), b(*)
  character(len=*), intent(in) :: flnm
  ! Local variables:
  integer          :: itype !!, rams_c_pos
  character        :: cgrid*1, flng*128, errmsg*120
  logical          :: there
  integer          :: ni
  integer          :: npts, iword
	integer, external :: rams_c_open
	integer :: ierr	
  print*,'getvar:',string

  do ni=1,nvbtab

     if(string==anal_table(ni)%string .and. ngrd==anal_table(ni)%ngrid) then
   
        write(cgrid,'(i1)') ngrd
        flng=trim(flnm)//'-g'//cgrid//'.vfm'

        inquire(file=flng,exist=there)
        if(.not.there) then
           errmsg='File not found - '//flng
           RAMS_getvar = 1
           call error_mess(errmsg)
           return
        endif

        npts=anal_table(ni)%nvalues
        itype=anal_table(ni)%idim_type
        iword=anal_table(ni)%npointer
        
        !  print*,'gv:opening'
        ierr= rams_c_open(trim(flng)//char(0),'r'//char(0))
        !  print*,'gv:opened'
        call vfirecr(10,a,npts,'LIN',b,iword)
        !  print*,'gv:vfirecr'
        call RAMS_c_close()
        !  print*,'gv:closed'

        RAMS_getvar=0
        print*,'getvar good:',string
        return

     endif
  enddo

  errmsg='Variable not available in this run - '//string
  call error_mess(errmsg)
  RAMS_getvar=1

  return
end function RAMS_getvar
