module module_error
  implicit none
  logical :: verbose = .false.
contains

  subroutine nferr_0_or_die(ierr,filename,action,variable,attribute)
    implicit none

    include 'netcdf.inc'

    character(len=*),intent(in) :: filename,action
    character(len=*),intent(in),optional :: variable,attribute
    integer, intent(in) :: ierr

    sad_bad_error: if(ierr/=0) then
       if(present(variable)) then
          if(present(attribute)) then
             write(0,4044) trim(filename),trim(variable),trim(attribute),trim(action),trim(nf_strerror(ierr)),ierr
          else
             write(0,2022) trim(filename),trim(variable),trim(action),trim(nf_strerror(ierr)),ierr
          endif
       else
          write(0,3033) trim(filename),trim(action),trim(nf_strerror(ierr)),ierr
       endif
       stop 4
    else if(verbose) then
       if(present(variable)) then
          if(present(attribute)) then
             write(0,1044) trim(filename),trim(variable),trim(attribute),trim(action)
          else
             write(0,1022) trim(filename),trim(variable),trim(action)
          endif
       else
          write(0,1033) trim(filename),trim(action)
       endif
    endif sad_bad_error

4044 format('ABORT: ',A,': name ',A,' attr. ',A,': ',A,': ',A,' (status ',I0,')')
3033 format('ABORT: ',A,': ',A,': ',A,' (status ',I0,')')
2022 format('ABORT: ',A,': name ',A,': ',A,': ',A,' (status ',I0,')')

1033 format(A,': ',A,': success')
1022 format(A,': name ',A,': ',A,': success')
1044 format(A,': name ',A,' attr. ',A,': ',A,': success')

  end subroutine nferr_0_or_die
end module module_error
