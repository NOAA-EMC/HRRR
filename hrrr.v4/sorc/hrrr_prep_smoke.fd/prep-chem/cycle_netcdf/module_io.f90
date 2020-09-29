module module_io
  use module_error
contains

  subroutine start_defining(outfile,ncout)
    implicit none
    include 'netcdf.inc'
    character(len=*), intent(in) :: outfile
    integer, intent(inout) :: ncout
    call nferr_0_or_die(ierr=nf_redef(ncout),&
         filename=trim(outfile),action='nf_redef')
  end subroutine start_defining

  subroutine stop_defining(outfile,ncout)
    implicit none
    include 'netcdf.inc'
    character(len=*), intent(in) :: outfile
    integer, intent(inout) :: ncout
    write(0,'(A,": ",A)') trim(outfile),'stop defining...'
    call nferr_0_or_die(ierr=nf_enddef(ncout),&
         filename=trim(outfile),action='nf_enddef')
    write(0,'(A,": ",A)') trim(outfile),'... stopped defining'
  end subroutine stop_defining

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine open_files(infile,outfile, ncin, ncout)
    implicit none
    include 'netcdf.inc'
    character(len=*), intent(in) :: infile, outfile
    integer, intent(inout) :: ncin,ncout

    call nferr_0_or_die(ierr=nf_open(trim(infile), NF_NOWRITE, ncin), &
         filename=trim(infile),action='nf_open read-only')
    call nferr_0_or_die(ierr=nf_open(trim(outfile), NF_WRITE, ncout), &
         filename=trim(outfile),action='nf_open read-write')
  end subroutine open_files

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine close_files(infile,outfile, ncin, ncout)
    implicit none
    include 'netcdf.inc'
    character(len=*), intent(in) :: infile, outfile
    integer, intent(inout) :: ncin,ncout

    call nferr_0_or_die(ierr=nf_close(ncout),filename=trim(outfile),action='nf_close')
    call nferr_0_or_die(ierr=nf_close(ncin),filename=trim(infile),action='nf_close')
  end subroutine close_files

end module module_io
