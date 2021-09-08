module module_var_info
  use module_types, only: var_info
  use module_att_info, only: copy_var_atts
  use module_error
contains
 
  subroutine free_var_info(info)
    implicit none
    type(var_info), intent(inout) :: info

    if(allocated(info%varname)) deallocate(info%varname)
    info%varid=-1
    info%vartype=-1
    info%ndims=-1
    info%datasize=-1
    if(allocated(info%dimids)) deallocate(info%dimids)
    if(allocated(info%dimsize)) deallocate(info%dimsize)
    if(allocated(info%real)) deallocate(info%real)
    if(allocated(info%int)) deallocate(info%int)
  end subroutine free_var_info

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical function get_var_info(varname, infile, ncid, info)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varname, infile
    type(var_info), intent(inout) :: info
    ! locals
    integer :: idim, ierr

    allocate(character(len=len_trim(varname)) :: info%varname)
    info%varname=varname

    get_var_info=( nf_inq_varid(ncid,info%varname,info%varid) == 0 )

    if(.not.get_var_info) return ! variable is not in the file

    call nferr_0_or_die(nf_inq_vartype(ncid,info%varid,info%vartype), &
         action='nf_inq_vartype',filename=trim(infile),variable=info%varname)
    
    call nferr_0_or_die(ierr=nf_inq_varndims(ncid,info%varid,info%ndims), &
         action='nf_inq_varndims',filename=trim(infile),variable=info%varname)

    allocate(info%dimids(info%ndims))
    allocate(info%dimsize(info%ndims))

    call nferr_0_or_die(ierr=nf_inq_vardimid(ncid,info%varid,info%dimids), &
         action='nf_inq_vardimid',filename=trim(infile),variable=info%varname)

    info%datasize=1
    do idim=1,info%ndims
       call nferr_0_or_die(ierr=nf_inq_dimlen(ncid,info%dimids(idim),info%dimsize(idim)), &
            action='nf_inq_dimlen',filename=trim(infile),variable=info%varname)
       info%datasize=info%datasize*info%dimsize(idim)
    enddo
  end function get_var_info
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_var(infile,ncid,info)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: infile
    type(var_info), intent(inout) :: info

    select case(info%vartype)
    case(NF_INT)
       allocate(info%int(info%datasize))
       call nferr_0_or_die(ierr=nf_get_var_int(ncid,info%varid,info%int), &
            action='nf_get_var_int',filename=trim(infile),variable=info%varname)
    case(NF_REAL)
       allocate(info%real(info%datasize))
       call nferr_0_or_die(ierr=nf_get_var_real(ncid,info%varid,info%real), &
            action='nf_get_var_real',filename=trim(infile),variable=info%varname)
    case DEFAULT
       write(0,3033) trim(infile),info%varname,"only 4-byte integer and real datatypes are supported"
       stop 1
    end select
3033 format('ABORT: ',A,': ',A,': ',A)
  end subroutine read_var

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine def_var(infile,ncin,from,outfile,ncout,to)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncout, ncin
    character(len=*), intent(in) :: infile, outfile
    type(var_info), intent(inout) :: from,to
    ! locals
    integer :: idim, iattr, nattr
    character(len=500) :: name

    if(from%varname /= to%varname) then
       write(0,3033) trim(infile),from%varname,'internal error: varname mismatch'
       stop 4
    endif

    to%vartype=from%vartype
    to%datasize=from%datasize
    to%ndims=from%ndims
    allocate(to%dimsize(from%ndims))
    allocate(to%dimids(from%ndims))

    do idim=1,to%ndims
       name=' '
       call nferr_0_or_die(ierr=nf_inq_dimname(ncin,from%dimids(idim),name), &
            filename=trim(infile),variable=from%varname,action='nf_inq_dimname')

       call nferr_0_or_die(ierr=nf_inq_dimid(ncout,trim(name),to%dimids(idim)), &
            filename=trim(outfile),variable=from%varname,action='nf_inq_dimid for '//trim(name))
       call nferr_0_or_die(ierr=nf_inq_dimlen(ncout,to%dimids(idim),to%dimsize(idim)), &
            filename=trim(outfile),variable=from%varname,action='nf_inq_dimlen for '//trim(name))

       if(from%dimsize(idim) /= to%dimsize(idim)) then
          write(0,3033) trim(infile),from%varname//' '//trim(name), &
               'dimension size does not match output file'
          stop 2
       endif
    enddo

    call nferr_0_or_die(ierr=nf_def_var(ncout,from%varname,from%vartype,to%ndims,to%dimids,to%varid), &
         filename=trim(outfile),variable=from%varname,action='nf_def_var')

    call copy_var_atts(ncin, infile, from, ncout, outfile, to)

3033 format('ABORT: ',A,': ',A,': ',A)
  end subroutine def_var

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_var(infile,from,outfile,ncout,to)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncout
    character(len=*), intent(in) :: infile, outfile
    type(var_info), intent(inout) :: from,to
    ! locals
    integer :: idim

    if(from%varname /= to%varname) then
       write(0,3033) trim(infile),from%varname,'internal error: varname mismatch'
       stop 4
    endif

    if(from%vartype /= to%vartype) then
       write(0,3033) trim(infile),from%varname,'data type does not match output file'
       stop 3
    end if

    if(from%ndims /= to%ndims) then
       write(0,3033) trim(infile),from%varname,'number of dimensions does not match output file'
       stop 2
    endif

    do idim=1,from%ndims
       if(from%dimsize(idim) /= to%dimsize(idim)) then
          write(0,3033) trim(infile),from%varname,'dimension size does not match output file'
          stop 2
       endif
    enddo

    select case(from%vartype)
    case(NF_INT)
       call nferr_0_or_die(ierr=nf_put_var_int(ncout,to%varid,from%int), &
            action='nf_get_var_int',filename=trim(outfile),variable=from%varname)
    case(NF_REAL)
       call nferr_0_or_die(ierr=nf_put_var_real(ncout,to%varid,from%real), &
            action='nf_get_var_real',filename=trim(outfile),variable=from%varname)
    case DEFAULT
       write(0,3033) trim(infile),from%varname,"only 4-byte integer and real datatypes are supported"
       stop 1
    end select
3033 format('ABORT: ',A,': ',A,': ',A)
  end subroutine write_var
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine zero_var(info)
    implicit none
    include 'netcdf.inc'
    type(var_info), intent(inout) :: info

    if(allocated(info%real)) then
       info%real=0.0
    endif
    if(allocated(info%int)) then
       info%int=0
    endif
  end subroutine zero_var

end module module_var_info
