module module_att_info

  use module_types
  use module_error

contains

  subroutine free_att_info(info)
    implicit none
    type(att_info), intent(inout) :: info

    if(allocated(info%attname)) deallocate(info%attname)
    if(allocated(info%cdata)) deallocate(info%cdata)
    info%attnum=-1
    info%atttype=-1
    info%attlen=-1
    info%idata=-1
  end subroutine free_att_info

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine free_att_list(info)
    implicit none
    type(att_list), intent(inout) :: info
    integer i
    if(allocated(info%atts)) then
       do i=1,info%natts
          call free_att_info(info%atts(i))
       enddo
    endif
    info%natts=-1
  end subroutine free_att_list

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_att_info(ncid, infile, varinfo, attinfo, attnum)
    implicit none
    include 'netcdf.inc'
    type(var_info), intent(in) :: varinfo
    type(att_info), intent(inout) :: attinfo
    integer, intent(in) :: ncid, attnum
    character(len=*), intent(in) :: infile
    ! locals
    character(len=500) :: name

    name=' '
    call nferr_0_or_die(ierr=nf_inq_attname(ncid,varinfo%varid,attnum,name), &
         filename=trim(infile),variable=varinfo%varname,action='nf_inq_attname')

    allocate(character(len=len_trim(name)) :: attinfo%attname)
    attinfo%attname=trim(name)

    call nferr_0_or_die(ierr=nf_inq_att(ncid,varinfo%varid,name, &
         attinfo%atttype,attinfo%attlen), &
         filename=trim(infile),action='nf_inq_att',variable=varinfo%varname, &
         attribute=attinfo%attname)
  end subroutine get_att_info

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine copy_att(ncin, infile, varin, attin, &
                      ncout, outfile, varout)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncin,ncout
    character(len=*), intent(in) :: infile, outfile
    type(var_info), intent(in) :: varin, varout
    type(att_info), intent(in) :: attin

    call nferr_0_or_die(ierr=nf_copy_att(ncin,varin%varid,attin%attname, &
         ncout,varout%varid), &
         filename=trim(outfile),action='nf_copy_att',variable=varin%varname, &
         attribute=attin%attname)

  end subroutine copy_att

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine copy_var_atts(ncin, infile, varin, &
                           ncout, outfile, varout)
    implicit none
    include 'netcdf.inc'
    integer, intent(in) :: ncin,ncout
    character(len=*), intent(in) :: infile, outfile
    type(var_info), intent(in) :: varin, varout
    ! locals
    type(att_info) :: attinfo
    integer :: iatt, nattr

    call nferr_0_or_die(ierr=nf_inq_varnatts(ncin,varin%varid,nattr), &
         filename=trim(infile),variable=varin%varname,action='nf_inq_varnatts')

    do iatt=1,nattr
       call get_att_info(ncin, infile, varin, attinfo, iatt)
       call copy_att(ncin, infile, varin, attinfo, ncout, outfile, varout)
       call free_att_info(attinfo)
    enddo

  end subroutine copy_var_atts

end module module_att_info
