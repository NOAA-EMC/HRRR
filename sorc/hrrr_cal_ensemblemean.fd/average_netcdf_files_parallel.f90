module module_writer
  implicit none
contains

  subroutine open_pnetcdf_for_write(comm,path,ncid)
    use pnetcdf
    use mpi
    implicit none
    integer, intent(out) :: ncid
    integer, intent(in) :: comm
    character(len=*), intent(in) :: path

    call check_nf90mpi(nf90mpi_open(mpi_comm=comm,path=trim(adjustl(path)),&
         omode=nf90_write,ncid=ncid,mpi_info=MPI_INFO_NULL),&
         'open for writing '//trim(adjustl(path)))
  end subroutine open_pnetcdf_for_write

  subroutine close_pnetcdf(path,ncid)
    use pnetcdf
    implicit none
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: path

    call check_nf90mpi(nf90mpi_close(ncid=ncid),'close '//trim(adjustl(path)))
  end subroutine close_pnetcdf

  function get_write_comm_size(ens_size)
    use mpi
    implicit none
    integer :: get_write_comm_size
    integer, intent(in) :: ens_size
    integer :: world_size,iret

    call mpi_comm_size(mpi_comm_world,world_size,iret)
    get_write_comm_size=world_size-ens_size
    if(get_write_comm_size<1) then
30     format('Too few ranks. Need at least ensemble_size+1 = ',I0)
       write(0,30) ens_size
       call mpi_abort(mpi_comm_world,1,iret)
    endif
  end function get_write_comm_size

  subroutine io_split_comm(write_comm_size,write_comms,local_comm,read_index,ens_size)
    use mpi
    implicit none
    integer, intent(in) :: ens_size, write_comm_size
    integer, intent(inout) :: write_comms(0:write_comm_size-1)
    integer, intent(out) :: local_comm,read_index

    integer :: world_size, world_rank, iret, color, key, iwrite

    call mpi_comm_size(mpi_comm_world,world_size,iret)
    call mpi_comm_rank(mpi_comm_world,world_rank,iret)

    local_comm=MPI_COMM_NULL
    if(world_rank>=write_comm_size) then
       color=2
       key=world_rank-write_comm_size
       read_index=key+1
    else
       color=1
       key=world_rank
       read_index=0
    endif
    call mpi_comm_split(mpi_comm_world,color,key,local_comm,iret)

    write_comms=MPI_COMM_NULL
    do iwrite=0,write_comm_size-1
       color=MPI_UNDEFINED
       if(world_rank==iwrite) then
          color=1
          key=0
       else if(world_rank>=write_comm_size) then
          color=1
          key=world_rank-write_comm_size+1
       endif
       call mpi_comm_split(mpi_comm_world,color,key,write_comms(iwrite),iret)
    enddo
  end subroutine io_split_comm

  function split_work(first_work,last_work,irank,nprocs)
    implicit none
    integer :: split_work(2)
    integer, intent(in) :: irank,nprocs,first_work,last_work
    integer :: nwork
    nwork=last_work-first_work+1
    split_work(1) = first_work + floor(real(irank)*real(nwork)/real(nprocs))
    split_work(2) = first_work-1 + floor(real(irank+1)*real(nwork)/real(nprocs))
  end function split_work

  subroutine reduce_and_write(local,write_comm_size,write_comm,ndims,dims,inner_size,outer_size,ens_size,ncid,path,varname,local_comm)
    use mpi
    use pnetcdf
    implicit none
    character(len=*), intent(in) :: path,varname
    integer, intent(in) :: ncid,ens_size
    integer, intent(in) :: write_comm_size,write_comm(0:write_comm_size-1)
    integer, intent(in) :: ndims,dims(ndims),inner_size,outer_size,local_comm
    real, intent(in) :: local(inner_size,outer_size)
    !
    real, allocatable :: to_send(:,:),to_recv(:,:)

    integer :: world_rank,i,j,toplen,bounds(2),iwrite,dummy(1,1)
    integer(kind=MPI_OFFSET_KIND) :: start(ndims,1), count(ndims,1)
    integer :: writers_used,ncvarid,iret

    call mpi_comm_rank(mpi_comm_world,world_rank,iret)

    if(write_comm_size>outer_size .and. world_rank==0) then
238    format(I4,': error: more write ranks than variable outer dimension size. Have ',I0,' but variable ',A,' has outer dimension size ',I0)
       write(0,238) world_rank,write_comm_size,trim(adjustl(varname)),outer_size
       call MPI_Abort(mpi_comm_world,1,iret)
    endif

    writers_used=0
    foreach_write_rank: do iwrite=0,write_comm_size-1
       bounds = split_work(1,outer_size,iwrite,write_comm_size)
       toplen=bounds(2)-bounds(1)+1
       if(world_rank==write_comm_size) then
318       format(I4,': send var ',A,' group',I4,' bounds=[',I0,',',I0']')
          print 318,world_rank,trim(adjustl(varname)),iwrite,bounds
       endif
       if(toplen>=1) then
          ! Too many writers
          writers_used=writers_used+1
       endif

       if_writing: if(world_rank==iwrite) then
          ! This rank writes this block of data
          allocate(to_send(inner_size,toplen))
          allocate(to_recv(inner_size,toplen))
          !$OMP PARALLEL DO PRIVATE(d1,d2,d3)
          do j=1,toplen
             do i=1,inner_size
                to_send(i,j)=0
             enddo
          enddo

!2811      format(I4,': ready to receive block ',I0)
!          print 2811,world_rank,iwrite

          call mpi_reduce(to_send,to_recv,toplen*inner_size,&
               mpi_real,mpi_sum,0,write_comm(iwrite),iret)
          !$OMP PARALLEL DO PRIVATE(d1,d2,d3)
          do j=1,toplen
             do i=1,inner_size
                to_recv(i,j)=to_recv(i,j)/ens_size
             enddo
          enddo
          deallocate(to_send)

!2387      format(I4,': received block ',I4)
!          print 2387,world_rank,iwrite
          exit ! make sure toplen and bounds are right
       else if(world_rank>=write_comm_size) then
          ! This rank read data which it must send to rank iwrite
!1182      format(I4,': reduce send block ',I0)
!          print 1182,world_rank,world_rank-write_comm_size+1

          call mpi_reduce(local(:,bounds(1):bounds(2)),dummy,toplen*inner_size,&
               mpi_real,mpi_sum,0,write_comm(iwrite),iret)

!2118      format(I4,': reduce finished sending block ',I0)
!          print 2118,world_rank,iwrite
       endif if_writing
    enddo foreach_write_rank

    if(world_rank<write_comm_size) then
       ! Write data

       if(.not.allocated(to_recv)) then
          write(0,*) world_rank,' to_recv not allocated'
          call MPI_Abort(mpi_comm_world,1,iret)
       endif

       call check_nf90mpi(nf90mpi_inq_varid(ncid,trim(adjustl(varname)),ncvarid),&
            trim(adjustl(path))//': find variable '//trim(adjustl(varname)))
       start=1
       count(:,1)=dims
       do i=ndims,1,-1
          if(dims(i)>1) then
             start(i,1)=bounds(1)
             count(i,1)=toplen
             exit
          endif
       enddo
       if(world_rank==0) then
482       format(I4,': write var ',A)
          print 482,world_rank,trim(adjustl(varname))
       endif

       call check_nf90mpi(nf90mpi_put_varn_all(ncid,ncvarid,to_recv,1,start,count),&
            trim(adjustl(path))//': write variable '//trim(adjustl(varname)))

       call MPI_Barrier(local_comm,iret)

       if(world_rank==0) then
284       format(I4,': done writing var ',A)
          print 284,world_rank,trim(adjustl(varname))
       endif
    endif

    if(allocated(to_send)) deallocate(to_send)
    if(allocated(to_recv)) deallocate(to_recv)

  end subroutine reduce_and_write

  subroutine check_nf90mpi(status,details)
    use pnetcdf
    use mpi
    implicit none
    integer, intent(in) :: status
    character(len=*), intent(in), optional :: details
    integer :: iret,mype

    if(status /= nf90_noerr) then 
       mype=-1
       call mpi_comm_rank(mpi_comm_world,mype,iret)
222    format('[rank',I4,'] ',A,": ",A)
       if(present(details)) then
          write(0,222) mype,details,nf90mpi_strerror(status)
       else
          write(0,222) mype,'netcdf error',nf90mpi_strerror(status)
       endif
       call MPI_Abort(mpi_comm_world,1,iret)
       stop 1
    end if
  end subroutine check_nf90mpi

end module module_writer

program average_netcdf_files_parallel 

  ! See usage_abort(why) subroutine below for calling conventions

  use mpi
  use netcdf
  use module_writer

  implicit none

  integer, parameter :: num_vars_max = 1000
  logical, parameter :: debug = .true.
  integer, parameter :: ones(10) = (/ 1,1,1,1,1,1,1,1,1,1 /)

  integer :: i, j, num_vars, iret, mype, npe, discard
  integer :: xtype, rcode, ens_size, field_list, natts, dimids(10)
  integer :: dims(4), dims_var(4,num_vars_max)
  integer :: num_dims(num_vars_max)
  integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in
  integer :: ncfileid, ndims, ncvarid, inner_size, outer_size
  integer :: local_comm, write_comm_size, read_index

  integer, allocatable :: write_comms(:)
  real, allocatable :: data(:,:)

  character(len=3) :: charnanal
  character(len=500) :: filenameout,datapath,fileprefix,file_to_open,varname
  character(len=500) :: vars(num_vars_max)

!!!!!!

  ! Initialize mpi
  call mpi_init(iret)
  call mpi_comm_rank(mpi_comm_world,mype,iret)
  call mpi_comm_size(mpi_comm_world,npe,iret)

  if(npe<2) then
     if(mype==0) then
        write(0,'(A)') 'ERROR: at least 2 MPI ranks required'
     endif
     call MPI_Abort(MPI_COMM_WORLD,1,iret)
     stop 1
  endif

  if(command_argument_count()/=5) then
     call usage_abort('PROGRAM IS ABORTING DUE TO INCORRECT ARGUMENTS')
     stop 1
  endif

  ! Get input data and print to screen
  call get_command_argument(1,charnanal)
  read(charnanal,'(i)',iostat=iret) field_list
  if(iret/=0) then
     call usage_abort('PROGRAM IS ABORTING DUE TO NON-INTEGER FIELD LIST (ARGUMENT 1)')
     stop 1
  endif
  if ( (field_list .lt. 1) .or. (field_list .gt. 3) ) then
     field_list = 1
  endif
  call get_command_argument(2,datapath)
  call get_command_argument(3,filenameout)  ! full path to output file
  call get_command_argument(4,fileprefix)
  call get_command_argument(5,charnanal)
  read(charnanal,'(i3)',iostat=iret) ens_size
  if(iret/=0) then
     call usage_abort('PROGRAM IS ABORTING DUE TO NON-INTEGER ENSEMBLE SIZE (ARGUMENT 5)')
     stop 1
  endif
  if(ens_size<1) then
     call usage_abort('PROGRAM IS ABORTING DUE TO NON-POSITIVE ENSEMBLE SIZE (ARGUMENT 5)')
     stop 1
  endif

  if(mype==0) then
     write(*,*)' '
     write(*,*)' field_list    = ',field_list
     write(*,*)' datapath      = ',trim(adjustl(datapath))
     write(*,*)' filenameout   = ',trim(adjustl(filenameout))
     write(*,*)' fileprefix    = ',trim(adjustl(fileprefix))
     write(*,*)' ens_size      = ',ens_size
     write(*,*)' '
  endif
  call mpi_barrier(mpi_comm_world,iret)

  ! Divide into readers and writers, and make comms for sending to the writers:
  write_comm_size=get_write_comm_size(ens_size)
  allocate(write_comms(write_comm_size))
  call io_split_comm(write_comm_size,write_comms,local_comm,read_index,ens_size)

  ! Get list of variables to process from member 1, only on PE 0...then broadcast
  if ( mype==0 ) then

     ! open member 1 file
     file_to_open = trim(adjustl(datapath))//'/'//trim(adjustl(fileprefix))//'001'
     call check_nf90(nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite,ncid=ncfileid),&
          'open '//trim(adjustl(file_to_open)))

     ! make sure the number of variables in the file isn't > num_vars_max
     call check_nf90(nf90_inquire(ncfileid, ndims_in, nvars_in, ngatts_in, unlimdimid_in),&
          'scan contents of '//trim(adjustl(file_to_open)))

     if ( nvars_in .gt. num_vars_max ) then
784     format('Too many variables: ',I0,'>',I0,' in ',A)
        write(0,784) nvars_in,num_vars_max,trim(adjustl(file_to_open))
        call mpi_abort(mpi_comm_world,1,iret)
        stop 1
     endif

     ! get the variables we want to process
     num_vars = 0
     all_vars_in_file: do i = 1,nvars_in

        write(charnanal,'(I0)') i
        call check_nf90(nf90_Inquire_Variable(ncfileid, i, varname, xtype, ndims, dimids, natts), &
             'read var #'//trim(charnanal)//' from '//trim(adjustl(file_to_open)))
        ! Output was varname, xtype,ndims

        if(xtype/=NF90_FLOAT) cycle

        check_list_1: if (field_list .eq. 1) then       ! fields for HRRR initialization
           if ( (varname .ne. 'U') .and. (varname .ne. 'V') .and. (varname .ne. 'W') .and. (varname .ne. 'T') .and. (varname .ne. 'QVAPOR') &
                .and. (varname .ne. 'MU') .and. (varname .ne. 'PH') .and. (varname .ne. 'MUB') .and. (varname .ne. 'PHB') &
                .and. (varname .ne. 'QRAIN') .and. (varname .ne. 'QNRAIN') .and. (varname .ne. 'QSNOW') &
                .and. (varname .ne. 'QGRAUP') .and. (varname .ne. 'QICE') .and. (varname .ne. 'QNICE') &
                .and. (varname .ne. 'QCLOUD') .and. (varname .ne. 'QNCLOUD') .and. (varname .ne. 'REFL_10CM') &
                .and. (varname .ne. 'QNIFA') .and. (varname .ne. 'QNWFA') &
                .and. (varname .ne. 'Q2') .and. (varname .ne. 'T2') .and. (varname .ne. 'TH2') .and. (varname .ne. 'TSK') .and. (varname .ne. 'PSFC') &
                .and. (varname .ne. 'U10') .and. (varname .ne. 'V10') .and. (varname .ne. 'WSPD10') .and. (varname .ne. 'WSPD80') &
                .and. (varname .ne. 'TSLB') .and. (varname .ne. 'SMOIS') .and. (varname .ne. 'SWDOWN') &
                .and. (varname .ne. 'P') .and. (varname .ne. 'PB') .and. (varname .ne. 'P_HYD') &
                .and. (varname .ne. 'RAINNC') .and. (varname .ne. 'RAINC') .and. (varname .ne. 'PREC_ACC_NC') & 
                .and. (varname .ne. 'WSPD10') .and. (varname .ne. 'WSPD80') .and. (varname .ne. 'W_UP_MAX') ) then
              cycle
           endif
        endif check_list_1

        check_list_2: if (field_list .eq. 2) then       ! fields for GSI observer
           if ( (varname .ne. 'U') .and. (varname .ne. 'V') .and. (varname .ne. 'W') .and. (varname .ne. 'T') &
                .and. (varname .ne. 'MU') .and. (varname .ne. 'PH') .and. (varname .ne. 'MUB') .and. (varname .ne. 'PHB') &
                .and. (varname .ne. 'QVAPOR') .and. (varname .ne. 'REFL_10CM') &
                .and. (varname .ne. 'Q2') .and. (varname .ne. 'T2') .and. (varname .ne. 'TH2') .and. (varname .ne. 'TSK') &
                .and. (varname .ne. 'PSFC') .and. (varname .ne. 'U10') .and. (varname .ne. 'V10') ) then
              cycle
           endif
        endif check_list_2

        num_vars = num_vars + 1
        vars(num_vars) = varname
        num_dims(num_vars) = ndims
        do j = 1,ndims
           write(charnanal,'(I0)') j
           call check_nf90(nf90_inquire_dimension( ncfileid, dimids(j), len=dims(j) ), &
                'get dim #'//trim(charnanal)//' from '//trim(adjustl(file_to_open)))
        enddo
        dims_var(1:4,num_vars) = dims(1:4)
303     format("process ",A,' var "',A,'" dims (',I0,',',I0,',',I0,',',I0,')')
        print 303,trim(adjustl(file_to_open)),trim(adjustl(varname)),dims(1:4)
        !if ( debug ) write(*,*)'processing ',trim(adjustl(varname))
     enddo all_vars_in_file


     write(*,*)''
     write(*,*)'There are total ',nvars_in,' variables in the file...'
     write(*,*)'...but just ',num_vars,' variables are floats that will be processed.'
     write(*,*)''

     discard = nf90_close(ncfileid) ! close file
  endif

  ! now broadcast stuff to other processors
  call mpi_bcast(num_vars,1,mpi_integer,0,mpi_comm_world,iret)
  call mpi_bcast(num_dims,num_vars_max,mpi_integer,0,mpi_comm_world,iret)
  call mpi_bcast(vars, num_vars_max*500 ,mpi_character,0,mpi_comm_world,iret) !  note...have to brodcast size of array times character length
  call mpi_bcast(dims_var,num_vars_max*4,mpi_integer,0,mpi_comm_world,iret)

  if(read_index>0) then
     write(charnanal,'(i3.3)') read_index
     file_to_open = trim(adjustl(datapath))//'/'//trim(adjustl(fileprefix))//charnanal
     call check_nf90(nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite,ncid=ncfileid),&
          'open '//trim(adjustl(file_to_open)))
  else
     call open_pnetcdf_for_write(local_comm,trim(adjustl(filenameout)),ncfileid)
  endif

  ! now loop over variables and average.
  do i = 1,num_vars ! All processors loop over the number of variables to process

     ! initialize data array to zero--this is unique to each PE
     if ( num_dims(i)==3 ) then
        inner_size=1
        outer_size=1
        do j=3,1,-1
           if(outer_size>1) then
              inner_size=inner_size*dims_var(j,i)
           else
              outer_size=outer_size*dims_var(j,i)
           endif
        enddo
     else if ( num_dims(i).eq.4) then
        inner_size=1
        outer_size=1
        do j=4,1,-1
           if(outer_size>1) then
              inner_size=inner_size*dims_var(j,i)
           else
              outer_size=outer_size*dims_var(j,i)
           endif
        enddo
     else
        if ( mype .eq. 0 ) write(*,*) 'variable ',trim(adjustl(vars(i))),' has only ',num_dims(i),' dimensions. cycle.'
        cycle
     endif

     if(read_index>0) then
        allocate(data(inner_size,outer_size))
        if( read_index==1 .and. debug) then
           print '(I4,A,A,A,4i5)',mype,': processing ',trim(adjustl(vars(i))),', which has dimensions: ',dims_var(1:num_dims(i),i)
        endif
        call check_nf90(nf90_inq_varid(ncfileid,trim(adjustl(vars(i))),ncvarid), &
             trim(adjustl(file_to_open))//': find var '//trim(adjustl(vars(i))))
        call check_nf90(nf90_get_var(ncid=ncfileid,varid=ncvarid,values=data,count=dims_var(:,i),start=ones,stride=ones),&
             trim(adjustl(file_to_open))//': read var '//trim(adjustl(vars(i))))
! 3372    format(I4,': ',A)
!         if(debug) then
!            print 3372,mype,'barrier after read...'
!         endif
        call mpi_barrier(local_comm,iret)
!         if(read_index==1 .and. debug) then
!            print 3372,mype,'...barrier end'
!         endif
        if( read_index==1 .and. debug) then
           write(*,fmt='(I4,A,A)') mype,': all read ranks are ready to send ',trim(adjustl(vars(i)))
        endif
     else
        allocate(data(1,1))
     endif

     call reduce_and_write(data,write_comm_size,write_comms,num_dims(i),dims_var(:,i), &
          inner_size,outer_size,ens_size,ncfileid,trim(adjustl(filenameout)), &
          trim(adjustl(vars(i))),local_comm)

     deallocate(data)
  enddo ! end loop over variables

91 format(I4,': ',A)

!   print 91,mype,'final mpi_barrier...'
!   call MPI_Barrier(MPI_COMM_WORLD,iret)
!   if(mype==0) then
!      print 91,mype,'...final mpi_barrier end'
!   endif

  !deallocate(ista,iend)
  if(read_index==0) then
     if(mype==0) then
        print 91,mype,'close output file'
     endif
     call close_pnetcdf(trim(adjustl(filenameout)),ncfileid)
  else
     ! close input file
     call check_nf90(nf90_close(ncfileid),"close"//trim(adjustl(file_to_open)))
  endif

  if(mype==0)  print 91,mype,'finalize...'

  call mpi_finalize(iret)

  stop

!!!!!!!!!! END OF MAIN PROGRAM !!!!!!!!!

contains

  subroutine check_nf90(status,details)
    use netcdf
    use mpi
    implicit none
    integer, intent(in) :: status
    character(len=*), intent(in), optional :: details
    integer :: iret,mype

    if(status /= nf90_noerr) then 
       mype=-1
       call mpi_comm_rank(mpi_comm_world,mype,iret)
222    format('[rank',I4,'] ',A,": ",A)
       if(present(details)) then
          write(0,222) mype,details,nf90_strerror(status)
       else
          write(0,222) mype,'netcdf error',nf90_strerror(status)
       endif
       call MPI_Abort(mpi_comm_world,1,iret)
       stop 1
    end if
  end subroutine check_nf90

  subroutine usage_abort(why)
    implicit none
    character(len=*), intent(in) :: why

    if(mype==0) then
       write(0,'(A)') 'Syntax: hrrr_cal_ensmean [1|2] /path/to/input/files/ /path/to/outfile input-prefix SIZE'
       write(0,'(A)') '  [1|2] = field list 1 (lots of fields) or 2 (a few fields)'
       write(0,'(A)') '  /path/to/input/files/ = directory with input data'
       write(0,'(A)') '  /path/to/outfile = full path to output file'
       write(0,'(A)') '  input-prefix = prefix to append to input file names'
       write(0,'(A)') '  SIZE = ensemble size'
       write(0,'(A)') 'Input files are /path/to/input/files/input-prefixNNN for NNN=1..SIZE'
       write(0,'(A)') 'The last SIZE MPI ranks will read files and the rest will write.'
       write(0,'(A)') ' '
       write(0,'(A)') why
    endif
    call MPI_Abort(mpi_comm_world,1,iret)
    stop 1
  end subroutine usage_abort
end program average_netcdf_files_parallel
