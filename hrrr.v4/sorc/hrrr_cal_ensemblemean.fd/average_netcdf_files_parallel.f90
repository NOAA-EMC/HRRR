program average_netcdf_files_parallel 

! commands to build on jet:
!
! module load intel/18.0.5.274
! module load mvapich2/2.3
! module load szip
! module load hdf5
! module load netcdf/4.2.1.1
! mpif90 -L${NETCDF}/lib -lnetcdf -lnetcdff -I${NETCDF}/include average_netcdf_files_parallel.f90 -o average_netcdf_files_parallel.x

use netcdf

implicit none

integer, parameter :: num_vars_max = 1000

integer :: ncstatus, ncfileid, ndims, ncvarid, ncfileid_out
integer :: i,j,num_vars
integer, dimension(4) :: dims
integer :: dims_var(num_vars_max,4)

character(len=500) :: input_file,varname,vars(num_vars_max)
character(len=500) filenamein,filenameout,datapath,fileprefix,file_to_open
character(len=3) charnanal

integer :: xtype, rcode, ens_size, natts, dimids(10)
integer :: num_dims(num_vars_max)
integer :: ndims_in, nvars_in, ngatts_in, unlimdimid_in

real :: rnanals

logical :: debug = .true.
real, allocatable  :: data_3d(:,:,:),dummy_3d(:,:,:)
real, allocatable  :: data_4d(:,:,:,:),dummy_4d(:,:,:,:)

! mpi variables
integer :: mype,npe,iret,irank,ie
integer :: mype_out = 0
integer , allocatable,dimension(:) :: ista, iend

! mpi definitions.
include 'mpif.h'

!!!!!!

! Initialize mpi
call mpi_init(iret)
call mpi_comm_rank(mpi_comm_world,mype,iret)
call mpi_comm_size(mpi_comm_world,npe,iret)

! Get input data and print to screen
call getarg(1,datapath)
call getarg(2,filenameout)  ! full path to output file
call getarg(3,fileprefix)
call getarg(4,charnanal)
read(charnanal,'(i3)') ens_size
rnanals=1.0/ens_size

if ( mype .eq. mype_out ) then
  write(*,*)' '
  write(*,*)' datapath      = ',trim(adjustl(datapath))
  write(*,*)' filenameout   = ',trim(adjustl(filenameout))
  write(*,*)' fileprefix    = ',trim(adjustl(fileprefix))
  write(*,*)' ens_size,rnanals= ',ens_size,rnanals
  write(*,*)' '
endif
call mpi_barrier(mpi_comm_world,iret)

!  Divide the total number of the input files among the available processors
allocate (ista(0:npe-1), iend(0:npe-1))
do irank = 0, npe - 1
   call para_range(1, ens_size, npe, irank, ista(irank), iend(irank))
end do
write(*,fmt='(a15,i4,a29,i3,a4,i3)') 'The processor ', mype, ' will read files index from ', ista(mype), ' to ', iend(mype)

! get list of variables to process from member 1, only on PE 0...then broadcast
if ( mype .eq. mype_out ) then

   ! open member 1 file
   file_to_open = trim(adjustl(datapath))//'/'//trim(adjustl(fileprefix))//'001'
   rcode = nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite,ncid=ncfileid)
   if ( rcode.ne.0) then
      write(*,*)'Error opening ',trim(adjustl(file_to_open))
      call mpi_finalize(iret)
      stop
   endif

   ! make sure the number of variables in the file isn't > num_vars_max
   rcode = nf90_inquire(ncfileid, ndims_in, nvars_in, ngatts_in, unlimdimid_in)
   if ( nvars_in .gt. num_vars_max ) then
      write(*,*)'Too many variables in ',trim(adjustl(file_to_open))
      call mpi_finalize(iret)
      stop
   endif

   ! get the variables we want to process
   num_vars = 0
   do i = 1,nvars_in ! loop over all variables in file

      rcode = nf90_Inquire_Variable(ncfileid, i, varname, xtype, ndims, dimids, natts) ! Output is varname, xtype,ndims






      if ( (varname .ne. 'U') .and. (varname .ne. 'V') .and. (varname .ne. 'W') .and. (varname .ne. 'T') .and. (varname .ne. 'QVAPOR') &
           .and. (varname .ne. 'MU') .and. (varname .ne. 'PH') .and. (varname .ne. 'MUB') .and. (varname .ne. 'PHB') .and. (varname .ne. 'W') &
           .and. (varname .ne. 'QRAIN') .and. (varname .ne. 'QNRAIN') .and. (varname .ne. 'QSNOW') &
           .and. (varname .ne. 'QGRAUP') .and. (varname .ne. 'QICE') .and. (varname .ne. 'QNICE') &
           .and. (varname .ne. 'QCLOUD') .and. (varname .ne. 'QNCLOUD') .and. (varname .ne. 'REFL_10CM') &
           .and. (varname .ne. 'QNIFA') .and. (varname .ne. 'QNWFA') &
           .and. (varname .ne. 'Q2') .and. (varname .ne. 'T2') .and. (varname .ne. 'TH2') .and. (varname .ne. 'TSK') .and. (varname .ne. 'PSFC') &
           .and. (varname .ne. 'U10') .and. (varname .ne. 'V10') .and. (varname .ne. 'WSPD10') .and. (varname .ne. 'WSPD80') &
           .and. (varname .ne. 'TSLB') .and. (varname .ne. 'SMOIS') .and. (varname .ne. 'SWDOWN') &
           .and. (varname .ne. 'P') .and. (varname .ne. 'PB') .and. (varname .ne. 'P_HYD') &
           .and. (varname .ne. 'RAINNC') .and. (varname .ne. 'RAINC') .and. (varname .ne. 'PREC_ACC_NC') & 
           .and. (varname .ne. 'WSPD10') .and. (varname .ne. 'WSPD80') .and. (varname .ne. 'W_UP_MAX') & 
           .and. (varname .ne. 'SWDOWN') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '') &
!           .and. (varname .ne. '') .and. (varname .ne. '')
            ) then
        rcode = -1
      endif






      if ( rcode .eq. 0 ) then
         if ( xtype.eq.5) then ! Only process floats (xtype = 5 is float) What if it's a double? ( xtype .eq. NF90_FLOAT/NF90_DOUBLE ??)
            num_vars = num_vars + 1
            vars(num_vars) = varname
            num_dims(num_vars) = ndims
            do j = 1,ndims
               rcode = nf90_inquire_dimension( ncfileid, dimids(j), len=dims(j) )
            enddo
            dims_var(num_vars,1:4) = dims(1:4)
           !if ( debug ) write(*,*)'processing ',trim(adjustl(varname))
         endif
       endif
   enddo


   write(*,*)''
   write(*,*)'There are total ',nvars_in,' variables in the file...'
   write(*,*)'...but just ',num_vars,' variables are floats that will be processed.'
   write(*,*)''

   rcode = nf90_close(ncfileid) ! close file

   ! open the output file for writing
   rcode = nf90_open(path=trim(adjustl(filenameout)), mode=nf90_write,ncid=ncfileid_out)
   if ( rcode .ne. 0 ) then
      write(*,*) 'Error opening ',trim(adjustl(filenameout)),' for writing.'
      call mpi_finalize(iret)
      stop
   endif
endif

! now broadcast stuff to other processors
call mpi_bcast(num_vars,1,mpi_integer,0,mpi_comm_world,iret)
call mpi_bcast(num_dims,num_vars_max,mpi_integer,0,mpi_comm_world,iret)
call mpi_bcast(vars, num_vars_max*500 ,mpi_character,0,mpi_comm_world,iret) !  note...have to brodcast size of array times character length
call mpi_bcast(dims_var,num_vars_max*4,mpi_integer,0,mpi_comm_world,iret)

! now loop over variables and average.
do i = 1,num_vars ! All processors loop over the number of variables to process

   ! initialize data array to zero--this is unique to each PE
   if ( num_dims(i).eq.3) then
      allocate(data_3d(dims_var(i,1),dims_var(i,2),dims_var(i,3)))
      allocate(dummy_3d(dims_var(i,1),dims_var(i,2),dims_var(i,3)))
      data_3d = 0.0
   else if ( num_dims(i).eq.4) then
      allocate(data_4d(dims_var(i,1),dims_var(i,2),dims_var(i,3),dims_var(i,4)))
      allocate(dummy_4d(dims_var(i,1),dims_var(i,2),dims_var(i,3),dims_var(i,4)))
      data_4d = 0.0
   else
      if ( mype .eq. mype_out ) write(*,*) 'variable ',trim(adjustl(vars(i))),' has only ',num_dims(i),' dimensions. cycle.'
      cycle
   endif

   if( mype.eq.mype_out .and. debug) write(*,fmt='(a12,a15,a25,4i4)') 'Processing ',trim(adjustl(vars(i))),', which has dimensions: ',dims_var(i,1:num_dims(i))

   do ie = ista(mype), iend(mype) ! Loop over the files handled by this processor

      write(charnanal,'(i3.3)') ie
      file_to_open = trim(adjustl(datapath))//'/'//trim(adjustl(fileprefix))//charnanal

      rcode = nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite,ncid=ncfileid)
      if ( rcode.ne.0) then
         write(*,*)'Error opening ',trim(adjustl(file_to_open)),' from PE ',mype
         call mpi_finalize(iret)
         stop
      endif

      if ( num_dims(i).eq.3) then
         call get_netcdf_var_3d_real(ncfileid,trim(adjustl(vars(i))),dims_var(i,1:4),dummy_3d)
        !if(debug) write(*,*) ' got data for ',trim(adjustl(vars(i))),' from PE ',mype
         data_3d = data_3d + dummy_3d
      else if ( num_dims(i).eq.4) then
         call get_netcdf_var_4d_real(ncfileid,trim(adjustl(vars(i))),dims_var(i,1:4),dummy_4d)
        !if(debug) write(*,*) ' got data for ',trim(adjustl(vars(i))),' from PE ',mype
         data_4d = data_4d + dummy_4d
      endif

      rcode = nf90_close(ncfileid) ! close file

   enddo ! end loop over files handled by this processor.

   call mpi_barrier(mpi_comm_world,iret)

   ! Sum up, take mean, output, clean-up, then go onto next variable
   if ( num_dims(i).eq.3) then
      call mpi_allreduce(data_3d, dummy_3d, product(dims_var(i,1:3)), mpi_real,mpi_sum,mpi_comm_world,iret)
      dummy_3d = dummy_3d * rnanals
      if (mype .eq. mype_out ) then
         rcode = nf90_inq_varid(ncfileid_out,trim(adjustl(vars(i))),ncvarid)
         rcode = nf90_put_var(ncfileid_out,ncvarid,dummy_3d)
         if ( rcode .ne. 0 ) then
            write(*,*) 'Error outputing ',trim(adjustl(vars(i)))
            call mpi_finalize(iret)
            stop
         endif
      endif
      deallocate(data_3d,dummy_3d)
   else if ( num_dims(i).eq.4) then
      call mpi_allreduce(data_4d, dummy_4d, product(dims_var(i,1:4)), mpi_real,mpi_sum,mpi_comm_world,iret)
      dummy_4d = dummy_4d * rnanals
      if (mype .eq. mype_out ) then
         rcode = nf90_inq_varid(ncfileid_out,trim(adjustl(vars(i))),ncvarid)
         rcode = nf90_put_var(ncfileid_out,ncvarid,dummy_4d)
         if ( rcode .ne. 0 ) then
            write(*,*) 'Error outputing ',trim(adjustl(vars(i)))
            call mpi_finalize(iret)
            stop
         endif
      endif
      deallocate(data_4d,dummy_4d)
   endif

enddo ! end loop over variables

! clean up
call mpi_finalize(iret)
deallocate(ista,iend)
if ( mype .eq. mype_out ) then
   rcode =  nf90_close(ncfileid_out)
   if ( rcode .ne. 0 ) then
      write(*,*) 'Error closing ',trim(adjustl(filenameout))
      stop
   else
      write(*,*)'All done!'
   endif
endif

stop

!!!!!!!!!! END OF MAIN PROGRAM !!!!!!!!!

contains

subroutine get_dims_cdf( cdfid, var, dims, ndims, debug )
  implicit none

  integer, intent(in) :: cdfid
  character (len=*), intent(in) :: var
  integer, intent(inout), dimension(4) :: dims
  integer, intent(inout) :: ndims
  logical, intent(in ) :: debug

  integer :: rcode, id_time
  character (len=80) :: varname
  integer :: natts, istart(10),iend(10), dimids(10)
  integer :: i, ivtype, ncvarid
  integer :: xtype

! rcode = nf90_inq_varid( cdfid, trim(adjustl(var)), ncvarid)
! rcode = nf90_Inquire_Variable(cdfid, ncvarid, varname, xtype, ndims, dimids, natts)
! if(debug) write(6,*) ' number of dims for ',trim(adjustl(var)),' ',ndims

! experiemnting
  rcode = nf90_Inquire_Variable(cdfid, 1, varname, xtype, ndims, dimids, natts)
  if(debug) write(6,*) ' number of dims for ',trim(adjustl(varname)),' ',ndims

  rcode = nf90_Inquire_Variable(cdfid, 2, varname, xtype, ndims, dimids, natts)
  if(debug) write(6,*) ' number of dims for ',trim(adjustl(varname)),' ',ndims

  rcode = nf90_Inquire_Variable(cdfid, 1000, varname, xtype, ndims, dimids, natts)
  if(debug) write(6,*) ' rcode = ',rcode

  if(debug) write(6,*) ' number of dims for ',trim(adjustl(varname)),' ',ndims

  do i=1,ndims
    rcode = nf90_inquire_dimension( cdfid, dimids(i), len=dims(i) )
    if(debug) write(6,*) ' dimension ',i,dims(i)
  enddo

end subroutine get_dims_cdf

!!!!!!! 3d vars

subroutine get_netcdf_var_3d_real(fileid,variable,dims,output)

   integer, intent(in) :: fileid, dims(4)
   character(len=*), intent(in) :: variable
   real, intent(inout), dimension(dims(1),dims(2),dims(3)) :: output

   integer :: ncstatus, ncvarid, istatus

   istatus = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid)
   if ( ncstatus /= 0 ) then
      write(*,*) 'Error inq_varid '//trim(adjustl(variable)) ; istatus = istatus + ncstatus
   endif
   ncstatus = nf90_get_var(fileid,ncvarid,output)
   if ( ncstatus /= 0 ) then
      write(*,*) 'Error inq_get_var '//trim(adjustl(variable)) ; istatus = istatus + ncstatus
   endif

   if ( istatus /= 0 ) output = 0.0  ! Set to zero if error

end subroutine get_netcdf_var_3d_real

!!!!!! 4d vars

subroutine get_netcdf_var_4d_real(fileid,variable,dims,output)

   integer, intent(in) :: fileid, dims(4)
   character(len=*), intent(in) :: variable
   real, intent(inout), dimension(dims(1),dims(2),dims(3),dims(4)) :: output

   integer :: ncstatus, ncvarid, istatus

   istatus = 0
   ncstatus = nf90_inq_varid(fileid,trim(adjustl(variable)),ncvarid)
   if ( ncstatus /= 0 ) then
      write(*,*) 'Error inq_varid '//trim(adjustl(variable)) ; istatus = istatus + ncstatus
   endif
   ncstatus = nf90_get_var(fileid,ncvarid,output)
   if ( ncstatus /= 0 ) then
      write(*,*) 'Error inq_get_var '//trim(adjustl(variable)) ; istatus = istatus + ncstatus
   endif

   if ( istatus /= 0 ) output = 0.0  ! Set to zero if error

end subroutine get_netcdf_var_4d_real

!!!!!!! other routines

subroutine para_range(n1, n2, nprocs, irank, ista, iend)

  implicit none

  integer, intent(in) :: n1, n2, nprocs, irank
  integer, intent(out) :: ista, iend

  integer :: iwork1, iwork2

  iwork1 = (n2 - n1 + 1) / nprocs
  iwork2 = mod(n2 - n1 + 1, nprocs)
  ista = irank * iwork1 + n1 + min(irank, iwork2)
  iend = ista + iwork1 - 1
  if (iwork2 > irank) iend = iend + 1
  return
end subroutine para_range

subroutine check(status)
   integer, intent ( in) :: status
   if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
   end if
end subroutine check 

!------------

end program average_netcdf_files_parallel 
