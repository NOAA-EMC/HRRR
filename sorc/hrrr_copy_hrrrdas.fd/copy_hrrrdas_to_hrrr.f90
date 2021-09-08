program copy_hrrrdas_to_hrrr

! commands to build on jet
!
! module purge
! module load intel/18.0.5.274
! module load mvapich2/2.3
! module load szip
! module load hdf5
! module load netcdf/4.2.1.1
! mpif90 -L${NETCDF}/lib -lnetcdf -lnetcdff -I${NETCDF}/include copy_hrrrdas_to_hrrr.f90 -o copy_hrrrdas_to_hrrr.x

! Copy fields from a HRRRDAS d02 WRF netcdf file to a HRRR d01 WRF netcdf file.
! The following must be true for this program to work correctly:
! * The HRRR d01 and HRRRDAS d02 grids are identical except for the extra gridpoints in HRRRDAS d02.
! * The extra gridpoints in the HRRRDAS d02 are on the east and north sides.

! Program written by David Dowell, with contributions from Craig Schwartz, December 2019

! usage:  copy_hrrrdas_to_hrrr <source file path> <target file path>

use netcdf

implicit none

integer, parameter :: num_vars_max = 1000

integer :: ncstatus, ncfileid_source, ndims, ndims_target, ncvarid, ncfileid_target
integer :: i,j,ii,jj,k,t,num_vars
integer, dimension(4) :: dims
integer :: dims_var_source(num_vars_max,4), dims_var_target(num_vars_max,4)

character(len=500) :: varname,varname_target,vars(num_vars_max)
character(len=500) :: sourcefilename,targetfilename

integer :: xtype, xtype_target, rcode, rcode_target, natts, dimids(10)
integer :: num_dims(num_vars_max)

logical :: debug = .true.
real, allocatable  :: data_3d_source(:,:,:), data_3d_target(:,:,:)
real, allocatable  :: data_4d_source(:,:,:,:), data_4d_target(:,:,:,:)

! mpi variables
integer :: mype,npe,iret,irank,ie
integer :: mype_out = 0
integer , allocatable,dimension(:) :: ista, iend

! mpi definitions.
include 'mpif.h'

!!!!!!

! Hard-coded values, which could be moved to a namelist or command-line input
num_vars = 40
vars(1)='U'
vars(2)='V'
vars(3)='W'
vars(4)='PH'
vars(5)='PHB'
vars(6)='T'
vars(7)='MU'
vars(8)='MUB'
vars(9)='P'
vars(10)='PB'
vars(11)='P_HYD'
vars(12)='Q2'
vars(13)='T2'
vars(14)='TH2'
vars(15)='PSFC'
vars(16)='U10'
vars(17)='V10'
vars(18)='WSPD10'
vars(19)='WSPD80'
vars(20)='QVAPOR'
vars(21)='QCLOUD'
vars(22)='QRAIN'
vars(23)='QICE'
vars(24)='QSNOW'
vars(25)='QGRAUP'
vars(26)='QNICE'
vars(27)='QNRAIN'
vars(28)='QNCLOUD'
vars(29)='TSLB'
vars(30)='SMOIS'
vars(31)='TSK'
vars(32)='RAINC'
vars(33)='RAINNC'
vars(34)='REFL_10CM'
vars(35)='SWDOWN'
vars(36)='W_UP_MAX'
vars(37)='PREC_ACC_NC'
vars(38)='CLDFRA_BL'
vars(39)='QC_BL'
vars(40)='QKE'

!!!!!!

! Initialize mpi
call mpi_init(iret)
call mpi_comm_rank(mpi_comm_world,mype,iret)
call mpi_comm_size(mpi_comm_world,npe,iret)

! Get input data and print to screen
call getarg(1,sourcefilename)  ! full path to source file
call getarg(2,targetfilename)  ! full path to target file

! open source file (read only)
rcode = nf90_open(path=trim(adjustl(sourcefilename)), mode=nf90_nowrite, ncid=ncfileid_source)
if ( rcode.ne.0) then
   write(*,*)'Error opening ',trim(adjustl(sourcefilename))
   call mpi_finalize(iret)
   stop
endif

! open the target file (read and write)
rcode = nf90_open(path=trim(adjustl(targetfilename)), mode=nf90_write, ncid=ncfileid_target)
if ( rcode .ne. 0 ) then
   write(*,*) 'Error opening ',trim(adjustl(targetfilename)),' for writing.'
   call mpi_finalize(iret)
   stop
endif

if ( mype .eq. mype_out ) then

   write(*,*)' '
   write(*,*)' npe            = ',npe
   write(*,*)' sourcefilename = ',trim(adjustl(sourcefilename))
   write(*,*)' targetfilename = ',trim(adjustl(targetfilename))
   write(*,*)' '


   ! get information about variables we want to process
   do i = 1, num_vars

      rcode = nf90_inq_varid(ncfileid_source, vars(i), ncvarid)
      rcode = nf90_inquire_variable(ncfileid_source, ncvarid, varname, xtype, ndims, dimids, natts) ! Output is varname, xtype, ndims

      if ( rcode .eq. 0 ) then
         if ( xtype .eq. 5) then ! Only process floats (xtype = 5 is float)
            num_dims(i) = ndims
            do j = 1, ndims
               rcode = nf90_inquire_dimension( ncfileid_source, dimids(j), len=dims(j) )
            enddo
            dims_var_source(i,1:4) = dims(1:4)
           !if ( debug ) write(*,*)'processing ',trim(adjustl(varname))

            rcode = nf90_inq_varid(ncfileid_target, vars(i), ncvarid)
            rcode_target = nf90_inquire_variable(ncfileid_target, ncvarid, varname_target, xtype_target, ndims_target, dimids, natts) ! Output is varname, xtype, ndims

            if (rcode_target .ne. 0) then
                write(*,*)'ERROR:  unable to obtain information about variable in target file'
                call mpi_finalize(iret)
                stop
            endif
            if (xtype_target .ne. 5) then
                write(*,*)'ERROR:  invalid value of xtype_target = ', xtype_target
                call mpi_finalize(iret)
                stop
            endif
            if (varname .ne. varname_target) then
               write(*,*)'ERROR:  variable names in source and target not the same for variable ', i
               write(*,*)'source varname = ', varname
               write(*,*)'target varname = ', varname_target
               call mpi_finalize(iret)
               stop
            endif
            if (ndims .ne. ndims_target) then
               write(*,*)'ERROR:  variable dimensions in source and target not the same for variable ', i
               write(*,*)'source ndims = ', ndims
               write(*,*)'target ndims = ', ndims_target
               call mpi_finalize(iret)
               stop
            endif
            do j = 1, ndims_target
               rcode = nf90_inquire_dimension( ncfileid_target, dimids(j), len=dims(j) )
            enddo
            dims_var_target(i,1:4) = dims(1:4)

!            write(*,*)
!            write(*,*) trim(adjustl(varname))
!            write(*,*) dims(1)
!            write(*,*) dims(2)
!            write(*,*) dims(3)
!            write(*,*) dims(4)

         endif
       endif

   enddo

endif

call mpi_barrier(mpi_comm_world,iret)

! now broadcast stuff to other processors
call mpi_bcast(num_vars,1,mpi_integer,0,mpi_comm_world,iret)
call mpi_bcast(num_dims,num_vars_max,mpi_integer,0,mpi_comm_world,iret)
call mpi_bcast(vars, num_vars_max*500 ,mpi_character,0,mpi_comm_world,iret) !  note...have to brodcast size of array times character length
call mpi_bcast(dims_var_source,num_vars_max*4,mpi_integer,0,mpi_comm_world,iret)
call mpi_bcast(dims_var_target,num_vars_max*4,mpi_integer,0,mpi_comm_world,iret)

!  Divide the total number of variables among the available processors
allocate (ista(0:npe-1), iend(0:npe-1))
do irank = 0, npe - 1
   call para_range(1, num_vars, npe, irank, ista(irank), iend(irank))
end do
write(*,fmt='(a15,i4,a33,i3,a4,i3)') 'The processor ', mype, ' will read variable indices from ', ista(mype), ' to ', iend(mype)


! now loop over variables for each processor
do i = ista(mype), iend(mype)

   ! initialize data array to zero--this is unique to each PE
   if ( num_dims(i).eq.3) then
      allocate(data_3d_source(dims_var_source(i,1),dims_var_source(i,2),dims_var_source(i,3)))
      data_3d_source = 0.0
      allocate(data_3d_target(dims_var_target(i,1),dims_var_target(i,2),dims_var_target(i,3)))
      data_3d_target = 0.0
   else if ( num_dims(i).eq.4) then
      allocate(data_4d_source(dims_var_source(i,1),dims_var_source(i,2),dims_var_source(i,3),dims_var_source(i,4)))
      data_4d_source = 0.0
      allocate(data_4d_target(dims_var_target(i,1),dims_var_target(i,2),dims_var_target(i,3),dims_var_target(i,4)))
      data_4d_target = 0.0
   else
      if ( mype .eq. mype_out ) write(*,*) 'variable ',trim(adjustl(vars(i))),' has only ',num_dims(i),' dimensions. cycle.'
      cycle
   endif

   if( debug) write(*,fmt='(a12,a15,a25,4i5)') 'Processing ',trim(adjustl(vars(i))),', which has dimensions: ',dims_var_source(i,1:num_dims(i))

   if ( num_dims(i).eq.3) then
      call get_netcdf_var_3d_real(ncfileid_source,trim(adjustl(vars(i))),dims_var_source(i,1:4),data_3d_source)
     !if(debug) write(*,*) ' got data for ',trim(adjustl(vars(i))),' from PE ',mype

      do t=1, dims_var_target(i,3)
        do jj=1, dims_var_target(i,2)
          do ii=1, dims_var_target(i,1)
            data_3d_target(ii, jj, t) = data_3d_source(ii, jj, t)
          enddo
        enddo
      enddo

      rcode = nf90_inq_varid(ncfileid_target,trim(adjustl(vars(i))),ncvarid)
      rcode = nf90_put_var(ncfileid_target,ncvarid,data_3d_target)
      if ( rcode .ne. 0 ) then
         write(*,*) 'Error outputing ',trim(adjustl(vars(i)))
         call mpi_finalize(iret)
         stop
      endif
      deallocate(data_3d_source, data_3d_target)

   else if ( num_dims(i).eq.4) then
      call get_netcdf_var_4d_real(ncfileid_source,trim(adjustl(vars(i))),dims_var_source(i,1:4),data_4d_source)
     !if(debug) write(*,*) ' got data for ',trim(adjustl(vars(i))),' from PE ',mype

      do t=1, dims_var_target(i,4)
        do k=1, dims_var_target(i,3)
          do jj=1, dims_var_target(i,2)
            do ii=1, dims_var_target(i,1)
              data_4d_target(ii, jj, k, t) = data_4d_source(ii, jj, k, t)
            enddo
          enddo
        enddo
      enddo

      rcode = nf90_inq_varid(ncfileid_target,trim(adjustl(vars(i))),ncvarid)
      rcode = nf90_put_var(ncfileid_target,ncvarid,data_4d_target)
      if ( rcode .ne. 0 ) then
         write(*,*) 'Error outputing ',trim(adjustl(vars(i)))
         call mpi_finalize(iret)
         stop
      endif
      deallocate(data_4d_source, data_4d_target)

   endif

enddo ! end loop over variables handled by this processor.

call mpi_barrier(mpi_comm_world,iret)

! clean up
call mpi_finalize(iret)
deallocate(ista,iend)
if ( mype .eq. mype_out ) then
   rcode =  nf90_close(ncfileid_source)
   if ( rcode .ne. 0 ) then
      write(*,*) 'Error closing ',trim(adjustl(sourcefilename))
      stop
   endif
   rcode =  nf90_close(ncfileid_target)
   if ( rcode .ne. 0 ) then
      write(*,*) 'Error closing ',trim(adjustl(targetfilename))
      stop
   else
      write(*,*)'All done!'
   endif
endif

stop

!!!!!!!!!! END OF MAIN PROGRAM !!!!!!!!!

contains

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

end program copy_hrrrdas_to_hrrr
