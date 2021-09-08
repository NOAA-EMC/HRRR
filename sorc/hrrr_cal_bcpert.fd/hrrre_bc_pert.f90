program hrrre_bc_pert

! David Dowell, 15 October 2019
!
! commands to build on jet:
!
! module load intel/18.0.5.274
! module load mvapich2/2.3
! module load szip
! module load hdf5
! module load netcdf/4.2.1.1
! mpif90 -L${NETCDF}/lib -lnetcdf -lnetcdff -I${NETCDF}/include hrrre_bc_pert.f90 -o hrrre_bc_pert.x
!
! command-line variables:
! (1) wrfbdy file name, including path
! (2) wrfinput file name, including path
! (3) perturbation bank directory

use netcdf

implicit none

! command-line variables
character(len=500) :: wrfbdy_file_name
character(len=500) :: wrfinput_file_name
character(len=500) :: pert_bank_dir

! hard-coded values that could be moved to a namelist
integer :: bank_size = 240
real :: scale_M = 1.0
real :: scale_U = 1.0
real :: scale_V = 1.0
real :: scale_T = 1.0
real :: scale_Q = 1.0
real :: bc_freq_sec = 10800.0
integer :: ipmin = 11
integer :: jpmin = 31

! other variables
character(len=500) :: file_to_open
integer :: wrfbdy_fileid
integer :: wrfinput_fileid
integer :: pert_bank_fileid

integer :: t, ntimes
integer :: bank_mem_num
integer :: bdy_width
integer :: nx, ny, nz
integer :: i, j, k, b
character(len=3) :: bank_mem_char

real :: r

real, allocatable  :: mub(:,:), mubu(:,:), mubv(:,:)
real, allocatable  :: msfu(:,:)
real, allocatable  :: msfv(:,:)

real, allocatable  :: mu_pert(:,:)
real, allocatable  :: u_pert(:,:,:)
real, allocatable  :: v_pert(:,:,:)
real, allocatable  :: t_pert(:,:,:)
real, allocatable  :: q_pert(:,:,:)

real, allocatable  :: mu_bxs(:,:,:)
real, allocatable  :: u_bxs(:,:,:,:)
real, allocatable  :: v_bxs(:,:,:,:)
real, allocatable  :: t_bxs(:,:,:,:)
real, allocatable  :: q_bxs(:,:,:,:)

real, allocatable  :: mu_bxe(:,:,:)
real, allocatable  :: u_bxe(:,:,:,:)
real, allocatable  :: v_bxe(:,:,:,:)
real, allocatable  :: t_bxe(:,:,:,:)
real, allocatable  :: q_bxe(:,:,:,:)

real, allocatable  :: mu_bys(:,:,:)
real, allocatable  :: u_bys(:,:,:,:)
real, allocatable  :: v_bys(:,:,:,:)
real, allocatable  :: t_bys(:,:,:,:)
real, allocatable  :: q_bys(:,:,:,:)

real, allocatable  :: mu_bye(:,:,:)
real, allocatable  :: u_bye(:,:,:,:)
real, allocatable  :: v_bye(:,:,:,:)
real, allocatable  :: t_bye(:,:,:,:)
real, allocatable  :: q_bye(:,:,:,:)

real, allocatable  :: mu_btx(:,:,:)
real, allocatable  :: u_btx(:,:,:,:)
real, allocatable  :: v_btx(:,:,:,:)
real, allocatable  :: t_btx(:,:,:,:)
real, allocatable  :: q_btx(:,:,:,:)

real, allocatable  :: mu_bty(:,:,:)
real, allocatable  :: u_bty(:,:,:,:)
real, allocatable  :: v_bty(:,:,:,:)
real, allocatable  :: t_bty(:,:,:,:)
real, allocatable  :: q_bty(:,:,:,:)

integer :: rcode, dimid, varid



!!!!!!

! Get command-line data
call getarg(1,wrfbdy_file_name)
call getarg(2,wrfinput_file_name)
call getarg(3,pert_bank_dir)

write(*,*)' '
write(*,*)' wrfbdy_file_name   = ',trim(adjustl(wrfbdy_file_name))
write(*,*)' wrfinput_file_name = ',trim(adjustl(wrfinput_file_name))
write(*,*)' pert_bank_dir      = ',trim(adjustl(pert_bank_dir))
write(*,*)' '

! open wrfbdy file
file_to_open = trim(adjustl(wrfbdy_file_name))
rcode = nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_write, ncid=wrfbdy_fileid)
if ( rcode.ne.0) then
   write(*,*)'Error opening ',trim(adjustl(file_to_open))
   stop
endif

! open wrfinput file
file_to_open = trim(adjustl(wrfinput_file_name))
rcode = nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite, ncid=wrfinput_fileid)
if ( rcode.ne.0) then
   write(*,*)'Error opening ',trim(adjustl(file_to_open))
   stop
endif

! get dimensions from wrfbdy file
rcode = nf90_inq_dimid(wrfbdy_fileid, "Time", dimid)
rcode = nf90_inquire_dimension(wrfbdy_fileid, dimid, len = ntimes)

rcode = nf90_inq_dimid(wrfbdy_fileid, "bdy_width", dimid)
rcode = nf90_inquire_dimension(wrfbdy_fileid, dimid, len = bdy_width)

rcode = nf90_inq_dimid(wrfbdy_fileid, "west_east", dimid)
rcode = nf90_inquire_dimension(wrfbdy_fileid, dimid, len = nx)

rcode = nf90_inq_dimid(wrfbdy_fileid, "south_north", dimid)
rcode = nf90_inquire_dimension(wrfbdy_fileid, dimid, len = ny)

rcode = nf90_inq_dimid(wrfbdy_fileid, "bottom_top", dimid)
rcode = nf90_inquire_dimension(wrfbdy_fileid, dimid, len = nz)

write(*,*)' ntimes = ', ntimes
write(*,*)' bdy_width = ', bdy_width
write(*,*)' nx = ', nx
write(*,*)' ny = ', ny
write(*,*)' nz = ', nz

! allocate and read fields from wrfinput file

allocate(mub(nx, ny))
allocate(mubu(nx+1, ny))
allocate(mubv(nx, ny+1))
allocate(msfu(nx+1, ny))
allocate(msfv(nx, ny+1))

rcode = nf90_inq_varid(wrfinput_fileid, "MUB", varid)
rcode = nf90_get_var(wrfinput_fileid, varid, mub)
write(*,*) ' rcode for MUB = ', rcode

rcode = nf90_inq_varid(wrfinput_fileid, "MAPFAC_U", varid)
rcode = nf90_get_var(wrfinput_fileid, varid, msfu)
write(*,*) ' rcode for MAPFAC_U = ', rcode

rcode = nf90_inq_varid(wrfinput_fileid, "MAPFAC_V", varid)
rcode = nf90_get_var(wrfinput_fileid, varid, msfv)
write(*,*) ' rcode for MAPFAC_V = ', rcode

rcode = nf90_close(wrfinput_fileid) ! close file 

! compute MUB at u and v gridpoints

do j=1, ny
  mubu(1,j) = mub(1,j)
  do i=2, nx
    mubu(i,j) = 0.5*(mub(i-1,j)+mub(i,j))
  end do
  mubu(nx+1,j) = mub(nx,j)
end do

do i=1, nx
  mubv(i,1) = mub(i,1)
  do j=2, ny
    mubv(i,j) = 0.5*(mub(i,j-1)+mub(i,j))
  end do
  mubv(i,ny+1) = mub(i,ny)
end do

! initialize random-number generator and allocate arrays

call random_seed

allocate(mu_pert(nx, ny))
allocate(u_pert(nx+1, ny, nz))
allocate(v_pert(nx, ny+1, nz))
allocate(t_pert(nx, ny, nz))
allocate(q_pert(nx, ny, nz))

allocate(mu_bxs(ny, bdy_width, ntimes))
allocate(u_bxs(ny, nz, bdy_width, ntimes))
allocate(v_bxs(ny+1, nz, bdy_width, ntimes))
allocate(t_bxs(ny, nz, bdy_width, ntimes))
allocate(q_bxs(ny, nz, bdy_width, ntimes))

allocate(mu_bxe(ny, bdy_width, ntimes))
allocate(u_bxe(ny, nz, bdy_width, ntimes))
allocate(v_bxe(ny+1, nz, bdy_width, ntimes))
allocate(t_bxe(ny, nz, bdy_width, ntimes))
allocate(q_bxe(ny, nz, bdy_width, ntimes))

allocate(mu_bys(nx, bdy_width, ntimes))
allocate(u_bys(nx+1, nz, bdy_width, ntimes))
allocate(v_bys(nx, nz, bdy_width, ntimes))
allocate(t_bys(nx, nz, bdy_width, ntimes))
allocate(q_bys(nx, nz, bdy_width, ntimes))

allocate(mu_bye(nx, bdy_width, ntimes))
allocate(u_bye(nx+1, nz, bdy_width, ntimes))
allocate(v_bye(nx, nz, bdy_width, ntimes))
allocate(t_bye(nx, nz, bdy_width, ntimes))
allocate(q_bye(nx, nz, bdy_width, ntimes))

allocate(mu_btx(ny, bdy_width, ntimes))
allocate(u_btx(ny, nz, bdy_width, ntimes))
allocate(v_btx(ny+1, nz, bdy_width, ntimes))
allocate(t_btx(ny, nz, bdy_width, ntimes))
allocate(q_btx(ny, nz, bdy_width, ntimes))

allocate(mu_bty(nx, bdy_width, ntimes))
allocate(u_bty(nx+1, nz, bdy_width, ntimes))
allocate(v_bty(nx, nz, bdy_width, ntimes))
allocate(t_bty(nx, nz, bdy_width, ntimes))
allocate(q_bty(nx, nz, bdy_width, ntimes))

! read boundary conditions

write(*,*)
write(*,*) ' reading boundary conditions from wrfbdy file'
write(*,*)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BXS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, mu_bxs)
write(*,*) ' rcode for MU_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BXS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, u_bxs)
write(*,*) ' rcode for U_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BXS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, v_bxs)
write(*,*) ' rcode for V_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BXS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, t_bxs)
write(*,*) ' rcode for T_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BXS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, q_bxs)
write(*,*) ' rcode for Q_BXS = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BXE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, mu_bxe)
write(*,*) ' rcode for MU_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BXE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, u_bxe)
write(*,*) ' rcode for U_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BXE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, v_bxe)
write(*,*) ' rcode for V_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BXE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, t_bxe)
write(*,*) ' rcode for T_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BXE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, q_bxe)
write(*,*) ' rcode for Q_BXE = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BYS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, mu_bys)
write(*,*) ' rcode for MU_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BYS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, u_bys)
write(*,*) ' rcode for U_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BYS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, v_bys)
write(*,*) ' rcode for V_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BYS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, t_bys)
write(*,*) ' rcode for T_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BYS", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, q_bys)
write(*,*) ' rcode for Q_BYS = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BYE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, mu_bye)
write(*,*) ' rcode for MU_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BYE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, u_bye)
write(*,*) ' rcode for U_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BYE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, v_bye)
write(*,*) ' rcode for V_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BYE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, t_bye)
write(*,*) ' rcode for T_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BYE", varid)
rcode = nf90_get_var(wrfbdy_fileid, varid, q_bye)
write(*,*) ' rcode for Q_BYE = ', rcode


! loop over all times in wrfbdy file

do t=1, ntimes

  ! select a random bank member
  call random_number(r)
  bank_mem_num = 1 + nint(r*(bank_size-1.0))
  write(bank_mem_char,'(i3)') bank_mem_num

  ! open pert_bank file
  file_to_open = trim(adjustl(pert_bank_dir))//'/pert_bank_mem_'//trim(adjustl(bank_mem_char))//'.nc'
  write(*,*)' pert. bank file = ', trim(adjustl(file_to_open))
  rcode = nf90_open(path=trim(adjustl(file_to_open)), mode=nf90_nowrite, ncid=pert_bank_fileid)
  if ( rcode.ne.0) then
     write(*,*)'Error opening ',trim(adjustl(file_to_open))
     stop
  endif

! read perturbations for each field
! multiplication by mub rather than (mu + mub) is an approximation

  rcode = nf90_inq_varid(pert_bank_fileid, "MU", varid)
  rcode = nf90_get_var(pert_bank_fileid, varid, mu_pert, start = (/ ipmin, jpmin, 1 /))
  write(*,*) ' rcode for MU = ', rcode
  mu_pert(:,:) = mu_pert(:,:) * scale_M

  rcode = nf90_inq_varid(pert_bank_fileid, "U", varid)
  rcode = nf90_get_var(pert_bank_fileid, varid, u_pert, start = (/ ipmin, jpmin, 1, 1 /))
  write(*,*) ' rcode for U  = ', rcode
  u_pert(:,:,:) = u_pert(:,:,:) * scale_U
  do k=1, nz
    u_pert(:,:,k) = u_pert(:,:,k) * mubu(:,:) / msfu(:,:)
  end do

  rcode = nf90_inq_varid(pert_bank_fileid, "V", varid)
  rcode = nf90_get_var(pert_bank_fileid, varid, v_pert, start = (/ ipmin, jpmin, 1, 1 /))
  write(*,*) ' rcode for V  = ', rcode
  v_pert(:,:,:) = v_pert(:,:,:) * scale_V
  do k=1, nz
    v_pert(:,:,k) = v_pert(:,:,k) * mubv(:,:) / msfv(:,:)
  end do

  rcode = nf90_inq_varid(pert_bank_fileid, "T", varid)
  rcode = nf90_get_var(pert_bank_fileid, varid, t_pert, start = (/ ipmin, jpmin, 1, 1 /))
  write(*,*) ' rcode for T  = ', rcode
  t_pert(:,:,:) = t_pert(:,:,:) * scale_T
  do k=1, nz
    t_pert(:,:,k) = t_pert(:,:,k) * mub(:,:)
  end do

  rcode = nf90_inq_varid(pert_bank_fileid, "QVAPOR", varid)
  rcode = nf90_get_var(pert_bank_fileid, varid, q_pert, start = (/ ipmin, jpmin, 1, 1 /))
  write(*,*) ' rcode for Q  = ', rcode
  q_pert(:,:,:) = q_pert(:,:,:) * scale_Q
  do k=1, nz
    q_pert(:,:,k) = q_pert(:,:,k) * mub(:,:)
  end do

  rcode = nf90_close(pert_bank_fileid) ! close perturbation file

  ! add perturbations to west side
  do b=1, bdy_width
    mu_bxs(:, b, t) = mu_bxs(:, b, t) + mu_pert(b, :)
    u_bxs(:, :, b, t) = u_bxs(:, :, b, t) + u_pert(b, :, :)
    v_bxs(:, :, b, t) = v_bxs(:, :, b, t) + v_pert(b, :, :)
    t_bxs(:, :, b, t) = t_bxs(:, :, b, t) + t_pert(b, :, :)
    q_bxs(:, :, b, t) = q_bxs(:, :, b, t) + q_pert(b, :, :)
  enddo

  ! add perturbations to east side
  do b=1, bdy_width
    mu_bxe(:, b, t) = mu_bxe(:, b, t) + mu_pert(nx+1-b, :)
    u_bxe(:, :, b, t) = u_bxe(:, :, b, t) + u_pert(nx+2-b, :, :)
    v_bxe(:, :, b, t) = v_bxe(:, :, b, t) + v_pert(nx+1-b, :, :)
    t_bxe(:, :, b, t) = t_bxe(:, :, b, t) + t_pert(nx+1-b, :, :)
    q_bxe(:, :, b, t) = q_bxe(:, :, b, t) + q_pert(nx+1-b, :, :)
  enddo

  ! add perturbations to south side
  do b=1, bdy_width
    mu_bys(:, b, t) = mu_bys(:, b, t) + mu_pert(:, b)
    u_bys(:, :, b, t) = u_bys(:, :, b, t) + u_pert(:, b, :)
    v_bys(:, :, b, t) = v_bys(:, :, b, t) + v_pert(:, b, :)
    t_bys(:, :, b, t) = t_bys(:, :, b, t) + t_pert(:, b, :)
    q_bys(:, :, b, t) = q_bys(:, :, b, t) + q_pert(:, b, :)
  enddo

  ! add perturbations to north side
  do b=1, bdy_width
    mu_bye(:, b, t) = mu_bye(:, b, t) + mu_pert(:, ny+1-b)
    u_bye(:, :, b, t) = u_bye(:, :, b, t) + u_pert(:, ny+1-b, :)
    v_bye(:, :, b, t) = v_bye(:, :, b, t) + v_pert(:, ny+2-b, :)
    t_bye(:, :, b, t) = t_bye(:, :, b, t) + t_pert(:, ny+1-b, :)
    q_bye(:, :, b, t) = q_bye(:, :, b, t) + q_pert(:, ny+1-b, :)
  enddo

enddo ! t=1, ntimes

! don't allow negative q values
q_bxs(:, :, :, :) = max(0.0, q_bxs(:, :, :, :))
q_bxe(:, :, :, :) = max(0.0, q_bxe(:, :, :, :))
q_bys(:, :, :, :) = max(0.0, q_bys(:, :, :, :))
q_bye(:, :, :, :) = max(0.0, q_bye(:, :, :, :))

! write out perturbed boundary conditions
write(*,*)
write(*,*) ' writing perturbed boundary conditions to wrfbdy file'
write(*,*)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bxs)
write(*,*) ' rcode for MU_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bxs)
write(*,*) ' rcode for U_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bxs)
write(*,*) ' rcode for V_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bxs)
write(*,*) ' rcode for T_BXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bxs)
write(*,*) ' rcode for Q_BXS = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bxe)
write(*,*) ' rcode for MU_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bxe)
write(*,*) ' rcode for U_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bxe)
write(*,*) ' rcode for V_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bxe)
write(*,*) ' rcode for T_BXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bxe)
write(*,*) ' rcode for Q_BXE = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bys)
write(*,*) ' rcode for MU_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bys)
write(*,*) ' rcode for U_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bys)
write(*,*) ' rcode for V_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bys)
write(*,*) ' rcode for T_BYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bys)
write(*,*) ' rcode for Q_BYS = ', rcode


rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bye)
write(*,*) ' rcode for MU_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bye)
write(*,*) ' rcode for U_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bye)
write(*,*) ' rcode for V_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bye)
write(*,*) ' rcode for T_BYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bye)
write(*,*) ' rcode for Q_BYE = ', rcode

! update tendencies

write(*,*)
write(*,*) ' updating tendencies'
write(*,*)

! west side
do t=1, ntimes-1
   mu_btx(:, :, t) = ( mu_bxs(:, :, t+1) - mu_bxs(:, :, t) ) / bc_freq_sec
   u_btx(:, :, :, t) = ( u_bxs(:, :, :, t+1) - u_bxs(:, :, :, t) ) / bc_freq_sec
   v_btx(:, :, :, t) = ( v_bxs(:, :, :, t+1) - v_bxs(:, :, :, t) ) / bc_freq_sec
   t_btx(:, :, :, t) = ( t_bxs(:, :, :, t+1) - t_bxs(:, :, :, t) ) / bc_freq_sec
   q_btx(:, :, :, t) = ( q_bxs(:, :, :, t+1) - q_bxs(:, :, :, t) ) / bc_freq_sec
enddo
mu_btx(:, :, ntimes) = mu_btx(:, :, ntimes-1)
u_btx(:, :, :, ntimes) = u_btx(:, :, :, ntimes-1)
v_btx(:, :, :, ntimes) = v_btx(:, :, :, ntimes-1)
t_btx(:, :, :, ntimes) = t_btx(:, :, :, ntimes-1)
q_btx(:, :, :, ntimes) = q_btx(:, :, :, ntimes-1)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BTXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_btx)
write(*,*) ' rcode for MU_BTXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BTXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_btx)
write(*,*) ' rcode for U_BTXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BTXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_btx)
write(*,*) ' rcode for V_BTXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BTXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_btx)
write(*,*) ' rcode for T_BTXS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BTXS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_btx)
write(*,*) ' rcode for Q_BTXS = ', rcode

! east side
do t=1, ntimes-1
   mu_btx(:, :, t) = ( mu_bxe(:, :, t+1) - mu_bxe(:, :, t) ) / bc_freq_sec
   u_btx(:, :, :, t) = ( u_bxe(:, :, :, t+1) - u_bxe(:, :, :, t) ) / bc_freq_sec
   v_btx(:, :, :, t) = ( v_bxe(:, :, :, t+1) - v_bxe(:, :, :, t) ) / bc_freq_sec
   t_btx(:, :, :, t) = ( t_bxe(:, :, :, t+1) - t_bxe(:, :, :, t) ) / bc_freq_sec
   q_btx(:, :, :, t) = ( q_bxe(:, :, :, t+1) - q_bxe(:, :, :, t) ) / bc_freq_sec
enddo
mu_btx(:, :, ntimes) = mu_btx(:, :, ntimes-1)
u_btx(:, :, :, ntimes) = u_btx(:, :, :, ntimes-1)
v_btx(:, :, :, ntimes) = v_btx(:, :, :, ntimes-1)
t_btx(:, :, :, ntimes) = t_btx(:, :, :, ntimes-1)
q_btx(:, :, :, ntimes) = q_btx(:, :, :, ntimes-1)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BTXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_btx)
write(*,*) ' rcode for MU_BTXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BTXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_btx)
write(*,*) ' rcode for U_BTXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BTXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_btx)
write(*,*) ' rcode for V_BTXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BTXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_btx)
write(*,*) ' rcode for T_BTXE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BTXE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_btx)
write(*,*) ' rcode for Q_BTXE = ', rcode

! south side
do t=1, ntimes-1
   mu_bty(:, :, t) = ( mu_bys(:, :, t+1) - mu_bys(:, :, t) ) / bc_freq_sec
   u_bty(:, :, :, t) = ( u_bys(:, :, :, t+1) - u_bys(:, :, :, t) ) / bc_freq_sec
   v_bty(:, :, :, t) = ( v_bys(:, :, :, t+1) - v_bys(:, :, :, t) ) / bc_freq_sec
   t_bty(:, :, :, t) = ( t_bys(:, :, :, t+1) - t_bys(:, :, :, t) ) / bc_freq_sec
   q_bty(:, :, :, t) = ( q_bys(:, :, :, t+1) - q_bys(:, :, :, t) ) / bc_freq_sec
enddo
mu_bty(:, :, ntimes) = mu_bty(:, :, ntimes-1)
u_bty(:, :, :, ntimes) = u_bty(:, :, :, ntimes-1)
v_bty(:, :, :, ntimes) = v_bty(:, :, :, ntimes-1)
t_bty(:, :, :, ntimes) = t_bty(:, :, :, ntimes-1)
q_bty(:, :, :, ntimes) = q_bty(:, :, :, ntimes-1)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BTYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bty)
write(*,*) ' rcode for MU_BTYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BTYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bty)
write(*,*) ' rcode for U_BTYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BTYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bty)
write(*,*) ' rcode for V_BTYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BTYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bty)
write(*,*) ' rcode for T_BTYS = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BTYS", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bty)
write(*,*) ' rcode for Q_BTYS = ', rcode

! north side
do t=1, ntimes-1
   mu_bty(:, :, t) = ( mu_bye(:, :, t+1) - mu_bye(:, :, t) ) / bc_freq_sec
   u_bty(:, :, :, t) = ( u_bye(:, :, :, t+1) - u_bye(:, :, :, t) ) / bc_freq_sec
   v_bty(:, :, :, t) = ( v_bye(:, :, :, t+1) - v_bye(:, :, :, t) ) / bc_freq_sec
   t_bty(:, :, :, t) = ( t_bye(:, :, :, t+1) - t_bye(:, :, :, t) ) / bc_freq_sec
   q_bty(:, :, :, t) = ( q_bye(:, :, :, t+1) - q_bye(:, :, :, t) ) / bc_freq_sec
enddo
mu_bty(:, :, ntimes) = mu_bty(:, :, ntimes-1)
u_bty(:, :, :, ntimes) = u_bty(:, :, :, ntimes-1)
v_bty(:, :, :, ntimes) = v_bty(:, :, :, ntimes-1)
t_bty(:, :, :, ntimes) = t_bty(:, :, :, ntimes-1)
q_bty(:, :, :, ntimes) = q_bty(:, :, :, ntimes-1)

rcode = nf90_inq_varid(wrfbdy_fileid, "MU_BTYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, mu_bty)
write(*,*) ' rcode for MU_BTYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "U_BTYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, u_bty)
write(*,*) ' rcode for U_BTYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "V_BTYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, v_bty)
write(*,*) ' rcode for V_BTYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "T_BTYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, t_bty)
write(*,*) ' rcode for T_BTYE = ', rcode

rcode = nf90_inq_varid(wrfbdy_fileid, "QVAPOR_BTYE", varid)
rcode = nf90_put_var(wrfbdy_fileid, varid, q_bty)
write(*,*) ' rcode for Q_BTYE = ', rcode

! close wrfbdy file

rcode = nf90_close(wrfbdy_fileid) ! close file


! deallocate arrays

deallocate(mub)
deallocate(mubu)
deallocate(mubv)
deallocate(msfu)
deallocate(msfv)

deallocate(mu_pert)
deallocate(u_pert)
deallocate(v_pert)
deallocate(t_pert)
deallocate(q_pert)

deallocate(mu_bxs)
deallocate(u_bxs)
deallocate(v_bxs)
deallocate(t_bxs)
deallocate(q_bxs)

deallocate(mu_bxe)
deallocate(u_bxe)
deallocate(v_bxe)
deallocate(t_bxe)
deallocate(q_bxe)

deallocate(mu_bys)
deallocate(u_bys)
deallocate(v_bys)
deallocate(t_bys)
deallocate(q_bys)

deallocate(mu_bye)
deallocate(u_bye)
deallocate(v_bye)
deallocate(t_bye)
deallocate(q_bye)

deallocate(mu_btx)
deallocate(u_btx)
deallocate(v_btx)
deallocate(t_btx)
deallocate(q_btx)

deallocate(mu_bty)
deallocate(u_bty)
deallocate(v_bty)
deallocate(t_bty)
deallocate(q_bty)


write(*,*)
write(*,*) ' all done'
write(*,*)

stop

end program hrrre_bc_pert
