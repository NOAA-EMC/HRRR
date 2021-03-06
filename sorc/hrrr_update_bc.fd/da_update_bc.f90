program da_update_bc

   !-----------------------------------------------------------------------
   ! Purpose: update BC file from wrfvar output.
   ! current version reads only wrf-netcdf file format
   !
   ! Y.-R. Guo, 03/18/2008:
   !   1) Fixed the bug for low_bdy_only;
   !   2) Introducing another namelist variable: update_lsm
   !      update_lsm = .true. --- The LSM predicted variables: 
   !                         TSLB, SMOIS, SNOW, SH2O, RHOSN, CANWAT, SNOWH
   !                              will be updated based on wrf_input file
   !                 = .false. -- no updated, default.
   !
   !-----------------------------------------------------------------------

   use mpi
   use da_netcdf_interface, only : da_get_var_3d_real_cdf, &
      da_put_var_3d_real_cdf, da_get_dims_cdf, da_put_var_2d_real_cdf, &
      da_get_var_2d_real_cdf, da_get_var_2d_int_cdf, da_get_bdytimestr_cdf, &
      da_get_times_cdf, da_get_bdyfrq, stderr, stdout, da_put_var_2d_int_cdf,&
      da_get_var_1d_real_cdf

   use da_module_couple_uv, only : da_couple_uv

   implicit none

   include 'netcdf.inc'

! MPI variables
  integer :: npe, mype, mypeLocal,ierror

   integer, parameter :: max_3d_variables = 20, &
                         max_2d_variables = 25
 
   character(len=512) :: da_file,      &
                         da_file_02,   &
                         wrf_bdy_file, &
                         wrf_input
 
   character(len=20) :: var_pref, var_name, vbt_name

   character(len=20) :: var3d(max_3d_variables), &
                        varsf(max_2d_variables)

   character(len=10), dimension(4) :: bdyname, tenname

   integer           :: ids, ide, jds, jde, kds, kde
   integer           :: num3d, num2d, ndims
   integer           :: time_level
   integer           :: i,j,k,l,m,n

   integer, dimension(4) :: dims
 
   real, allocatable, dimension(:,:,:) :: tend3d, scnd3d, frst3d, full3d, full3d2

   real, allocatable, dimension(:,:,:) :: u, v, u2, v2

   real, allocatable, dimension(:,  :) :: mu, mub, msfu, msfv, msfm,       &
                                          mu2, tend2d, scnd2d, frst2d, full2d

   real, allocatable, dimension(:,  :) :: tsk, tsk_wrfvar,slice,tem
   real, allocatable, dimension(:,:)   :: snow, snowc, snowh

   integer           :: chlvl,cflvl
   real,allocatable, dimension(:) :: c1h, c2h, c1f, c2f
   real,allocatable, dimension(:) :: c1, c2

   integer, allocatable, dimension(:,:) :: ivgtyp, full2dint

   character(len=80), allocatable, dimension(:) :: times, &
                                                   thisbdytime, nextbdytime &
                                                  ,inittime
 
   integer :: east_end, north_end, io_status, cdfid, varid, domain_id, iswater
   integer :: imodel ! 1 - RAP, 2 - HRRR
   integer :: iostatus(4)

   logical :: debug, update_lateral_bdy, update_low_bdy, update_lsm, keep_tsk_wrf
   logical :: keep_snow_wrf, var4d_lbc

   integer(8) :: bdyfrq, bdyfrqini
   real :: uvMAX, uvMIN, diff, gradThresh, multiple
   real :: uvTendMAX, uvTendMIN, tTendMAX, tTendMIN, qTendMAX, qTendMIN
   integer :: gradPTs, kk, ii, k2, i2, boxsizehalf, imax, imin, kmax, kmin
   logical :: l_uv, l_limit_uv, l_limit_t, l_limit_q, l_limit_uvgrad
   logical :: update_fld, update_tend, l_critical
   integer :: smooth_loops ! do smoothing multiple times
   integer :: smooth_pts   ! 0-none, 1-9pts, 2-25pts, 3-49pts
   integer :: smoothTrigger   ! when >smoothTrigger large shear points found, do smoothing
   integer :: criticalTrigger ! when >criticalTrigger large shear points found,skip updating current variable
   integer :: knt_shear, knt_shear_max
   integer :: timelevel_out

   character(len=512) :: wrfvar_output_file    ! obsolete. Kept for backward compatibility
   logical            :: cycling, low_bdy_only ! obsolete. Kept for backward compatibility

   integer, parameter :: namelist_unit = 7, &
                         ori_unit = 11, &
                         new_unit = 12

   namelist /control_param/ imodel,       &
                            da_file,      &
                            da_file_02,   &
                            wrf_bdy_file, &
                            wrf_input, domain_id, var4d_lbc, &
                            debug, update_lateral_bdy, update_low_bdy, update_lsm, &
                            keep_tsk_wrf, keep_snow_wrf, iswater, &
                            wrfvar_output_file, cycling, low_bdy_only, &
                            l_limit_uv,uvTendMAX,uvTendMIN, &
                            l_limit_uvgrad,uvMAX,uvMIN, gradPTs,gradThresh,boxsizehalf, &
                            l_limit_t, tTendMAX, tTendMIN, &
                            l_limit_q, qTendMAX, qTendMIN, &
                            update_fld, update_tend,       &
                            smooth_pts,smooth_loops,smoothTrigger,criticalTrigger

!
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!
! NCEP LSF has to use all cores allocated to run this application 
! but this if check can make sure only one core run through the real code.
if(mype==0) then
!

   da_file            = 'wrfvar_output'
   da_file_02         = 'ana02'
   wrf_bdy_file       = 'wrfbdy_d01'
   wrf_input          = 'wrfinput_d01'
   domain_id          = 1
   imodel             = 1

   var4d_lbc          = .false.
   debug              = .false. 
   update_lateral_bdy = .true.
   update_low_bdy     = .true.
   update_lsm         = .false.
   keep_tsk_wrf       = .true.
   keep_snow_wrf      = .true.
!   iswater            = 16      ! USGS water index: 16, MODIS water index: 17
   iswater            = 17      ! USGS water index: 16, MODIS water index: 17

   wrfvar_output_file = 'OBSOLETE'
   cycling            = .false.
   low_bdy_only       = .false.
   l_limit_uv=.false.
   l_limit_uvgrad=.false.
   l_limit_t=.false.
   l_limit_q=.false.
   update_fld=.true.
   update_tend=.true.
   smooth_pts=0
   smooth_loops=0
   smoothTrigger=50000
   criticalTrigger=50000
   uvMAX=1.0E6
   uvMIN=-1.0E6
   gradPTs =3 
   gradThresh=2.5E6
   boxsizehalf=10 !10 grid points for half box side
   uvTendMAX=400.0
   uvTendMIN=-400.0
   tTendMAX=400.0
   tTendMIN=-400.0
   qTendMAX=400.0
   qTendMIN=-400.0
   knt_shear=0

   !---------------------------------------------------------------------
   ! Read namelist
   !---------------------------------------------------------------------
   io_status = 0

   open(unit = namelist_unit, file = 'parame.in', &
          status = 'old' , access = 'sequential', &
          form   = 'formatted', action = 'read', &
          iostat = io_status)

   if (io_status /= 0) then
      write(unit=stdout,fmt=*) 'Error to open namelist file: parame.in.'
      write(unit=stdout,fmt=*) 'Will work for updating lateral boundary only.'
   else
      read(unit=namelist_unit, nml = control_param , iostat = io_status)

      if (io_status /= 0) then
         write(unit=stdout,fmt=*) 'Error to read control_param. Stopped.'
         stop
      end if

      ! deal with the old namelist
      if ( index(wrfvar_output_file, 'OBSOLETE') <= 0 ) then
         ! wrfvar_output_file is set in the user's parame.in
         ! reset the settings
         da_file = wrfvar_output_file
         if ( domain_id > 1 ) then
            low_bdy_only = .true.
         end if
         if ( cycling .and. domain_id == 1 ) then
            update_lateral_bdy = .true.
            update_low_bdy     = .true.
         else
            if ( low_bdy_only ) then
               update_lateral_bdy = .false.
               update_low_bdy     = .true.
            else
               update_lateral_bdy = .true.
               update_low_bdy     = .false.
            end if
         end if
      end if

      WRITE(unit=stdout, fmt='(2a)') &
           'da_file       = ', trim(da_file), &
           'da_file_02    = ', trim(da_file_02), &
           'wrf_bdy_file  = ', trim(wrf_bdy_file), &
           'wrf_input     = ', trim(wrf_input)

      WRITE(unit=stdout, fmt='(2(a, L10))')             &
           'update_lateral_bdy = ', update_lateral_bdy, &
           'update_low_bdy     = ', update_low_bdy

      if ( update_lsm ) keep_snow_wrf = .false.

      close(unit=namelist_unit)
   end if

   ! 3D need update
if(imodel==1)then   ! RAP
   num3d=7
   var3d(1)='U'
   var3d(2)='V'
   var3d(3)='T'
   var3d(4)='PH'
   var3d(5)='QVAPOR'
   var3d(6)='QNWFA'
   var3d(7)='QNIFA'
   print *,'RAP Num3d',num3d,'Variables - ',var3d(1:num3d) 
else ! HRRR - set it to 2
   num3d=15
   var3d(1)='U'
   var3d(2)='V'
   var3d(3)='T'
   var3d(4)='PH'
   var3d(5)='QVAPOR'
   var3d(6)='QCLOUD'
   var3d(7)='QICE'
   var3d(8)='QRAIN'
   var3d(9)='QSNOW'
   var3d(10)='QGRAUP'
   var3d(11)='QNRAIN'
   var3d(12)='QNICE'
   var3d(13)='QNCLOUD'
   var3d(14)='QNWFA'
   var3d(15)='QNIFA'
   print *,'HRRR Num3d',num3d,'Variables - ',var3d(1:num3d)
endif

   ! 2D need update
   num2d=23
   varsf(1)='MUB'
   varsf(2)='MU'
   varsf(3)='MAPFAC_U'
   varsf(4)='MAPFAC_V'
   varsf(5)='MAPFAC_M'
   varsf(6)='TMN'
   varsf(7)='SST'
   varsf(8)='TSK'
   varsf(9)='VEGFRA'
   varsf(10)='ALBBCK'
   varsf(11)='TSLB'
   varsf(12)='SMOIS'
   varsf(13)='SNOW'
   varsf(14)='SEAICE'
   varsf(15)='SH2O'
   varsf(16)='CANWAT'
   varsf(17)='RHOSN'
   varsf(18)='SNOWH'
   varsf(19)='LANDMASK'
   varsf(20)='IVGTYP'
   varsf(21)='ISLTYP'
   varsf(22)='SNOWC'
   varsf(23)='XLAND'

   if ( domain_id > 1 ) then
      write(unit=stdout, fmt='(a,i2)') 'Nested domain ID=',domain_id
      write(unit=stdout, fmt='(a)') &
        'No wrfbdy file needed, only low boundary need to be updated.'
      if ( update_lateral_bdy ) then
         write(unit=stdout, fmt='(a)') &
            'Re-setting update_lateral_bdy to be false for nested domain.'
         update_lateral_bdy = .false.
      end if
      update_low_bdy     = .true.
   end if

   if ( update_lateral_bdy ) then
   ! First, the boundary times
   call da_get_dims_cdf(wrf_bdy_file, 'Times', dims, ndims, debug)

   if (debug) then
      write(unit=stdout, fmt='(a,i2,2x,a,4i6)') &
           'Times: ndims=', ndims, 'dims=', (dims(i), i=1,ndims)
   end if

   time_level = dims(2)

   if (time_level < 1) then
      write(unit=stdout, fmt='(a,i2/a)') &
           'time_level = ', time_level, &
           'We need at least one time-level BDY.'
      stop 'Wrong BDY file.'
   end if

   allocate(times(dims(2)))
   allocate(thisbdytime(dims(2)))
   allocate(nextbdytime(dims(2)))
!tgs
   allocate(inittime(dims(2)))

   call da_get_times_cdf(wrf_bdy_file, times, timelevel_out, dims(2), debug)
!tgs
   call da_get_times_cdf(wrfvar_output_file, inittime, timelevel_out, dims(2), debug)

   call da_get_bdytimestr_cdf(wrf_bdy_file, 'thisbdytime', thisbdytime, dims(2), debug)
   call da_get_bdytimestr_cdf(wrf_bdy_file, 'nextbdytime', nextbdytime, dims(2), debug)

   call da_get_bdyfrq(thisbdytime(1), nextbdytime(1), bdyfrq, debug)
!tgs   
   call da_get_bdyfrq(inittime(1), nextbdytime(1), bdyfrqini, debug)
   if (debug) then
      do n=1, dims(2)
         write(unit=stdout, fmt='(3(a, i2, 2a,2x))') &
           '       times(', n, ')=', trim(times(n)), &
           'thisbdytime (', n, ')=', trim(thisbdytime(n)), &
           'nextbdytime (', n, ')=', trim(nextbdytime(n)), &
!tgs
           'inittime    (', n, ')=', trim(inittime(n))
          print *,'bdyfrq=',bdyfrq
          print *,'bdyfrqini=',bdyfrqini
      end do
   end if

   end if

   east_end=0
   north_end=0
!
!
   call da_get_dims_cdf(da_file, 'C1H', dims, ndims, debug)
   allocate(c1h(dims(1)), c2h(dims(1)))
   chlvl=dims(1)
   call da_get_var_1d_real_cdf( da_file, 'C1H', c1h, dims(1), 1, debug)
   call da_get_var_1d_real_cdf( da_file, 'C2H', c2h, dims(1), 1, debug)
!   c1h=1.0
!   c2h=0.0
!   do i=1,chlvl
!      write(*,*) i,c1h(i),c2h(i)
!   enddo
   call da_get_dims_cdf(da_file, 'C1F', dims, ndims, debug)
   allocate(c1f(dims(1)), c2f(dims(1)))
   cflvl=dims(1)
   call da_get_var_1d_real_cdf( da_file, 'C1F', c1f, dims(1), 1, debug)
   call da_get_var_1d_real_cdf( da_file, 'C2F', c2f, dims(1), 1, debug)
!   c1f=1.0
!   c2f=0.0
!   do i=1,cflvl
!      write(*,*) i,c1f(i),c2f(i)
!   enddo
!
   cdfid = ncopn(da_file, NCWRITE, io_status )

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv

   do n=1,num2d

      io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
      if (io_status /= 0 ) then
         print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                   n, io_status, trim(varsf(n)), " does not exist"
         cycle
      endif

      call da_get_dims_cdf( da_file, trim(varsf(n)), dims, &
         ndims, debug)

      select case(trim(varsf(n)))
      case ('MU') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(MU(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, &
            trim(varsf(n)), MU, dims(1), dims(2), 1, debug)

         east_end=dims(1)+1
         north_end=dims(2)+1

         if ( var4d_lbc ) then
            allocate(MU2(dims(1), dims(2)))

            call da_get_var_2d_real_cdf( da_file_02, &
               trim(varsf(n)), MU2, dims(1), dims(2), 1, debug)
         end if
      case ('MUB') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(MUB(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), MUB, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_U') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfu(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfu, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_V') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfv(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfv, &
                                   dims(1), dims(2), 1, debug)
      case ('MAPFAC_M') ;
         if ( .not. update_lateral_bdy ) cycle

         allocate(msfm(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), msfm, &
                                   dims(1), dims(2), 1, debug)
      case ('TSK') ;
         if ( .not. update_low_bdy ) cycle

         allocate(tsk(dims(1), dims(2)))
         allocate(tsk_wrfvar(dims(1), dims(2)))
         allocate(ivgtyp(dims(1), dims(2)))

         call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), tsk, &
                                   dims(1), dims(2), 1, debug)

         if ( keep_tsk_wrf ) then
            call da_get_var_2d_real_cdf( da_file, trim(varsf(n)), tsk_wrfvar, &
                                      dims(1), dims(2), 1, debug)
            !hcl call da_get_var_2d_int_cdf( da_file, 'IVGTYP', ivgtyp, &
            call da_get_var_2d_int_cdf( wrf_input, 'IVGTYP', ivgtyp, &
                                      dims(1), dims(2), 1, debug)
            ! update TSK.
            do j=1,dims(2)
               do i=1,dims(1)
                  if (ivgtyp(i,j) /= iswater)  tsk(i,j)=tsk_wrfvar(i,j)
               end do
            end do
         end if

            call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), tsk, &
                                      dims(1), dims(2), 1, debug)
            deallocate(tsk)
            deallocate(ivgtyp)
            deallocate(tsk_wrfvar)

         !hcl case ('TMN', 'SST', 'VEGFRA', 'ALBBCK', 'SEAICE') ;
         case ('TMN', 'SST', 'VEGFRA', 'ALBBCK', 'SEAICE', 'LANDMASK', 'XLAND') ;
            if ( .not. update_low_bdy ) cycle

            allocate(full2d(dims(1), dims(2)))

            call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)

            call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug)
            deallocate(full2d)

         case ('IVGTYP', 'ISLTYP') ;  !hcl add
            if ( .not. update_low_bdy ) cycle

            allocate(full2dint(dims(1), dims(2)))

            call da_get_var_2d_int_cdf( wrf_input, trim(varsf(n)), full2dint, &
                                      dims(1), dims(2), 1, debug)

            call da_put_var_2d_int_cdf( da_file, trim(varsf(n)), full2dint, &
                                      dims(1), dims(2), 1, debug)
            deallocate(full2dint)

         case ('SNOW', 'RHOSN', 'SNOWH', 'SNOWC') ;
            if ( (.not. update_lsm) .and. (.not. update_low_bdy) ) cycle
            if ( keep_snow_wrf ) cycle
               allocate(full2d(dims(1), dims(2)))

               call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )

               call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
               deallocate(full2d)

         case ('CANWAT') ;
            if ( .not. update_lsm ) cycle
               allocate(full2d(dims(1), dims(2)))

               call da_get_var_2d_real_cdf( wrf_input, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
!               print *,"sum(full2d^2)=", sum(full2d*full2d)

               call da_put_var_2d_real_cdf( da_file, trim(varsf(n)), full2d, &
                                      dims(1), dims(2), 1, debug )
               deallocate(full2d)

         case ('TSLB', 'SMOIS', 'SH2O') ;
            if( .not. update_lsm ) cycle
               allocate(full3d(dims(1), dims(2), dims(3)))

               call da_get_var_3d_real_cdf( wrf_input, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
!               print *,"sum(full3d^2)=", sum(full3d*full3d)

               call da_put_var_3d_real_cdf( da_file, trim(varsf(n)), full3d, &
                                      dims(1), dims(2), dims(3), 1, debug )
               deallocate(full3d)

         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here. varsf(n)=', trim(varsf(n))
      end select
   end do

   ! check for snow over water
 if( update_lsm ) then !tgs - do not change snow when update_lsm is .false.
   iostatus(1) = nf_inq_varid(cdfid, 'IVGTYP', varid)
   iostatus(2) = nf_inq_varid(cdfid, 'SNOW',   varid)
   iostatus(3) = nf_inq_varid(cdfid, 'SNOWC',  varid)
   iostatus(4) = nf_inq_varid(cdfid, 'SNOWH',  varid)
   if ( iostatus(1) == 0 ) then
      allocate(snow(dims(1), dims(2)))
      allocate(snowc(dims(1), dims(2)))
      allocate(snowh(dims(1), dims(2)))
      allocate(ivgtyp(dims(1), dims(2)))
      if ( iostatus(1) == 0 ) then
         call da_get_var_2d_int_cdf( da_file, 'IVGTYP', ivgtyp,    &
                               dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(2) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOW',    snow,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(3) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOWC',  snowc,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(4) == 0 ) then
         call da_get_var_2d_real_cdf( da_file, 'SNOWH',  snowh,     &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(2) == 0 ) then
         do j = 1, dims(2)
            do i = 1, dims(1)
               if (ivgtyp(i,j) == iswater)  then
                  if ( snow(i,j) > 0.0 ) then
                     write(unit=stdout,fmt=*) 'Remove snow over water at i, j = ', i, j
                     if ( iostatus(2) == 0 ) snow(i,j)  = 0.0
                     if ( iostatus(3) == 0 ) snowc(i,j) = 0.0
                     if ( iostatus(4) == 0 ) snowh(i,j) = 0.0
                  end if
               end if
            end do
         end do
      end if
      if ( iostatus(2) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOW',   snow, &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(3) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOWC',  snowc, &
                                dims(1), dims(2), 1, debug)
      end if
      if ( iostatus(4) == 0 ) then
         call da_put_var_2d_real_cdf( da_file, 'SNOWH',  snowh, &
                                dims(1), dims(2), 1, debug)
      end if
      deallocate(snow)
      deallocate(snowc)
      deallocate(snowh)
      deallocate(ivgtyp)
   end if
 endif ! update_lsm
   
 if ( update_lateral_bdy ) then

   if (east_end < 1 .or. north_end < 1) then
      write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
      stop
   end if

   write(unit=stdout,fmt='(/a/)') 'Processing the lateral boundary condition:'

   ! boundary variables
   bdyname(1)='_BXS'
   bdyname(2)='_BXE'
   bdyname(3)='_BYS'
   bdyname(4)='_BYE'

   ! boundary tendancy variables
   tenname(1)='_BTXS'
   tenname(2)='_BTXE'
   tenname(3)='_BTYS'
   tenname(4)='_BTYE'

   do m=1,4
      write(var_name,'(a,a)') 'MU' , trim(bdyname(m))
      write(vbt_name,'(a,a)') 'MU' , trim(tenname(m))

      call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

      allocate(frst2d(dims(1), dims(2)))
      allocate(scnd2d(dims(1), dims(2)))
      allocate(tend2d(dims(1), dims(2)))

      ! Get variable at second time level
      if ( .not. var4d_lbc ) then
         if (time_level > 1) then
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), scnd2d, &
                                      dims(1), dims(2), 2, debug)
         else
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                      dims(1), dims(2), 1, debug)
            call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                      dims(1), dims(2), 1, debug)
         end if
      end if

      if (debug) then
         write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 1, debug)

         write(unit=ori_unit, fmt='(a, 10i12)') &
              ' old ', (i, i=1,dims(2))
         do j=1,dims(1)
            write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at first time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*real(bdyfrq)
               if (var4d_lbc) scnd2d(j,l)=MU2(l,j)
               frst2d(j,l)=MU(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*real(bdyfrq)
               if (var4d_lbc) scnd2d(j,l)=MU2(east_end-l,j)
               frst2d(j,l)=MU(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*real(bdyfrq)
               if (var4d_lbc) scnd2d(i,l)=MU2(i,l)
               frst2d(i,l)=MU(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*real(bdyfrq)
               if (var4d_lbc) scnd2d(i,l)=MU2(i,north_end-l)
               frst2d(i,l)=MU(i,north_end-l)
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            tend2d(i,l)=(scnd2d(i,l)-frst2d(i,l))/real(bdyfrqini)
         end do
      end do

      if (debug) then
         write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         write(unit=new_unit, fmt='(a, 10i12)') &
              ' new ', (i, i=1,dims(2))

         do j=1,dims(1)
            write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! output new variable at first time level
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                dims(1), dims(2), 1, debug)
      ! output new tendancy 
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                dims(1), dims(2), 1, debug)

      deallocate(frst2d)
      deallocate(scnd2d)
      deallocate(tend2d)
   end do

   !---------------------------------------------------------------------
   ! For 3D variables

   ! Get U
   call da_get_dims_cdf( da_file, 'U', dims, ndims, debug)

   ! call da_get_att_cdf( da_file, 'U', debug)

   allocate(u(dims(1), dims(2), dims(3)))

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   allocate(c1(dims(3)),c2(dims(3)))
   if(dims(3)==chlvl) then
      c1=c1h
      c2=c2h
   elseif(dims(3)==cflvl) then
      c1=c1f
      c2=c2f
   else
      write(*,*) 'mismatch dimension',chlvl,cflvl,dims(3)
   endif

   call da_get_var_3d_real_cdf( da_file, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)
   if ( var4d_lbc ) then
      allocate(u2(dims(1), dims(2), dims(3)))
      call da_get_var_3d_real_cdf( da_file_02, 'U', u2, &
                                dims(1), dims(2), dims(3), 1, debug)
   end if

   ! do j=1,dims(2)
   !    write(unit=stdout, fmt='(2(a,i5), a, f12.8)') &
   !       'u(', dims(1), ',', j, ',1)=', u(dims(1),j,1)
   ! end do

   ! Get V
   call da_get_dims_cdf( da_file, 'V', dims, ndims, debug)

   ! call da_get_att_cdf( da_file, 'V', debug)

   allocate(v(dims(1), dims(2), dims(3)))

   call da_get_var_3d_real_cdf( da_file, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)
   if ( var4d_lbc ) then
      allocate(v2(dims(1), dims(2), dims(3)))
      call da_get_var_3d_real_cdf( da_file_02, 'V', v2, &
                                dims(1), dims(2), dims(3), 1, debug)
   end if

   ! do i=1,dims(1)
   !    write(unit=stdout, fmt='(2(a,i5), a, f12.8)') &
   !       'v(', i, ',', dims(2), ',1)=', v(i,dims(2),1)
   ! end do

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'Before couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'Before couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv ( u, v, MU, MUB, msfu, msfv, c1, c2, ids, ide, jds, jde, kds, kde)
   if ( var4d_lbc ) then
      call da_couple_uv ( u2, v2, MU2, MUB, msfu, msfv, c1, c2, ids, ide, jds, jde, kds, kde)
   end if

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if
 
   deallocate(c1, c2)
   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( da_file, trim(var3d(n)), dims, ndims, debug)

      allocate(full3d(dims(1), dims(2), dims(3)))
      if ( var4d_lbc ) allocate(full3d2(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(2)+1

      allocate(c1(dims(3)),c2(dims(3)))
      if(dims(3)==chlvl) then
         c1=c1h
         c2=c2h
      elseif(dims(3)==cflvl) then
         c1=c1f
         c2=c2f
      else
         write(*,*) 'mismatch dimension',chlvl,cflvl,dims(3)
      endif


      select case(trim(var3d(n)))
      case ('U') ;           ! U
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=u(:,:,:)
         if ( var4d_lbc ) full3d2(:,:,:)=u2(:,:,:)
      case ('V') ;           ! V 
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=v(:,:,:)
         if ( var4d_lbc ) full3d2(:,:,:)=v2(:,:,:)
      case ('W') ;
         ! var_pref = 'R' // trim(var3d(n))
         var_pref = trim(var3d(n))

         call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)
         if ( var4d_lbc ) &
            call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
               full3d2, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))/msfm(i,j)
                  if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*((c1(k)*mu2(i,j))+(c1(k)*mub(i,j)+c2(k)))/msfm(i,j)
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('T', 'PH') ;
         var_pref=trim(var3d(n))
 
         call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)
         if ( var4d_lbc ) &
            call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
               full3d2, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
                  if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*((c1(k)*mu2(i,j))+(c1(k)*mub(i,j)+c2(k)))
               end do
            end do
         end do

            if (debug) then
               write(unit=stdout, fmt='(3a,e20.12,4x)') &
                    'After  couple Sample ', trim(var3d(n)), &
                    '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
            end if
!      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP', 'QNRAIN', 'QNICE') ;
      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP', 'QNRAIN', 'QNICE','QNCLOUD','QNWFA', 'QNIFA') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)
 
         call da_get_var_3d_real_cdf( da_file, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)
         if ( var4d_lbc ) &
            call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
               full3d2, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
                  if ( var4d_lbc ) full3d2(i,j,k)=full3d2(i,j,k)*((c1(k)*mu2(i,j))+(c1(k)*mub(i,j)+c2(k)))
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      do m=1,4
         write(var_name,'(a,a)') trim(var_pref), trim(bdyname(m))
         write(vbt_name,'(a,a)') trim(var_pref), trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(scnd3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))

         ! Get variable at second time level
         if ( .not. var4d_lbc ) then
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                     dims(1), dims(2), dims(3), 1, debug)
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                     dims(1), dims(2), dims(3), 1, debug)
            if (time_level > 1) then
               call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), scnd3d, &
                                         dims(1), dims(2), dims(3), 2, debug)
            end if
         end if

         if (debug) then
            write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=ori_unit, fmt='(a, 10i12)') &
                 ' old ', (i, i=1,dims(3))
            do j=1,dims(1)
               write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if
   
         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*real(bdyfrq)
               if ( var4d_lbc ) scnd3d(j,k,l)=full3d2(l,j,k)
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*real(bdyfrq)
               if ( var4d_lbc ) scnd3d(j,k,l)=full3d2(east_end-l,j,k)
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*real(bdyfrq)
               if ( var4d_lbc )scnd3d(i,k,l)=full3d2(i,l,k)
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 2 .and. .not. var4d_lbc) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*real(bdyfrq)
               if ( var4d_lbc ) scnd3d(i,k,l)=full3d2(i,north_end-l,k)
               frst3d(i,k,l)=full3d(i,north_end-l,k)
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'cal. tend: bdyname(', m, ')=', trim(vbt_name)
         select case(trim(vbt_name))
         case ('U_BTXS','U_BTXE','U_BTYS','U_BTYE','V_BTXS','V_BTXE','V_BTYS','V_BTYE');
           l_uv=.true.
         case default;
           l_uv=.false.
         end select

         ! calculate new tendancy 
         allocate(slice(dims(1), dims(2)))
         allocate(tem(dims(1), dims(2)))
         knt_shear_max=0
         do l=1,dims(3)
            !!! find large horizontal gradient (> gradThresh) and modify them
            !!! before the final computation of tend3d(:,:,:)
            !to check if there is deep layer of sharp gradient
            slice(:,:)=frst3d(:,:,l)
            if (l_uv) then
              knt_shear = 0
              do k=1,dims(2)
                 do i=gradPTs+1,dims(1)
                    diff=slice(i,k)-slice(i-gradPTs,k) 
                    multiple=sign(1.0,slice(i,k)) * sign(1.0,slice(i-gradPTs,k) )
                    if (abs(diff) > gradThresh .and. multiple < 0.0 ) then
                      knt_shear=knt_shear+1
                      write(unit=stdout, fmt='(a,3i4,f16.1)') 'large shear (i,k,l,diff):', i,k,l,diff
                      if (knt_shear > knt_shear_max) knt_shear_max=knt_shear
                    endif
                 enddo
              enddo
              write(unit=stdout, fmt='(2a,i3,i10)') 'total points of large shear: ', trim(var_name), l, knt_shear
            endif
            if (l_uv .and. l_limit_uvgrad) then
              do k=1,dims(2)
                 do i=gradPTs+1,dims(1)
                    diff=slice(i,k)-slice(i-gradPTs,k)
                    if (abs(diff) > gradThresh) then
                      write(unit=stdout, fmt='(a,3i4,f12.3)') 'large gradient', i,k,l,diff
                      ii=i
                      kk=k

                      kmax=min(kk+boxsizehalf,dims(2))
                      kmin=max(1,kk-boxsizehalf)
                      imax=min(ii+boxsizehalf,dims(1))
                      imin=max(1,ii-boxsizehalf)
                      do k2=kmin,kmax
                        do i2=imin,imax
                          if (slice(i2,k2) > uvMAX) then
                            write(unit=stdout, fmt='(a,3i5,2e10.2)') 'UV capped(i,k,l):', i2,k2,l,slice(i2,k2), uvMAX
                            slice(i2,k2)=uvMAX
                          else if (slice(i2,k2)<uvMIN) then
                            write(unit=stdout, fmt='(a,3i5,2e10.2)') 'UV bottomed(i,k,l):', i2,k2,l,slice(i2,k2), uvMIN
                            slice(i2,k2)=uvMIN
                          endif
                        enddo
                      enddo
                    end if
                 enddo
              enddo
              frst3d(:,:,l)=slice(:,:)
            endif
            if (l_uv .and. smooth_loops > 0 .and. smooth_pts >0 .and. knt_shear > smoothTrigger) then
              write(unit=stdout,fmt='(a,4i6)') 'smoothing wind field(loops,pts,knt_shear,trigger)', &
                  smooth_loops, smooth_pts,knt_shear,smoothTrigger
              slice(:,:)=frst3d(:,:,l)
              do k2=1,smooth_loops
                tem=slice
                do k=1+smooth_pts,dims(2)-smooth_pts
                  do i=1+smooth_pts,dims(1)-smooth_pts
                     tem(i,k)=0.0
                     do kk=k-smooth_pts, k+smooth_pts
                        do ii=i-smooth_pts, i+smooth_pts
                           tem(i,k)= tem(i,k) + slice(ii,kk)/( (2*smooth_pts+1)**2 )
                        enddo
                     enddo 
                  enddo
                enddo
                slice=tem
              enddo
              frst3d(:,:,l)=slice(:,:)
            endif

            do k=1,dims(2)
               do i=1,dims(1)
!tgs bdyfrqini - time interval between analysis time and second time in wrfbdy_d01
                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/real(bdyfrqini)
!tgs                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/real(bdyfrq)

                  !!! cap tendency to avoid model crash
                  select case(trim(vbt_name))
                  case ('U_BTXS','U_BTXE','U_BTYS','U_BTYE','V_BTXS','V_BTXE','V_BTYS','V_BTYE');
                    if (l_limit_uv) then
                      if (tend3d(i,k,l)>uvTendMAX) then
                        write(unit=stdout, fmt='(a,3i5,2e10.2)') 'UV tendency capped(i,k,l):', i,k,l,tend3d(i,k,l),uvTendMAX
                        tend3d(i,k,l)=uvTendMAX
                      else if (tend3d(i,k,l)<uvTendMIN) then
                        write(unit=stdout, fmt='(a,3i5,2e10.2)') 'UV tendency bottomed(i,k,l):', i,k,l,tend3d(i,k,l),uvTendMIN
                        tend3d(i,k,l)=uvTendMIN
                      end if
                    end if
                  case ('T_BTXS','T_BTXE','T_BTYS','T_BTYE');
                    if (l_limit_t) then
                      if (tend3d(i,k,l)>tTendMAX) then
                        write(unit=stdout, fmt='(a,3i5,2e10.2)') 't tendency capped(i,k,l):', i,k,l,tend3d(i,k,l),tTendMAX
                        tend3d(i,k,l)=tTendMAX
                      else if (tend3d(i,k,l)<tTendMIN) then
                        write(unit=stdout, fmt='(a,3i5,2e10.2)') 't tendency bottomed(i,k,l):', i,k,l,tend3d(i,k,l),tTendMIN
                        tend3d(i,k,l)=tTendMIN
                      end if
                    end if
                  case ('QVAPOR_BTXS','QVAPOR_BTXE','QVAPOR_BTYS','QVAPOR_BTYE');
                    if (l_limit_q) then
                      if (tend3d(i,k,l)>qTendMAX) then
                        write(unit=stdout, fmt='(a,3i5,2e12.5)') 'q tendency capped(i,k,l):', i,k,l,tend3d(i,k,l),qTendMAX
                        tend3d(i,k,l)=qTendMAX
                      else if (tend3d(i,k,l)<qTendMIN) then
                        write(unit=stdout, fmt='(a,3i5,2e12.5)') 'q tendency bottomed(i,k,l):', i,k,l,tend3d(i,k,l),qTendMIN
                        tend3d(i,k,l)=qTendMIN
                      end if
                    end if
                  end select
               end do
            end do
         end do
         deallocate(slice)
         deallocate(tem)

         if (debug) then
            write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=new_unit, fmt='(a, 10i12)') &
                 ' new ', (i, i=1,dims(3))

            do j=1,dims(1)
               write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         ! output new variable at first time level
         l_critical=.false.
         if (l_uv) then
            if (knt_shear_max > criticalTrigger) then
              l_critical=.true.
              print*, 'critical condition met, skip updating ', trim(var_name),&
               ' ', trim(vbt_name) 
            endif
         endif
         if (.not. l_critical) then
           if (update_fld) &
           call da_put_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                  dims(1), dims(2), dims(3), 1, debug)
           if (update_tend) &
           call da_put_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                   dims(1), dims(2), dims(3), 1, debug)
         endif

         deallocate(frst3d)
         deallocate(scnd3d)
         deallocate(tend3d)
      end do
      
      deallocate(full3d)
      if ( var4d_lbc ) deallocate(full3d2)
      deallocate(c1, c2)
   end do

   deallocate(MU)
   if ( var4d_lbc ) deallocate(MU2)
   deallocate(u)
   if ( var4d_lbc ) deallocate(u2)
   deallocate(v)
   if ( var4d_lbc ) deallocate(v2)

!--------------------- second time level-----------------------------------------
!- for var4d_lbc, we need to update the second time level LBC
   if ( var4d_lbc .and. update_lateral_bdy .and. time_level > 1 ) then

   east_end=0
   north_end=0

   cdfid = ncopn(da_file_02, NCNOWRIT, io_status )

   ! For 2D variables
   ! Get mu, mub, msfu, and msfv


   io_status = nf_inq_varid(cdfid, trim(varsf(n)), varid)
   if (io_status /= 0 ) then
      print '(/"N=",i2," io_status=",i5,5x,"VAR=",a,a)', &
                n, io_status, trim(varsf(n)), " does not exist"
      stop
   endif

   call da_get_dims_cdf( da_file_02, 'MU', dims, &
      ndims, debug)

   allocate(MU(dims(1), dims(2)))

   call da_get_var_2d_real_cdf( da_file_02, &
      'MU', MU, dims(1), dims(2), 1, debug)

   east_end=dims(1)+1
   north_end=dims(2)+1

   if (east_end < 1 .or. north_end < 1) then
      write(unit=stdout, fmt='(a)') 'Wrong data for Boundary.'
      stop
   end if

   write(unit=stdout,fmt='(/a/)') 'Processing the lateral boundary condition:'

   ! boundary variables
   bdyname(1)='_BXS'
   bdyname(2)='_BXE'
   bdyname(3)='_BYS'
   bdyname(4)='_BYE'

   ! boundary tendancy variables
   tenname(1)='_BTXS'
   tenname(2)='_BTXE'
   tenname(3)='_BTYS'
   tenname(4)='_BTYE'

   do m=1,4
      write(var_name,'(a,a)') 'MU' , trim(bdyname(m))
      write(vbt_name,'(a,a)') 'MU' , trim(tenname(m))

      call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

      allocate(frst2d(dims(1), dims(2)))
      allocate(scnd2d(dims(1), dims(2)))
      allocate(tend2d(dims(1), dims(2)))

      ! Get variable at third time level
      if (time_level > 2) then
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), scnd2d, &
                                   dims(1), dims(2), 3, debug)
      else
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                   dims(1), dims(2), 2, debug)
         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 2, debug)
      end if

      if (debug) then
         write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         call da_get_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                   dims(1), dims(2), 2, debug)

         write(unit=ori_unit, fmt='(a, 10i12)') &
              ' old ', (i, i=1,dims(2))
         do j=1,dims(1)
            write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! calculate variable at second time level
      select case(m)
      case (1) ;             ! West boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*real(bdyfrq)
               frst2d(j,l)=MU(l,j)
            end do
         end do
      case (2) ;             ! East boundary
         do l=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
                  scnd2d(j,l)=frst2d(j,l)+tend2d(j,l)*real(bdyfrq)
               frst2d(j,l)=MU(east_end-l,j)
            end do
         end do
      case (3) ;             ! South boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*real(bdyfrq)
               frst2d(i,l)=MU(i,l)
            end do
         end do
      case (4) ;             ! North boundary
         do l=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
                  scnd2d(i,l)=frst2d(i,l)+tend2d(i,l)*real(bdyfrq)
               frst2d(i,l)=MU(i,north_end-l)
            end do
         end do
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. mu, m=', m
      end select

      ! calculate new tendancy 
      do l=1,dims(2)
         do i=1,dims(1)
            tend2d(i,l)=(scnd2d(i,l)-frst2d(i,l))/real(bdyfrq)
         end do
      end do

      if (debug) then
         write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
              'No.', m, 'Variable: ', trim(vbt_name), &
              'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

         write(unit=new_unit, fmt='(a, 10i12)') &
              ' new ', (i, i=1,dims(2))

         do j=1,dims(1)
            write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                  j, (tend2d(j,i), i=1,dims(2))
         end do
      end if

      ! output new variable at first time level
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(var_name), frst2d, &
                                dims(1), dims(2), 2, debug)
      ! output new tendancy 
      call da_put_var_2d_real_cdf( wrf_bdy_file, trim(vbt_name), tend2d, &
                                dims(1), dims(2), 2, debug)

      deallocate(frst2d)
      deallocate(scnd2d)
      deallocate(tend2d)
   end do

   !---------------------------------------------------------------------
   ! For 3D variables

   ! Get U
   call da_get_dims_cdf( da_file_02, 'U', dims, ndims, debug)

   ! call da_get_att_cdf( da_file_02, 'U', debug)

   allocate(u(dims(1), dims(2), dims(3)))

   allocate(c1(dims(3)),c2(dims(3)))
   if(dims(3)==chlvl) then
      c1=c1h
      c2=c2h
   elseif(dims(3)==cflvl) then
      c1=c1f
      c2=c2f
   else
      write(*,*) 'mismatch dimension',chlvl,cflvl,dims(3)
   endif

   ids=1
   ide=dims(1)-1
   jds=1
   jde=dims(2)
   kds=1
   kde=dims(3)

   call da_get_var_3d_real_cdf( da_file_02, 'U', u, &
                             dims(1), dims(2), dims(3), 1, debug)

   ! Get V
   call da_get_dims_cdf( da_file_02, 'V', dims, ndims, debug)

   allocate(v(dims(1), dims(2), dims(3)))

   call da_get_var_3d_real_cdf( da_file_02, 'V', v, &
                             dims(1), dims(2), dims(3), 1, debug)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'Before couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'Before couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if

   !---------------------------------------------------------------------
   ! Couple u, v.
   call da_couple_uv ( u, v, MU, MUB, msfu, msfv, c1, c2, ids, ide, jds, jde, kds, kde)

   if (debug) then
      write(unit=stdout, fmt='(a,e20.12,4x)') &
           'After  couple Sample u=', u(dims(1)/2,dims(2)/2,dims(3)/2), &
           'After  couple Sample v=', v(dims(1)/2,dims(2)/2,dims(3)/2)
   end if
   deallocate(c1, c2)

   !---------------------------------------------------------------------
   !For 3D variables

   do n=1,num3d
      write(unit=stdout, fmt='(a, i3, 2a)') 'Processing: var3d(', n, ')=', trim(var3d(n))

      call da_get_dims_cdf( da_file_02, trim(var3d(n)), dims, ndims, debug)

      allocate(full3d(dims(1), dims(2), dims(3)))

      east_end=dims(1)+1
      north_end=dims(2)+1

      allocate(c1(dims(3)),c2(dims(3)))
      if(dims(3)==chlvl) then
         c1=c1h
         c2=c2h
      elseif(dims(3)==cflvl) then
         c1=c1f
         c2=c2f
      else
         write(*,*) 'mismatch dimension',chlvl,cflvl,dims(3)
      endif

      select case(trim(var3d(n)))
      case ('U') ;           ! U
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=u(:,:,:)
      case ('V') ;           ! V 
         ! var_pref='R' // trim(var3d(n))
         var_pref=trim(var3d(n))
         full3d(:,:,:)=v(:,:,:)
      case ('W') ;
         ! var_pref = 'R' // trim(var3d(n))
         var_pref = trim(var3d(n))

         call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))/msfm(i,j)
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case ('T', 'PH') ;
         var_pref=trim(var3d(n))
 
         call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
               end do
            end do
         end do

            if (debug) then
               write(unit=stdout, fmt='(3a,e20.12,4x)') &
                    'After  couple Sample ', trim(var3d(n)), &
                    '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
            end if

!      case ('QVAPOR','QNWFA','QNIFA') ;   
      case ('QVAPOR', 'QCLOUD', 'QRAIN', 'QICE', 'QSNOW', 'QGRAUP', 'QNRAIN', 'QNICE', 'QNCLOUD', 'QNWFA', 'QNIFA') ;
         ! var_pref='R' // var3d(n)(1:2)
         ! var_pref=var3d(n)(1:2)
         var_pref=var3d(n)
 
         call da_get_var_3d_real_cdf( da_file_02, trim(var3d(n)), &
            full3d, dims(1), dims(2), dims(3), 1, debug)

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'Before couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if

         do k=1,dims(3)
            do j=1,dims(2)
               do i=1,dims(1)
                  full3d(i,j,k)=full3d(i,j,k)*((c1(k)*mu(i,j))+(c1(k)*mub(i,j)+c2(k)))
               end do
            end do
         end do

         if (debug) then
            write(unit=stdout, fmt='(3a,e20.12,4x)') &
                 'After  couple Sample ', trim(var3d(n)), &
                 '=', full3d(dims(1)/2,dims(2)/2,dims(3)/2)
         end if
      case default ;
         write(unit=stdout,fmt=*) 'It is impossible here. var3d(', n, ')=', trim(var3d(n))
      end select

      do m=1,4
         write(var_name,'(a,a)') trim(var_pref) , trim(bdyname(m))
         write(vbt_name,'(a,a)') trim(var_pref) , trim(tenname(m))

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'Processing: bdyname(', m, ')=', trim(var_name)

         call da_get_dims_cdf( wrf_bdy_file, trim(var_name), dims, ndims, debug)

         allocate(frst3d(dims(1), dims(2), dims(3)))
         allocate(scnd3d(dims(1), dims(2), dims(3)))
         allocate(tend3d(dims(1), dims(2), dims(3)))

         ! Get variable at second time level
         if (time_level > 2) then
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), scnd3d, &
                                      dims(1), dims(2), dims(3), 3, debug)
         else
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                      dims(1), dims(2), dims(3), 2, debug)
            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 2, debug)
         end if

         if (debug) then
            write(unit=ori_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            call da_get_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                      dims(1), dims(2), dims(3), 2, debug)

            write(unit=ori_unit, fmt='(a, 10i12)') &
                 ' old ', (i, i=1,dims(3))
            do j=1,dims(1)
               write(unit=ori_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if
   
         select case(trim(bdyname(m)))
         case ('_BXS') ;             ! West boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*real(bdyfrq)
               frst3d(j,k,l)=full3d(l,j,k)
            end do
            end do
            end do
         case ('_BXE') ;             ! East boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do j=1,dims(1)
               if (time_level < 3) &
               scnd3d(j,k,l)=frst3d(j,k,l)+tend3d(j,k,l)*real(bdyfrq)
               frst3d(j,k,l)=full3d(east_end-l,j,k)
            end do
            end do
            end do
         case ('_BYS') ;             ! South boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*real(bdyfrq)
               frst3d(i,k,l)=full3d(i,l,k)
            end do
            end do
            end do
         case ('_BYE') ;             ! North boundary
            do l=1,dims(3)
            do k=1,dims(2)
            do i=1,dims(1)
               if (time_level < 3) &
               scnd3d(i,k,l)=frst3d(i,k,l)+tend3d(i,k,l)*real(bdyfrq)
               frst3d(i,k,l)=full3d(i,north_end-l,k)
            end do
            end do
            end do
         case default ;
            write(unit=stdout,fmt=*) 'It is impossible here.'
            write(unit=stdout,fmt=*) 'bdyname(', m, ')=', trim(bdyname(m))
            stop
         end select

         write(unit=stdout, fmt='(a, i3, 2a)') &
            'cal. tend: bdyname(', m, ')=', trim(vbt_name)

         ! calculate new tendancy 
         do l=1,dims(3)
            do k=1,dims(2)
               do i=1,dims(1)
                  tend3d(i,k,l)=(scnd3d(i,k,l)-frst3d(i,k,l))/real(bdyfrq)
               end do
            end do
         end do

         if (debug) then
            write(unit=new_unit, fmt='(a,i2,2x,2a/a,i2,2x,a,4i6)') &
                 'No.', m, 'Variable: ', trim(vbt_name), &
                 'ndims=', ndims, 'dims=', (dims(i), i=1,ndims)

            write(unit=new_unit, fmt='(a, 10i12)') &
                 ' new ', (i, i=1,dims(3))

            do j=1,dims(1)
               write(unit=new_unit, fmt='(i4, 1x, 10e20.7)') &
                     j, (tend3d(j,dims(2)/2,i), i=1,dims(3))
            end do
         end if

         ! output new variable at first time level
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(var_name), frst3d, &
                                dims(1), dims(2), dims(3), 2, debug)
         call da_put_var_3d_real_cdf( wrf_bdy_file, trim(vbt_name), tend3d, &
                                   dims(1), dims(2), dims(3), 2, debug)

         deallocate(frst3d)
         deallocate(scnd3d)
         deallocate(tend3d)
      end do
      
      deallocate(full3d)
      deallocate(c1, c2)
   end do

   deallocate(MU)
   deallocate(u)
   deallocate(v)

   end if ! end of update second time level LBC for var4d_lbc

   deallocate(MUB)
   deallocate(msfu)
   deallocate(msfv)
   deallocate(times)
   deallocate(thisbdytime)
   deallocate(nextbdytime)

 end if ! end if update_lateral_bdy

 write(unit=stdout,fmt=*) &
    '=================================================================='
 if ( update_lateral_bdy ) then
    write(unit=stdout,fmt=*) 'Lateral boundary tendency updated.'
 end if
 if ( update_low_bdy ) then
    write(unit=stdout,fmt=*) 'Low boundary updated with wrf_input fields.'
 end if
 if ( update_lsm ) then
    write(unit=stdout,fmt=*) 'LSM variables updated with wrf_input fields.'
 end if

   if (io_status == 0) &
      write (unit=stdout,fmt=*) "*** Update_bc completed successfully ***"


endif ! mype==0

  call MPI_FINALIZE(ierror)
!

end program da_update_bc

