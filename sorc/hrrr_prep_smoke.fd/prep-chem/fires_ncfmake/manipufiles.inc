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
  else
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

!------------------------------------------------------------------------------
subroutine file_manipulate ( BINRYFIL,ncfile_diagnostic,ncfile_wrfinput,length_input)

  ! SAM 10/27/11, taken from NCAR netcdf utilites - reads WRF/Chem netcdf files,
  ! and generates a netcdf file for output variables defined in module/species.f

  use species

  implicit none

  interface
     subroutine nferr_0_or_die(ierr,filename,action,variable,attribute)
       implicit none
       character(len=*),intent(in) :: filename,action
       character(len=*),intent(in),optional :: variable,attribute
       integer, intent(in) :: ierr

     end subroutine nferr_0_or_die
  end interface

  include 'netcdf.inc'

  character (len=*), intent(in)                   :: ncfile_diagnostic
  character (len=*), intent(in)                   :: ncfile_wrfinput
  character (len=10)                               :: option
  character (len=3)                                :: go_change
  !character (len=10), intent(in)                   :: plot_var
  integer                                          :: i,j,k, ivtype, length_input
  integer                                          :: time1, time2
  logical                                          :: op_diag = .TRUE.
  logical                                          :: op_att = .TRUE.
  logical                                          :: op_rot = .false.
  logical                                          :: FirstTime = .TRUE.
  logical                                          :: static = .FALSE.

  character (len=120)                               :: varnam, att_name, value_chr, print_time
  character (len=80)                               :: att_sav(10)
  integer                                          :: dimids(10), FieldType
  character (len=80)                               ::  units, MemoryOrder, description, stagger

  integer                                          :: cdfid, rcode, id_var, id_varo
  integer                                          :: id_att, attlen, ios
  integer                                          :: nDims, nVars, nAtts, unlimDimID, dims(4)
  integer                                          :: unit_place, order_place, nVars_sav
  integer                                          :: type_to_get, dims3, iatt
  integer                                          :: id_time, n_times, itimes

  double precision,  allocatable, dimension(:,:,:) :: data_dp_r
  real,    allocatable, dimension(:,:,:)           :: data_r
  integer, allocatable, dimension(:,:,:)           :: data_i
  real,    allocatable, dimension(:,:)             :: data_ts
  real,    allocatable, dimension(:)               :: value_real
  real*8,  allocatable, dimension(:)               :: value_dble
  real,    allocatable, dimension(:,:)             :: xlat, xlong, diff, alpha, hgt, xlon2d, xlat2d
  real,    allocatable, dimension(:,:,:)           :: uuu, vvv, pressure, height, tk, tmp
  real,    allocatable, dimension(:,:,:)           :: rh,moist, AODhld
  real,    allocatable, dimension(:,:,:)           :: tauaer1,tauaer2,tauaer3,tauaer4,pm25_dry,pm25_intrn,ext55wrf
  real,    allocatable, dimension(:,:,:,:)         :: data1d
  integer                                          :: istarto(4), iendo(4)
  integer                                          :: istarto2(3), iendo2(3)  ! start,end indexes for 2-D netcdf output arrays

  character (len=80)                               :: times(999)
  character (len=120)                              :: dname
  integer                                          :: istart(4), iend(4), isample(4)
  integer                                          :: istart_t(2), iend_t(2)
  real                                             :: sample_value_r, minvalue_r, maxvalue_r
  integer                                          :: sample_value_i, minvalue_i, maxvalue_i
  integer                                          :: value_int
  integer                                          :: wedim, sndim, btdim, kk, ii, jj, iii, dval
  integer :: timedimid,datedimid,wedimid,sndimid,btdimid

  integer                                          :: map_proj
  real                                             :: truelat1, truelat2, stand_lon, cone
  real                                             :: dx, dy, dummy_stdlon, dummy_lon1
  real, parameter                                  :: PI = 3.141592653589793
  real, parameter                                  :: RAD_PER_DEG = PI/180.
  ! 3/28/18 scale Santa Rosa Fire emissions to 2100. tonnes CO/hr, BBEM2 emission
  ! sums are calculated offline in plot program, and include flaming fraction
  ! increases that will go into WRF/Chem
  real, parameter                                  :: EM_SCALE = 1. !2100./92510. !  WARNING-WARNING-WARNING emission scaling done
  integer                                          :: ts_xy(3)
  real                                             :: ts_ll(3)
  character (len=10)                               :: ts_var(100)
  integer                                          :: ts_i
  character (len=2)                                :: ts_type
  character,    allocatable, dimension(:,:)        :: time_out
  integer                                          :: ncod,rcodeo,L1C,L2C
  real                                             :: aodsum,fac,facint
  integer, parameter                               :: nbin_o=8
  REAL , allocatable :: chem(:,:,:,:),dz8w(:,:,:),alt(:,:,:),relhum(:,:,:)
  REAL , allocatable :: chm(:,:,:,:)
  integer                      ::    ids,ide,jds,jde, kds,kde,          &
       ims,ime, jms,jme, kms,kme,                             &
       its,ite, jts,jte, kts,kte
  !
  integer                      ::    n3d
  REAL,    allocatable, dimension(:,:,:)         :: data3dn
  REAL,    allocatable, dimension(:,:)         :: data2dn
  integer                                          :: nstar
  ! Constants for Murphy and Koop (2005) saturation vapor pressure (over water) formula
  real                                             :: alph,d2,d3,d4,esat,eamb
  real, parameter                                  :: zeroC = 273.15

  character (len=256), intent(in)                   :: BINRYFIL
  integer                                           :: p_3d,p_2d
  type_to_get = 5
  time1 = 0
  time2 = 0

  ! Open binary file from prep_chem_sources
  OPEN(13,FILE=BINRYFIL,form='unformatted')
  read(13) i
  write(6,*)i,' matrices in binary file'
  read(13) option
  write(6,*)option,'=ENAME'
  read(13) i
  write(6,*)i,'=TIME inbinary file QQ'
write(0,*) 'got here'
  ! Open netCDF file 
write(0,*) 'got here'
  rcode = nf_open(trim(ncfile_wrfinput), NF_WRITE, cdfid )
write(0,*) 'got here'
  call nferr_0_or_die(rcode,'nf_open for NF_WRITE',ncfile_wrfinput)
  ! Get the times first:
write(0,*) 'got here'
  rcode = nf_inq_varid ( cdfid, 'Times', id_var )
  call nferr_0_or_die(rcode,'nf_inq_varid',ncfile_wrfinput,'Times')
  id_time=id_var
write(0,*) 'got here'
  rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  call nferr_0_or_die(rcode,'nf_inq_var',ncfile_wrfinput,varnam)
write(0,*) 'got here'
  do i=1,ndims
     rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
     write(*,'(A,3(I,1X))')'Time variable ndims,i,dimids(i),dims(i)=',i,dimids(i),dims(i)
     call flush(6)
  enddo
write(0,*) 'got here'
  n_times = dims(2)
  do i=1,dims(2)
     istart_t(1) = 1
     iend_t(1) = dims(1)
     istart_t(2) = i
     iend_t(2) = 1
     rcode = NF_GET_VARA_TEXT  ( cdfid, id_time,      &
          istart_t, iend_t, times(i))
     call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_GET_VARA_TEXT')
  enddo
  ! n_times = dims(1)
  ! Pull of time from 'reference_date' attribute of variable=time
  !         do iatt = 1,natts
  !         rcode = nf_inq_attname(cdfid,id_time,iatt,att_name)
  !        L2C = max(1,index(att_name,' ')-1)
  !        if(att_name(1:L2C).eq.'reference_date')then
  !     rcode = NF_GET_ATT_TEXT(cdfid, id_time, att_name, value_chr )
  !        write(Times(n_times),'(A19)')value_chr(1:19)
  !        endif
  !         enddo


  print*,"TIMES in file"
  do itimes = 1,n_times
     print_time = times(itimes)
     print*,trim(print_time)
  enddo
  !   write(datestamp,'(A4,4A2)')print_time(1:4),print_time(6:7),print_time(9:10), &
  !   print_time(12:13),print_time(15:16)
  !   rcodeo = nf_create('op_'//datestamp//'.nc', nf_netcdf4, ncod ) ! 2/21/18  put datestamp in output filename
  rcodeo = nf_create(ncfile_diagnostic, nf_netcdf4, ncod ) ! 2/21/18  put datestamp in output filename
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_create for NF_NETCDF4')
  !  if( rcodeo == 0) then
  !    write(6,*) ' Output file opened, writing constant latitude_longitude'
  !     rcodeo = nf_def_var(ncod,'latitude_longitude',NF_INT,0,dimids,id_var)
  !     write(*,*)'LtLn const,id_var,rcodeos=',id_var,rcodeo
  !     call flush(6)
  !      att_name='grid_mapping_name'
  !      value_chr='latitude_longitude'
  !      attlen=index(value_chr,'  ')-1
  !      rcodeo = NF_PUT_ATT_TEXT(ncod, id_var,att_name,attlen, value_chr )
  !      att_name='longitude_of_prime_meridian';attlen=1
  !      rcodeo = NF_PUT_ATT_REAL(ncod,id_var,att_name,NF_REAL,attlen, 0. )
  !      att_name='earth_radius';attlen=1
  !      rcodeo = NF_PUT_ATT_REAL(ncod, id_var,att_name,NF_REAL,attlen, 6371229. )
  !  else
  !    write(6,*) ' error opening output netcdf file scratch.ncf'
  !    stop
  !  end if
  do i=1,dims(2)
     istart_t(1) = 1
     iend_t(1) = dims(1)
     istart_t(2) = i
     iend_t(2) = 1
     rcode = NF_GET_VARA_TEXT  ( cdfid, id_time,      &
          istart_t, iend_t, times(i))
     call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_GET_VARA_TEXT')
  enddo

  rcode = nf_inq(cdfid, nDims, nVars, nAtts, unlimDimID)
  call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_inq')
  write(*,*)'nAtts,nDims= ',nAtts,nDims
  call flush(6)
  nVars_sav = nVars
write(0,*) 'got here'

  ! Get some header information
  btdim = 24   ! max for geo_grid static data - not that we will need this
  do ii = 1, nDims
     rcode = nf_inq_dim(cdfid, ii, dname, dval)
     call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_inq_dim')
     if     (dname .eq. 'west_east') then
        wedim = dval
     elseif (dname .eq. 'south_north') then
        sndim = dval
     elseif (dname .eq. 'bottom_top') then
        btdim = dval
        !   elseif (dname .eq. 'num_metgrid_levels') then
        !     btdim = dval
     endif
  enddo

  if(nAtts.gt.0)then ! 10/27/11, AFWA files have no attributes
     print*,"GLOBAL ATTRIBUTES:"
     print*," "
     do iatt = 1,nAtts
        rcode = nf_inq_attname(cdfid,nf_global,iatt,att_name)
        call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_inq_attname')
        rcode = nf_inq_att( cdfid,nf_global,att_name,ivtype,attlen )
        call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_inq_att')
        if (ivtype == 2) then
           !     L1C=index(att_name,' ')-1
           L1C=len_trim(att_name)
           rcode = NF_GET_ATT_TEXT(cdfid, nf_global, att_name, value_chr )
           call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_GET_ATT_TEXT')
           ! Modify character attributes for Optical output netcdf file creates, specific to CLASSIC netcdf output
           L2C = max(1,index(value_chr,'Model')+4)
           IF(att_name(1:L1C).EQ.'source')THEN
              value_chr=value_chr(1:L2C)//', Optical Output'
              !         attlen = max(1,index(value_chr,' ')-1)
              attlen = len_trim(value_chr)
              rcodeo = NF_PUT_ATT_TEXT(ncod, nf_global, att_name,attlen, value_chr )
              call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_TEXT')
              ! End of character modifies
           else
              rcodeo = NF_PUT_ATT_TEXT(ncod, nf_global, att_name,attlen, value_chr )
              call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_TEXT')
           endif

           write(*,'(A," : ",A)') att_name(1:40),value_chr(1:attlen)
        elseif (ivtype == 4) then
           rcode = NF_GET_ATT_INT(cdfid, nf_global, att_name, value_int )
           call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_GET_ATT_INT')
           rcodeo = NF_PUT_ATT_INT(ncod, nf_global, att_name,NF_INT,attlen, value_int )
           call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_INT')
           write(6,'(A," : ",i5)') att_name(1:40),value_int
        elseif (ivtype == 5) then
           allocate (value_real(attlen))
           rcode = NF_GET_ATT_REAL(cdfid, nf_global, att_name, value_real )
           call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_GET_ATT_REAL')
           rcodeo = NF_PUT_ATT_REAL(ncod, nf_global, att_name,NF_FLOAT,attlen, value_real )
           call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_REAL')
           if (attlen .gt. 1) then
              print*,att_name(1:40),": ",value_real
           else
              write(6,'(A," : ",e12.4)') att_name(1:40),value_real(1)
           endif
           deallocate (value_real)
        endif
     enddo
     ! Add attribute title to netcdf file
     att_name(1:5)='TITLE'
     value_chr='Custom netcdf for binary emission files from prep_chem_sources (Stu McKeen,3-18)'
     attlen = len_trim(value_chr)
     rcodeo = NF_PUT_ATT_TEXT(ncod, nf_global, att_name,attlen, value_chr )
     call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_TEXT')
     print*," "
     print*,"--------------------------------------------------------------------- "
     print*," "

     rcode = NF_GET_ATT_TEXT(cdfid, nf_global, "TITLE", value_chr )
     call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_global')
     if ( INDEX(value_chr,'OUTPUT FROM ') == 0 ) then
        !! diagnostics only available for wrfout data
        print*,"This is not a wrfout file - no diagnostics will be done"
        stop
     endif

  else ! Part of if(nAtts>0) test - if you made it here, there are no attributes in input netcdf file
     att_name(1:5)='TITLE'
     value_chr='Custom netcdf for binary emission files from prep_chem_sources (Stu McKeen,3-18)'
     !         attlen = max(1,index(value_chr,' ')-1)
     attlen = len_trim(value_chr)
     rcodeo = NF_PUT_ATT_TEXT(ncod, nf_global, att_name,attlen, value_chr )
     call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_ATT_TEXT')
     !         write(*,*)'Attribute write to output, rcodeo= ',rcodeo
     !         call flush(6)
     !         stop888
  endif  ! End of if(nAtts>0) test

write(0,*) 'got here'

  ! OPTIONS
  !===========================================================================================

  ! Before we start, sort out locations if we are generating time series output
  !     if ( option  == "-ts" ) then
  !       allocate (data_ts(ts_i,btdim+1))
  !       nVars = ts_i
  !       !!if ( ts_type == "ll" ) call calc_nearest_xy(cdfid,ts_xy,ts_ll)
  !       if ( ts_type == "ll" ) then
  !          call latlon_to_ij( cdfid, proj, ts_ll, ts_xy )
  !          if ( ts_xy(1) < 0 .OR. ts_xy(1) > wedim ) STOP '   STOP: LON outside domain'
  !          if ( ts_xy(2) < 0 .OR. ts_xy(2) > sndim ) STOP '   STOP: LAT outside domain'
  !       endif
  !     endif


  ! Write dimension information to output netcdf
  rcodeo = nf_def_dim(ncod, 'Time', n_times,timedimid) ! Only one time slice in file
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_dim','Time')
  rcodeo = nf_def_dim(ncod, 'DateStrLen', 19,datedimid) ! consistency with 2014 output
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_dim','DateStrLen')
  rcodeo = nf_def_dim(ncod, 'west_east', wedim,wedimid) ! consistency with 2014 output
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_dim','west_east')
  rcodeo = nf_def_dim(ncod, 'south_north', sndim,sndimid) ! consistency with 2014 output
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_dim','south_north')
  !     rcodeo = nf_def_dim(ncod, 'longitude', wedim,L1C) ! consistency with 2014 output
  !     rcodeo = nf_def_dim(ncod, 'latitude', sndim,L1C) ! consistency with 2014 output
  !     rcodeo = nf_def_dim(ncod, 'bottom_top', btdim,L1C) ! consistency with 2014 output
  rcodeo = nf_def_dim(ncod, 'bottom_top', 1,btdimid) ! consistency with 2014 output
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_dim','bottom_top')
  !     nDimso=5
  ! Write Time information to output netcdf
  !rcode = nf_inq_var( cdfid, id_time, varnam, ivtype, ndims, dimids, natts )
  !call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_inq_var')
  ! rcodeo = nf_def_var(ncod,'Times',NF_CHAR,ndims,dimids,L1C)
  dimids(1)=datedimid
  dimids(2)=timedimid
  rcodeo = nf_def_var(ncod,'Times',NF_CHAR,2,dimids,L1C)
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_def_var')

write(0,*) 'got here'

  !     write(*,*)'NF_NOERR,rcodeo= ',NF_NOERR,rcodeo
  ! Time has no attributes
  call initializ_3d(cdfid,ncod)
  call initializ_2d(cdfid,ncod)
  call initialize_wrfinput_2d(cdfid,ncfile_wrfinput)
  !     if(num_rw.gt.0)call ncf_readwrite( cdfid,ncod,0,0)
  rcodeo=nf_enddef(ncod)
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_enddef')
  !     rcodeo = nf_close(ncod)
  !   rcodeo = nf_open('op'//trim(file), NF_WRITE, ncod )
  ! 7/20/17 after define mode turned off, now in data mode - transfer time indepen. data to new file
  ! intime=0,iflag=1 means write out time independent file from orig to new
  !     if(num_rw.gt.0)call ncf_readwrite( cdfid,ncod,0,1)
  !     times(1)='2005-06-15_12:00:00'
  do i=1,2
     rcodeo = nf_inq_dimlen( ncod, dimids(i), dims(i) )
     call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_inq_dimlen')
     write(*,*)'i,dimids(i),dims(i)=',i,dimids(i),dims(i)
     print *,'dim ',i,' id ',dimids(i),' is ',dims(i)
  enddo
  if (allocated(time_out)) deallocate(time_out)
  allocate ( time_out(dims(1),dims(2)))
  do i=1,dims(1)
     time_out(i,1) = times(1)(i:i)
     write(*,*)'times def,i,time_out(i,1)=',i,time_out(i,1)
     call flush(6)
  enddo
  rcodeo = nf_inq_varid ( ncod, "Times", id_var )
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_inq_varid','Times')
  rcodeo = NF_PUT_VAR_TEXT( ncod,id_var,time_out)
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_VAR_TEXT')
  ! do i=1,dims(2)
  !   istart_t(1) = 1
  !   iend_t(1) = dims(1)
  !   istart_t(2) = i
  !   iend_t(2) = 1
  !   rcodeo = NF_PUT_VARA_TEXT  ( ncod, id_time,      &
  !                    istart_t, iend_t, times(i))
  ! enddo
write(0,*) 'got here'

  ! Now we are ready to start with the time loop
  if ( time1 == 0 ) time1 = 1
  if ( time2 == 0 ) time2 = n_times

  do itimes = time1,time2
     !   do itimes = time1,time1
     if ( static ) then
        print_time = " "            
     else
        print_time = times(itimes)
     endif

     istart        = 1
     istart(4)     = itimes
     iend          = 1
     iend(1)       = wedim
     iend(2)       = sndim
     !      iend(3)       = btdim
     iend(3)       = 1
     iendo=iend
     istarto=istart
     istarto2(1)=istarto(1)
     istarto2(2)=istarto(2)
     istarto2(3)=istart(4)
     iendo2(1)       = iend(1)
     iendo2(2)       = iend(2)
     iendo2(3)       = iend(4)
     !     if(num_rw.gt.0)call ncf_readwrite( cdfid,ncod,itimes,1)
     if (allocated(data1d)) deallocate(data1d)
     allocate ( data1d(1:iend(1),1:iend(2),1:iend(3),1))
     if (allocated(AODhld)) deallocate(AODhld)
     allocate ( AODhld(1:iend(1),1:iend(2),1))
     if (allocated(data2dn)) deallocate(data2dn)
     allocate ( data2dn(1:iend(1),1:iend(2)))
     write(6,*) 'iend(1,2)=',iend(1),iend(2)

     !     do n3d=1,num3dnew
     ! First 26 binary matrices are output with 4 dimensions, all chem/aerosol
     ! species
     !KY      do n3d=1,26
write(0,*) 'write 3d vars'
     do n3d=1,7
        write(*,*)'n3d=',n3d
        call flush(6)
        read(13)data2dn
        do ii=1,iend(1)
           do jj=1,iend(2)
              data1d(ii,jj,1,1)=EM_SCALE*data2dn(ii,jj) !  WARNING-WARNING-WARNING emission scaling done
           enddo
        enddo
        rcodeo = NF_PUT_VARA_REAL( ncod,idvaro3dn(n3d),istarto,iendo,data1d)
        call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_VARA_REAL',var3dnew(n3d))
        !KY      write(*,*)'idvaro3dn(n3d)=',idvaro3dn(n3d)
     enddo
     ! Next 5 binary matrices are flam fract, frp and fire size matrices, output with 3 dimensions
write(0,*) 'write 2d vars'
     do n3d=1,6
        write(*,*)'n3d=',n3d
        call flush(6)
        read(13)data2dn
        do ii=1,iend(1)
           do jj=1,iend(2)
              AODhld(ii,jj,1)=data2dn(ii,jj)
           enddo
        enddo
        print *,'write istart = ',istarto2
        print *,'write   iend = ',iendo2
        rcodeo = NF_PUT_VARA_REAL( ncod,idvaro2dn(n3d),istarto2,iendo2,AODhld)
        call nferr_0_or_die(rcodeo,ncfile_diagnostic,'NF_PUT_VARA_REAL',var2dnew(n3d))

        if( wrfinput(n3d) /= 0 ) then
           rcode = NF_PUT_VARA_REAL(cdfid,varid_wrfinput(n3d),istarto2,iendo2,AODhld)
           call nferr_0_or_die(rcode,ncfile_wrfinput,'NF_PUT_VARA_REAL',var2dnew(n3d))
        endif
     enddo
     ! Next 6 binary matrices are extra che/aerosol species, output with 4 dimensions
     !KY   do n3d=num3dnew-7,num3dnew-2
     !KY   write(*,*)'n3d=',n3d
     !KY   call flush(6)
     !KY   read(13)data2dn
     !KY   do ii=1,iend(1)
     !KY   do jj=1,iend(2)
     !KY   data1d(ii,jj,1,1)=EM_SCALE*data2dn(ii,jj) !  WARNING-WARNING-WARNING emission scaling done
     !KY   enddo
     !KY   enddo
     !KY   rcodeo = NF_PUT_VARA_REAL( ncod,idvaro3dn(n3d),istarto,iendo,data1d)
     !KY   enddo
     ! Last 2  matrices are zeros, not included in the prep_chem_sources output
     !KY   do n3d=num3dnew-1,num3dnew
     !KY   write(*,*)'n3d=',n3d
     !KY   call flush(6)
     !KY   do ii=1,iend(1)
     !KY   do jj=1,iend(2)
     !KY   data1d(ii,jj,1,1)=0.
     !KY   enddo
     !KY   enddo
     !KY   rcodeo = NF_PUT_VARA_REAL( ncod,idvaro3dn(n3d),istarto,iendo,data1d)
     !KY   enddo
     ! deallocate everything 
     !    if (type_to_get .eq. 5) deallocate (data_r)
     !    if (type_to_get .eq. 6) deallocate (data_dp_r)
     !    if (type_to_get .eq. 4) deallocate (data_i)

  enddo ! end of do itimes = time1,time2 loop
write(0,*) 'got here'

  !===========================================================================================
  write(6,*)'End of reading binary file and netcdf output'
  close(13)
  ! END OF OPTIONS
  rcodeo = nf_close(ncod)
  call nferr_0_or_die(rcodeo,ncfile_diagnostic,'nf_close')
  rcode = nf_close(cdfid)
  call nferr_0_or_die(rcode,ncfile_wrfinput,'nf_close')
  ! call ncclos(cdfid,rcode)
  print*,"rcode,rcodeo=  ",rcode,rcodeo
  print*,"   --- End of input file ---   "

end subroutine file_manipulate
!-------------------------------------------------------------------------------------------
