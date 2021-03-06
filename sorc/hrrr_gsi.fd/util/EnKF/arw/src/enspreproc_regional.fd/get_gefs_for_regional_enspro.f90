subroutine get_gefs_for_regional_enspro(enpert4arw,wrt_pert_sub,wrt_pert_mem,jcap_ens)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gefs_for_regionl  read gefsozone for regional
!   prgmmr: parrish          org: np22                date: 2010-09-26
!
! abstract: read gefs and interpolate to regional ensemble grid.
!          (adaptation of get_gefs_ensperts_dualres)
!
!
! program history log:
!   2010-09-26  parrish, initial documentation
!   2012-01-17  wu, clean up, add/setup option "full_ensemble"
!   2012-02-08  parrish - a little more cleanup
!   2012-10-11  wu      - dual resolution for options of regional hybens
!   2013-02-21  wu      - add call to general_destroy_spec_vars to fix memory problem
!   2013-10-19  todling - all guess variables in met-guess
!   2014-11-30  todling - update interface to general_read_gfs routines
!   2014-12-03  derber - changes to call for general_read_gfsatm
!   2015-05-12  wu      - changes to read in multiple ensemble for 4DEnVar
!   2015-09-20  s.liu   - use general sub2grid in grads1a
!   2016-05-19  Carley/s.liu   - prevent the GSI from printing out erroneous error  
!                               when using ensembles from different time
!   2020-07-01  Bi   - add code to get netCDF data, if use_gfs_ncio=.true.
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: idsl5,regional,use_gfs_nemsio,use_gfs_ncio,&
                     ncepgfs_head,ncepgfs_headv
  use gridmod, only: nlon,nlat,lat2,lon2,nsig,rotate_wind_ll2xy
  use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens
  use hybrid_ensemble_parameters, only: ps_bar,nelen
  use hybrid_ensemble_parameters, only: n_ens,grd_ens,grd_anl,grd_a1,grd_e1,p_e2a,uv_hyb_ens,dual_res
  use hybrid_ensemble_parameters, only: full_ensemble,q_hyb_ens,l_ens_in_diff_time,write_ens_sprd
  use hybrid_ensemble_parameters, only: ntlevs_ens,ensemble_path
 !use hybrid_ensemble_parameters, only: add_bias_perturbation
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundledestroy
  use constants,only: zero,half,fv,rd_over_cp,one,h300,i_missing,r60,r3600
  use constants, only: rd,grav
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_min,mpi_max
  use mpimod, only: mpi_info_null,mpi_offset_kind,mpi_mode_create
  use mpimod, only: mpi_mode_wronly
  use kinds, only: r_kind,i_kind,r_single
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub,general_sub2grid
  use general_sub2grid_mod, only: general_suba2sube,general_sube2suba
  use general_sub2grid_mod, only: general_sub2grid_destroy_info
  use general_sub2grid_mod, only: general_gather2grid
  use general_specmod, only: spec_vars,general_init_spec_vars,general_destroy_spec_vars
  use egrid2agrid_mod, only: g_create_egrid2points_slow,egrid2agrid_parm,g_egrid2points_faster
  use sigio_module, only: sigio_intkind,sigio_head,sigio_srhead
  use guess_grids, only: ges_prsl,ntguessig
  use guess_grids, only: ges_tsen,ifilesig,hrdifsig
  use aniso_ens_util, only: intp_spl
  use obsmod, only: iadate
  use mpimod, only: npe
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_metguess_mod, only: GSI_MetGuess_Bundle
  use mpeu_util, only: die
  use gsi_4dvar, only: nhr_assimilation
  use get_wrf_mass_ensperts_mod, only: get_wrf_mass_ensperts_class

  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
  use module_fv3gfs_ncio, only: Dimension, Dataset, open_dataset, get_dim, &
                                read_vardata, get_idate_from_time_units,&
                                read_attribute, close_dataset

  implicit none

  logical, intent(in) :: enpert4arw,wrt_pert_sub,wrt_pert_mem
  integer(i_kind),intent(in) :: jcap_ens
  type(sub2grid_info) grd_gfs,grd_mix,grd_gfst,grd_arw
  type(get_wrf_mass_ensperts_class) :: wrf_mass_ensperts
  type(spec_vars) sp_gfs
  real(r_kind),allocatable,dimension(:,:,:) :: pri,prsl,prsl1000
  real(r_kind),pointer,dimension(:,:,:) :: vor =>null()
  real(r_kind),pointer,dimension(:,:,:) :: div =>null()
  real(r_kind),pointer,dimension(:,:,:) :: u   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: v   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: tv  =>null()
  real(r_kind),pointer,dimension(:,:,:) :: q   =>null()
  real(r_kind),pointer,dimension(:,:,:) :: cwmr=>null()
  real(r_kind),pointer,dimension(:,:,:) :: oz  =>null()
  real(r_kind),pointer,dimension(:,:)   :: z =>null()
  real(r_kind),pointer,dimension(:,:)   :: ps=>null()
  real(r_kind),allocatable,dimension(:) :: ak5,bk5,ck5,tref5
  real(r_kind),allocatable :: work_sub(:,:,:,:),work(:,:,:,:),work_reg(:,:,:,:)
  real(r_kind),allocatable :: tmp_ens(:,:,:,:),tmp_anl(:,:,:,:),tmp_ens2(:,:,:,:)
  real(r_kind),allocatable,dimension(:,:,:)::stbar,vpbar,tbar,rhbar,ozbar,cwbar
  real(r_kind),allocatable,dimension(:,:)::  pbar_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:)::st_eg,vp_eg,t_eg,rh_eg,oz_eg,cw_eg
  real(r_kind),allocatable,dimension(:,:,:):: p_eg_nmmb
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl_e
  real(r_kind),allocatable,dimension(:,:,:)::tsen,qs
  real(r_kind),allocatable,dimension(:,:,:)::ut,vt,tt,rht,ozt,cwt
  real(r_single),allocatable,dimension(:,:,:):: w3
  real(r_single),allocatable,dimension(:,:):: w2
  real(r_single),allocatable,dimension(:,:,:,:)::en_perts
  real(r_kind),dimension(:,:,:,:),allocatable:: workh
  real(r_kind),dimension(:),allocatable:: z1
  integer, dimension(:), allocatable :: pert_unit

  logical, parameter :: wordy_write_logging=.false.

  logical :: iwrite
  integer :: nproc, my_write_count, writing_rank, workh_index, iworkh

  character(len=*),parameter::myname='get_gefs_for_regional'
  real(r_kind) bar_norm,sig_norm,kapr,kap1,trk
  integer(i_kind) iret,i,j,k,k2,n,mm1,iderivative
  integer(i_kind) mype_out
  integer(i_kind) ic2,ic3,it
  integer(i_kind) ku,kv,kt,kq,koz,kcw,kz,kps
  character(255) filename,filelists(ntlevs_ens)
  character(6) sfilename

  logical ice
  integer(sigio_intkind):: lunges = 11
  type(sigio_head):: sighead
  type(egrid2agrid_parm) :: p_g2r
  integer(i_kind) inner_vars,num_fields,nlat_gfs,nlon_gfs,nsig_gfs,jcap_gfs,jcap_gfs_test
  integer(i_kind) nord_g2r,num_fieldst
  logical,allocatable :: vector(:)
  real(r_kind),parameter::  zero_001=0.001_r_kind
  real(r_kind),allocatable,dimension(:) :: xspli,yspli,xsplo,ysplo
  integer(i_kind) iyr,ihourg,kr
  integer(i_kind),dimension(7):: idate
  integer(i_kind),dimension(4):: idate4
  integer(i_kind),dimension(8) :: ida,jda 
  integer(i_kind),dimension(5) :: iadate_gfs
  integer(i_kind),dimension(6):: idate6
  real(r_kind) hourg
  real(r_kind),dimension(5):: fha
  integer(i_kind) istatus
  real(r_kind) rdog,h,dz
  real(r_kind),allocatable::height(:),zbarl(:,:,:)
  logical add_bias_perturbation,inithead
  integer(i_kind) n_ens_temp
  real(r_kind),allocatable::psfc_out(:,:)
  integer(i_kind) ilook,jlook,ier
  character(len=3)   :: charfhr
  character(len=7) charmem

  type(Dataset) :: atmges
  type(ncepgfs_head):: gfshead
  type(ncepgfs_headv):: gfsheadv
  real(r_single),allocatable,dimension(:) :: aknc, bknc, fhour1
  type(Dimension) :: ncdim

  real(r_kind) dlon,dlat,uob,vob,dlon_ens,dlat_ens
  integer(i_kind) ii,jj,n1
  integer(i_kind) iimax,iimin,jjmax,jjmin
  integer(i_kind) nming1,nming2
  integer(i_kind) its,ite
  real(r_kind) ratio_x,ratio_y

  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer(i_kind) :: idvc,idsl,lonb,latb,levs,jcap,nvcoord
  character(8) filetype, mdlname
  real(r_single),allocatable,dimension(:,:,:) :: vcoord
  integer(i_kind) iret2
  type(nemsio_gfile) :: gfile_atm

  type(gsi_bundle) :: atm_bundle
  type(gsi_grid)   :: atm_grid
  integer(i_kind),parameter :: n2d=2
  integer(i_kind),parameter :: n3d=8
  character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
  character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                  'vor ', 'div ', &
                                                  'tv  ', 'q   ', &
                                                  'cw  ', 'oz  '  /)

  real(r_kind), pointer :: ges_ps(:,:  )=>NULL()
  real(r_kind), pointer :: ges_z (:,:  )=>NULL()
  real(r_kind), pointer :: ges_u (:,:,:)=>NULL()
  real(r_kind), pointer :: ges_v (:,:,:)=>NULL()
  real(r_kind), pointer :: ges_tv(:,:,:)=>NULL()
  real(r_kind), pointer :: ges_q (:,:,:)=>NULL()
  real(r_kind), allocatable :: ges_z_ens(:,:)

  integer(i_kind) :: iunit,lunit,count
  integer(mpi_offset_kind) :: disp
  character(len=500) :: filenameout

  add_bias_perturbation=.false.  !  not fully activated yet--testing new adjustment of ps perturbions 1st

  if(ntlevs_ens > 1) then
     do i=1,ntlevs_ens
        write(filelists(i),'("filelist",i2.2)')ifilesig(i)
     enddo
     its=1
     ite=ntlevs_ens
  else
     write(filelists(1),'("filelist",i2.2)')nhr_assimilation
     its=ntguessig
     ite=ntguessig
  endif

  do it=its,ite
! get pointers for typical meteorological fields
  ier=0
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ps',ges_ps,istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'z', ges_z, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'u', ges_u, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'v', ges_v, istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'tv',ges_tv,istatus );ier=ier+istatus
  call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'q' ,ges_q, istatus );ier=ier+istatus
  if (ier/=0) call die(trim(myname),'cannot get pointers for met-fields, ier =',ier)

!     figure out what are acceptable dimensions for global grid, based on resolution of input spectral coefs
!   need to inquire from file what is spectral truncation, then setup general spectral structure variable

  if(ntlevs_ens > 1) then
     open(10,file=trim(filelists(it)),form='formatted',err=30)
  else
     open(10,file=trim(filelists(1)),form='formatted',err=30)
  endif
  rewind (10) 
  do n=1,200
     read(10,'(a)',err=20,end=40)filename 
  enddo
40 n_ens=n-1

!    set n_ens_temp depending on if we want to add bias perturbation to the ensemble

  if(add_bias_perturbation) then
     n_ens_temp=n_ens+1
  else
     n_ens_temp=n_ens
  end if

  rewind (10) 
  read(10,'(a)',err=20,end=20)filename 
!===========
  if ((.not. use_gfs_nemsio) .and. (.not. use_gfs_ncio))then

     open(lunges,file=trim(filename),form='unformatted')
     call sigio_srhead(lunges,sighead,iret)
     close(lunges)

     hourg=sighead%fhour
     idate4=sighead%idate
     nvcoord=sighead%nvcoord

     if(mype == 0) then
        write(6,*) ' sighead%fhour,sighead%idate=',sighead%fhour,sighead%idate
        write(6,*) ' iadate(y,m,d,hr,min)=',iadate
        write(6,*) ' sighead%jcap,sighead%levs=',sighead%jcap,sighead%levs
        write(6,*) ' sighead%latf,sighead%lonf=',sighead%latf,sighead%lonf
        write(6,*) ' sighead%idvc,sighead%nvcoord=',sighead%idvc,sighead%nvcoord
        write(6,*) ' sighead%idsl=',sighead%idsl
        do k=1,sighead%levs+1
           write(6,*)' k,vcoord=',k,sighead%vcoord(k,:)
        end do
     end if

     idsl=sighead%idsl
     idvc=sighead%idvc
     nlat_gfs=sighead%latf+2
     nlon_gfs=sighead%lonf
     nsig_gfs=sighead%levs
     if(sighead%jcap > 0)then
        jcap_gfs=sighead%jcap
     else if(jcap_ens > 0)then
        jcap_gfs=jcap_ens
     else
        write(6,*)'ERROR jcap is undefined'
        call stop2(555)
     endif

     if (allocated(vcoord))     deallocate(vcoord)
     allocate(vcoord(nsig_gfs+1,3,2))
     vcoord(1:nsig_gfs+1,1:sighead%nvcoord,1)=sighead%vcoord(1:nsig_gfs+1,1:sighead%nvcoord)

! Extract header information
!     hourg    = sighead%fhour
!     idate4(1)= sighead%idate(1)
!     idate4(2)= sighead%idate(2)
!     idate4(3)= sighead%idate(3)
!     idate4(4)= sighead%idate(4)

  else if ( use_gfs_nemsio ) then

     call nemsio_init(iret=iret)
     call nemsio_open(gfile_atm,filename,'READ',iret=iret)
     idate         = i_missing
     nfhour        = i_missing; nfminute      = i_missing
     nfsecondn     = i_missing; nfsecondd     = i_missing
     idsl  = i_missing
     call nemsio_getfilehead(gfile_atm, idate=idate, gtype=filetype,  &
           modelname=mdlname, nfhour=nfhour, nfminute=nfminute,       &
           nfsecondn=nfsecondn, nfsecondd=nfsecondd,                  &
           dimx=lonb, dimy=latb,   dimz=levs, &
           jcap=jcap, idvc=idvc, &
           idsl=idsl,    iret=iret2)
     ! FV3GFS write component does not include JCAP, infer from DIMY-2
     if (jcap<0) jcap=latb-2

     if ( nfhour == i_missing .or. nfminute == i_missing .or. &
          nfsecondn == i_missing .or. nfsecondd == i_missing ) then
          write(6,*)'READ_FILES: ***ERROR*** some forecast hour info ', &
                 'are not defined in ', trim(filename)
          write(6,*)'READ_FILES: nfhour = ', &
                 hourg
          call stop2(80)
     endif

     ! FV3GFS write component does not include JCAP, infer from DIMY-2

     hourg = float(nfhour) + float(nfminute)/r60 +  &
             float(nfsecondn)/float(nfsecondd)/r3600
     idate4(1) = idate(4)  !hour
     idate4(2) = idate(2)  !month
     idate4(3) = idate(3)  !day
     idate4(4) = idate(1)  !year
     nlat_gfs=latb+2
     nlon_gfs=lonb
     nsig_gfs=levs
     if(jcap > 0)then
        jcap_gfs=jcap
     else if(jcap_ens > 0)then
        jcap_gfs=jcap_ens
     else
        write(6,*)'ERROR jcap is undefined'
        call stop2(555)
     endif

     if (allocated(vcoord))     deallocate(vcoord)
     allocate(vcoord(nsig_gfs+1,3,2))
     call nemsio_getfilehead(gfile_atm,iret=iret2,vcoord=vcoord)
     if ( iret2 /= 0 ) then
        write(6,*)' GESINFO:  ***ERROR*** problem reading header ', &
              'vcoord, Status = ',iret2
        call stop2(99)
     endif

     call nemsio_close(gfile_atm,iret=iret)
!       Determine the type of vertical coordinate used by model because that
!       nvcoord is no longer part of NEMSIO header output.
     nvcoord=3
     if(maxval(vcoord(:,3,1))==zero .and. &
        minval(vcoord(:,3,1))==zero ) then
           nvcoord=2
           if(maxval(vcoord(:,2,1))==zero .and. &
              minval(vcoord(:,2,1))==zero ) then
              nvcoord=1
           end if
     end if
     if(mype == 0) then
         write(6,*) 'fhour,idate=',hourg,idate4
         write(6,*) ' iadate(y,m,d,hr,min)=',iadate
         write(6,*) ' jcap,levs=',jcap,levs
         write(6,*) ' latf,lonf=',latb,lonb
         write(6,*) ' idvc,nvcoord=',idvc,nvcoord
         write(6,*) ' idsl=',idsl
         do k=1,levs+1
            write(6,*)' k,vcoord=',k,vcoord(k,:,1)
         end do
     end if

!  add netCDF header information: 
     else ! use_gfs_ncio and get this information
        write(sfilename,'("sfcf",i2.2)')nhr_assimilation
        ! open the netCDF file
        atmges = open_dataset(filename,paropen=.true.)
        ! get dimension sizes
        ncdim = get_dim(atmges, 'grid_xt'); gfshead%lonb = ncdim%len
        ncdim = get_dim(atmges, 'grid_yt'); gfshead%latb = ncdim%len
        ncdim = get_dim(atmges, 'pfull') ; gfshead%levs = ncdim%len
        nsig_gfs = gfshead%levs
        ! hard code jcap,idsl,idvc
        gfshead%jcap = -9999
        gfshead%idsl= 1
        gfshead%idvc = 2

        ! FV3GFS write component does not include JCAP, infer from DIMY-2
        !njcap=latb-2

        nlat_gfs=gfshead%latb+2
        nlon_gfs=gfshead%lonb
        nsig_gfs=gfshead%levs

        jcap_gfs=gfshead%latb-2

        if (mype==mype_out) write(6,*)'GESINFO:  Read NCEP FV3GFS netCDF ', &
           'format file, ',trim(filename)
        ! hard code nvcoord to be 2
        gfshead%nvcoord=2 ! ak and bk
        if (allocated(gfsheadv%vcoord)) deallocate(gfsheadv%vcoord)
        allocate(gfsheadv%vcoord(gfshead%levs+1,gfshead%nvcoord))
        call read_attribute(atmges, 'ak', aknc)
        call read_attribute(atmges, 'bk', bknc)
        do k=1,gfshead%levs+1
           kr = gfshead%levs+2-k
           gfsheadv%vcoord(k,1) = aknc(kr)
           gfsheadv%vcoord(k,2) = bknc(kr)
        end do
        deallocate(aknc,bknc)
        ! get time information
        idate6 = get_idate_from_time_units(atmges)
        gfshead%idate(1) = idate6(4)  !hour
        gfshead%idate(2) = idate6(2)  !month
        gfshead%idate(3) = idate6(3)  !day
        gfshead%idate(4) = idate6(1)  !year
        call read_vardata(atmges, 'time', fhour1) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
        gfshead%fhour = fhour1(1)

        call close_dataset(atmges)

        if(mype == 0) then
          write(6,*) ' netCDF:fhour,idate=',fhour1,idate6
          write(6,*) ' netCDF:iadate(y,m,d,hr,min)=',iadate
          write(6,*) ' netCDF: jcap,levs=',gfshead%levs
          write(6,*) ' netCDF: latb,lonb=',gfshead%latb,gfshead%lonb
          write(6,*) ' netCDF: nvcoord=',gfshead%nvcoord
          write(6,*) ' netCDF: idvc,idsl=',gfshead%idvc,gfshead%idsl
        endif

        hourg = fhour1(1)
        idate4(1) = idate6(4)
        idate4(2) = idate6(2)
        idate4(3) = idate6(3)
        idate4(4) = idate6(1)

  endif ! use_gfs_nemsio
!===========
! Compute valid time from ensemble date and forecast length and compare to iadate, the analysis time
  iyr=idate4(4)
  ihourg=hourg
  if(iyr>=0.and.iyr<=99) then
     if(iyr>51) then
        iyr=iyr+1900
     else
        iyr=iyr+2000
     end if
  end if
  fha=zero ; ida=0; jda=0
  fha(2)=ihourg    ! relative time interval in hours
  ida(1)=iyr       ! year
  ida(2)=idate4(2) ! month
  ida(3)=idate4(3) ! day
  ida(4)=0         ! time zone
  ida(5)=idate4(1) ! hour
  call w3movdat(fha,ida,jda)
  iadate_gfs(1)=jda(1) ! year
  iadate_gfs(2)=jda(2) ! mon
  iadate_gfs(3)=jda(3) ! day
  if(ntlevs_ens > 1) then
     iadate_gfs(4)=jda(5)+hrdifsig(ntguessig)-hrdifsig(it) ! hour
  else
     iadate_gfs(4)=jda(5) ! hour
  endif
  iadate_gfs(5)=0      ! minute
  if(mype == 0) then
     write(6,*)' in get_gefs_for_regional, iadate_gefs=',iadate_gfs
     write(6,*)' in get_gefs_for_regional, iadate    =',iadate
  end if
           call w3fs21(iadate,nming1)
           call w3fs21(iadate_gfs,nming2)
  if( (nming1/=nming2) .and. (.not.l_ens_in_diff_time) ) then
     if(mype == 0) write(6,*)' GEFS ENSEMBLE MEMBER DATE NOT EQUAL TO ANALYSIS DATE, PROGRAM STOPS'
!     call stop2(85)
  end if
     

!         set up ak5,bk5,ck5 for use in computing 3d pressure field (needed for vertical interp to regional)
!                            following is code segment from gesinfo.F90
  allocate(ak5(nsig_gfs+1))
  allocate(bk5(nsig_gfs+1))
  allocate(ck5(nsig_gfs+1))
  allocate(tref5(nsig_gfs))

  idvc=gfshead%idvc
  idsl=gfshead%idsl

  do k=1,nsig_gfs+1
     ak5(k)=zero
     bk5(k)=zero
     ck5(k)=zero
  end do
  if (use_gfs_ncio)then
     if (gfshead%nvcoord == 1) then
        do k=1,nsig_gfs+1
           bk5(k) = gfsheadv%vcoord(k,1)
        end do
     elseif (gfshead%nvcoord == 2) then
        do k = 1,nsig_gfs+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
        end do
     elseif (gfshead%nvcoord == 3) then
        do k = 1,nsig_gfs+1
           ak5(k) = gfsheadv%vcoord(k,1)*zero_001
           bk5(k) = gfsheadv%vcoord(k,2)
           ck5(k) = gfsheadv%vcoord(k,3)*zero_001
        end do
     else
        write(6,*)'GET_GEFS_FOR_REGIONAL netCDF:  ***ERROR*** INVALID value for nvcoord=',gfshead%nvcoord
        call stop2(85)
     endif
  else
     if (nvcoord == 1) then
        do k=1,nsig_gfs+1
           bk5(k) = vcoord(k,1,1)
        end do
     elseif (nvcoord == 2) then
        do k = 1,nsig_gfs+1
           ak5(k) = vcoord(k,1,1)*zero_001
           bk5(k) = vcoord(k,2,1)
        end do
     elseif (nvcoord == 3) then
        do k = 1,nsig_gfs+1
           ak5(k) = vcoord(k,1,1)*zero_001
           bk5(k) = vcoord(k,2,1)
           ck5(k) = vcoord(k,3,1)*zero_001
        end do
     else
        write(6,*)'READ_GFS_OZONE_FOR_REGIONAL:  ***ERROR*** INVALID value for nvcoord=',nvcoord
        call stop2(85)
     endif
  endif

! Load reference temperature array (used by general coordinate)
  do k=1,nsig_gfs
     tref5(k)=h300
  end do


  inner_vars=1
  num_fields=6*nsig_gfs+2      !  want to transfer u,v,t,q,oz,cw,ps,z from gfs subdomain to slab
                            !  later go through this code, adapting gsibundlemod, since currently 
                            !   hardwired.
  num_fieldst=min(num_fields,npe)
  allocate(vector(num_fields))
  vector=.false.
  vector(1:2*nsig_gfs)=uv_hyb_ens
  call general_sub2grid_create_info(grd_gfst,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fieldst, &
                                  .not.regional)
  call general_sub2grid_create_info(grd_gfs,inner_vars,nlat_gfs,nlon_gfs,nsig_gfs,num_fields, &
                                  .not.regional,vector)
  jcap_gfs_test=jcap_gfs
  call general_init_spec_vars(sp_gfs,jcap_gfs,jcap_gfs_test,grd_gfs%nlat,grd_gfs%nlon)

!  also want to set up regional grid structure variable grd_mix, which still has number of
!   vertical levels set to nsig_gfs, but horizontal dimensions set to regional domain.

  call general_sub2grid_create_info(grd_mix,inner_vars,grd_ens%nlat,grd_ens%nlon,nsig_gfs, &
                                    num_fields,regional,vector)

!  create interpolation information for global grid to regional ensemble grid

  nord_g2r=4
  call g_create_egrid2points_slow(grd_ens%nlat*grd_ens%nlon,region_lat_ens,region_lon_ens, &
                    grd_gfs%nlat,sp_gfs%rlats,grd_gfs%nlon,sp_gfs%rlons,nord_g2r,p_g2r)

!  allocate mix ensemble space--horizontal on regional domain, vertical still gefs 
  allocate(st_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(vp_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate( t_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(rh_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(oz_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate(cw_eg(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,n_ens))
  allocate( p_eg_nmmb(grd_mix%lat2,grd_mix%lon2,n_ens))
  st_eg=zero ; vp_eg=zero ; t_eg=zero ; rh_eg=zero ; oz_eg=zero ; cw_eg=zero 
  p_eg_nmmb=zero

!
! prepare terrain height
!
  allocate(ges_z_ens(grd_mix%lat2,grd_mix%lon2))
  if (dual_res) then
     allocate ( tmp_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
     allocate ( tmp_anl(lat2,lon2,nsig,1) )
     tmp_anl=0.0_r_kind
     tmp_anl(:,:,1,1)=ges_z(:,:)
     call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
     ges_z_ens(:,:)=tmp_ens(:,:,1,1)
     deallocate(tmp_ens)
     deallocate(tmp_anl)
  else
     ges_z_ens(:,:)=ges_z(:,:)
  endif
!                begin loop over ensemble members

  rewind(10)
  inithead=.true.
  do n=1,n_ens
     read(10,'(a)',err=20,end=20)filename 


!    allocate necessary space on global grid
     call gsi_gridcreate(atm_grid,grd_gfs%lat2,grd_gfs%lon2,grd_gfs%nsig)
     call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
     if(istatus/=0) then
       write(6,*)myname,': trouble creating atm_bundle'
       call stop2(999)
     endif

     if(use_gfs_nemsio)then
        call general_read_gfsatm_nems_rapv5(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,.true.,iret)
     else if (use_gfs_ncio) then
        call general_read_gfsatm_nc(grd_gfst,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,.true.,iret)
     else
        call general_read_gfsatm(grd_gfst,sp_gfs,sp_gfs,filename,uv_hyb_ens,.false.,.true., &
               atm_bundle,inithead,iret)
     end if
     inithead = .false.

     ier = 0
     call gsi_bundlegetpointer(atm_bundle,'vor' ,vor ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'div' ,div ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'u'   ,u   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'v'   ,v   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'tv'  ,tv  ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'q'   ,q   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'oz'  ,oz  ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'cw'  ,cwmr,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'z'   ,z   ,istatus) ; ier = ier + istatus
     call gsi_bundlegetpointer(atm_bundle,'ps'  ,ps  ,istatus) ; ier = ier + istatus
     if ( ier /= 0 ) call die(myname,': missing atm_bundle vars, aborting ...',ier)

     allocate(work_sub(grd_gfs%inner_vars,grd_gfs%lat2,grd_gfs%lon2,num_fields))
     do k=1,grd_gfs%nsig
        ku=k ; kv=k+grd_gfs%nsig ; kt=k+2*grd_gfs%nsig ; kq=k+3*grd_gfs%nsig ; koz=k+4*grd_gfs%nsig
        kcw=k+5*grd_gfs%nsig
        do j=1,grd_gfs%lon2
           do i=1,grd_gfs%lat2
              work_sub(1,i,j,ku)=u(i,j,k)
              work_sub(1,i,j,kv)=v(i,j,k)
              work_sub(1,i,j,kt)=tv(i,j,k)
              work_sub(1,i,j,kq)=q(i,j,k)
              work_sub(1,i,j,koz)=oz(i,j,k)
              work_sub(1,i,j,kcw)=cwmr(i,j,k)
           end do
        end do
     end do
     kz=num_fields ; kps=kz-1
     do j=1,grd_gfs%lon2
        do i=1,grd_gfs%lat2
           work_sub(1,i,j,kz)=z(i,j)
           work_sub(1,i,j,kps)=ps(i,j)
        end do
     end do

     call gsi_bundledestroy(atm_bundle,istatus)

     allocate(work(grd_gfs%inner_vars,grd_gfs%nlat,grd_gfs%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     call general_sub2grid(grd_gfs,work_sub,work)
     deallocate(work_sub)

!    then interpolate to regional analysis grid
     allocate(work_reg(grd_mix%inner_vars,grd_mix%nlat,grd_mix%nlon,grd_gfs%kbegin_loc:grd_gfs%kend_alloc))
     do k=grd_gfs%kbegin_loc,grd_gfs%kend_loc
        call g_egrid2points_faster(p_g2r,work(1,1,1,k),work_reg(1,1,1,k),vector(k))
     end do
     deallocate(work)

!    next general_grid2sub to go to regional grid subdomains.
     allocate(work_sub(grd_mix%inner_vars,grd_mix%lat2,grd_mix%lon2,num_fields))
     call general_grid2sub(grd_mix,work_reg,work_sub)
     deallocate(work_reg)
     allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
     kz=num_fields ; kps=kz-1
!    compute 3d pressure on interfaces
     kap1=rd_over_cp+one
     kapr=one/rd_over_cp
     pri=zero
     k=1
     k2=grd_mix%nsig+1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pri(i,j,k)=work_sub(1,i,j,kps)
           pri(i,j,k2)=zero
        end do
     end do
     if (idvc /= 3) then
        do k=2,grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 pri(i,j,k)=ak5(k)+bk5(k)*work_sub(1,i,j,kps)
              end do
           end do
        end do
     else
        do k=2,grd_mix%nsig
           kt=k+2*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 trk=(half*(work_sub(1,i,j,kt-1)+work_sub(1,i,j,kt))/tref5(k))**kapr
                 pri(i,j,k)=ak5(k)+(bk5(k)*work_sub(1,i,j,kps))+(ck5(k)*trk)
              end do
           end do
        end do
     end if

!    Get 3d pressure field now on interfaces
     allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     if (idsl5/=2) then
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                           (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
              end do
           end do
        end do
     else
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              do k=1,grd_mix%nsig
                 prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
              end do
           end do
        end do
     end if
!  !Compute geopotential height at interface between layers
     allocate(zbarl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     allocate(height(grd_mix%nsig))
     rdog=rd/grav
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           k  = 1
           kt=k+2*grd_mix%nsig
           h  = rdog * work_sub(1,i,j,kt)
           dz = h * log(pri(i,j,k)/prsl(i,j,k))
           height(k) = work_sub(1,i,j,kz)+dz

           do k=2,grd_mix%nsig
              kt=k+2*grd_mix%nsig
              h  = rdog * half * (work_sub(1,i,j,kt-1)+work_sub(1,i,j,kt))
              dz = h * log(prsl(i,j,k-1)/prsl(i,j,k))
              height(k) = height(k-1) + dz
           end do
           do k=1,grd_mix%nsig
              zbarl(i,j,k)=height(k)
           end do
        end do
     end do
     deallocate(pri,height)
!! recompute pbar using routine Wan-Shu obtained from Matt Pyle:

     allocate(tt(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     allocate(psfc_out(grd_mix%lat2,grd_mix%lon2))
     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              tt(i,j,k)=work_sub(1,i,j,kt)
           end do
        end do
     end do
     mm1=mype+1
     ilook=-1 ; jlook=-1
     allocate(prsl1000(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
     prsl1000=1000._r_kind*prsl
     call compute_nmm_surfacep ( ges_z(:,:), zbarl,prsl1000, &
                                 psfc_out,grd_mix%nsig,grd_mix%lat2,grd_mix%lon2, &
                                 ilook,jlook)
     deallocate(tt,zbarl,prsl1000)
     psfc_out=.001_r_kind*psfc_out

! If not using Q perturbations, convert to RH
     if (.not.q_hyb_ens) then
        allocate(tsen(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
        allocate(qs(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
!    Compute RH and potential virtual temp
!    First step is go get sensible temperature and 3d pressure
        do k=1,grd_mix%nsig
           kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 tsen(i,j,k)= work_sub(1,i,j,kt)/(one+fv*max(zero,work_sub(1,i,j,kq)))
              end do
           end do 
        end do

        ice=.true.
        iderivative=0
        call genqsat(qs,tsen,prsl,grd_mix%lat2,grd_mix%lon2,grd_mix%nsig,ice,iderivative)

        do k=1,grd_mix%nsig
           kt=k+2*grd_mix%nsig ; kq=k+3*grd_mix%nsig
           do j=1,grd_mix%lon2
              do i=1,grd_mix%lat2
                 if(enpert4arw) then
                    work_sub(1,i,j,kq) = work_sub(1,i,j,kq)
                 else
                    work_sub(1,i,j,kq) = work_sub(1,i,j,kq)/qs(i,j,k)
                 endif
              end do
           end do
        end do
        deallocate(qs,tsen)
     end if
     do k=1,grd_mix%nsig
        kt=k+2*grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              if(enpert4arw) then
                 work_sub(1,i,j,kt)=work_sub(1,i,j,kt)/(one+fv*max(zero,work_sub(1,i,j,kq))) &
                                                      /(0.01_r_kind*prsl(i,j,k))**rd_over_cp
              else
                 work_sub(1,i,j,kt)=work_sub(1,i,j,kt)/(0.01_r_kind*prsl(i,j,k))**rd_over_cp
              endif
           end do
        end do
     end do

     deallocate(prsl)

     iimax=0
     iimin=grd_mix%nlat
     jjmax=0
     jjmin=grd_mix%nlon
     ratio_x=(nlon-one)/(grd_mix%nlon-one)
     ratio_y=(nlat-one)/(grd_mix%nlat-one)
     do k=1,grd_mix%nsig
        ku=k ; kv=ku+grd_mix%nsig ; kt=kv+grd_mix%nsig ; kq=kt+grd_mix%nsig ; koz=kq+grd_mix%nsig
        kcw=koz+grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2

              ii=i+grd_mix%istart(mm1)-2
              jj=j+grd_mix%jstart(mm1)-2
              ii=min(grd_mix%nlat,max(1,ii))
              jj=min(grd_mix%nlon,max(1,jj))
              iimax=max(ii,iimax)
              iimin=min(ii,iimin)
              jjmax=max(jj,jjmax)
              jjmin=min(jj,jjmin)
              dlon_ens=float(jj)
              dlat_ens=float(ii)
              dlon=one+(dlon_ens-one)*ratio_x
              dlat=one+(dlat_ens-one)*ratio_y
              
              call rotate_wind_ll2xy(work_sub(1,i,j,ku),work_sub(1,i,j,kv), &
                                     uob,vob,region_lon_ens(ii,jj),dlon,dlat)
              st_eg(i,j,k,n)=uob
              vp_eg(i,j,k,n)=vob

               t_eg(i,j,k,n)=work_sub(1,i,j,kt)     !  now pot virtual temp
              rh_eg(i,j,k,n)=work_sub(1,i,j,kq)     !  now rh
              oz_eg(i,j,k,n)=work_sub(1,i,j,koz)
              cw_eg(i,j,k,n)=work_sub(1,i,j,kcw)
           end do
        end do
     end do
     kz=num_fields ; kps=kz-1
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg_nmmb(i,j,n)=psfc_out(i,j)
        end do
     end do
     deallocate(work_sub,psfc_out)

  end do   !  end loop over ensemble members.
! 
  deallocate(ges_z_ens)

!   next, compute mean of ensembles.

  allocate(stbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(vpbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate( tbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(rhbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(ozbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(cwbar(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  allocate(pbar_nmmb(grd_mix%lat2,grd_mix%lon2))

!   compute mean state
  stbar=zero ; vpbar=zero ; tbar=zero ; rhbar=zero ; ozbar=zero ; cwbar=zero 
  pbar_nmmb=zero
  do n=1,n_ens
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              stbar(i,j,k)=stbar(i,j,k)+st_eg(i,j,k,n)
              vpbar(i,j,k)=vpbar(i,j,k)+vp_eg(i,j,k,n)
               tbar(i,j,k)= tbar(i,j,k)+ t_eg(i,j,k,n)
              rhbar(i,j,k)=rhbar(i,j,k)+rh_eg(i,j,k,n)
              ozbar(i,j,k)=ozbar(i,j,k)+oz_eg(i,j,k,n)
              cwbar(i,j,k)=cwbar(i,j,k)+cw_eg(i,j,k,n)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           pbar_nmmb(i,j)=pbar_nmmb(i,j)+p_eg_nmmb(i,j,n)
        end do
     end do
  end do

! Convert to mean
  bar_norm = one/float(n_ens)
  do k=1,grd_mix%nsig
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           stbar(i,j,k)=stbar(i,j,k)*bar_norm
           vpbar(i,j,k)=vpbar(i,j,k)*bar_norm
            tbar(i,j,k)= tbar(i,j,k)*bar_norm
           rhbar(i,j,k)=rhbar(i,j,k)*bar_norm
           ozbar(i,j,k)=ozbar(i,j,k)*bar_norm
           cwbar(i,j,k)=cwbar(i,j,k)*bar_norm
        end do
     end do
  end do
  do j=1,grd_mix%lon2
     do i=1,grd_mix%lat2
        pbar_nmmb(i,j)=pbar_nmmb(i,j)*bar_norm
!   also save pbar to module array ps_bar for possible use in vertical localization
!                                                    in terms of scale heights/normalized p/p
        ps_bar(i,j,1)=pbar_nmmb(i,j)
     end do
  end do

! Subtract mean from ensemble members, but save scaling by sqrt(1/(nens-1)) until after vertical interpolation
  n1=1
!www  ensemble perturbation for all but the first member if full_ensemble
  if(full_ensemble)n1=2

  do n=n1,n_ens
     do k=1,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              st_eg(i,j,k,n)=st_eg(i,j,k,n)-stbar(i,j,k)
              vp_eg(i,j,k,n)=vp_eg(i,j,k,n)-vpbar(i,j,k)
               t_eg(i,j,k,n)= t_eg(i,j,k,n)- tbar(i,j,k)
              rh_eg(i,j,k,n)=rh_eg(i,j,k,n)-rhbar(i,j,k)
              oz_eg(i,j,k,n)=oz_eg(i,j,k,n)-ozbar(i,j,k)
              cw_eg(i,j,k,n)=cw_eg(i,j,k,n)-cwbar(i,j,k)
           end do
        end do
     end do
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           p_eg_nmmb(i,j,n)=p_eg_nmmb(i,j,n)-pbar_nmmb(i,j)
        end do
     end do
  end do
  deallocate(stbar,vpbar,rhbar,ozbar,cwbar)

! now obtain mean pressure prsl
! compute 3d pressure on interfaces
  kap1=rd_over_cp+one
  kapr=one/rd_over_cp
  allocate(pri(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig+1))
  pri=zero
  k=1
  k2=grd_mix%nsig+1
  do j=1,grd_mix%lon2
     do i=1,grd_mix%lat2
        pri(i,j,k)=pbar_nmmb(i,j)
        pri(i,j,k2)=zero
     end do
  end do
  if (idvc /= 3) then
     do k=2,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              pri(i,j,k)=ak5(k)+bk5(k)*pbar_nmmb(i,j)
           end do
        end do
     end do
  else
     do k=2,grd_mix%nsig
        do j=1,grd_mix%lon2
           do i=1,grd_mix%lat2
              trk=(half*(tbar(i,j,k-1)+tbar(i,j,k))/tref5(k))**kapr
              pri(i,j,k)=ak5(k)+(bk5(k)*pbar_nmmb(i,j))+(ck5(k)*trk)
           end do
        end do
     end do
  end if

! Get 3d pressure field now at layer midpoints
  allocate(prsl(grd_mix%lat2,grd_mix%lon2,grd_mix%nsig))
  if (idsl/=2) then
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           do k=1,grd_mix%nsig
              prsl(i,j,k)=((pri(i,j,k)**kap1-pri(i,j,k+1)**kap1)/&
                        (kap1*(pri(i,j,k)-pri(i,j,k+1))))**kapr
           end do
        end do
     end do
  else
     do j=1,grd_mix%lon2
        do i=1,grd_mix%lat2
           do k=1,grd_mix%nsig
              prsl(i,j,k)=(pri(i,j,k)+pri(i,j,k+1))*half
           end do
        end do
     end do
  end if
  deallocate(pri,pbar_nmmb,tbar)
  deallocate(ak5,bk5,ck5,tref5)

! interpolate/extrapolate in vertical using yoshi's spline code.

!  first need ges_prsl_e, the 3d pressure on the ensemble grid.

  allocate(ges_prsl_e(grd_ens%inner_vars,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  if(dual_res) then
     call general_suba2sube(grd_a1,grd_e1,p_e2a,ges_prsl(:,1,1,it),ges_prsl_e(1,:,1,1),regional) ! x?
  else
     ges_prsl_e(1,:,:,:)=ges_prsl(:,:,:,it)
  end if

  allocate(xspli(grd_mix%nsig),yspli(grd_mix%nsig),xsplo(grd_ens%nsig),ysplo(grd_ens%nsig))

  allocate(ut(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(vt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(tt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(rht(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(ozt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(cwt(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))

  allocate(w3(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig))
  allocate(w2(grd_ens%lat2,grd_ens%lon2))
  allocate(en_perts(n_ens,grd_ens%lat2,grd_ens%lon2,nc2d+nc3d*grd_ens%nsig))

  do n=1,n_ens
     do j=1,grd_ens%lon2
        do i=1,grd_ens%lat2
           do k=1,grd_mix%nsig
              xspli(k)=log(prsl(i,j,k)*10.0_r_kind)
           end do
           do k=1,grd_ens%nsig
              xsplo(k)=log(ges_prsl_e(1,i,j,k)*10._r_kind)
           end do

!    u
           do k=1,grd_mix%nsig
              yspli(k)=st_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ut(i,j,k)=ysplo(k)
           end do
!    v
           do k=1,grd_mix%nsig
              yspli(k)=vp_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              vt(i,j,k)=ysplo(k)
           end do
!    t
           do k=1,grd_mix%nsig
              yspli(k)=t_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ysplo(k)=ysplo(k)*(0.01_r_kind*ges_prsl_e(1,i,j,k))**rd_over_cp  ! converting from pot Tv to Tv
              tt(i,j,k)=ysplo(k)
           end do
!    rh
           do k=1,grd_mix%nsig
              yspli(k)=rh_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              rht(i,j,k)=ysplo(k)
           end do
!       oz
           do k=1,grd_mix%nsig
              yspli(k)=oz_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              ozt(i,j,k)=ysplo(k)
           end do
!    cw
           do k=1,grd_mix%nsig
              yspli(k)=cw_eg(i,j,k,n)
           end do
           call intp_spl(xspli,yspli,xsplo,ysplo,grd_mix%nsig,grd_ens%nsig)
!               following is to correct for bug in intp_spl
           do k=1,grd_ens%nsig
              if(xsplo(k) < xspli(grd_mix%nsig)) ysplo(k)=yspli(grd_mix%nsig)
              if(xsplo(k) > xspli(1)) ysplo(k)=yspli(1)
           end do
           do k=1,grd_ens%nsig
              cwt(i,j,k)=ysplo(k)
           end do

        end do
     end do

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
     if(n==1 .and. full_ensemble)then

        allocate(qs(lat2,lon2,nsig))
        ice=.true.
        iderivative=0
        do k=1,nsig
           do j=1,lon2
              do i=1,lat2
                 qs(i,j,k)=ges_q(i,j,k)
              end do
           end do
        end do
        call genqsat(qs,ges_tsen(:,:,:,it),ges_prsl(:,:,:,it),lat2,lon2,nsig,ice,iderivative)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!! The first member is full perturbation based on regional first guess !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! put fist guess in ensemble grid & Subtract guess from 1st ensemble member (ensemble mean)

        if (dual_res) then
           allocate ( tmp_ens(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
           allocate ( tmp_ens2(grd_ens%lat2,grd_ens%lon2,grd_ens%nsig,1) )
           allocate ( tmp_anl(lat2,lon2,nsig,1) )

           if (.not.q_hyb_ens) then
              tmp_anl(:,:,:,1)=qs(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
              tmp_anl(:,:,:,1)=ges_q(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens2,regional)
              rht(:,:,:) = rht(:,:,:)-tmp_ens2(:,:,:,1)/tmp_ens(:,:,:,1)
           else
              tmp_anl(:,:,:,1)=ges_q(:,:,:)
              call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens2,regional)
              rht(:,:,:) = rht(:,:,:)-tmp_ens2(:,:,:,1)
           end if

           tmp_anl(:,:,:,1)=ges_u(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           ut(:,:,:) = ut(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,:,1)=ges_v(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           vt(:,:,:) = vt(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,:,1)=ges_tv(:,:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           tt(:,:,:) = tt(:,:,:)-tmp_ens(:,:,:,1)
           tmp_anl(:,:,1,1)=ges_ps(:,:)
           call general_suba2sube(grd_a1,grd_e1,p_e2a,tmp_anl,tmp_ens,regional)
           p_eg_nmmb(:,:,n) = p_eg_nmmb(:,:,n)-tmp_ens(:,:,1,1)
           deallocate(tmp_anl,tmp_ens,tmp_ens2)
        else
           do k=1,grd_ens%nsig
              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    ut(i,j,k) = ut(i,j,k)-ges_u(i,j,k)
                    vt(i,j,k) = vt(i,j,k)-ges_v(i,j,k)
                    tt(i,j,k) = tt(i,j,k)-ges_tv(i,j,k)
                 end do
              end do
           end do

           if (.not.q_hyb_ens) then
              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       rht(i,j,k) = rht(i,j,k)-ges_q(i,j,k)/qs(i,j,k)
                    end do
                 end do
              end do
           else
              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       rht(i,j,k) = rht(i,j,k)-ges_q(i,j,k)
                    end do
                 end do
              end do
           end if

           do j=1,grd_ens%lon2
              do i=1,grd_ens%lat2
                 p_eg_nmmb(i,j,n) = p_eg_nmmb(i,j,n)-ges_ps(i,j)
              end do
           end do
        endif
        deallocate(qs)

     endif   ! n==1 .and. full_ensemble

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww

!   transfer from temporary arrays to perturbation arrays and normalize by sig_norm

! sig_norm from the following
! 2*J_b = x^T * (beta1*B + beta2*P_ens)^(-1) * x
! where  P_ens is the ensemble covariance which is the sum of outer products of the
! ensemble perturbations (unnormalized) divided by n_ens-1  (or n_ens, depending on who you read).
     sig_norm=sqrt(one/max(one,n_ens_temp-one))

     do ic3=1,nc3d

        select case (trim(cvars3d(ic3)))

           case('sf','SF')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = ut(i,j,k)*sig_norm
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

           case('vp','VP')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = vt(i,j,k)*sig_norm
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

           case('t','T')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = tt(i,j,k)*sig_norm
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

           case('q','Q')

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                       w3(i,j,k) = rht(i,j,k)*sig_norm
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

           case('oz','OZ')
!          temporarily ignore ozone perturbations

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                   !   w3(i,j,k) = ozt(i,j,k)*sig_norm
                       w3(i,j,k) = zero
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

           case('cw','CW')
!          temporarily ignore cloud water perturbations

              do k=1,grd_ens%nsig
                 do j=1,grd_ens%lon2
                    do i=1,grd_ens%lat2
                   !   w3(i,j,k) = cwt(i,j,k)*sig_norm
                       w3(i,j,k) = zero
                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
                    end do
                 end do
              end do

        end select
     end do
     do ic2=1,nc2d

        select case (trim(cvars2d(ic2)))

           case('ps','PS')

              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    w2(i,j) = p_eg_nmmb(i,j,n)*sig_norm
                    en_perts(n,i,j,nc3d*grd_ens%nsig+ic2)=w2(i,j)
                 end do
              end do

           case('sst','SST')

! dtk: temporarily ignore sst perturbations in hybrid
              do j=1,grd_ens%lon2
                 do i=1,grd_ens%lat2
                    w2(i,j) = zero
                    en_perts(n,i,j,nc3d*grd_ens%nsig+ic2)=w2(i,j)
                 end do
              end do

        end select
     end do
  end do

  call general_sub2grid_destroy_info(grd_gfs)
  call general_sub2grid_destroy_info(grd_mix)
  call general_sub2grid_destroy_info(grd_gfst)
!
!
! CALCULATE ENSEMBLE SPREAD Here

  call general_destroy_spec_vars(sp_gfs)
  deallocate(vector)
  deallocate(st_eg,vp_eg,t_eg,rh_eg)
  deallocate(oz_eg,cw_eg,p_eg_nmmb)
  deallocate(ges_prsl_e)
  deallocate(xspli,yspli,xsplo,ysplo)
  deallocate(prsl)
  deallocate(ut,vt,tt,rht,ozt,cwt)

  enddo ! it=1,ntlevs_ens

  iunit=20
  if(wrt_pert_sub) then  ! write perturbations in subdomain
     write(filename,'(a,I4.4)') 'saved_en_perts.pe',mype
     if(mype==0) write(*,*) 'save en_perts as ', trim(filename)
     open(iunit,file=trim(filename),form='unformatted')
     do n=1,n_ens
!
        write(iunit) n
        write(iunit) ps_bar(:,:,1)
!
        do ic3=1,nc3d

           do k=1,grd_ens%nsig
             w3(:,:,k)=en_perts(n,:,:,(ic3-1)*grd_ens%nsig+k)
           enddo
           write(iunit) cvars3d(ic3)
           write(iunit) w3

        end do
        do ic2=1,nc2d

           w2(:,:)=en_perts(n,:,:,nc3d*grd_ens%nsig+ic2)
           write(iunit) cvars2d(ic2)
           write(iunit) w2
        end do

     end do
     close(iunit)
     deallocate(w3,w2)
  endif

  if(wrt_pert_mem) then
     inner_vars=1
     num_fields=nc3d*grd_ens%nsig+nc2d
     allocate(vector(num_fields))
     vector=.false.

     if(mype==0) write(*,*) 'final==',inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields,regional
     call general_sub2grid_create_info(grd_arw,inner_vars,  &
                    grd_ens%nlat,grd_ens%nlon,grd_ens%nsig, &
                               num_fields,regional,vector)
     allocate(work_sub(inner_vars,grd_arw%lat2,grd_arw%lon2,grd_arw%num_fields))
     allocate(work(inner_vars,grd_arw%nlat,grd_arw%nlon,grd_arw%kbegin_loc:grd_arw%kend_alloc))
     do n = 1,n_ens
        do k = 1,num_fields ; do j = 1,grd_arw%lon2 ; do i = 1,grd_arw%lat2
           work_sub(1,i,j,k) =    en_perts(n,i,j,k)
        enddo ; enddo ; enddo

        call general_sub2grid(grd_arw,work_sub,work)

        write(charmem,'("_mem",i3.3)') n
        filenameout="enspreproc_arw" // trim(adjustl(charmem)) 

        call mpi_file_open(mpi_comm_world,trim(adjustl(filenameout)), &
                         mpi_mode_wronly+mpi_mode_create, &
                         mpi_info_null,lunit,ierror)
        if ( ierror /= 0 ) then
           write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_OPEN failed on task =', &   
                                      mype ,' ierror = ',ierror,' aborting!'
           goto 999
        endif

        disp = grd_arw%nlat * grd_arw%nlon * (grd_arw%kbegin_loc-1) * r_kind

        call mpi_file_set_view(lunit,disp,mpi_rtype,mpi_rtype,'native',mpi_info_null,ierror)        
        if ( ierror /= 0 ) then
           write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_SET_VIEW failed on task = ',&
                  mype ,' ierror = ',ierror,' aborting!'
           goto 999
        endif

        count = grd_arw%nlat * grd_arw%nlon * grd_arw%nlevs_alloc

        call mpi_file_write(lunit,work,count,mpi_rtype,istatus,ierror)          
        if ( ierror /= 0 ) then
           write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_WRITE failed on task =', &
                          mype ,' ierror = ',ierror,' aborting!'
           goto 999
        endif

        call mpi_file_close(lunit,ierror)
        if ( ierror /= 0 ) then
           write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_CLOSE failed on task =', &
                          mype ,' ierror = ',ierror,' aborting!'
           goto 999
        endif

     enddo ! do i_ens = 1,n_ens

     deallocate(work_sub,work,vector)
  endif

  if(enpert4arw) then
     inner_vars=1
     num_fields=1
     allocate(vector(num_fields))
     vector=.false.

     if(wordy_write_logging .and. mype==0) then
        write(*,*) 'final==',inner_vars,grd_ens%nlat,grd_ens%nlon, &
                   grd_ens%nsig,num_fields,regional
     endif

     call general_sub2grid_create_info(grd_arw,inner_vars,  &
                    grd_ens%nlat,grd_ens%nlon,grd_ens%nsig, &
                               num_fields,regional,vector)

     allocate(z1(grd_arw%inner_vars*grd_arw%nlat*grd_arw%nlon))

     sig_norm=1.0_r_kind/sig_norm

     ! nproc = 1 more than highest rank to use for writing:
     nproc=npe

     ! Figure out how many files my rank will write:
     my_write_count=0
     count_local_workh: do n=1,n_ens
        writing_rank=max(min((n*nproc)/n_ens,nproc-1),0)
        if(writing_rank==mype) then
           my_write_count = my_write_count+1
        endif
     end do count_local_workh

     init_work_arrays: if(my_write_count<1) then
        ! We never write, but we need to allocate and initialize
        ! arrays so that debuggers will not flag an invalid array
        ! being sent as an argument.
        allocate(workh(1,1,1,1))
        allocate(pert_unit(1))
        workh=0
     else
        ! Allocate enough 3D workh slices and units to fit all of our writes:
        allocate(workh(grd_arw%inner_vars,grd_arw%nlat, &
                       grd_arw%nlon,my_write_count))
        allocate(pert_unit(my_write_count))
     endif init_work_arrays
     pert_unit=-1
     
194  format("rank ",I0,": member ",I0," file ",A,": ",A)
199  format("rank ",I0,": member ",I0," level ",I0," file ",A,": ",A)

     ! Open output files on appropriate ranks and write headers to each file:
     iworkh=1
     init_output_files: do n=1,n_ens
        writing_rank=max(min((n*nproc)/n_ens,nproc-1),0)
        iwrite= ( mype==writing_rank )
        init_one_output_file: if(iwrite) then
           write(filename,'(a,I4.4)') 'en_perts4arw.mem',n

           if(wordy_write_logging) then
              write(*,194) &
                   mype, n, trim(filename), 'open file and write header'
              write(*,*) nc3d,nc2d,cvars3d,cvars2d
              write(*,*) grd_arw%nlat,grd_arw%nlon,grd_arw%nsig
           endif

           open(newunit=iunit, &
                file=trim(filename),form='unformatted')
           pert_unit(iworkh)=iunit
           write(iunit) nc3d,nc2d,cvars3d,cvars2d
           write(iunit) grd_arw%nlat,grd_arw%nlon,grd_arw%nsig

           iworkh=iworkh+1
        endif init_one_output_file
     end do init_output_files

     sigma_loop: do k=1,nc3d*grd_ens%nsig+nc2d
        ! Gather data for level k to each processor that will write it.
        iworkh=1
        gather_data: do n=1,n_ens
           writing_rank=max(min((n*nproc)/n_ens,nproc-1),0)
           iwrite= ( mype==writing_rank )
           
           if(iwrite) then
              write(filename,'(a,I4.4)') 'en_perts4arw.mem',n
              if(wordy_write_logging) then
                 write(*,199) &
                      mype, n, k, trim(filename), 'gather perturbations'
              endif
           end if

           ! Copy to 1D z1 array for communication.
           !$OMP PARALLEL DO PRIVATE(i,j,ii)
           fill_z1_j: do j=1,lon2
              fill_z1_i: do i=1,lat2
                 ii= 1 + (j-1)*lat2 + (i-1)
                 z1(ii)=en_perts(n,i,j,k)*sig_norm
              end do fill_z1_i
           end do fill_z1_j

           ! change Ps from CB to Pa
           if(k==nc3d*grd_ens%nsig+1) then
              !$OMP PARALLEL DO PRIVATE(i,j,ii)
              scale_z1_j: do j=1,lon2
                 scale_z1_i: do i=1,lat2
                    ii= 1 + (j-1)*lat2 + (i-1)
                    z1(ii)=z1(ii)*1000.0
                 enddo scale_z1_i
              enddo scale_z1_j
           end if

           ! Collect z1 to writing rank's workh array:
           call general_gather2grid(grd_arw,z1,workh(:,:,:,iworkh),writing_rank)
           
           if(iwrite) then
              ! Go to next index of writing array.
              iworkh=iworkh+1
           endif
        end do gather_data

        ! Write data for level k on each process that should write:
        iworkh=1
        write_data: do n=1,n_ens
           writing_rank=max(min((n*nproc)/n_ens,nproc-1),0)
           iwrite= ( mype==writing_rank )

           write_one_file: if(iwrite) then
              write(filename,'(a,I4.4)') 'en_perts4arw.mem',n
              if(wordy_write_logging) then
                 write(*,199) &
                      mype, n, k, trim(filename), 'write perturbations'
                 write(*,*) nc3d,nc2d,cvars3d,cvars2d
                 write(*,*) grd_arw%nlat,grd_arw%nlon,grd_arw%nsig
                 write(*,*) k,maxval(workh),minval(workh)
              endif

              iunit=pert_unit(iworkh)
              write(iunit) workh(:,:,:,iworkh)
           endif write_one_file
        end do write_data
     end do sigma_loop

     ! Close all files
     iworkh=1
     close_output_files: do n=1,n_ens
        writing_rank=max(min((n*nproc)/n_ens,nproc-1),0)
        iwrite= ( mype==writing_rank )
        if(iwrite) then
           if(wordy_write_logging) then
              write(*,194) &
                   mype, n, trim(filename), 'close perturbation file'
           endif
           iunit=pert_unit(iworkh)
           close(iunit)
           pert_unit(iworkh)=-1
           iworkh=iworkh+1
        endif
     enddo close_output_files

     deallocate(z1,workh,vector,pert_unit)
  endif

  deallocate(en_perts)

  return

30 write(6,*) 'GET_GEFS+FOR_REGIONAL open filelist failed '
   call stop2(555)
20 write(6,*) 'GET_GEFS+FOR_REGIONAL read gfs ens failed ',n
   call stop2(555)
999 write(6,*) 'GET_GEFS+FOR_REGIONAL create full field failed',n
   call stop2(666)
end subroutine get_gefs_for_regional_enspro

subroutine general_read_gfsatm_nems_rapv5(grd,sp_a,filename,uvflag,vordivflag,zflag, &
           gfs_bundle,init_head,iret_read)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_read_gfsatm  adaptation of read_gfsatm for general resolutions
!   prgmmr: parrish          org: np22                date: 1990-10-10
!
! abstract: copied from read_gfsatm, primarily for reading in gefs sigma files, where the
!            input resolution and the grid that variables are reconstructed on can be
!            different from the analysis grid/resolution.
!
! program history log:
!   2010-02-25  parrish
!   2010-03-29  kleist     - modifications to allow for st/vp perturbations instead of u,v
!   2012-01-17  wu         - increase character length for variable "filename"
!   2014-11-30  todling    - genelize interface to handle bundle instead of fields;
!                            internal code should be generalized
!   2014-12-03  derber     - introduce vordivflag, zflag and optimize routines
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     sp_b     - structure variable containing spectral information for input
!                     fields
!                    (initialized by general_init_spec_vars, located in general_specmod.f90)
!     filename - input sigma file name
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!     init_head- flag to read header record.  Usually .true. unless repeatedly
!                reading similar files (ensembles)
!
!   output argument list:
!     gfs_bundle  - bundle carrying guess fields
!     iret_read - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
   use kinds, only: r_kind,r_single,i_kind
   use mpimod, only: mype
   use general_sub2grid_mod, only: sub2grid_info
   use general_specmod, only: spec_vars
   use mpimod, only: npe
   use constants, only: zero,one,fv,r0_01
   use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
   use ncepnems_io, only: error_msg
   use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
   use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
   use general_commvars_mod, only: fill2_ns,filluv2_ns
   use constants, only: two,pi,half,deg2rad,r60,r3600
   use gsi_bundlemod, only: gsi_bundle
   use gsi_bundlemod, only: gsi_bundlegetpointer
   use control_vectors, only: imp_physics

   implicit none

   ! Declare local parameters
   real(r_kind),parameter:: r0_001 = 0.001_r_kind

   ! Declare passed variables
   type(sub2grid_info)                   ,intent(in   ) :: grd
   type(spec_vars)                       ,intent(in   ) :: sp_a
   character(*)                          ,intent(in   ) :: filename
   logical                               ,intent(in   ) :: uvflag,zflag,vordivflag,init_head
   integer(i_kind)                       ,intent(  out) :: iret_read
   type(gsi_bundle)                      ,intent(inout) :: gfs_bundle

   real(r_kind),pointer,dimension(:,:)       :: ptr2d
   real(r_kind),pointer,dimension(:,:,:)     :: ptr3d
   real(r_kind),pointer,dimension(:,:)       :: g_ps
   real(r_kind),pointer,dimension(:,:,:)     :: g_vor,g_div,&
                                                g_cwmr,g_q,g_oz,g_tv

   real(r_kind),allocatable,dimension(:,:)   :: g_z
   real(r_kind),allocatable,dimension(:,:,:) :: g_u,g_v

   ! Declare local variables
   character(len=120) :: my_name = 'GENERAL_READ_GFSATM_NEMS'
   character(len=1)   :: null = ' '
   integer(i_kind):: iret,nlatm2,nlevs,icm,nord_int
   integer(i_kind):: i,j,k,icount,kk
   integer(i_kind) :: ier,istatus,iredundant
   integer(i_kind) :: latb, lonb, levs, nframe
   integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
   integer(i_kind) :: istop = 101
   integer(i_kind),dimension(npe)::ilev,iflag,mype_use
   integer(i_kind),dimension(7):: idate
   integer(i_kind),dimension(4):: odate
   real(r_kind) :: fhour

   real(r_kind),allocatable,dimension(:):: spec_div,spec_vor
   real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
        grid_vor, grid_div, grid_b, grid_b2
   real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
   real(r_kind),allocatable,dimension(:)   :: work, work_v
   real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1
   real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
   real(4),allocatable,dimension(:) :: r4lats,r4lons

   logical :: procuse,diff_res,eqspace
   type(nemsio_gfile) :: gfile
   type(egrid2agrid_parm) :: p_high
   logical,dimension(1) :: vector

   !******************************************************************************
   ! Initialize variables used below
   iret_read=0
   iret=0
   nlatm2=grd%nlat-2
   iflag = 0
   ilev = 0

   nlevs=grd%nsig
   mype_use=-1
   icount=0
   procuse=.false.
   if ( mype == 0 ) procuse = .true.
   do i=1,npe
      if ( grd%recvcounts_s(i-1) > 0 ) then
         icount = icount+1
         mype_use(icount)=i-1
         if ( i-1 == mype ) procuse=.true.
      endif
   enddo
   icm=icount
   allocate( work(grd%itotsub),work_v(grd%itotsub) )
   work=zero
   work_v=zero

   if ( procuse ) then

      if ( init_head)call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)

      if (  nframe /= 0 ) then
         if ( mype == 0 ) &
            write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      endif

      fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year

      diff_res=.false.
      if ( latb /= nlatm2 ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
         !call stop2(101)
      endif
      if ( lonb /= grd%nlon ) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
         !call stop2(101)
      endif
      if ( levs /= grd%nsig ) then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      endif

      allocate( spec_vor(sp_a%nc), spec_div(sp_a%nc) )
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if ( diff_res ) then
         allocate(grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate(grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      endif
      allocate(rwork1d0(latb*lonb))
      allocate(rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
         rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      enddo
      do j=1,lonb
         rlons(j)=deg2rad*r4lons(j)
      enddo
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      enddo

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace=eqspace)
      deallocate(rlats,rlons)

   endif ! if ( procuse )

   ! Get pointer to relevant variables (this should be made flexible and general)
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'sf',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'div',g_div ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both sf and div'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'vp',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'vor',g_vor ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both vp and vor'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   iredundant=0
   call gsi_bundlegetpointer(gfs_bundle,'t' ,g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   call gsi_bundlegetpointer(gfs_bundle,'tv',g_tv  ,ier)
   if ( ier == 0 ) iredundant = iredundant + 1
   if ( iredundant==2 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'cannot handle having both t and tv'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   istatus=0
   call gsi_bundlegetpointer(gfs_bundle,'ps',g_ps  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'q' ,g_q   ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'oz',g_oz  ,ier);istatus=istatus+ier
   call gsi_bundlegetpointer(gfs_bundle,'cw',g_cwmr,ier);istatus=istatus+ier
   if ( istatus /= 0 ) then
      if ( mype == 0 ) then
         write(6,*) 'general_read_gfsatm_nems: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
   endif
   allocate(g_u(grd%lat2,grd%lon2,grd%nsig),g_v(grd%lat2,grd%lon2,grd%nsig))
   allocate(g_z(grd%lat2,grd%lon2))

   icount=0

   !   Process guess fields according to type of input file.   NCEP_SIGIO files
   !   are spectral coefficient files and need to be transformed to the grid.
   !   Once on the grid, fields need to be scattered from the full domain to
   !   sub-domains.

   !  Only read Terrain when zflag is true.
   if ( zflag ) then

      icount=icount+1
      iflag(icount)=1
      ilev(icount)=1

      ! Terrain:  spectral --> grid transform, scatter to all mpi tasks
      if (mype==mype_use(icount)) then
         ! read hs
         call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','read',istop+2,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif
   endif

   icount=icount+1
   iflag(icount)=2
   ilev(icount)=1

   ! Surface pressure:  same procedure as terrain
   if (mype==mype_use(icount)) then
      ! read ps
      call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop+3,iret)
      rwork1d1 = r0_001*rwork1d0 ! convert Pa to cb
      if ( diff_res ) then
         vector(1)=.false.
         grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
         call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
         call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
         do kk=1,grd%itotsub
            i=grd%ltosi_s(kk)
            j=grd%ltosj_s(kk)
            work(kk)=grid2(i,j,1)
         enddo
      else
         grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/))
         call general_fill_ns(grd,grid,work)
      endif
   endif
   if ( icount == icm ) then
      call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vordivflag)
   endif

   !   Thermodynamic variable:  s-->g transform, communicate to all tasks
   !   For multilevel fields, each task handles a given level.  Periodic
   !   mpi_alltoallv calls communicate the grids to all mpi tasks.
   !   Finally, the grids are loaded into guess arrays used later in the
   !   code.

   do k=1,nlevs

      icount=icount+1
      iflag(icount)=3
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! read T/Tv/etc.
         call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop+7,iret)
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d1,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+7,iret)
         rwork1d0=rwork1d0*(one+fv*rwork1d1)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      if ( vordivflag .or. .not. uvflag ) then

         icount=icount+1
         iflag(icount)=4
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Vorticity
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_vor(grd%nlon,nlatm2))
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_vor,work)
            deallocate(grid_vor)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=5
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! Divergence
            ! Convert grid u,v to div and vor
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work_v(kk)=grid2(i,j,1)
               enddo
               do j=1,grd%nlon
                  do i=2,grd%nlat-1
                     grid_v(j,grd%nlat-i)=grid2(i,j,1)
                  enddo
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
            allocate( grid_div(grd%nlon,nlatm2) )
            call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
            call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
            ! Load values into rows for south and north pole
            call general_fill_ns(grd,grid_div,work)
            deallocate(grid_div)
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( vordivflag .or. .not. uvflag )

      if ( uvflag ) then

         icount=icount+1
         iflag(icount)=6
         ilev(icount)=k

         if (mype==mype_use(icount)) then

            ! U
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work,work_v)
            endif
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

         icount=icount+1
         iflag(icount)=7
         ilev(icount)=k

         if (mype==mype_use(icount)) then
            ! V
            call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
            call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
            if ( diff_res ) then
               grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
               grid_b2=reshape(rwork1d1,(/size(grid_b2,1),size(grid_b2,2)/))
               vector(1)=.true.
               call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
               call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
               do kk=1,grd%itotsub
                  i=grd%ltosi_s(kk)
                  j=grd%ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
               enddo
            else
               grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
               grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
               ! Note work_v and work are switched because output must be in work.
               call general_filluv_ns(grd,slons,clons,grid,grid_v,work_v,work)
            endif
         endif
         if ( icount == icm ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

      endif ! if ( uvflag )

      icount=icount+1
      iflag(icount)=8
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Specific humidity
         call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+6,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=9
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Ozone mixing ratio
         call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','read',istop+8,iret)
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif
      endif
      if ( icount == icm ) then
         call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
              icount,iflag,ilev,work,uvflag,vordivflag)
      endif

      icount=icount+1
      iflag(icount)=10
      ilev(icount)=k

      if (mype==mype_use(icount)) then
         ! Cloud condensate mixing ratio.
         work=zero
         call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
         if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
         if (imp_physics == 11) then
            call nemsio_readrecv(gfile,'icmr','mid layer',k,rwork1d1,iret=iret)
            if (iret /= 0) then
               call error_msg(trim(my_name),trim(filename),'icmr','read',istop+10,iret)
            else
               rwork1d0 = rwork1d0 + rwork1d1
            endif
         endif
         if ( diff_res ) then
            grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
            vector(1)=.false.
            call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
            call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
            do kk=1,grd%itotsub
               i=grd%ltosi_s(kk)
               j=grd%ltosj_s(kk)
               work(kk)=grid2(i,j,1)
            enddo
         else
            grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
            call general_fill_ns(grd,grid,work)
         endif

            endif

         if ( icount == icm .or. k == nlevs ) then
            call general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
                 icount,iflag,ilev,work,uvflag,vordivflag)
         endif

   enddo ! do k=1,nlevs

   if ( procuse ) then
      if ( diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
      call destroy_egrid2agrid(p_high)
      deallocate(spec_div,spec_vor)
      deallocate(rwork1d1,clons,slons)
      deallocate(rwork1d0)
      deallocate(grid,grid_v)
      call nemsio_close(gfile,iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop+9,iret)
   endif
   deallocate(work)

   ! Convert dry temperature to virtual temperature
   !do k=1,grd%nsig
   !   do j=1,grd%lon2
   !      do i=1,grd%lat2
   !         g_tv(i,j,k) = g_tv(i,j,k)*(one+fv*g_q(i,j,k))
   !      enddo
   !   enddo
   !enddo

   ! Load u->div and v->vor slot when uv are used instead
   if ( uvflag ) then
      call gsi_bundlegetpointer(gfs_bundle,'u' ,ptr3d,ier)
      if ( ier == 0 ) then
         ptr3d=g_u
         call gsi_bundlegetpointer(gfs_bundle,'v' ,ptr3d,ier)
         if ( ier == 0 ) ptr3d=g_v
      else ! in this case, overload: return u/v in sf/vp slot
         call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
         if ( ier == 0 ) then
            ptr3d=g_u
            call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
            if ( ier == 0 ) ptr3d=g_v
         endif
      endif
   else ! in this case, overload: return u/v in sf/vp slot
      call gsi_bundlegetpointer(gfs_bundle,'sf' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_u
      call gsi_bundlegetpointer(gfs_bundle,'vp' ,ptr3d,ier)
      if ( ier == 0 ) ptr3d=g_v
   endif
   if (zflag) then
      call gsi_bundlegetpointer(gfs_bundle,'z' ,ptr2d,ier)
      if ( ier == 0 ) ptr2d=g_z
   endif

   ! Clean up
   deallocate(g_z)
   deallocate(g_u,g_v)

   ! Print date/time stamp
   if ( mype == 0 ) then
      write(6,700) lonb,latb,nlevs,grd%nlon,nlatm2,&
            fhour,odate,trim(filename)
700   format('GENERAL_READ_GFSATM_NEMS: read lonb,latb,levs=',&
            3i6,', scatter nlon,nlat=',2i6,', hour=',f6.1,', idate=',4i5,1x,a)
   endif

   return

end subroutine general_read_gfsatm_nems_rapv5

subroutine general_reload(grd,g_z,g_ps,g_tv,g_vor,g_div,g_u,g_v,g_q,g_oz,g_cwmr, &
           icount,iflag,ilev,work,uvflag,vdflag)

! !USES:

  use kinds, only: r_kind,i_kind
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype
  use general_sub2grid_mod, only: sub2grid_info
  implicit none

! !INPUT PARAMETERS:

  type(sub2grid_info),                intent(in   ) :: grd
  integer(i_kind),                    intent(inout) :: icount
  integer(i_kind),dimension(npe),     intent(inout) :: ilev,iflag
  real(r_kind),dimension(grd%itotsub),intent(in   ) :: work
  logical,                            intent(in   ) :: uvflag,vdflag

! !OUTPUT PARAMETERS:

  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(  out) :: g_ps
  real(r_kind),dimension(grd%lat2,grd%lon2),         intent(inout) :: g_z
  real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
       g_vor,g_div,g_cwmr,g_q,g_oz,g_tv


! !DESCRIPTION: Transfer contents of 2-d array global to 3-d subdomain array
!
! !REVISION HISTORY:
!   2004-05-14  treadon
!   2004-07-15  todling, protex-compliant prologue
!   2014-12-03  derber     - introduce vdflag and optimize routines
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!   treadon          org: np23                date: 2004-05-14
!
!EOP
!-------------------------------------------------------------------------

   integer(i_kind) i,j,k,ij,klev
   real(r_kind),dimension(grd%lat2*grd%lon2,npe):: sub

   call mpi_alltoallv(work,grd%sendcounts_s,grd%sdispls_s,mpi_rtype,&
        sub,grd%recvcounts_s,grd%rdispls_s,mpi_rtype,&
        mpi_comm_world,ierror)

!$omp parallel do  schedule(dynamic,1) private(k,i,j,ij,klev)
   do k=1,icount
      if ( iflag(k) == 1 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_z(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 2 ) then
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_ps(i,j)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 3 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_tv(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 4 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_vor(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_u(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 5 ) then
         klev=ilev(k)
         if ( vdflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_div(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
         if ( .not. uvflag ) then
           ij=0
           do j=1,grd%lon2
              do i=1,grd%lat2
                 ij=ij+1
                 g_v(i,j,klev)=sub(ij,k)
              enddo
           enddo
         endif
      elseif ( iflag(k) == 6 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  u '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_u(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 7 ) then
         if ( .not. uvflag) then
           write(6,*) 'error in general_reload  v '
         endif
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_v(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 8 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_q(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 9 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_oz(i,j,klev)=sub(ij,k)
            enddo
         enddo
      elseif ( iflag(k) == 10 ) then
         klev=ilev(k)
         ij=0
         do j=1,grd%lon2
            do i=1,grd%lat2
               ij=ij+1
               g_cwmr(i,j,klev)=sub(ij,k)
            enddo
         enddo
      endif
   enddo ! do k=1,icount

   icount=0
   ilev=0
   iflag=0

   return

end subroutine general_reload

