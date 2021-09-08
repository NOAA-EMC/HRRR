
subroutine setupozlay(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave,init_pass)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupozlay --- Compute rhs of oi for sbuv ozone obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   1998-04-10  weiyu yang
!   1999-03-01  wu, ozone processing moved into setuprhs from setupoz
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  kleist - modify to use pressure as vertical coordinate
!   2004-05-28  kleist - subroutine call update
!   2004-06-17  treadon - update documentation
!   2004-07-08  todling - added only's; removed gridmod; bug fix in diag
!   2004-07-15  todling - protex-compliant prologue; added intent's
!   2004-10-06  parrish - increase size of stats_oz for nonlinear qc,
!                         add nonlin qc penalty calc and obs count                 
!   2004-11-22  derber - remove weight, add logical for boundary point
!   2004-12-22  treadon - add outer loop number to name of diagnostic file
!   2005-03-02  dee - reorganize diagnostic file writes so that
!                         concatenated files are self-contained
!   2005-03-09  parrish - nonlinear qc change to account for inflated obs error
!   2005-03-16  derber  - change call to sproz to save observation time
!   2005-04-11  treadon - add logical to toggle on/off nonlinear qc code
!   2005-05-18  wu - add use of OMI total ozone data
!   2005-09-22  derber - modify extensively - combine with sproz - no change
!   2005-09-28  derber  - combine with prep,spr,remove tran and clean up
!   2005-10-07  treadon - fix bug in increment of ii
!   2005-10-14  derber  - input grid location and fix regional lat/lon
!   2006-01-09  treadon - remove unused variables
!   2006-02-03  derber  - modify for new obs control
!   2006-02-17  treadon - correct bug when processing data not assimilated
!   2006-03-21  treadon - add option to perturb observation
!   2006-06-06  su - move to wgtlim to constants module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-09      su  - remove option to perturb observation
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - include rozcon with interpolation weights
!   2007-06-08  kleist/treadon - add prefix (task id or path) to diag_ozone_file
!   2007-06-05  tremolet - add observation diagnostics structure
!   2008-05-23  safford - add subprogram doc block, rm unused uses and vars
!   2008-01-20  todling - add obsdiag info to diag files
!   2009-01-08  todling - re-implemented obsdiag/tail
!   2009-10-19  guo     - changed for multi-pass setup with dtime_check() and new
!                         arguments init_pass and last_pass.
!   2009-12-08  guo     - cleaned diag output rewind with open(position='rewind')
!   2011-12-07  todling - bug fix: need luse check when saving obssens
!   2012-09-10  wargan/guo - add hooks for omieff"
!   2013-01-26  parrish - change from grdcrd to grdcrd1, tintrp2a to tintrp2a1, intrp2a to intrp2a1,
!                           intrp3oz to intrp3oz1. (to allow successful debug compile on WCOSS)
!   2013-09-10  guo        - patched to take reference pressure from the observation
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-11-29  shlyaeva - save linearized H(x) for EnKF.
!   2016-12-09  mccarty - add netcdf_diag capability
!   2017-10-27  todling - revised netcdf output for lay case; obs-sens needs attention
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!     nreal          - number of pieces of non-ozone info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of ozone obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use mpeu_util, only: die,perr,getindex
  use kinds, only: r_kind,r_single,i_kind

  use state_vectors, only: svars3d, levels, nsdim

  use constants, only : zero,half,one,two,tiny_r_kind
  use constants, only : rozcon,cg_term,wgtlim,h300,r10

  use m_obsdiags, only : ozhead
  use obsmod, only : i_oz_ob_type,dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave,lobsdiag_forenkf
  use m_obsNode, only: obsNode
  use m_ozNode, only : ozNode, ozNode_typecast
  use m_obsLList, only : obsLList_appendNode
  use m_obsLList, only : obsLList_tailNode
  use obsmod, only : nloz_omi
  use obsmod, only : obs_diag,luse_obsdiag

  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ij,nsig

  use guess_grids, only : nfldsig,ges_prsi,ntguessig,hrdifsig

  use ozinfo, only : jpch_oz,error_oz,pob_oz,gross_oz,nusis_oz
  use ozinfo, only : iuse_oz,b_oz,pg_oz

  use jfunc, only : jiter,last,miter,jiterstart
  use sparsearr, only: sparr2, new, size, writearray, fullarray
  
  use m_dtime, only: dtime_setup, dtime_check, dtime_show
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of ozone obs
  logical                          , intent(in   ) :: ozone_diagsave   ! switch on diagnostic output (.false.=no output)
  logical                          , intent(in   ) :: init_pass        ! state of "setup" processing

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz), intent(inout) :: stats_oz ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  character(len=*),parameter:: myname="setupozlay"

! Declare external calls for code analysis
  external:: intrp2a1
  external:: tintrp2a1
  external:: intrp3oz1
  external:: grdcrd1
  external:: stop2

! Declare local variables  
  
  real(r_kind) omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) psi,errorinv
  real(r_kind),dimension(nlevs):: ozges,varinv3,ozone_inv,ozobs
  real(r_kind),dimension(nlevs):: ratio_errors,error
  real(r_kind),dimension(nlevs-1):: ozp
  real(r_kind),dimension(nloz_omi) :: ozp_omi
  real(r_kind),dimension(nlevs):: pobs,gross,tnoise
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig+1)::prsitmp
  real(r_single),dimension(nlevs):: pob4,grs4,err4
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
  real(r_kind),dimension(nloz_omi):: apriori, efficiency,pob_oz_omi
  real(r_kind),dimension(nloz_omi+1):: ozges1
  real(r_kind),dimension(miter) :: obsdiag_iuse

  real(r_kind),dimension(nsig,nlevs) :: doz_dz
  real(r_kind),dimension(nsig,nloz_omi+1):: doz_dz1
  integer(i_kind) :: oz_ind, nind, nnz
  type(sparr2) :: dhx_dx
  real(r_single), dimension(nsdim) :: dhx_dx_array

  integer(i_kind) i,nlev,ii,jj,iextra,istat,ibin, kk
  integer(i_kind) k,j,nz,jc,idia,irdim1,istatus,ioff0
  integer(i_kind) ioff,itoss,ikeep,ierror_toq,ierror_poq
  integer(i_kind) isolz,ifovn,itoqf
  integer(i_kind) mm1,itime,ilat,ilon,ilate,ilone,itoq,ipoq
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  integer(i_kind),dimension(nlevs):: ipos,iouse,ikeepk

  real(r_kind),dimension(4):: tempwij
  integer(i_kind) nlevp,nlayers
  
  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical:: ozdiagexist
  logical,dimension(nobs):: luse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical:: l_may_be_passive, proceed

  logical:: in_curbin, in_anybin, save_jacobian
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(ozNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_oz

  save_jacobian = ozone_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0

  mm1=mype+1


!
!*********************************************************************************
! Initialize arrays
  do j=1,nlevs
     ipos(j)=0
     iouse(j)=-2
     tnoise(j)=1.e10_r_kind
     gross(j)=1.e10_r_kind
     pobs(j)=1.e10_r_kind
  end do

! Locate data for satellite in ozinfo arrays
  itoss =1
  l_may_be_passive=.false.
  jc=0
  do j=1,jpch_oz
     if (isis == nusis_oz(j)) then
        jc=jc+1
        if (jc > nlevs) then
           write(6,*)'SETUPOZLAY:  ***ERROR*** in level numbers, jc,nlevs=',jc,nlevs,&
                ' ***STOP IN SETUPOZLAY***'
           call stop2(71)
        endif
        ipos(jc)=j

        iouse(jc)=iuse_oz(j)
        tnoise(jc)=error_oz(j)
        gross(jc)=min(r10*gross_oz(j),h300)
        if (obstype == 'sbuv2' .or. obstype == 'ompsnp') then
           pobs(jc)=pob_oz(j) * 1.01325_r_kind
        else
           pobs(jc)=pob_oz(j)
        endif

        if (iouse(jc)<-1 .or. (iouse(jc)==-1 .and. &
             .not.ozone_diagsave)) then
           tnoise(jc)=1.e10_r_kind
           gross(jc) =1.e10_r_kind
        endif
        if (iouse(jc)>-1) l_may_be_passive=.true.
        if (tnoise(jc)<1.e4_r_kind) itoss=0
     endif
  end do
  nlev=jc

! Handle error conditions
  if (nlevs>nlev) write(6,*)'SETUPOZLAY:  level number reduced for ',obstype,' ', &
       nlevs,' --> ',nlev
  if(nlev == 0 .or. itoss == 1)then
     if (nlev == 0 .and. mype == 0) then
        write(6,*)'SETUPOZLAY:  no levels found for ',isis
     endif
     if (itoss==1 .and. mype == 0) then
        if (mype==0) write(6,*)'SETUPOZLAY:  all obs variances > 1.e4.  Do not use ',&
             'data from satellite ',isis
     endif
     if (nobs>0) read(lunin)

!    Release memory of local guess arrays
     call final_vars_

     return
  endif
  if(ozone_diagsave)then
     irdim1=7
     ioff0=irdim1
     if(lobsdiagsave) irdim1=irdim1+4*miter+1
     if (save_jacobian) then
       nnz   = nsig                   ! number of non-zero elements in dH(x)/dx profile
       nind   = 1
       call new(dhx_dx, nnz, nind)
       irdim1 = irdim1 + size(dhx_dx)
     endif

     allocate(rdiagbuf(irdim1,nlevs,nobs))
     if(netcdf_diag) call init_netcdf_diag_
  end if


! Read and transform ozone data
  read(lunin) data,luse,ioid

!    index information for data array (see reading routine)
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)
  itoq=7      ! index of total ozone error flag (sbuv2 only)
  ipoq=8      ! index of profile ozone error flag (sbuv2 only)
  isolz=8     ! index of solar zenith angle   (gome and omi only)
  itoqf=9     ! index of row anomaly           (omi only)
  ifovn=14    ! index of scan position (gome and omi only)


! If requested, save data for diagnostic ouput
  if(ozone_diagsave)ii=0

! Convert observation (lat,lon) from earth to grid relative values
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     if(in_curbin) then
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        dtime=data(itime,i)
 
        if (obstype == 'sbuv2' .or. obstype == 'ompsnp') then
           if (nobskeep>0) then
!             write(6,*)'setupozlay: nobskeep',nobskeep
              call stop2(259)
           end if
 
           ierror_toq = nint(data(itoq,i))
           ierror_poq = nint(data(ipoq,i))

!          Note:  ozp as log(pobs)
           call intrp2a1(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
             dlon,nsig+1,mype)
  
!          Map observation pressure to guess vertical coordinate
           psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
           do nz=1,nlevs-1
              if ((pobs(nz)*psi) < one) then
                 ozp(nz) = pobs(nz)/r10
              else
                 ozp(nz) = prsitmp(1)
              end if
              call grdcrd1(ozp(nz),prsitmp,nsig+1,-1)
           enddo
        end if
       
        if (obstype == 'omieff' .or. obstype == 'tomseff') then
           pob_oz_omi(nloz_omi) = 1000.0_r_kind* 1.01325_r_kind
           do j=nloz_omi-1, 1, -1
              pob_oz_omi(j) = pob_oz_omi(j+1)/2.0
           enddo
           call intrp2a1(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
                dlon,nsig+1,mype)
        
!          Map observation pressure to guess vertical coordinate
           psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
           do nz=1,nloz_omi - 1
              if ((pob_oz_omi(nz)*psi) < one) then
                 ozp_omi(nz) = pob_oz_omi(nz)/r10
              else
                 ozp_omi(nz) = prsitmp(1)
              end if
              call grdcrd1(ozp_omi(nz),prsitmp,nsig+1,-1)
           enddo
           ozp_omi(nloz_omi) = prsitmp(1)
           call grdcrd1(ozp_omi(nloz_omi),prsitmp,nsig+1,-1)
        end if
        
        if (obstype /= 'omieff' .and. obstype /= 'tomseff') then
           call intrp3oz1(ges_oz,ozges,dlat,dlon,ozp,dtime,&
                nlevs,mype,doz_dz)
        endif

        

        if(ozone_diagsave .and. luse(i))then
           ii=ii+1
           idiagbuf(1,ii)=mype                  ! mpi task number
           diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
           diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
           diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
        endif

!       Interpolate interface pressure to obs location
!       Calculate innovations, perform gross checks, and accumualte
!       numbers for statistics

!       For OMI/GOME, nlev=1 
        do k=1,nlev
           j=ipos(k)
           if (obstype == 'omieff' .or. obstype == 'tomseff' ) then
              ioff=ifovn+1 !
           else
              ioff=nreal+k ! SBUV and OMI w/o efficiency factors
           endif

!          Compute innovation and load obs error into local array
            ! KW  OMI and TOMS have averaging kernels
           if (obstype == 'omieff' .or. obstype == 'tomseff' ) then
              ! everything in data is from top to bottom
              nlayers = nloz_omi + 1
              apriori(1:nloz_omi) = data(ioff:ioff+nloz_omi -1, i)
              ioff = ioff + nloz_omi
              efficiency(1:nloz_omi) = data(ioff:ioff+nloz_omi -1, i)
              ! Compute ozges
              call intrp3oz1(ges_oz,ozges1,dlat,dlon,ozp_omi,dtime,&
                nlayers,mype,doz_dz1)
              ozges(k) = zero
              doz_dz(:,k) = zero
              do kk = 1, nloz_omi
                 ozges(k) = ozges(k) + apriori(kk) + efficiency(kk)*(ozges1(kk)-apriori(kk))
                 doz_dz(:,k) = doz_dz(:,k) + efficiency(kk)*doz_dz1(:,kk)
              end do
              ioff = 37_i_kind
              ozobs(k) = data(ioff,i)
           else ! Applying averaging kernels for OMI
              apriori(1:nloz_omi) = -99.99 ! this will identify non-OMIEFF data for intoz
              ozobs(k) = data(ioff,i)
           endif

           ozone_inv(k) = ozobs(k)-ozges(k)
           error(k)     = tnoise(k)

!          Set inverse obs error squared and ratio_errors
           if (error(k)<1.e4_r_kind) then
              varinv3(k) = one/(error(k)**2)
              ratio_errors(k) = one
           else
              varinv3(k) = zero
              ratio_errors(k) = zero
           endif

!          Perform gross check
           if(abs(ozone_inv(k)) > gross(k) .or. ozobs(k) > 1000._r_kind .or. &
                ozges(k)<tiny_r_kind) then
              varinv3(k)=zero
              ratio_errors(k)=zero
!             write(6,*)'SETUPOZ:  reset O3 varinv3=',varinv3(k)
              if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
           endif

!          Accumulate numbers for statistics
           rat_err2 = ratio_errors(k)**2
           if (varinv3(k)>tiny_r_kind .or. &
                (iouse(k)==-1 .and. ozone_diagsave)) then
              if(luse(i))then
                 omg=ozone_inv(k)
                 stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
                 stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
                 stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
                 stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3(k)*rat_err2  ! penalty
                 stats_oz(6,j) = stats_oz(6,j) + ozobs(k)                     ! obs

                 exp_arg = -half*varinv3(k)*omg**2
                 errorinv = sqrt(varinv3(k))
                 if (pg_oz(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
                    arg  = exp(exp_arg)
                    wnotgross= one-pg_oz(j)
                    cg_oz=b_oz(j)*errorinv
                    wgross = cg_term*pg_oz(j)/(cg_oz*wnotgross)
                    term = log((arg+wgross)/(one+wgross))
                    wgt  = one-wgross/(arg+wgross)
                 else
                    term = exp_arg
                    wgt  = one
                 endif
                 stats_oz(8,j) = stats_oz(8,j) -two*rat_err2*term
                 if(wgt < wgtlim) stats_oz(9,j)=stats_oz(9,j)+one
              end if
           endif

!          If not assimilating this observation, reset inverse variance to zero
           if (iouse(k)<1) then
              varinv3(k)=zero
              ratio_errors(k)=zero
              rat_err2 = zero
           end if
           if (rat_err2*varinv3(k)>tiny_r_kind .and. luse(i)) &
              stats_oz(7,j) = stats_oz(7,j) + one

!          Optionally save data for diagnostics
           if (ozone_diagsave .and. luse(i)) then
              rdiagbuf(1,k,ii) = ozobs(k)
              rdiagbuf(2,k,ii) = ozone_inv(k)           ! obs-ges
              errorinv = sqrt(varinv3(k)*rat_err2)
              rdiagbuf(3,k,ii) = errorinv               ! inverse observation error
              if (obstype == 'gome' .or. obstype == 'omieff'  .or. &
                  obstype == 'omi'  .or. obstype == 'tomseff' .or. &
                  obstype == 'ompstc8') then
                 rdiagbuf(4,k,ii) = data(isolz,i)       ! solar zenith angle
                 rdiagbuf(5,k,ii) = data(ifovn,i)       ! field of view number
              else
                 rdiagbuf(4,k,ii) = rmiss                
                 rdiagbuf(5,k,ii) = rmiss               
              endif
              if (obstype == 'omieff' .or. obstype == 'omi' ) then
                 rdiagbuf(6,k,ii) = data(itoqf,i)       ! row anomaly index
              else
                 rdiagbuf(6,k,ii) = rmiss                
              endif
              rdiagbuf(7,k,ii) = 1.e+10_r_single          ! spread (filled in by EnKF)

              idia = ioff0
              if (save_jacobian) then
                 oz_ind = getindex(svars3d, 'oz')
                 if (oz_ind < 0) then
                    print *, 'Error: no variable oz in state vector. Exiting.'
                    call stop2(1300)
                 endif

                 dhx_dx%st_ind(1)  = sum(levels(1:oz_ind-1)) + 1
                 dhx_dx%end_ind(1) = sum(levels(1:oz_ind-1)) + nsig
                 dhx_dx%val = doz_dz(:,k)

                 call writearray(dhx_dx, rdiagbuf(idia+1:irdim1,k,ii))

                 idia = idia+size(dhx_dx)
              endif

              if (netcdf_diag) then
                 call nc_diag_metadata("MPI_Task_Number", mype                      )
                 call nc_diag_metadata("Latitude",        sngl(data(ilate,i))       )
                 call nc_diag_metadata("Longitude",       sngl(data(ilone,i))       )
                 call nc_diag_metadata("Time",            sngl(data(itime,i)-time_offset) )
                 call nc_diag_metadata("Reference_Pressure",     sngl(pobs(k))      )
                 call nc_diag_metadata("Analysis_Use_Flag",      iouse(k)           )
                 call nc_diag_metadata("Observation",                  sngl(ozobs(k)))
                 call nc_diag_metadata("Inverse_Observation_Error",    sngl(errorinv))
                 call nc_diag_metadata("Obs_Minus_Forecast_adjusted",  sngl(ozone_inv(k)))
                 call nc_diag_metadata("Obs_Minus_Forecast_unadjusted",sngl(ozone_inv(k)))
                 if (obstype == 'gome' .or. obstype == 'omieff'  .or. &
                     obstype == 'omi'  .or. obstype == 'tomseff' ) then
                    call nc_diag_metadata("Solar_Zenith_Angle", sngl(data(isolz,i)) )
                    call nc_diag_metadata("Scan_Position",      sngl(data(ifovn,i)) )
                 else
                    call nc_diag_metadata("Solar_Zenith_Angle",        sngl(rmiss) )
                    call nc_diag_metadata("Scan_Position",             sngl(rmiss) )
                 endif
                 if (obstype == 'omieff' .or. obstype == 'omi' ) then
                    call nc_diag_metadata("Row_Anomaly_Index", sngl(data(itoqf,i))  )
                 else
                    call nc_diag_metadata("Row_Anomaly_Index",         sngl(rmiss)  )
                 endif
                 if (save_jacobian) then
                    call fullarray(dhx_dx, dhx_dx_array)
                    call nc_diag_data2d("Observation_Operator_Jacobian", dhx_dx_array)
                 endif
              endif
           endif

        end do
!       Check all information for obs.  If there is at least one piece of
!       information that passed quality control, use this observation.
        ikeepk=0
        do k=1,nlevs
           if ((ratio_errors(k)**2)*varinv3(k)>1.e-10_r_kind) ikeepk(k)=1
        end do
        ikeep=maxval(ikeepk)
     endif ! (in_curbin)

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        IF (ibin<1.OR.ibin>nobs_bins) write(6,*)'SETUPOZLAY: ',mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if(in_curbin) then
!          Process obs have at least one piece of information that passed qc checks
           if (.not. last .and. ikeep==1) then
 
              allocate(my_head)
              m_alloc(ibin) = m_alloc(ibin) +1
              my_node => my_head        ! this is a workaround
              call obsLList_appendNode(ozhead(ibin),my_node)
              my_node => null()

              my_head%idv = is
              my_head%iob = ioid(i)
              my_head%elat= data(ilate,i)
              my_head%elon= data(ilone,i)

              nlevp=max(nlev-1,1)
              if (obstype == 'omieff' .or. obstype == 'tomseff' ) nlevp = nloz_omi
              allocate(my_head%res(nlev), &
                       my_head%err2(nlev), &
                       my_head%raterr2(nlev), &
                       my_head%prs(nlevp), &
                       my_head%wij(4,nsig), &
                       my_head%dprsi(nsig), &
                       my_head%ipos(nlev),  &
                       my_head%apriori(nloz_omi), &
                       my_head%efficiency(nloz_omi), stat=istatus)
              if (istatus/=0) write(6,*)'SETUPOZLAY:  allocate error for oz_point, istatus=',istatus
              if(luse_obsdiag)allocate(my_head%diags(nlev))

!             Set number of levels for this obs
              my_head%nloz = nlev-1  ! NOTE: for OMI/GOME, nloz=0

!             Set (i,j) indices of guess gridpoint that bound obs location
              call get_ij(mm1,dlat,dlon,my_head%ij,tempwij)

              call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime,hrdifsig,&
                   nsig+1,mype,nfldsig)

              my_head%rozcon = rozcon
              do k = 1,nsig
                 my_head%dprsi(k) = prsitmp(k)-prsitmp(k+1)
                 my_head%wij(1,k)=tempwij(1)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 my_head%wij(2,k)=tempwij(2)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 my_head%wij(3,k)=tempwij(3)*rozcon*(prsitmp(k)-prsitmp(k+1))
                 my_head%wij(4,k)=tempwij(4)*rozcon*(prsitmp(k)-prsitmp(k+1))
              end do

!             Increment data counter and save information used in
!             inner loop minimization (int* and stp* routines)
 
              my_head%luse=luse(i)
              my_head%time=dtime

              if (obstype == 'sbuv2'.or. obstype == 'ompsnp' ) then
                 do k=1,nlevs-1
                    my_head%prs(k) = ozp(k)
                 enddo
              else if (obstype == 'omieff' .or. obstype == 'tomseff') then
                 do k=1,nloz_omi
                    my_head%prs(k) = ozp_omi(k)
                 enddo
              else ! GOME or OMI w/o efficiency factors

                 my_head%prs(1) = zero   ! any value is OK, never used
              endif  
              
              my_head => null()
           endif ! < .not.last >
        endif ! (in_curbin)

!       Link obs to diagnostics structure
           do k=1,nlevs
              if (luse_obsdiag) then
                 if (.not.lobsdiag_allocated) then
                    if (.not.associated(obsdiags(i_oz_ob_type,ibin)%head)) then
                       obsdiags(i_oz_ob_type,ibin)%n_alloc = 0
                       allocate(obsdiags(i_oz_ob_type,ibin)%head,stat=istat)
                       if (istat/=0) then
                          write(6,*)'setupozlay: failure to allocate obsdiags',istat
                          call stop2(260)
                       end if
                       obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
                    else
                       allocate(obsdiags(i_oz_ob_type,ibin)%tail%next,stat=istat)
                       if (istat/=0) then
                          write(6,*)'setupozlay: failure to allocate obsdiags',istat
                          call stop2(261)
                       end if
                       obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
                    end if
                    obsdiags(i_oz_ob_type,ibin)%n_alloc = obsdiags(i_oz_ob_type,ibin)%n_alloc +1
       
                    allocate(obsdiags(i_oz_ob_type,ibin)%tail%muse(miter+1))
                    allocate(obsdiags(i_oz_ob_type,ibin)%tail%nldepart(miter+1))
                    allocate(obsdiags(i_oz_ob_type,ibin)%tail%tldepart(miter))
                    allocate(obsdiags(i_oz_ob_type,ibin)%tail%obssen(miter))
                    obsdiags(i_oz_ob_type,ibin)%tail%indxglb=ioid(i)
                    obsdiags(i_oz_ob_type,ibin)%tail%nchnperobs=-99999
                    obsdiags(i_oz_ob_type,ibin)%tail%luse=luse(i)
                    obsdiags(i_oz_ob_type,ibin)%tail%muse(:)=.false.
                    obsdiags(i_oz_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
                    obsdiags(i_oz_ob_type,ibin)%tail%tldepart(:)=zero
                    obsdiags(i_oz_ob_type,ibin)%tail%wgtjo=-huge(zero)
                    obsdiags(i_oz_ob_type,ibin)%tail%obssen(:)=zero
       
                    n_alloc(ibin) = n_alloc(ibin) +1
                    my_diag => obsdiags(i_oz_ob_type,ibin)%tail
                    my_diag%idv = is
                    my_diag%iob = ioid(i)
                    my_diag%ich = k
                    my_diag%elat= data(ilate,i)
                    my_diag%elon= data(ilone,i)
                 else
                    if (.not.associated(obsdiags(i_oz_ob_type,ibin)%tail)) then
                       obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%head
                    else
                       obsdiags(i_oz_ob_type,ibin)%tail => obsdiags(i_oz_ob_type,ibin)%tail%next
                    end if
                    if (.not.associated(obsdiags(i_oz_ob_type,ibin)%tail)) then
                       call die(myname,'.not.associated(obsdiags(i_oz_ob_type,ibin)%tail)')
                    endif
                    if (obsdiags(i_oz_ob_type,ibin)%tail%indxglb/=ioid(i)) then
                       write(6,*)'setupozlay: index error'
                       call stop2(262)
                    end if
                 endif
              endif

           if(in_curbin) then
              if (luse_obsdiag) then
                 obsdiags(i_oz_ob_type,ibin)%tail%muse(jiter)= (ikeepk(k)==1)
                 obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jiter)=ozone_inv(k)
                 obsdiags(i_oz_ob_type,ibin)%tail%wgtjo= varinv3(k)*ratio_errors(k)**2
              endif
 
              if (.not. last .and. ikeep==1) then
                 !my_head => ozNode_typecast(obsLList_tailNode(ozhead(ibin)))
                 my_node => obsLList_tailNode(ozhead(ibin))
                 if(.not.associated(my_node)) &
                    call die(myname,'unexpected, associated(my_node) =',associated(my_node))
                 my_head => ozNode_typecast(my_node)
                 if(.not.associated(my_head)) &
                    call die(myname,'unexpected, associated(my_head) =',associated(my_head))
                 my_node => my_head

                 my_head%ipos(k)    = ipos(k)
                 my_head%res(k)     = ozone_inv(k)
                 my_head%err2(k)    = varinv3(k)
                 my_head%raterr2(k) = ratio_errors(k)**2
                 my_head%apriori(1:nloz_omi) = apriori(1:nloz_omi)
                 my_head%efficiency(1:nloz_omi) = efficiency(1:nloz_omi)

                 if (luse_obsdiag) then
                    my_head%diags(k)%ptr => obsdiags(i_oz_ob_type,ibin)%tail

                    my_diag => my_head%diags(k)%ptr
                    if(my_head%idv /= my_diag%idv .or. &
                       my_head%iob /= my_diag%iob .or. &
                                 k /= my_diag%ich ) then
                       call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                                (/is,ioid(i),k,ibin/))
                       call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,k/))
                       call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
                       call die(myname)
                    endif
                 endif

                 my_head => null()
              endif

              if (ozone_diagsave.and.lobsdiagsave.and.luse(i)) then
                 idia=6
                 do jj=1,miter
                    idia=idia+1
                    if (obsdiags(i_oz_ob_type,ibin)%tail%muse(jj)) then
                       rdiagbuf(idia,k,ii) = one
                       obsdiag_iuse(jj)    = one
                    else
                       rdiagbuf(idia,k,ii) = -one
                       obsdiag_iuse(jj)    = -one
                    endif
                 enddo
                 do jj=1,miter+1
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%nldepart(jj)
                 enddo
                 do jj=1,miter
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%tldepart(jj)
                 enddo
                 do jj=1,miter
                    idia=idia+1
                    rdiagbuf(idia,k,ii) = obsdiags(i_oz_ob_type,ibin)%tail%obssen(jj)
                 enddo
                 if (netcdf_diag) then
!                   TBD: Sensitivities must be written out in coordination w/ rest of obs
!                   call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                              )
!                   call nc_diag_data2d("ObsDiagSave_nldepart", obsdiags(i_oz_ob_type,ibin)%tail%nldepart )
!                   call nc_diag_data2d("ObsDiagSave_tldepart", obsdiags(i_oz_ob_type,ibin)%tail%tldepart )
!                   call nc_diag_data2d("ObsDiagSave_obssen",   obsdiags(i_oz_ob_type,ibin)%tail%obssen   )
                 endif
              endif
           endif ! (in_curbin)

        enddo ! < over nlevs >

     else

        if(in_curbin) then
           if (ozone_diagsave.and.lobsdiagsave.and.luse(i)) then
              rdiagbuf(7:irdim1,1:nlevs,ii) = zero
           endif
        endif ! (in_curbin)
 
     endif ! < l_may_be_passive >

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (ozone_diagsave) then

     if (netcdf_diag) call nc_diag_write

     if (binary_diag .and. ii>0) then
        filex=obstype
        write(string,100) jiter
100     format('_',i2.2)
        diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
        if(init_pass) then
           open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
        else
           inquire(file=diag_ozone_file,exist=ozdiagexist)
           if (ozdiagexist) then
              open(4,file=diag_ozone_file,form='unformatted',status='old',position='append')
           else
              open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
           endif
        endif
        iextra=0
        if (init_pass .and. mype==mype_diaghdr(is)) then
           write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,irdim1,ioff0
           write(6,*)'SETUPOZLAY:   write header record for ',&
                isis,iint,ireal,irdim1,' to file ',trim(diag_ozone_file),' ',ianldate
           do i=1,nlevs
              pob4(i)=pobs(i)
              grs4(i)=gross(i)
              err4(i)=tnoise(i)
           end do
           write(4) pob4,grs4,err4,iouse
        endif
        write(4) ii
        write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,:,1:ii)
        close(4)
     endif ! binary_diag
  endif ! ozone_diagsave

! Release memory of local guess arrays
  call final_vars_

! clean up
  call dtime_show('setupozlay','diagsave:oz',i_oz_ob_type)
  if(ozone_diagsave) deallocate(rdiagbuf)

! End of routine
  return

  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::oz', ivar, istatus )
  proceed=ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get oz ...
     varname='oz'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_oz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_oz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_oz(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_oz(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.

     write(string,900) jiter
900  format('_',i2.2,'.nc4')
     filex=obstype
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)

     inquire(file=diag_ozone_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_ozone_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_ozone_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_ozone_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_ozone_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_ozone_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Number_of_state_vars", nsdim          )
     endif

  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '  ozlay'
! contents interleafed above should be moved here (RTodling)
  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_oz)) deallocate(ges_oz)
  end subroutine final_vars_

end subroutine setupozlay


subroutine setupozlev(lunin,mype,stats_oz,nlevs,nreal,nobs,&
     obstype,isis,is,ozone_diagsave,init_pass)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupozlev --- Compute rhs of oi for mls ozone mixing ratio obs at pressure levels
!
!   prgrmmr:     H.Liu          org: np22                date: 2010-10-18
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   2010-10-18  h.liu   - subroutine for mls data: based on setupoz and Sienkiewicz's setupo3lv
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2014-05-12  wargan  - refine MLS gross check
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2016-12-09  mccarty - add netcdf_diag capability
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!     nreal          - number of pieces of non-ozone info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of ozone obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use mpeu_util, only: die,perr,getindex
  use kinds, only: r_kind,r_single,i_kind

  use state_vectors, only: svars3d, levels
  use sparsearr, only : sparr2, new, size, writearray

  use m_obsdiags, only : o3lhead
  use obsmod, only : i_o3l_ob_type,dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave,lobsdiag_forenkf
  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use m_obsNode, only: obsNode
  use m_o3lNode, only : o3lNode
  use m_obsLList, only : obsLList_appendNode
  use obsmod, only : obs_diag,luse_obsdiag

  use guess_grids, only : nfldsig,ges_lnprsl,hrdifsig

  use constants, only : zero,half,one,two,tiny_r_kind,four
  use constants, only : cg_term,wgtlim,r10,constoz

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ijk,nsig

  use ozinfo, only : gross_oz, jpch_oz, nusis_oz
  use ozinfo, only : b_oz,pg_oz

  use jfunc, only : jiter,last,miter,jiterstart
  
  use m_dtime, only: dtime_setup, dtime_check, dtime_show

  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  implicit none
  
! !INPUT PARAMETERS:

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-ozone info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of ozone obs
  logical                          , intent(in   ) :: ozone_diagsave   ! switch on diagnostic output (.false.=no output)
  logical                          , intent(in   ) :: init_pass        ! state of "setup" processing

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_oz), intent(inout) :: stats_oz ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: rmiss = -9999.9_r_kind
  character(len=*),parameter:: myname="setupozlev"

! Declare external calls for code analysis
  external:: tintrp2a1,tintrp2a11
  external:: tintrp3
  external:: grdcrd1
  external:: stop2

! Declare local variables  
  real(r_kind) :: delz
  integer(i_kind) :: iz, oz_ind, nind, nnz
  type(sparr2) :: dhx_dx
  
  real(r_kind) o3ges, o3ppmv
  real(r_kind) rlow,rhgh,sfcchk
  real(r_kind) omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_oz,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) errorinv
  real(r_kind) psges,ozlv
  
  real(r_kind) varinv3,ratio_errors
  real(r_kind) dpres,obserror,ozone_inv,preso3l
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig):: prsltmp
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf

  integer(i_kind) i,ii,jj,iextra,istat,ibin
  integer(i_kind) k,j,idia,irdim1,ioff0
  integer(i_kind) isolz,iuse
  integer(i_kind) mm1,itime,ilat,ilon,ilate,ilone,iozmr,ilev,ipres,iprcs,imls_levs
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  real(r_kind) gross

  character(12) string
  character(10) filex
  character(128) diag_ozone_file

  logical:: ozdiagexist
  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical proceed

  logical:: in_curbin, in_anybin, save_jacobian
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  class(obsNode),pointer:: my_node
  type(o3lNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_oz

  save_jacobian = ozone_diagsave .and. jiter==jiterstart .and. lobsdiag_forenkf

! Check to see if required guess fields are available
! Question: Should a message be produced before return, to inform the
! system what has been going on?
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0

  mm1=mype+1


!
!*********************************************************************************
! Initialize arrays

  if(ozone_diagsave)then
     irdim1=7
     ioff0 = irdim1
     if(lobsdiagsave) irdim1=irdim1+4*miter+1
     if (save_jacobian) then
       nnz   = 2                   ! number of non-zero elements in dH(x)/dx profile
       nind  = 1
       call new(dhx_dx, nnz, nind)
       irdim1 = irdim1 + size(dhx_dx)
     endif
     allocate(rdiagbuf(irdim1,1,nobs))
     rdiagbuf=0._r_single
     if(netcdf_diag) call init_netcdf_diag_
  end if

! index information for data array (see reading routine)
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)
  isolz=7     ! index of solar zenith angle 
  iuse=8      ! index of usage flag
  ipres=9     ! index of pressure in log(cb)
  iprcs=10    ! index of mixing ratio precision in ppmv
  ilev=11     ! index of obs level
  imls_levs=12 ! index of mls nrt vertical levels
  iozmr=13    ! index of ozone mixing ratio in ppmv

! Read and transform ozone data
  read(lunin) data,luse,ioid

! Set flag for obs use
  do i=1,nobs
     muse(i)=nint(data(iuse,i))<=jiter
  end do

! If requested, save data for diagnostic ouput
  if(ozone_diagsave)ii=0

! Convert observation (lat,lon) from earth to grid relative values
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) cycle

     dpres=data(ipres,i)   !pressure in log(cb)
     preso3l =r10*exp(dpres)

     dlat=data(ilat,i)
     dlon=data(ilon,i)
     dtime=data(itime,i)
     obserror=data(iprcs,i)
              
     if (nobskeep>0) then
        write(6,*)'setupozlev: nobskeep',nobskeep
        call stop2(338)
     end if
 
!    Link observation to appropriate observation bin
     if (nobs_bins>1) then
        ibin = NINT( dtime/hr_obsbin ) + 1
     else
        ibin = 1
     endif
     IF (ibin<1.OR.ibin>nobs_bins) write(6,*) 'SETUPOZLEV: ', mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

!    Link obs to diagnostics structure
     if (luse_obsdiag) then
        if (.not.lobsdiag_allocated) then
           if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%head)) then
              obsdiags(i_o3l_ob_type,ibin)%n_alloc = 0
              allocate(obsdiags(i_o3l_ob_type,ibin)%head,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupozlev: failure to allocate obsdiags',istat
                 call stop2(256)
              end if
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
           else
              allocate(obsdiags(i_o3l_ob_type,ibin)%tail%next,stat=istat)
              if (istat/=0) then
                 write(6,*)'setupozlev: failure to allocate obsdiags',istat
                 call stop2(257)
              end if
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
           end if
           obsdiags(i_o3l_ob_type,ibin)%n_alloc = obsdiags(i_o3l_ob_type,ibin)%n_alloc +1
    
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%muse(miter+1))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(miter+1))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(miter))
           allocate(obsdiags(i_o3l_ob_type,ibin)%tail%obssen(miter))
           obsdiags(i_o3l_ob_type,ibin)%tail%indxglb=ioid(i)
           obsdiags(i_o3l_ob_type,ibin)%tail%nchnperobs=-99999
           obsdiags(i_o3l_ob_type,ibin)%tail%luse=luse(i)
           obsdiags(i_o3l_ob_type,ibin)%tail%muse(:)=.false.
           obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
           obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(:)=zero
           obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo=-huge(zero)
           obsdiags(i_o3l_ob_type,ibin)%tail%obssen(:)=zero
    
           n_alloc(ibin) = n_alloc(ibin) +1
           my_diag => obsdiags(i_o3l_ob_type,ibin)%tail
           my_diag%idv = is
           my_diag%iob = ioid(i)
           my_diag%ich = 1
           my_diag%elat= data(ilate,i)
           my_diag%elon= data(ilone,i)
    
        else
           if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%tail)) then
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%head
           else
              obsdiags(i_o3l_ob_type,ibin)%tail => obsdiags(i_o3l_ob_type,ibin)%tail%next
           end if
           if (.not.associated(obsdiags(i_o3l_ob_type,ibin)%tail)) then
              call die(myname,'.not.associated(obsdiags(i_o3l_ob_type,ibin)%tail)')
           endif
           if (obsdiags(i_o3l_ob_type,ibin)%tail%indxglb/=ioid(i)) then
              write(6,*)'setupozlev: index error'
              call stop2(258)
           end if
        endif
     endif

     if(.not.in_curbin) cycle

!    Interpolate ps to obs locations/times
     call tintrp2a11(ges_ps,psges,dlat,dlon,dtime,hrdifsig, &
          mype,nfldsig)

!    Interpolate log(pres) at mid-layers to obs locations/times
     call tintrp2a1(ges_lnprsl,prsltmp,dlat,dlon,dtime,hrdifsig, &
          nsig,mype,nfldsig)

!    Get approximate k value of surface by using surface pressure
!    for surface check.
     sfcchk=log(psges)
     call grdcrd1(sfcchk,prsltmp,nsig,-1)

     if(ozone_diagsave .and. luse(i))then
        ii=ii+1
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
     endif

     ozlv=data(iozmr,i)      ! ozone mixing ratio in ppmv at pressure level

!    Pressure level of data (dpres) converted to grid coordinate
!    (wrt mid-layer pressure)
     call grdcrd1(dpres,prsltmp,nsig,-1)

!    Check if observation above model top or below model surface

     rlow=max(sfcchk-dpres,zero)
     rhgh=max(dpres-0.001_r_kind-float(nsig),zero)

!    calculate factor for error adjustment if too (high,low)
     ratio_errors=obserror/(obserror+1.0e6_r_kind*rhgh+four*rlow)

!    Check to see if observations is above the top of the model
     if (dpres > float(nsig)) then
         ratio_errors=zero
         obserror=1.0e6_r_kind
     end if

!    Interpolate guess ozone to observation location and time
     call tintrp31(ges_oz,o3ges,dlat,dlon,dpres,dtime, &
       hrdifsig,mype,nfldsig)
     iz = max(1, min( int(dpres), nsig))
     delz = max(zero, min(dpres - float(iz), one))
     if (save_jacobian) then
        oz_ind = getindex(svars3d, 'oz')
        if (oz_ind < 0) then
           print *, 'Error: no variable oz in state vector. Exiting.'
           call stop2(1300)
        endif

        dhx_dx%st_ind(1)  = iz  + sum(levels(1:oz_ind-1))         
        dhx_dx%end_ind(1) = min(iz + 1,nsig) + sum(levels(1:oz_ind-1))

        dhx_dx%val(1) = constoz * (one - delz)         ! weight for iz's level
        dhx_dx%val(2) = constoz * delz               ! weight for iz+1's level
     endif


!    Compute innovations - background o3ges in g/g so adjust units
!    Leave increment in ppmv for gross checks,  etc.

     o3ppmv = o3ges * constoz
     ozone_inv=ozlv - o3ppmv

!    Perform gross checks, and accumualte numbers for statistics

     j=nint(data(ilev,i))  !the entry # in ozinfo.txt

!    Set inverse obs error squared and ratio_errors
     if (obserror>zero .and. obserror<1.e4_r_kind) then
        varinv3 = one/(obserror**2)
        ratio_errors = one*ratio_errors
     else
        varinv3 = zero
        ratio_errors = zero
     endif

!    toss the obs not recommended by the data provider
     if (nint(data(iuse,i)) == 1000 ) then
        varinv3=zero
        ratio_errors=zero
     endif

!    Perform gross check (smallness of O-F criterion added)
     do jj=1,jpch_oz
        if (isis == nusis_oz(jj) .and. jj == j) then
           gross=gross_oz(jj)
        endif
     end do

     if( abs(ozone_inv)/obserror > gross .or.ozlv > 1.e+02_r_kind ) then
        varinv3=zero
        ratio_errors=zero
        if(luse(i))stats_oz(2,j) = stats_oz(2,j) + one ! number of obs tossed
     endif

!    check if gross check failed, mark failed obs for non-use
     if (ratio_errors/obserror <=tiny_r_kind) then
         muse(i)=.false.
     end if

!    Accumulate numbers for statistics
     rat_err2 = ratio_errors**2
     if (varinv3>tiny_r_kind .or. ozone_diagsave) then
        if(luse(i))then
           omg=ozone_inv
           stats_oz(1,j) = stats_oz(1,j) + one                          ! # obs
           stats_oz(3,j) = stats_oz(3,j) + omg                          ! (o-g)
           stats_oz(4,j) = stats_oz(4,j) + omg*omg                      ! (o-g)**2
           stats_oz(5,j) = stats_oz(5,j) + omg*omg*varinv3*rat_err2  ! penalty
           stats_oz(6,j) = stats_oz(6,j) + ozlv                        ! obs

           exp_arg = -half*varinv3*omg**2
           errorinv = sqrt(varinv3)
           if (pg_oz(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
              arg  = exp(exp_arg)
              wnotgross= one-pg_oz(j)
              cg_oz=b_oz(j)*errorinv
              wgross = cg_term*pg_oz(j)/(cg_oz*wnotgross)
              term = log((arg+wgross)/(one+wgross))
              wgt  = one-wgross/(arg+wgross)
           else
              term = exp_arg
              wgt  = one
           endif

           stats_oz(8,j) = stats_oz(8,j) -two*rat_err2*term
           if(wgt < wgtlim) stats_oz(9,j)=stats_oz(9,j)+one
        end if
     endif

!    If not assimilating this observation, reset inverse variance to zero
     if ( .not. muse(i)) then
        varinv3=zero
        ratio_errors=zero
        rat_err2 = zero
     end if
     if (rat_err2*varinv3>tiny_r_kind .and. luse(i)) &
        stats_oz(7,j) = stats_oz(7,j) + one

     if (luse_obsdiag) then
        obsdiags(i_o3l_ob_type,ibin)%tail%muse(jiter)=muse(i)
        obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jiter)=ozone_inv
        obsdiags(i_o3l_ob_type,ibin)%tail%wgtjo= varinv3*ratio_errors**2
     endif

     if (.not. last .and. muse(i) ) then

        allocate(my_head)
        m_alloc(ibin) = m_alloc(ibin) +1
        my_node => my_head        ! this is a workaround
        call obsLList_appendNode(o3lhead(ibin),my_node)
        my_node => null()

        my_head%idv = is
        my_head%iob = ioid(i)
        my_head%elat= data(ilate,i)
        my_head%elon= data(ilone,i)

!       Set (i,j,k) indices of guess gridpoint that bound obs location
        my_head%dlev = dpres
        call get_ijk(mm1,dlat,dlon,dpres,my_head%ij,my_head%wij)

        do k=1,8
           my_head%wij(k)=my_head%wij(k)*constoz
        end do

        my_head%res        = ozone_inv
        my_head%err2       = varinv3
        my_head%raterr2    = ratio_errors**2
        my_head%luse       = luse(i)
        my_head%time       = dtime
        my_head%b          = b_oz(j)
        my_head%pg         = pg_oz(j)

        if (luse_obsdiag) then
           my_head%diags => obsdiags(i_o3l_ob_type,ibin)%tail

           my_diag => my_head%diags
           if(my_head%idv /= my_diag%idv .or. &
              my_head%iob /= my_diag%iob ) then
              call perr(myname,'mismatching %[head,diags]%(idv,iob,ich,ibin) =', &
                        (/is,ioid(i),k,ibin/))
              call perr(myname,'my_head%(idv,iob,ich) =',(/my_head%idv,my_head%iob,k/))
              call perr(myname,'my_diag%(idv,iob,ich) =',(/my_diag%idv,my_diag%iob,my_diag%ich/))
              call die(myname)
           endif
        endif

        my_head => null()
     endif

!    Optionally save data for diagnostics
     if (ozone_diagsave .and. luse(i)) then
        errorinv = sqrt(varinv3*rat_err2)

        if (binary_diag) call contents_binary_diag_
        if (netcdf_diag) call contents_netcdf_diag_
     end if   !end if(ozone_diagsave )

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (ozone_diagsave) then
     if (netcdf_diag) call nc_diag_write
     if (binary_diag .and. ii>0) then
        filex=obstype
        write(string,100) jiter
100     format('_',i2.2)
        diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
        if(init_pass) then
           open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
        else
           inquire(file=diag_ozone_file,exist=ozdiagexist)
           if (ozdiagexist) then
              open(4,file=diag_ozone_file,form='unformatted',status='old',position='append')
           else
              open(4,file=diag_ozone_file,form='unformatted',status='unknown',position='rewind')
           endif
        endif
        iextra=0
        if (init_pass .and. mype==mype_diaghdr(is)) then
           write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,irdim1,ioff0
           write(6,*)'SETUPOZLEV:   write header record for ',&
                isis,iint,ireal,irdim1,' to file ',trim(diag_ozone_file),' ',ianldate
        endif
        write(4) ii
        write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,1,1:ii)
        close(4)
     endif ! binary_diag
  endif ! ozone_diagsave

! Release memory of local guess arrays
  call final_vars_

! clean up
  call dtime_show('setupozlev','diagsave:ozlv',i_o3l_ob_type)
  if(ozone_diagsave) deallocate(rdiagbuf)

! End of routine
  return
  contains

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::oz' , ivar, istatus )
  proceed=proceed.and.ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get oz ...
     varname='oz'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)
     if (istatus==0) then
         if(allocated(ges_oz))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_oz(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_oz(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)
            ges_oz(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  character(len=80) string
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false.
     write(string,900) jiter
900  format('_',i2.2,'.nc4')
     filex=obstype
     diag_ozone_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)

     inquire(file=diag_ozone_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_ozone_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_ozone_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_ozone_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_ozone_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_ozone_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
        call nc_diag_header("Satellite_Sensor", isis)
        call nc_diag_header("Satellite", dplat(is))
        call nc_diag_header("Observation_type", obstype)
     endif

  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_
        rdiagbuf(1,1,ii) = ozlv                ! obs
        rdiagbuf(2,1,ii) = ozone_inv           ! obs-ges
        rdiagbuf(3,1,ii) = errorinv            ! inverse observation error
        rdiagbuf(4,1,ii) = preso3l             ! override solar zenith angle with a reference pressure (in hPa)
        rdiagbuf(5,1,ii) = rmiss               ! fovn
        rdiagbuf(6,1,ii) = obserror            ! ozone mixing ratio precision
        rdiagbuf(7,1,ii) = 1.e+10_r_single     ! spread (filled in by EnKF)

        if (lobsdiagsave) then
           idia=6
           do jj=1,miter
              idia=idia+1
              if (obsdiags(i_o3l_ob_type,ibin)%tail%muse(jj)) then
                 rdiagbuf(idia,1,ii) = one
              else
                 rdiagbuf(idia,1,ii) = -one
              endif
           enddo
           do jj=1,miter+1
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%nldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%tldepart(jj)
           enddo
           do jj=1,miter
              idia=idia+1
              rdiagbuf(idia,1,ii) = obsdiags(i_o3l_ob_type,ibin)%tail%obssen(jj)
           enddo
        endif
        if (save_jacobian) then
           call writearray(dhx_dx, rdiagbuf(idia+1:irdim1,1,ii))
           idia = idia + size(dhx_dx)
        endif

  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '  ozlev'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Latitude",                     sngl(data(ilate,i))            )
           call nc_diag_metadata("Longitude",                    sngl(data(ilone,i))            )
           call nc_diag_metadata("MPI_Task_Number",              mype                           )
           call nc_diag_metadata("Time",                         sngl(data(itime,i)-time_offset))
           call nc_diag_metadata("Inverse_Observation_Error",    sngl(errorinv)                 )
           call nc_diag_metadata("Observation",                  sngl(ozlv)                     ) 
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",  sngl(ozone_inv)                )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted",sngl(ozone_inv)                )
           call nc_diag_metadata("Reference_Pressure",           sngl(preso3l)                  )
           call nc_diag_metadata("Input_Observation_Error",      sngl(obserror)                 ) 

           if (lobsdiagsave) then
              do jj=1,miter
                 if (obsdiags(i_o3l_ob_type,ibin)%tail%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else 
                       obsdiag_iuse(jj) = -one
                 endif
              enddo

              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", obsdiags(i_o3l_ob_type,ibin)%tail%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", obsdiags(i_o3l_ob_type,ibin)%tail%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   obsdiags(i_o3l_ob_type,ibin)%tail%obssen   )
           endif
  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_oz)) deallocate(ges_oz)
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setupozlev
