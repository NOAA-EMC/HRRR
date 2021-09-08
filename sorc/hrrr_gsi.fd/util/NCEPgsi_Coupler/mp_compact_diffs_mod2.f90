module mp_compact_diffs_mod2
!$$$ module documentation block
!           .      .    .                                       .
! module:   mp_compact_diffs_mod2
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-06  lueken - added module doc block
!
! subroutines included:
!   sub init_mp_compact_diffs2
!   sub destroy_mp_compact_diffs2
!   sub cdiff_sd2ew0
!   sub cdiff_sd2ew1
!   sub cdiff_ew2sd1
!   sub cdiff_sd2ew
!   sub cdiff_sd2ew2
!   sub cdiff_sd2ew3
!   sub cdiff_ew2sd
!   sub cdiff_ew2sd2
!   sub cdiff_ew2sd3
!   sub cdiff_sd2ns0
!   sub cdiff_sd2ns1
!   sub cdiff_ns2sd1
!   sub cdiff_sd2ns
!   sub cdiff_sd2ns2
!   sub cdiff_sd2ns3
!   sub cdiff_ns2sd
!   sub cdiff_ns2sd2
!   sub cdiff_ns2sd3
!   sub mp_compact_dlon
!   sub mp_compact_dlon_ad
!   sub mp_compact_dlat
!   sub mp_compact_dlat_ad
!   sub mp_uv_pole
!   sub mp_uv_pole_ad
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: izero,ione
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_mp_compact_diffs2
  public :: destroy_mp_compact_diffs2
  public :: cdiff_sd2ew0
  public :: cdiff_sd2ew1
  public :: cdiff_ew2sd1
  public :: cdiff_sd2ew
  public :: cdiff_sd2ew2
  public :: cdiff_sd2ew3
  public :: cdiff_ew2sd
  public :: cdiff_ew2sd2
  public :: cdiff_ew2sd3
  public :: cdiff_sd2ns0
  public :: cdiff_sd2ns1
  public :: cdiff_ns2sd1
  public :: cdiff_sd2ns
  public :: cdiff_sd2ns2
  public :: cdiff_sd2ns3
  public :: cdiff_ns2sd
  public :: cdiff_ns2sd2
  public :: cdiff_ns2sd3
  public :: mp_compact_dlon
  public :: mp_compact_dlon_ad
  public :: mp_compact_dlat
  public :: mp_compact_dlat_ad
  public :: mp_uv_pole
  public :: mp_uv_pole_ad
! set passed variables to public
  public :: nlat_1,slow_pole,nlat_0,nlon_0,nlon_1

  integer(i_kind),allocatable::list_sd2ew(:,:)   ! list_sd2ew(1,j) = lat index 1 for ew strip j
                                                 ! list_sd2ew(2,j) = lat index 2 for ew strip j
                                                 ! list_sd2ew(3,j) = vert index for ew strip j
                                                 ! list_sd2ew(4,j) = pe of this lat/vert index strip
  integer(i_kind) nlat_0,nlat_1
  integer(i_kind) nallsend_sd2ew,nallrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ew,nrecv_sd2ew
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ew,ndrecv_sd2ew
  integer(i_kind),allocatable,dimension(:,:)::info_send_sd2ew,info_recv_sd2ew
  integer(i_kind) nallsend_ew2sd,nallrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::nsend_ew2sd,nrecv_ew2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ew2sd,ndrecv_ew2sd
  integer(i_kind),allocatable,dimension(:,:)::info_send_ew2sd,info_recv_ew2sd

  integer(i_kind),allocatable::list_sd2ns(:,:)   ! list_sd2ns(1,j) = lon index 1 for ew strip j
                                                 ! list_sd2ns(2,j) = lon index 2 for ew strip j
                                                 ! list_sd2ns(3,j) = vert index for ew strip j
                                                 ! list_sd2ns(4,j) = pe of this lat/vert index strip
  integer(i_kind) nlon_0,nlon_1
  integer(i_kind) nallsend_sd2ns,nallrecv_sd2ns
  integer(i_kind),allocatable,dimension(:)::nsend_sd2ns,nrecv_sd2ns
  integer(i_kind),allocatable,dimension(:)::ndsend_sd2ns,ndrecv_sd2ns
  integer(i_kind),allocatable,dimension(:,:)::info_send_sd2ns,info_recv_sd2ns
  integer(i_kind) nallsend_ns2sd,nallrecv_ns2sd
  integer(i_kind),allocatable,dimension(:)::nsend_ns2sd,nrecv_ns2sd
  integer(i_kind),allocatable,dimension(:)::ndsend_ns2sd,ndrecv_ns2sd
  integer(i_kind),allocatable,dimension(:,:)::info_send_ns2sd,info_recv_ns2sd

  logical slow_pole

contains

subroutine init_mp_compact_diffs2(nlev,mype,slow_pole_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mp_compact_diffs2
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    slow_pole_in
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype
  logical        ,intent(in   ) :: slow_pole_in

  slow_pole=slow_pole_in
  call cdiff_sd2ew0(nlev,mype)
  call cdiff_sd2ew1(nlev,mype)
  call cdiff_ew2sd1(mype)
  call cdiff_sd2ns0(nlev,mype)
  call cdiff_sd2ns1(nlev,mype)
  call cdiff_ns2sd1(mype)

end subroutine init_mp_compact_diffs2

subroutine destroy_mp_compact_diffs2
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_mp_compact_diffs2
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(list_sd2ew,nsend_sd2ew,nrecv_sd2ew,ndsend_sd2ew,ndrecv_sd2ew)
  deallocate(info_send_sd2ew,info_recv_sd2ew,nsend_ew2sd,nrecv_ew2sd)
  deallocate(ndsend_ew2sd,ndrecv_ew2sd,info_send_ew2sd,info_recv_ew2sd)

  deallocate(list_sd2ns,nsend_sd2ns,nrecv_sd2ns,ndsend_sd2ns,ndrecv_sd2ns)
  deallocate(info_send_sd2ns,info_recv_sd2ns,nsend_ns2sd,nrecv_ns2sd)
  deallocate(ndsend_ns2sd,ndrecv_ns2sd,info_send_ns2sd,info_recv_ns2sd)

end subroutine destroy_mp_compact_diffs2

subroutine cdiff_sd2ew0(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew0
!   prgmmr:
!
! abstract: create ew (lat strips) subdivision for use in global spectral transform
!
!  output:
!
!     nlat_0,nlat_1:   range of lat/vert index on processor mype
!
!                    1 <= nlat_0 <= nlat_1 <= ((nlat+1)/2)*nlev
!                    if npe > ((nlat+1)/2)*nlev, then will have nlat_0 = -1, nlat_1 = -2
!                    on some processors, and nlat_0=nlat_1 on the
!                    remaining ((nlat+1)/2)*nlev processors
!
!     list_sd2ew(4,((nlat+1)/2)*nlev):  global definition of contents of each lat/vert strip
!                      list_sd2ew(1,j) = lat index 1 for ew strip j
!                      list_sd2ew(2,j) = lat index 2 for ew strip j
!                      list_sd2ew(3,j) = vert level for ew strip j
!                      list_sd2ew(4,j) = pe of this lat/vert strip
!
!                      because pole values are computed from the row adjacent to the pole,
!                      the latitudes are kept in adjacent pairs, ie (1,2),(3,4),...,(nlat-1,nlat)
!
!                      if the number of lats is odd, then the second pair contains two duplicate
!                      latitudes, ie  (1,2),(3,3),(4,5),...,(nlat-1,nlat)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat
  use mpimod, only: npe
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) nlat_this,nlat_tot,kchk,i,k,kk,n,nn

  allocate(list_sd2ew(4,((nlat+ione)/2)*nlev))

  nlat_tot=((nlat+ione)/2)*nlev
  nlat_this=nlat_tot/npe
  if(mod(nlat_tot,npe)/=izero) nlat_this=nlat_this+ione
  if(mod(nlat_tot,npe)==izero) then
     kchk=npe
  else
     kchk=mod(nlat_tot,npe)
  end if

  nn=izero
  do k=1,nlev
     nn=nn+ione
     list_sd2ew(1,nn)=ione
     list_sd2ew(2,nn)=2_i_kind
     list_sd2ew(3,nn)=k
     list_sd2ew(4,nn)=-ione
     if(mod(nlat,2_i_kind)/=izero) then
!                           nlat odd:
        nn=nn+ione
        list_sd2ew(1,nn)=3_i_kind
        list_sd2ew(2,nn)=3_i_kind
        list_sd2ew(3,nn)=k
        list_sd2ew(4,nn)=-ione
        do i=4,nlat-ione,2
           nn=nn+ione
           list_sd2ew(1,nn)=i
           list_sd2ew(2,nn)=i+ione
           list_sd2ew(3,nn)=k
           list_sd2ew(4,nn)=-ione
        end do

     else
!                           nlat even:
        do i=3,nlat-ione,2
           nn=nn+ione
           list_sd2ew(1,nn)=i
           list_sd2ew(2,nn)=i+ione
           list_sd2ew(3,nn)=k
           list_sd2ew(4,nn)=-ione
        end do

     end if

  end do

!  if(mype==izero) write(0,*)' nn,nlat_tot,nlat,nlev=',nn,nlat_tot,nlat,nlev

  nlat_0=-ione
  nlat_1=-2_i_kind
  nn=izero
  do n=1,npe
     if(n<=kchk) then
        kk=nlat_this
     else
        kk=nlat_this-ione
     end if
     if(kk>izero) then
        if(mype+ione==n) then
           nlat_0=nn+ione
           nlat_1=nn+kk
        end if
        do k=1,kk
           nn=nn+ione
           list_sd2ew(4,nn)=n
        end do
     end if
  end do
! write(0,*) '  mype,nlat_0,nlat_1,nlat_1-nlat0+1=',mype,nlat_0,nlat_1,nlat_1-nlat_0+1
! if(mype==izero) then
!    do i=1,nlat_tot
!       write(0,'(" i,list_sd2ew(:,i)=",i5,4i6)')i,list_sd2ew(1:4,i)
!    end do
! end if

end subroutine cdiff_sd2ew0

subroutine cdiff_sd2ew1(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew1
!   prgmmr:
!
! abstract: continue with setup for subdomain to lat strip interchanges
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) list2(nlat,nlev)
  integer(i_kind) i,ii,ii0,ilat,ilat_1,ilat_2,ivert,j,mm1,nn,nlonloc,ipe,ilatm,ilon,mpi_string1
  integer(i_kind) isig,nlat_tot,i12

  allocate(nsend_sd2ew(npe),nrecv_sd2ew(npe),ndsend_sd2ew(npe+ione),ndrecv_sd2ew(npe+ione))
  mm1=mype+ione
  nlat_tot=((nlat+ione)/2)*nlev

  nn=izero
  list2=izero
  do j=1,nlat_tot
     ilat_1=list_sd2ew(1,j)
     ilat_2=list_sd2ew(2,j)
     ivert=list_sd2ew(3,j)
     if(list2(ilat_1,ivert)/=izero.or.list2(ilat_2,ivert)/=izero) then
        if(mype==izero) write(0,*)' problem in cdiff_sd2ew1'
        call mpi_finalize(i)
        stop
     end if
     list2(ilat_1,ivert)=j
     list2(ilat_2,ivert)=j
  end do
  do ivert=1,nlev
     do ilat=1,nlat
        if(list2(ilat,ivert)==izero) then
           if(mype==izero) write(0,*)' problem in cdiff_sd2ew1'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend_sd2ew=izero
  nlonloc=lon2-2_i_kind
  do ivert=1,nlev
     do i=2,lat2-ione
        ilat=i+istart(mm1)-2_i_kind
        j=list2(ilat,ivert)
        ipe=list_sd2ew(4,j)
        nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+nlonloc
     end do
  end do

  ndsend_sd2ew(1)=izero
  do i=2,npe+ione
     ndsend_sd2ew(i)=ndsend_sd2ew(i-ione)+nsend_sd2ew(i-ione)
  end do
  nallsend_sd2ew=ndsend_sd2ew(npe+ione)
  allocate(info_send_sd2ew(4,nallsend_sd2ew))
  nsend_sd2ew=izero
  do ivert=1,nlev
     do i=2,lat2-ione
        ilat=i+istart(mm1)-2_i_kind
        ilatm=list2(ilat,ivert)
        ilat_1=list_sd2ew(1,ilatm)
        ilat_2=list_sd2ew(2,ilatm)
        i12=izero
        if(ilat_1==ilat) i12=ione
        if(ilat_2==ilat) i12=2_i_kind
        isig =list_sd2ew(3,ilatm)
        ipe=list_sd2ew(4,ilatm)
        do ii=2,lon2-ione
           ilon=ii+jstart(mm1)-2_i_kind
           nsend_sd2ew(ipe)=nsend_sd2ew(ipe)+ione
           ii0=ndsend_sd2ew(ipe)+nsend_sd2ew(ipe)
           info_send_sd2ew(1,ii0)=ilon
           info_send_sd2ew(2,ii0)=ilatm
           info_send_sd2ew(3,ii0)=i12
           info_send_sd2ew(4,ii0)=isig
        end do
     end do
  end do

  call mpi_alltoall(nsend_sd2ew,ione,mpi_integer4,nrecv_sd2ew,ione,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ew(1)=izero
  do i=2,npe+ione
     ndrecv_sd2ew(i)=ndrecv_sd2ew(i-ione)+nrecv_sd2ew(i-ione)
  end do
  nallrecv_sd2ew=ndrecv_sd2ew(npe+ione)
  allocate(info_recv_sd2ew(4,nallrecv_sd2ew))
  call mpi_type_contiguous(4_i_kind,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ew,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     info_recv_sd2ew,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_sd2ew1

subroutine cdiff_ew2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,jstart,istart,ilat1,jlon1
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,i1,i2,ilat_1,ilat_2,ivert,j,k,mm1,ipe,ilon,mpi_string1,nn

  allocate(nsend_ew2sd(npe),nrecv_ew2sd(npe),ndsend_ew2sd(npe+ione),ndrecv_ew2sd(npe+ione))
  mm1=mype+ione

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
     nn=izero
     do k=nlat_0,nlat_1
        ilat_1=list_sd2ew(1,k)
        ilat_2=list_sd2ew(2,k)
        ivert=list_sd2ew(3,k)
        i1=ilat_1-istart(ipe)+2_i_kind
        if(i1>=ione.and.i1<=ilat1(ipe)+2_i_kind) then
           do j=1,jlon1(ipe)+2_i_kind
              nn=nn+ione
           end do
        end if
        if(ilat_1==ilat_2) cycle
        i2=ilat_2-istart(ipe)+2_i_kind
        if(i2>=ione.and.i2<=ilat1(ipe)+2_i_kind) then
           do j=1,jlon1(ipe)+2_i_kind
              nn=nn+ione
           end do
        end if
     end do
     nsend_ew2sd(ipe)=nn
  end do

  ndsend_ew2sd(1)=izero
  do i=2,npe+ione
     ndsend_ew2sd(i)=ndsend_ew2sd(i-ione)+nsend_ew2sd(i-ione)
  end do
  nallsend_ew2sd=ndsend_ew2sd(npe+ione)
  allocate(info_send_ew2sd(4,nallsend_ew2sd))
  nn=izero
  do ipe=1,npe
     do k=nlat_0,nlat_1
        ilat_1=list_sd2ew(1,k)
        ilat_2=list_sd2ew(2,k)
        ivert=list_sd2ew(3,k)
        i1=ilat_1-istart(ipe)+2_i_kind
        if(i1>=ione.and.i1<=ilat1(ipe)+2_i_kind) then
           do j=1,jlon1(ipe)+2_i_kind
              ilon=j+jstart(ipe)-2_i_kind
              if(ilon<ione) ilon=ilon+nlon
              if(ilon>nlon) ilon=ilon-nlon
              nn=nn+ione
              info_send_ew2sd(1,nn)=ilon
              info_send_ew2sd(2,nn)=j
              info_send_ew2sd(3,nn)=k
              info_send_ew2sd(4,nn)=ione
           end do
        end if
        if(ilat_1==ilat_2) cycle
        i2=ilat_2-istart(ipe)+2_i_kind
        if(i2>=ione.and.i2<=ilat1(ipe)+2_i_kind) then
           do j=1,jlon1(ipe)+2_i_kind
              ilon=j+jstart(ipe)-2_i_kind
              if(ilon<ione) ilon=ilon+nlon
              if(ilon>nlon) ilon=ilon-nlon
              nn=nn+ione
              info_send_ew2sd(1,nn)=ilon
              info_send_ew2sd(2,nn)=j
              info_send_ew2sd(3,nn)=k
              info_send_ew2sd(4,nn)=2_i_kind
           end do
        end if
     end do
  end do

  call mpi_alltoall(nsend_ew2sd,ione,mpi_integer4,nrecv_ew2sd,ione,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ew2sd(1)=izero
  do i=2,npe+ione
     ndrecv_ew2sd(i)=ndrecv_ew2sd(i-ione)+nrecv_ew2sd(i-ione)
  end do
  nallrecv_ew2sd=ndrecv_ew2sd(npe+ione)
  allocate(info_recv_ew2sd(4,nallrecv_ew2sd))
  call mpi_type_contiguous(4_i_kind,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ew2sd,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     info_recv_ew2sd,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_ew2sd1

subroutine cdiff_sd2ew(u_sd,u_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ew (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_sd
!
!   output argument list:
!    u_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u_ew

  integer(i_kind) i12,ilat,ilatm,ilon,ivert,j,mm1
  real(r_kind),allocatable::sendbuf(:),recvbuf(:)

  mm1=mype+ione

  allocate(sendbuf(nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)
     ilatm=info_send_sd2ew(2,j)
     i12=info_send_sd2ew(3,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     sendbuf(j)=u_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(nallrecv_sd2ew))
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_rtype, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     i12=info_recv_sd2ew(3,j)
     u_ew(i12,ilon,ilatm)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ew

subroutine cdiff_sd2ew2(u1_sd,u2_sd,u1_ew,u2_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ew (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ew,u2_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u2_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u1_ew
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u2_ew

  integer(i_kind) i12,ilat,ilatm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+ione

  allocate(sendbuf(2,nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)
     ilatm=info_send_sd2ew(2,j)
     i12=info_send_sd2ew(3,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(2,nallrecv_sd2ew))
  call mpi_type_contiguous(2_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     i12=info_recv_sd2ew(3,j)
     u1_ew(i12,ilon,ilatm)=recvbuf(1,j)
     u2_ew(i12,ilon,ilatm)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ew2

subroutine cdiff_sd2ew3(u1_sd,u2_sd,u3_sd,u1_ew,u2_ew,u3_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ew3
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ew (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd,u3_sd
!
!   output argument list:
!    u1_ew,u2_ew,u3_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd,u2_sd,u3_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: u1_ew,u2_ew,u3_ew

  integer(i_kind) i12,ilat,ilatm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+ione

  allocate(sendbuf(3,nallsend_sd2ew))
  do j=1,nallsend_sd2ew
     ilon=info_send_sd2ew(1,j)
     ilatm=info_send_sd2ew(2,j)
     i12=info_send_sd2ew(3,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(3,j)=u3_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(3,nallrecv_sd2ew))
  call mpi_type_contiguous(3_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ew,ndsend_sd2ew,mpi_string1, &
                     recvbuf,nrecv_sd2ew,ndrecv_sd2ew,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ew
     ilon=info_recv_sd2ew(1,j)
     ilatm=info_recv_sd2ew(2,j)
     i12=info_recv_sd2ew(3,j)
     u1_ew(i12,ilon,ilatm)=recvbuf(1,j)
     u2_ew(i12,ilon,ilatm)=recvbuf(2,j)
     u3_ew(i12,ilon,ilatm)=recvbuf(3,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ew3

subroutine cdiff_ew2sd(u_sd,u_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_ew
!
!   output argument list:
!    u_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: u_ew

  real(r_kind),allocatable::sendbuf(:),recvbuf(:)
  integer(i_kind) i12,ilat,ivert,j,mm1,ilatm,ilon,ilonloc

  mm1=mype+ione

  allocate(sendbuf(nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     i12=info_send_ew2sd(4,j)
     sendbuf(j)=u_ew(i12,ilon,ilatm)
  end do
  allocate(recvbuf(nallrecv_ew2sd))
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_rtype, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     i12=info_recv_ew2sd(4,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     u_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(j)
!--------------check for north or south pole
     ilat=-ione
     if(list_sd2ew(i12,ilatm)==nlat) ilat=nlat+ione
     if(list_sd2ew(i12,ilatm)==ione) ilat=izero
     if(ilat==-ione) cycle
!-----------------do repeat rows for north/south pole
     u_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_ew2sd

subroutine cdiff_ew2sd2(u1_sd,u2_sd,u1_ew,u2_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ew,u2_ew
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: u1_ew,u2_ew

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,mm1,ilatm,ilon,ilonloc,mpi_string1

  mm1=mype+ione

  allocate(sendbuf(2,nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     i12=info_send_ew2sd(4,j)
     sendbuf(1,j)=u1_ew(i12,ilon,ilatm)
     sendbuf(2,j)=u2_ew(i12,ilon,ilatm)
  end do
  allocate(recvbuf(2,nallrecv_ew2sd))
  call mpi_type_contiguous(2_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     i12=info_recv_ew2sd(4,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
!--------------check for north or south pole
     ilat=-ione
     if(list_sd2ew(i12,ilatm)==nlat) ilat=nlat+ione
     if(list_sd2ew(i12,ilatm)==ione) ilat=izero
     if(ilat==-ione) cycle
!-----------------do repeat rows for north/south pole
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_ew2sd2

subroutine cdiff_ew2sd3(u1_sd,u2_sd,u3_sd,u1_ew,u2_ew,u3_ew,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ew2sd3
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ew (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_ew,u2,ew,u3_ew
!
!   output argument list:
!    u1_sd,u2_sd,u3_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd,u3_sd
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: u1_ew,u2_ew,u3_ew

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,mm1,ilatm,ilon,ilonloc,mpi_string1

  mm1=mype+ione

  allocate(sendbuf(3,nallsend_ew2sd))
  do j=1,nallsend_ew2sd
     ilon=info_send_ew2sd(1,j)
     ilatm=info_send_ew2sd(3,j)
     i12=info_send_ew2sd(4,j)
     sendbuf(1,j)=u1_ew(i12,ilon,ilatm)
     sendbuf(2,j)=u2_ew(i12,ilon,ilatm)
     sendbuf(3,j)=u3_ew(i12,ilon,ilatm)
  end do
  allocate(recvbuf(3,nallrecv_ew2sd))
  call mpi_type_contiguous(3_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ew2sd,ndsend_ew2sd,mpi_string1, &
                     recvbuf,nrecv_ew2sd,ndrecv_ew2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ew2sd
     ilonloc=info_recv_ew2sd(2,j)
     ilatm=info_recv_ew2sd(3,j)
     i12=info_recv_ew2sd(4,j)
     ilat=list_sd2ew(i12,ilatm)
     ivert=list_sd2ew(3,ilatm)
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
     u3_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(3,j)
!--------------check for north or south pole
     ilat=-ione
     if(list_sd2ew(i12,ilatm)==nlat) ilat=nlat+ione
     if(list_sd2ew(i12,ilatm)==ione) ilat=izero
     if(ilat==-ione) cycle
!-----------------do repeat rows for north/south pole
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
     u3_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(3,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_ew2sd3

subroutine cdiff_sd2ns0(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns0
!   prgmmr:
!
! abstract: create ns (lon strips) subdivision for use with compact differences in latitude
!
!  output:
!
!     nlon_0,nlon_1:   range of lon/vert index on processor mype
!
!                    1 <= nlon_0 <= nlon_1 <= (nlon/2)*nlev
!                    if npe > (nlon/2)*nlev, then will have nlon_0 = -1, nlon_1 = -2
!                    on some processors, and nlon_0=nlon_1 on the
!                    remaining (nlon/2)*nlev processors
!
!     list_sd2ns(4,(nlon/2)*nlev):  global definition of contents of each lon/vert strip
!                      list_sd2ns(1,j) = lon index 1 for ns strip j
!                      list_sd2ns(2,j) = lon index 2 for ns strip j
!                      list_sd2ns(3,j) = vert level for ns strip j
!                      list_sd2ns(4,j) = pe of this lon/vert strip
!
!       NOTE:  only works for nlon even, because longitudes must be in pairs to
!                complete a great circle through the poles.
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon
  use mpimod, only: npe
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) nlon_this,nlon_tot,kchk,i,k,kk,n,nn,nlonh

  if(mod(nlon,2_i_kind)/=izero) then
     write(6,*)' FAILURE IN cdiff_sd2ns0, nlon not even'
     call stop2(99)
  end if
  nlonh=nlon/2
  allocate(list_sd2ns(4,nlonh*nlev))

  nlon_tot=nlonh*nlev
  nlon_this=nlon_tot/npe
  if(mod(nlon_tot,npe)/=izero) nlon_this=nlon_this+ione
  if(mod(nlon_tot,npe)==izero) then
     kchk=npe
  else
     kchk=mod(nlon_tot,npe)
  end if

  nn=izero
  do k=1,nlev
     do i=1,nlonh
        nn=nn+ione
        list_sd2ns(1,nn)=i
        list_sd2ns(2,nn)=i+nlonh
        list_sd2ns(3,nn)=k
        list_sd2ns(4,nn)=-ione
     end do

  end do

!  if(mype==izero) write(0,*)' nn,nlon_tot,nlon,nlonh,nlev=',nn,nlon_tot,nlon,nlonh,nlev

  nlon_0=-ione
  nlon_1=-2_i_kind
  nn=izero
  do n=1,npe
     if(n<=kchk) then
        kk=nlon_this
     else
        kk=nlon_this-ione
     end if
     if(kk>izero) then
        if(mype+ione==n) then
           nlon_0=nn+ione
           nlon_1=nn+kk
        end if
        do k=1,kk
           nn=nn+ione
           list_sd2ns(4,nn)=n
        end do
     end if
  end do
!  write(0,*) '  mype,nlon_0,nlon_1,nlon_1-nlon0+1=',mype,nlon_0,nlon_1,nlon_1-nlon_0+1
!  if(mype==izero) then
!     do i=1,nlon_tot
!        write(0,'(" i,list_sd2ns(:,i)=",i5,4i6)')i,list_sd2ns(1:4,i)
!     end do
!  end if

end subroutine cdiff_sd2ns0

subroutine cdiff_sd2ns1(nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns1
!   prgmmr:
!
! abstract: continue with setup for subdomain to lat strip interchanges
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlon,lon2,lat2,jstart,istart
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none

  integer(i_kind),intent(in   ) :: nlev,mype

  integer(i_kind) list2(nlon,nlev)
  integer(i_kind) i,ii,ii0,ilat,ilon_1,ilon_2,ivert,j,mm1,nn,ipe,ilon,mpi_string1
  integer(i_kind) isig,nlon_tot,i12,nlonh,nlatloc,ilonm

  allocate(nsend_sd2ns(npe),nrecv_sd2ns(npe),ndsend_sd2ns(npe+ione),ndrecv_sd2ns(npe+ione))
  mm1=mype+ione
  nlonh=nlon/2
  nlon_tot=nlonh*nlev

  nn=izero
  list2=izero
  do j=1,nlon_tot
     ilon_1=list_sd2ns(1,j)
     ilon_2=list_sd2ns(2,j)
     ivert=list_sd2ns(3,j)
     if(list2(ilon_1,ivert)/=izero.or.list2(ilon_2,ivert)/=izero) then
        if(mype==izero) write(0,*)' problem in cdiff_sd2ns1'
        call mpi_finalize(i)
        stop
     end if
     list2(ilon_1,ivert)=j
     list2(ilon_2,ivert)=j
  end do
  do ivert=1,nlev
     do ilon=1,nlon
        if(list2(ilon,ivert)==izero) then
           if(mype==izero) write(0,*)' problem in cdiff_sd2ns1'
           call mpi_finalize(i)
           stop
        end if
     end do
  end do

!  obtain counts of points to send to each pe from this pe

  nsend_sd2ns=izero
  nlatloc=lat2-2_i_kind
  do ivert=1,nlev
     do i=2,lon2-ione
        ilon=i+jstart(mm1)-2_i_kind
        j=list2(ilon,ivert)
        ipe=list_sd2ns(4,j)
        nsend_sd2ns(ipe)=nsend_sd2ns(ipe)+nlatloc
     end do
  end do

  ndsend_sd2ns(1)=izero
  do i=2,npe+ione
     ndsend_sd2ns(i)=ndsend_sd2ns(i-ione)+nsend_sd2ns(i-ione)
  end do
  nallsend_sd2ns=ndsend_sd2ns(npe+ione)
  allocate(info_send_sd2ns(4,nallsend_sd2ns))
  nsend_sd2ns=izero
  do ivert=1,nlev
     do i=2,lon2-ione
        ilon=i+jstart(mm1)-2_i_kind
        ilonm=list2(ilon,ivert)
        ilon_1=list_sd2ns(1,ilonm)
        ilon_2=list_sd2ns(2,ilonm)
        i12=izero
        if(ilon_1==ilon) i12=ione
        if(ilon_2==ilon) i12=2_i_kind
        isig =list_sd2ns(3,ilonm)
        ipe=list_sd2ns(4,ilonm)
        do ii=2,lat2-ione
           ilat=ii+istart(mm1)-2_i_kind
           nsend_sd2ns(ipe)=nsend_sd2ns(ipe)+ione
           ii0=ndsend_sd2ns(ipe)+nsend_sd2ns(ipe)
           info_send_sd2ns(1,ii0)=ilat
           info_send_sd2ns(2,ii0)=ilonm
           info_send_sd2ns(3,ii0)=i12
           info_send_sd2ns(4,ii0)=isig
        end do
     end do
  end do

  call mpi_alltoall(nsend_sd2ns,ione,mpi_integer4,nrecv_sd2ns,ione,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_sd2ns(1)=izero
  do i=2,npe+ione
     ndrecv_sd2ns(i)=ndrecv_sd2ns(i-ione)+nrecv_sd2ns(i-ione)
  end do
  nallrecv_sd2ns=ndrecv_sd2ns(npe+ione)
  allocate(info_recv_sd2ns(4,nallrecv_sd2ns))
  call mpi_type_contiguous(4_i_kind,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_sd2ns,nsend_sd2ns,ndsend_sd2ns,mpi_string1, &
                     info_recv_sd2ns,nrecv_sd2ns,ndrecv_sd2ns,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_sd2ns1

subroutine cdiff_ns2sd1(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ns2sd1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lon strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    mype       - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,nlon,jstart,istart,ilat1,jlon1
  use mpimod, only: npe,mpi_comm_world,ierror,mpi_integer4
  implicit none


  integer(i_kind),intent(in   ) :: mype

  integer(i_kind) i,i1,i2,ilat,ilon_1,ilon_2,ivert,j,k,mm1,ipe,mpi_string1,nn
  integer(i_kind) iloop

  allocate(nsend_ns2sd(npe),nrecv_ns2sd(npe),ndsend_ns2sd(npe+ione),ndrecv_ns2sd(npe+ione))
  mm1=mype+ione

!      1.  for each pe, gather up list of points from this set of lat strips destined
!             for subdomain of pe
  do ipe=1,npe
     nn=izero
     do k=nlon_0,nlon_1
        ilon_1=list_sd2ns(1,k)
        ilon_2=list_sd2ns(2,k)
        ivert=list_sd2ns(3,k)
        i1=ilon_1-jstart(ipe)+2_i_kind
        do iloop=-1,1
           if(i1+iloop*nlon>=ione.and.i1+iloop*nlon<=jlon1(ipe)+2_i_kind) then
              do j=1,ilat1(ipe)+2_i_kind
                 ilat=j+istart(ipe)-2_i_kind
                 if(ilat<ione.or.ilat>nlat) cycle
                 nn=nn+ione
              end do
           end if
        end do
        i2=ilon_2-jstart(ipe)+2_i_kind
        do iloop=-1,1
           if(i2+iloop*nlon>=ione.and.i2+iloop*nlon<=jlon1(ipe)+2_i_kind) then
              do j=1,ilat1(ipe)+2_i_kind
                 ilat=j+istart(ipe)-2_i_kind
                 if(ilat<ione.or.ilat>nlat) cycle
                 nn=nn+ione
              end do
           end if
        end do
     end do
     nsend_ns2sd(ipe)=nn
  end do

  ndsend_ns2sd(1)=izero
  do i=2,npe+ione
     ndsend_ns2sd(i)=ndsend_ns2sd(i-ione)+nsend_ns2sd(i-ione)
  end do
  nallsend_ns2sd=ndsend_ns2sd(npe+ione)
  allocate(info_send_ns2sd(4,nallsend_ns2sd))
  nn=izero
  do ipe=1,npe
     do k=nlon_0,nlon_1
        ilon_1=list_sd2ns(1,k)
        ilon_2=list_sd2ns(2,k)
        ivert=list_sd2ns(3,k)
        i1=ilon_1-jstart(ipe)+2_i_kind
        do iloop=-1,1
           if(i1+iloop*nlon>=ione.and.i1+iloop*nlon<=jlon1(ipe)+2_i_kind) then
              do j=1,ilat1(ipe)+2_i_kind
                 ilat=j+istart(ipe)-2_i_kind
                 if(ilat<ione.or.ilat>nlat) cycle
                 nn=nn+ione
                 info_send_ns2sd(1,nn)=ilat
                 info_send_ns2sd(2,nn)=i1+iloop*nlon
                 info_send_ns2sd(3,nn)=k
                 info_send_ns2sd(4,nn)=ione
              end do
           end if
        end do
        i2=ilon_2-jstart(ipe)+2_i_kind
        do iloop=-1,1
           if(i2+iloop*nlon>=ione.and.i2+iloop*nlon<=jlon1(ipe)+2_i_kind) then
              do j=1,ilat1(ipe)+2_i_kind
                 ilat=j+istart(ipe)-2_i_kind
                 if(ilat<ione.or.ilat>nlat) cycle
                 nn=nn+ione
                 info_send_ns2sd(1,nn)=ilat
                 info_send_ns2sd(2,nn)=i2+iloop*nlon
                 info_send_ns2sd(3,nn)=k
                 info_send_ns2sd(4,nn)=2_i_kind
              end do
           end if
        end do
     end do
  end do

  call mpi_alltoall(nsend_ns2sd,ione,mpi_integer4,nrecv_ns2sd,ione,mpi_integer4,mpi_comm_world,ierror)
  ndrecv_ns2sd(1)=izero
  do i=2,npe+ione
     ndrecv_ns2sd(i)=ndrecv_ns2sd(i-ione)+nrecv_ns2sd(i-ione)
  end do
  nallrecv_ns2sd=ndrecv_ns2sd(npe+ione)
  allocate(info_recv_ns2sd(4,nallrecv_ns2sd))
  call mpi_type_contiguous(4_i_kind,mpi_integer4,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(info_send_ns2sd,nsend_ns2sd,ndsend_ns2sd,mpi_string1, &
                     info_recv_ns2sd,nrecv_ns2sd,ndrecv_ns2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)

end subroutine cdiff_ns2sd1

subroutine cdiff_sd2ns(u_sd,u_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ns (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_sd
!
!   output argument list:
!    u_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: u_ns

  integer(i_kind) i12,ilat,ilonm,ilon,ivert,j,mm1
  real(r_kind),allocatable::sendbuf(:),recvbuf(:)

  mm1=mype+ione

  allocate(sendbuf(nallsend_sd2ns))
  do j=1,nallsend_sd2ns
     ilat=info_send_sd2ns(1,j)
     ilonm=info_send_sd2ns(2,j)
     i12=info_send_sd2ns(3,j)
     ilon=list_sd2ns(i12,ilonm)
     ivert=list_sd2ns(3,ilonm)
     sendbuf(j)=u_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(nallrecv_sd2ns))
  call mpi_alltoallv(sendbuf,nsend_sd2ns,ndsend_sd2ns,mpi_rtype, &
                     recvbuf,nrecv_sd2ns,ndrecv_sd2ns,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ns
     ilat=info_recv_sd2ns(1,j)
     ilonm=info_recv_sd2ns(2,j)
     i12=info_recv_sd2ns(3,j)
     u_ns(i12,ilat,ilonm)=recvbuf(j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ns

subroutine cdiff_sd2ns2(u1_sd,u2_sd,u1_ns,u2_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns2
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ns (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd
!
!   output argument list:
!    u1_ns,u2_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: u1_ns,u2_ns

  integer(i_kind) i12,ilat,ilonm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+ione

  allocate(sendbuf(2,nallsend_sd2ns))
  do j=1,nallsend_sd2ns
     ilat=info_send_sd2ns(1,j)
     ilonm=info_send_sd2ns(2,j)
     i12=info_send_sd2ns(3,j)
     ilon=list_sd2ns(i12,ilonm)
     ivert=list_sd2ns(3,ilonm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(2,nallrecv_sd2ns))
  call mpi_type_contiguous(2_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ns,ndsend_sd2ns,mpi_string1, &
                     recvbuf,nrecv_sd2ns,ndrecv_sd2ns,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ns
     ilat=info_recv_sd2ns(1,j)
     ilonm=info_recv_sd2ns(2,j)
     i12=info_recv_sd2ns(3,j)
     u1_ns(i12,ilat,ilonm)=recvbuf(1,j)
     u2_ns(i12,ilat,ilonm)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ns2

subroutine cdiff_sd2ns3(u1_sd,u2_sd,u3_sd,u1_ns,u2_ns,u3_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_sd2ns3
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_sd (subdomains) to u_ns (lat strips)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_sd,u2_sd,u3_sd
!
!   output argument list:
!    u1_ns,u2_ns,u3_ns
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,jstart,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none

  integer(i_kind)                             ,intent(in   ) :: nlev,mype

  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(in   ) :: u1_sd,u2_sd,u3_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: u1_ns,u2_ns,u3_ns

  integer(i_kind) i12,ilat,ilonm,ilon,ivert,j,mm1,mpi_string1
  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)

  mm1=mype+ione

  allocate(sendbuf(3,nallsend_sd2ns))
  do j=1,nallsend_sd2ns
     ilat=info_send_sd2ns(1,j)
     ilonm=info_send_sd2ns(2,j)
     i12=info_send_sd2ns(3,j)
     ilon=list_sd2ns(i12,ilonm)
     ivert=list_sd2ns(3,ilonm)
     sendbuf(1,j)=u1_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(2,j)=u2_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
     sendbuf(3,j)=u3_sd(ilat-istart(mm1)+2_i_kind,ilon-jstart(mm1)+2_i_kind,ivert)
  end do
  allocate(recvbuf(3,nallrecv_sd2ns))
  call mpi_type_contiguous(3_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_sd2ns,ndsend_sd2ns,mpi_string1, &
                     recvbuf,nrecv_sd2ns,ndrecv_sd2ns,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)

  do j=1,nallrecv_sd2ns
     ilat=info_recv_sd2ns(1,j)
     ilonm=info_recv_sd2ns(2,j)
     i12=info_recv_sd2ns(3,j)
     u1_ns(i12,ilat,ilonm)=recvbuf(1,j)
     u2_ns(i12,ilat,ilonm)=recvbuf(2,j)
     u3_ns(i12,ilat,ilonm)=recvbuf(3,j)
  end do
  deallocate(recvbuf)

end subroutine cdiff_sd2ns3

subroutine cdiff_ns2sd(u_sd,u_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ns2sd
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u_ns
!
!   output argument list:
!    u_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: u_ns

  real(r_kind),allocatable::sendbuf(:),recvbuf(:)
  integer(i_kind) i12,ilat,ivert,j,k,mm1,ilonm,ilonloc

  mm1=mype+ione

  allocate(sendbuf(nallsend_ns2sd))
  do j=1,nallsend_ns2sd
     ilat=info_send_ns2sd(1,j)
     ilonm=info_send_ns2sd(3,j)
     i12=info_send_ns2sd(4,j)
     sendbuf(j)=u_ns(i12,ilat,ilonm)
  end do
  allocate(recvbuf(nallrecv_ns2sd))
  call mpi_alltoallv(sendbuf,nsend_ns2sd,ndsend_ns2sd,mpi_rtype, &
                     recvbuf,nrecv_ns2sd,ndrecv_ns2sd,mpi_rtype,mpi_comm_world,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ns2sd
     ilat=info_recv_ns2sd(1,j)
     ilonloc=info_recv_ns2sd(2,j)
     ilonm=info_recv_ns2sd(3,j)
     ivert=list_sd2ns(3,ilonm)
     u_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(j)
  end do
  deallocate(recvbuf)

!-----------------do repeat rows for north/south pole
  if(nlat+ione-istart(mm1)+2_i_kind==lat2) then
     do k=1,nlev
        do j=1,lon2
           u_sd(lat2,j,k)=u_sd(lat2-ione,j,k)
        end do
     end do
  end if
  if(2_i_kind-istart(mm1)==ione) then
     do k=1,nlev
        do j=1,lon2
           u_sd(1,j,k)=u_sd(2,j,k)
        end do
     end do
  end if

end subroutine cdiff_ns2sd

subroutine cdiff_ns2sd2(u1_sd,u2_sd,u1_ns,u2_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_mp_compact_diffs1
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_ns,u2_ns
!
!   output argument list:
!    u1_sd,u2_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: u1_ns,u2_ns

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,k,mm1,ilonm,ilonloc,mpi_string1

  mm1=mype+ione

  allocate(sendbuf(2,nallsend_ns2sd))
  do j=1,nallsend_ns2sd
     ilat=info_send_ns2sd(1,j)
     ilonm=info_send_ns2sd(3,j)
     i12=info_send_ns2sd(4,j)
     sendbuf(1,j)=u1_ns(i12,ilat,ilonm)
     sendbuf(2,j)=u2_ns(i12,ilat,ilonm)
  end do
  allocate(recvbuf(2,nallrecv_ns2sd))
  call mpi_type_contiguous(2_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ns2sd,ndsend_ns2sd,mpi_string1, &
                     recvbuf,nrecv_ns2sd,ndrecv_ns2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ns2sd
     ilat=info_recv_ns2sd(1,j)
     ilonloc=info_recv_ns2sd(2,j)
     ilonm=info_recv_ns2sd(3,j)
     ivert=list_sd2ns(3,ilonm)
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
  end do
  deallocate(recvbuf)

!-----------------do repeat rows for north/south pole
  if(nlat+ione-istart(mm1)+2_i_kind==lat2) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(lat2,j,k)=u1_sd(lat2-ione,j,k)
           u2_sd(lat2,j,k)=u2_sd(lat2-ione,j,k)
        end do
     end do
  end if
  if(2_i_kind-istart(mm1)==ione) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(1,j,k)=u1_sd(2,j,k)
           u2_sd(1,j,k)=u2_sd(2,j,k)
        end do
     end do
  end if

end subroutine cdiff_ns2sd2

subroutine cdiff_ns2sd3(u1_sd,u2_sd,u3_sd,u1_ns,u2_ns,u3_ns,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    cdiff_ns2sd3
!   prgmmr:
!
! abstract: use mpi_alltoallv to move u_ns (lat strips) to u_sd (subdomains)
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    u1_ns,u2_ns,u3_ns
!
!   output argument list:
!    u1_sd,u2_sd,u3_sd
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use gridmod, only: nlat,lon2,lat2,istart
  use mpimod, only: mpi_comm_world,ierror,mpi_rtype
  implicit none


  integer(i_kind)                             ,intent(in   ) :: nlev,mype
  real(r_kind),dimension(lat2,lon2,nlev)      ,intent(  out) :: u1_sd,u2_sd,u3_sd
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: u1_ns,u2_ns,u3_ns

  real(r_kind),allocatable::sendbuf(:,:),recvbuf(:,:)
  integer(i_kind) i12,ilat,ivert,j,k,mm1,ilonm,ilonloc,mpi_string1

  mm1=mype+ione

  allocate(sendbuf(3,nallsend_ns2sd))
  do j=1,nallsend_ns2sd
     ilat=info_send_ns2sd(1,j)
     ilonm=info_send_ns2sd(3,j)
     i12=info_send_ns2sd(4,j)
     sendbuf(1,j)=u1_ns(i12,ilat,ilonm)
     sendbuf(2,j)=u2_ns(i12,ilat,ilonm)
     sendbuf(3,j)=u3_ns(i12,ilat,ilonm)
  end do
  allocate(recvbuf(3,nallrecv_ns2sd))
  call mpi_type_contiguous(3_i_kind,mpi_rtype,mpi_string1,ierror)
  call mpi_type_commit(mpi_string1,ierror)
  call mpi_alltoallv(sendbuf,nsend_ns2sd,ndsend_ns2sd,mpi_string1, &
                     recvbuf,nrecv_ns2sd,ndrecv_ns2sd,mpi_string1,mpi_comm_world,ierror)
  call mpi_type_free(mpi_string1,ierror)
  deallocate(sendbuf)
  do j=1,nallrecv_ns2sd
     ilat=info_recv_ns2sd(1,j)
     ilonloc=info_recv_ns2sd(2,j)
     ilonm=info_recv_ns2sd(3,j)
     ivert=list_sd2ns(3,ilonm)
     u1_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(1,j)
     u2_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(2,j)
     u3_sd(ilat-istart(mm1)+2_i_kind,ilonloc,ivert)=recvbuf(3,j)
  end do
  deallocate(recvbuf)

!-----------------do repeat rows for north/south pole
  if(nlat+ione-istart(mm1)+2_i_kind==lat2) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(lat2,j,k)=u1_sd(lat2-ione,j,k)
           u2_sd(lat2,j,k)=u2_sd(lat2-ione,j,k)
           u3_sd(lat2,j,k)=u3_sd(lat2-ione,j,k)
        end do
     end do
  end if
  if(2_i_kind-istart(mm1)==ione) then
     do k=1,nlev
        do j=1,lon2
           u1_sd(1,j,k)=u1_sd(2,j,k)
           u2_sd(1,j,k)=u2_sd(2,j,k)
           u3_sd(1,j,k)=u3_sd(2,j,k)
        end do
     end do
  end if

end subroutine cdiff_ns2sd3

subroutine mp_compact_dlon(b,dbdx,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!
!   output argument list:
!    dbdx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: b
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(  out) :: dbdx

  integer(i_kind) ny,nxh,nbp,nya,nxa
  integer(i_kind) lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1,lacoy2,lbcoy2,lcy
  integer(i_kind) ix,iy,k,i12,ilat
  real(r_kind),dimension(nlon):: work3,grid3,grid3pol
  real(r_kind) polu,polv

  ny=nlat-2_i_kind
  nxh=nlon/2
  nbp=2*noq+ione
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=ione
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-ione

!  outer loop over lat strips
  do k=nlat_0,nlat_1
     do i12=1,2
        ilat=list_sd2ew(i12,k)
        iy=ilat-ione
        if(iy>=ione.and.iy<=ny) then

! Initialize output arrays to zero
           do ix=1,nlon
              dbdx(i12,ix,k)=zero
           end do

! Transfer scaler input field to work array.
! Zero other work arrays.
           do ix=1,nlon
              work3(ix)=b(i12,ix,k)
              grid3(ix)=zero
           end do

! Compute x (east-west) derivatives on sphere
           call mp_xdcirdp(work3,grid3,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2), &
                        nlon,noq,nxh)

! Make corrections for convergence of meridians:
           do ix=1,nlon
              grid3(ix)=grid3(ix)*coef(lcy+iy)
           end do

           if(iy==ione.or.iy==ny) then
              if(.not.vector) then
                 polu=zero
                 polv=zero
                 do ix=1,nlon
                    polu=polu+grid3(ix)*coslon(ix)
                    polv=polv+grid3(ix)*sinlon(ix)
                 end do
                 polu=polu/float(nlon)
                 polv=polv/float(nlon)
                 do ix=1,nlon
                    grid3pol(ix)=polu*coslon(ix)+polv*sinlon(ix)
                 end do
              else
                 do ix=1,nlon
                    grid3pol(ix)=zero
                 end do
              end if
           end if

! Load result into output array
           do ix=1,nlon
              dbdx(i12,ix,k)=grid3(ix)
           end do

! Load pole row if we are adjacent to pole
           if(iy==ione) then
              do ix=1,nlon
                 dbdx(1,ix,k)=grid3pol(ix)
              end do
           else if(iy==ny) then
              do ix=1,nlon
                 dbdx(2,ix,k)=grid3pol(ix)
              end do
           end if

        end if

     end do
  end do

end subroutine mp_compact_dlon

subroutine mp_compact_dlon_ad(b,dbdx,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!    dbdx
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: b
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(in   ) :: dbdx

  integer(i_kind) ny,nxh,nbp,nya,nxa
  integer(i_kind) lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1,lacoy2,lbcoy2,lcy
  integer(i_kind) ix,iy,k,i12,ilat
  real(r_kind),dimension(nlon):: work3,grid3,grid3pol
  real(r_kind) polu,polv

  ny=nlat-2_i_kind
  nxh=nlon/2
  nbp=2*noq+ione
  nya=ny*nbp
  nxa=nxh*nbp

  lacox1=ione
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-ione


!  outer loop over lat strips
  do k=nlat_0,nlat_1
     do i12=1,2
        ilat=list_sd2ew(i12,k)
        iy=ilat-ione
        if(iy>=ione.and.iy<=ny) then

! adjoint of Load pole row if we are adjacent to pole
           if(iy==ione) then
              do ix=1,nlon
                 grid3pol(ix)=dbdx(1,ix,k)
              end do
           else if(iy==ny) then
              do ix=1,nlon
                 grid3pol(ix)=dbdx(2,ix,k)
              end do
           end if
 
! adjoint of Load result into output array
           do ix=1,nlon
              grid3(ix)=dbdx(i12,ix,k)
           end do
 
           if(iy==ione.or.iy==ny) then
              if(.not.vector) then
                 polu=zero
                 polv=zero
                 do ix=1,nlon
                    polu=polu+grid3pol(ix)*coslon(ix)
                    polv=polv+grid3pol(ix)*sinlon(ix)
                 end do
                 polu=polu/float(nlon)
                 polv=polv/float(nlon)
                 do ix=1,nlon
                    grid3(ix)=grid3(ix)+polu*coslon(ix)+polv*sinlon(ix)
                 end do
              else
                 do ix=1,nlon
                    grid3pol(ix)=zero
                 end do
              end if
           end if

! adjoint Make corrections for convergence of meridians:
           do ix=1,nlon
              grid3(ix)=grid3(ix)*coef(lcy+iy)
           end do

! adjoint Compute x (east-west) derivatives on sphere
           call mp_xdcirdp(grid3,work3,coef(lacox1),coef(lbcox1),coef(lacox2),coef(lbcox2), &
                        nlon,noq,nxh)

! Transfer scaler input field to work array.
! Zero other work arrays.
           do ix=1,nlon
!             NOTE:  Adjoint of first derivative is its negative
              b(i12,ix,k)=b(i12,ix,k)-work3(ix)
           end do
        end if

     end do
  end do

end subroutine mp_compact_dlon_ad

subroutine mp_compact_dlat(b,dbdy,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!
!   output argument list:
!    dbdy
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,half
  use gridmod, only: nlon,nlat
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) ::  b
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(  out) :: dbdy

  integer(i_kind) ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer(i_kind) lbcoy2,lacoy2,iy,i,lcy,k
  real(r_kind),dimension(2,nlat-2):: work2,grid4
  real(r_kind) grid4n,grid4s


! Set parameters for calls to subsequent routines
  ny=nlat-2_i_kind
  nxh=nlon/2
  nbp=2*noq+ione
  nya=ny*nbp
  nxa=nxh*nbp
  
  lacox1=ione
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-ione

!  outer loop over lon strips
  do k=nlon_0,nlon_1

! Initialize output arrays to zero
     do i=1,nlat
        dbdy(1,i,k)=zero
        dbdy(2,i,k)=zero
     end do

! Transfer scalar input field to work array.
! Zero other work arrays.
     do i=1,ny
        work2(1,i) = b(1,i+1,k)
        work2(2,i) = b(2,i+1,k)
        grid4(1,i)=zero
        grid4(2,i)=zero
     end do

     if(vector) then
!    multiply by cos(lat) ( 1/coef(lcy+iy) )
        do iy=1,ny
           work2(1,iy)=work2(1,iy)/coef(lcy+iy)
           work2(2,iy)=work2(2,iy)/coef(lcy+iy)
        enddo
     end if

! Compute y (south-north) derivatives on sphere
     call mp_ydsphdp(work2,grid4, &
          coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),ny,noq)

     if(vector) then
!  divide by cos(lat)
        do iy=1,ny
           grid4(1,iy)=grid4(1,iy)*coef(lcy+iy)
           grid4(2,iy)=grid4(2,iy)*coef(lcy+iy)
        enddo
        grid4n= zero
        grid4s= zero
     else
        grid4n=half*(grid4(1,ny)-grid4(2,ny))
        grid4s=half*(grid4(1, 1)-grid4(2, 1))
     end if

! Load result into output array
     dbdy(1,1,k)=grid4s
     dbdy(2,1,k)=-grid4s
     dbdy(1,nlat,k)=grid4n
     dbdy(2,nlat,k)=-grid4n
     do i=1,ny
        dbdy(1,i+ione,k) = grid4(1,i)
        dbdy(2,i+ione,k) = grid4(2,i)
     end do
  
  end do

end subroutine mp_compact_dlat

subroutine mp_compact_dlat_ad(b,dbdy,vector)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    vector
!    b
!    dbdy
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero,half
  use gridmod, only: nlon,nlat
  use compact_diffs, only: coef,noq
  implicit none

  logical                                     ,intent(in   ) :: vector
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(inout) :: b
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1),intent(in   ) :: dbdy

  integer(i_kind) ny,nxh,nbp,nya,nxa,lacox1,lbcox1,lacox2,lbcox2,lacoy1,lbcoy1
  integer(i_kind) lbcoy2,lacoy2,iy,i,lcy,k
  real(r_kind),dimension(2,nlat-2):: work2,grid4
  real(r_kind) grid4n,grid4s


! Set parameters for calls to subsequent routines
  ny=nlat-2_i_kind
  nxh=nlon/2
  nbp=2*noq+ione
  nya=ny*nbp
  nxa=nxh*nbp
  
  lacox1=ione
  lbcox1=lacox1+nxa
  lacox2=lbcox1+nxa
  lbcox2=lacox2+nxa
  lacoy1=lbcox2+nxa
  lbcoy1=lacoy1+nya
  lacoy2=lbcoy1+nya
  lbcoy2=lacoy2+nya
  lcy   =lbcoy2+nya-ione

!  outer loop over lon strips
  do k=nlon_0,nlon_1

! adjoint Load result into output array
     do i=1,ny
        grid4(1,i) = dbdy(1,i+ione,k)
        grid4(2,i) = dbdy(2,i+ione,k)
     end do
   ! grid4s=dbdy(1,1,k)-dbdy(2,1,k)
   ! grid4n=dbdy(1,nlat,k)-dbdy(2,nlat,k)
     grid4s=-dbdy(1,1,k)+dbdy(2,1,k)
     grid4n=-dbdy(1,nlat,k)+dbdy(2,nlat,k)

     if(vector) then
!  divide by cos(lat)
        grid4n= zero
        grid4s= zero
        do iy=1,ny
           grid4(1,iy)=grid4(1,iy)*coef(lcy+iy)
           grid4(2,iy)=grid4(2,iy)*coef(lcy+iy)
        enddo
     else
   !    grid4(1,ny)=grid4(1,ny)+half*grid4n
   !    grid4(2,ny)=grid4(2,ny)-half*grid4n
   !    grid4(1, 1)=grid4(1, 1)+half*grid4s
   !    grid4(2, 1)=grid4(2, 1)-half*grid4s
        grid4(1,ny)=grid4(1,ny)-half*grid4n
        grid4(2,ny)=grid4(2,ny)+half*grid4n
        grid4(1, 1)=grid4(1, 1)-half*grid4s
        grid4(2, 1)=grid4(2, 1)+half*grid4s
     end if

! adjoint Compute y (south-north) derivatives on sphere
     work2=zero
     call mp_tydsphdp(work2,grid4, &
          coef(lacoy1),coef(lbcoy1),coef(lacoy2),coef(lbcoy2),ny,noq)

     if(vector) then
!    multiply by cos(lat) ( 1/coef(lcy+iy) )
        do iy=1,ny
           work2(1,iy)=work2(1,iy)/coef(lcy+iy)
           work2(2,iy)=work2(2,iy)/coef(lcy+iy)
        enddo
     end if

! accumulate to output field
     do i=1,ny
        b(1,i+ione,k) = b(1,i+ione,k) - work2(1,i)
        b(2,i+ione,k) = b(2,i+ione,k) - work2(2,i)
     end do

  end do

end subroutine mp_compact_dlat_ad

subroutine mp_uv_pole(u,v)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_uv_pole
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    u,v
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  implicit none

  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: u,v

  integer(i_kind) ilat1,ilat2,ix,k
  real(r_kind) polnu,polnv,polsu,polsv


  do k=nlat_0,nlat_1
     ilat1=list_sd2ew(1,k)
     ilat2=list_sd2ew(2,k)
     if(ilat1==ione) then

!       do south pole
        polsu=zero
        polsv=zero
        do ix=1,nlon
           polsu=polsu+u(2,ix,k)*coslon(ix)+v(2,ix,k)*sinlon(ix)
           polsv=polsv+u(2,ix,k)*sinlon(ix)-v(2,ix,k)*coslon(ix)
        end do
        polsu=polsu/float(nlon)
        polsv=polsv/float(nlon)
        do ix=1,nlon
           u(1,ix,k)=polsu*coslon(ix)+polsv*sinlon(ix)
           v(1,ix,k)=polsu*sinlon(ix)-polsv*coslon(ix)
        end do

     else if(ilat2==nlat) then

!       do north pole
        polnu=zero
        polnv=zero
        do ix=1,nlon
           polnu=polnu+u(1,ix,k)*coslon(ix)-v(1,ix,k)*sinlon(ix)
           polnv=polnv+u(1,ix,k)*sinlon(ix)+v(1,ix,k)*coslon(ix)
        end do
        polnu=polnu/float(nlon)
        polnv=polnv/float(nlon)
        do ix=1,nlon
           u(2,ix,k)= polnu*coslon(ix)+polnv*sinlon(ix)
           v(2,ix,k)=-polnu*sinlon(ix)+polnv*coslon(ix)
        end do

     else
        cycle
     end if

  end do

end subroutine mp_uv_pole

subroutine mp_uv_pole_ad(u,v)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_uv_pole_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    u,v
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: zero
  use gridmod, only: nlon,nlat,sinlon,coslon
  implicit none

  real(r_kind),dimension(2,nlon,nlat_0:nlat_1),intent(inout) :: u,v

  integer(i_kind) ilat1,ilat2,ix,k
  real(r_kind) polnu,polnv,polsu,polsv


  do k=nlat_0,nlat_1
     ilat1=list_sd2ew(1,k)
     ilat2=list_sd2ew(2,k)
     if(ilat1==ione) then
 
!       do south pole
        polsu=zero
        polsv=zero
        do ix=1,nlon
           polsu=polsu+u(1,ix,k)*coslon(ix)+v(1,ix,k)*sinlon(ix)
           polsv=polsv+u(1,ix,k)*sinlon(ix)-v(1,ix,k)*coslon(ix)
           u(1,ix,k)=zero
           v(1,ix,k)=zero
        end do
        polsu=polsu/float(nlon)
        polsv=polsv/float(nlon)
        do ix=1,nlon
           u(2,ix,k)=u(2,ix,k)+polsu*coslon(ix)+polsv*sinlon(ix)
           v(2,ix,k)=v(2,ix,k)+polsu*sinlon(ix)-polsv*coslon(ix)
        end do

     else if(ilat2==nlat) then

!       do north pole
        polnu=zero
        polnv=zero
        do ix=1,nlon
           polnu=polnu+u(2,ix,k)*coslon(ix)-v(2,ix,k)*sinlon(ix)
           polnv=polnv+u(2,ix,k)*sinlon(ix)+v(2,ix,k)*coslon(ix)
           u(2,ix,k)=zero
           v(2,ix,k)=zero
        end do
        polnu=polnu/float(nlon)
        polnv=polnv/float(nlon)
        do ix=1,nlon
           u(1,ix,k)=u(1,ix,k)+polnu*coslon(ix)+polnv*sinlon(ix)
           v(1,ix,k)=v(1,ix,k)-polnu*sinlon(ix)+polnv*coslon(ix)
        end do

     else
        cycle
     end if

  end do

end subroutine mp_uv_pole_ad

end module mp_compact_diffs_mod2

subroutine mp_getuv2(u,v,st,vp,mype,nlev)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_getuv2
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    st,vp
!
!   output argument list:
!    u,v
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nlat,nlon
  use mp_compact_diffs_mod2, only: nlon_0,nlon_1,nlat_0,nlat_1,slow_pole, &
                          mp_compact_dlon,mp_compact_dlat,mp_uv_pole, &
                    cdiff_sd2ew2,cdiff_ew2sd2,cdiff_sd2ns2,cdiff_ns2sd2
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,nlev
  real(r_kind),dimension(lat2,lon2,nlev),intent(in   ) :: st,vp
  real(r_kind),dimension(lat2,lon2,nlev),intent(  out) :: u,v

! Declare local variables
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1):: st_ew,vp_ew,stx_ew,vpx_ew
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1):: st_ns,vp_ns,sty_ns,vpy_ns
  real(r_kind),dimension(lat2,lon2,nlev):: st_x,st_y,vp_x,vp_y


  st_ew=zero ; vp_ew=zero
  call cdiff_sd2ew2(st,vp,st_ew,vp_ew,nlev,mype)
  st_ns=zero ; vp_ns=zero
  call cdiff_sd2ns2(st,vp,st_ns,vp_ns,nlev,mype)

  stx_ew=zero
  call mp_compact_dlon(st_ew,stx_ew,.false.)
  vpx_ew=zero
  call mp_compact_dlon(vp_ew,vpx_ew,.false.)
  sty_ns=zero
  call mp_compact_dlat(st_ns,sty_ns,.false.)
  vpy_ns=zero
  call mp_compact_dlat(vp_ns,vpy_ns,.false.)
  vp_x=zero ; st_x=zero
  call cdiff_ew2sd2(vp_x,st_x,vpx_ew,stx_ew,nlev,mype)
  st_y=zero ; vp_y=zero
  call cdiff_ns2sd2(st_y,vp_y,sty_ns,vpy_ns,nlev,mype)
  u=vp_x-st_y
  v=vp_y+st_x
  if(slow_pole) then
     st_ew=zero
     vp_ew=zero
     call cdiff_sd2ew2(u,v,st_ew,vp_ew,nlev,mype)
     call mp_uv_pole(st_ew,vp_ew)
     u=zero
     v=zero
     call cdiff_ew2sd2(u,v,st_ew,vp_ew,nlev,mype)
  end if

end subroutine mp_getuv2

subroutine mp_compact_dlon2(b,dbdx,vector,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon2
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    b
!    vector
!
!   output argument list:
!    dbdx
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nlon
  use mp_compact_diffs_mod2, only: nlat_0,nlat_1,mp_compact_dlon, &
                    cdiff_sd2ew,cdiff_ew2sd
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,nlev
  real(r_kind),dimension(lat2,lon2,nlev),intent(in   ) :: b
  real(r_kind),dimension(lat2,lon2,nlev),intent(  out) :: dbdx
  logical                               ,intent(in   ) :: vector

! Declare local variables
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1):: b_ew,bx_ew


  b_ew=zero
  call cdiff_sd2ew(b,b_ew,nlev,mype)

  bx_ew=zero
  call mp_compact_dlon(b_ew,bx_ew,vector)
  dbdx=zero
  call cdiff_ew2sd(dbdx,bx_ew,nlev,mype)

end subroutine mp_compact_dlon2

subroutine mp_compact_dlon2_ad(b,dbdx,vector,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlon2_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    b
!    dbdx
!    vector
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nlon
  use mp_compact_diffs_mod2, only: nlat_0,nlat_1,mp_compact_dlon_ad, &
                    cdiff_sd2ew,cdiff_ew2sd
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,nlev
  real(r_kind),dimension(lat2,lon2,nlev),intent(inout) :: b
  real(r_kind),dimension(lat2,lon2,nlev),intent(in   ) :: dbdx
  logical                               ,intent(in   ) :: vector

! Declare local variables
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1):: b_ew,bx_ew
  real(r_kind),dimension(lat2,lon2,nlev):: btemp

  bx_ew=zero
  call cdiff_sd2ew(dbdx,bx_ew,nlev,mype)
  b_ew=zero
  call mp_compact_dlon_ad(b_ew,bx_ew,vector)
  btemp=zero
  call cdiff_ew2sd(btemp,b_ew,nlev,mype)
  b=b+btemp

end subroutine mp_compact_dlon2_ad

subroutine mp_compact_dlat2(b,dbdy,vector,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat2
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    b
!    vector
!
!   output argument list:
!    dbdy
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nlat,nlon
  use mp_compact_diffs_mod2, only: nlon_0,nlon_1,nlat_0,nlat_1,mp_compact_dlat,slow_pole, &
                    cdiff_sd2ns,cdiff_ns2sd,cdiff_sd2ew,cdiff_ew2sd,mp_uv_pole
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,nlev
  real(r_kind),dimension(lat2,lon2,nlev),intent(in   ) :: b
  real(r_kind),dimension(lat2,lon2,nlev),intent(  out) :: dbdy
  logical                               ,intent(in   ) :: vector

! Declare local variables
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1):: b_ns,by_ns
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1):: by_ew,bx_ew


  b_ns=zero
  call cdiff_sd2ns(b,b_ns,nlev,mype)

  by_ns=zero
  call mp_compact_dlat(b_ns,by_ns,vector)
  dbdy=zero
  call cdiff_ns2sd(dbdy,by_ns,nlev,mype)
  if(.not.vector.and.slow_pole) then
     by_ew=zero
     call cdiff_sd2ew(dbdy,by_ew,nlev,mype)
     bx_ew=zero
     call mp_uv_pole(bx_ew,by_ew)
     dbdy=zero
     call cdiff_ew2sd(dbdy,by_ew,nlev,mype)
  end if

end subroutine mp_compact_dlat2

subroutine mp_compact_dlat2_ad(b,dbdy,vector,nlev,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mp_compact_dlat2_ad
!   prgmmr:
!
! abstract:
!
! program history list:
!   2009-08-06  lueken - added subprogram doc block
!
!   input argument list:
!    nlev
!    mype       - mpi task id
!    b
!    dbdy
!    vector
!
!   output argument list:
!    b
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2,nlat,nlon
  use mp_compact_diffs_mod2, only: nlon_0,nlon_1,nlat_0,nlat_1,mp_compact_dlat_ad, &
                    cdiff_sd2ns,cdiff_ns2sd,mp_uv_pole_ad,cdiff_sd2ew,cdiff_ew2sd,slow_pole
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: mype,nlev
  real(r_kind),dimension(lat2,lon2,nlev),intent(inout) :: b
  real(r_kind),dimension(lat2,lon2,nlev),intent(in   ) :: dbdy
  logical                               ,intent(in   ) :: vector

! Declare local variables
  real(r_kind),dimension(2,nlat,nlon_0:nlon_1):: b_ns,by_ns
  real(r_kind),dimension(2,nlon,nlat_0:nlat_1):: bx_ew,by_ew
  real(r_kind),dimension(lat2,lon2,nlev):: btemp

  if(.not.vector.and.slow_pole) then
     by_ew=zero
     call cdiff_sd2ew(dbdy,by_ew,nlev,mype)
     bx_ew=zero
     call mp_uv_pole_ad(bx_ew,by_ew)
     btemp=zero
     call cdiff_ew2sd(btemp,by_ew,nlev,mype)
  else
     btemp=dbdy
  end if
  by_ns=zero
  call cdiff_sd2ns(btemp,by_ns,nlev,mype)
  b_ns=zero
  call mp_compact_dlat_ad(b_ns,by_ns,vector)
  btemp=zero
  call cdiff_ns2sd(btemp,b_ns,nlev,mype)
  b=b+btemp

end subroutine mp_compact_dlat2_ad
