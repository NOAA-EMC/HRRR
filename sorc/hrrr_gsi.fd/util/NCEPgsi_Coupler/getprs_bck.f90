subroutine getprs_bck(ps,tv,prs)
! subprogram:    getprs       get 3d pressure or 3d pressure deriv
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: calculate 3d pressure and its horizontal derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm/RS6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: ione,zero,half,one_tenth,rd_over_cp,one
  use gridmod,only: nsig,lat2,lon2,ak5,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta1_ll,eta2_ll,pdtop_ll,pt_ll,&
       regional,wrf_mass_regional,twodvar_regional

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: tv
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,trk
  integer(i_kind) i,j,k,k2  

! Declare local parameter
  real(r_kind),parameter:: ten = 10.0_r_kind

  kapr=one/rd_over_cp
  prs=zero 

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=one_tenth* &
                      (eta1_ll(k)*pdtop_ll + &
                      eta2_ll(k)*(ten*ps(i,j)-pdtop_ll-pt_ll) + &
                      pt_ll)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=one_tenth*(eta1_ll(k)*(ten*ps(i,j)-pt_ll) + pt_ll)
              end do
           end do
        end do
     endif
  else
     k=ione
     k2=nsig+ione
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3_i_kind) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=ak5(k)+bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 trk=(half*(tv(i,j,k-ione)+tv(i,j,k))/tref5(k))**kapr
                 prs(i,j,k)=ak5(k)+(bk5(k)*ps(i,j))+(ck5(k)*trk)
              end do
           end do
        end do
     end if
  end if

  return
end subroutine getprs_bck


subroutine getprs_horiz_bck(ps_x,ps_y,prs,prs_x,prs_y)
!$$$  subprogram docuentation block
!                .     .    .                     .
! subprogram:    getprs_horiz
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete documentation block, rm unused var k2
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$
  
  use kinds,only: r_kind,i_kind
  use constants,only: ione,zero
  use gridmod,only: nsig,lat2,lon2,nlat,nlon
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll
  use mpimod, only: nvarbal_id,nnnvsbal
  use compact_diffs, only: compact_dlat,compact_dlon
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: prs
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k,iflg
  real(r_kind),dimension(lat2,lon2,nsig):: st,vp,t
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork,hwork_x,hwork_y


  if(regional)then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
                 prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
              end do
           end do
        end do
     else
        prs_x=zero ; prs_y=zero
     end if
  else
     iflg=ione
     st=zero
     vp=zero
     t=zero
     hwork=zero
     call sub2grid2(hwork,st,vp,prs,t,iflg)
     do k=1,nnnvsbal
        if(nvarbal_id(k) == 3_i_kind)then
           call compact_dlon(hwork(1,1,k),hwork_x(1,1,k),.false.)
           call compact_dlat(hwork(1,1,k),hwork_y(1,1,k),.false.)
        end if
     end do
     call grid2sub2(hwork_x,st,vp,prs_x,t)
     call grid2sub2(hwork_y,st,vp,prs_y,t)
  end if

  return
end subroutine getprs_horiz_bck


 subroutine getprs_bck_tl(ps,t,prs)

!$$$ subprogram documentation block
!               .      .    .                       .
! subprogram:    getprs_tl    TLM of getprs
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: TLM of routine that gets 3d pressure and its derivatives
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code;
!                       - fix buf for t dimension
!   2008-06-04  safford - complete doc block, rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     ps       - surface pressure
!
!   output argument list:
!     prs        - 3d pressure
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  
  use kinds,only: r_kind,i_kind
  use constants,only: ione,zero,one,rd_over_cp,half
  use gridmod,only: nsig,lat2,lon2,bk5,ck5,idvc5,tref5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta2_ll,eta1_ll,regional,wrf_mass_regional,&
       twodvar_regional
  use nonlinmod, only: bck_tv
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ) :: t
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: prs

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  integer(i_kind) i,j,k,k2,it

  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=eta2_ll(k)*ps(i,j)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=eta1_ll(k)*ps(i,j)
              end do
           end do
        end do
     endif
  else
     k=ione
     k2=nsig+ione
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=ps(i,j)
           prs(i,j,k2)=zero
        end do
     end do
     if (idvc5 /= 3_i_kind) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 prs(i,j,k)=bk5(k)*ps(i,j)
              end do
           end do
        end do
     else
        kapr=one/rd_over_cp
        kaprm1=kapr-one
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 t9trm=half*(bck_tv(i,j,k-ione)+bck_tv(i,j,k))/tref5(k)
                 tc1=half/tref5(k)
                 trk=kapr*tc1*(t(i,j,k-ione)+t(i,j,k))*(t9trm**kaprm1)
                 prs(i,j,k)=bk5(k)*ps(i,j) + ck5(k)*trk
              end do
           end do
        end do
     end if
  end if

  return
end subroutine getprs_bck_tl


subroutine getprs_horiz_bck_tl(ps_x,ps_y,prs,prs_x,prs_y)
!$$$ subprogram documentation block
!               .      .    .                     .
! subprogram:   getprs_horiz_tl
!
!  prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete doc block
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     prs      - 3d pressure
!     ps_x     - dps/dx
!     ps_y     - dps/dy
!
!   output argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
! attributes:
!   language:  f90
!   machine    ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: ione,zero
  use gridmod,only: nsig,lat2,lon2,nlat,nlon
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll
  use mpimod, only: nvarbal_id,nnnvsbal
  use compact_diffs, only: compact_dlat,compact_dlon
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: ps_x,ps_y
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: prs
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(  out) :: prs_x,prs_y

! Declare local variables
  integer(i_kind) i,j,k,iflg
  real(r_kind),dimension(lat2,lon2,nsig):: st,vp,t
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork,hwork_x,hwork_y


  if(regional)then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 prs_x(i,j,k)=eta2_ll(k)*ps_x(i,j)
                 prs_y(i,j,k)=eta2_ll(k)*ps_y(i,j)
              end do
           end do
        end do
     else
        prs_x=zero
        prs_y=zero
     end if
  else
     iflg=ione
     st=zero
     vp=zero
     t=zero
     call sub2grid2(hwork,st,vp,prs,t,iflg)
     do k=1,nnnvsbal
        if(nvarbal_id(k) == 3_i_kind)then
           call compact_dlon(hwork(1,1,k),hwork_x(1,1,k),.false.)
           call compact_dlat(hwork(1,1,k),hwork_y(1,1,k),.false.)
        end if
     end do
     call grid2sub2(hwork_x,st,vp,prs_x,t)
     call grid2sub2(hwork_y,st,vp,prs_y,t)
  endif

  return
end subroutine getprs_horiz_bck_tl


subroutine getprs_bck_ad(ps,t,prs)
!$$$ subprogram documentation block
!               .      .    .                    .
! subprogram:    getprs_ad    adjoint of getprs_tl
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: adjoint of linear routine that gets 3d pressure and derivs
!
! program history log:
!   2005-09-29  kleist
!   2006-04-12  treadon - replace sigi with bk5
!   2006-07-31  kleist  - analysis variable change from ln(ps) to ps
!   2007-05-08  kleist  - add generalized vert coord and derivative call
!   2007-07-26  cucurull- compute 3d pressure and derivatives in different subroutines
!                       - remove gues_tv from argument list; clean up code
!   2008-06-04  safford - complete doc block, rm unused uses
!   2008-09-05  lueken  - merged ed's changes into q1fy09 code
!
!   input argument list:
!     prs        - 3d pressure
!
!   output argument list:
!     ps       - log surface pressure
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  
  use kinds,only: r_kind,i_kind
  use gridmod,only: nsig,lat2,lon2,bk5,ck5,tref5,idvc5
  use gridmod,only: wrf_nmm_regional,nems_nmmb_regional,eta2_ll,regional,wrf_mass_regional,eta1_ll,&
       twodvar_regional
  use nonlinmod, only: bck_tv
  use constants,only: ione,zero,half,one,rd_over_cp

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(inout) :: prs
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(inout) :: t
  real(r_kind),dimension(lat2,lon2)          ,intent(inout) :: ps

! Declare local variables
  real(r_kind) kapr,kaprm1,trk,tc1,t9trm
  integer(i_kind) i,j,k


  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + eta2_ll(k)*prs(i,j,k)
              end do
           end do
        end do
     elseif (wrf_mass_regional .or. twodvar_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + eta1_ll(k)*prs(i,j,k)
              end do
           end do
        end do
     endif
  else
     if (idvc5 /= 3_i_kind) then
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
              end do
           end do
        end do
     else
        kapr=one/rd_over_cp
        kaprm1=kapr-one
        do k=2,nsig
           do j=1,lon2
              do i=1,lat2
                 t9trm=half*(bck_tv(i,j,k-ione)+bck_tv(i,j,k))/tref5(k)
                 tc1=half/tref5(k)
                 ps(i,j) = ps(i,j) + bk5(k)*prs(i,j,k)
                 trk = ck5(k)*prs(i,j,k)
                 t(i,j,k-ione) = t(i,j,k-ione) + kapr*tc1*trk*(t9trm**kaprm1)
                 t(i,j,k     ) = t(i,j,k     ) + kapr*tc1*trk*(t9trm**kaprm1)
              end do
           end do
        end do
     end if
     k=ione
     do j=1,lon2
        do i=1,lat2
           ps(i,j)=ps(i,j) + prs(i,j,k)
        end do
     end do
  end if

  do k=1,nsig+ione
     do j=1,lon2
        do i=1,lat2
           prs(i,j,k)=zero
        end do
     end do
  end do 
 
  return
end subroutine getprs_bck_ad


subroutine getprs_horiz_bck_ad(ps_x,ps_y,prs,prs_x,prs_y)
!$$$ subprogram documentation block
!               .      .    .              .
! subprogram:  getprs_horiz_ad
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-06-04  safford - complete doc block
!   2008-09-05  lueken  - merged ed's changes into q1fy09
!
!   input argument list:
!     prs_x      - dp/dx
!     prs_y      - dp/dy
!
!   output argument list:
!     prs      - 3d pressure
!     ps_x     - d(ln(ps))/dx
!     ps_y     - d(ln(ps))/dy
!
!  notes:
!     Adjoint check performed and verified on 2005-08-29 by d. kleist
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds,only: r_kind,i_kind
  use constants,only: ione,zero
  use gridmod,only: nsig,lat2,lon2,nlat,nlon
  use gridmod,only: regional,wrf_nmm_regional,nems_nmmb_regional,eta2_ll
  use mpimod, only: nvarbal_id,nnnvsbal
  use compact_diffs, only: tcompact_dlat,tcompact_dlon

  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: prs_x,prs_y
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(inout) :: prs
  real(r_kind),dimension(lat2,lon2)          ,intent(inout) :: ps_x,ps_y

! Declare local variables
  integer(i_kind) i,j,k,iflg
  real(r_kind),dimension(lat2,lon2,nsig):: st,vp,t
  real(r_kind),dimension(nlat,nlon,nnnvsbal):: hwork,hwork_x,hwork_y

! Adjoint of horizontal derivatives
  if (regional) then
     if(wrf_nmm_regional.or.nems_nmmb_regional) then
        do k=1,nsig+ione
           do j=1,lon2
              do i=1,lat2
                 ps_y(i,j) = ps_y(i,j) + eta2_ll(k)*prs_y(i,j,k)
                 ps_x(i,j) = ps_x(i,j) + eta2_ll(k)*prs_x(i,j,k)
              end do
           end do
        end do
     end if
  else
     iflg=ione
     st=zero
     vp=zero
     t=zero
     hwork=zero
     call sub2grid2(hwork_x,st,vp,prs_x,t,iflg)
     call sub2grid2(hwork_y,st,vp,prs_y,t,iflg)
     do k=1,nnnvsbal
        if(nvarbal_id(k) == 3_i_kind)then
           call tcompact_dlon(hwork(1,1,k),hwork_x(1,1,k),.false.)
           call tcompact_dlat(hwork(1,1,k),hwork_y(1,1,k),.false.)
        end if
     end do
     call grid2sub2(hwork,st,vp,prs_x,t)
     do k=1,nsig+ione
        do j=1,lon2
           do i=1,lat2
              prs(i,j,k)=prs(i,j,k)+prs_x(i,j,k)
           end do
        end do
     end do
  end if


  return
end subroutine getprs_horiz_bck_ad
