module stppsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppsmod    module for stpps and its tangent linear stpps_tl
!  prgmmr:
!
! abstract: module for stpps and its tangent linear stpps_tl
!
! program history log:
!   2005-05-18  Yanqiu zhu - wrap stpps and its tangent linear stpps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpps_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2013-10-28  todling - reame p3d to prse
!
! subroutines included:
!   sub stpps
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpps

contains

subroutine stpps(pshead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpps       calculate penalty and contribution to
!                             stepsize for sfcp, using nonlinear qc
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize for
!           surface pressure with nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1997-12-14  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpps and stpps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-18  derber  - modify to output of b1 and b3
!   2007-02-15  rancic  - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling  - update to use gsi_bundlemod
!
!   input argument list:
!     pshead
!     rp       - search direction for ps
!     sp       - analysis increment for ps
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsize estimates  (==0 means use outer iteration values)
!                                         
!   output argument list:         
!     out(1:nstep)   - contribution to penalty for surface pressure - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: ps_ob_type
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gridmod, only: latlon1n1
  use jfunc, only: l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(ps_ob_type),pointer            ,intent(in   ) :: pshead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,kk,ier,istatus
  real(r_kind) val,val2,w1,w2,w3,w4,time_ps
  real(r_kind) cg_ps,ps,wgross,wnotgross,ps_pg
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: xhat_dt_prse
  real(r_kind),pointer,dimension(:) :: dhat_dt_prse
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(ps_ob_type), pointer :: psptr

  out=zero_quad

!  If no ps data return
  if(.not. associated(pshead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'prse',xhat_dt_prse,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'prse',dhat_dt_prse,istatus);ier=istatus+ier
  endif
  if(ier/=0)return

  psptr => pshead
  do while (associated(psptr))
     if(psptr%luse)then
        if(nstep > 0)then
           j1 = psptr%ij(1)
           j2 = psptr%ij(2)
           j3 = psptr%ij(3)
           j4 = psptr%ij(4)
           w1 = psptr%wij(1)
           w2 = psptr%wij(2)
           w3 = psptr%wij(3)
           w4 = psptr%wij(4)
           val =w1* rp(j1)+w2* rp(j2)+w3* rp(j3)+w4* rp(j4)
           val2=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)-psptr%res
           if(l_foto) then
              time_ps = psptr%time*r3600
              val =val +(w1*dhat_dt_prse(j1)+w2*dhat_dt_prse(j2)+ &
                         w3*dhat_dt_prse(j3)+w4*dhat_dt_prse(j4))*time_ps
              val2=val2+(w1*xhat_dt_prse(j1)+w2*xhat_dt_prse(j2)+ &
                         w3*xhat_dt_prse(j3)+w4*xhat_dt_prse(j4))*time_ps
           end if
           do kk=1,nstep
              ps=val2+sges(kk)*val
              pen(kk)=ps*ps*psptr%err2
           end do
        else
           pen(1)=psptr%res*psptr%res*psptr%err2
        end if

!  Modify penalty term if nonlinear QC

        if (nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                             psptr%b  > tiny_r_kind) then
           ps_pg=psptr%pg*varqc_iter
           cg_ps=cg_term/psptr%b
           wnotgross= one-ps_pg
           wgross =ps_pg*cg_ps/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
           end do
        endif
     
        out(1) = out(1)+pen(1)*psptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*psptr%raterr2
        end do
     end if

     psptr => psptr%llpoint
  end do
  
  return
end subroutine stpps

end module stppsmod
