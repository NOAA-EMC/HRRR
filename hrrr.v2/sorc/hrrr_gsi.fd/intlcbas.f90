module intlcbasmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intlcbasmod    module for intlcbas 
!   prgmmr:
!
! abstract: module for intlcbas 
!
! program history log:
!
! subroutines included:
!   sub intlcbas
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intlcbas

contains

subroutine intlcbas(lcbashead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlcbas      apply nonlin qc obs operator for conv. lcbas
!   prgmmr: zhu           org: np23                date: 2012-01-20
!
! abstract: apply observation operator and adjoint for conventional lcbas
!           observations with nonlinear qc operator
!
! program history log:
!
!   input argument list:
!     lcbashead
!     slcbas    - increment in grid space
!     rlcbas
!
!   output argument list:
!     rlcbas    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: lcbas_ob_type, lsaveobsens, l_do_adjoint
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(lcbas_ob_type),pointer,intent(in ) :: lcbashead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_lcbas,p0,grad,wnotgross,wgross,pg_lcbas
  real(r_kind),pointer,dimension(:) :: slcbas
  real(r_kind),pointer,dimension(:) :: rlcbas
  type(lcbas_ob_type), pointer :: lcbasptr

! If no lcbas data return
  if(.not. associated(lcbashead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'lcbas',slcbas,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'lcbas',rlcbas,istatus);ier=istatus+ier
  if(ier/=0)return

  lcbasptr => lcbashead
  do while (associated(lcbasptr))
     j1=lcbasptr%ij(1)
     j2=lcbasptr%ij(2)
     j3=lcbasptr%ij(3)
     j4=lcbasptr%ij(4)
     w1=lcbasptr%wij(1)
     w2=lcbasptr%wij(2)
     w3=lcbasptr%wij(3)
     w4=lcbasptr%wij(4)

!    Forward model
     val=w1*slcbas(j1)+w2*slcbas(j2)&
        +w3*slcbas(j3)+w4*slcbas(j4)

     if (lsaveobsens) then
        lcbasptr%diags%obssen(jiter) = val*lcbasptr%raterr2*lcbasptr%err2
     else
        if (lcbasptr%luse) lcbasptr%diags%tldepart(jiter)=val
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           grad = lcbasptr%diags%obssen(jiter)
 
        else
           val=val-lcbasptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. lcbasptr%pg > tiny_r_kind .and. &
                                lcbasptr%b  > tiny_r_kind) then
              pg_lcbas=lcbasptr%pg*varqc_iter
              cg_lcbas=cg_term/lcbasptr%b
              wnotgross= one-pg_lcbas
              wgross = pg_lcbas*cg_lcbas/wnotgross
              p0   = wgross/(wgross+exp(-half*lcbasptr%err2*val**2))
              val = val*(one-p0)
           endif

           grad = val*lcbasptr%raterr2*lcbasptr%err2
        endif

!       Adjoint
        rlcbas(j1)=rlcbas(j1)+w1*grad
        rlcbas(j2)=rlcbas(j2)+w2*grad
        rlcbas(j3)=rlcbas(j3)+w3*grad
        rlcbas(j4)=rlcbas(j4)+w4*grad
     endif

     lcbasptr => lcbasptr%llpoint

  end do

  return
end subroutine intlcbas

end module intlcbasmod
