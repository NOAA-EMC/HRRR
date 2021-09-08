module da_module_couple_uv
contains
subroutine da_couple_uv (u, v, mu, mub, msfu, msfv, c1h, c2h, ids, ide, jds, jde, kds, kde)
   implicit none
   integer, intent(in) :: ids, ide, jds, jde, kds, kde
   real, intent(inout) :: u(ids:ide+1,jds:jde,kds:kde)
   real, intent(inout) :: v(ids:ide,jds:jde+1,kds:kde)
   real, intent(in) :: msfu(ids:ide+1,jds:jde)
   real, intent(in) :: msfv(ids:ide,jds:jde+1)
   real, intent(in) :: mu(ids:ide,jds:jde)
   real, intent(in) :: mub(ids:ide,jds:jde)
   real,dimension(kds:kde),intent(in) :: c1h, c2h
   real, allocatable :: muu(:,:), muv(:,:)
   ! update_bc
   ! if (trace_use) call da_trace_entry("da_couple_uv")
   allocate(muu(ids:ide+1, jds:jde ))
   allocate(muv(ids:ide , jds:jde+1))
   ! couple variables u, v
   call da_calc_mu_uv (mu, mub, muu, muv, ids, ide, jds, jde)
   call da_couple (muu, u, msfu, c1h, c2h, ids, ide+1, jds, jde, kds, kde)
   call da_couple (muv, v, msfv, c1h, c2h, ids, ide, jds, jde+1, kds, kde)
   deallocate(muu)
   deallocate(muv)
   ! if (trace_use) call da_trace_exit("da_couple_uv")
end subroutine da_couple_uv
subroutine da_calc_mu_uv (mu, mub, muu, muv, &
                        ids, ide, jds, jde)
   implicit none
   integer, intent(in) :: ids, ide, jds, jde
   real, dimension(ids:ide, jds:jde), intent(in ) :: mu, mub
   real, dimension(ids:ide+1, jds:jde ), intent( out) :: muu
   real, dimension(ids:ide , jds:jde+1), intent( out) :: muv
   real, dimension(ids-1:ide+1, jds-1:jde+1) :: mut
   integer :: i, j
   do j=jds,jde
      do i=ids,ide
         mut(i,j) = mu(i,j)+mub(i,j)
      end do
      mut(ids-1,j) = mut(ids,j)
      mut(ide+1,j) = mut(ide,j)
   end do
   do i=ids-1,ide+1
      mut(i,jds-1)=mut(i,jds)
      mut(i,jde+1)=mut(i,jde)
   end do
   do j=jds,jde
      do i=ids,ide+1
         muu(i,j) = 0.5*(mut(i,j)+mut(i-1,j))
      end do
   end do
   do j=jds,jde+1
      do i=ids,ide
         muv(i,j) = 0.5*(mut(i,j)+mut(i,j-1))
      end do
   end do
end subroutine da_calc_mu_uv
subroutine da_couple (mut, field, msf, c1h, c2h, ids, ide, jds, jde, kds, kde)
   implicit none
   integer, intent(in) :: ids, ide, jds, jde, kds, kde
   real,dimension(ids:ide, jds:jde),intent(in) :: mut
   real, intent(in) :: msf(ids:ide, jds:jde)
   real,dimension(kds:kde),intent(in) :: c1h, c2h
   real, intent(inout) :: field(ids:ide, jds:jde, kds:kde)
   integer :: i, j, k
   ! update_bc
   ! if (trace_use) call da_trace_entry("da_couple")
   do j=jds,jde
      do k=kds,kde
         do i=ids,ide
            field(i,j,k)=field(i,j,k)*(c1h(k)*mut(i,j)+c2h(k))/msf(i,j)
         end do
      end do
   end do
   ! if (trace_use) call da_trace_exit("da_couple")
end subroutine da_couple
end module da_module_couple_uv
