 program gausslat_driver

!--------------------------------------------------------------
! test iplib routine gausslat. 
!
! call routine gausslat to calculate the gaussian latitudes for
! a t382 grid.  output is piped to standard output.
!--------------------------------------------------------------

 implicit none

 integer :: j, jmax
        real slat(584), wlat(584)
! real, allocatable :: slat(:), wlat(:)

 jmax = 584  ! t382 grid

! allocate (slat(jmax))
! allocate (wlat(jmax))
!        real slat(384), wlat(384)
 print*,'CALL ROUTINE GAUSSLAT'

 call gausslat(jmax,slat,wlat)

 do j = 1, jmax
   print*,'J/SLAT/WLAT ',j, slat(j), wlat(j)
 enddo

! deallocate (slat, wlat)

 print*,'NORMAL TERMINATION'

 end program gausslat_driver
