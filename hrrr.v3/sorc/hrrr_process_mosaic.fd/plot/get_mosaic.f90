PROGRAM mosaic_max
 
   integer :: nx, ny, nz
   real, allocatable :: ref0(:,:,:)
   real, allocatable :: maxref(:,:)
   real*8, allocatable :: ref3d_column(:,:)


   write(*,*) 'open'
   OPEN(12, &
file='/scratch3/BMC/wrfruc/mhu/rapcode/GSI_r1216/data_process/mosaic/test/RefInGSI.dat', form='unformatted' )
   write(*,*) 'read'
   read(12) maxlvl,nlon,nlat,numref
   write(*,*) 'maxlvl=',maxlvl
   write(*,*) 'nlon=',nlon
   write(*,*) 'nlat=',nlat
   write(*,*) 'numref=',numref

   allocate (ref0(nlon,nlat,maxlvl))
   allocate (ref3d_column(maxlvl+2,numref))
   read(12) ref3d_column
   close(12)

   ref0=-999.0
   do n=1,numref
     i=int(ref3d_column(1,n))
     j=int(ref3d_column(2,n))
     do k=1,maxlvl
       ref0(i,j,k)=ref3d_column(k+2,n)
     enddo
   enddo

   allocate(maxref(nlon,nlat))
   do j = 1,nlat
   do i = 1,nlon
       maxref(i,j) = -9999.0
       do k = 1,maxlvl
         if (ref0(i,j,k).gt.maxref(i,j))    &
             maxref(i,j) =  ref0(i,j,k)
!             maxref(i,j) =  ref0(i,j,10)
       end do

       if( maxref(i,j) < 0.0 .and. maxref(i,j) > -40.0 ) then
            maxref(i,j) = 0.0
       elseif(maxref(i,j) <=-40.0 .and. maxref(i,j) > -100.0)  then
            maxref(i,j)= -10.0
       elseif(maxref(i,j) <=-100.0) then
            maxref(i,j) = -20.0
       endif

   end do
   end do

   write(*,*) maxval(maxref), minval(maxref),nlon,nlat
   OPEN(88,file='CompositeRefInGSI.dat',form='unformatted')
     write(88) maxref
   close(88)

END PROGRAM
