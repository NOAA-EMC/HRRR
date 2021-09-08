      subroutine getnemsandplace(nfile,im,jm,spval,VarName,VcoordName               &
                           ,l,impf,jmpf,nframed2,NSTAT,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(in) :: spval
      integer,intent(in) :: im,jm,l,impf,jmpf,nstat
      integer :: iret,i,j,nframed2,nframe,N
      integer :: IHINDX(NSTAT),JHINDX(NSTAT)
      real ::  dummy(im,jm), BUFVAR(NSTAT)
      real, allocatable:: dum1d(:)

      
        nframe=nframed2*2
	allocate(dum1d((impf)*(jmpf)))
        call nemsio_readrecv(nfile,trim(VarName)                      &  
        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 
        if (iret /= 0) then
          print*,VarName,VcoordName,l," not found in NEMS file-Assigned missing values"
          dummy=spval
	else 
	  do j=1,jm
	    do i=1,im
	      dummy(i,j)=dum1d((j-1)*impf+i+nframed2)
	if (I .eq. 99 .and. J .eq. 72) then
	write(0,*) 'dummy(99,72): ', dummy(I,J)
	endif
	    end do
	  end do

	  do N=1,NSTAT
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N)=DUMMY(I,J)
          enddo

	end if
	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine getnemsandplace_3d(nfile,im,jm,lm,spval,VarName,VcoordName               &
                                ,l,impf,jmpf,nframed2,NSTAT,IHINDX,JHINDX,BUFVAR)
!      
      use nemsio_module
      implicit none
      type(nemsio_gfile),intent(inout) :: nfile
      INCLUDE "mpif.h"
!
      character(len=20),intent(in) :: VarName,VcoordName
      real,intent(in) :: spval
      integer,intent(in) :: im,jm,lm,l,impf,jmpf,nstat
      integer :: iret,i,j,nframed2,nframe,N
      integer :: IHINDX(NSTAT),JHINDX(NSTAT)
      real ::  dummy(im,jm), BUFVAR(NSTAT,LM)
      real, allocatable:: dum1d(:)
      
        nframe=nframed2*2
	allocate(dum1d((impf)*(jmpf)))

!        do L=1,LM
        call nemsio_readrecv(nfile,trim(VarName)                      &  
        ,trim(VcoordName),l,dum1d,nframe=nframe,iret=iret)	 
        if (iret /= 0) then
          print*,VarName,VcoordName,l," not found in NEMS file-Assigned missing values"
          dummy=spval
	else 
	  do j=1,jm
	    do i=1,im
	      dummy(i,j)=dum1d((j-1)*impf+i+nframed2)
	    end do
	  end do

	  do N=1,NSTAT
            I=IHINDX(N)
            J=JHINDX(N)
            BUFVAR(N,L)=DUMMY(I,J)
          enddo

	end if
	deallocate(dum1d)
                                                                                          
      
       RETURN
       END    
