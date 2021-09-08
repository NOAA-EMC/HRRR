!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine RAMS_varlib(cvar,n1,n2,n3,n4,n5,ngrd,flnm  &
                 ,cdname,cdunits,ivar_type,a,b)

implicit none
integer :: n1,n2,n3,n4,n5,ngrd
character*(*) cvar,flnm,cdname,cdunits

real :: a(*),b(*)

integer, external :: RAMS_getvar, lastchar!, irfree, iralloc
integer :: ierr_getvar,ifound,ivar_type
integer :: lv,lv2,idim_type,irecind,irecsize,ierr
common /getvar/ierr_getvar,ifound

ivar_type=0
ierr_getvar=0
ierr=0
ifound=0
lv=len_trim(cvar)

! 3D VELOCITY AND VORTICITY VARIABLES

if(cvar(1:lv).eq.'u') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   cdname='u'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'v') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
   cdname='v'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'uavg') then
   ivar_type=3
   ierr= RAMS_getvar('UP',idim_type,ngrd,a,b,flnm)
   !call RAMS_comp_avgu(n1,n2,n3,a)
   cdname='u'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'vavg') then
   ivar_type=3
   ierr= RAMS_getvar('VP',idim_type,ngrd,a,b,flnm)
  ! call RAMS_comp_avgv(n1,n2,n3,a)
   cdname='v'
   cdunits='m/s'

elseif(cvar(1:lv).eq.'lat') then
   ivar_type=2
   ierr= RAMS_getvar('GLAT',idim_type,ngrd,a,b,flnm)
   cdname='latitude'
   cdunits='deg'

elseif(cvar(1:lv).eq.'lon') then
   ivar_type=2
   ierr= RAMS_getvar('GLON',idim_type,ngrd,a,b,flnm)
   cdname='longitude'
   cdunits='deg'

elseif(cvar(1:lv).eq.'land') then

   ivar_type = 2
   ierr = RAMS_getvar('PATCH_AREA',idim_type,ngrd,a,b,flnm)
   call RAMS_comp_1minus(n1,n2,1,a)
   cdname='land frac area'
   cdunits=''



else

   print*,'Variable name not found in hvlib.f - ',cvar(1:lv)
   ivar_type=0

endif

if(ierr_getvar.eq.1.or.ifound.eq.0) ivar_type=0

return
end
!-----------------------------------------------------------------------------
subroutine RAMS_comp_1minus(nnxp,nnyp,nnzp,a)
implicit none
integer :: nnxp,nnyp,nnzp
real, dimension(nnxp,nnyp,nnzp) ::a
a(:,:,:)=1.-a(:,:,:)
end subroutine RAMS_comp_1minus
!-----------------------------------------------------------------------------

integer function RAMS_getvar(stringg,itype,ngrd,a,b,flnm)

use an_header

implicit none
integer :: itype,ngrd,il,lastchar,ill
integer :: ierr_getvar,ifound,ni,npts,iword
character*(*) flnm,cgrid*1,flng*240,errmsg*120,stringg,string*10
logical there
real :: a(*),b(*)

common /getvar/ierr_getvar,ifound


!para leitura de analises de medias
il=lastchar(flnm)

ill=lastchar(stringg)

if(flnm(il-18:il-18).eq.'M') then
string=stringg(1:ill)//'M'

else

string=stringg(1:ill)
Endif
!!!!!!!!!!!!!!
do ni=1,nvbtab
   if(string.eq.anal_table(ni)%string.and.ngrd.eq.anal_table(ni)%ngrid) then
      write(cgrid,'(i1)') ngrd
      flng=flnm//'-g'//cgrid//'.vfm'//char(0)
      print*,flng
      inquire(file=flng,exist=there)
      if(.not.there) then
         errmsg='File not found - '//flng
         call error_mess(errmsg)
         return
      endif
      npts=anal_table(ni)%nvalues
      itype=anal_table(ni)%idim_type
      iword=anal_table(ni)%npointer            
!
      print*,'------------------------------------------'
      print*,'Get var: ',anal_table(ni)%string,' type=', itype
      print*,'Size of record=',npts,' pointer=',iword
      print*,'------------------------------------------'
!      
      call RAMS_c_open(flng,'r'//char(0))
      call vfirecr(10,a,npts,'LIN',b,iword)
      call RAMS_c_close()
      RAMS_getvar=0
      ifound=ifound+1
      return

   endif
enddo

errmsg='Variable not available in this run - '//string
call error_mess(errmsg)
RAMS_getvar=1
ierr_getvar=1

return
end
!-----------------------------------------------------------------------------
!     New version that just uses ls and the /tmp directory
      subroutine chem_RAMS_filelist(fnames,file_prefix,maxfiles,nfile,task)
      character fnames(maxfiles)*(*),file_prefix*(*)
!      character file*128,command*128
      character file*240,command*240
      integer :: maxfiles,nfiles
      integer task
! This version uses nfile as flag for whether to stop if no files exist.
!    If nfile.ge.0, then stop

      iflag=nfile

      nfile = 0
      print *, ' '
      print *, 'checking directory - ',file_prefix
      
      iprelen=index(file_prefix,' ')
      if(iprelen.eq.0) iprelen=len(file_prefix)
      command=  &
!          '/bin/ls -1 '//file_prefix(1:iprelen)//' >/tmp/RAMS_filelist'
          '/bin/ls -1 '//file_prefix(1:iprelen)//' >./prep_chem_src_filelist'
      call system(command)
!      command= 'chmod 777 /tmp/RAMS_filelist'
      command= 'chmod 777 ./prep_chem_src_filelist'
      call system(command)
      
!     Open the directory list and read through the files
      iun=98
!      open(unit=iun,file='/tmp/RAMS_filelist',status='old',err=15)
      open(unit=iun,file='./prep_chem_src_filelist',status='old',err=15)
      rewind iun
      
      do nf=1,1000000
         read(iun,'(a128)',end=30,err=30) file
         if(nf< maxfiles) then 
           if(task == 1) fnames(nf) = file(1:len_trim(file)-9)!use this for RAMS analysis
	   if(task == 2) fnames(nf) = file
	 else
	    print*,'number of files > maxfiles'
	    stop 333
	 endif
      enddo
      
 30   continue

      close(iun)
!      command= '/bin/rm -f /tmp/RAMS_filelist'
      command= '/bin/rm -f ./prep_chem_src_filelist'
      call system(command)

      nfile=nf-1

      if (nfile .eq. 0) then
         print *, 'No prep-chem-src files for prefix:',file_prefix
         if(iflag.ge.0) stop './prep_chem_src_filelist: no_files'
      endif
      
      return 
 15   print *, 'prep_chem_src_filelist: Error opening ./prep_chem_src_filelist'
      stop  'file error : run again'
      return
      
 100  continue

      return
      end




!-----------------------------------------------------------------------------
subroutine chem_rams_read_header(flnm)

use an_header

implicit none

character*(*) flnm
character(len=128) :: flnm2
integer lenf,nv


if(allocated(anal_table)) deallocate(anal_table)

! open analysis file and read in commons

flnm2=flnm(1:len_trim(flnm))//'-head.txt'
open(10,file=flnm2(1:len_trim(flnm2)),status='old')
read(10,*) nvbtab
allocate (anal_table(nvbtab))
do nv=1,nvbtab
   read(10,*)  anal_table(nv)%string   &
	      ,anal_table(nv)%npointer  &
	      ,anal_table(nv)%idim_type  &
	      ,anal_table(nv)%ngrid  &
	      ,anal_table(nv)%nvalues
enddo

call commio('ANAL','READ',10)
close(10)


return
end
!-----------------------------------------------------------------------------

subroutine ep_getvar(ivar,cvar,dvar,name_var,units_var,r2d,nx,ny,nz,nzg,npatch,&
             ng,flnm,itype)
 
implicit none
integer  nx,ny,nz,nzg,npatch,ng,itype,ivar
character *(*) cvar,dvar,name_var,units_var,flnm
real, dimension (nx,ny), intent(out)   :: r2d

!real, dimension (nx,ny,nz,npatch) :: a4d,b4d
real, dimension (nx,ny,nz) :: a3d,b3d
real, dimension (nx,ny)    :: a2d,b2d 

if(dvar=='2d') then
     call RAMS_varlib(cvar,nx,ny,nz,nzg,npatch,ng,flnm,      &
   		      name_var,units_var,itype,a2d,b2d) 
     r2d(:,:) = a2d(:,:)
     
elseif(dvar=='3d') then 
!checar caso de 3d tipo up,vp, etc ...     
     call RAMS_varlib(cvar,nx,ny,nz,nzg,npatch,ng,flnm,      &
   		   name_var,units_var,itype,a3d,b3d)  
		   
      r2d(:,:) = a3d(:,:,1)
endif      
  		   
return
end

!-----------------------------------------------------------------------------

subroutine determine_dim_var(stringg,itype)

use an_header

implicit none

integer :: itype,npts,iword,ni
character*(*) stringg
do ni=1,nvbtab
 !print*,'xxxx',trim(stringg),anal_table(ni)%string
   if(trim(stringg).eq.anal_table(ni)%string) then
       npts=anal_table(ni)%nvalues
      itype=anal_table(ni)%idim_type
      iword=anal_table(ni)%npointer
      
      return
   endif
enddo
return
end
      
!---------------------------------------------------------------------
subroutine geo_grid(nx,ny,rlat,rlon,dep_glon1,dep_glon2,  &
 		    dep_glat1,dep_glat2,		  &
		    rlatmin,rlatmax,rlonmin,rlonmax,	  &
 		    nxg,nyg,proj)
use grid_dims	 , only : nxpmax,nypmax
use grid_dims_out, only : glatg,glong
implicit none
integer :: n,i,j,nx,ny,nxg,nyg
real :: rlat(nx,ny),rlon(nx,ny),delta
real dep_glon1,dep_glon2,  &
     dep_glat1,dep_glat2, &
     rlatmin,rlatmax,rlonmin,rlonmax
real :: x,xx,rlon1,rlat1,dlon_min,dlat_min
character*(*) proj
! common/grid2/ glatg(nypmax),glong(nxpmax)

dep_glon1=rlon(1,1)
dep_glon2=rlon(nx,1)
delta = .01*(int(100.*dep_glon2)-int(100.*dep_glon1))
if(delta .gt. rlon(2,1)-rlon(1,1)) then
  do n=1,ny
    if(rlon(1,n).gt.dep_glon1) dep_glon1=rlon(1,n)
    if(rlon(nx,n).lt.dep_glon2) dep_glon2=rlon(nx,n)
  enddo
  dep_glon2= (dep_glon2-dep_glon1)/(nx-1)
else
  ! Domain circles around globe
  dep_glon1=rlon(1,1)
  dep_glon2=360+rlon(nx-1,1)
  do n=1,ny
    if(rlon(1,n).gt.dep_glon1) dep_glon1=rlon(1,n)
    if(rlon(nx-1,n)+360.lt.dep_glon2) dep_glon2=rlon(nx-1,n)+360.
  enddo
  dep_glon2= (dep_glon2-dep_glon1)/(nx-2)
endif

dep_glat1=rlat(1,1)
dep_glat2=rlat(1,ny)
do n=1,nx
  if(rlat(n,1).gt.dep_glat1)  dep_glat1=rlat(n,1)
  if(rlat(n,ny).lt.dep_glat2) dep_glat2=rlat(n,ny)
enddo
dep_glat2= (dep_glat2-dep_glat1)/(ny-1)

! Skip if domain circles globe
if(delta .gt. rlon(2,1)-rlon(1,1)) then
!---versao 1
!10/08/98
  x=0.
  xx=0.
  do n=1,ny
   x=x+rlon(1,n)
   xx=xx+ (rlon(nx,n)-rlon(1,n))/(nx-1)
  enddo
  dep_glon1= x/ny
  dep_glon2=xx/ny
endif  

  x=0.
  xx=0.
  do n=1,nx
   x=x+rlat(n,1)
   xx=xx+ (rlat(n,ny)-rlat(n,1))/(ny-1)
  enddo
  dep_glat1= x/nx
  dep_glat2=xx/nx


! NAO USANDO VERSAO TELESCOPICA
go to 100
!- versao 2
!-2007  grade telescopica------------------------
dlon_min=1.e10
dlat_min=1.e10
!print*,nx,ny
do i=2,nx;do j=2,ny 
dlon_min=min(dlon_min,abs(rlon(i,j)-rlon(i-1,j)))
dlat_min=min(dlat_min,abs(rlat(i,j)-rlat(i,j-1)))
!print*,i,j,abs(rlon(i,j)-rlon(i-1,j)),abs(rlat(i,j)-rlat(i,j-1))
enddo;enddo
dep_glat2=dlat_min
dep_glon2=dlon_min
!print*,dlon_min,dlat_min; call flush(6)
!pause

!stop 333
!-2007------------------------------------------



100 continue
if(proj.ne.'YES'.and.proj.ne.'yes') then
   nxg=nx
   nyg=ny
  
else
            
!...... Grade para o GRADS:

rlatmin=rlat(1,1)
rlatmax=rlat(1,1)
rlonmin=rlon(1,1)
rlonmax=rlon(1,1)
do i=1,nx
 do j=1,ny
    rlatmin=min(rlatmin,rlat(i,j))
    rlatmax=max(rlatmax,rlat(i,j))
    rlonmin=min(rlonmin,rlon(i,j))
    rlonmax=max(rlonmax,rlon(i,j))
 enddo
enddo 

!...... Definicao da grade do GRADS 
!
! Para testar dependencia com a resolucao da grade
!
! dep_glon2=0.5*dep_glon2
! dep_glat2=0.5*dep_glat2
 
 nxg=int((rlonmax-rlonmin)/dep_glon2+0.5)-1
 nyg=int((rlatmax-rlatmin)/dep_glat2+0.5)-1
 rlon1=rlonmin-(nxg-nx-1)*dep_glon2
 rlat1=rlatmin-(nyg-ny-1)*dep_glat2

! rlon1=rlonmin-2*dep_glon2
! rlat1=rlatmin-2*dep_glat2

 rlon1=rlonmin-dep_glon2
 rlat1=rlatmin-dep_glat2
 dep_glat1= rlat1
 dep_glon1= rlon1

! print*,rlonmin,rlonmax,rlatmin,rlatmax
! print*,nxg,nyg,dep_glon2,dep_glat2,dep_glat1,dep_glon1
! stop 200
endif
!Define  grade do GRADS

 do i=1,nxg
  glong(i)=dep_glon1+float(i-1)*dep_glon2
! print*,' i lon=',i,glong(i)
 enddo

 do j=1,nyg
  glatg(j)=dep_glat1+float(j-1)*dep_glat2
! print*,' j lat=',j,glatg(j)
 enddo

return
end
!---------------------------------------------------------------------                  

subroutine Matriz_interp(ng,nxg,nyg,nxr,nyr,rlat1,dlat, &
           rlon1,dlon,iinf,jinf,rmi,proj,mean_type)
use mem_grid, only : xtn,ytn,deltaxn,deltayn,polelat,polelon,stdlat1   &
     		   ,stdlat2   
use grid_dims_out, only : glatg,glong,grid_type
implicit none
  integer :: ng,nxg,nyg,nxr,nyr
  real rlat1,dlat, rlon1,dlon
  character*(*) proj,mean_type
  integer itype_proj ! = 0 normal, =1 usa o ponto mais proximo

  real:: rmi(nxg,nyg,4),iinf(nxg,nyg),jinf(nxg,nyg)
  integer :: i,j,l,i1,i2,j1,j2,iy,ix
  real x,y,undef
  !
  if(proj.ne.'YES'.AND.proj.ne.'yes') RETURN

  !       Construcao da matriz de interpolacao.
  !       Flag para pontos do grads fora do dominio do modelo

!!!  if(itype_proj==0) then

      undef=-9.99e+15
      do i=1,nxg
   	 do j=1,nyg
   	    iinf(i,j)=1
   	    jinf(i,j)=1
   	    do l=1,4
   	       rmi(i,j,l)=undef
   	    enddo
   	 enddo
      enddo
  
!!  else 
!!      rmi(:,:,:)=-9.99e+15
!!  endif
  
  do i=1,nxg
     do j=1,nyg
        !       Encontra posicao do ponto de grade do GRADS na grade do RAMS
        !
        !        glatg(i)=-37.113
        !        glong(j)=-79.128
        !        xlat=glatg(i)
        !        xlon= glong(j)
        !        Call ll_xy(glatg(i),glong(j),polelat,polelon,stdlat2,x,y)
        !       call getops(pla,plo,xlat,xlon,polelat,polelon)
        !       call pstoxy(x,y,pla,plo,6376000.)
        !
        if(trim(grid_type)=='polar' .or. trim(grid_type) =='rams') then
	     call ge_to_xy(polelat,polelon,stdlat2,glong(i),glatg(j),x,y)
	elseif( trim(grid_type) =='lambert') then
	     call ll_lc2(glatg(j),glong(i),polelat,polelon,stdlat1,stdlat2,x,y)
        elseif(grid_type .eq. 'mercator')then
          call llij_merc(glatg(j),glong(i),polelat,polelon,stdlat1,x,y)   
	else
	     stop 'no projection defined '
	endif
	!  print*,'xx',glatg(j),glong(i),x,y !tks
        !
        !        print*,x,y,glong(i),glatg(j),polelat,polelon

        !       Elimina pontos fora:
        if(x.lt.xtn(1,ng).or.x.gt.xtn(nxr,ng)) go to 777
        if(y.lt.ytn(1,ng).or.y.gt.ytn(nyr,ng)) go to 777
        !        
        do ix=1,nxr
           if(x.le.xtn(ix,ng)) go to 555
        enddo
555     continue
        i1=ix-1
        i2=ix
        iinf(i,j)=i1         

        do iy=1,nyr
           if(y.le.ytn(iy,ng)) go to 666
        enddo
666     continue
        j1=iy-1
        j2=iy                
        jinf(i,j)=j1
        !        

!!!!       if(itype_proj==0) then
       if(mean_type == 'BAV' .or. mean_type == 'bav') then
!projecao pela media ponderada
            rmi(i,j,1)=(x-xtn(i1,ng))/deltaxn(ng)
            rmi(i,j,2)=1.-rmi(i,j,1)
            !	      
            rmi(i,j,3)=(y-ytn(j1,ng))/deltayn(ng)
            rmi(i,j,4)=1.-rmi(i,j,3)

        elseif(mean_type == 'VMP' .or. mean_type == 'vmp') then

!-srf-set2005 - projecao pelo ponto mais proximo
            if(x-xtn(i1,ng) <= 0.5*deltaxn(ng) ) then
	      rmi(i,j,1)= 0.
	    else
	      rmi(i,j,1)= 1.
	    endif
	    
	    if(y-ytn(j1,ng) <= 0.5*deltayn(ng)) then
	      rmi(i,j,3) = 0.
	    else
	      rmi(i,j,3) = 1.
	    endif   
            rmi(i,j,2)=1.-rmi(i,j,1)	     
            rmi(i,j,4)=1.-rmi(i,j,3)
            
        else
	    print*,'MEAN_TYPE not available:'
	    print*,'Use VMP or BAV'
	    print*,'stop at Matriz_interp routine' 
	    stop 1234
		
        endif



777     continue
!
!                if(rmi(i,j,1)+rmi(i,j,2)+rmi(i,j,3)+rmi(i,j,4)> 0)then
!                if(abs(rmi(i,j,1)+rmi(i,j,2)+rmi(i,j,3)+rmi(i,j,4)).ne.2)then
!		 print*,'XXXXXXXXXXXXXXXXXXXXXXX'
!		 stop 33333
!		 print*,i,j,rmi(i,j,1)+rmi(i,j,2)+rmi(i,j,3)+rmi(i,j,4)
!		 print*,rmi(i,j,1),rmi(i,j,2),rmi(i,j,3),rmi(i,j,4)
!		 endif
!		 endif
        !
        !        
     enddo
  enddo
  return
end Subroutine Matriz_interp

!---------------------------------------------------------------------                  
 subroutine ge_to_xy(polelat,polelon,stdlat2,xlon,xlat,x,y)

 use grid_dims, only : pi180, r_earth

!transformacao horizontal:

 b = 1.0+sin(pi180*xlat)*sin(pi180*polelat)+ &
     cos(pi180*xlat)*cos(pi180*polelat)*cos(pi180*(xlon-polelon))

 ! f = 2.00*r_earth/b
 sctop = 1. + sin(abs(stdlat2) * pi180)
 f = sctop*r_earth/b

 y = f*(cos(pi180*polelat)*sin(pi180*xlat) -                &
     sin(pi180*polelat)*cos(pi180*xlat)*cos(pi180*(xlon-polelon)))
     
 x = f*(cos(pi180*xlat)*sin(pi180*(xlon - polelon)))

 !write( 16,*) ' X2: ',x, 'Y2: ',y,xlat,polelat
 
 return

 end

!-------------------------------------------------------------------
subroutine define_lim(ng,nxg,nyg,rlat1,dlat,rlon1,dlon, &
		     lati,latf,loni,lonf,nxa,nxb,nya,nyb,proj,&
 		     nx,ny,rlat,rlon)
use grid_dims_out, only : glatg,glong
implicit none
!       include 'rconfig.h'
       integer n,nlon,nlat,i,j,ng,nxg,nyg,nx,ny,nxa,nxb,nya,nyb
       real :: rlat(nx,ny),rlon(nx,ny)
       real x, xx
       real lati,latf,loni,lonf,rlat1,dlat,rlon1,dlon
       character*(*) proj

       do i=1,nxg
       if(loni.le.glong(i)) go to 100
!        print*,' i lon=',i,glong(i),loni
       enddo
 100   continue
       nxa=max(i,1)

       do j=1,nyg
       if(lati.le.glatg(j)) go to 101
!        print*,' j lat=',j,glatg(j),lati
       enddo
 101   continue
       nya=max(j,1)

       nlon=abs(int(((lonf-loni))/dlon))+1
       nlat=abs(int(((latf-lati))/dlat))+1
       nxb=min(nxa+nlon,nxg)
       nyb=min(nya+nlat,nyg)
       rlon1=glong(nxa)
       rlat1=glatg(nya)
!----------19-07-2001

       if(proj.ne.'YES'.AND.proj.ne.'yes') then
       x=0
       xx=0
       do j=nya,nyb
	x=x+rlon(nxa,j)
	xx=xx+ (rlon(nxb,j)-rlon(nxa,j))/(nxb-nxa)
       enddo
       rlon1= x/(nyb-nya+1)
       dlon =xx/(nyb-nya+1)
       
       x=0
       xx=0
       do n=nxa,nxb
	x=x+rlat(n,nya)
	xx=xx+ (rlat(n,nyb)-rlat(n,nya))/(nyb-nya)
       enddo
       rlat1= x/(nxb-nxa+1)
       dlat =xx/(nxb-nxa+1)
       
       endif



!------------18-07-2001
!       if(proj.ne.'YES'.AND.proj.ne.'yes') then
!       call define_grid2(loni,lonf,lati,latf,nxg,nyg,&
!                        rlat,rlon,nxa,nxb,nya,nyb)
!	print*,nxa,nxb,nya,nyb

!       x=0.
!       xx=0.
!       do j=nya,nyb
!	x=x+rlon(nxa,j)
!!certo >>	x=x+rlon(nxa(j),j)
!	xx=xx+ (rlon(nxb,j)-rlon(nxa,j))/(nx-1)
!!certo>>>	xx=xx+ (rlon(nxb(j),j)-rlon(nxa(j),j))/(nx-1)
!       enddo
!       dep_glon1= x/ny
!       dep_glon2=xx/ny
!       
!
!>>>>>>>> aqui nxa e nxb dependem de j (nxa(j), nxb(j)
!              nya == nya(i) 
!	      nyb == nyb(i)
!	      consertar na rotina define_grid2
!   colocar dimension nxa(nYpmax) ...nxb(NYPMAX)
!                    nya(nXmax)
!-----------------------------


!       print*,nxa,nya,nxb,nyb,nlon,nlat,rlon1,rlat1
       return
       end
!---------------------------------------------------------------------
subroutine proj_rams_to_grads(nxr,nyr,nzz,nxg,nyg, nvert, &
			      rmi,iinf,jinf,	    &
    			      rout,routgrads,proj)
implicit none
integer i,j,k,nxr,nyr,nzz,nxg,nyg,nvert,j1,i1,j2,i2
real r1,r2,r3,r4,rr1,rr2
character*(*) proj
real :: rout(nxr,nyr,nzz),routgrads(nxg,nyg,nvert)
real :: rmi(nxg,nyg,4),iinf(nxg,nyg),jinf(nxg,nyg)

if(proj.ne.'YES'.AND.proj.ne.'yes') then
  if(nxg.ne.nxr.AND.nyg.ne.nyr) then
   print*,'Projection with problems nxr nxg ...'
   stop
  endif
     routgrads(:,:,1:nvert)= rout(:,:,1:nvert)
  return
endif
!print*,'nvert=',nvert, 'nzz=',nzz
!stop 33
do i=1,nxg
  do j=1,nyg
  
   r1= rmi(i,j,1)
   r2= rmi(i,j,2)
   r3= rmi(i,j,3)
   r4= rmi(i,j,4)
   i1= iinf(i,j)
   i2= i1+1
   j1= jinf(i,j)
   j2= j1+1

   do k=1,nvert
   !do k=1,nzz

    rr1=   rout(i1,j1,k)*(1.-r1)+rout(i2,j1,k)*(1.-r2) 
    rr2=   rout(i1,j2,k)*(1.-r1)+rout(i2,j2,k)*(1.-r2) 
    routgrads(i,j,k)=rr1*(1.-r3)+	   rr2*(1.-r4)
    
    if(abs(routgrads(i,j,k)).gt.1.E+9)  &
    	      routgrads(i,j,k)=-9.99E+33
    !if(abs(routgrads(i,j,k)).lt.1.E-15)  &
    !	      routgrads(i,j,k)=-9.99E+33
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write(2,0998)
!   write(2,0999) i,j,i1,j1,glatg(j),glong(i)
!   write(2,1000) rlat(i1,j1),rlat(i2,j1),rlat(i1,j2),rlat(i2,j2)
!   write(2,1001) rlon(i1,j1),rlon(i2,j1),rlon(i1,j2),rlon(i2,j2)
!   write(2,1002) rout(i1,j1,k),rout(i2,j1,k),rout(i1,j2,k),&
!		  rout(i2,j2,k), routgrads(i,j,k)
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   enddo
  enddo
 enddo

! 0998   format(1x,'---------------------------------------------')
! 0999   format(1x,4i3,2f10.2)
! 1000   format(1x,4f10.2)
! 1001   format(1x,4f10.2)
! 1002   format(1x,4f10.2,f16.3)

!xxxxxxxxxxxxxxxxxxxxxxxxx
!      k=1
!      do jj=1,nyr
!      do ii=1,nxr
!         write(10,'(2i3,3f8.1)')ii,jj,rlat(ii,jj),rlon(ii,jj)
!     +             ,rout(ii,jj,k)
!      enddo
!      enddo
!      do jj=1,nyg
!      do ii=1,nxg
!         write(11,'(2i3,3f8.1)')ii,jj,glatg(jj),glong(ii)
!     +             ,routgrads(ii,jj,k)
!      enddo
!      enddo
!xxxxxxxxxxxxxxxxxxxxxxxxx
return
end
!-------------------------------------------------------------------
subroutine write_bin(iunit,nrec,nx,ny,nvert,nxa,nxb,nya,nyb,rout)
implicit none
integer, intent(inout) :: nrec
integer, intent(in) :: iunit,nx,ny,nvert,nxa,nxb,nya,nyb
real, intent(in), dimension(nx,ny,nvert):: rout
integer i,j,k

do k=1,nvert
  nrec=nrec+1
  
  !PRINT*, '*****************************'
  PRINT*, 'max:', maxval(rout(:,:,k))
  PRINT*, 'min:', minval(rout(:,:,k), MASK=rout(:,:,k) .GT. 1.e-20)
  PRINT*, '*****************************'
  
  write (iunit,rec=nrec) ((rout(i,j,k),i=nxa,nxb),j=nya,nyb)
enddo
end subroutine write_bin

!-------------------------------------------------------------------
subroutine write_ctl(ng,iunit,wfln,ihour,iday,imon,iyear,nvert,ihtran)
use emiss_vars_emissions, only: emiss_spc_name                 & 
                               ,emiss_nspecies                 &
                               ,number_sources                 &
                               ,src_name,found_emiss_spc       &
                               ! plume properties
			       ,emiss_plume_name               &
			       ,found_emiss_plume
			       
use grid_dims_out
use bbbem_plumerise, only: nveg_agreg,spc_suf
implicit none
character(*) :: wfln(2)
integer ng,ihour,iday,imon,iyear,nvert,iunit,iv,isp,nsrc,ihtran,total

character*15 chdate,chstep,xchstep
character*3 cmo(12) 
character*5 unitX  ,ctotal,cihtran
integer nfiles,imin,nvp,i,marco
real zlev(nvert),pass 


data cmo/'jan','feb','mar','apr','may','jun','jul','aug','sep', &
	'oct','nov','dec'/	  

nfiles =1 
imin=0 
zlev (:)=0.
chdate='00:00z00mmm1900'
write(chdate(1:2),'(i2.2)') ihour
write(chdate(4:5),'(i2.2)') imin
write(chdate(7:8),'(i2.2)') iday
chdate(9:11)=cmo(imon)(1:3)
write(chdate(12:15),'(i4.2)') iyear
xchstep='          1dy'

nvp=0
! number of variables:
  do isp=1,emiss_nspecies
    do nsrc=1, number_sources
        !print*,'found=',found_emiss_spc(isp,nsrc)
        if(found_emiss_spc(isp,nsrc) == 0) cycle ! if not found, cycle
        nvp=nvp+1
    enddo
 enddo

!-plumerise section
 if(use_bbem_plumerise == 1) then 
  do isp=1,emiss_nspecies+1
    do iv=1, nveg_agreg
        !print*,'found plume=',found_emiss_plume(isp,iv)
        if(found_emiss_plume(isp,iv) == 0) cycle ! if not found, cycle
        nvp=nvp+1
    enddo
  enddo
 endif 
 if(use_volcanoes == 1) then 
   nvp=nvp+3 !injh + tdur + elev
 endif 
 if(use_degass_volcanoes == 1) then 
   nvp=nvp+1 ! plume height
   nvp=nvp+1 ! elevation
 endif 
write(cihtran, '(i5)') ihtran


 open(iunit,file=wfln(2),status='unknown')
 write(iunit,2001) '^'//trim(wfln(1)(1:len_trim(wfln(1))))
#if RADM_WRF_FIM
 write(iunit,2002) 'OPTIONS byteswapped'
#endif
 write(iunit,2002) 'undef -9.99e33'
 write(iunit,2002) 'title Source Emission'
 write(iunit,2003) nxb(ng)-nxa(ng)+1,(dep_glon(i,ng),i=1,2)
 
 if (ihtran == 3) then
   i=0
   total=(nyb(ng)-nya(ng)+1)
   !allocate(lats(total))
   
   write(ctotal, '(i5)') total
   pass=0
   marco = 0
   write(iunit,fmt='(A,i4,A)',advance="no") 'ydef ', nyb(ng)-nya(ng)+1,' levels '
   do i=1, total
    	!rlat(i) = dep_glat(1,ng)+pass
	if (marco == 10) then
	  write(iunit,fmt='(f15.3)') glatg(i)
	  marco = 0
	else
	  if (i == total ) then
	    write(iunit,fmt='(f15.3)') glatg(i)
	  else 
	    write(iunit,fmt='(f15.3)',advance="no") glatg(i)
	  endif
	endif
	!pass=dep_glat(2,ng)+pass
	marco = marco + 1
   enddo
    
 else
   write(iunit,2004) nyb(ng)-nya(ng)+1,(dep_glat(i,ng),i=1,2)
 endif
 
 write(iunit,2005) nvert,zlev(1:nvert)
 write(iunit,2006) nfiles,chdate,xchstep   
 write(iunit,2007) nvp
         
 do isp=1,emiss_nspecies
   do nsrc=1, number_sources
     if(found_emiss_spc(isp,nsrc) == 0) cycle ! if not found, cycle
     !write(iunit,2008) vp(i),zlevmax(ng),vpln(isp),vpun(i) ! 3d dim var
      write(iunit,2008) emiss_spc_name(isp,nsrc)(1:len_trim(emiss_spc_name(isp,nsrc)))//'_'//src_name(nsrc)&
                       ,0,src_name(nsrc),'kg/m^2' !2d dim var
  enddo
 enddo
 
 !-plumerise section
 if(use_bbem_plumerise == 1) then   
   do isp=1,emiss_nspecies+1 ! nspecies + fire size
     do iv=1, nveg_agreg 
      if(found_emiss_plume(isp,iv) == 0) cycle ! if not found, cycle
            
      if(use_bbem == 1)&
       write(iunit,2009) emiss_plume_name(isp,iv)(1:len_trim(emiss_plume_name(isp,iv)))//'_'//spc_suf(iv)&
                        ,0,spc_suf(iv),'fract' !2d dim var
      if(use_bbem == 2) then 
       unitx='fract'
       if(emiss_plume_name(isp,iv)=="mean_size".or.emiss_plume_name(isp,iv)=="std_size") unitx="km^2"
       if(emiss_plume_name(isp,iv)=="mean_frp" .or.emiss_plume_name(isp,iv)=="std_frp" ) unitx="MW"
       write(iunit,2009) adjustl(emiss_plume_name(isp,iv))&!(1:len_trim(emiss_plume_name(isp,iv))))&
                        ,0,"bburn ",unitx !2d dim var
      endif
   enddo
   enddo
 endif
 !-volcanic eruption section
 if(use_volcanoes == 1) then   
            
       write(iunit,2010) 'INJH'//'_'//'geoge',0,'geoge','m' !2d dim var
       write(iunit,2010) 'ELEV'//'_'//'geoge',0,'geoge','m' !2d dim var
       write(iunit,2010) 'TDUR'//'_'//'geoge',0,'geoge','s' !2d dim var
 endif

 !-degasing volcanic section
 if(use_degass_volcanoes == 1) then   
            
       write(iunit,2011) 'PLUM_H'//'_'//'geoge',0,'geoge','m' !2d dim var
       write(iunit,2011) 'V_ELEV'//'_'//'geoge',0,'geoge','m' !2d dim var
 endif
 ! 
 ! 
 write(iunit,2002) 'endvars'
 close(iunit)



2001  format('dset ',a)
2002  format(a)
2003  format('xdef ',i4,' linear ',2f15.3)
2004  format('ydef ',i4,' linear ',2f15.3)
2005  format('zdef ',i4,' levels ',60f10.1)
2006  format('tdef ',i4,' linear ',2a15)
2007  format('vars ',i4)
2008  format(a10,i4,' 99    - EMISSION DATA : ',a40,'[',a8,']')
2009  format(a15,i4,' 99    - PLUME    DATA : ',a40,'[',a8,']')
2010  format(a10,i4,' 99    - VOLC. ERUPTION DATA : ',a40,'[',a8,']')
2011  format(a10,i4,' 99    - VOLC. DEGASS DATA : ',a40,'[',a8,']')
2055  format(60f7.0)

end subroutine write_ctl
! ---------------------------------------------------------------
!--------------------------------------------------------------------------
      subroutine gauss_lat(njg,glat)
!         
!         program to calculate Gaussian latitudes
!         enter number of lats after prompt
!
!      parameter(n1max=1000)
      real glat(njg)
      character form*10



!      print*,'Enter number of gaussian latitudes (power of 2):'
!      read(*,'(i3)') njg
       if(mod(njg,2).ne.0) then
        print*,'number of gaussian latitudes must be a power of 2)'
        stop 
      endif


      call gauss_lat_nmc(glat,njg)

      print*,'ydef ',njg,' levels'
      ncol=8
      write(form,106) ncol
 106  format('(',i3,'f10.5)')
      do j=1,njg,ncol
!       write(*,'(i3,2x,f10.5)') j,glat(j)
        jb=j
        je=j+ncol-1
        if(je.gt.njg) je=njg
        write(*,form) (glat(jj),jj=jb,je)
      end do

!      do j=2,njg
!      print*,j,glat(j)-glat(j-1)
!      enddo
      return
      end
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gaulat      calculates gaussian grid latitudes
!   prgmmr: s. j. lord       org: w/nmc22    date: 91-06-06
!
! abstract: calculates gaussian grid latitudes
!
! program history log:
!   91-06-06  s. j. lord - copied from kanamitsu library
!   930921 m.fiorino - changed from colatitude to latitude
!   yy-mm-dd  modifier1   description of change
!   yy-mm-dd  modifier2   description of change
!
! usage:    call pgm-name(inarg1, inarg2, wrkarg, outarg1, ... )
!   input argument list:
!     inarg1   - generic description, including content, units,
!     inarg2   - type.  explain function if control variable.
!
!   output argument list:      (including work arrays)
!     wrkarg   - generic description, etc., as above.
!     outarg1  - explain completely if error return
!     errflag  - even if many lines are needed
!
!   input files:   (delete if no input files in subprogram)
!     ddname1  - generic name & content
!
!   output files:  (delete if no output files in subprogram)
!     ddname2  - generic name & content as above
!     ft06f001 - include if any printout
!
! remarks: list caveats, other helpful hints or information
!
! attributes:
!   language: indicate extensions, compiler options
!   machine:  nas, cyber, whatever
!
!$$$
      subroutine gauss_lat_nmc(gaul,k)                                     
!                                                                   
      implicit double precision (a-h,o-z)
      dimension a(500)
      integer, intent(in) :: k
!      real gaul(k)
      real, intent(out):: gaul(k)                                                
                                                      
!
      save
!                                                                   
      esp=1.d-14                                                    
      c=(1.d0-(2.d0/3.14159265358979d0)**2)*0.25d0                  
      fk=k                                                          
      kk=k/2                                                        
      if(kk> 500) stop 'error is gauss_lat_nmc'                                                    
      call bsslz1(a,kk)                                             
      do 30 is=1,kk                                                 
      xz=cos(a(is)/sqrt((fk+0.5d0)**2+c))                           
      iter=0                                                        
   10 pkm2=1.d0                                                     
      pkm1=xz                                                       
      iter=iter+1                                                   
      if(iter.gt.10) go to 70                                       
      do 20 n=2,k                                                   
      fn=n                                                          
      pk=((2.d0*fn-1.d0)*xz*pkm1-(fn-1.d0)*pkm2)/fn                 
      pkm2=pkm1                                                     
   20 pkm1=pk                                                       
      pkm1=pkm2                                                     
      pkmrk=(fk*(pkm1-xz*pk))/(1.d0-xz**2)                          
      sp=pk/pkmrk                                                   
      xz=xz-sp                                                      
      avsp=abs(sp)                                                  
      if(avsp.gt.esp) go to 10                                      
      a(is)=xz                                                      
   30 continue                                                      
      if(k.eq.kk*2) go to 50                                        
      a(kk+1)=0.d0                                                  
      pk=2.d0/fk**2                                                 
      do 40 n=2,k,2                                                 
      fn=n                                                          
   40 pk=pk*fn**2/(fn-1.d0)**2                                      
   50 continue                                                      
      do 60 n=1,kk                                                  
      l=k+1-n                                                       
      a(l)=-a(n)                                                    
   60 continue                                                      
!                                                                   
      radi=180./(4.*atan(1.))                                       
      do 211 n=1,k                                                  
      gaul(n)=acos(a(n))*radi-90.0                                       
  211 continue                                                      
!     print *,'gaussian lat (deg) for jmax=',k                      
!     print *,(gaul(n),n=1,k)                                       
!                                                                   
      return                                                        
   70 write(6,6000)                                                 
 6000 format(//5x,14herror in gauaw//)
      stop
      end        
      subroutine bsslz1(bes,n)                                      
!                                                                   
!                                                                   
      implicit double precision (a-h,o-z)                                      
      dimension bes(n)                                              
      dimension bz(50)                                              
!                                                                   
      data pi/3.14159265358979d0/                                   
      data bz  / 2.4048255577d0, 5.5200781103d0,                       &
       8.6537279129d0,11.7915344391d0,14.9309177086d0,18.0710639679d0, &
      21.2116366299d0,24.3524715308d0,27.4934791320d0,30.6346064684d0, &
      33.7758202136d0,36.9170983537d0,40.0584257646d0,43.1997917132d0, &
      46.3411883717d0,49.4826098974d0,52.6240518411d0,55.7655107550d0, &
      58.9069839261d0,62.0484691902d0,65.1899648002d0,68.3314693299d0, &
      71.4729816036d0,74.6145006437d0,77.7560256304d0,80.8975558711d0, &
      84.0390907769d0,87.1806298436d0,90.3221726372d0,93.4637187819d0, &
      96.6052679510d0,99.7468198587d0,102.888374254d0,106.029930916d0, &
      109.171489649d0,112.313050280d0,115.454612653d0,118.596176630d0, &
      121.737742088d0,124.879308913d0,128.020877005d0,131.162446275d0, &
      134.304016638d0,137.445588020d0,140.587160352d0,143.728733573d0, &
      146.870307625d0,150.011882457d0,153.153458019d0,156.295034268d0/

      nn=n                                                          
      if(n.le.50) go to 12                                          
      bes(50)=bz(50)                                                
      do 5 j=51,n                                                   
    5 bes(j)=bes(j-1)+pi                                            
      nn=49                                                         
   12 do 15 j=1,nn                                                  
   15 bes(j)=bz(j)                                                  
      return                                                        
      end                                                           
       

!--------------------------------------------------------------------------
subroutine yrevert(ni,nj,npatches,a)
implicit none
integer :: ni,nj,npatches,ni2,i,j
real, dimension(ni,nj,npatches) :: a,dummy
real var(npatches)

dummy(:,:,:) = a(:,:,:)
      
!- reverte a ordem em Y

do j = 1, int(nj/2)
   do i = 1, ni 
      var(:)		  = dummy (i, j     ,:) 
      dummy (i,      j,:) = dummy (i, nj-j+1,:)
      dummy (i, nj-j+1,:) = var(:)

   enddo
enddo

a(:,:,:) = dummy(:,:,:)

return ! <<<<<<   only y-revert


!escreve de -180 a + 180
ni2=int(ni/2)
do j = 1, nj
   do i = 1, ni2 
  	if(j==1) print*,i,ni2+i+1,ni2+i-1
  	a(i    ,j,:)	= dummy (ni2+i, j,:)
  	a(ni2+i,j,:)	= dummy (i,	j,:)
   enddo
enddo
return
end


!-------------------------------------------------------------------------
subroutine update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon&
                                   ,rlon,rlat,qlon,qlat,icity,jcity,city,grid_type)
use mem_grid, only : stdlat1,stdlat2
implicit none
character*(*) city,grid_type
integer :: ng,n1,n2,i,j,i1,i2,j1,j2,icity,jcity
real  :: qlat,qlon,deltax,deltay,plat,plon,qx,qy,dx,dy,dxm,dym
real  :: xt(n1),yt(n2)
real, dimension(n1,n2) :: rlon,rlat


!- local var
real ::  xt_grid_type(n1),yt_grid_type(n2) &
        ,deltax_grid_type,deltay_grid_type
	
! - set parameters needed according to the grid type

if(grid_type == 'll') then

        xt_grid_type(:) = rlon(:,1)
        yt_grid_type(:) = rlat(1,:)
	qx = qlon
        qy = qlat
	deltax_grid_type= xt_grid_type(2)-xt_grid_type(1)  ! == grid_resolucao_lon
	deltay_grid_type= yt_grid_type(2)-yt_grid_type(1)  ! == grid_resolucao_lat
        icity =  (nint((qx-xt_grid_type(1))/deltax_grid_type)) + 1	
        jcity =  (nint((qy-yt_grid_type(1))/deltay_grid_type)) + 1
        if(icity .lt. 1 .or. icity .gt. n1)  icity = -999
        if(jcity .lt. 1 .or. jcity .gt. n2)  jcity = -999
        
elseif(grid_type == 'gg') then
       call get_ij_gg(n1,n2,rlat,rlon,qlon,qlat,icity,jcity)

elseif(grid_type == 'fim') then
       call get_ij_fim(n1,n2,rlat,rlon,qlon,qlat,icity,jcity)

elseif(grid_type == 'fv3') then
       call get_ij_fv3(n1,n2,rlat,rlon,qlon,qlat,icity,jcity)

elseif(grid_type == 'rams' .or. grid_type == 'polar' .or. grid_type == 'lambert' &
        .or. grid_type == 'mercator' ) then 
        
        xt_grid_type(:) = xt(:)
        yt_grid_type(:) = yt(:)
	deltax_grid_type= deltax
	deltay_grid_type= deltay


! Transforma qlat e qlon para outras projecoes
        if(grid_type .eq. 'lambert')then
          call ll_lc2(qlat,qlon,plat,plon,stdlat1,stdlat2,qx,qy)   !tks
        elseif(grid_type .eq. 'mercator')then
          call llij_merc(qlat,qlon,plat,plon,stdlat1,qx,qy)   !tks
        else
          call ge_to_xy(plat,plon,stdlat2,qlon,qlat,qx,qy)
        endif

!- 10012008
!-use this only for grid with cte spacing
      !- new way: faster/more precise/smarter
      !icity =	(nint((qx-xt_grid_type(1))/deltax_grid_type)) + 1	 
      !jcity =	(nint((qy-yt_grid_type(1))/deltay_grid_type)) + 1
      !if(icity .lt. 1 .or. icity .gt. n1)  icity = -999
      !if(jcity .lt. 1 .or. jcity .gt. n2)  jcity = -999
      !return

!- use this for generic grid (old way )

! Define grid box where the city belongs
	do i = 1,n1
	if(qx.le.xt_grid_type(i)) go to 100
	enddo
        
	!print*,'1: Cidade ',city, 'esta fora do dominio do modelo'
	icity=-999;jcity=-999
        return
  100	continue
  	i1 = i - 1
  	i2 = i
!
	do j = 1,n2
	if(qy.le.yt_grid_type(j)) go to 200
	enddo

	!print*,'2: Cidade ',city, 'esta fora do dominio do modelo'
        icity=-999;jcity=-999
        return
  200	continue
  	j1 = j - 1
  	j2 = j
	
	if(i1.eq.0.or.j1.eq.0) then
	 !print*,'3: Cidade ',city, 'esta fora do dominio do modelo'
	 icity=-999;jcity=-999
	 return
        endif
!	escolha da localizacao da cidade no rams = (icity,jcity)
!
  	dx = qx - xt(i1)
  	dy = qy - yt(j1)
	dxm = 0.5 * (xt(i2) - xt(i1)) !dxm = deltax_grid_type/2.
	dym = 0.5 * (yt(j2) - yt(j1)) !dym = deltay_grid_type/2.

	if(dx.le.dxm.and.dy.le.dym) then
         icity=i1
  	 jcity=j1
  	endif
	 
	if(dx.gt.dxm.and.dy.lt.dym) then
	 icity=i2
	 jcity=j1
        endif

  	if(dx.lt.dxm.and.dy.gt.dym) then
  	 icity=i1
	 jcity=j2
	endif
	
	if(dx.gt.dxm.and.dy.gt.dym)  then
         icity=i2
         jcity=j2
        endif 

endif

end subroutine update_emissions_by_city
!-----------------------------------------------------------------
!-------------------------------------------------------------------------
subroutine update_emissions_by_point(ng,n1,n2,xt,yt,deltax,deltay,plat,plon&
                                   ,rlon,rlat,qlon,qlat,ipoint,jpoint,grid_type)
use mem_grid, only : stdlat1,stdlat2
implicit none
character*(*) grid_type
integer :: ng,n1,n2,i,j,i1,i2,j1,j2,ipoint,jpoint
real  :: qlat,qlon,deltax,deltay,plat,plon,qx,qy,dx,dy,dxm,dym
real  :: xt(n1),yt(n2)
real, dimension(n1,n2) :: rlon,rlat


!- local var
real ::  xt_grid_type(n1),yt_grid_type(n2) &
        ,deltax_grid_type,deltay_grid_type
	
! - set parameters needed according to the grid type

if(grid_type == 'll') then

        xt_grid_type(:) = rlon(:,1)
        yt_grid_type(:) = rlat(1,:)
	qx = qlon
        qy = qlat
	deltax_grid_type= xt_grid_type(2)-xt_grid_type(1)  ! == grid_resolucao_lon
	deltay_grid_type= yt_grid_type(2)-yt_grid_type(1)  ! == grid_resolucao_lat
        ipoint =  (nint((qx-xt_grid_type(1))/deltax_grid_type)) + 1	
        jpoint =  (nint((qy-yt_grid_type(1))/deltay_grid_type)) + 1
        if(ipoint .lt. 1 .or. ipoint .gt. n1)  ipoint = -999
        if(jpoint .lt. 1 .or. jpoint .gt. n2)  jpoint = -999
        
elseif(grid_type == 'gg') then
       call get_ij_gg(n1,n2,rlat,rlon,qlon,qlat,ipoint,jpoint)

elseif(grid_type == 'fim') then
       call get_ij_fim(n1,n2,rlat,rlon,qlon,qlat,ipoint,jpoint)

elseif(grid_type == 'fv3') then
       call get_ij_fv3(n1,n2,rlat,rlon,qlon,qlat,ipoint,jpoint)

elseif(grid_type == 'rams' .or. grid_type == 'polar' .or. grid_type == 'lambert' &
                                                     .or. grid_type == 'mercator') then 
        
        xt_grid_type(:) = xt(:)
        yt_grid_type(:) = yt(:)
	deltax_grid_type= deltax
	deltay_grid_type= deltay


! Transforma qlat e qlon para outras projecoes
        if(grid_type .eq. 'lambert')then
          call ll_lc2(qlat,qlon,plat,plon,stdlat1,stdlat2,qx,qy)   !tks
        elseif(grid_type .eq. 'mercator')then
          call llij_merc(qlat,qlon,plat,plon,stdlat1,qx,qy)   !tks
        else
          call ge_to_xy(plat,plon,stdlat2,qlon,qlat,qx,qy)
        endif


!old        call ge_to_xy(plat,plon,qlon,qlat,qx,qy)



!- use this for generic grid (old way )

! Define grid box where the city belongs
	do i = 1,n1
	if(qx.le.xt_grid_type(i)) go to 100
	enddo
        
	print*,'Ponto fora do dominio'
	ipoint=-999;jpoint=-999
        return
  100	continue
  	i1 = i - 1
  	i2 = i
!
	do j = 1,n2
	if(qy.le.yt_grid_type(j)) go to 200
	enddo

	print*,'Ponto fora do dominio'
        ipoint=-999;jpoint=-999
        return
  200	continue
  	j1 = j - 1
  	j2 = j
	
	if(i1.eq.0.or.j1.eq.0) then
	 print*,'Ponto fora do dominio'
	 ipoint=-999;jpoint=-999
	 return
        endif
!	escolha da localizacao da cidade no rams = (ipoint,jpoint)
!
  	dx = qx - xt(i1)
  	dy = qy - yt(j1)
	dxm = 0.5 * (xt(i2) - xt(i1)) !dxm = deltax_grid_type/2.
	dym = 0.5 * (yt(j2) - yt(j1)) !dym = deltay_grid_type/2.

	if(dx.le.dxm.and.dy.le.dym) then
         ipoint=i1
  	 jpoint=j1
  	endif
	 
	if(dx.gt.dxm.and.dy.lt.dym) then
	 ipoint=i2
	 jpoint=j1
        endif

  	if(dx.lt.dxm.and.dy.gt.dym) then
  	 ipoint=i1
	 jpoint=j2
	endif
	
	if(dx.gt.dxm.and.dy.gt.dym)  then
         ipoint=i2
         jpoint=j2
        endif 

endif

end subroutine update_emissions_by_point

!-----------------------------------------------------------------
!-------------------------------------------------------------------------
subroutine update_emissions_by_runway(ng,n1,n2,xt,yt,deltax,deltay,plat,plon&
                                   ,rlon,rlat,qlon,qlat,icity,jcity,grid_type,stdlat2)
implicit none
character*(*) grid_type
integer :: ng,n1,n2,i,j,i1,i2,j1,j2,icity,jcity
real  :: qlat,qlon,deltax,deltay,plat,plon,qx,qy,dx,dy,dxm,dym,stdlat2
real  :: xt(n1),yt(n2)
real, dimension(n1,n2) :: rlon,rlat


!- local var
real ::  xt_grid_type(n1),yt_grid_type(n2) &
        ,deltax_grid_type,deltay_grid_type
	
if(grid_type == 'lambert') stop 'update_emissions_by_city not ready for lambert'
! - set parameters needed according to the grid type

if(grid_type == 'll') then

        xt_grid_type(:) = rlon(:,1)
        yt_grid_type(:) = rlat(1,:)
	qx = qlon
        qy = qlat
	deltax_grid_type= xt_grid_type(2)-xt_grid_type(1)  ! == grid_resolucao_lon
	deltay_grid_type= yt_grid_type(2)-yt_grid_type(1)  ! == grid_resolucao_lat
        icity =  (nint((qx-xt_grid_type(1))/deltax_grid_type)) + 1	
        jcity =  (nint((qy-yt_grid_type(1))/deltay_grid_type)) + 1
        if(icity .lt. 1 .or. icity .gt. n1)  icity = -999
        if(jcity .lt. 1 .or. jcity .gt. n2)  jcity = -999
        

elseif(grid_type == 'rams' .or. grid_type == 'polar' .or. grid_type == 'lambert') then 
        
        xt_grid_type(:) = xt(:)
        yt_grid_type(:) = yt(:)
	deltax_grid_type= deltax
	deltay_grid_type= deltay


! Transforma qlat e qlon para polar estereografico.
        call ge_to_xy(plat,plon,stdlat2,qlon,qlat,qx,qy)


!- 10012008
!-use this only for grid with cte spacing
      !- new way: faster/more precise/smarter
      !icity =	(nint((qx-xt_grid_type(1))/deltax_grid_type)) + 1	 
      !jcity =	(nint((qy-yt_grid_type(1))/deltay_grid_type)) + 1
      !if(icity .lt. 1 .or. icity .gt. n1)  icity = -999
      !if(jcity .lt. 1 .or. jcity .gt. n2)  jcity = -999
      !return

!- use this for generic grid (old way )

! Define grid box where the city belongs
	do i = 1,n1
	if(qx.le.xt_grid_type(i)) go to 100
	enddo
        
	!print*,'1: Cidade ',city, 'esta fora do dominio do modelo'
	icity=-999;jcity=-999
        return
  100	continue
  	i1 = i - 1
  	i2 = i
!
	do j = 1,n2
	if(qy.le.yt_grid_type(j)) go to 200
	enddo

	!print*,'2: Cidade ',city, 'esta fora do dominio do modelo'
        icity=-999;jcity=-999
        return
  200	continue
  	j1 = j - 1
  	j2 = j
	
	if(i1.eq.0.or.j1.eq.0) then
	 !print*,'3: Cidade ',city, 'esta fora do dominio do modelo'
	 icity=-999;jcity=-999
	 return
        endif
!	escolha da localizacao da cidade no rams = (icity,jcity)
!
  	dx = qx - xt(i1)
  	dy = qy - yt(j1)
	dxm = 0.5 * (xt(i2) - xt(i1)) !dxm = deltax_grid_type/2.
	dym = 0.5 * (yt(j2) - yt(j1)) !dym = deltay_grid_type/2.

	if(dx.le.dxm.and.dy.le.dym) then
         icity=i1
  	 jcity=j1
  	endif
	 
	if(dx.gt.dxm.and.dy.lt.dym) then
	 icity=i2
	 jcity=j1
        endif

  	if(dx.lt.dxm.and.dy.gt.dym) then
  	 icity=i1
	 jcity=j2
	endif
	
	if(dx.gt.dxm.and.dy.gt.dym)  then
         icity=i2
         jcity=j2
        endif 

endif

end subroutine update_emissions_by_runway
!-----------------------------------------------------------------
subroutine interpol(dlat1,dlat2,dlon1,dlon2         &
	           ,data11,data21,data12,data22    &
		   ,TX)
!longitude interpol:
      tx_co11 = (data11*dlon2 + data21*dlon1 ) / (dlon1+dlon2)

      tx_co12 = (data12*dlon2 + data22*dlon1 ) / (dlon1+dlon2)
		
!lat interpol:

      tx =  (tx_co11*dlat2 + tx_co12*dlat1)/(dlat1+dlat2)
return
end
!-----------------------------------------------------------------
subroutine apply_land_restriction(n1,n2,rland,qsc)
implicit none
integer :: n1,n2,i,j
real    :: mass_1, mass_2, mass_3, fx
real, dimension(n1,n2)  :: qsc,rland

mass_1=0.
mass_2=0.
do i=1,n1
do j=1,n2
  mass_1= mass_1 + qsc(i,j)
  mass_2= mass_2 + qsc(i,j)*rland(i,j)
!  print*,mass_1,mass_2,qsc(i,j),rland(i,j)
enddo
enddo

fx = mass_1/(mass_2+1.e-16)

mass_3=0.
do i=1,n1
do j=1,n2
  qsc(i,j) = qsc(i,j)*rland(i,j)*fx
  mass_3 = mass_3 + qsc(i,j)
enddo
enddo

!print*,'============================================================='
!print*,'Apllying LAND mask correction'
!print*, mass_1,mass_2,mass_3,fx
!print*,'============================================================='
return
end
!
!-------------------------------------------------------------------
subroutine date1(ib,iy,im,id)
implicit none
integer ib,iy,im,id
iy=int(ib/10000)
im=int( (ib-iy*10000)/100 )
id=ib - (iy*10000 + im*100)
end subroutine date1
!----------------------------------------------------------------------------------------------------
!
subroutine read_logan(ng,n1,n2,rlat,rlon,rland,qsco2,qsco,qspm25)
use grid_dims, only : pi180, r_earth
!* We have prepared 1x1 (this means  1 degree by 1 degree) distributions 
!  for three types of biomass burning in the developing world:
!
!   woodfuels burning (including both fuelwood and charcoal burning),
!   residue and dung used as biofuels, 
!   burning of residues in the fields.  
!
!* All three of these files contain amount of biomass burned per box
!  in units of Tg dry matter.
!
!  The I,J, numbering uses the I-index to move east to west around the globe
!   starting with its I = 1 box bordering (left edge) on the international
!   date line.  The J ordering starts with the J = 1 box bottom edge on the
!   South Pole and moves north.
!
!
!* The woodfuels file is called WDF.1x1 and total of all 1x1 boxes is 1323.17 Tg.
!* The residue/dung biofuels file is CMB.1x1 and total is	       732.82 Tg.
!* The burning-in-fields file	is BIF.1x1 and totals		       406.36 Tg.
!
!
!!* The derivation of these distributions is discussed in the paper by Yevich
!and Logan, 2003.  If you use any of these files, please cite the following:
!
!Yevich, R. and J.A. Logan, An assessment of biofuel use and burning of
!agricultural waste in the developing world, Global Biogeochemical Cycles,
!doi:  2002gb001952 (in press 2003).
!-----------------------------
real, dimension(n1,n2)  :: rlat,rlon,qsco2,qsco,qspm25,rland
parameter (nlon=360, nlat=180)
!definicoes para leitura dos arquivos da base 'LOGAN'
real data_BIF(360,180),data_CMB(360,180),data_WDF(360,180),data_dummy(360,180)
real long(nlon),lat(nlat)
real longLogan(nlon),latLogan(nlat)
real ef_bif(5),ef_cmb(5),ef_wdf(5)

!Emission factors (g/kg)   CO2   CO    CH4  NOx  PM25
data (ef_bif(i),i=1,5)   /1132., 51., 2.2, 2.5, 5.4/ ! burning of residues in the fields.
data (ef_cmb(i),i=1,5)   /1156., 81., 4.22,2.22, 3.9/ ! residue and dung used as biofuels.
data (ef_wdf(i),i=1,5)   /1467., 70., 4.5, 2.14, 7.2/ ! woodfuels burnings.(fuelwood and 
                                                      !                 charcoal burning).
character*20 filename

  print*,'============================================================='
  filename='BIF.1x1'
  print *,'LOGAN source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_bif(i,j),i=1,360),j=1,180)
  close(11)
1 format(8e10.3)

  filename='CMB.1x1'
  print *,'LOGAN source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_cmb(i,j),i=1,360),j=1,180)
  close(11)

  filename='WDF.1x1'
  print *,'LOGAN source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_wdf(i,j),i=1,360),j=1,180)
  close(11)
  print*,'============================================================='


!  open(11,form='unformatted',access='direct',status='unknown',recl=4*nlon*nlat)
!  nrec=0
!  nrec=nrec+1
!  write(11,rec=nrec) data_wdf
!  nrec=nrec+1
!  write(11,rec=nrec) data_cmb
!  nrec=nrec+1
!  write(11,rec=nrec) data_bif
!  close(11)

!  do i=1,nlon
!   do j=1,nlat
!      bif=bif+ data_BIF(i,j)
!      cmb=cmb+ data_CMB(i,j)
!      wdf=wdf+ data_WDF(i,j)
!   if(data_BIF(i,j)+data_CMB(i,j)+data_WDF(i,j).gt.1.e-10) &
!   print*,i,j,data_BIF(i,j),data_CMB(i,j),data_WDF(i,j)
!   enddo
!  enddo
!  print*,bif,cmb,wdf
!    
  long(1) = -180.
  do i=2,nlon
   long(i)=long(i-1) + 1.
!   print*,i,long(i)
  enddo

  lat(1) = -90.
  do j=2,nlat
   lat(j)=lat(j-1) + 1.
!   print*,j,lat(j)
  enddo


! Os valores estao em Teragramas de materia seca por ano: converta os para 
! kg[gas] por dia por metro quadrado

dlat=1.*pi180
dlon=1.*pi180
do j=1,180
 area = cos(0.5*(lat(j)+lat(j))*pi180) * (r_earth**2) *dlat * dlon
 fx = 1.e+9     * & !convert para kg
      (1./365.) * & ! converte para dia
      (1./area)     ! converte para m2 ==> kg[materia seca]/ (m^2 dia )  
 do i=1,360
  data_BIF(i,j) = data_BIF(i,j) * fx
  data_CMB(i,j) = data_CMB(i,j) * fx
  data_WDF(i,j) = data_WDF(i,j) * fx
 enddo
enddo


!interpolacao para a grade do modelo:
! Pontos de grade do dado original
latlogan(1)=-89.5
do j=2,179
 latlogan(j) =0.5*(lat(j+1)+lat(j))
!print*,'latlogan=',latlogan(j)
enddo

longlogan(1)=-179.5
longlogan(360)=179.5 
do i=2,359
 longlogan(i)=0.5*(long(i+1)+long(i))
!longlogan(360-k+1)=0.5*(longmax(k)+longmin(k))
!if(longlogan(360-k+1) .gt. 180) longlogan(360-k+1) =longlogan(360-k+1) -360. 
!print*,i,longlogan(i),longlogan(360)
enddo
!
do kgas=1,5
 if(kgas <= 2 .or. kgas == 5) then ! somente para CO2, CO e PM25

!  print*,'LOGAN database      kgas=',kgas
   
  do i=1,nlon
   do j=1,nlat
       data_dummy(i,j)=( ef_bif(kgas)*data_BIF(i,j) + &
                         ef_cmb(kgas)*data_CMB(i,j) + &
		         ef_wdf(kgas)*data_WDF(i,j) ) *0.001 ! 0.001 convert fator de
		         				     !emissao de g/kg para kg/kg
   enddo
  enddo
!neste ponto data_dummy esta em kg[gas]/m^2/dia


! interpola para os pontos de grade do modelo 
! Dominio das fontes reduzido de 2 pontos interiores:
 do i=3,n1-2
  do j=3,n2-2

!rlat(1,1) = -30.
!rlon(i,1) = 40.   
 rrlat=rlat(i,j)
 rrlon=rlon(i,j)
   
   
    do kk= 1,360
!     print*,rlon(i,j),rrlon,longlogan(kk)
     if(rrlon .le. longlogan(kk) ) go to 100
    enddo
100 continue
      k1 = kk-1
      k2 = kk
      
      
      dlon1=     rrlon - longlogan(k1)
      dlon2= - ( rrlon - longlogan(k2) )

      if(k1.eq.0) then 
      k1=360
      dlon1=     rrlon - longlogan(k1) +360.
      endif
      if(k2.gt.360) then
      k2=1
      dlon2= - ( rrlon - longlogan(k2) - 360.)
      endif
   
    do ii= 1,180
!     print*,rrlat,latlogan(ii)
     if(rrlat .le. latlogan(ii)  ) go to 200
    enddo
200 continue
      i1= ii-1
      i2= ii
!     print*,i1,i2,rrlat,latlogan(i1)
!
      dlat1=     rrlat - latlogan(i1)
      dlat2= - ( rrlat - latlogan(i2) )
      


      call interpol(dlat1,dlat2,dlon1,dlon2         &
	             ,data_dummy(k1,i1),data_dummy(k2,i1) &
                     ,data_dummy(k1,i2),data_dummy(k2,i2) &
		     ,TX)

      if(kgas == 1)  qsco2(i,j) =   TX ! kg[gas]/m^2/dia
      if(kgas == 2)   qsco(i,j) =   TX ! kg[gas]/m^2/dia
      if(kgas == 5) qspm25(i,j) =   TX ! kg[gas]/m^2/dia

!      if(kgas == 2)  then
!       if(qsco(i,j).gt.1.e-10) then 
!        print*,qsco(i,j),i1,i2,k1,k2
!        print*,data_dummy(k1,i1),data_dummy(k2,i1) &
!                     ,data_dummy(k1,i2),data_dummy(k2,i2)
!       endif
!      stop
!      endif

!longitude interpol:
!      tx_co11 = (data_dummy(k1,i1)*dlon2 + data_dummy(k2,i1)*dlon1 ) / &
!                (dlon1+dlon2)
!
!      tx_co12 = (data_dummy(k1,i2)*dlon2 + data_dummy(k2,i2)*dlon1 ) / &
!                (dlon1+dlon2)	
!lat interpol:
!
!      tx_co =  (tx_co11*dlat2 + tx_co12*dlat1)/(dlat1+dlat2)


   enddo
  enddo      
 endif
enddo
  
!Aplica restricao de fontes emissoras somente sobre o continente e ajusta a massa total
!usando o parametro LAND = % de terra.
call apply_land_restriction(n1,n2,rland,qsco2)
call apply_land_restriction(n1,n2,rland,qsco)
call apply_land_restriction(n1,n2,rland,qspm25)

return
end
!----------------------------------------------------------------------------------------------------

subroutine determine_julian_day(iyear,imon,iday,cyear,cmonX,cdayX,jday,cjday)
 implicit none
 integer julday
 integer, intent(in):: imon,iday,iyear
 character (len=*), intent(out)  :: cjday,cyear,cmonX,cdayX
 integer, intent(out):: jday
 integer i
 character*2 cday(31),cmon(12)
 data (cday(i),i=1,31) /'01','02','03','04','05','06','07','08','09','10',  &
                       '11','12','13','14','15','16','17','18','19','20',  &
                       '21','22','23','24','25','26','27','28','29','30', '31'/
 data (cmon(i),i=1,12) /'01','02','03','04','05','06','07','08','09','10',  &
                       '11','12'/
 
    cmonX=cmon(imon); cdayX=cday(iday)
    write(cyear,'(I4)') iyear
    
    jday=julday(imon,iday,iyear)
    write(cjday,'(i3.3)') jday

end subroutine determine_julian_day

!--------------------------------------------------------------------------------------
subroutine get_area_ll(area,im,jm)
   use grid_dims_out, only : dep_glat, dep_glon
   use mem_grid, only : nnxp, nnyp
   implicit none
   real, parameter :: radius_earth     = 6367000. 
   real area(im,jm)
   integer :: i, j,im,jm,ng
   real :: d2r, pi, factor, phi_1, phi_2,dlon,dlat

   
   !-for lat-lon only one grid is allowed
   ng=1
   
   !- grid resolution
   dlon = dep_glon(2,ng) ! 360.0 / im
   dlat = dep_glat(2,ng) !180.0 / ( jm - 1)
   
   !- check arrays dimensions
   if(im .ne.  nnxp(ng)) stop 'im .ne.  nnxp(ng)' 
   if(jm .ne.  nnyp(ng)) stop 'jm .ne.  nnyp(ng)' 
   
   pi = 4.0 * atan ( 1.0 )
   d2r = pi / 180.0
   

!  Gridbox area
!  ------------
!  factor = 2. * pi * ( radius_earth**2 ) / im               !srf 28/05/2011 - only for global domains
   factor = 2. * pi * ( radius_earth**2 ) * (im*dlon/360.)/im !srf 28/05/2011 - in general
   
   do j = 2, jm-1
      phi_1  = d2r * ( dep_glat(1,ng) + (j-0.5-1)*dlat ) ! cell edges
      phi_2  = d2r * ( dep_glat(1,ng) + (j+0.5-1)*dlat ) ! cell edges
      area(1:im,j) = factor * abs ( sin(phi_1) - sin(phi_2) )
      !print*,'area=',area(1:1,j),factor,dep_glat(1,ng) , (j+0.5-1)*dlat, abs ( sin(phi_1) - sin(phi_2) )
   end do

!  polar caps
!  ----------
   phi_1  = d2r * ( dep_glat(1,ng) )            ! cell edges
   phi_2  = d2r * ( dep_glat(1,ng) + 0.5*dlat ) ! cell edges

   area(1:im,1) = factor * abs ( sin(phi_1) - sin(phi_2) )
   area(1:im,jm) = area(1:im,1)

!#ifdef DEBUG
!   factor = ( (radius_earth * d2r) **2 ) * dlon * dlat
!   do j = 2, jm-1
!      phi_1 = factor * cos ( d2r * lat(j) )
!      print *, j, area(j), phi_1, phi_1/area(j)
!   end do
!#endif

end subroutine get_area_ll
!--------------------------------------------------------------------------------------------------------
subroutine get_area_fim(area,n1,n2)
use grid_dims_out, only : fim_data_dir
  implicit none
  real,intent(OUT):: area(n1,n2)
  integer,intent(IN) :: n1,n2
  
!  local var
  real, allocatable :: fim_area (:)
  integer           :: npoints,nprox,i,j
  logical           :: there
  character(16)     :: header
  real              :: dum
  
   
  npoints = n1
  ALLOCATE(fim_area(npoints))   
!
!- read area from FIM data file
! inquire(file='./glvl.dat',exist=there)
  inquire(file=trim(fim_data_dir)//'/glvl.dat',exist=there)
  if(.not.there) then
     print*,'file not found: ./glvl.dat' 
     stop 'at get_area_fim routine'
  endif
! open(unit=28,file="./glvl.dat", form="unformatted")
  open(unit=28,file=trim(fim_data_dir)//"/glvl.dat", form="unformatted")
     read(28)header
     read(28)header
     read(28)dum
     read(28)dum
     read(28)nprox
     do i=1,6
     read(28)nprox
     enddo
     do i=1,6
     read(28)nprox
     enddo
     read(28)fim_area
  close(28)

  j=1
  do i=1,n1     
    area(i,j)=fim_area(i)
  enddo

  deallocate(fim_area)

end subroutine get_area_fim

!--------------------------------------------------------------------------------------------------------

subroutine get_area_rams(area,im,jm,xt,yt,xm,ym)
implicit none
real, parameter :: radius_earth     = 6367000.  
real area(im,jm),fmapt(im,jm),dxt(im,jm),dyt(im,jm)
real xt(im),yt(jm),xm(im),ym(jm)
real c1,xt2,yt2
integer im,jm,i,j
c1 = (2. * radius_earth) ** 2
do j = 1,jm
   do i = 1,im
      xt2 = xt(i) * xt(i)
      yt2 = yt(j) * yt(j)

      fmapt(i,j) = 1. + (xt2 + yt2) / c1
   enddo
enddo

do j = 1,jm
   do i = 2,im
      dxt(i,j)=fmapt(i,j)/(xm(i)-xm(i-1))
   enddo
   dxt(1,j)=dxt(2,j)*fmapt(1,j)/fmapt(2,j)  
enddo

do i = 1,im
   do j = 2,jm
      dyt(i,j)=fmapt(i,j)/(ym(j)-ym(j-1))
   enddo
   dyt(i,1)=dyt(i,2)*fmapt(i,1)/fmapt(i,2)
enddo

!- area per grid box
 area(:,:) = 1./(dxt(:,:)*dyt(:,:))
!print*,'area=',area
!do j = 1,jm
!   do i = 1,im
!      call xy_ll(glat(i,j),glon(i,j),platn(ngrid),plonn(ngrid),  &
!         stdlat2,xt(i),yt(j))
!   enddo
!enddo
end subroutine get_area_rams
!------------------------------------------------------------------------------------------------------
subroutine get_area_gg(area,im,jm,rlat,rlon) 
!   use grid_dims_out, only : dep_glat, dep_glon
   use mem_grid, only : nnxp, nnyp
   implicit none
   real, parameter :: radius_earth     = 6367000. 
   real area(im,jm),rlat(im,jm), rlon(im,jm)
   integer :: i, j,im,jm,ng
   real :: d2r, pi, factor, phi_1, phi_2,dlon,dlat,lambda_1,lambda_2

   
   !-for Gaussian only one grid is allowed
   ng=1
   
   !- check arrays dimensions
   if(im .ne.  nnxp(ng)) stop 'im .ne.  nnxp(ng) at gg grid' 
   if(jm .ne.  nnyp(ng)) stop 'jm .ne.  nnyp(ng) at gg grid'
   
   pi = 4.0 * atan ( 1.0 )
   d2r = pi / 180.0
   !initial value for checking
   area=-9999.
!  Gridbox area for Gaussian grids:
!  ------------
   factor = radius_earth**2 
   do i=1,im-1
     do j = 1, jm-1
      lambda_1 = d2r * rlon(i,j)
      lambda_2 = d2r * rlon(i+1,j)
      
      phi_1  = d2r *  rlat(i,j)
      phi_2  = d2r *  rlat(i,j+1)
      area(i,j) = factor*abs(lambda_2 - lambda_1)* abs ( sin(phi_1) - sin(phi_2) )
  
   !print*,'area=',area(i,j),rlon(i,j),rlon(i+1,j),rlat(i,j),rlat(i,j+1)
   enddo; enddo
   
   area(1:im,jm)=area(1:im,1)
   area(im,1:jm)=area(1,1:jm)

!   do i=1,im
!     do j = 1, jm
!    if(area(i,j) < 0. ) print*,'area2=',area(i,j),i,j
!    enddo
!    enddo
!  stop 3333
  ! polar caps
  ! ----------
  ! phi_1  = d2r * ( dep_glat(1,ng) )            ! cell edges
  ! phi_2  = d2r * ( dep_glat(1,ng) + 0.5*dlat ) ! cell edges

  !area(1:im,1) = factor * abs ( sin(phi_1) - sin(phi_2) )
  !area(1:im,jm) = area(1:im,1)

end subroutine get_area_gg
!------------------------------------------------------------------------------------------------------
subroutine get_area_byX(r2d,im,jm,km,rlatx,rlonx) 
! Use location of corner grid cells to determine area on sphere
!   use grid_dims_out, only : dep_glat, dep_glon
   use mem_grid, only : nnxp, nnyp
   implicit none
   real, parameter :: radius_earth     = 6367000. 
   integer :: i, j,im,jm,km,ng
   real r2d(im,jm,km)
   real rlatx(im+1,jm+1), rlonx(im+1,jm+1)
   REAL A1,A2,A3,A4,B1,B2,B3,B4
   real :: d2r, pi, factor, phi_1, phi_2,dlon,dlat,lambda_1,lambda_2

   
   !-for Gaussian only one grid is allowed
   ng=1
   
   !- check arrays dimensions
   if(im .ne.  nnxp(ng)) stop 'im .ne.  nnxp(ng) at gg grid' 
   if(jm .ne.  nnyp(ng)) stop 'jm .ne.  nnyp(ng) at gg grid'
   
   pi = 4.0 * atan ( 1.0 )
   d2r = pi / 180.0
   !initial value for checking
   r2d(:,:,3)=-9999.
!  Gridbox area for Gaussian grids:
!  ------------
   factor = (d2r*radius_earth)**2 
   do i=1,im
     do j = 1, jm
      A1=rlonx(i,j)-r2d(i,j,2)
      A2=rlonx(i,j+1)-r2d(i,j,2)
      A3=rlonx(i+1,j+1)-r2d(i,j,2)
      A4=rlonx(i+1,j)-r2d(i,j,2)
      B1=rlatx(i,j)-r2d(i,j,1)
      B2=rlatx(i,j+1)-r2d(i,j,1)
      B3=rlatx(i+1,j+1)-r2d(i,j,1)
      B4=rlatx(i+1,j)-r2d(i,j,1)
      r2d(i,j,3)=.5*ABS(A1*B2+A2*B3+A3*B4+A4*B1-B1*A2-B2*A3-B3*A4-B4*A1)*factor ! Area (m^2)
   !print*,'area=',area(i,j),rlon(i,j),rlon(i+1,j),rlat(i,j),rlat(i,j+1)
   enddo; enddo
end subroutine get_area_byX
!--------------------------------------------------------------------------------------------------------
subroutine check_consistency

use mem_grid
use grid_dims_out

if( (maxval(nnxp(1:ngrids)) .gt. nxpmax) .or. &
    (maxval(nnyp(1:ngrids)) .gt. nypmax) ) then
    print*,'too many nnxp or nnyp points'
    print*,'increase NXPMAX/NYPMAX at grid_dims.f90'
    stop 22
endif

if(use_bbem .le. 0 .and. use_gfedv2 /= 1 ) use_bbem_plumerise = 0

if(merge_GFEDv2_bbem == 0) then
 if(use_bbem .ne. 0 .and. use_gfedv2 == 1) &
     stop 'simultaneous use of 3bem and gfedv2 is not allowed when the merging is not desired'
endif


if(chem_out_format == 'VFM') chem_out_format='vfm'
if(chem_out_format == 'HDF') chem_out_format='hdf'
if(chem_out_format /= 'vfm' .and. chem_out_format /= 'hdf' ) &
      stop ' output format not defined use vfm or hdf'
if(grid_type =='RAMS') grid_type ='rams'
if(grid_type =='LL') grid_type ='ll'
if(grid_type =='GG') grid_type ='gg'
if(grid_type =='POLAR') grid_type ='polar'
if(grid_type =='LAMBERT') grid_type ='lambert'
if(grid_type =='MERCATOR') grid_type ='mercator'
if(grid_type =='FIM') grid_type ='fim'
if(grid_type =='FV3') grid_type ='fv3'
if( use_volcanoes == 1 .and. use_degass_volcanoes == 1 ) &
 stop 'eruption and degassing emission are not allowed simultaneuosly'

end subroutine check_consistency
!------------------------------------------------------------------------------------------------------

subroutine write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
use chem1_list, only: chemical_mechanism
use aer1_list, only: aerosol_mechanism
use grid_dims_out
use emiss_vars_emissions, only : emiss_nspecies,number_sources,emiss_spc_name,src_name,emiss_plume_name
use bbbem_plumerise, only : nveg_agreg,veg_name,spc_suf
use volcanoes_emissions, only	: INJH,DURA,ash_size_dist
use volc_degassing_emissions, only : PLUM_HEIGTH,ELEV
implicit none
integer iunit_out
integer ng,nxp,nyp,nvert,ihour,iday,imon,iyear,isp,nsrc,iv
real zlev(nvert)

!-- local var
integer imin,i,ident_chem_mec,ident_aer,ident_aer_mode
character*15 chdate,chstep,xchstep
character*3 cmo(12)
data cmo/'jan','feb','mar','apr','may','jun','jul','aug','sep', &
	'oct','nov','dec'/	  

imin=0
chdate='00:00z00mmm1900'
write(chdate(1:2),'(i2.2)') ihour
write(chdate(4:5),'(i2.2)') imin
write(chdate(7:8),'(i2.2)') iday
chdate(9:11)=cmo(imon)(1:3)
write(chdate(12:15),'(i4.2)') iyear
xchstep='          1dy'

!  write initial header 
if(isp == 0) then

write(iunit_out,*)  nxp,(dep_glon(i,ng),i=1,2)
write(iunit_out,*)  nyp,(dep_glat(i,ng),i=1,2)
write(iunit_out,*)  chdate(7:15)
write(iunit_out,*)  trim(chemical_mechanism(1:len_trim(chemical_mechanism))),"   ",&
                    trim( aerosol_mechanism(1:len_trim( aerosol_mechanism)))
!write(iunit_out,*) 'nxp= ', nxp,(dep_glon(i,ng),i=1,2)
!write(iunit_out,*) 'nyp= ', nyp,(dep_glat(i,ng),i=1,2)
!write(iunit_out,*) 'date=	    ',chdate(7:15)
!write(iunit_out,*) 'nvar=',nvar
return
endif
if(iv == 0) then
  call get_specie_number_of_chem_mechanism(emiss_spc_name(isp,nsrc),ident_chem_mec,ident_aer,ident_aer_mode)
!  write(iunit_out,*) 'specie= ',emiss_spc_name(isp,nsrc),' ident_chem_mec= ',ident_chem_mec
!  write(iunit_out,*) 'source= ',src_name(nsrc), ' unit= ', 'kg/m2/day'
  if(ident_chem_mec /= -1 .and.ident_aer == -1 ) then
  
      write(iunit_out,*) 'chemistry ' !-chemistry section
      write(iunit_out,*)  trim(emiss_spc_name(isp,nsrc)(1:len_trim(emiss_spc_name(isp,nsrc))+1))&
                                 ,ident_chem_mec, trim(src_name(nsrc)(1:len_trim(src_name(nsrc))))&
				 ,nsrc,' kg.m^-2.day^-1'
      
! WRF- including ash size distribution info
      if(trim(emiss_spc_name(isp,nsrc)(1:len_trim(emiss_spc_name(isp,nsrc))+1)) == 'ASH' )then
       write(iunit_out,*)'Volc ASH size distribution (fraction)'
       write(iunit_out,"(10F7.4)") ash_size_dist(:)
      endif
      
  elseif( ident_chem_mec == -1 .and.ident_aer /= -1 ) then     

! BRAMS- including ash size distribution info
      write(iunit_out,*) 'aerosol'   !-aerosol section
      write(iunit_out,*)  trim(emiss_spc_name(isp,nsrc)(1:len_trim(emiss_spc_name(isp,nsrc))+1))&
                                 ,ident_aer,ident_aer_mode, trim(src_name(nsrc)(1:len_trim(src_name(nsrc))))&
				 ,nsrc,' kg.m^-2.day^-1'
      if(trim(emiss_spc_name(isp,nsrc)(1:len_trim(emiss_spc_name(isp,nsrc))+1)) == 'V_ASH1') then
       write(iunit_out,*)'Volc ASH size distribution (fraction)'
       write(iunit_out,"(10F7.4)") ash_size_dist(:)
      endif
  
  
  else
      print*,'both ident_chem_mec and  ident_aer are -1'
      stop 6666
  endif

  return
endif
!---- plumerise section
if(nsrc == 2) then
if(iv > 0 .and. isp <= emiss_nspecies) then  
  call get_specie_number_of_chem_mechanism(emiss_spc_name(isp,nsrc),ident_chem_mec,ident_aer,ident_aer_mode)
  
  if(use_bbem == 1)  then 
   write(iunit_out,*) 'plume', use_bbem
   write(iunit_out,*)  trim(emiss_plume_name(isp,iv)(1:len_trim(emiss_plume_name(isp,iv))+1))&
                             ,ident_chem_mec,trim(veg_name(iv)(1:len_trim(veg_name(iv))+1)),iv,' fraction'
  endif
  if(use_bbem == 2)  then
   write(iunit_out,*) 'plumefre', use_bbem
   write(iunit_out,*)  trim(emiss_plume_name(isp,iv)(1:len_trim(emiss_plume_name(isp,iv))+1))
  endif
  return
elseif(iv > 0 .and. isp == emiss_nspecies+1) then
  write(iunit_out,*) 'plume ','fire-size: ',trim(veg_name(iv)(1:len_trim(veg_name(iv))+1)),iv, 'm^2'
  return
endif  
endif  
!---- volcanoes section
if(nsrc == 4 .and. use_volcanoes == 1) then
      write(iunit_out,*) 'volcanic-eruption'
      if(iv==INJH)         write(iunit_out,*) 'INJECT_HEIGHT',' meters'
      if(iv==DURA)         write(iunit_out,*) 'TIME_DURATION',' seconds'
      if(iv==ELEV)         write(iunit_out,*) 'VENT_ELEVATION',' meters'
      return

elseif(nsrc == 4 .and. use_degass_volcanoes == 1 ) then
      write(iunit_out,*) 'volcanic-degassing'
      if(iv==PLUM_HEIGTH)  write(iunit_out,*) 'INJECT_HEIGHT',' meters'
      if(iv==DURA)         write(iunit_out,*) 'TIME_DURATION',' seconds'
      if(iv==ELEV)         write(iunit_out,*) 'VENT_ELEVATION',' meters'
      return

endif

end subroutine write_header
!------------------------------------------------------------------------------------------------------

subroutine get_specie_number_of_chem_mechanism(emiss_spc_name,ident_chem_mec,ident_aer,ident_aer_mode)

use chem1_list, only : chem_nspecies=>nspecies,spc_chem_alloc=>spc_alloc &
                      ,spc_chem_name=>spc_name

use aer1_list, only :  aer_nspecies=>nspecies,spc_aer_alloc=>spc_alloc, nmodes&
                      ,spc_aer_name=>aer_name
implicit none
character (len=*) :: emiss_spc_name
integer ispc,ident_chem_mec, imode,ident_aer,ident_aer_mode

!default values;
ident_chem_mec =-1
ident_aer      =-1
ident_aer_mode =-1

do ispc=1,chem_nspecies ! loop at the species of chemical mechanism from SPACK
 if(emiss_spc_name == spc_chem_name(ispc)) then 
      ident_chem_mec = ispc   ! number this specie at chem1_list  table
      return
      !exit
 endif
enddo

!- aerosol section 
 do ispc=1,aer_nspecies
  do imode= 1,nmodes
    if(emiss_spc_name == spc_aer_name(imode,ispc) ) then
      ident_aer      = ispc
      ident_aer_mode = imode
      return
    endif
  enddo;enddo

end subroutine get_specie_number_of_chem_mechanism
!------------------------------------------------------------------------------------------------------
subroutine merge_bburn(ng,ngrids,n1,n2,n3,rlat,rlon,rland)

use gfedv2_emissions, only : gfedv2_nspecies=>nspecies& !don't use AeM_nspecies
                            ,gfedv2_spc_name=>AeM_spc_name,gfedv2_g

use bbbem_emissions, only : bbbem_nspecies=>nspecies& !don't use AeM_nspecies
                           ,bbbem_spc_name=>AeM_spc_name,bbbem_g

!use bbbem_plumerise, only : bbbem_plume_g           
implicit none
integer, intent(in):: ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland

!local var
integer i,j, ispc

do j = 1,n2
  do i = 1,n1

!- using GFEDv2 over the Africa for operational forecast
    if(rlon(i,j) .le. -20. ) cycle

     do ispc = 1, bbbem_nspecies
     
      if(trim(gfedv2_spc_name(ispc)) == trim(bbbem_spc_name(ispc))) & 
      bbbem_g(ispc)%src(i,j,1)  =  gfedv2_g(ispc)%src(i,j,1)
     
     enddo
  enddo
enddo

end subroutine merge_bburn

!-----------------------------------------------------------------------------
       subroutine htap2fim(npoints,icos_nprox,icos_grid4,icos_edge4)
! Stu McKeen 5/13/15
        implicit none
        integer, parameter :: nlon=3600,nlat=1800
        integer, intent(in) :: npoints
        real,	 intent(in) :: icos_grid4(2,npoints) ! grid location (ll), single precision 
        integer, intent(in) :: icos_nprox(npoints)	!number of neighbors(5 or 6)
        real,	 intent(in) :: icos_edge4(6,2,2,npoints) ! end points of edges (ll)prep_chem_sources.inp, single precision
        real, allocatable :: dlonmxrd(:)	!maximum delta longitude (radians) for each fim grid for search
        integer, allocatable, dimension(:,:) :: htap_fimN
        real*8, dimension(8) :: XPLY,YPLY
        real*8  d, d2r, dist, distance,xhld,yhld,XREF,YREF,x_tran,y_tran,xlt,xln,ref_lt
        real*8 Xhtap,Yhtap,dlatmax,dlonmax  !dlatmax = delta lat max difference (radians!) allowd in search
        integer :: i,j,NPOLY,ii,jj
        real*8 :: XLOC,YLOC,MINDST,twopi,xdif,dlonmx
! transformation from spherical (lat,lon) to plane-perpendicular of slab at reference latitude ref_lt (x,y)
        x_tran(xln,xlt)=dsin(xln)*dcos(xlt)
!       y_tran(xln,xlt,ref_lt)=dcos(ref_lt)*dsin(xlt)-dsin(ref_lt)*dcos(xlt)*dsin(1.57079633+xln)
        y_tran(xln,xlt,ref_lt)=dsin(xlt-ref_lt)+dcos(xlt)*dsin(xlt)*(1.-dcos(xln))
        d2r = 4.0*ATAN(1.0)/180.0
        twopi=360.*d2r
          if( .not. allocated (htap_fimN)) then
                  allocate(  htap_fimN(nlon,nlat) )
                  htap_fimN = 0
          endif
          WRITE(10,*)'In htap2fim,npoints= ',npoints
          CALL FLUSH(10)
          dlatmax=4./sqrt(float(npoints))
! Precalculate maximum longitudal distance for search allowed:each fim grid (radians)
       ALLOCATE(dlonmxrd(npoints))    
       dlonmxrd=-99.
       do i=1,npoints
       ii=icos_nprox(i)
       do j=1,ii
       dist=abs(icos_grid4(2,i)-icos_edge4(j,1,2,i))
       if(dist.gt.180.)dist=360.-dist
       dlonmxrd(i)=MAX(dlonmxrd(i),dist*d2r)
       enddo
       enddo
!
          do ii=1,nlat
!         do ii=34,34
          Yhtap= (-89.95 + (ii-1)*0.1)*d2r
!         dlonmax=sin(Yhtap)*sin(Yhtap)*twopi*.25+(1.-sin(Yhtap)*sin(Yhtap))*dlatmax
          dlonmax=sin(Yhtap)*sin(Yhtap)*twopi*.5+(1.-sin(Yhtap)*sin(Yhtap))*dlatmax
          do jj=1,nlon
!         do jj=1697,1697
          Xhtap=(-179.95 + (jj-1)*0.1)*d2r
          if(Xhtap.lt.0.)Xhtap=Xhtap+twopi
          do j=1,npoints ! Start search
          XREF=icos_grid4(2,j)*d2r
          if(XREF.lt.0.)XREF=XREF+twopi
          YREF=icos_grid4(1,j)*d2r
          xdif=xref-xhtap
          if(xdif.ge.twopi*.5)xdif=xdif-twopi
          if(xdif.le.(-.5*twopi))xdif=xdif+twopi
!         if(abs(yref-yhtap).lt.dlatmax.and.abs(xdif).lt.dlonmax)then  ! Limit search to nearby lats and lons
          if(abs(yref-yhtap).lt.dlatmax.and.(j.eq.1.or.j.eq.npoints.or.abs(xdif).le.dlonmxrd(j)))then  ! Limit search to nearby lats and lons
! Following forces plane contact with sphere at HTAP point - will give >1 finds at high lats 3/17/15
          XLOC=0.
          YLOC=0.
          XREF=Xhtap
          YREF=Yhtap
! Following forces plane contact with avg of HTAP point and FIM grid cntr, still >1 find at 88deg 3/17/15
!         XREF=.5*(XREF+Xhtap)
!         YREF=.5*(YREF+Yhtap)
!         XLOC=x_tran(Xhtap-xref,Yhtap)
!         YLOC=y_tran(Xhtap-xref,Yhtap,yref)
          do i=1,icos_nprox(j)
          xhld=icos_edge4(i,1,2,j)*d2r
          if(xhld.lt.0.)xhld=xhld+twopi
          yhld=icos_edge4(i,1,1,j)*d2r
          XPLY(i)=x_tran(xhld-xref,yhld)
          YPLY(i)=y_tran(xhld-xref,yhld,yref)
!         WRITE(10,'(A,I6,1P4E11.4,0P2F13.5)')'i,xhld,yhld,XPLY,YPLY=',i,xhld,yhld,XPLY(i),YPLY(i),icos_edge4(i,1,2,j),icos_edge4(i,1,1,j)
          CALL FLUSH(10)
          enddo
          NPOLY=icos_nprox(j)
          XPLY(NPOLY+1)=XPLY(1)
          YPLY(NPOLY+1)=YPLY(1)
          XPLY(NPOLY+2)=XPLY(2)
          YPLY(NPOLY+2)=YPLY(2)
          CALL PINPOL(NPOLY,XPLY,YPLY,XLOC,YLOC,MINDST)
          IF(MINDST.GE.0.)THEN
          XREF=icos_grid4(2,j)*d2r
          YREF=icos_grid4(1,j)*d2r
          WRITE(10,'(A,3I7,1P4E12.5)')'IN,ii,jj,j,htap_lon,htap_lat,xfim,yfim= ',ii,jj,j,xhtap/d2r,yhtap/d2r, &
          xref/d2r,yref/d2r
!         WRITE(10,'(A,I6,2F13.5,1Pe11.3,0P2I5)')'IN,j,lon,lat,mindst=',j,XLOC,YLOC,MINDST,ii,jj
          CALL FLUSH(10)
! took 1-hr on zeus for complete search, and not exiting on find 5/17/15
          IF(htap_fimN(jj,ii).NE.0)THEN
          WRITE(10,*)'STOPPING - > ONE GRID, II,JJ,J,FIRST=',II,JJ,J,htap_fimN(jj,ii)
          STOP'888'
          ENDIF
          htap_fimN(jj,ii)=j
          GOTO 10  ! break out of test, search found, above test for multiple finds passed in previous run
!         ELSE
!         WRITE(10,'(A,I6,2F13.5,1Pe11.3,0P2F13.5)')'OUT,j,lon,lat,mindst=',j,XLOC,YLOC,MINDST,icos_grid4(2,j),icos_grid4(1,j)
          ENDIF
          endif ! endof if(abs(yref-yhtap).lt.dlatmax.and.abs(xdif).lt.dlonmax)then  ! Limit search to nearby lats and lons
          enddo ! endof do j=1,npoint
 10       CONTINUE
          IF(htap_fimN(jj,ii).EQ.0)THEN
          WRITE(10,'(A,3I7,1P5E12.5)')'ii,jj,j,htap_lon,htap_lat,xdif,dlatmax,dlonmax= ',ii,jj,j,xhtap/d2r,yhtap/d2r, &
          xdif/d2r,dlatmax/d2r,dlonmx/d2r
          WRITE(10,*)'STOPPING - NO GRID FIND, II,JJ=',II,JJ
          STOP'777'
          ENDIF
          enddo ! endof do jj=1,nlon
          enddo ! endof do ii=1,nlat
          deallocate(dlonmxrd)
          open(11,FILE='htapLL_to_fimN.bin',FORM='UNFORMATTED')
          write(11)htap_fimN
          close(11)
          call flush(10)
!         stop'file_written,stopping'
end subroutine htap2fim
!-----------------------------------------------------------------------------
  subroutine fv3_ltln_buff(n1,n2,fv3lat,fv3lon,icall) ! Buffer FV3 lat,lons into memory
        integer, intent(in) :: n1,n2,icall
        real, intent(inout) :: fv3lat(n1,n2),fv3lon(n1,n2)
        real, allocatable :: bufflat(:,:),bufflon(:,:)
        save
 if(icall.eq.0)then
  if( allocated (bufflat))then
  deallocate(bufflat)
  deallocate(bufflon)
  endif
  allocate(bufflat(n1,n2))
  allocate(bufflon(n1,n2))
  bufflat(:,:)=fv3lat(:,:)
  bufflon(:,:)=fv3lon(:,:)
  return
 endif
  fv3lat(:,:)=bufflat(:,:)
  fv3lon(:,:)=bufflon(:,:)
end subroutine fv3_ltln_buff
!-----------------------------------------------------------------------------

!******************  *******************  ******************************
      SUBROUTINE PINPOL(NN,X,Y,XPOINT,YPOINT,MINDST)
! SUBPROGRAM PINPOL
! CHECK IF POINT IS INSIDE A GENERAL POLYGON
! Reference: "A point-in-polygon program", S.W. Sloan, Advances in
! Engineering Software, 1985, Vol. 7, No. 1, pages 45-47
!
! INPUT PARAMETERS:
! 'N' I IS THE NUMBER OF SIDES/VERTICES DEFINING THE POLYGON
! 'SMALLD' IS A SMALL DOUBLE PRECISION NUMBER
! 'LARGED' IS A LARGE DOUBLE PRECISION NUMBER
! 'X' IS A VECTOR OF NODAL X--COORDS (ANTICLOCKWISE ORDER)
! 'Y' IS A VECTOR OF NODAL Y--COORDS (ANTICLOCKWISE ORDER)
! BOTH OF THESE VECTORS MUST BE OF LENGTH N+2 WHERE
! X( N+1 )=X( 1 ), X( N+2 )=X( 2 )
! Y( N+1 )=Y( 1 ), Y( N+2 )=Y( 2 )
! 'XPOINT' IS THE X-COORD OF THE POINT TO BE TESTED
! 'YPOINT' IS THE Y-COORD OF THE POINT TO BE TESTED
! 
! OUTPUT PARAMETERS:
!
! 'MINDST'THE DISTANCE FROM THE POINT TO THE NEAREST POINT
! ON THE POLYGON
! IF 'MINDST' IS LT ZERO THEN POINT IS OUTSIDE THE POLYGON
! IF 'MINDST' IS EQ ZERO THEN POINT IS ON A SIDE OF THE POLYGON
! IF 'MINDST' IS GT ZERO THEN POINT IS INSIDE THE POLYGON
!
! NOTES:
!
! THIS IS AN IMPROVED VERSION OF THE ALGORTIHM OF NORDBECK AND RYSTEDT
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!     INTEGER  :: N,I,J
      INTEGER  :: NN
      DOUBLE PRECISION  :: X(*),Y(*),XPOINT,YPOINT,SMALLD,LARGED,D,AREA,DET, &
      MINDST, X1, Y1, X1P, Y1P, X21, Y21, T, DX, DY, C00000, C00001
      LOGICAL  :: SNEAR
!
      PARAMETER( C00000=0.0D0, C00001=1.0D0)
      PARAMETER( SMALLD=1.0D-33, LARGED=1.0D33)
!
! 'SNEAR' IS .TRUE. IF DISTANCE TO NEAREST SIDE IS LESS THAN
! DISTANCE TO NEAREST VERTEX
! 'SNEAR' IS .FALSE. IF DISTANCE TO NEAREST VERTEX IS LESS THAN
! DISTANCE TO NEAREST SIDE
! 'MINDST' IS SQUARE OP DISTANCE TO CLOSEST POINT ON THE POLYGON
!
      MINDST=LARGED
! LOOP OVER EACH SIDE DEFINING POLYGON
!
      DO 10 I=1,NN
!
! START OF SIDE HAS COORDS (X1, Y1 )
! END OF SIDE HAS COORDS (X2, 12 )
! POINT HAS COORDS (XPOINT, YPOINT)
      X1=X( I)
      Y1=Y( I)
      X21=X( I+1 )-X1
      Y21=Y( I+1 )-Y1
      X1P=X1-XPOINT
      Y1P=Y1-YPOINT
! POINTS ON INFINITE LINE DEFINED BY
! X=X1+T*( X1-X2)
! Y=Y1+T*(Y1-Y2 )
! WHERE
! T=O AT (X1,Y1)
! T=1 AT (X2, Y2)
! FIND WHERE NORMAL PASSING THROUGH (XPOINT, YPOINT)
! INTERSECTS INFINITE LINE
      T=-(X1P*X21+Y1P*Y21 )/(X21*X21+Y21*Y21)
!     WRITE(10,*)'I,X1,Y1,XPNT,YPNT,T=',I,X1,Y1,XPOINT,YPOINT,T
!     CALL FLUSH(10)
      IF(T.LT.C00000)THEN
! NORMAL DOES NOT INTERSECT SIDE
! POINT IS CLOSEST TO VERTEX (X1, Y1 )
! COMPUTE SQUARE OF DISTANCE TO THIS VERTEX
      D=X1P*X1P+Y1P*Y1P
      IF(D.LT.MINDST)THEN
! POINT IS CLOSER TO (X1, Y1) THAN ANY OTHER VERTEX OR
! SIDE
!
      SNEAR= .FALSE.
      MINDST=D
      J=I
      END IF
      ELSE IF(T.LE.C00001)THEN
!
! NORMAL INTERSECTS SIDE
!
      DX=X1P+T*X21
      DY=Y1P+T*Y21
      D=DX*DX+DY*DY
      IF(D.LT.MINDST)THEN
! POINT IS CLOSER TO THIS SIDE THAN TO ANY
! OTHER SIDE OR VERTEX
      SNEAR=.TRUE.
      MINDST=D
      J=I
      END IF
      END IF
!     WRITE(10,*)'SNEAR,D,MINDST=',SNEAR,D,MINDST
 10   CONTINUE
!     CALL FLUSH(10)
!     ISTOP=1
!     IF(ISTOP.EQ.1)STOP999
      MINDST=SQRT( MINDST )
      IF(MINDST.LT.SMALLD)THEN
!
! POINT IS ON SIDE OF POLYGON
!
      MINDST=C00000
      ELSE
      IF( SNEAR)THEN
!
! POINT IS CLOSER TO ITS NEAREST SIDE THAN TO ITS NEAREST
! VERTEX, CHECK IF POINT IS TO LEFT OR RIGHT OF THIS SIDE
! IF POINT IS TO LEFT OF SIDE IT IS INSIDE POLYGON, ELSE
! POINT IS OUTSIDE POLYGON
!
      AREA=DET(X( J),X( J+1 ),XPOINT, Y(J), Y( J+1), YPOINT)
      MINDST=SIGN( MINDST ,AREA)
      ELSE
!
! POINT IS CLOSER TO ITS NEAREST VERTEX THAN ITS NEAREST SIDE,
! CHECK IF NEAREST VERTEX IS CONCAVE
! IF THE NEAREST VERTEX IS CONCAVE THEN POINT IS INSIDE THE
! POLYGON, ELSE THE POINT IS OUTSIDE THE POLYGON
      IF( J .EQ.1)THEN
      J=NN+1
      END IF
      AREA=DET( X(J+1 ),X( J),X( J-1), Y( J+1), Y( J), Y(J-1))
      MINDST=SIGN( MINDST ,AREA)
      END IF
      END IF
      END SUBROUTINE PINPOL
      FUNCTION DET(X1,X2,X3,Y1,Y2,Y3)
!
! COMPUTE TWICE THE AREA OF THE TRIANGLE DEFINIED BY THREE POINTS
! WITH COORDS (X1, Y1 ), (X2, Y2) AND (X3, Y3) USING DETERMINANT
! FORMULA
! INPUT PARAMETERS:
!  'X1,Y1' COORDS OF POINT 1
!  'X2,Y2' COORDS OF POINT 2
!  'X3,Y3' COORDS OF POINT 3
! OUTPUT PARAMETERS,
! 'DET' TWICE THE AREA OF THE TRIANGLE DEFINED BY THE THREE POINTS
! NOTES:
! DET IS POSITIVE IF POINTS 1,2 AND 3 DEFINE TRIANGLE IN
! ANTICLOCKWISE ORDER
! DET IS NEGATIVE IF POINTS 1,2 AND 3 DEFINE TRIANGLE IN
! CLOCKWISE ORDER
! DET IS ZERO IF AT LEAST TWO OF THE POINTS ARE COINCIDENT OR IF
! ALL THREE POINTS ARE COLLINEAR
!
!
      DOUBLE PRECISION  :: X1, X2, X3, Y1, Y2, Y3, DET
!
      DET=(X1-X3 )*(Y2-Y3 )-(X2-X3 )*(Y1-Y3)
!
      END FUNCTION DET
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---------------------------------------------------------
! EMK...For plotting
subroutine write_maproj(ng,iunit,filename)

   ! Modules
   use mem_grid  ! from RAMS
   use wps_util  
   implicit none

   ! Arguments
   integer,intent(in) :: ng
   integer,intent(in) :: iunit
   character(len=*),intent(in) :: filename

   ! Local variables
   integer :: iproj_ncarg

   if (ihtran == 1) then ! Polar stereographic
      iproj_ncarg = 1
   else if (ihtran == 2) then ! Lambert conformal
      iproj_ncarg = 3
   else if (ihtran == 3) then ! Mercator
      iproj_ncarg = 9
   else
!      print*,'ERROR, invalid map projection'
!      print*,'ihtran = ',ihtran
!      stop
       return
   end if

   ! Write map projection information
!   print*,'EMK: maproj file is ',trim(filename)
   open(unit=iunit,file=trim(filename),status='unknown')
   write(unit=iunit,fmt='(A,i3)') 'grid: ',ng
   write(unit=iunit,fmt='(A,i3)') 'projection: ',iproj_ncarg
   write(unit=iunit,fmt='(A,f15.3)') 'standard_latitude1: ',stdlat1
   write(unit=iunit,fmt='(A,f15.3)') 'standard_latitude2: ',stdlat2
   write(unit=iunit,fmt='(A,f15.3)') 'standard_longitude: ',stdlon
   write(unit=iunit,fmt='(A,f15.3)') 'center_latitude: ', centlat(ng)
   write(unit=iunit,fmt='(A,f15.3)') 'center_longitude: ', centlon(ng)
!   write(unit=iunit,fmt='(A,f15.3)') 'starting_latitude: ', &
!        wps_grids(ng)%wps_lat_t(1,1)
!   write(unit=iunit,fmt='(A,f15.3)') 'starting_longitude: ', &
!        wps_grids(ng)%wps_lon_t(1,1)
!   write(unit=iunit,fmt='(A,f15.3)') 'ending_latitude: ', &
!        wps_grids(ng)%wps_lat_t(nnxp(ng),nnyp(ng))
!   write(unit=iunit,fmt='(A,f15.3)') 'ending_longitude: ', &
!        wps_grids(ng)%wps_lon_t(nnxp(ng),nnyp(ng))
#ifdef WPS
   write(unit=iunit,fmt='(A,f15.3)') 'starting_latitude: ', &
        wps_grids(ng)%swlat
   write(unit=iunit,fmt='(A,f15.3)') 'starting_longitude: ', &
        wps_grids(ng)%swlon
   write(unit=iunit,fmt='(A,f15.3)') 'ending_latitude: ', &
        wps_grids(ng)%nelat
   write(unit=iunit,fmt='(A,f15.3)') 'ending_longitude: ', &
        wps_grids(ng)%nelon
#endif
   close(unit=iunit)

   return
end subroutine write_maproj



