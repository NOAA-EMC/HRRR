!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module gfedv2_emissions    
use AeM_emission_factors,  nspecies=>AeM_nspecies

!---------------------------------------------------------------------------
  type gfedv2_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type gfedv2_vars

  type (gfedv2_vars), allocatable :: gfedv2_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_gfedv2(gfedv2,n1,n2,n3,nspecies)

    implicit none

    type (gfedv2_vars),dimension(nspecies)  :: gfedv2
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies
    
    do ispc=1,nspecies
     allocate (gfedv2(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_gfedv2

  !---------------------------------------------------------------

  subroutine nullify_gfedv2(gfedv2,nspecies)

    implicit none

    type (gfedv2_vars),dimension(nspecies)  :: gfedv2
    integer ispc,nspecies

    do ispc=1,nspecies
       if (associated(gfedv2(ispc)%src))    nullify (gfedv2(ispc)%src)
    enddo

    return
  end subroutine nullify_gfedv2

end module gfedv2_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_gfedv2(n1,n2,n3)
    use gfedv2_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(gfedv2_g)) allocate(gfedv2_g(nspecies))
   
    !do i=1,nspecies
    ! if(associated(gfedv2_g(i)%src)) deallocate(gfedv2_g(i)%src)
    !enddo

    call nullify_gfedv2(gfedv2_g(:),nspecies)      
    call alloc_gfedv2  (gfedv2_g(:),n1,n2,n3,nspecies) 
  end subroutine mem_gfedv2

!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine read_gfedv2(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon&
                      ,rland,deltax,deltay,xt,yt,plat,plon)

use grid_dims_out, only: grid_type,gfedv2_data_dir
use gfedv2_emissions
implicit none
integer, parameter ::  nlon = 360, nlat=180, nmonths=12 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, dimension(12) :: mondays
data mondays/31,28,31,30,31,30,31,31,30,31,30,31/
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,igbox,jgbox,j1,j2,&
          ic,jc

character*240 filename
character*180 dummy
real longgfedv2(nlon),latgfedv2(nlat)
real lat,lon,RAWsrc(0:nmonths,nlon,nlat),rrdummy(nlon,nlat)
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),rdummy
real dx,dy,  dxm ,dym, xt(n1), yt(n2),plat,plon, ilatn,ilonn  
integer iveg, veg_type(nlon,nlat),veg_type_model(n1,n2)


where(emission_factor <0.) emission_factor=0. ! avoid negative values for emission factor

!go to 33
! leitura em BIN
!filename='GFEDv2_CO_monthly_mean_1997to2004.gra'
!print *,'opening   ', filename
!open(11,file=filename,status='old',access='direct',&
!     recl=4*nlon*nlat)!, convert='little_endian')        
!do i=0,12
!read (11, REC=i+1) RAWsrc(i,:,:)       
!enddo
!close (11)
!reverte a ordem em latitude
!do k=0,nmonths
!do j=1,(nlat)/2
!   do i=1,nlon
!      rdummy=RAWsrc(k,i,j)
!      RAWsrc(k,i,j)=RAWsrc(k,i,nlat-j+1)
!      RAWsrc(k,i,nlat-j+1)=rdummy
!   enddo
!enddo
!enddo
!33 continue
!
!
!- reading using VFM format
filename=trim(gfedv2_data_dir)//'/'//'GFEDv2_CO_monthly_mean_1997to2004.vfm'
print*,'================================================================='
print *,'GFEDv2 source opening   ', trim(filename)
print*,'================================================================='
 open(11,file=filename,form='formatted',status='OLD')	
 do i=0,12
 !call vforec(11, RAWsrc(i,:,:)	 ,nlon*nlat,24,rrdummy,'LIN')!14 EDGAR -CO   BB: kg/m^2/dia
  call vfirec(11,RAWsrc(i,:,:),nlon*nlat,'LIN')!preserve 'LIN' em maiusculo.
 enddo
 close (11)
!
! zero out neg values
where(RAWsrc < 0.) RAWsrc = 0.
!

! compatibilize a legenda de vegetacao com a adoptada no veg_agreg
!gfedv2: 1=sav, 2=trop for, 3=extra trop fore
! agreg: 1 =trop for, 2 =extra trop for, 3 =sav, 4 = past, 5=deserto, 0 = agua/gelo
! veg dominante por grid de 1x1 grau (data from GFEDv2)
where(nint(RAWsrc(0,:,:)) == 1) veg_type(:,:)=3
where(nint(RAWsrc(0,:,:)) == 2) veg_type(:,:)=1
where(nint(RAWsrc(0,:,:)) == 3) veg_type(:,:)=2
!
!
!lat e lon gfedv2 (corner mais ao sul e mais a oeste)                                  
   ilatn=1.;ilonn=1.  
do i=1,nlon
 longgfedv2(i)=-179.5 + (i-1)*ilonn
enddo
do j=1,nlat
 latgfedv2(j)= -89.5 + (j-1)*ilatn
enddo

!--- performs the interpolation to model grid box

do i=1,n1
  do j=1,n2

   call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longgfedv2,latgfedv2 &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

! RAWsrc contains CO data 
   call interpol_gfedv(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imon,nmonths,nspecies,RAWsrc,veg_type,tx)

! 
! TX is the value interpolated to the model grid box.
!-obs: the raw data is  the total monthly and the units are in g /m^2
      gfedv2_g(:)%src(i,j,1)=TX(:) * 1.e-3/mondays(imon) ! converte para kg/m2/day

    enddo
enddo      
!print*,'gfedv2_g(CO)%src(i,j,1)=',gfedv2_g(CO)%src(:,:,1)
!return
do ispc=1,nspecies 
  call apply_land_restriction(n1,n2,rland,gfedv2_g(ispc)%src(:,:,1))
enddo  

end subroutine read_gfedv2
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_gfedv2_indentity(spc_name,ident)
use chem1_list
use gfedv2_emissions, only : gfedv2_nspecies=>nspecies& !don't use AeM_nspecies
                            ,gfedv2_spc_name=>AeM_spc_name
implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,gfedv2_nspecies
  ident=-1
  if(spc_name == gfedv2_spc_name(isp)) then
      print*,'==>gfedv2 found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of gfedv2'
!print*,'ident=',ident
!stop 444
end subroutine get_gfedv2_indentity
!---------------------------------------------------------------
subroutine interpol_gfedv(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
	                ,imon,nmonths,nspecies,RAWsrc,veg_type&
			,tx)
use grid_dims_out, only: grid_type
use AeM_emission_factors
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,imon,nmonths,nspecies,ispc
real, dimension(n1,n2) :: rlat,rlon
real, dimension(0:nmonths,nlon,nlat) :: RAWsrc
real ilatn,ilonn,tx(nspecies),delta
integer  veg_type(nlon,nlat)

!-local var
real dlonr,dlatr,usdum
integer qi1,qi2,qj1,qj2,ncount,ii,jj
integer iveg , veg_type_model(n1,n2)

!if(ic.ge.0 .and. jc .ge. 0) then 
             
!if(ic.ge.0 .and. jc .ge. 0) then 
if(grid_type == 'fim') then

 ncount = 0
 dlonr = 0.
 dlatr = 0.
 do ii=1,n1-1
     ncount = ncount+1
     dlonr= dlonr + rlon(ii+1,1)-rlon(ii,1)
     dlatr= dlatr + rlat(ii+1,1)-rlat(ii,1)
 enddo
 dlonr = 0.5*dlonr/(float(ncount) + 1.E-10)
 dlatr = 0.5*dlatr/(float(ncount) + 1.E-10)
elseif(grid_type == 'mercator') then
 dlonr=0.5*(rlon(2,j)-rlon(1,j))
 dlatr=0.5*(rlat(i,n2)-rlat(i,1))/float(n2-1)
else    	     
 delta = .01*(int(100.*rlon(n1,j))-int(100.*rlon(1,j)))
 if (delta .gt. rlon(2,j)-rlon(1,j)) then            
   dlonr=0.5*(rlon(n1,j)-rlon(1,j))/float(n1-1)
 else
   dlonr=180./float(n1-1)
 endif
 dlatr=0.5*(rlat(i,n2)-rlat(i,1))/float(n2-1)
endif

qi1=int(dlonr/ilonn+0.5)
qi2=int(dlonr/ilonn+0.5)
qj1=int(dlatr/ilatn+0.5)
qj2=int(dlatr/ilatn+0.5)
 
ncount = 0
TX(:)  = 0.

do jj = min(max(1,jc-qj1),nlat),min(nlat,jc+qj2)
   do ii = min(max(1,ic-qi1),nlon),min(nlon,ic+qi2)   
     	
     if(veg_type(ii,jj) > 0)  then 
      
       ncount = ncount + 1
     
! use emission factors to convert from CO to others species ...
       TX(:) = TX(:) + RAWsrc(imon,ii,jj) *& ! RAWsrc contains CO data 
               emission_factor(veg_type(ii,jj),: )  /  &
               emission_factor(veg_type(ii,jj),CO) 
      
      endif 
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol_gfedv
