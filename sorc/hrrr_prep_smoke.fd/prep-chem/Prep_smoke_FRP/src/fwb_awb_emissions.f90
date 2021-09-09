!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module FWBAWB_emissions    
use AeM_emission_factors, only: nspecies=>AeM_nspecies, N2_nitrogenio=>N2, AeM_spc_name, bburn2, bburn3

!---------------------------------------------------------------------------
  type FWBAWB_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type FWBAWB_vars

  type (FWBAWB_vars), allocatable :: FWBAWB_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_FWBAWB(FWBAWB,n1,n2,n3,nspecies)

    implicit none

    type (FWBAWB_vars),dimension(nspecies)  :: FWBAWB
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies
    
    do ispc=1,nspecies
     allocate (FWBAWB(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_FWBAWB

  !---------------------------------------------------------------

  subroutine nullify_FWBAWB(FWBAWB,nspecies)

    implicit none

    type (FWBAWB_vars),dimension(nspecies)  :: FWBAWB
    integer ispc,nspecies

    do ispc=1,nspecies
       if (associated(FWBAWB(ispc)%src))    nullify (FWBAWB(ispc)%src)
    enddo

    return
  end subroutine nullify_FWBAWB

end module FWBAWB_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_FWBAWB(n1,n2,n3)
    use FWBAWB_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(FWBAWB_g)) allocate(FWBAWB_g(nspecies))
   
    !do i=1,nspecies
    ! if(associated(FWBAWB_g(i)%src)) deallocate(FWBAWB_g(i)%src)
    !enddo

    call nullify_FWBAWB(FWBAWB_g(:),nspecies)      
    call alloc_FWBAWB  (FWBAWB_g(:),n1,n2,n3,nspecies) 
  end subroutine mem_FWBAWB

!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine read_FWBAWB(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon&
                      ,rland,deltax,deltay,xt,yt,plat,plon)
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
use grid_dims_out, only: grid_type,FWBAWB_data_dir
use FWBAWB_emissions
implicit none
integer, parameter ::  nlon = 360, nlat=180, nmonths=1 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, dimension(12) :: mondays
real,allocatable ,save, dimension(:,:) :: data_BIF,data_CMB,data_WDF
data mondays/31,28,31,30,31,30,31,31,30,31,30,31/
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,igbox,jgbox,j1,j2,&
          ic,jc

character*240 filename
character*180 dummy
real longFWBAWB(nlon),latFWBAWB(nlat)
real, parameter ::  ilatn=1. , ilonn=1. 
real lat,lon,RAWsrc(3,nlon,nlat)
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),rdummy
real dx,dy,  dxm ,dym, xt(n1), yt(n2),plat,plon, dlat, dlon,area,fx
real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6367000.

integer :: bif = 1 &! burning of residues in the fields.
          ,cmb = 2 &! residue and dung used as biofuels.
	  ,wdf = 3  ! woodfuels burnings.(fuelwood and 
	  	    !		      charcoal burning).
!intrinsic ::  cos

if( .not. allocated (data_BIF)) allocate(data_BIF(nlon,nlat),data_CMB(nlon,nlat),data_WDF(nlon,nlat))

!lat e lon FWBAWB (corner mais ao sul e mais a oeste)					   
do i=1,nlon;  longFWBAWB(i) =-179.5 + (i-1)*ilonn;  enddo
do j=1,nlat;   latFWBAWB(j) = -89.5 + (j-1)*ilatn;  enddo

	  
if(ng == 1) then  ! need to read just one time


  print*,'============================================================='
  filename=trim(FWBAWB_data_dir)//'/'//'BIF.1x1'
  print *,'BIF source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_bif(i,j),i=1,360),j=1,180)
  close(11)
1 format(8e10.3)

  filename=trim(FWBAWB_data_dir)//'/'//'CMB.1x1'
  print *,'CMD source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_cmb(i,j),i=1,360),j=1,180)
  close(11)

  filename=trim(FWBAWB_data_dir)//'/'//'WDF.1x1'
  print *,'WDF source: opening   ', filename
  open(11,file=filename,status='old')
  read(11,1)((data_wdf(i,j),i=1,360),j=1,180)
  close(11)
  print*,'============================================================='


! Os valores estao em Teragramas de materia seca por ano: converta-os para 
! kg[dry matter] por dia por metro quadrado

 dlat=ilatn*pi180
 dlon=ilonn*pi180
 do j=1,180
   ! Not all compilers are aware of the cosd function
   ! using cos with pi180
   ! cosd = cos (arg in degrees)
   ! area = cosd(0.5*(latFWBAWB(j)+latFWBAWB(j))) * (r_earth**2) *dlat * dlon
   area = cos(0.5*(latFWBAWB(j)+latFWBAWB(j))*pi180) * (r_earth**2) *dlat * dlon
   fx = 1.e+9    * & ! convert to kg
       (1./365.) * & ! convert to day
       (1./area)     ! converte to m^2 ==> kg[dry matter]/ (m^2 day )  
   do i=1,360
    data_BIF(i,j) = data_BIF(i,j) * fx
    data_CMB(i,j) = data_CMB(i,j) * fx
    data_WDF(i,j) = data_WDF(i,j) * fx
   enddo
 enddo

endif ! ng==1


!--- accumulate in only one array
  RAWsrc(bif,:,:) = data_BIF(:,:)
  RAWsrc(cmb,:,:) = data_CMB(:,:)
  RAWsrc(wdf,:,:) = data_WDF(:,:)

!--- performs the interpolation to the model grid box

  do i=1,n1
    do j=1,n2

     call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longFWBAWB,latFWBAWB &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

     call interpol_FWBAWB(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imon,nmonths,nspecies,RAWsrc,bif,cmb,wdf,tx)

! 
   ! TX is the value interpolated to the model grid box.
    do ispc = 1, nspecies 
     FWBAWB_g(ispc)%src(i,j,1)=TX(ispc) ! unit: kg/m2/day
    enddo

   enddo
  enddo      
!-print*,'FWBAWB_g(CO)%src(i,j,1)=',FWBAWB_g(CO)%src(:,:,1)
!-return
  do ispc=1,nspecies 
    call apply_land_restriction(n1,n2,rland,FWBAWB_g(ispc)%src(:,:,1))
  enddo  
!- deallocate memory that we do not need anymore    
if(ng==ngrids) deallocate (data_BIF,data_CMB,data_WDF)

end subroutine read_FWBAWB
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_FWBAWB_indentity(spc_name,ident)
!use aer1_list
use FWBAWB_emissions, only : FWBAWB_nspecies=>nspecies& !don't use AeM_nspecies
                            ,FWBAWB_spc_name=>AeM_spc_name, bburn2, bburn3
implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

!- special section for aerosols from charcoal production, waste burning, etc:
!- them are included in the urban inventories
if(spc_name == 'URBAN2' ) then 
  ident=bburn2
  return
ENDIF
if(spc_name == 'URBAN3') then 
  ident=bburn3
  return
ENDIF
IF(spc_name == 'BBURN2' .or. spc_name == 'BBURN3') then
  ident = -1
  return
ENDIF
!------

do isp = 1,FWBAWB_nspecies
  ident=-1
  if(spc_name == FWBAWB_spc_name(isp)) then
      ident=isp
      print*,'==>FWBAWB found for ',spc_name !,ident
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of FWBAWB'
!print*,'ident=',ident
!stop 444
end subroutine get_FWBAWB_indentity
!---------------------------------------------------------------
subroutine interpol_FWBAWB(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
	                ,imon,nmonths,nspecies,RAWsrc,bif,cmd,wdf&
			,tx)
use AeM_emission_factors, N2_nitrogenio=>N2
use grid_dims_out, only: grid_type
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,imon,nmonths,nspecies,ispc
real, dimension(n1,n2) :: rlat,rlon
real, dimension(3,nlon,nlat) :: RAWsrc
real ilatn,ilonn,tx(nspecies),delta
integer bif,cmd,wdf

!-local var
real dlonr,dlatr,usdum
integer qi1,qi2,qj1,qj2,ncount,ii,jj

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

where(emission_factor <0.) emission_factor=0. ! avoid negative values for emission factor
 
ncount = 0
TX(:)  = 0.

do jj = min(max(1,jc-qj1),nlat),min(nlat,jc+qj2)
   do ii = min(max(1,ic-qi1),nlon),min(nlon,ic+qi2)   
     	      
       ncount = ncount + 1
     
! use emission factors to convert from dry matter to  species ...
       TX(:) = TX(:) + RAWsrc(bif,ii,jj) *      emission_factor(AgResid  ,:) *1.e-3 + & 
                       RAWsrc(cmd,ii,jj) *      emission_factor(AgResid  ,:) *1.e-3 + & 
                       RAWsrc(wdf,ii,jj) * 0.5*(emission_factor(Biofuel  ,:)        + & 
                                                emission_factor(CharcBurn,:))*1.e-3 
      
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol_FWBAWB
!---------------------------------------------------------------
