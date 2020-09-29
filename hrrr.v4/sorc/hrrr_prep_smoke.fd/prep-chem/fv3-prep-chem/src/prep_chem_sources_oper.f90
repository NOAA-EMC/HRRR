!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
program prep_chem_sources

use grid_dims_out
implicit none

character (len=180) :: rams_anal_prefix

integer :: ihour,iday,imon,iyear

namelist/rp_input/  grid_type,rams_anal_prefix   	       &
		   ,ihour,iday,imon,iyear       	       &
                   ,use_retro                   	       &
		   ,retro_data_dir              	       &	    
		   ,use_edgar                   	       &
		   ,edgar_data_dir              	       &
                   ,use_fwbawb                  	       & 
		   ,fwbawb_data_dir             	       &
                   ,use_gfedv2                  	       & 
		   ,gfedv2_data_dir             	       &
		   ,use_bioge                                  &
		   ,bioge_data_dir                             &
 		   ,use_bbem                    	       &
		   ,use_bbem_plumerise          	       &
		   ,merge_GFEDv2_bbem                          &
		   ,bbem_wfabba_data_dir        	       &
		   ,bbem_modis_data_dir              	       &      
		   ,bbem_inpe_data_dir          	       &
		   ,bbem_extra_data_dir         	       &
		   ,veg_type_data_dir                          &		   
		   ,carbon_density_data_dir     	       &
		   ,houghton_data_dir	        	       &	   
                   ,grid_resolucao_lon          	       &
                   ,grid_resolucao_lat          	       &
	           ,nlat,lat_beg,lon_beg,delta_lat,delta_lon   &
		   ,proj_to_ll,lati,latf,loni,lonf             &
                   ,chem_out_prefix,chem_out_format

print *, ' '
print *, 'Opening prep_chem_sources.inp file'

open(5,file='prep_chem_sources.inp',status='old')
read(5,rp_input)
close(5)
!
!- check  consistency of namelist
call check_consistency


!- determine the way to follow
if(trim(grid_type)=='rams' .or. trim(grid_type) =='RAMS') then
  grid_type='rams'
  call rams_grid_type(rams_anal_prefix,ihour,iday,imon,iyear)

else

  call other_grid_type(ihour,iday,imon,iyear)

endif
end
!-----------------------------------------------------------------------------

subroutine other_grid_type(ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch
use grid_dims_out 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear
!character (len=*), intent(in)  ::  chem_out_prefix

integer :: i,j,iv,ng,nfn,nfiles,nvert
character (len=240) :: fnames(maxfiles)
real, allocatable, dimension(:,:,:) :: r2d


!- determine the grid configuration
ngrids = 1
proj_to_ll='NO'
nnzp(1:ngrids)=1

! -----------------
! -  start GRID LOOP (ONLY ONE is permited)   -
! -----------------

do ng=1,ngrids
  nfn=1

  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  !-lon/lat configuration
                        nnxp(ng)=int(delta_lon/grid_resolucao_lon + 0.1)
  if(grid_type == 'll')      then
                        nnyp(ng)=int(delta_lat/grid_resolucao_lat + 0.1) + 1
  elseif(grid_type == 'gg')  then
                        nnyp(ng)=nlat
  else

    print*,'no grid defined: grid_type=',grid_type(1:len_trim(grid_type))
    stop
  endif
  !- allocate arrays for lat,lon,land
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  !- lon
  do i=1,nnxp(ng)
     r2d(i,1:nnyp(ng),2)=lon_beg + grid_resolucao_lon*(i-1) 
  enddo
  !- lat
  if(grid_type == 'll')      then
       do j=1,nnyp(ng)
             r2d(1:nnxp(ng),j,1)=lat_beg + grid_resolucao_lat*(j-1) 
       enddo
  elseif(grid_type == 'gg')  then
        call gauss_lat(nlat,r2d(1:1,1:nnyp(ng),1))
        !>>>> r2d(1:nnxp(ng),1:nnyp(ng),2)=r2d(1,1:nnyp(ng),2)!<<<<<<<<<<<
  endif
  ! for now land=1 everywhere
  r2d(:,:,3)=1.      !<<<<<<<<<<<
  !
  !
  !
  !.................
  ! determine the output dimension and arrays:
   nxgrads(ng)    = nnxp(ng)
   nygrads(ng)    = nnyp(ng)	     
   dep_glon(1,ng) = lon_beg
   dep_glon(2,ng) = grid_resolucao_lon
   dep_glat(1,ng) = lat_beg
   dep_glat(2,ng) = grid_resolucao_lat
   nxa(ng) = 1
   nxb(ng) = nnxp(ng)
   nya(ng) = 1
   nyb(ng) = nnyp(ng)
  
  !.................
  !go to 200
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
   nvert = 1        !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nnzp(ng) &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
  !
  200 continue
  !
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for visualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng))


  deallocate(r2d)
enddo ! enddo ngrids




end subroutine other_grid_type
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
subroutine rams_grid_type(rams_anal_prefix,ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch
use grid_dims_out 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear
character (len=*), intent(in)  :: rams_anal_prefix

integer :: i,iv,ng,nfn,nfiles,ivar_type,nvert
character (len=240) :: fnames(maxfiles),car_var(3),dim_var(3)& 
                      ,name_var(3),units_var(3)

data (car_var(iv), iv=1,3)/'lat', 'lon', 'land'/
data (dim_var(iv), iv=1,3)/'2d', '2d', '3d'/

real, allocatable, dimension(:,:,:) :: r2d


call chem_RAMS_filelist(fnames,trim(rams_anal_prefix)//'*-head.txt',maxfiles,nfiles,1)

call chem_RAMS_read_header(fnames(1)(1:len_trim(fnames(1))))

! --- All model informations have been collected


! -----------------
! -  start GRID LOOP   -
! -----------------

do ng=1,ngrids
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  nfn=1
  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  do iv=1,3
        !call determine_dim_var(car_var(iv),ivar_type)
        !print*,car_var(iv),ivar_type
	!stop 3
        call ep_getvar(iv,car_var(iv),dim_var(iv),name_var(iv),units_var(iv)&
	              ,r2d(:,:,iv), nnxp(ng),nnyp(ng),nnzp(ng),nzg,npatch&
		      ,ng,fnames(nfn)(1:len_trim(fnames(nfn))),ivar_type)
  enddo

  !.................
  ! determine the output dimension and arrays:
  !
  call geo_grid(nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2),  &
               dep_glon(1,ng),dep_glon(2,ng),	  &
               dep_glat(1,ng),dep_glat(2,ng),	  &
               rlatmin,rlatmax,rlonmin,rlonmax,   &
               nxgrads(ng),nygrads(ng), 	  &
               proj_to_ll(1:len_trim(proj_to_ll)))
  !.................            
  call Matriz_interp(ng,nxgrads(ng),nygrads(ng),   &
        	nnxp(ng),nnyp(ng),  	           &
        	dep_glat(1,ng),dep_glat(2,ng),     &
        	dep_glon(1,ng),dep_glon(2,ng),     &
        	iinf,jinf,rmi,  		   &
  		proj_to_ll(1:len_trim(proj_to_ll)),'VMP') ! mean_type='VMP' ! vizinho mais proximo
  	       !proj_to_ll(1:len_trim(proj_to_ll)),mean_type(1:len_trim(mean_type)))

  !_................
  call define_lim(ng,nxgrads(ng),nygrads(ng),	       &
    	         dep_glat(1,ng),dep_glat(2,ng),        &
    	         dep_glon(1,ng),dep_glon(2,ng),        &
    	         lati(ng),latf(ng),loni(ng),lonf(ng),  &
    	         nxa(ng),nxb(ng),nya(ng),nyb(ng),      &
    	         proj_to_ll(1:len_trim(proj_to_ll)),   &
    	         nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2))
  !.................
  !go to 200
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
  nvert = 1         !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nvert &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
  200 continue
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for vizualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng))


  deallocate(r2d)
enddo ! enddo ngrids
end subroutine rams_grid_type
!-----------------------------------------------------------------------------

subroutine prep_sources(ng,n1,n2,n3,rlat,rlon,rland,ihour,iday,imon,iyear)

use mem_grid, only : xtn,ytn, xmn,ymn,deltaxn,deltayn,platn,plonn,polelat,polelon,ngrids
use grid_dims_out
implicit none
integer, intent(in) :: ng,n1,n2,n3,ihour,iday,imon,iyear
real, dimension(n1,n2) :: rlat,rlon,rland
!
!------------------------------------------------------------
!  RETRO anthropogenic emission 
if(use_retro == 1) then   
   call mem_retro(n1,n2,n3)
   call read_retro_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))

endif
!------------------------------------------------------------
!  Biogenic  emission 
if(use_bioge == 1) then
   call mem_bioge(n1,n2,n3)
   call read_bioge(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  EDGAR anthropogenic emission 
if(use_edgar == 1) then
   call mem_edgar(n1,n2,n3)
   call read_edgar_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  GFEDv2 Biomass Burning emission 
if(use_GFEDv2 == 1)  then 
   call mem_gfedv2(n1,n2,n3)
   call read_gfedv2(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng),deltayn(ng)&
                ,xtn(1,ng),ytn(1,ng),platn(ng),plonn(ng))
endif		
!------------------------------------------------------------
!  FWB and AWB  emission 
!   woodfuels burning (including both fuelwood and charcoal burning),
!   residue and dung used as biofuels, 
!   burning of residues in the fields.  
if(use_fwbawb == 1)  then 
   call mem_fwbawb(n1,n2,n3)
   call read_fwbawb(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng),deltayn(ng)&
                ,xtn(1,ng),ytn(1,ng),platn(ng),plonn(ng))
endif		
!------------------------------------------------------------
!  BBBEM Biomass Burning emission 
if(use_bbem == 1)   then
   call mem_bbbem(n1,n2,n3)
   call process_bbbem(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
  		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif

!------------------------------------------------------------
! use GFEDv2 information for areas where 3BEM has not information
if(merge_GFEDv2_bbem == 1)   then
   call merge_bburn(ng,ngrids,n1,n2,n3,rlat,rlon,rland)
endif

end subroutine prep_sources
!-----------------------------------------------------------------------------
subroutine select_data(ng,n1,n2,n3)
use chem1_list
use grid_dims_out
use emiss_vars_emissions!, only : emiss_nspecies,emiss_spc_name&
                        !        ,emiss_g
use edgar_emissions, only :  edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name,edgar_g

use retro_emissions, only :  retro_nspecies=>nspecies&
                            ,retro_spc_name=>spc_name,retro_g

use bioge_emissions, only :  bioge_nspecies=>nspecies&
                            ,bioge_spc_name=>spc_name,bioge_g

use gfedv2_emissions, only : gfedv2_nspecies=>nspecies& !don't use AeM_nspecies
                            ,gfedv2_spc_name=>AeM_spc_name,gfedv2_g

use bbbem_emissions, only : bbbem_nspecies=>nspecies& !don't use AeM_nspecies
                           ,bbbem_spc_name=>AeM_spc_name,bbbem_g

use bbbem_plumerise, only : bbbem_plumerise_nspecies=>nspecies    & !don't use AeM_nspecies
                           ,bbbem_plumerise_spc_name=>AeM_spc_name&
			   ,bbbem_plume_g           &
			   ,bbbem_plume_fire_prop_g &
			   ,spc_suf,nveg_agreg,qarea_agreg&
			   ,bbbem_plume_mean_g

use fwbawb_emissions, only : fwbawb_nspecies=>nspecies& !don't use AeM_nspecies
                            ,fwbawb_spc_name=>AeM_spc_name,fwbawb_g
			   
implicit none
integer, intent(in):: ng,n1,n2,n3
integer isp,ident,iespc,iv,i,j, use_mean_fraction_plumerise, ident_fwbawb

!- determine the number of species which will have source emissions
!- defined and will be write out for the atmospheric transport model
 emiss_nspecies = 0 
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) emiss_nspecies=emiss_nspecies+1    
 enddo

!- allocate the memory needed for that 
 call  mem_emiss_vars(n1,n2,n3)
!if(ng==2) stop 33
!
!
!--------------------- copy from scratch arrays to output array
!
!------ Part 1 : Anthropogenic sources ----------------------------------
!- in this section, the antropogenic sources will be defined for all species
!- in which exist data available (RETRO or EDGAR)
 iespc=0

!- loop at chem1-list of species and check only the one will have sources

if(use_retro == 1  .or.  use_edgar == 1  .or. use_fwbawb == 1) then   
   
   do isp=1,nspecies

    if(spc_alloc(1,isp) == 1) then
  	
       ident = -1      
       if(use_retro == 1 ) then     
  	  print*,'- searching RETRO antro emission for specie= ',trim(spc_name(isp))
   
  	  !- try getting data from RETRO
  	  !- first get the address of specie (isp) at the RETRO 'space' (ident) 
  	  !
	  call get_retro_indentity(spc_name(isp),ident)
  	     
	     if(ident > 0) then
  	       ! ident > 0 => there exist source emission from retro database
  	          iespc=iespc+1
  	          emiss_spc_name(iespc,antro) 	 = retro_spc_name(ident)
  	          emiss_g(iespc)%src_antro(:,:,1)= retro_g(ident)%src(:,:,1)
  	          found_emiss_spc(iespc,antro)   = 1
  	     else
  	      call convert_retro_to_racm(isp,iespc,ident)
  	
  	     endif ! ident > 0
  	
       endif ! use-retro
  	
       ! if not have, try EDGAR
       if(ident == -1 .and. use_edgar== 1) then
  	 
	      print*,'- searching EDGAR antro emission for specie= ',trim(spc_name(isp))
  	      
	      call get_edgar_indentity(spc_name(isp),ident)
  	      if(ident > 0) then
  		   iespc=iespc+1
  		   emiss_spc_name(iespc,antro)	  = edgar_spc_name(ident)
  		   emiss_g(iespc)%src_antro(:,:,1)= edgar_g(ident)%src(:,:,1)
  		   found_emiss_spc(iespc,antro)   = 1
  	      endif
       
       endif

       if(use_fwbawb == 1 ) then     
  	     print*,'- searching FWB and AWB antro emission for specie= ',trim(spc_name(isp))
  	     call get_fwbawb_indentity(spc_name(isp),ident_fwbawb)
  	     if(ident_fwbawb > 0) then
  	       if(ident == -1) then 
	           iespc=iespc+1
  	           emiss_spc_name(iespc,antro)    = fwbawb_spc_name(ident_fwbawb)
    	           found_emiss_spc(iespc,antro)   = 1
	       endif	
	       emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) + &
	                                         fwbawb_g(ident_fwbawb)%src(:,:,1)
             else
	      !call convert_AeM_to_racm(isp,iespc,ident_fwbawb,'antro')  <<< falta terminar/checar correcao
	     endif
       endif

       if(ident == -1) then
  	  print*,'No antro emissions for "',trim(spc_name(isp)), '" was found'
       endif

     endif !- if spc_alloc

   enddo !- do nspecies

endif
!---------------------------------------------------------------end 
!
!- Part 2 : Biom. Burning sources -----------------------------start 
!- in this section, the bio burn sources will be defined for all species
!- in which exist data available
if(use_gfedv2 == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) then
    print*,'- searching bio burn emission for specie= ',trim(spc_name(isp))

    !- try gettting data from GFEDv2
     !- first get the address of specie (isp) at the GFEDv2 'space' (ident) 
     call get_GFEDv2_indentity(spc_name(isp),ident)
     if(ident > 0) then
       ! ident > 0 => there exist source emission from retro database
       iespc=iespc+1
       emiss_spc_name(iespc,bburn)         = gfedv2_spc_name(ident)
       emiss_g(iespc)%src_bburn(:,:,1)=gfedv2_g(ident)%src(:,:,1)
       found_emiss_spc(iespc,bburn) = 1
       !print*,emiss_spc_name(iespc,bburn),iespc,gfedv2_spc_name(ident),ident
     else
      call convert_AeM_to_racm(isp,iespc,ident, 'bburn')
     
     endif ! ident > 0
       
  endif ! if species
 enddo !loop species
endif 
!---------------------------------------------------------------end 

!- Part 3 : Biom. Burning sources -----------------------------start 
!- in this section, the bio burn sources will be defined for all species
!- in which exist data available
if(use_bbem == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) then
    print*,'- searching bio burn emission for specie= ',trim(spc_name(isp))

    !- try gettting data from 3BEM
     !- first get the address of specie (isp) at the 3BEM 'space' (ident) 
     call get_bbbem_indentity(spc_name(isp),ident)
     if(ident > 0) then
       ! ident > 0 => there exist source emission from retro database
       iespc=iespc+1
       emiss_spc_name(iespc,bburn)          = bbbem_spc_name(ident)
       emiss_g(iespc)%src_bburn(:,:,1)= bbbem_g(ident)%src(:,:,1)
       found_emiss_spc(iespc,bburn)   = 1
       !print*,emiss_spc_name(iespc,bburn),iespc,3BEM_spc_name(ident),ident
     else
      call convert_AeM_to_racm(isp,iespc,ident, 'bburn')
     
     endif ! ident > 0
  endif
 enddo
endif
!---------------------------------------------------------------end 

!- Part 4 : Plumerise data for biom. burn. sources -----------start 
if(use_bbem_plumerise == 1) then
  use_mean_fraction_plumerise = 1
  iespc=0
  if( use_mean_fraction_plumerise == 0 ) then
     !- loop at chem1-list of species and check only the one will have sources
     do isp=1,nspecies
      if(spc_alloc(1,isp) == 1) then
    	print*,'- searching PLUME bio burn emission for specie= ',trim(spc_name(isp))
    	call get_bbbem_indentity(spc_name(isp),ident)
    	if(ident > 0) then
    	    iespc=iespc+1
    	
    	    do iv=1,nveg_agreg
    	       
    	       emiss_plume_name(iespc,iv)   = bbbem_spc_name(ident)!//'_'//spc_suf(iv)
    	       !print*,'emiss_plume_name=',iespc,iv, emiss_plume_name(iespc,iv)
    	       
    	       emiss_plume_g   (iespc,iv)%src_plume(:,:)= bbbem_plume_g(ident,iv)%src(:,:,1)	   
    	       
    	       found_emiss_plume(iespc,iv) = 1
    	       print*,'found plume=',emiss_plume_name(iespc,iv),iespc
    	    enddo
    	endif
       endif
     enddo
  else
     iespc=iespc+1
    
     do iv=1,nveg_agreg
     	
     	emiss_plume_name(iespc,iv)   = 'mean_fct'
     	!print*,'emiss_plume_name=',iespc,iv, emiss_plume_name(iespc,iv)
     	
     	emiss_plume_g	(iespc,iv)%src_plume(:,:)= bbbem_plume_mean_g(iv)%src(:,:,1)	    
     	
     	found_emiss_plume(iespc,iv) = 1
     	print*,'found plume=',emiss_plume_name(iespc,iv),iespc
     enddo
  endif
  ! special section for the aggregated fire size
  iespc=iespc+1
  do iv=1,nveg_agreg
      emiss_plume_name(iespc,iv)= 'firesize'!//'_'//trim(spc_suf(iv))
   
      emiss_plume_g   (iespc,iv)%src_plume(:,:)=  bbbem_plume_fire_prop_g(qarea_agreg,iv)%fire_prop(:,:)       
   
      found_emiss_plume(iespc,iv) = 1
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc
  enddo
endif

!---------------------------------------------------------------end 

!- Part 5 : Biogenic data  sources                 -----------start 
if(use_bioge == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
  do isp=1,nspecies
     if(spc_alloc(1,isp) == 1) then
         print*,'- searching biogenic emission for specie= ',trim(spc_name(isp))

         !- try gettting data from bioge
         !- first get the address of specie (isp) at the bioge 'space' (ident) 
         call get_bioge_indentity(spc_name(isp),ident)
         
	 if(ident > 0) then
             ! ident > 0 => there exist source emission from biogenic database
             iespc=iespc+1
             emiss_spc_name(iespc,bioge)	 = bioge_spc_name(ident)
             emiss_g(iespc)%src_bioge(:,:,1)=bioge_g(ident)%src(:,:,1)
             found_emiss_spc(iespc,bioge) = 1
             print*,emiss_spc_name(iespc,bioge),iespc,bioge_spc_name(ident),ident
         
	 else
         
	     call convert_bioge_to_racm(isp,iespc,ident)
         
         endif ! ident > 0
  	 
     endif ! if species
  enddo !loop species
endif 

end subroutine select_data
!-----------------------------------------------------------------------------

subroutine output(ng,nvert,ihour,iday,imon,iyear,ngrids, nxp, nyp, nzp)
use chem1_list
use grid_dims_out
use emiss_vars_emissions, only : emiss_nspecies,number_sources,emiss_spc_name&
                                ,emiss_g,found_emiss_spc&
				,emiss_plume_g, found_emiss_plume
use bbbem_plumerise, only: nveg_agreg
				
implicit none
integer, intent(in) :: ng,ihour,iday,imon,iyear,nvert,ngrids, nxp, nyp, nzp

!- local arrays, ...
character*240 wfln(3)
integer isp,nrec,iftimes,nsrc,iunit_bin,iv,iunit_out
!-pc-cluster
integer, parameter :: bytes=4
!-tupay
!integer, parameter :: bytes=1
character*2 ccgrid
real, allocatable, dimension(:,:,:) :: routgrads
real, pointer, dimension(:,:,:) :: src_dummy
real, pointer, dimension(:,:) :: src_dummy_2d
real, dimension(nxp,nyp) :: dummy

!unit of bin data file (visualization) 
iunit_bin=19
!unit of output data file (for the atmos model) 
iunit_out=20

!- allocate space for output array
allocate (routgrads(nxgrads(ng),nygrads(ng),nvert))

!- build the files names for grads/output format
iftimes=10000*ihour
write(ccgrid,'(a1,i1)') 'g',ng

!-- gra file
call makefnam(wfln(1),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid,'gra')
!- ctl file
call makefnam(wfln(2),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid,'ctl')
!- vfm or hdf file
call makefnam(wfln(3),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid, &
	     chem_out_format(1:len_trim(chem_out_format)))


!- open and write the grads control file
   call write_ctl(ng,iunit_bin,wfln(:),ihour,iday,imon,iyear,nvert)

!- open the binary data file for GRADS vizualization file
   open(iunit_bin,file=wfln(1),form='unformatted',access='direct',status='unknown',  &
        recl=bytes*(nxb(ng)-nxa(ng)+1)*(nyb(ng)-nya(ng)+1))	  
   nrec=0
  
!- open the output file at native grid to be read by the atmos model
   open(iunit_out,file=wfln(3),form='formatted',status='replace')
   isp  = 0
   nsrc = 0
   iv   = 0
   call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
   
   
   
   
   do isp=1,emiss_nspecies 
    do nsrc=1,number_sources
       
       if(found_emiss_spc(isp,nsrc) == 0) cycle ! if not found, cycle
       
      
       
       if(nsrc==1) src_dummy => emiss_g(isp)%src_antro(:,:,:)
       if(nsrc==2) src_dummy => emiss_g(isp)%src_bburn(:,:,:)
       if(nsrc==3) src_dummy => emiss_g(isp)%src_bioge(:,:,:)
       
       !- this is the output at native grid to be read by the atmos model
       !--- V-format 
!srf-para operacao, somente CO e PM25 devem ser escritos no VFM
       if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm'  .and. &
          isp < 3) then
!       if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
!-srf comentado para operacao velha
!        call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
        
	call vforec(iunit_out,src_dummy(:,:,1),nxp*nyp,24,dummy,'LIN')
       
       endif
       !--- HDF 5 format

!>>>>>      ! inserir chamada para formato HDF5 : escrever  o array: src_dummy(:,:,1)

       
       !- this out put is for GRADS vizualization file
       !--- project the grid if needed
       call proj_rams_to_grads(nxp        ,nyp        , nvert     &
 	                      ,nxgrads(ng),nygrads(ng), nvert        &
 	                      ,rmi,iinf,jinf		             &
 	                      ,src_dummy                             &
			      ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
       call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
                      nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
		      print*,'emissions rec=',nrec, emiss_spc_name(isp,nsrc)
    enddo
   enddo

!- plumerise section
   nsrc = 2 ! bburn only
   !---- plumerise section
   
   if(use_bbem_plumerise == 1) then
    do isp=1,emiss_nspecies+1 
       do iv=1,nveg_agreg
       print*,'plumerise:',isp,iv
            
!	  if(isp == 3 .or. isp==4) cycle
	  
	  
	  !print*,' isp - iv=',isp,iv
	  !print*,'found=',found_emiss_plume(isp,iv)
          
          if(found_emiss_plume(isp,iv) == 0) cycle
          src_dummy_2d => emiss_plume_g(isp,iv)%src_plume(:,:)
          !print*,'emiss_nspecies=',emiss_nspecies
          !print*,'isp=',isp, 'emiss_spc_name=',emiss_spc_name(isp,bburn);call flush(6)
          !- this is the output at native grid to be read by the atmos model
          !--- V-format 
          if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
!-srf comentado para operacao velha
!           call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
           call vforec(iunit_out,src_dummy_2d(:,:),nxp*nyp,24,dummy,'LIN')
         
          endif
          !--- HDF 5 format

!>>>>>          ! inserir chamada para formato HDF5 : escrever  o array: src_dummy(:,:,1)



          !- this out put is for GRADS vizualization file
          !--- project the grid if needed
          call proj_rams_to_grads(nxp        ,nyp   , nvert     &
 	                         ,nxgrads(ng),nygrads(ng), nvert        &
 	                         ,rmi,iinf,jinf		                &
 	                         ,src_dummy_2d                          &
			         ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
       
          call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
                      nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
		      print*,'plume rec=',nrec
       enddo
    enddo
   endif
   
close(iunit_bin)
close(iunit_out)

deallocate(routgrads)
end subroutine output
!-----------------------------------------------------------------------------

