!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
module grid_dims_out

use grid_dims, only : nxpmax,nypmax,nzpmax,maxgrds
character (len=180) :: grid_type,proj_to_ll    &
                      ,retro_data_dir          &
                      ,wb_data_dir             &
		      ,edgar_data_dir          &
		      ,fim_data_dir            &
		      ,fv3_data_dir            &
		      ,gocart_data_dir         &
		      ,ceds_data_dir           &
		      ,gocart_bg_data_dir      &
		      ,afwa_erod_data_dir      &
                      ,streets_data_dir        &
                      ,seac4rs_data_dir        &
		      ,bioge_data_dir          &
		      ,fwbawb_data_dir         &
                      ,gfedv2_data_dir         &
		      ,bbem_wfabba_data_dir    &
		      ,bbem_modis_data_dir     &
		      ,bbem_inpe_data_dir      &
		      ,bbem_extra_data_dir     &
		      ,bbem_fre_data_dir       &		
		      ,veg_type_data_dir       &
   		      ,vcf_type_data_dir       &
   		      ,olson_data_dir          &
! --	      	     	   		      	     
		      ,use_these_values	       &
		      ,carbon_density_data_dir &
                      ,degass_volc_data_dir    &       	    
		      ,fuel_data_dir           &
		      ,houghton_data_dir       &
		      ,chem_out_prefix         &
		      ,special_output_to_wrf   &
		      ,chem_out_format         &   
   		      ,user_data_dir       
character (len=12) begin_eruption	


integer::              use_retro          &
		      ,use_edgar          &
		      ,use_gocart         &
		      ,use_ceds           &
		      ,use_gocart_bg      &
		      ,use_afwa_erod      &
                      ,use_streets        &
                      ,use_seac4rs        &
                      ,use_gfedv2         &
		      ,use_bioge          &
		      ,use_bbem           &
		      ,use_bbem_plumerise &
              ,pond               &
              ,use_diurnal_cycle  &		      
              ,use_fwbawb         &      
	      ,merge_GFEDv2_bbem  &
	      ,use_wb             &
              ,use_volcanoes	  &       	    
              ,volcano_index	  &      	    
              ,use_degass_volcanoes  & 
              ,use_vcf            		   

integer, parameter :: maxfiles=2000
integer, parameter :: maxgx=nxpmax, maxgy=nypmax
integer :: nxgrads(maxgrds),nygrads(maxgrds)   &
    	  ,    nxa(maxgrds),    nxb(maxgrds)   &
	  ,    nya(maxgrds),    nyb(maxgrds)   &
     	  ,iinf(maxgx,maxgy), jinf(maxgx,maxgy)  
real rmi(maxgx,maxgy,4)
real, dimension(maxgrds) :: lati,latf,loni,lonf
real, dimension(2,maxgrds) :: dep_glat, dep_glon
real :: rlatmin,rlatmax,rlonmin,rlonmax
real :: glatg(nypmax),glong(nxpmax)
!------ for others grids
integer nlat
real lat_beg,lon_beg,delta_lat,delta_lon, &
     grid_resolucao_lon,grid_resolucao_lat


End module grid_dims_out
