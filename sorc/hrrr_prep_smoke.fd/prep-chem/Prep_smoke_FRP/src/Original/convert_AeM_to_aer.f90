!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_AeM_to_aer(isp,iespc,ident,source_type,spc_name,nx,ny)
use aer1_list    , only : aerosol_mechanism
use grid_dims_out, only : use_bbem, use_gfedv2, use_fwbawb
use bbbem_emissions , only:  bbbem_g                     
use gfedv2_emissions, only:  gfedv2_g                     
use fwbawb_emissions, only : fwbawb_g
use AeM_emission_factors 
use emiss_vars_emissions

implicit none 
integer, intent(in) :: isp,nx,ny
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  
character (len=*) ::  source_type

real :: dummy(nx,ny), factor

!-- AeM    |    aer 

IF(aerosol_mechanism == 'MATRIX') then

  if(spc_name == 'boc_bcar') then
   ident = BC
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   
   if(use_bbem .ne. 0 .and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = bbbem_g (ident)%src(:,:,1)
   
   if(use_gfedv2 == 1 .and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)
   
   if((use_bbem .ne. 0 .or. use_gfedv2 == 1 ).and. trim(source_type) == 'bburn') found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name

   return
 endif
 if(spc_name == 'boc_ocar') then
   ident = OC
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = bbbem_g (ident)%src(:,:,1)
   if(use_gfedv2 == 1.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)
   
   if((use_bbem .ne. 0 .or. use_gfedv2 == 1).and. trim(source_type) == 'bburn') found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name

   return
 endif
 
 ! akk_sulf      =>    1% 2.5% SO2
 if(spc_name == 'akk_sulf') then
   ident = SO2
   factor = 0.01 * 0.025 * 96.07/62.66 
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = factor*bbbem_g (ident)%src(:,:,1)
   if(use_gfedv2 == 1.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = factor*gfedv2_g(ident)%src(:,:,1)
   
   if((use_bbem .ne. 0 .or. use_gfedv2 == 1).and. trim(source_type) == 'bburn') found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + factor*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name

   return
 endif
 if(spc_name == 'acc_sulf') then
   ident = SO2
   factor = 0.99 * 0.025 * 96.07/62.66 
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = factor*bbbem_g (ident)%src(:,:,1)
   if(use_gfedv2 == 1.and. trim(source_type) == 'bburn') emiss_g(iespc)%src_bburn(:,:,1)   = factor*gfedv2_g(ident)%src(:,:,1)
   
   if((use_bbem .ne. 0 .or. use_gfedv2 == 1).and. trim(source_type) == 'bburn') found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + factor*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name

   return
 endif
  
ENDIF


end subroutine convert_AeM_to_aer
