!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_AeM_to_relacs_reac(isp,iespc,ident,source_type,spc_name_dummy)
use chem1_list
!use chem1_list, only : alke, bio,ora2,aro,ket,alka,ald

use grid_dims_out, only:  use_bbem, use_gfedv2, use_fwbawb
use emiss_vars_emissions
use bbbem_emissions , only:  bbbem_g                     
use gfedv2_emissions, only:  gfedv2_g                     
use fwbawb_emissions, only : fwbawb_g

use AeM_emission_factors 


!implicit none  
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*) source_type
character (len=*), intent(in)  :: spc_name_dummy !kml 


!--     AeM    |    RELACS
!--     Emissions for RACM weighted by Agregation factor Agg (only organics)
!--     Agregation factors for RELACS can be found in Crassier et al 2000, 
!-- 	Atmos.Environ. 34 2633-2644
!--     Agregation factors for RACM can be found in Stockwell et al 1997, 
!-- 	J. Geos Res. 102, 25847-25879


!- NOx	=>   NO
!      change MP 201108 : NO=NOX/2,   NO2 = NOX/2 in molecules (40% and 60% in mass)
if(spc_name_dummy == 'NO') then
   ident = NOx
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 0.4* bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.4*gfedv2_g(ident)%src(:,:,1)
   
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + 0.4*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1


   print*,'==> converted from AeM - found for ',spc_name_dummy

   return
endif
if(spc_name_dummy == 'NO2') then
   ident = NOx
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 0.6* bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.6*gfedv2_g(ident)%src(:,:,1)
   
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + 0.6*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1


   print*,'==> converted from AeM - found for ',spc_name_dummy

   return
endif


!----------------------------------------------------------------------------
!- C2H6 =>	ETH (Agg_RACM=1) (Agg=1)
if(spc_name_dummy == 'ETH') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)

   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!----------------------------------------------------------------------------
!- ALKE  (m=33)  
!- C2H4	(m=28)				=> ALKE (Agg_RACM=1) (ETE, Agg=0.96)
!tr_2_butene (m=56) (butene_tr_2)   	=> ALKE (Agg_RACM=1) (OLI, Agg=1.04)
!cis_2_butene (m=56)     (butene_cis_2) => ALKE (Agg_RACM=1) (OLI, Agg=1.04)
!-2_pentene (m=70)                      => ALKE (Agg_RACM=1) (OLI, Agg=1.04)
!cyclopentene (m=68)                    => ALKE (Agg_RACM=0.5) (OLI, Agg=1.04)
!i-butene (butene_i)(m=56)              => ALKE (Agg_RACM=0.5) (OLI, Agg=1.04)
!butadiene (m=54)              		=> ALKE (Agg_RACM=1) (DIEN, Agg=1.04)
!pentadienes (m=68)            		=> ALKE (Agg_RACM=1) (DIEN, Agg=1.04)
!cyclopentadiene (m=66)       		=> ALKE (Agg_RACM=1) (DIEN, Agg=1.04)
!hexadienes (m=82)             		=> ALKE (Agg_RACM=1) (DIEN, Agg=1.04)
! C3H6 (m=42)   			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 1_butene (m=56)   			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 1_pentene (m=70)  			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 2_Me_Butene (m=70)			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 4_me_1_pentene (m=84) 		=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 2_me_1_pentene (m=84)			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! 1_hexene (m=84)			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
! octenes (m=112)			=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)



if(spc_name_dummy == 'ALKE') then
   ident = ALKE   
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  0.96*bbbem_g(C2H4)%src(:,:,1)+ &
                                                        1.04*bbbem_g(butene_tr_2 )%src(:,:,1) + &
                                                        1.04*bbbem_g(butene_cis_2)%src(:,:,1) + &
                                                        1.04*bbbem_g(pentene_2   )%src(:,:,1) + &
                                                        0.5*1.04*bbbem_g(cyclopentene)%src(:,:,1) + &
                                                        0.5*1.04*bbbem_g(butene_i    )%src(:,:,1) + &
                                                        1.04*bbbem_g(butadiene      )%src(:,:,1) + &
                                                        1.04*bbbem_g(pentadienes    )%src(:,:,1) + &
                                                        1.04*bbbem_g(cyclopentadiene)%src(:,:,1) + &
                                                        1.04*bbbem_g(hexadienes     )%src(:,:,1) + &
							1.04*bbbem_g(C3H6	     )%src(:,:,1) + &
 							1.04*bbbem_g(butene_1      )%src(:,:,1) + &
							1.04*bbbem_g(pentene_1     )%src(:,:,1) + &
  							1.04*bbbem_g(Butene_2_Me   )%src(:,:,1) + &
  							1.04*bbbem_g(pentene_4_me_1)%src(:,:,1) + &
  							1.04*bbbem_g(pentene_2_me_1)%src(:,:,1) + &
 							1.04*bbbem_g(hexene_1      )%src(:,:,1) + & 
							1.04*bbbem_g(octenes       )%src(:,:,1)   

   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.96*gfedv2_g(C2H4)%src(:,:,1)+ &
                                                        1.04*gfedv2_g(butene_tr_2 )%src(:,:,1) + &
                                                        1.04*gfedv2_g(butene_cis_2)%src(:,:,1) + &
                                                        1.04*gfedv2_g(pentene_2     )%src(:,:,1) + &
                                                        0.5*1.04*gfedv2_g(cyclopentene)%src(:,:,1) + &
                                                        0.5*1.04*gfedv2_g(butene_i    )%src(:,:,1) + &
							1.04*gfedv2_g(butadiene	)%src(:,:,1) + &
  							1.04*gfedv2_g(pentadienes	)%src(:,:,1) + &
  							1.04*gfedv2_g(cyclopentadiene)%src(:,:,1) + &
  							1.04*gfedv2_g(hexadienes	)%src(:,:,1) + &
							1.04*gfedv2_g(C3H6	       )%src(:,:,1) + &
  							1.04*gfedv2_g(butene_1      )%src(:,:,1) + &
  							1.04*gfedv2_g(pentene_1     )%src(:,:,1) + &
   							1.04*gfedv2_g(Butene_2_Me   )%src(:,:,1) + &
   							1.04*gfedv2_g(pentene_4_me_1)%src(:,:,1) + &
  							1.04*gfedv2_g(pentene_2_me_1)%src(:,:,1) + &
 							1.04*gfedv2_g(hexene_1      )%src(:,:,1) + & 
							1.04*gfedv2_g(octenes       )%src(:,:,1) 
							    	       

							


   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) +&
                                                        0.96*fwbawb_g(C2H4)%src(:,:,1)+ &
                                                        1.04*fwbawb_g(butene_tr_2 )%src(:,:,1) + &
                                                        1.04*fwbawb_g(butene_cis_2)%src(:,:,1) + &
                                                        1.04*fwbawb_g(pentene_2     )%src(:,:,1) + &
                                                        0.5*1.04*fwbawb_g(cyclopentene)%src(:,:,1) + &
                                                        0.5*1.04*fwbawb_g(butene_i    )%src(:,:,1) + &
							1.04*fwbawb_g(butadiene	)%src(:,:,1) + &
  							1.04*fwbawb_g(pentadienes	)%src(:,:,1) + &
  							1.04*fwbawb_g(cyclopentadiene)%src(:,:,1) + &
  							1.04*fwbawb_g(hexadienes	)%src(:,:,1) + &
							1.04*fwbawb_g(C3H6	       )%src(:,:,1) + &
  							1.04*fwbawb_g(butene_1      )%src(:,:,1) + &
  							1.04*fwbawb_g(pentene_1     )%src(:,:,1) + &
   							1.04*fwbawb_g(Butene_2_Me   )%src(:,:,1) + &
   							1.04*fwbawb_g(pentene_4_me_1)%src(:,:,1) + &
  							1.04*fwbawb_g(pentene_2_me_1)%src(:,:,1) + &
 							1.04*fwbawb_g(hexene_1      )%src(:,:,1) + & 
							1.04*fwbawb_g(octenes       )%src(:,:,1)+ &
                                                        1.04*fwbawb_g(pentene_2   )%src(:,:,1) 
   
							  

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif



!----------------------------------------------------------------------------
!Formaldehyde	      =>  HCHO   (Agg_RACM=1) (Agg=1)
if(spc_name_dummy == 'HCHO') then
   ident = Formaldehyde
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!----------------------------------------------------------------------------
!HFo	=>       ORA1   (Agg_RACM=1) (Agg=1)
if(spc_name_dummy == 'ORA1') then
   ident = HFo
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif



!----------------------------------------------------------------------------
! BIO (m=68)   (Agg_RACM=1) (Agg=1)
!Isoprene  (m=68)		=>	BIO
!API (m=136) 0.5*terpenes	=>	BIO
!LIM (m=136) 0.2*terpenes	=>	BIO

if(spc_name_dummy == 'BIO') then
   ident = BIO 
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(Isoprene)%src(:,:,1) + 0.7*bbbem_g(TERPENES)%src(:,:,1) 
                                                            
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(Isoprene)%src(:,:,1) + 0.7*gfedv2_g(TERPENES)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(Isoprene)%src(:,:,1) + &
											  0.7*fwbawb_g(TERPENES)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!---------------------------------------------------------------------------
!-ORA2 (m=60)   (Agg_RACM=1) (Agg=1)
! HAc (m=60)
! Propanoic (m=74)
! HAc	     =>   ORA2
! Propanoic  =>   ORA2
if(spc_name_dummy == 'ORA2') then
   ident = ORA2
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy

   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(HAc	     )%src(:,:,1)  + &
                                                            bbbem_g(Propanoic)%src(:,:,1)     
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   =  gfedv2_g(HAc      )%src(:,:,1) + &
   							    gfedv2_g(Propanoic)%src(:,:,1)
							    
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                                                            fwbawb_g(HAc      )%src(:,:,1) + &
   							                                   fwbawb_g(Propanoic)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!----------------------------------------------------------------------------
!-ARO (m=98)
! benzene (m=82)		=> ARO (Agg_RACM=0.29) (TOL, Agg=0.87)
! toluene (m=92)		=> ARO (Agg_RACM=1) (TOL, Agg=0.87)
! ethylbenzene (m=106)		=> ARO (Agg_RACM=1) (TOL, Agg=0.87)
! styrene (m=104)		=> ARO (Agg_RACM=1) (TOL, Agg=0.87)
! PAH (m=164)			=> ARO (Agg_RACM=1) (TOL, Agg=0.87)
! XYL 	(m=106)			=> ARO (Agg_RACM=1) (TOL, Agg=1.04)
! Phenol (m=94)	(CSL,(m=108))	=> ARO (Agg_RACM=1) (CSL, Agg=1.04)

if(spc_name_dummy == 'ARO') then
   ident = ARO
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy

   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 0.29*0.87*bbbem_g(benzene	)%src(:,:,1) + &
                                                           0.87*bbbem_g(toluene	)%src(:,:,1) + &
                                                           0.87*bbbem_g(ethylbenzene)%src(:,:,1) + &
                                                           0.87*bbbem_g(styrene	)%src(:,:,1) + &
                                                           0.87*bbbem_g(PAH 	)%src(:,:,1)  + &
							   1.04*bbbem_g(xylenes)%src(:,:,1) + &
							   1.04*bbbem_g(Phenol)%src(:,:,1) 
      
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.29*0.87*gfedv2_g(benzene	)%src(:,:,1) + &
                                                           0.87*gfedv2_g(toluene	)%src(:,:,1) + &
                                                           0.87*gfedv2_g(ethylbenzene)%src(:,:,1) + &
                                                           0.87*gfedv2_g(styrene	)%src(:,:,1) + &
                                                           0.87*gfedv2_g(PAH 	)%src(:,:,1)  + &
							   1.04*gfedv2_g(xylenes)%src(:,:,1) + &
							   1.04*gfedv2_g(Phenol)%src(:,:,1) 

							   
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                           0.29*0.87*fwbawb_g(benzene	)%src(:,:,1) + &
                                                           0.87*fwbawb_g(toluene	)%src(:,:,1) + &
                                                           0.87*fwbawb_g(ethylbenzene)%src(:,:,1) + &
                                                           0.87*fwbawb_g(styrene	)%src(:,:,1) + &
                                                           0.87*fwbawb_g(PAH 	)%src(:,:,1)  + &
							   1.04*fwbawb_g(xylenes)%src(:,:,1) + &
							   1.04*fwbawb_g(Phenol)%src(:,:,1) 

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif



!----------------------------------------------------------------------------
!-KET (m=72)
!-Acetone (m=58)	  =>KET (Agg_RACM=0.33) (Agg=1)
!-2_Butanone (m=72)       =>KET (Agg_RACM=1.61) (Agg=1)
!-2_3_Butanedione (m=86)  =>KET (Agg_RACM=1.61) (Agg=1)
!-Pentanones (m=86)	  =>KET (Agg_RACM=1.61) (Agg=1)
!-Hexanones (m=100)	  =>KET (Agg_RACM=1.61) (Agg=1)
!-Heptanones (m=114)	  =>KET (Agg_RACM=1.61) (Agg=1)
!-Octanones (m=128	  =>KET (Agg_RACM=1.61) (Agg=1)

if(spc_name_dummy == 'KET') then
   ident = KET
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 0.33*bbbem_g(Acetone        )%src(:,:,1) + &
  							1.61*bbbem_g(Butanone_2     )%src(:,:,1) + &
  							1.61*bbbem_g(Butanedione_2_3)%src(:,:,1) + &
  							1.61*bbbem_g(Pentanones     )%src(:,:,1) + &
   							1.61*bbbem_g(Hexanones      )%src(:,:,1) + &
   							1.61*bbbem_g(Heptanones     )%src(:,:,1) + &
  							1.61*bbbem_g(Octanones      )%src(:,:,1)		

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 0.33*gfedv2_g(Acetone	)%src(:,:,1) + &
  							1.61*gfedv2_g(Butanone_2	)%src(:,:,1) + &
  							1.61*gfedv2_g(Butanedione_2_3)%src(:,:,1) + &
  							1.61*gfedv2_g(Pentanones	)%src(:,:,1) + &
   							1.61*gfedv2_g(Hexanones	)%src(:,:,1) + &
   							1.61*gfedv2_g(Heptanones	)%src(:,:,1) + &
  							1.61*gfedv2_g(Octanones	)%src(:,:,1)		


   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          0.33*fwbawb_g(Acetone	  )%src(:,:,1) + &
  							  1.61*fwbawb_g(Butanone_2	  )%src(:,:,1) + &
  							  1.61*fwbawb_g(Butanedione_2_3)%src(:,:,1) + &
  							  1.61*fwbawb_g(Pentanones	  )%src(:,:,1) + &
   							  1.61*fwbawb_g(Hexanones	  )%src(:,:,1) + &
  							  1.61*fwbawb_g(Heptanones	  )%src(:,:,1) + &
  							  1.61*fwbawb_g(Octanones	  )%src(:,:,1)

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!----------------------------------------------------------------------------
!- ALKA (m=62)
!heptane (m=100) (HC8 (m=114))  	=> ALKA (Agg_RACM=0.94) (HC8 )
!Butanols (m=74) (HC8 (m=114))  	=> ALKA (Agg_RACM=0.91) (HC8 )*	
!cyclopentanol (m=86) (HC8 (m=114)) 	=> ALKA (Agg_RACM=1.01) (HC8 )*
!* Calculated using (16) of Stockwell 1997
! k_i from Atkinson and Arey, 2003
! Chemical Reviews, Vol. 103, No. 12
!2_Me-butane (m=72) HC3 (m=44) 		=> ALKA (Agg_RACM=1.11) (HC3 )
!C3H8 (m=44)	    HC3 (m=44) 		=> ALKA (Agg_RACM=0.57) (HC3 )
!n_butane (m=58)    HC3 (m=44) 		=> ALKA (Agg_RACM=1.11) (HC3 )
!i-butane (m=58)    HC3 (m=44) 		=> ALKA (Agg_RACM=1.11) (HC3 )
!Methanol (m=32)    HC3 (m=44) 		=> ALKA (Agg_RACM=0.49) (HC3 )
!Ethanol (m=46)     HC3 (m=44) 		=> ALKA (Agg_RACM=1.37) (HC3 )
!C2H2 (m=26)        HC3 (m=44) 		=> ALKA (Agg_RACM=0.41) (HC3 )
!n_pentane (m=72)   HC5 (m=72) 		=> ALKA (Agg_RACM=0.97) (HC5 )
!n_hexane (m=86)    HC5 (m=72) 		=> ALKA (Agg_RACM=0.97) (HC5 )
!isohexanes (m=86)  HC5 (m=72) 		=> ALKA (Agg_RACM=0.97) (HC5 )
!1_propanol (m=60)  HC5 (m=72)	  	=> ALKA (Agg_RACM=1.07) (HC5 )
!2-propanol (m=60)  HC5 (m=72) 		=> ALKA (Agg_RACM=1.07) (HC5 )
!--HC3 0.77
!--HC5 1.23
!--HC8 1.58

if(spc_name_dummy == 'ALKA') then
   ident = ALKA
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 1.58*0.94*bbbem_g(heptane         )%src(:,:,1) + &
  							1.58*0.91*bbbem_g(Butanols     )%src(:,:,1) + &
  							1.58*1.01*bbbem_g(cyclopentanol)%src(:,:,1) + &
							0.77*1.11*bbbem_g(butane_2_Me)%src(:,:,1) + &
  							0.77*0.57*bbbem_g(C3H8       )%src(:,:,1) + &
  							0.77*1.11*bbbem_g(butane_n   )%src(:,:,1) + &
  							0.77*1.11*bbbem_g(butane_i   )%src(:,:,1) + &
  							0.77*0.49*bbbem_g(Methanol   )%src(:,:,1) + &
  							0.77*1.37*bbbem_g(Ethanol    )%src(:,:,1) + &
  							0.77*0.41*bbbem_g(C2H2       )%src(:,:,1) + &
							1.23*0.97*bbbem_g(pentane_n )%src(:,:,1) + &
  							1.23*0.97*bbbem_g(hexane_n  )%src(:,:,1) + &
  							1.23*0.97*bbbem_g(isohexanes)%src(:,:,1) + &
  							1.23*1.07*bbbem_g(Propanol_1)%src(:,:,1) + &
  							1.23*1.07*bbbem_g(propanol_2)%src(:,:,1) 	
	 	   

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 1.58*0.94*gfedv2_g(heptane         )%src(:,:,1) + &
  							1.58*0.91*gfedv2_g(Butanols     )%src(:,:,1) + &
  							1.58*1.01*gfedv2_g(cyclopentanol)%src(:,:,1) + &
							0.77*1.11*gfedv2_g(butane_2_Me)%src(:,:,1) + &
  							0.77*0.57*gfedv2_g(C3H8       )%src(:,:,1) + &
  							0.77*1.11*gfedv2_g(butane_n   )%src(:,:,1) + &
  							0.77*1.11*gfedv2_g(butane_i   )%src(:,:,1) + &
  							0.77*0.49*gfedv2_g(Methanol   )%src(:,:,1) + &
  							0.77*1.37*gfedv2_g(Ethanol    )%src(:,:,1) + &
  							0.77*.41*gfedv2_g(C2H2       )%src(:,:,1) + &
							1.23*0.97*gfedv2_g(pentane_n )%src(:,:,1) + &
  							1.23*0.97*gfedv2_g(hexane_n  )%src(:,:,1) + &
  							1.23*0.97*gfedv2_g(isohexanes)%src(:,:,1) + &
  							1.23*1.07*gfedv2_g(Propanol_1)%src(:,:,1) + &
  							1.23*1.07*gfedv2_g(propanol_2)%src(:,:,1) 	
							

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
  							1.58*0.94*fwbawb_g(heptane         )%src(:,:,1) + &
  							1.58*0.91*fwbawb_g(Butanols     )%src(:,:,1) + &
  							1.58*1.01*fwbawb_g(cyclopentanol)%src(:,:,1) + &
							0.77*1.11*fwbawb_g(butane_2_Me)%src(:,:,1) + &
  							0.77*0.57*fwbawb_g(C3H8       )%src(:,:,1) + &
  							0.77*1.11*fwbawb_g(butane_n   )%src(:,:,1) + &
  							0.77*1.11*fwbawb_g(butane_i   )%src(:,:,1) + &
  							0.77*0.49*fwbawb_g(Methanol   )%src(:,:,1) + &
  							0.77*1.37*fwbawb_g(Ethanol    )%src(:,:,1) + &
  							0.77*.41*fwbawb_g(C2H2       )%src(:,:,1) + &
							1.23*0.97*fwbawb_g(pentane_n )%src(:,:,1) + &
  							1.23*0.97*fwbawb_g(hexane_n  )%src(:,:,1) + &
  							1.23*0.97*fwbawb_g(isohexanes)%src(:,:,1) + &
  							1.23*1.07*fwbawb_g(Propanol_1)%src(:,:,1) + &
  							1.23*1.07*fwbawb_g(propanol_2)%src(:,:,1) 	
							

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif

!----------------------------------------------------------------------------
!-ALD (m=44)   (Agg_RACM=1) (Agg=1)
!Acetald (m=44)	                 =>   ALD
!Hydroxyacetaldehyde (m=60)	=>   ALD
!Propanal (m=58)	   	=>   ALD
!Butanals (m=72)	   	=>   ALD
!Hexanals (m=86)             	=>   ALD
!Heptanals (m=100)	   	=>   ALD
!Benzaldehyde (m=106)	   	=>   ALD
if(spc_name_dummy == 'ALD') then
   ident = ALD
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name_dummy
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = bbbem_g(Acetald 	   )%src(:,:,1) + &
 							bbbem_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							bbbem_g(Propanal	   )%src(:,:,1) + &
  							bbbem_g(Butanals	   )%src(:,:,1) + &
  							bbbem_g(Hexanals	   )%src(:,:,1) + &
  							bbbem_g(Heptanals	   )%src(:,:,1) + &
  							bbbem_g(Benzaldehyde	   )%src(:,:,1) 	 

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = gfedv2_g(Acetald	    )%src(:,:,1) + &
  							gfedv2_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							gfedv2_g(Propanal	    )%src(:,:,1) + &
  							gfedv2_g(Butanals	    )%src(:,:,1) + &
  							gfedv2_g(Hexanals	    )%src(:,:,1) + &
 							gfedv2_g(Heptanals	    )%src(:,:,1) + &
  							gfedv2_g(Benzaldehyde	    )%src(:,:,1)	  

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
							  fwbawb_g(Acetald	      )%src(:,:,1) + &
               						  fwbawb_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							  fwbawb_g(Propanal	      )%src(:,:,1) + &
  							  fwbawb_g(Butanals	      )%src(:,:,1) + &
  							  fwbawb_g(Hexanals	      )%src(:,:,1) + &
 							  fwbawb_g(Heptanals	      )%src(:,:,1) + &
  							  fwbawb_g(Benzaldehyde       )%src(:,:,1)


   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name_dummy
   return
endif


end subroutine convert_AeM_to_relacs_reac
