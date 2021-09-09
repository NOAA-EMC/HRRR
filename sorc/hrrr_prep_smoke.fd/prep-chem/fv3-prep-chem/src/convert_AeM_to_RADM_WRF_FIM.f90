!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_AeM_to_wrf(isp,iespc,ident,source_type,spc_name)
!use chem1_list
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
character (len=*), intent(in)  :: spc_name  !kml 


!--     AeM    |    RACM 

!- NOx	=>   NO
!      change MP 201108 : NO=NOX/2,   NO2 = NOX/2 in molecules (40% and 60% in mass)
if(spc_name == 'NO') then
   ident = NOx
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 0.4* bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.4*gfedv2_g(ident)%src(:,:,1)
   
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + 0.4*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1


   print*,'==> converted from AeM - found for ',spc_name

   return
endif
if(spc_name == 'NO2') then
   ident = NOx
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 0.6* bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 0.6*gfedv2_g(ident)%src(:,:,1)
   
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn)    = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + 0.6*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1


   print*,'==> converted from AeM - found for ',spc_name

   return
endif


!----------------------------------------------------------------------------
!- C2H6 =>	ETH
if(spc_name == 'ETH') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!- C2H4	=> ETE
if(spc_name == 'ETE') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!- Xylene   =>     XYL
if(spc_name == 'XYL') then
   ident = xylenes
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
! - CSL (m=108)
! Phenol (m=94)
!RACM)
if(spc_name == 'CSL') then
   ident = Phenol
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 108/94* bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = 108/94*gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + 108/94*fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!Formaldehyde	      =>  HCHO
if(spc_name == 'HCHO') then
   ident = Formaldehyde
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!HFo	=>       ORA1
if(spc_name == 'ORA1') then
   ident = HFo
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif



!----------------------------------------------------------------------------

!Isoprene  =>	ISO
if(spc_name == 'ISO') then
   ident = Isoprene
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(ident)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   = gfedv2_g(ident)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                          + fwbawb_g(ident)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!---------------------------------------------------------------------------
!-ORA2 (m=60)
! HAc (m=60)
! Propanoic (m=74)
! HAc	     =>   ORA2
! Propanoic  =>   ORA2
if(spc_name == 'ORA2') then
   ident = HAc
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name

   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  bbbem_g(HAc	     )%src(:,:,1)  + &
                                                           60/74* bbbem_g(Propanoic)%src(:,:,1)     
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   =  gfedv2_g(HAc      )%src(:,:,1) + &
   							    60/74*gfedv2_g(Propanoic)%src(:,:,1)
							    
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                                                            fwbawb_g(HAc      )%src(:,:,1) + &
   							                                   60/74*fwbawb_g(Propanoic)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-TOL (m=92)
! benzene (m=82)
! toluene (m=92)
! ethylbenzene (m=106)
! styrene (m=104)
! PAH (m=164)
!benzene	  =>      TOL
!toluene	  =>      TOL
!ethylbenzene	  =>      TOL
!styrene	  =>      TOL
!PAH	          =>      TOL
!    change MP 201108    Toluene only => TOL
if(spc_name == 'TOL') then
   ident = toluene
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name

   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   = 92/82*bbbem_g(benzene	)%src(:,:,1) + &
                                                            bbbem_g(toluene	)%src(:,:,1) + &
                                                           92/106*bbbem_g(ethylbenzene)%src(:,:,1) + &
                                                           92/104*bbbem_g(styrene	)%src(:,:,1) + &
                                                           92/164*bbbem_g(PAH 	)%src(:,:,1)    
      
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   =  92/82*gfedv2_g(benzene	 )%src(:,:,1) + &
   							    gfedv2_g(toluene	 )%src(:,:,1) + &
   							    92/106*gfedv2_g(ethylbenzene)%src(:,:,1) + &
   							    92/104*gfedv2_g(styrene	 )%src(:,:,1) + &
   							    92/164*gfedv2_g(PAH 	 )%src(:,:,1)	
							    
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          92/82*fwbawb_g(benzene	)%src(:,:,1) + &
   							  fwbawb_g(toluene	)%src(:,:,1) + &
   							  92/106*fwbawb_g(ethylbenzene )%src(:,:,1) + &
   							  92/104*fwbawb_g(styrene	)%src(:,:,1) + &
   							  92/164*fwbawb_g(PAH  	)%src(:,:,1)   

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!API        =>	0.5*terpenes
if(spc_name == 'API') then
   ident = TERPENES
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  0.5* bbbem_g(TERPENES)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   =  0.5*gfedv2_g(TERPENES)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                     + 0.5* fwbawb_g(TERPENES)%src(:,:,1)
   
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!LIM        =>	0.2*terpenes
if(spc_name == 'LIM') then
   ident = TERPENES
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1)   =  0.2* bbbem_g(TERPENES)%src(:,:,1)
   if(use_gfedv2 == 1) emiss_g(iespc)%src_bburn(:,:,1)   =  0.2*gfedv2_g(TERPENES)%src(:,:,1)

							    
   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) &
                                                                                     + 0.2* fwbawb_g(TERPENES)%src(:,:,1)
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-OLT (m=42)
! C3H6 (m=42)
! 1_butene (m=56)
! 1_pentene (m=70)
! 2_Me_Butene (m=70)
! 4_me_1_pentene (m=84)
! 2_me_1_pentene (m=84)
! 1_hexene (m=84)
! octenes (m=112)
!-C3H6	          =>	OLT
!-1_butene	  =>	OLT
!-1_pentene	  =>	OLT
!-2_Me_Butene	  =>	OLT
!-4_me_1_pentene  =>	OLT
!-2_me_1_pentene  =>	OLT
!-1_hexene	  =>    OLT
!-octenes	  =>    OLT
!     change MP 201108 C3H6 only => OLT
if(spc_name == 'OLT') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = bbbem_g(C3H6	      )%src(:,:,1) + &
 							42/56*bbbem_g(butene_1      )%src(:,:,1) + &
							42/70*bbbem_g(pentene_1     )%src(:,:,1) + &
  							42/70*bbbem_g(Butene_2_Me   )%src(:,:,1) + &
  							42/84*bbbem_g(pentene_4_me_1)%src(:,:,1) + &
  							42/84*bbbem_g(pentene_2_me_1)%src(:,:,1) + &
 							42/84*bbbem_g(hexene_1      )%src(:,:,1) + & 
							42/112*bbbem_g(octenes       )%src(:,:,1)    	       

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = gfedv2_g(C3H6	       )%src(:,:,1) + &
  							42/56*gfedv2_g(butene_1      )%src(:,:,1) + &
  							42/70*gfedv2_g(pentene_1     )%src(:,:,1) + &
   							42/70*gfedv2_g(Butene_2_Me   )%src(:,:,1) + &
   							42/84*gfedv2_g(pentene_4_me_1)%src(:,:,1) + &
  							42/84*gfedv2_g(pentene_2_me_1)%src(:,:,1) + &
 							42/84*gfedv2_g(hexene_1      )%src(:,:,1) + & 
							42/112*gfedv2_g(octenes       )%src(:,:,1)    	       


   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          fwbawb_g(C3H6 	 )%src(:,:,1) + &
  						          42/56*fwbawb_g(butene_1	 )%src(:,:,1) + &
  						          42/70*fwbawb_g(pentene_1	 )%src(:,:,1) + &
    						          42/70*fwbawb_g(Butene_2_Me   )%src(:,:,1) + &
   						          42/84*fwbawb_g(pentene_4_me_1)%src(:,:,1) + &
  						          42/84*fwbawb_g(pentene_2_me_1)%src(:,:,1) + &
 						          42/84*fwbawb_g(hexene_1	 )%src(:,:,1) + & 
						          42/112*fwbawb_g(octenes	 )%src(:,:,1)	 

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-KET (m=72)
!-Acetone (m=58)	  =>KET
!-2_Butanone (m=72)       =>KET
!-2_3_Butanedione (m=86)  =>KET
!-Pentanones (m=86)	  =>KET
!-Hexanones (m=100)	  =>KET
!-Heptanones (m=114)	  =>KET
!-Octanones (m=128	  =>KET
if(spc_name == 'KET') then
   ident = Acetone
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 72/58*bbbem_g(Acetone        )%src(:,:,1) + &
  							bbbem_g(Butanone_2     )%src(:,:,1) + &
  							72/86*bbbem_g(Butanedione_2_3)%src(:,:,1) + &
  							72/86*bbbem_g(Pentanones     )%src(:,:,1) + &
   							72/100*bbbem_g(Hexanones      )%src(:,:,1) + &
   							72/114*bbbem_g(Heptanones     )%src(:,:,1) + &
  							72/128*bbbem_g(Octanones      )%src(:,:,1)		

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 72/58*gfedv2_g(Acetone	)%src(:,:,1) + &
  							gfedv2_g(Butanone_2	)%src(:,:,1) + &
  							72/86*gfedv2_g(Butanedione_2_3)%src(:,:,1) + &
  							72/86*gfedv2_g(Pentanones	)%src(:,:,1) + &
   							72/100*gfedv2_g(Hexanones	)%src(:,:,1) + &
   							72/114*gfedv2_g(Heptanones	)%src(:,:,1) + &
  							72/128*gfedv2_g(Octanones	)%src(:,:,1)		


   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          72/58*fwbawb_g(Acetone	  )%src(:,:,1) + &
  							  fwbawb_g(Butanone_2	  )%src(:,:,1) + &
  							  72/86*fwbawb_g(Butanedione_2_3)%src(:,:,1) + &
  							  72/86*fwbawb_g(Pentanones	  )%src(:,:,1) + &
   							  72/100*fwbawb_g(Hexanones	  )%src(:,:,1) + &
  							  72/114*fwbawb_g(Heptanones	  )%src(:,:,1) + &
  							  72/128*fwbawb_g(Octanones	  )%src(:,:,1)

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------
!-DIEN (m=54)
!butadiene (m=54)	       => DIEN
!pentadienes (m=68)	       => DIEN
!cyclopentadiene (m=66)       => DIEN
!hexadienes (m=82)	       => DIEN
if(spc_name == 'DIEN') then
   ident = butadiene
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = bbbem_g(butadiene      )%src(:,:,1) + &
  							54/68*bbbem_g(pentadienes    )%src(:,:,1) + &
  							54/66*bbbem_g(cyclopentadiene)%src(:,:,1) + &
  							54/82*bbbem_g(hexadienes     )%src(:,:,1) 		


   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = gfedv2_g(butadiene	)%src(:,:,1) + &
  							54/68*gfedv2_g(pentadienes	)%src(:,:,1) + &
  							54/66*gfedv2_g(cyclopentadiene)%src(:,:,1) + &
  							54/82*gfedv2_g(hexadienes	)%src(:,:,1)		

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          fwbawb_g(butadiene	  )%src(:,:,1) + &
  							  54/68*fwbawb_g(pentadienes	  )%src(:,:,1) + &
  							  54/66*fwbawb_g(cyclopentadiene)%src(:,:,1) + &
  							  54/82*fwbawb_g(hexadienes	  )%src(:,:,1)


   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-HC8 (m=114)
!heptane (m=100)	 => HC8
!Butanols (m=74)	 => HC8
!cyclopentanol (m=86)	 => HC8
if(spc_name == 'HC8') then
   ident = heptane
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 114/100*bbbem_g(heptane         )%src(:,:,1) + &
  							114/74*bbbem_g(Butanols     )%src(:,:,1) + &
  							114/86*bbbem_g(cyclopentanol)%src(:,:,1)	   

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 114/100*gfedv2_g(heptane      )%src(:,:,1) + &
  							114/74*gfedv2_g(Butanols     )%src(:,:,1) + &
  							114/86*gfedv2_g(cyclopentanol)%src(:,:,1)	    

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
 							  114/100*fwbawb_g(heptane      )%src(:,:,1) + &
  							  114/74*fwbawb_g(Butanols     )%src(:,:,1) + &
  							  114/86*fwbawb_g(cyclopentanol)%src(:,:,1)	    


   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------
!-OLI (m=68)
!tr_2_butene (m=56) (butene_tr_2)  => OLI 
!cis_2_butene (m=56)	 (butene_cis_2) => OLI 
!-2_pentene (m=70)	                => OLI
!cyclopentene (m=68)	                => OLI
!i-butene (butene_i)(m=56)	        => OLT or OLI
if(spc_name == 'OLI') then
   ident = butene_tr_2
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 68/56*bbbem_g(butene_tr_2 )%src(:,:,1) + &
  							68/56*bbbem_g(butene_cis_2)%src(:,:,1) + &
  							68/70*bbbem_g(pentene_2   )%src(:,:,1) + &
  							bbbem_g(cyclopentene)%src(:,:,1) + &
  							68/56*bbbem_g(butene_i    )%src(:,:,1)	  

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 68/56*gfedv2_g(butene_tr_2 )%src(:,:,1) + &
  							68/56*gfedv2_g(butene_cis_2)%src(:,:,1) + &
  							68/70*gfedv2_g(pentene_2     )%src(:,:,1) + &
  							gfedv2_g(cyclopentene)%src(:,:,1) + &
  							68/56*gfedv2_g(butene_i    )%src(:,:,1)	  


   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1

   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
  							  68/56*fwbawb_g(pentene_2   )%src(:,:,1) + &
                                                          68/56*fwbawb_g(butene_tr_2 )%src(:,:,1) + &
  							  68/70*fwbawb_g(butene_cis_2)%src(:,:,1) + &
  							  fwbawb_g(cyclopentene)%src(:,:,1) + &
  							  68/56*fwbawb_g(butene_i    )%src(:,:,1)	  

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-ALD (m=44)
!Acetald (m=44)	                 =>   ALD
!Hydroxyacetaldehyde (m=60)	=>   ALD
!Propanal (m=58)	   	=>   ALD
!Butanals (m=72)	   	=>   ALD
!Hexanals (m=86)             	=>   ALD
!Heptanals (m=100)	   	=>   ALD
!Benzaldehyde (m=106)	   	=>   ALD
if(spc_name == 'ALD') then
   ident = Acetald
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = bbbem_g(Acetald 	   )%src(:,:,1) + &
 							44/60*bbbem_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							44/58*bbbem_g(Propanal	   )%src(:,:,1) + &
  							44/72*bbbem_g(Butanals	   )%src(:,:,1) + &
  							44/86*bbbem_g(Hexanals	   )%src(:,:,1) + &
  							44/100*bbbem_g(Heptanals	   )%src(:,:,1) + &
  							44/106*bbbem_g(Benzaldehyde	   )%src(:,:,1) 	 

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = gfedv2_g(Acetald	    )%src(:,:,1) + &
  							44/60*gfedv2_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							44/58*gfedv2_g(Propanal	    )%src(:,:,1) + &
  							44/72*gfedv2_g(Butanals	    )%src(:,:,1) + &
  							44/86*gfedv2_g(Hexanals	    )%src(:,:,1) + &
 							44/100*gfedv2_g(Heptanals	    )%src(:,:,1) + &
  							44/106*gfedv2_g(Benzaldehyde	    )%src(:,:,1)	  

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
							  fwbawb_g(Acetald	      )%src(:,:,1) + &
               						  44/60*fwbawb_g(Hydroxyacetaldehyde)%src(:,:,1) + &
  							  44/58*fwbawb_g(Propanal	      )%src(:,:,1) + &
  							  44/72*fwbawb_g(Butanals	      )%src(:,:,1) + &
  							  44/86*fwbawb_g(Hexanals	      )%src(:,:,1) + &
 							  44/100*fwbawb_g(Heptanals	      )%src(:,:,1) + &
  							  44/106*fwbawb_g(Benzaldehyde       )%src(:,:,1)


   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-HC3 (m=44)
!2_Me-butane (m=72)	 => HC3
!C3H8 (m=44)		 => HC3
!n_butane (m=58)	 => HC3
!i-butane (m=58)	 => HC3
!Methanol (m=32)	 => HC3
!Ethanol (m=46) 	 => HC3
!C2H2 (m=26)		 => HC3
if(spc_name == 'HC3') then
   ident = butane_2_Me
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = 44/72*bbbem_g(butane_2_Me)%src(:,:,1) + &
  							bbbem_g(C3H8       )%src(:,:,1) + &
  							44/58*bbbem_g(butane_n   )%src(:,:,1) + &
  							44/58*bbbem_g(butane_i   )%src(:,:,1) + &
  							44/32*bbbem_g(Methanol   )%src(:,:,1) + &
  							44/46*bbbem_g(Ethanol    )%src(:,:,1) + &
  							44/26*bbbem_g(C2H2       )%src(:,:,1) 	 

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = 44/72*gfedv2_g(butane_2_Me)%src(:,:,1) + &
  							gfedv2_g(C3H8	    )%src(:,:,1) + &
  							44/58*gfedv2_g(butane_n   )%src(:,:,1) + &
  							44/58*gfedv2_g(butane_i   )%src(:,:,1) + &
  							44/32*gfedv2_g(Methanol   )%src(:,:,1) + &
 							44/46*gfedv2_g(Ethanol    )%src(:,:,1) + &
  							44/26*gfedv2_g(C2H2	    )%src(:,:,1)	  

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
   
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
                                                          44/72*fwbawb_g(butane_2_Me)%src(:,:,1) + &
			       				  fwbawb_g(C3H8       )%src(:,:,1) + &
			        			  44/58*fwbawb_g(butane_n   )%src(:,:,1) + &
				        		  44/58*fwbawb_g(butane_i   )%src(:,:,1) + &
					        	  44/32*fwbawb_g(Methanol   )%src(:,:,1) + &
							  44/46*fwbawb_g(Ethanol    )%src(:,:,1) + &
							  44/26*fwbawb_g(C2H2       )%src(:,:,1) 
!                                                        
!                                                                                                               

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!-HC5 (m=72)
!n_pentane (m=72)	=> HC5
!n_hexane (m=86)	=> HC5
!isohexanes (m=86)	=> HC5
!1_propanol (m=60)	=> HC5
!2-propanol (m=60)	=> HC5
if(spc_name == 'HC5') then
   ident = pentane_n
   iespc=iespc+1
   emiss_spc_name(iespc,bburn)           = spc_name
   if(use_bbem .ne. 0) emiss_g(iespc)%src_bburn(:,:,1) = bbbem_g(pentane_n )%src(:,:,1) + &
  							72/86*bbbem_g(hexane_n  )%src(:,:,1) + &
  							72/86*bbbem_g(isohexanes)%src(:,:,1) + &
  							72/60*bbbem_g(Propanol_1)%src(:,:,1) + &
  							72/60*bbbem_g(propanol_2)%src(:,:,1) 	

   if(use_gfedv2== 1) emiss_g(iespc)%src_bburn(:,:,1) = gfedv2_g(pentane_n )%src(:,:,1) + &
  							72/86*gfedv2_g(hexane_n  )%src(:,:,1) + &
  							72/86*gfedv2_g(isohexanes)%src(:,:,1) + &
  							72/60*gfedv2_g(Propanol_1)%src(:,:,1) + &
  							72/60*gfedv2_g(propanol_2)%src(:,:,1)

   if(use_bbem .ne. 0 .or. use_gfedv2 == 1) found_emiss_spc(iespc,bburn) = 1
      
   !- this emission is included in the "antro" category 
   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1)+ &
							  fwbawb_g(pentane_n )%src(:,:,1) + &
  							  72/86*fwbawb_g(hexane_n  )%src(:,:,1) + &
  							  72/86*fwbawb_g(isohexanes)%src(:,:,1) + &
  							  72/60*fwbawb_g(Propanol_1)%src(:,:,1) + &
  							  72/60*fwbawb_g(propanol_2)%src(:,:,1)

   if(use_fwbawb == 1 .and. trim(source_type) == 'antro') found_emiss_spc(iespc,antro)    = 1

   print*,'==> converted from AeM - found for ',spc_name
   return
endif

end subroutine convert_AeM_to_wrf
