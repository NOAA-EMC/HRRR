!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_ceds_to_wrf(isp,iespc,ident,spc_name)  !kml
!use chem1_list 


use emiss_vars_emissions
use ceds_emissions, only:   ceds_nspecies=>nspecies&
                            ,ceds_spc_name=>spc_name&
			    ,ceds_g                 &
			    ,CO                      &
			    ,NOx                     &
			    ,CO2                     &
			    ,CH4                     &
			    ,SO2
!		    ,N2O                     &
!		    ,NMVOC                   &
!		    ,SO4		       


!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml 

!molar mass weighting!
!--     EDGAR    |    RACM

!--------------------------------------------------------------------
!--	VOC BREAKDOWN PERFIL  
!- 	(Third Assessment Report to IPCC, TAR)
!--     			Industrial	Biomass burning
!--     Species		wt%	#C atoms	wt%	#C atoms
!--     Alcohols		3.2	2.5		8.1	1.5
!--     Ethane		4.7	2		7	2
!--     Propane		5.5	3		2	3
!--     Butanes		10.9	4		0.6	4
!--     Pentanes		9.4	5		1.4	5
!--     Higher alkanes	18.2	7.5		1.3	8
!--     Ethene		5.2	2		14.6	2
!--     Propene		2.4	3		7	3
!--     Ethyne		2.2	2		6	2
!--     Other alkenes, alkynes, dienes	3.8	4.8	7.6	4.6
!--     Benzene		3	6		9.5	6
!--     Toluene		4.9	7		4.1	7
!--     Xylene		3.6	8		1.2	8
!--     Trimethylbenzene	0.7	9		-	-
!--     Other aromatics	3.1	9.6		1	8
!--     Esters		1.4	5.2		-	-
!--     Ethers		1.7	4.7		5.5	5
!--     Chlorinated HC's	0.5	2.6		-	-
!--     Formaldehyde	0.5	1		1.2	1
!--     Other aldehydes	1.6	3.7		6.1	3.7
!--     Ketones		1.9	4.6		0.8	3.6
!--     Acids			3.6	1.9		15.1	1.9
!--     Others		8.1	4.9		-	-
!--
!--	All emissions will be consedered as industrial´s 
!-----------------------------------------------------------------------------------
!--------------------------------------------------------------------
!--	VOC from TAR -> RACM  
!--     TAR	 			RACM			
!--     Alcohols			HC3, HC5, HC8	
!--     Ethane				ETH					
!--     Propane				HC3			
!--     Butanes				HC3			
!--     Pentanes			HC5		
!--     Higher alkanes			HC5, HC8	
!--     Ethene				ETE			
!--     Propene				OLT			
!--     Ethyne				
!--     Other alkenes, alkynes, dienes	HC3, HC5, HC8	
!--     Benzene				TOL						
!--     Toluene				TOL						
!--     Xylene				XYL					
!--     Trimethylbenzene		TOL?				
!--     Other aromatics						
!--     Esters				HC3, HC5				
!--     Ethers				HC8			
!--     Chlorinated HC's		HC3		
!--     Formaldehyde			HCHO	
!--     Other aldehydes			ALD			
!--     Ketones	KET			
!--     Acids				ORA1, ORA2				
!--     Others	
!-----------------------------------------------------------------------------------!-- 	


!if (isp.le.chem_nspecies) then
!     spc_name(isp)    = spc_chem_name (isp)
! else
!    iv=chem_nspecies
!endif
!if(isp.gt.chem_nspecies) then
!   iv=iv+1
!   spc_name(iv)    = spc_aer_name (imode,isp)
! endif


!----------------------------------------------------------------------------

!- NOx	=>NO, NO2 (?) change MP201108: NO=NOX/2 and NO2=NOX/2 in molecules (40%
!and 60% in mass)
if(spc_name == 'NO') then
   ident = NOx
   iespc = iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.9*ceds_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from ceds - found for ',spc_name
   return
endif
if(spc_name == 'NO2') then
   ident = NOx
   iespc = iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.1*ceds_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!NoHC!- C2H6 =>	ETH   (wt%=4.7)
!NoHCif(spc_name == 'ETH') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.047*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC!HC3 (m=44)
!NoHC! Alcohols (Stockwell, 1997) 96% in molecules (mean weight m=45) (95% in mass)  
!NoHC!                   (HC3) (wt%=3.2)
!NoHC! C2H2 (m=26)	    (HC3) (wt%=2.2)
!NoHC! C3H8	(m=44)	    (HC3) (wt%=5.5)
!NoHC! C4H10 (m=58)	    (HC3) (wt%=10.9)
!NoHC! Chlorinated hydocarbon (mean weight m= 150)    (HC3) (wt%=0.5)
!NoHC! Esters (Stockwell, 1997)- CnH2nO2 75%n molecules ( mean weight m=83)(69% in mass)(HC3)=> ALKA    (wt%=1.4)
!NoHC
!NoHC
!NoHC
!NoHCif(spc_name == 'HC3') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)= &
!NoHC                     44/45*0.95*0.032*ceds_g(ident)%src(:,:,1) +&
!NoHC                     44/26*0.022*ceds_g(ident)%src(:,:,1)      +&
!NoHC                     0.055*ceds_g(ident)%src(:,:,1)     	+&
!NoHC                     44/58*0.109*ceds_g(ident)%src(:,:,1)      +&
!NoHC                     44/150*0.005*ceds_g(ident)%src(:,:,1)     +&
!NoHC                     44/83*0.69*0.014*ceds_g(ident)%src(:,:,1)      
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC!----------------------------------------------------------------------------
!NoHC!! HC5 (m=72)
!NoHC!- alcohols (Stockwell, 1997) 4% in molecules (mean weight m=60)(5% in mass)  (HC5)	
!NoHC!C5H12 (m=72)  	    (HC5)   (wt%=9.4)
!NoHC!C6H14 and higher   (Stockwell, 1997) 50% in molecules 
!NoHC! 	(mean weight m=86) (43% in mass)  (HC5) (wt%=18.2)
!NoHC!Esters - CnH2nO2   (Stockwell, 1997) 25% in molecules (mean weight = 112) (31% in mass)(HC5)	
!NoHC
!NoHC
!NoHC
!NoHCif(spc_name == 'HC5') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)= &
!NoHC                     72/60*0.05*0.032*ceds_g(ident)%src(:,:,1) +& 
!NoHC                     0.094*ceds_g(ident)%src(:,:,1)      	+&
!NoHC                     72/86*0.43*0.182*ceds_g(ident)%src(:,:,1) +&
!NoHC                     72/112*0.31*0.014*ceds_g(ident)%src(:,:,1)
!NoHC
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC!----------------------------------------------------------------------------
!NoHC! HC8 (114)
!NoHC!- C6H14 + higher alkanes (stockwell, 1997) 50% in mecules (mean weight m=114) (57% in mass)(HC8)
!NoHC!-- (wt%=18.2)
!NoHC!   Ethers ( (wt%=1.7) (assumed m=114, no other data!)
!NoHC
!NoHC
!NoHC
!NoHCif(spc_name == 'HC8') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)= 0.57*0.182*ceds_g(ident)%src(:,:,1) + &
!NoHC                     0.017*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC! ETE
!NoHC! C2H4	(m=28) (ETE) 	(wt%=5.2)
!NoHCif(spc_name == 'ETE') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)=0.052*ceds_g(ident)%src(:,:,1) 
!NoHC   found_emiss_spc(iespc,antro)   = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC!----------------------------------------------------------------------------
!NoHC!- OLT 
!NoHC! C3H6 (m=42)  (propene) OLT) 	=> (OLT) (wt%=2.4)
!NoHC
!NoHCif(spc_name == 'OLT') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)= 0.024*ceds_g(ident)%src(:,:,1)    
!NoHC   found_emiss_spc(iespc,antro)   = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC!----------------------------------------------------------------------------
!NoHC!- DIEN
!NoHC!--  Other alkenes, alkynes, dienes  => DIEN (wt%=3.8)
!NoHCif(spc_name == 'DIEN') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)    = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1)= 0.038*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)   = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC!- TOL (m=92)
!NoHC! Benzene C6H6 (m=82)  (TOL)		 => (TOL) ( wt%=3)
!NoHC! Toluene (C7H8) (TOL)			=> (TOL)  (wt%=4.9)	
!NoHC
!NoHC
!NoHCif(spc_name == 'TOL') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 92/82*0.03*ceds_g(ident)%src(:,:,1)+&
!NoHC                                 0.049*ceds_g(ident)%src(:,:,1) 
!NoHC                                 
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC!- XYL (m=106)
!NoHC! Xylene (C8H10 m=106)	(XYL)		 => (XYL)  (wt%=3.6)
!NoHC! Trimethylbenzene (C9H12 m=120)  (XYL)	 => (XYL) (wt%=0.7)
!NoHC! Other aromatics m=106) (XYL)		 => (XYL)  (wt%=3.1)
!NoHC
!NoHC
!NoHCif(spc_name == 'XYL') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) =0.036*ceds_g(ident)%src(:,:,1) + &
!NoHC                                 106/120*0.007*ceds_g(ident)%src(:,:,1) + &
!NoHC                                 0.031*ceds_g(ident)%src(:,:,1)
!NoHC				  
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC! Methanal  =>	HCHO   (wt%=0.5)
!NoHCif(spc_name == 'HCHO') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.005*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC! ALD = 44  (wt%=1.6)
!NoHC! OTHER ALKANALS (assumed mainly acetaldehyde m =44)
!NoHCif(spc_name == 'ALD') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.016*ceds_g(ident)%src(:,:,1) 
!NoHC   
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC! Ketones    =>	KET   (no weighting) (wt%=1.9)
!NoHCif(spc_name == 'KET') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.019*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC
!NoHC!ORA1 (m=46)  (wt%_acids=3.6)
!NoHC!Acids      =>    ORA1 (formic acid ) : assumed 50% in molecules (44% in mass) of acids
!NoHCif(spc_name == 'ORA1') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.44*0.036*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif
!NoHC
!NoHC
!NoHC!----------------------------------------------------------------------------
!NoHC! ORA2 (m=60)  (wt%_acids=3.6)
!NoHC!Acids      =>    ORA2 (acetic acid and higher acids) : assumed 50% in
!NoHC!molecules (56% in mass)  of acids
!NoHC
!NoHCif(spc_name == 'ORA2') then
!NoHC   ident = NMVOC
!NoHC   iespc = iespc+1
!NoHC   emiss_spc_name(iespc,antro)     = spc_name
!NoHC   emiss_g(iespc)%src_antro(:,:,1) = 0.56*0.036*ceds_g(ident)%src(:,:,1)
!NoHC   found_emiss_spc(iespc,antro)    = 1
!NoHC   print*,'==> converted from edgar - found for ',spc_name
!NoHC   return
!NoHCendif


end subroutine convert_ceds_to_wrf
