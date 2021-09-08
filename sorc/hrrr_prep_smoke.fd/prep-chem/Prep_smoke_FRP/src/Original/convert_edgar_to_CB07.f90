!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_edgar_to_CB07(isp,iespc,ident,spc_name)  !kml
!use chem1_list 


use emiss_vars_emissions
use edgar_emissions, only:   edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name&
			    ,edgar_g                 &
			    ,CO                      &
			    ,NOX                     &
			    ,CO2                     &
			    ,CH4                     &
			    ,SO2                     &
			    ,N2O                     &
			    ,NMVOC                   &
			    ,SO4		       


!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml 


!--     EDGAR    |    RELACS

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
!--	All emissions will be consedered as industrial
!-----------------------------------------------------------------------------------
!--------------------------------------------------------------------
!--	VOC from TAR -> CB07 
!--     TAR	 	CB07
!--     Alcohols	0.5*MEOH +0.5*ETOH
!--     Ethane	PAR		
!--     Propane	PAR
!--     Butanes	PAR
!--     Pentanes	PAR
!--     Higher alkanes	PAR
!--     Ethene	ETH
!--     Propene	OLE + PAR
!--     Ethyne	-
!--     Other alkenes, alkynes, dienes	DIEN
!--     Benzene	TOL		
!--     Toluene	TOL			
!--     Xylene	XYL	
!--     Trimethylbenzene	XYL + PAR			
!--     Other aromatics		-	MGLY?
!--     Esters	-		
!--     Ethers	-
!--     Chlorinated HC's	-
!--     Formaldehyde	FORM	
!--     Other aldehydes	ALD2	
!--     Ketones	- KET ?
!--     Acids		-	
!--     Others	8.1	4.9	
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

!- NOx	=>NO, NO2 (?) change MP201108: NO=NOX/2 and NO2=NOX/2 in molecules 
!Considered all as NO 
if(spc_name == 'NO') then
   ident = NOx
   iespc = iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.7*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif
!!!!NO2?
if(spc_name == 'NO2') then
   ident = NOx
   iespc = iespc+1
   emiss_spc_name(iespc,antro)  	 = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.3*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!! => PAR
!- C2H6	                       => 2* PAR (paraffin carbon bond) (wt%=2.2)
!- C3H8	                       => 3* PAR (wt%=5.5)
!- C4H10	            	   => 4* PAR (wt%=10.9)
!- C5H12                       => 5* PAR (wt%=9.4)
!- C6H14_PLUS_HIGHER_ALKANES   => 6* PAR (wt%=18.2)
!- Trimethylbenzene (C9H12)    => XYL + PAR (wt%=0.7)
!- C3H6                        => OLE + PAR (wt%=4.7)
if(spc_name == 'PAR') then
   ident = NMVOC
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.933*0.022*edgar_g(ident )%src(:,:,1) +&
                                     0.955*0.055*edgar_g(ident )%src(:,:,1) +&
                                     0.965*0.109*edgar_g(ident )%src(:,:,1) +&
                                     0.972*0.094*edgar_g(ident )%src(:,:,1) +&
									 0.976*0.182*edgar_g(ident )%src(:,:,1)+&
                                     0.117*0.007*edgar_g(ident )%src(:,:,1)+&
                                     0.333*0.047*edgar_g(ident )%src(:,:,1)
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------
! => OLE
! C3H6  => OLE + PAR  (wt%=4.7)
if(spc_name == 'OLE') then
   ident = NMVOC
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.666*0.047*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
! ETH 
! C2H4	(m=28) (ETH) (wt%=5.2)
if(spc_name == 'ETH') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)    = spc_name
   emiss_g(iespc)%src_antro(:,:,1)=0.052*edgar_g(ident)%src(:,:,1) 
   found_emiss_spc(iespc,antro)   = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------
! => XYL 
!- Trimethylbenzene (C9H12) =>	   XYL  + PAR (wt%=0.7)
!- Xylene (C8H10)	    =>     XYL (wt%=3.6)
if(spc_name == 'XYL') then
   ident = NMVOC
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1)   = 0.833*0.007*edgar_g(ident)%src(:,:,1) +&
                                             0.036*edgar_g(ident)%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
! 0.5 Alcohol =>    MEOH (methanol) (wt%=3.2)
if(spc_name == 'MEOH') then
   ident = NMVOC
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1)   = 0.5*0.032*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------

!  0.5 Alcohol =>      ETOH  (Ethanol) (wt%=3.2)
if(spc_name == 'ETOH') then
   ident = ALCOHOLS
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.5*0.032*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------
!- DIEN
!--  Other alkenes, alkynes, dienes  => DIEN (wt%=3.8)
if(spc_name == 'DIEN') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)    = spc_name
   emiss_g(iespc)%src_antro(:,:,1)= 0.038*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)   = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!- TOL (m=92)
! Benzene C6H6 (m=82)  (TOL)		 => (TOL) ( wt%=3)
! Toluene (C7H8) (TOL)			=> (TOL)  (wt%=4.9)	


if(spc_name == 'TOL') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.03*edgar_g(ident)%src(:,:,1)+&
                                 0.049*edgar_g(ident)%src(:,:,1) 
                                 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------

!- XYL (m=106)
! Xylene (C8H10 m=106)	(XYL)		 	=> (XYL)  (wt%=3.6)
! Trimethylbenzene (C9H12 m=120)  (XYL)	=> (XYL) (wt%=0.7)

if(spc_name == 'XYL') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) =0.036*edgar_g(ident)%src(:,:,1) + &
                                 0.833*0.007*edgar_g(ident)%src(:,:,1)
				  
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif



!----------------------------------------------------------------------------

! Methanal  =>	HCHO   (wt%=0.5)
if(spc_name == 'HCHO') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.005*edgar_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

! ALD 2(assumed mainly acetaldehyde m =44)
if(spc_name == 'ALD2') then
   ident = NMVOC
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.016*edgar_g(ident)%src(:,:,1) 
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from edgar - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

! Ketones    =>	KET   (no weighting) (wt%=1.9)
!if(spc_name == 'KET') then
!   ident = NMVOC
!   iespc = iespc+1
!   emiss_spc_name(iespc,antro)     = spc_name
!   emiss_g(iespc)%src_antro(:,:,1) = 0.019*edgar_g(ident)%src(:,:,1)
!   found_emiss_spc(iespc,antro)    = 1
!   print*,'==> converted from edgar - found for ',spc_name
!   return
!endif


end subroutine convert_edgar_to_CB07
