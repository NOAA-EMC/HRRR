!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_retro_to_relacs_reac(isp,iespc,ident,spc_name)  !kml
!use chem1_list 


use emiss_vars_emissions
use retro_emissions, only:   retro_nspecies=>nspecies&
                            ,retro_spc_name=>spc_name&
			    ,retro_g                     &
                            ,ACIDS		         &   
                            ,ALCOHOLS		      	 &
			    ,BENZENE		      	 &
			    ,C2H2		      	 &
			    ,C2H4		      	 &
			    ,C2H6		      	 &
			    ,C3H6		      	 &
			    ,C3H8		      	 &
			    ,C4H10		      	 &
			    ,C5H12		      	 &
			    ,C6H14_PLUS_HIGHER_ALKANES	 &
			    ,CHLORINATED_HYDROCARBONS 	 &
			    ,CO 		      	 &
			    ,ESTERS		      	 &
			    ,ETHERS		      	 &
			    ,KETONES		      	 &
			    ,METHANAL		      	 &
			    ,NOX		      	 &
			    ,OTHER_ALKANALS	      	 &
			    ,OTHER_AROMATICS	      	 &
			    ,OTHER_VOC  	      	 &
			    ,TOLUENE		      	 &
			    ,TRIMETHYLBENZENES        	 &
			    ,XYLENE		      	 


!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml 


!--     Retro    |    RELACS
!--     Emissions for RACM weighted by Agregation factor Agg (only organics)
!--     Agregation factors for RACM can be found in Stockwell et al 1997, 
!-- 	J. Geos Res. 102, 25847-25879
!--     Agregation factors for RELACS can be found in Crassier et al 2000, 
!-- 	Atmos.Environ. 34 2633-2644



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
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.7*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif
if(spc_name == 'NO2') then
   ident = NOx
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.3*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!- C2H6 =>	ETH  (Agg_RACM=1)  (Agg=1)
if(spc_name == 'ETH') then
   ident = C2H6
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!- ALKA (m=62)
! C2H2 (m=26)	    (HC3) (Agg_RACM=0.41) => ALKA 
! C3H8	(m=44)	    (HC3) (Agg_RACM=0.57)      => ALKA 
! C4H10 (m=58)	    (HC3)  (Agg_RACM=1.11) 	=> ALKA   
! Chlorinated hydocarbon (mean weight m= 150) 	 (Agg_RACM=1)*   (HC3)	=> ALKA 
! Esters (Stockwell, 1997)- CnH2nO2 75%n molecules ( mean weight m=83)(69% in mass)(Agg_RACM=1)*(HC3)=> ALKA 
! alcohols (Stockwell, 1997) 96% in molecules (mean weight m=45) (95% in mass)  (Agg_RACM=1.37)  (HC3)=> ALKA
!C5H12 (m=72)  	    (HC5) (Agg_RACM=0.97)	=> ALKA
!C6H14 and higher  (Agg_RACM=0.97) (Stockwell, 1997)50% in molecules (mean weight m=86)(43% in mass)  (HC5)	=> ALKA
!Esters - CnH2nO2  (Agg_RACM=1)* (Stockwell, 1997) 25% in molecules (mean weight = 112) (31% in mass)(HC5)	=> ALKA
!- alcohols (Stockwell, 1997) 4% in molecules (mean weight m=60)(5% in mass)  	    (Agg_RACM=1.07)  (HC5)	=> ALKA
!- C6H14 + higher alkanes (stockwell, 1997) 50% in molecules (mean weight m=114)(57% in mass)(HC8)=>ALKA
!-- (Agg_RACM=0.97)
!-- AggRELACS:
!--HC3 0.77
!--HC5 1.23
!--HC8 1.58


if(spc_name == 'ALKA') then
   ident = ALKA
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.77*0.41*retro_g(C2H2 )%src(:,:,1) +&
                                     0.77*0.57*retro_g(C3H8 )%src(:,:,1) +&
                                     0.77*1.11*retro_g(C4H10)%src(:,:,1) +&
                                     0.77*retro_g(CHLORINATED_HYDROCARBONS)%src(:,:,1) +&
                                     0.69*0.77*retro_g(ESTERS)  %src(:,:,1) +&
                                     0.95*0.77*1.37*retro_g(ALCOHOLS)%src(:,:,1) +&
                                     1.23*0.97*retro_g(C5H12 )%src(:,:,1) +                    &
                                     1.23*0.97*0.43*retro_g(C6H14_PLUS_HIGHER_ALKANES )%src(:,:,1) +&
                                     1.23*0.31*retro_g(ESTERS)%src(:,:,1) +&
                                     1.23*1.07*0.05*retro_g(ALCOHOLS)%src(:,:,1) +& 
                                     1.58*0.97*0.57*retro_g(C6H14_PLUS_HIGHER_ALKANES)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

!- ALKE (m=33)
! C3H6 (m=42) (OLT) (Agg_RACM=1)	=> ALKE (OLT, Agg=1.04)
! C2H4	(m=28) (ETE) (Agg_RACM=1)	=> ALKE (ETE, Agg=0.96)
if(spc_name == 'ALKE') then
   ident = ALKE
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 1.04*retro_g(C3H6)%src(:,:,1) + &
                                     0.96*retro_g(C2H4)%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

!- ARO (m=98)
! Trimethylbenzene (C9H12 m=120)  (XYL)	 (Agg_RACM=1)	=> ARO (XYL, Agg=1.04)
! Xylene (C8H10 m=106)	(XYL)		 (Agg_RACM=1)	=> ARO (XYL, Agg=1.04)
! Other aromatics m=106) (XYL)		 (Agg_RACM=1)	=> ARO (XYL, Agg=1.04)  
! Toluene (C7H8) (TOL)			 (Agg_RACM=1)	=> ARO (TOL, Agg=0.87)	
! Benzene C6H6 (m=82)  (TOL)		 (Agg_RACM=0.29)	=> ARO	 (TOL, Agg=0.87)
if(spc_name == 'ARO') then
   ident = ARO
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 1.04*retro_g(Trimethylbenzenes)%src(:,:,1) +&
                                       1.04*retro_g(Xylene           )%src(:,:,1) +&
                                       1.04*retro_g(OTHER_AROMATICS  )%src(:,:,1) +&
                                       0.87*retro_g(Toluene          )%src(:,:,1) +&
                                       0.29*0.87*retro_g(Benzene     )%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

! Methanal  =>	HCHO  (Agg_RACM=1) (Agg=1)
if(spc_name == 'HCHO') then
   ident = Methanal
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

! ALD = 44  (Agg_RACM=1) (Agg=1)
! OTHER ALKANALS (assumed mainly acetaldehyde m =44)
if(spc_name == 'ALD') then
   ident = OTHER_ALKANALS
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

! Ketones    =>	KET  (Agg_RACM=1)  (Agg=1) (no weighting)
if(spc_name == 'KET') then
   ident = Ketones
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

!ORA1 (m=46)  (Agg_RACM=1)  (Agg=1)
!Acids      =>    ORA1 (formic acid ) : assumed 50% in molecules (44% in mass) of acids
if(spc_name == 'ORA1') then
   ident = ACIDS
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.44*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif
!----------------------------------------------------------------------------
! ORA2 (m=60)  (Agg_RACM=1) (Agg=1)
!Acids      =>    ORA2 (acetic acid and higher acids) : assumed 50% in
!molecules (56% in mass)  of acids

if(spc_name == 'ORA2') then
   ident = ACIDS
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.56*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


end subroutine convert_retro_to_relacs_reac
