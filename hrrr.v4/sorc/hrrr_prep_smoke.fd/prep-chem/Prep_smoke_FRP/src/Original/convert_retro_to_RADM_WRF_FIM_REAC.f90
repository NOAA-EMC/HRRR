!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_retro_to_wrf_reac(isp,iespc,ident,spc_name)  !kml
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


!--     Retro    |    RACM
!--     Emissions for RACM weighted by Agregation factor Agg (only organics)
!--     Agregation factors for RACM can be found in Stockwell et al 1997, 
!-- 	J. Geos Res. 102, 25847-25879


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
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.7*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif
if(spc_name == 'NO2') then
   ident = NOx
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.3*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!- C2H6 =>	ETH   (Agg=1)
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

! HC3 (m=44)
!- C2H2 (m=26)	   =>  HC3 (Agg=0.41) (Acetylene)
!- C3H8	(m=44)     =>  HC3 (Agg=0.57) (Propane)
!- C4H10 (m=58)	   =>  HC3 (Agg=1.11) (Butane)
!- Chlorinated hydocarbon (mean weight m= 150)     =>  HC3 (Agg=1)*
!- Esters (Stockwell, 1997)- CnH2nO2 75%n molecules (mean weight m=83)(69% in mass)  =>  HC3 (Agg=1)*
!--* No data!! 
!- alcohols (Stockwell, 1997) 96% in molecules (mean weight m=45) (95% in mass)  =>  HC3 (Agg=1.37)
!--(assumed ethanol)


if(spc_name == 'HC3') then
   ident = C2H2
   iespc=iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.41*retro_g(C2H2 )%src(:,:,1) +&
                                     0.57*retro_g(C3H8 )%src(:,:,1) +&
                                     1.11*retro_g(C4H10)%src(:,:,1) +&
                       retro_g(CHLORINATED_HYDROCARBONS)%src(:,:,1) +&
               	                  0.69*retro_g(ESTERS)  %src(:,:,1) +&
                             0.95*1.37*retro_g(ALCOHOLS)%src(:,:,1) 
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

!HC5 = 72
!C5H12 (m=72)            =>	HC5 (Agg=0.97)
!C6H14 and higher (Stockwell, 1997)50% in molecules (mean weight m=86)(43% in mass)=>	HC5  (Agg=0.97)
!Esters - CnH2nO2 (Stockwell, 1997) 25% in molecules (mean weight = 112) (31% in mass)=>HC5 (Agg=1)*
!- alcohols (Stockwell, 1997) 4% in molecules (mean weight m=60)(5% in mass)  => HC5  (Agg=1.07)
if(spc_name == 'HC5') then
   ident = C5H12
   iespc=iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.97*retro_g(C5H12 )%src(:,:,1) + &
            0.97*0.43*retro_g(C6H14_PLUS_HIGHER_ALKANES )%src(:,:,1) + &
                                     0.31*retro_g(ESTERS)%src(:,:,1) + &
                              1.07*0.05*retro_g(ALCOHOLS)%src(:,:,1) 
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

! HC8 = 114
!- C6H14 + higher alkanes (stockwell, 1997) 50% in molecules (mean weight m=114)(57% in mass) =>HC8  
!--Assumed (Agg=1)
if(spc_name == 'HC8') then
   ident = C6H14_PLUS_HIGHER_ALKANES
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.57*retro_g(ident)%src(:,:,1) 
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

!- C3H6	=> OLT   (Agg=1)
if(spc_name == 'OLT') then
   ident = C3H6
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif



!----------------------------------------------------------------------------

!- C2H4	=> ETE   (Agg=1)
if(spc_name == 'ETE') then
   ident = C2H4
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif




!----------------------------------------------------------------------------

! XYL m=106   (Agg=1)
!- Trimethylbenzene (C9H12 m=120) =>	   XYL
!- Xylene (C8H10 m=106)	    =>     XYL
!- Other aromatics m=106)	    =>     XYL
if(spc_name == 'XYL') then
   ident = Xylene
   iespc=iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(Trimethylbenzenes)%src(:,:,1) +&
                                     retro_g(Xylene           )%src(:,:,1) +&
                                     retro_g(OTHER_AROMATICS  )%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!----------------------------------------------------------------------------

! TOL m=92 
!Toluene (C7H8)     =>	TOL   (Agg=1)
!Benzene C6H6 (m=82)       =>  TOL   (Agg=0.29)
if(spc_name == 'TOL') then
   ident = Toluene
   iespc = iespc+1
   emiss_spc_name(iespc,antro)     = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(Toluene)%src(:,:,1) +&
                                0.29*retro_g(Benzene)%src(:,:,1) 
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!----------------------------------------------------------------------------

! Methanal  =>	HCHO  (Agg=1)
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

! ALD = 44  (Agg=1)
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

! Ketones    =>	KET (no weighting)
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

!ORA1 (m=46)  (Agg=1)
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
! ORA2 (m=60) (Agg=1)
!Acids      =>    ORA2 (acetic acid and higher acids) : assumed 50% in
!molecules (56% in mass)  of acids

if(spc_name == 'ORA2') then
   ident = ACIDS
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1)   = 0.56*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)      = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

end subroutine convert_retro_to_wrf_reac
