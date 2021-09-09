!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_retro_to_CB07(isp,iespc,ident,spc_name)
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
			    ,OTHER_ALKANALS      	 &
			    ,OTHER_AROMATICS	      	 &
			    ,OTHER_VOC  	      	 &
			    ,TOLUENE		      	 &
			    ,TRIMETHYLBENZENES        	 &
			    ,XYLENE		      	 


!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml
!-- retro    |    CB2002


!print*,'dentro conv=',isp,spc_name


!ALCOHOLS / 2     =>    MEOH (methanol) 
if(spc_name == 'MEOH') then
   ident = ALCOHOLS
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1)   = 0.5*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!ALCOHOLS / 2     =>      ETOH  (Ethanol) 
if(spc_name == 'ETOH') then
   ident = ALCOHOLS
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.5*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

! C2H4  => ETH (ethene)
if(spc_name == 'ETH') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

! C3H6  => OLE + PAR
if(spc_name == 'OLE') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.666*retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif
! NOx => NO and NO2
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

!- METHANAL  => FORM (formaldehyde)
if(spc_name == 'FORM') then
   ident = METHANAL
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!- OTHER_AROMATICS => MGLY (methylglyoxal and other aromatics)
!if(spc_name == 'MGLY') then
!   ident = OTHER_AROMATICS
!   iespc=iespc+1
!   emiss_spc_name(iespc,antro)           = spc_name
!   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
!   found_emiss_spc(iespc,antro)    = 1
!   print*,'==> converted from retro - found for ',spc_name
!   return
!endif


!- Trimethylbenzene (C9H12) =>	   XYL  + PAR
!- Xylene (C8H10)	    =>     XYL
if(spc_name == 'XYL') then
   ident = XYLENE
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1)   = 0.833*retro_g(Trimethylbenzenes)%src(:,:,1) +&
                                             retro_g(Xylene          )%src(:,:,1) 
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif


!Toluene (C7H8)     =>	TOL (Toluene and other monoalkyl aromatics)
if(spc_name == 'TOL') then
   ident = TOLUENE
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = retro_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

!- C2H6	                       => 2* PAR (paraffin carbon bond)
!- C3H8	                       => 3* PAR
!- C4H10	               => 4* PAR
!- C5H12                       => 5* PAR
!- C6H14_PLUS_HIGHER_ALKANES   => 6* PAR
!- Trimethylbenzene (C9H12)    => XYL + PAR
!- C3H6                        => OLE + PAR
if(spc_name == 'PAR') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,antro)           = spc_name
   emiss_g(iespc)%src_antro(:,:,1) = 0.933*retro_g(C2H6 )%src(:,:,1) +&
                                     0.955*retro_g(C3H8 )%src(:,:,1) +&
                                     0.965*retro_g(C4H10)%src(:,:,1) +&
                                     0.972*retro_g(C5H12)%src(:,:,1) +&
				     0.976*retro_g(C6H14_PLUS_HIGHER_ALKANES)%src(:,:,1)+&
                                     0.117* retro_g(Trimethylbenzenes)%src(:,:,1)+&
                                     0.333*retro_g(C3H6)%src(:,:,1)
   
   found_emiss_spc(iespc,antro)    = 1
   print*,'==> converted from retro - found for ',spc_name
   return
endif

end subroutine convert_retro_to_CB07
