!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

! ####################################################################
! # Acetone and other ketones aren't converted                       #
! # Terpenes aren't converted                                        #
! # (in previous version (bioge) terpenes are converted to isoprene) #
! ####################################################################

subroutine convert_megan_to_CB07(isp,iespc,ident,spc_name)
!use chem1_list
use emiss_vars_emissions
use megan_emissions, only:   megan_nspecies=>nspecies&
                            ,megan_spc_name=>spc_name&
			    ,megan_g     		&
                	    ,CO  	 	 	&!x
			    ,CH4      	 		&!x
			    ,C2H6	 	 	&!x=ethane
			    ,C2H4	 	 	&!x=ethene
			    ,C3H6		   	&!x=propene
			    ,C3H8		   	&!x=propane
			    ,CH2O   	 		&!x=formaldehyde
			    ,CH3CHO   	 		&!x=acetaldehyde
			    ,CH3OH   	        &!x=methanal
			    ,ACETONE 	        &!x
			    ,OTHKETONES         &!x
			    ,TOLUENE            &
			    ,ISOPRENE	        &!x
			    ,MONOTERPENES	    &!x
			    ,SESQUITERPENES     !x

!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml 

!-- megan    |    CB2002 

!- NO     => NO
!if(spc_name == 'NO') then
!   ident = NO
!   iespc=iespc+1
!   emiss_spc_name(iespc,megan)          = spc_name
!   emiss_g(iespc)%src_megan(:,:,1) = megan_g(ident )%src(:,:,1)
! 
!   found_emiss_spc(iespc,megan)   = 1
!   print*,'==> converted from megan - found for ',spc_name
!   return
!endif
   
!- CH3OH  => MEOH 
if(spc_name == 'MEOH') then
   ident = CH3OH
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident )%src(:,:,1)
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!- C3H8   =>  PAR
!- C2H6   =>  PAR
!- C3H6   => OLE + PAR
if(spc_name == 'PAR') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) =0.955*megan_g(C3H8)%src(:,:,1) + &
                                    0.933*megan_g(C2H6)%src(:,:,1) + &
                                    0.333* megan_g(C3H6)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!- C2H4	=> ETH
if(spc_name == 'ETH') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!- C3H6	=> OLE + PAR
if(spc_name == 'OLE') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.666*megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

! - ISOPRENE  => ISOP
if(spc_name == 'ISOP') then
   ident = ISOPRENE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

! -Formaldehyde	=> FORM
if(spc_name == 'FORM') then
   ident = CH2O
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!Acetald	        =>   ALD2
if(spc_name == 'ALD2') then
   ident = CH3CHO
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif



!toluene	  =>      TOL
if(spc_name == 'TOL') then
   ident = TOLUENE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif


end subroutine convert_megan_to_CB07
