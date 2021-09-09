!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_bioge_to_CB07(isp,iespc,ident,spc_name)
!use chem1_list
use emiss_vars_emissions
use bioge_emissions, only:   bioge_nspecies=>nspecies&
                            ,bioge_spc_name=>spc_name&
			    ,bioge_g     &
                            ,NO      	 &!x
			    ,CO      	 &!x
			    ,CH3OH   	 &!x
			    ,C3H8    	 &!x
			    ,C2H6    	 &!x
			    ,C2H4    	 &!x
			    ,ACETONE 	 &!x
			    ,NVOC        &
			    ,TERPENES	 &!x
			    ,ISOPRENE	 &!x
			    ,C3H6         !x == propene

!implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(in)  :: spc_name  !kml 

!-- bioge    |    CB2002 

!- NO     => NO
!if(spc_name == 'NO') then
!   ident = NO
!   iespc=iespc+1
!   emiss_spc_name(iespc,bioge)          = spc_name
!   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident )%src(:,:,1)
! 
!   found_emiss_spc(iespc,bioge)   = 1
!   print*,'==> converted from bioge - found for ',spc_name
!   return
!endif
   
!- CH3OH  => MEOH 
if(spc_name == 'MEOH') then
   ident = CH3OH
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident )%src(:,:,1)
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C3H8   =>  PAR
!- C2H6   =>  PAR
!- C3H6   => OLE + PAR
if(spc_name == 'PAR') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) =0.955*bioge_g(C3H8)%src(:,:,1) + &
                                    0.933*bioge_g(C2H6)%src(:,:,1) + &
                                    0.333* bioge_g(C3H6)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C2H4	=> ETH
if(spc_name == 'ETH') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C3H6	=> OLE + PAR
if(spc_name == 'OLE') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.666*bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

! - ISOPRENE  => ISOP
if(spc_name == 'ISOP') then
   ident = ISOPRENE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

end subroutine convert_bioge_to_CB07
