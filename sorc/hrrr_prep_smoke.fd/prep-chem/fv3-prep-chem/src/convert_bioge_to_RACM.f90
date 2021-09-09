!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_bioge_to_racm(isp,iespc,ident,spc_name)
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


!-- bioge    |    racm 

!---------------------------
!Jean-Michel message:
!I have calculated the fraction of terpenes to be sent to RACM in API and
!LIM. For that, I used the data from the BAI site
!(http://bai.acd.ucar.edu/Data/BVOC/ ).
!
!To summarize, I only used data concerning monoterpenes since it looks
!like the others terpenes are not yet well known, and computed the
!quantity of monoterpenes with one double bond and the quantity of
!monoterpenes with two double bonds. Then I used the total quantity of
!monoterpenes to calculate the ratio of terpenes to be sent to both
!variables.
!The results are :!
!        .  API = 0.5*terpenes
!        .  LIM = 0.2*terpenes
!
!API        =>	0.5*terpenes
if(spc_name == 'API') then
   ident = API
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.5*bioge_g(TERPENES)%src(:,:,1) 
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!LIM        =>	0.2*terpenes
if(spc_name == 'LIM') then
   ident = LIM
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.2*bioge_g(TERPENES)%src(:,:,1) 
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif
!---------------------------
!HC3 (m=44)
!- CH3OH(m=32)                       =>  HC3 ! MP correction
!- C3H8	                       =>  HC3
if(spc_name == 'HC3') then
   ident = HC3
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 44/32*bioge_g(CH3OH )%src(:,:,1) + &
                                     bioge_g(C3H8  )%src(:,:,1)                                         
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C2H6 =>	ETH
if(spc_name == 'ETH') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C2H4	=> ETE
if(spc_name == 'ETE') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!- C3H6	=> OLT
if(spc_name == 'OLT') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif
!     change MP 201108   KET = 0 (low reaction of acetone with OH)
!- ACETONE	=> KET
!if(spc_name == 'KET') then
!   ident = ACETONE
!   iespc=iespc+1
!   emiss_spc_name(iespc,bioge)     = spc_name
!   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
!   found_emiss_spc(iespc,bioge)    = 1
!   print*,'==> converted from bioge - found for ',spc_name
!   return
!endif

!- ISOPRENE	=> ISO
if(spc_name == 'ISO') then
   ident = ISOPRENE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

end subroutine convert_bioge_to_racm
