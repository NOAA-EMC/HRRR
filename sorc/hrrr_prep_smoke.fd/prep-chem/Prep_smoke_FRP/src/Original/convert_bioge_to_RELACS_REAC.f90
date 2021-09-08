!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_bioge_to_relacs_reac(isp,iespc,ident,spc_name)
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


!-- bioge    |   RELACS

!--     Emissions for RACM weighted by Agregation factor Agg (only organics)
!--     Agregation factors for RELACS can be found in Crassier et al 2000, 
!-- 	Atmos.Environ. 34 2633-2644
!--     Agregation factors for RACM can be found in Stockwell et al 1997, 
!-- 	J. Geos Res. 102, 25847-25879

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
!BIO (m=68) 
!API (m=136) 0.5*terpenes	=>	BIO  (Agg_RACM=1) (Agg=1)
!LIM (m=136) 0.2*terpenes	=>	BIO  (Agg_RACM=1) (Agg=1)
!ISOPRENE (m=68)		=> 	BIO  (Agg_RACM=1) (Agg=1)

if(spc_name == 'BIO') then
   ident = BIO
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.7*bioge_g(TERPENES)%src(:,:,1) + &
                                     bioge_g(ISOPRENE)%src(:,:,1)
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!---------------------------
!ALKA (m=62)
!- CH3OH(m=32)  (HC3 ! MP correction)	=> ALKA  (Agg_RACM=0.49) (HC3 )
!- C3H8	  (m=44)  (HC3) 		=> ALKA   (Agg_RACM=0.57) (HC3 )
!--HC3 Agg=0.77



if(spc_name == 'ALKA') then
   ident = ALKA
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.49*0.77*bioge_g(CH3OH)%src(:,:,1) + &
                                     0.57*0.77*bioge_g(C3H8)%src(:,:,1)                                         
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!---------------------------
!- C2H6 =>	ETH (Agg_RACM=1) (Agg=1)
if(spc_name == 'ETH') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!---------------------------
!ALKE (m=33)
!- C2H4	(ETE) (m=28)	=> ALKE (Agg_RACM=1) (ETE, Agg=0.96)
!- C3H6 (OLT) (m=42)	=> ALKE (Agg_RACM=1) (ETE, Agg=1.04)
if(spc_name == 'ALKE') then
   ident = ALKE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.96*bioge_g(C2H4)%src(:,:,1) +&
                                     1.04*bioge_g(C3H6)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif

!---------------------------
! KET
!     change MP 201108   KET = 0 (low reaction of acetone with OH)
! in spite of the precious comment, i will convert it, 
! since in this approach reactivity is taken into account (M S Gácita)
!- ACETONE	=> KET  (Agg_RACM=0.33) (Agg=1)
if(spc_name == 'KET') then
   ident = ACETONE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.33*bioge_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from bioge - found for ',spc_name
   return
endif


end subroutine convert_bioge_to_relacs_reac
