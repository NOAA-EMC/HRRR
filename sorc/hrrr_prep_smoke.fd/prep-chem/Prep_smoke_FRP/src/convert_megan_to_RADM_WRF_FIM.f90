!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_megan_to_wrf(isp,iespc,ident,spc_name)
!use chem1_list
use emiss_vars_emissions
use megan_emissions, only:   megan_nspecies=>nspecies&
                            ,megan_spc_name=>spc_name&
			    ,megan_g     		&
                	    ,CO  	 	 		&!x
			    ,CH4      	 		&!x
			    ,C2H6	 	 		&!x=ethane
			    ,C2H4	 	 		&!x=ethene
			    ,C3H6		   		&!x=propene
			    ,C3H8		   		&!x=propane
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


!-- megan    |   RELACS

!Molar mass weighting!!

!---------------------------
!ISO (m=68) 
!ISOPRENE (m=68)

if(spc_name == 'ISO') then
   ident = ISOPRENE
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ISOPRENE)%src(:,:,1)
   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

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
!Note (Madeleine Sánchez)
!Following the message above, I will use just monoterpenes 
! disconsidering the sesquiterpenes
! this might be re-evaluated!!
!
!API (m=68) 
!API (m=136) 0.5*terpenes	=>	BIO  (Agg_RACM=1) (Agg=1)

if(spc_name == 'API') then
   ident = MONOTERPENES
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.5*68/136*megan_g(MONOTERPENES)%src(:,:,1)    
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!
!LIM(m=68) 
!LIM (m=136) 0.2*terpenes	=>	BIO  (Agg_RACM=1) (Agg=1)


if(spc_name == 'LIM') then
   ident = MONOTERPENES
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 0.2*68/136*megan_g(MONOTERPENES)%src(:,:,1)   
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!---------------------------
! HC3 (m=44)
!- CH3OH(m=32)  (HC3 ! MP correction)	=> HC3 (Agg_RACM=0.49) (HC3 )
!- Propane (C3H8)	  (m=44)  (HC3) 		=> HC3   (Agg_RACM=0.57) (HC3 )


if(spc_name == 'HC3') then
   ident = CH3OH
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)           = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = 44/32*megan_g(CH3OH)%src(:,:,1) + &
                                     megan_g(C3H8)%src(:,:,1)                                         
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!---------------------------
!- C2H6 =>	ETH (Agg_RACM=1) 
if(spc_name == 'ETH') then
   ident = C2H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(ident)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif

!---------------------------
! ETE
!- C2H4	(ETE) (m=28)	=> ETE 
if(spc_name == 'ETE') then
   ident = C2H4
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(C2H4)%src(:,:,1) 
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif
!---------------------------
! OLT(m=33)
!- C3H6 (OLT) (m=42)	=> OLT
if(spc_name == 'OLT') then
   ident = C3H6
   iespc=iespc+1
   emiss_spc_name(iespc,bioge)     = spc_name
   emiss_g(iespc)%src_bioge(:,:,1) = megan_g(C3H6)%src(:,:,1)
   found_emiss_spc(iespc,bioge)    = 1
   print*,'==> converted from megan - found for ',spc_name
   return
endif
!---------------------------

! HCHO
! CH2O   	 	=> ALD  (Agg_RACM=1) (Agg=1)		

if(spc_name == 'HCHO') then
        ident = CH2O
	iespc=iespc+1
	emiss_spc_name(iespc,bioge)     = spc_name
	emiss_g(iespc)%src_bioge(:,:,1) = megan_g(CH2O)%src(:,:,1) 
	found_emiss_spc(iespc,bioge)    = 1
    print*,'==> converted from megan - found for ',spc_name
    return
endif
!---------------------------
! KET
!     change MP 201108   KET = 0 (low reaction of acetone with OH)
! (just Acetone)
!- ACETONE	=> KET (disconsidered)
!- OTHKETONES	=> KET
if(spc_name == 'KET') then
        ident = OTHKETONES
	iespc=iespc+1
	emiss_spc_name(iespc,bioge)     = spc_name
	emiss_g(iespc)%src_bioge(:,:,1) = megan_g(OTHKETONES)%src(:,:,1)
	found_emiss_spc(iespc,bioge)    = 1
    print*,'==> converted from megan - found for ',spc_name
    return
endif

!---------------------------
! ALD
! CH3CHO   	 	=> ALD  

if(spc_name == 'ALD') then
        ident = CH3CHO
	iespc=iespc+1
	emiss_spc_name(iespc,bioge)     = spc_name
	emiss_g(iespc)%src_bioge(:,:,1) = megan_g(CH3CHO)%src(:,:,1) 
	found_emiss_spc(iespc,bioge)    = 1
    print*,'==> converted from megan - found for ',spc_name
    return
endif

!---------------------------
! TOL
! TOLUENE   	 	=> TOL

if(spc_name == 'TOL') then
        ident = TOLUENE
	iespc=iespc+1
	emiss_spc_name(iespc,bioge)     = spc_name
	emiss_g(iespc)%src_bioge(:,:,1) = megan_g(TOLUENE)%src(:,:,1) 
	found_emiss_spc(iespc,bioge)    = 1
    print*,'==> converted from megan - found for ',spc_name
    return
endif

end subroutine convert_megan_to_wrf
