!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

subroutine convert_edgar_to_aer(isp,iespc,ident,spc_name,n1,n2)
use aer1_list    , only : aerosol_mechanism
use grid_dims_out, only : use_bioge
use edgar_emissions, only:   edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name&
			    ,edgar_g                 &           
    ,  SO2      &
    ,  SO4      & 
    ,  URBAN2	& ! PM2.5
    ,  URBAN3   & ! PM10
    ,  BC	&
    ,  OC        

use megan_emissions, only:   megan_nspecies=>nspecies&
                            ,megan_spc_name=>spc_name&
			    ,megan_g     		&
                	    ,MONOTERPENES	&!x
			    ,SESQUITERPENES     !x
use bioge_emissions, only:   bioge_nspecies=>nspecies&
                            ,bioge_spc_name=>spc_name&
			    ,bioge_g     &
                            ,TERPENES	 
use emiss_vars_emissions

implicit none  ! < necessario para outros esquemas 
integer, intent(in) :: isp,n1,n2
integer, intent(inout) :: iespc,ident
character(len=*), intent(in)  :: spc_name  !kml 

real :: dummy(n1,n2)

!-- edgar/bioge    |    aer 


!print*,'dentro aer edgar=',isp,iespc,spc_name,edgar_nspecies

IF(aerosol_mechanism == 'MATRIX') then


  ! bc1_bcar      =>    BC 
  if(spc_name == 'bc1_bcar') then
     ident = BC
     iespc=iespc+1
     emiss_spc_name(iespc,antro)     = spc_name
     emiss_g(iespc)%src_antro(:,:,1) = edgar_g(BC)%src(:,:,1)!*
     found_emiss_spc(iespc,antro)    = 1
     print*,'==> converted from edgar - found for ',spc_name
     return
  endif

  ! occ_ocar     =>    OC (HTAP) + 10% terpeno do MEGAN
  if(spc_name == 'occ_ocar') then
     dummy=0.0
     if    (bioge == 1) then ! GEIA
       dummy(1:n1,1:n2)=0.1*bioge_g(TERPENES)%src(1:n1,1:n2,1)
     
     elseif(bioge == 2) then ! MEGAN
        dummy(1:n1,1:n2)=0.1*(megan_g(MONOTERPENES  )%src(1:n1,1:n2,1)+ &
                              megan_g(SESQUITERPENES)%src(1:n1,1:n2,1) )
     endif    
     ident = OC
     iespc=iespc+1
     emiss_spc_name(iespc,antro)     = spc_name
     emiss_g(iespc)%src_antro(:,:,1) = edgar_g(OC)%src(:,:,1) +dummy(:,:)
     found_emiss_spc(iespc,antro)    = 1
     print*,'==> converted from edgar - found for ',spc_name
     return
  endif

  ! akk_sulf      =>    1% 2.5% SO2
  if(spc_name == 'akk_sulf') then
    ident = SO2
    iespc=iespc+1
    emiss_spc_name(iespc,antro)           = spc_name
    emiss_g(iespc)%src_antro(:,:,1) = 0.01 * 0.025*edgar_g(SO2)%src(:,:,1) * &
                                      96.07/62.66  ! = PM_SO4/PM_SO2
    found_emiss_spc(iespc,antro)    = 1
    print*,'==> converted from edgar - found for ',spc_name
    return
  endif

  ! acc_sulf      =>    99% 2.5% SO2
  if(spc_name == 'acc_sulf') then
    ident = SO2
    iespc=iespc+1
    emiss_spc_name(iespc,antro)           = spc_name
    emiss_g(iespc)%src_antro(:,:,1)   = 0.99 * 0.025*edgar_g(SO2)%src(:,:,1) *&
                                      96.07/62.66  ! = PM_SO4/PM_SO2
    
    found_emiss_spc(iespc,antro)    = 1
    print*,'==> converted from edgar - found for ',spc_name
    return
  endif

! SO4      =>   
!if(spc_name == 'SO42') then
!   !ident = SO42
!   iespc=iespc+1
!   emiss_spc_name(iespc,antro)           = spc_name
!   emiss_g(iespc)%src_antro(:,:,1)   = 0.5*edgar_g(SO4)%src(:,:,1)!*
!   found_emiss_spc(iespc,antro)    = 1
!   print*,'==> converted from edgar - found for ',spc_name
!   return
!endif
!* ref: Stier et al., The aerosol-climate model ECHAM5-HAM. Atmos.
!  Chem. Phys., 5,1125-1156,2005.
ENDIF

end subroutine convert_edgar_to_aer
