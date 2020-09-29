!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module emiss_vars_emissions
use grid_dims_out, only: use_bbem_plumerise
!---------------------------------------------------------------------------
  character(LEN=20), allocatable, dimension(:,:) :: emiss_spc_name
  integer, parameter :: number_sources = 4  !- number of diff types of sources 
  integer emiss_nspecies
  
  character(LEN=20),dimension(number_sources),parameter :: src_name= &
! '12345678901234567890'
(/                      &
  'antro               '&    
, 'bburn               '&
, 'bioge               '&
, 'geoge               '/)

  integer, allocatable, dimension(:,:) :: found_emiss_spc
  integer     :: &
   antro   = 01, & ! anthropogenic sources
   bburn   = 02, & ! biomass burning sources 
   bioge   = 03, & ! biogenic sources ! must be equal to number_sources
   geoge   = 04    ! biogenic sources ! must be equal to number_sources

  type emiss_vars   
     real, pointer, dimension(:,:,:)  :: src_antro ! antropogenic (indust-urban-etc) sources
     real, pointer, dimension(:,:,:)  :: src_bburn ! biomass burning sources
     real, pointer, dimension(:,:,:)  :: src_bioge ! biogenic sources
     real, pointer, dimension(:,:,:)  :: src_geoge ! biogenic sources

  end type emiss_vars

  type (emiss_vars), allocatable :: emiss_g(:)

!- for plumerise mechanism
  character(LEN=20), allocatable, dimension(:,:) :: emiss_plume_name
  integer, allocatable, dimension(:,:) :: found_emiss_plume
  
  type emiss_plume_vars   
     real, pointer, dimension(:,:)  :: src_plume

  end type emiss_plume_vars

  type (emiss_plume_vars), allocatable :: emiss_plume_g(:,:)


contains
  !---------------------------------------------------------------

  subroutine alloc_emiss_vars(emiss,emiss_plume,n1,n2,n3,nspecies,nveg_agreg)

    implicit none

    type (emiss_vars)      ,dimension(nspecies)  :: emiss
    type (emiss_plume_vars),dimension(nspecies+1,nveg_agreg)  :: emiss_plume
    integer,intent(in) :: n1,n2,n3
    integer iv,ispc,nspecies,nveg_agreg
    
    do ispc=1,nspecies
     allocate (emiss(ispc)%src_antro(n1,n2,n3));emiss(ispc)%src_antro(:,:,:) = 0.
     allocate (emiss(ispc)%src_bburn(n1,n2,n3));emiss(ispc)%src_bburn(:,:,:) = 0.
     allocate (emiss(ispc)%src_bioge(n1,n2,n3));emiss(ispc)%src_bioge(:,:,:) = 0.
     allocate (emiss(ispc)%src_geoge(n1,n2,n3));emiss(ispc)%src_geoge(:,:,:) = 0.
    enddo
   
    if(use_bbem_plumerise == 1) then 
       do ispc=1,nspecies+1 ! last one will be used for the size of fire
         do iv=1,nveg_agreg
            allocate (emiss_plume(ispc,iv)%src_plume(n1,n2))
	              emiss_plume(ispc,iv)%src_plume(: ,: ) = 0.
         enddo
       enddo
    endif


    return
  end subroutine alloc_emiss_vars

  !---------------------------------------------------------------

  subroutine nullify_emiss_vars(emiss,emiss_plume,nspecies,nveg_agreg)

    implicit none

    type (emiss_vars),dimension(nspecies)  :: emiss
    type (emiss_plume_vars),dimension(nspecies+1,nveg_agreg)  :: emiss_plume
    integer iv,ispc,nspecies,nveg_agreg

    do ispc=1,nspecies
       if (associated(emiss(ispc)%src_antro)) nullify (emiss(ispc)%src_antro)
       if (associated(emiss(ispc)%src_bburn)) nullify (emiss(ispc)%src_bburn)
       if (associated(emiss(ispc)%src_bioge)) nullify (emiss(ispc)%src_bioge)
       if (associated(emiss(ispc)%src_geoge)) nullify (emiss(ispc)%src_geoge)
    enddo

    if(use_bbem_plumerise == 1) then 
       do ispc=1,nspecies+1 ! last one will be used for the size of fire
         do iv=1,nveg_agreg
            if (associated(emiss_plume(ispc,iv)%src_plume)) nullify (emiss_plume(ispc,iv)%src_plume)
         enddo
       enddo
    endif

    return
  end subroutine nullify_emiss_vars

end module emiss_vars_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_emiss_vars(n1,n2,n3)
    use emiss_vars_emissions
    use bbbem_plumerise, only: nveg_agreg
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

     if(.not. allocated(emiss_g)      ) allocate(emiss_g(emiss_nspecies))

     if(.not. allocated(emiss_plume_g)) allocate &
                       (emiss_plume_g(emiss_nspecies+1,nveg_agreg))
!   
    call nullify_emiss_vars(emiss_g(:),emiss_plume_g(:,:),emiss_nspecies,nveg_agreg)    
      
    call alloc_emiss_vars  (emiss_g(:),emiss_plume_g(:,:)&
                           ,n1,n2,n3,emiss_nspecies,nveg_agreg) 
    if(.not. allocated(emiss_spc_name  )) allocate(emiss_spc_name(emiss_nspecies,number_sources))
    if(.not. allocated(emiss_plume_name)) allocate(emiss_plume_name(emiss_nspecies+1,nveg_agreg))
    
    if(.not. allocated(found_emiss_spc   )) allocate(found_emiss_spc  (emiss_nspecies  ,number_sources))
    if(.not. allocated(found_emiss_plume )) allocate(found_emiss_plume(emiss_nspecies+1,nveg_agreg    ))

    ! set default value for found parameter
    found_emiss_spc  (:,:) = 0  ! => not found
    found_emiss_plume(:,:) = 0  ! => not found
 end subroutine mem_emiss_vars

!---------------------------------------------------------------
!
