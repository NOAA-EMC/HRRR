!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
!  3BEM - Brazilian Biomass Burning Emission Model                              !
!###############################################################################!

module gfedv2_plumerise    
use AeM_emission_factors,  nspecies=>AeM_nspecies, N2_nitrogenio=>N2
 
  integer, parameter :: nveg_agreg      = 4
  integer, parameter :: tropical_forest = 1
  integer, parameter :: boreal_forest   = 2
  integer, parameter :: savannah        = 3
  integer, parameter :: grassland       = 4
  character(len=20), parameter :: veg_name(nveg_agreg) = (/ &
                               'Tropical-Forest', &
                               'Boreal-Forest  ', &
                               'Savanna        ', &
                               'Grassland      ' /)
  character(len=20), parameter :: spc_suf(nveg_agreg) = (/ &
                               'agtf' , &  ! trop forest
                               'agef' , &  ! extratrop forest
                               'agsv' , &  ! savanna
                               'aggr'   /) ! grassland

  real,   dimension(0:nveg_agreg) :: flaming
  data flaming/&  
  0.00,& ! 
  0.45,& ! % biomass burned at flaming phase : tropical forest       igbp 2 and 4
  0.45,& ! % biomass burned at flaming phase : extratropical forest  igbp 1 , 3 and 5
  0.75,& ! % biomass burned at flaming phase : cerrado/woody savanna igbp 6 to 9
  0.97 / ! % biomass burned at flaming phase : pastagem/lavoura:     igbp 10 to 17


  type gfedv2_plume_vars   
     real, pointer, dimension(:,:,:)  :: src
  end type gfedv2_plume_vars

  type (gfedv2_plume_vars), allocatable :: gfedv2_plume_g(:,:)

  
  integer, parameter ::  nprop = 3

  integer, parameter ::        &
  qarea_agreg		   =1  &
, qarea_std_agreg     	   =2  &
, qfires_agreg             =3 

  
  type gfedv2_plume_vars2   
     real, pointer, dimension(:,:)  :: fire_prop
  end type gfedv2_plume_vars2

  type (gfedv2_plume_vars2), allocatable :: gfedv2_plume_fire_prop_g(:,:)


! mean values for save memory
  type gfedv2_plume_mean_vars   
     real, pointer, dimension(:,:,:)  :: src
  end type gfedv2_plume_mean_vars

  type (gfedv2_plume_mean_vars), allocatable :: gfedv2_plume_mean_g(:)




contains
  !---------------------------------------------------------------

  subroutine alloc_gfedv2_plume(gfedv2_plume,gfedv2_plume_fire_prop,gfedv2_plume_mean&
                              ,n1,n2,n3,nspecies)

    implicit none

    type (gfedv2_plume_vars )     ,dimension(nspecies,nveg_agreg)  :: gfedv2_plume
    type (gfedv2_plume_mean_vars ),dimension(         nveg_agreg)  :: gfedv2_plume_mean
    type (gfedv2_plume_vars2),dimension(nprop,   nveg_agreg)  :: gfedv2_plume_fire_prop
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies,iv
    
    do ispc=1,nspecies
      do iv=1,nveg_agreg
       allocate (gfedv2_plume(ispc,iv)%src(n1,n2,n3))
       gfedv2_plume(ispc,iv)%src(:,:,:)=0.
      enddo
    enddo
    do iv=1,nveg_agreg
     allocate (gfedv2_plume_mean(iv)%src(n1,n2,n3))
     gfedv2_plume_mean(iv)%src(:,:,:)=0.
    enddo
    do ispc=1,nprop
      do iv=1,nveg_agreg
       allocate (gfedv2_plume_fire_prop(ispc,iv)%fire_prop(n1,n2))
       gfedv2_plume_fire_prop(ispc,iv)%fire_prop(:,:)=0.
      enddo
    enddo

    return
  end subroutine alloc_gfedv2_plume

  !---------------------------------------------------------------

  subroutine nullify_gfedv2_plume(gfedv2_plume,gfedv2_plume_fire_prop,gfedv2_plume_mean,nspecies)

    implicit none

    type (gfedv2_plume_vars )     ,dimension(nspecies,nveg_agreg)  :: gfedv2_plume
    type (gfedv2_plume_mean_vars ),dimension(         nveg_agreg)  :: gfedv2_plume_mean
    type (gfedv2_plume_vars2)     ,dimension(nprop,   nveg_agreg)  :: gfedv2_plume_fire_prop
    integer ispc,nspecies,iv
    
    do ispc=1,nspecies
      do iv=1,nveg_agreg
       if (associated(gfedv2_plume(ispc,iv)%src))    nullify (gfedv2_plume(ispc,iv)%src)
      enddo
    enddo
    do iv=1,nveg_agreg
     if (associated(gfedv2_plume_mean(iv)%src))	 nullify (gfedv2_plume_mean(iv)%src)
    enddo
    
    do ispc=1,nprop
    do iv=1,nveg_agreg
      if (associated(gfedv2_plume_fire_prop(ispc,iv)%fire_prop))  nullify &
                    (gfedv2_plume_fire_prop(ispc,iv)%fire_prop)
    enddo
    enddo

    return
  end subroutine nullify_gfedv2_plume

end module gfedv2_plumerise
!------------------------------------------------------------------------------------------------------

  subroutine mem_gfedv2_plume(n1,n2,n3)
    use gfedv2_plumerise
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(gfedv2_plume_g))           allocate(gfedv2_plume_g     (nspecies,nveg_agreg))
    if(.not. allocated(gfedv2_plume_mean_g))      allocate(gfedv2_plume_mean_g(         nveg_agreg))
    if(.not. allocated(gfedv2_plume_fire_prop_g)) allocate(gfedv2_plume_fire_prop_g(nprop,nveg_agreg))
   
    !do i=1,nspecies
    ! if(associated(gfedv2_g(i)%src)) deallocate(gfedv2_g(i)%src)
    !enddo

    call nullify_gfedv2_plume(gfedv2_plume_g(:,:),gfedv2_plume_fire_prop_g(:,:)&
                            ,gfedv2_plume_mean_g(:)  ,nspecies)      
    call alloc_gfedv2_plume  (gfedv2_plume_g(:,:),gfedv2_plume_fire_prop_g(:,:)&
                            ,gfedv2_plume_mean_g(:)  ,n1,n2,n3,nspecies) 
  end subroutine mem_gfedv2_plume

!------------------------------------------------------------------------------------------------------
