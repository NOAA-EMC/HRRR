!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
!  3BEM - Brazilian Biomass Burning Emission Model                              !
!###############################################################################!

module bbbem_plumerise    
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
!  0.00,& ! 
!  0.45,& ! % biomass burned at flaming phase : tropical forest       igbp 2 and 4
!  0.45,& ! % biomass burned at flaming phase : extratropical forest  igbp 1 , 3 and 5
!  0.75,& ! % biomass burned at flaming phase : cerrado/woody savanna igbp 6 to 9
!  0.97 / ! % biomass burned at flaming phase : pastagem/lavoura:     igbp 10 to 17
0.00,& ! 
0.30,& ! % biomass burned at flaming phase : floresta              igbp 1 a 4
0.30,& ! % biomass burned at flaming phase : floresta              igbp 1 a 4
0.50,& ! % biomass burned at flaming phase : cerrado/woody savanna igbp 5 a 9
0.00 / ! % biomass burned at flaming phase : pastagem/lavoura:     igbp 10 a 17


  type bbbem_plume_vars   
     real, pointer, dimension(:,:,:)  :: src
  end type bbbem_plume_vars

  type (bbbem_plume_vars), allocatable :: bbbem_plume_g(:,:)

  
  integer, parameter ::  nprop = 3

  integer, parameter ::        &
  qarea_agreg		   =1  &
, qarea_std_agreg     	   =2  &
, qfires_agreg             =3 

  
  type bbbem_plume_vars2   
     real, pointer, dimension(:,:)  :: fire_prop
  end type bbbem_plume_vars2

  type (bbbem_plume_vars2), allocatable :: bbbem_plume_fire_prop_g(:,:)


! mean values for save memory
  type bbbem_plume_mean_vars   
     real, pointer, dimension(:,:,:)  :: src
  end type bbbem_plume_mean_vars

  type (bbbem_plume_mean_vars), allocatable :: bbbem_plume_mean_g(:)




contains
  !---------------------------------------------------------------

  subroutine alloc_bbbem_plume(bbbem_plume,bbbem_plume_fire_prop,bbbem_plume_mean&
                              ,n1,n2,n3,nspecies)

    implicit none

    type (bbbem_plume_vars )     ,dimension(nspecies,nveg_agreg)  :: bbbem_plume
    type (bbbem_plume_mean_vars ),dimension(         nveg_agreg)  :: bbbem_plume_mean
    type (bbbem_plume_vars2),dimension(nprop,   nveg_agreg)  :: bbbem_plume_fire_prop
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies,iv
    
    do ispc=1,nspecies
      do iv=1,nveg_agreg
       allocate (bbbem_plume(ispc,iv)%src(n1,n2,n3))
       bbbem_plume(ispc,iv)%src(:,:,:)=0.
      enddo
    enddo
    do iv=1,nveg_agreg
     allocate (bbbem_plume_mean(iv)%src(n1,n2,n3))
     bbbem_plume_mean(iv)%src(:,:,:)=0.
    enddo
    do ispc=1,nprop
      do iv=1,nveg_agreg
       allocate (bbbem_plume_fire_prop(ispc,iv)%fire_prop(n1,n2))
       bbbem_plume_fire_prop(ispc,iv)%fire_prop(:,:)=0.
      enddo
    enddo

    return
  end subroutine alloc_bbbem_plume

  !---------------------------------------------------------------

  subroutine nullify_bbbem_plume(bbbem_plume,bbbem_plume_fire_prop,bbbem_plume_mean,nspecies)

    implicit none

    type (bbbem_plume_vars )     ,dimension(nspecies,nveg_agreg)  :: bbbem_plume
    type (bbbem_plume_mean_vars ),dimension(         nveg_agreg)  :: bbbem_plume_mean
    type (bbbem_plume_vars2)     ,dimension(nprop,   nveg_agreg)  :: bbbem_plume_fire_prop
    integer ispc,nspecies,iv
    
    do ispc=1,nspecies
      do iv=1,nveg_agreg
       if (associated(bbbem_plume(ispc,iv)%src))    nullify (bbbem_plume(ispc,iv)%src)
      enddo
    enddo
    do iv=1,nveg_agreg
     if (associated(bbbem_plume_mean(iv)%src))	 nullify (bbbem_plume_mean(iv)%src)
    enddo
    
    do ispc=1,nprop
    do iv=1,nveg_agreg
      if (associated(bbbem_plume_fire_prop(ispc,iv)%fire_prop))  nullify &
                    (bbbem_plume_fire_prop(ispc,iv)%fire_prop)
    enddo
    enddo

    return
  end subroutine nullify_bbbem_plume

end module bbbem_plumerise
!------------------------------------------------------------------------------------------------------

  subroutine mem_bbbem_plume(n1,n2,n3)
    use bbbem_plumerise
    implicit none
    integer i
    integer:: n1,n2,n3

    if(.not. allocated(bbbem_plume_g))           allocate(bbbem_plume_g     (nspecies,nveg_agreg))
    if(.not. allocated(bbbem_plume_mean_g))      allocate(bbbem_plume_mean_g(         nveg_agreg))
    if(.not. allocated(bbbem_plume_fire_prop_g)) allocate(bbbem_plume_fire_prop_g(nprop,nveg_agreg))
   
    !do i=1,nspecies
    ! if(associated(bbbem_g(i)%src)) deallocate(bbbem_g(i)%src)
    !enddo

    call nullify_bbbem_plume(bbbem_plume_g(:,:),bbbem_plume_fire_prop_g(:,:)&
                            ,bbbem_plume_mean_g(:)  ,nspecies)      
    call alloc_bbbem_plume  (bbbem_plume_g(:,:),bbbem_plume_fire_prop_g(:,:)&
                            ,bbbem_plume_mean_g(:)  ,n1,n2,n3,nspecies) 
  end subroutine mem_bbbem_plume

!------------------------------------------------------------------------------------------------------
