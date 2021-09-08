!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module fire_properties

!---------------------------------------------------------------------------
integer, parameter :: ntfocos=200000 ! max number of fires per day

integer, parameter ::  nprop = 12

integer, parameter ::         &
 QLATI			  =1  &
,QLONG     		  =2  &
,QAREA    		  =3  &
,QSIZE			  =4  &
,QFRPW			  =5  &
,QFLAM			  =6  &
,QVEG			  =7  &
,QVEG_AGREG    		  =8  &
,QTIME    		  =9  &
,QFRE                     =10 &
,DFRPW                    =11 &
,DSIZE                    =12

!---------------------------------------------------------------------------
!character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
!(/                           & 
!  'ACIDS                    '&
!, 'ALCOHOLS                 '&
!, 'BENZENE                  '&
!, 'C2H2                     '&
!, 'XYLENE                   '/)

!---------------------------------------------------------------------------

  integer :: nfocos_sensor_gt_1km,nfocos_sensor_le_1km
 
  type fire_vars  
     real, pointer, dimension(:)  :: fire_prop
  end type fire_vars

  type (fire_vars), allocatable :: fire_sensor_gt_1km(:),fire_sensor_le_1km(:)



contains
  !---------------------------------------------------------------

  subroutine alloc_fire(fire_sensor_gt_1km,fire_sensor_le_1km)

    implicit none

    type (fire_vars),dimension(nprop)  :: fire_sensor_gt_1km,fire_sensor_le_1km
    integer i
    
    do i=1,nprop
     allocate (fire_sensor_gt_1km(i)%fire_prop(ntfocos))
     allocate (fire_sensor_le_1km(i)%fire_prop(ntfocos))
    enddo

    return
  end subroutine alloc_fire

  !---------------------------------------------------------------

  subroutine nullify_fire(fire_sensor_gt_1km,fire_sensor_le_1km)

    implicit none

    type (fire_vars),dimension(nprop)  :: fire_sensor_gt_1km
    type (fire_vars),dimension(nprop)  :: fire_sensor_le_1km
    integer i

    do i=1,nprop
       if (associated(fire_sensor_gt_1km(i)%fire_prop))   nullify (fire_sensor_gt_1km(i)%fire_prop)
       if (associated(fire_sensor_le_1km(i)%fire_prop))   nullify (fire_sensor_le_1km(i)%fire_prop)
    enddo

    return
  end subroutine nullify_fire

end module fire_properties

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_fire()
    use fire_properties
    implicit none
    integer i

     if(.not. allocated(fire_sensor_gt_1km)) allocate(fire_sensor_gt_1km(nprop))
     if(.not. allocated(fire_sensor_le_1km)) allocate(fire_sensor_le_1km(nprop))
!   
    !do i=1,nspecies
    ! if(associated(retro_g(i)%src)) deallocate(retro_g(i)%src)
    !enddo
    call nullify_fire(fire_sensor_gt_1km(:),fire_sensor_le_1km(:))      
    call alloc_fire  (fire_sensor_gt_1km(:),fire_sensor_le_1km(:)) 
  end subroutine mem_fire

!---------------------------------------------------------------
!subroutine get_fire_indentity(spc_name,ident)
!use chem1_list
!use retro_emissions, only :  retro_nspecies=>nspecies&
!                            ,retro_spc_name=>spc_name

!implicit none
!integer isp
!character (len=*), intent(in)  :: spc_name
!integer          , intent(out) :: ident

!do isp = 1,retro_nspecies
!  ident=-1
!  if(spc_name == retro_spc_name(isp)) then
!      print*,'==>retro found for ',spc_name
!      ident=isp
!      return
!   endif
!enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of retro'
!print*,'ident=',ident
!stop 444
!end subroutine get_fire_indentity
!---------------------------------------------------------------
