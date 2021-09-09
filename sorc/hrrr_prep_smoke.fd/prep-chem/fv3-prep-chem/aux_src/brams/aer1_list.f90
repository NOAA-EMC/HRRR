MODULE aer1_list
  IMPLICIT NONE
  
  
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=5
  INTEGER,PARAMETER :: nmodes=3
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
      'sdust  ' & !
     ,'bburn  ' & !
     ,'urban  ' & !
     ,'bioge  ' & !
     ,'marin  ' & !
   /)
  
  
  !Number of each specie   
  INTEGER,PARAMETER :: sdust=001
  INTEGER,PARAMETER :: bburn=002
  INTEGER,PARAMETER :: urban=003
  INTEGER,PARAMETER :: bioge=004
  INTEGER,PARAMETER :: marin=005
  
  
  !for memory allocattion: 
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes) :: mode_name=(/ &
      '1  ' & !
     ,'2  ' & !
     ,'3  ' &
      /)
 
  INTEGER,PARAMETER :: nucle = 1 ! nucleation mode
  INTEGER,PARAMETER :: accum = 2 ! accumulation mode
  INTEGER,PARAMETER :: coarse = 3 ! coarse mode 
  
  !define if a specific mode will exist (=1) or not (=0)
  !for  modes nucle = 1, accum = 2,coarse = 3  
  INTEGER,PARAMETER,DIMENSION(nmodes,nspecies) :: mode_alloc=RESHAPE((/ &
    0 , 0 , 0 ,    & ! sdust
    0 , 1 , 1 ,    & ! bburn (0, pm25, pm10)
    1 , 1 , 0 ,    & ! urban
    0 , 0 , 0 ,    & ! bioge
    0 , 0 , 0      & ! marin
    /),(/nmodes,nspecies/))
    
      CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes,nspecies) :: aer_name=RESHAPE((/ &
    'sdust1' , 'sdust2' , 'sdust2' ,    & ! sdust
    'PM0   ' , 'PM25  ' , 'PM10  ' ,    & ! bburn (0, pm25, pm10)
    'SO41  ' , 'SO4   ' , 'urban3' ,    & ! urban
    'bioge1' , 'bioge2' , 'bioge3' ,    & ! bioge
    'marin1' , 'marin2' , 'marin3'      & ! marin
    /),(/nmodes,nspecies/))


  
   !section for aer type 1: dus
  !This parameters are use for documentation only. 
  !Use them in a program in substitution of numerical terms.
  INTEGER,PARAMETER :: src     = 1 ! source term 
  INTEGER,PARAMETER :: ddp     = 2 ! dry deposition 
  INTEGER,PARAMETER :: wdp     = 3 ! wet deposition 
  INTEGER,PARAMETER :: fdda    = 4 ! four-dim assimilation 
  INTEGER,PARAMETER :: offline = 5 ! off-line emissions: 
                                   !=1, emission will be read from file
				   !=0, emission will be calculated during the model simulation (on-line emission)
  INTEGER,PARAMETER :: transport = 6 ! transported species 
                                   !=1, yes
				   !=0, not

  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda, 5=offline emission, 6=transport]) 
  ! attention : for aerosols,  mode_alloc(ispc) = spc_alloc(transport,imode,ispc)
  INTEGER,PARAMETER,DIMENSION(6,nmodes,nspecies) :: spc_alloc=RESHAPE((/ &
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode nucle, aer sdust
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode accum, aer sdust
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode coarse, aer sdust
!
!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode nucle, aer bburn
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! mode accum, aer bburn
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! mode coarse, aer bburn
!
!
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! mode nucle, aer urban
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! mode accum, aer urban
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode coarse, aer urban
!
!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode nucle, aer bioge
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode accum, aer bioge
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode coarse, aer bioge
!
!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode nucle, aer marin
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! mode accum, aer marin
    0 , 0 , 0 , 0 , 0 , 0   & ! mode coarse, aer marin
!
    /),(/6,nmodes,nspecies/))
  
  ! effective particle radius (meter)
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_radius=RESHAPE((/ &
    1.95e-7 , 1.95e-7 , 1.95e-7 ,    & ! sdust
    1.95e-7 , 1.95e-7 , 1.00e-5 ,    & ! bburn (0, pm25, pm10) meters
    1.95e-7 , 1.95e-7 , 1.95e-7 ,    & ! urban
    1.95e-7 , 1.95e-7 , 1.95e-7 ,    & ! bioge
    1.95e-7 , 1.95e-7 , 1.95e-7      & ! marin
    /),(/nmodes,nspecies/))

  ! particle density kg/m^3
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_dens=RESHAPE((/ &
    2.65e+3 , 2.65e+3 , 2.65e+3 ,    & ! sdust
    1.35e+3 , 1.35e+3 , 1.35e+3 ,    & ! bburn (0, pm25, pm10) kg/m^3
    1.35e+3 , 1.35e+3 , 1.35e+3 ,    & ! urban
    1.35e+3 , 1.35e+3 , 1.35e+3 ,    & ! bioge
    2.20e+3 , 2.20e+3 , 2.20e+3      & ! marin
    /),(/nmodes,nspecies/))
  
  
END MODULE aer1_list
