MODULE aer1_list
  IMPLICIT NONE


  CHARACTER(LEN=24),PARAMETER :: aerosol_mechanism='SIMPLE'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=6
  INTEGER,PARAMETER :: nmodes=10


  !Name of species
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
      'sdust  ' & !
     ,'bburn  ' & !
     ,'urban  ' & !
     ,'bioge  ' & !
     ,'marin  ' & !
     ,'v_ash  ' & !
   /)


  !Number of each specie
  INTEGER,PARAMETER :: sdust=001
  INTEGER,PARAMETER :: bburn=002
  INTEGER,PARAMETER :: urban=003
  INTEGER,PARAMETER :: bioge=004
  INTEGER,PARAMETER :: marin=005
  INTEGER,PARAMETER :: v_ash=006


  !for memory allocattion:
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0


  !Name of species
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes) :: mode_name=(/ &
      '1  ' & !
     ,'2  ' & !
     ,'3  ' & !
     ,'4  ' & !
     ,'5  ' & !
     ,'6  ' & !
     ,'7  ' & !
     ,'8  ' & !
     ,'9  ' & !
     ,'10 ' &
      /)

  INTEGER,PARAMETER :: nucle = 1 ! nucleation mode
  INTEGER,PARAMETER :: accum = 2 ! accumulation mode
  INTEGER,PARAMETER :: coarse = 3 ! coarse mode
  INTEGER, PARAMETER :: X = 0
  !define if a specific mode will exist (=1) or not (=0)
  !for  bins 1 to 10
  INTEGER,PARAMETER,DIMENSION(nmodes,nspecies) :: mode_alloc=RESHAPE((/ &
!----------------------------------------------------------
!-bin size 1   2   3  4   5   6   7   8   9  10
!----------------------------------------------------------
          0 , 0 , 0 ,0 , 0 , 0 , 0 , 0 , 0 , 0,   & ! sdust
          0 , 1 , 1 ,0 , 0 , 0 , 0 , 0 , 0 , 0,   & ! bburn (0, pm25, pm10)
          0 , 1 , 1 ,0 , 0 , 0 , 0 , 0 , 0 , 0,   & ! urban
          0 , 0 , 0 ,0 , 0 , 0 , 0 , 0 , 0 , 0,   & ! bioge
          1 , 1 , 1 ,0 , 0 , 0 , 0 , 0 , 0 , 0,   & ! marin
          1 , 1 , 1 ,1 , 1 , 1 , 1 , 1 , 1 , 1    & ! v_ash
!----------------------------------------------------------
          /),(/nmodes,nspecies/))

  INTEGER,PARAMETER,DIMENSION(nmodes) :: numb_mod_alloc=(/ X , X , X , X , X , X , X , X , X , X /)! numb = 10 (specie = 0)

  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes,nspecies) :: aer_name=RESHAPE((/ &
    'sdust1 ' , 'sdust2 ' , 'sdust3 ' , 'sdust4 ' , 'sdust5 ' , &
    'sdust6 ' , 'sdust7 ' , 'sdust8 ' , 'sdust9 ' , 'sdust10',  & ! sdust
    'BBURN1 ' , 'BBURN2 ' , 'BBURN3 ' , 'BBURN4 ' , 'BBURN5 ' , &
    'BBURN6 ' , 'BBURN7 ' , 'BBURN8 ' , 'BBURN9 ' , 'BBURN10',  & ! bburn (0, pm25, pm10)
    'URBAN1 ' , 'URBAN2 ' , 'URBAN3 ' , 'URBAN4 ' , 'URBAN5 ' , &
    'URBAN6 ' , 'URBAN7 ' , 'URBAN8 ' , 'URBAN9 ' , 'URBAN10',  & ! URBAN (0, pm25, pm10)
    'bioge1 ' , 'bioge2 ' , 'bioge3 ' , 'bioge4 ' , 'bioge5 ' , &
    'bioge6 ' , 'bioge7 ' , 'bioge8 ' , 'bioge9 ' , 'bioge10',  & ! bioge
    'marin1 ' , 'marin2 ' , 'marin3 ' , 'marin4 ' , 'marin5 ' , &
    'marin6 ' , 'marin7 ' , 'marin8 ' , 'marin9 ' , 'marin10',  & ! marin
    'V_ASH1 ' , 'V_ASH2 ' , 'V_ASH3 ' , 'V_ASH4 ' , 'V_ASH5 ' , &
    'V_ASH6 ' , 'V_ASH7 ' , 'V_ASH8 ' , 'V_ASH9 ' , 'V_ASH10'   & ! VOLC ASH
    /),(/nmodes,nspecies/))

  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes) :: numb_name=(/ &
      'Dummy','Dummy','Dummy','Dummy','Dummy', &
      'Dummy','Dummy','Dummy','Dummy','Dummy' /)


  real :: mass_bin_dist(nmodes) ! only for ash


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
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 1
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 2
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 3
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 4
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 5
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 6
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 7
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 8
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 9
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer sdust bin 10

!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 1  - PM0
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! aer bburn bin 2  - PM2.5
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! aer bburn bin 3  - PM10
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 4
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 5
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 6
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 7
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 8
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 9
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bburn bin 10
!
!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 1
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! aer urban bin 2 ! urban PM2.5
    1 , 1 , 1 , 0 , 0 , 1 ,  & ! aer urban bin 3 ! continental
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 4
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 5
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 6
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 7
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 8
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 9
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer urban bin 10
!
!
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 1
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 2
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 3
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 4
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 5
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 6
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 7
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 8
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 9
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer bioge bin 10
!
    1 , 1 , 1 , 0 , 1 , 1 ,  & ! aer marin bin 1
    1 , 1 , 1 , 0 , 1 , 1 ,  & ! aer marin bin 2
    1 , 1 , 1 , 0 , 1 , 1 ,  & ! aer marin bin 3
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 4
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 5
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 6
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 7
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 8
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 9
    0 , 0 , 0 , 0 , 0 , 0 ,  & ! aer marin bin 10
!
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 1
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 2
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 3
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 4
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 5
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 6
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 7
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 8
    1 , 1 , 0 , 0 , 0 , 1 ,  & ! aer v_ash bin 9
    1 , 1 , 0 , 0 , 0 , 1    & ! aer v_ash bin 10
!
!
    /),(/6,nmodes,nspecies/))

    INTEGER,PARAMETER,DIMENSION(6,nmodes) :: numb_alloc=reshape((/ &
      X , X , X , X , X , X , & ! 0X
      X , X , X , X , X , X , & ! 02
      X , X , X , X , X , X , & ! 03
      X , X , X , X , X , X , & ! 04
      X , X , X , X , X , X , & ! 05
      X , X , X , X , X , X , & ! 06
      X , X , X , X , X , X , & ! 07*
      X , X , X , X , X , X , & ! 08 *
      X , X , X , X , X , X , & ! 09 *
      X , X , X , X , X , X   & ! X0
      !
      /),(/6,nmodes/))




  ! effective particle radius (meter)
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_radius=RESHAPE((/ &
!------------------------------------------------------------------------------------------------------------------
!-bin size  1          2        3        4          5       6       7       8     9    10
!------------------------------------------------------------------------------------------------------------------
     	  1.95e-7 , 1.95e-7 , 1.95e-7 , 999., 999., 999., 999., 999., 999., 999.,   & ! sdust
     	  1.95e-7 , 1.95e-7 , 1.00e-5 , 999., 999., 999., 999., 999., 999., 999.,   & ! bburn (0, pm25, pm10) meters
     	  1.95e-7 , 1.95e-7 , 1.95e-7 , 999., 999., 999., 999., 999., 999., 999.,   & ! urban
     	  1.95e-7 , 1.95e-7 , 1.95e-7 , 999., 999., 999., 999., 999., 999., 999.,   & ! bioge
     	  8.25e-8 , 2.82e-7 , 1.61e-6 , 999., 999., 999., 999., 999., 999., 999.,   & ! marin
     	  0.98e-6,  2.93e-6,  5.89e-6, 11.72e-6, 23.44e-6, 46.88e-6, 93.75e-6, 0.1875e-3, 0.375e-3, 0.750e-3   & ! v_ash
!---------------------------------------------------------- ---------------------------- ----------------------------
    /),(/nmodes,nspecies/))




  ! particle density kg/m^3
  !srf-dez2013: changing density for ash from 2500 to 900 kg/m3
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_dens=RESHAPE((/ &
!------------------------------------------------------------------------------------------------------------------
!-bin size 1   2   3  4   5   6   7   8   9  10
!------------------------------------------------------------------------------------------------------------------
    2.65e+3 , 2.65e+3 , 2.65e+3 , 999., 999., 999., 999., 999., 999., 999.,   & ! sdust
    1.35e+3 , 1.35e+3 , 1.35e+3 , 999., 999., 999., 999., 999., 999., 999.,   & ! bburn (0, pm25, pm10) kg/m^3
    1.35e+3 , 1.35e+3 , 1.35e+3 , 999., 999., 999., 999., 999., 999., 999.,   & ! urban
    1.35e+3 , 1.35e+3 , 1.35e+3 , 999., 999., 999., 999., 999., 999., 999.,   & ! bioge
    2.17e+3 , 2.17e+3 , 2.17e+3 , 999., 999., 999., 999., 999., 999., 999.,   & ! marin
    0.90e+3 , 0.90e+3 , 0.90e+3 , 0.90e+3, 0.90e+3, 0.90e+3, 0.90e+3, 0.90e+3, 0.90e+3, 0.90e+3    & ! v_ash
!    2.50e+3 , 2.50e+3 , 2.50e+3 , 2.50e+3, 2.50e+3, 2.50e+3, 2.50e+3, 2.50e+3, 2.50e+3, 2.50e+3    & ! v_ash
!------------------------------------------------------------------------------------------------------------------
    /),(/nmodes,nspecies/))



! - end of SIMPLE AER MODEL relevant parameters
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!----- the section below is DUMMY and only is used by the MATRIX aerosol model.
!----- it must be keept in this aer1_list file for consistency with the code for the MATRIX aerosol model.

    CHARACTER(LEN=1 ),PARAMETER :: matrix_level='-99'
    INTEGER,PARAMETER :: N_matrix_level=-99
    INTEGER,PARAMETER :: nspeciesmx=5   !kml: definido no memMatrix como nmass_spcs = 5
    INTEGER,PARAMETER :: ninorg=3     
    INTEGER,PARAMETER :: nmodesmx=18    !kml: definido no memMatrix como nmodes_max = 18
    
    CHARACTER(LEN=8),PARAMETER,DIMENSION(ninorg) :: inorg_spc_name=(/  &
      'nitrate '   &   !kml... ocupam as 3 primeiras posicoes do aero_spcs
     ,'ammonium'   & 
     ,'water   '   & 
     /)
    CHARACTER(LEN=13),PARAMETER,DIMENSION(ninorg) :: inorg_name=        (/ &
    'mass_nitrato ','mass_ammonium','mass_water   ' /)
   
    INTEGER,PARAMETER,DIMENSION(ninorg) :: inorg_mod_alloc=(/ &
             X ,X , X /)!   = 3 

    ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda, 5=offline emission, 6=transport])
    ! attention : for aerosols,  mode_alloc(ispc) = spc_alloc(transport,imode,ispc)
    INTEGER,PARAMETER,DIMENSION(6,ninorg) :: inorg_alloc=reshape((/ &
     X , X , X , X , X , X , & ! mass_nitrate   t49
     X , X , X , X , X , X , & ! mass_ammonium  t50
     X , X , X , X , X , X   & ! mass_water     t51
     /),(/6,ninorg/))
    INTEGER,PARAMETER :: sulf=1
    INTEGER,PARAMETER :: bcar=1
    INTEGER,PARAMETER :: ocar=1
    INTEGER,PARAMETER :: dust=1
    INTEGER,PARAMETER :: seas=1
    INTEGER,PARAMETER::akk =1 , &! 
                       acc =1 , &!
                       dd1 =1 , &
                       ds1 =1 , &!
                       dd2 =1 , &!
                       ds2 =1 , &!
                       ssa =1 , &!
                       ssc =1 , &!
                       sss =1 , &!
                       occ =1 , &!
                       bc1 =1 , &!
                       bc2 =1 , &!
                       bc3 =1 , &!
                       ocs =1 , &!
                       dbc =1 , &!
                       boc =1 , &!
                       bcs =1 , &!
                       mxx =1
   INTEGER,PARAMETER,DIMENSION(nmodesmx,nspeciesmx) :: aer12matrix=RESHAPE((/ &
         ! X X X X X X X X X X X X X X X X X X
           X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,&! SULF
           X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,&! BCAR
           X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,&! OCAR
           X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,&! DUST
           X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X &! SEAS 
         /),(/nmodesmx,nspeciesmx/))

   INTEGER,PARAMETER, DIMENSION(ninorg) :: aer1_inorg2matrix=(/X,X,X/)
   !                                                     X X X X X X X X X X X X X X X X X X  
   INTEGER,PARAMETER, DIMENSION(nmodesmx) :: aer22matrix=(/X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X/)

END MODULE aer1_list
