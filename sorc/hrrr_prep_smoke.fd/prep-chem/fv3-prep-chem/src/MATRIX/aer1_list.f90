MODULE aer1_list
  IMPLICIT NONE

  CHARACTER(LEN=24),PARAMETER :: aerosol_mechanism='MATRIX'
  CHARACTER(LEN=1 ),PARAMETER :: matrix_level='1'
  INTEGER,PARAMETER :: N_matrix_level=1
  
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=5   !kml: definido no memMatrix como nmass_spcs = 5
  INTEGER,PARAMETER :: ninorg=3     
  
  INTEGER,PARAMETER :: nmodes=18    !kml: definido no memMatrix como nmodes_max = 18
  INTEGER,PARAMETER :: number_aer_transported=51    

  !- Name of species
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
      'sulf   ' & !
     ,'bcar   ' & !
     ,'ocar   ' & !
     ,'dust   ' & !
     ,'seas   ' & !
   /)


  CHARACTER(LEN=8),PARAMETER,DIMENSION(ninorg) :: inorg_spc_name=(/  &
      'nitrate '   &   !kml... ocupam as 3 primeiras posicoes do aero_spcs
     ,'ammonium'   & 
     ,'water   '   & 
   /)


!kml: A principio, emitir sulfato somente no mecanismo de gases (SO2) e no MatrixDriver passar o SULF para o
!kml: GAS(1)=H2SO4 usando GAS(1)=SULF*dn0*96.07/98.079
!kml: Posteriormente, testar o particionamento em 97.5%SO2 => SO2 da quimica de gases e 2.5%SO2 => 
!kml: matrix na forma de particulado, sendo 99%=>ACC e 1%=>AKK.

!kml: No MatrixDriver passar o HNO3 do mecanismo de gases para o GAS(2)=HNO3 usando GAS(2)=HNO3*dn0*62.0049/63.01

!kml: O NH3 nao existe no mecanismo quimico e, portanto, precisa ser incluido no chem1_list(emissao, transporte, 
!kml: deposicao seca e deposicao umida)
!kml: No MatrixDriver passar o NH3 do mecanismo de gases para o GAS(3)=NH3 usando GAS(2)=NH3*dn0*18.03851/17.031
!   /)


  !Number of each specie
  INTEGER,PARAMETER :: sulf=001
  INTEGER,PARAMETER :: bcar=002
  INTEGER,PARAMETER :: ocar=003
  INTEGER,PARAMETER :: dust=004
  INTEGER,PARAMETER :: seas=005

  !Number of each inorganic aer specie
  INTEGER,PARAMETER :: NITRATE =001
  INTEGER,PARAMETER :: AMMONIUM=002
  INTEGER,PARAMETER :: WATER   =003


  !Number of each gas specie
!  INTEGER,PARAMETER :: H2SO4=001
!  INTEGER,PARAMETER :: HNO3 =002
!  INTEGER,PARAMETER :: NH3  =003


  !for memory allocattion:
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0
  INTEGER,PARAMETER :: X = 0


  !Name of species
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes) :: mode_name=(/ &
  !kml: Definido em memMatrix, o conjunto de fato utilizado depende do mecanismo e eh definido no setup
  !kml: cruzando as matrizes mname e modes"x".   
      'akk' & !Sulfate Aitken mode     
     ,'acc' & !Sulfate accum. mode
     ,'dd1' & !Dust accum. mode  (=<5% inorg.) + Sulfate
     ,'ds1' & !Dust accum. mode  ( >5% inorg.) + Sulfate
     ,'dd2' & !Dust coarse. mode (=<5% inorg.) + Sulfate
     ,'ds2' & !Dust coarse mode  ( >5% inorg.) + Sulfate
     ,'ssa' & !Sea Salt accum. mode            + Sulfate
     ,'ssc' & !Sea Salt coarse mode            + Sulfate
     ,'sss' & !Sea Salt                        + Sulfate
     ,'occ' & !Organic carbon                  + Sulfate
     ,'bc1' & !Black Carbon ( =<5% inorg.)     + Sulfate
     ,'bc2' & !Black Carbon (5-20% inorg.)     + Sulfate
     ,'bc3' & !Black Carbon ( >20% inorg.)     + Sulfate
     ,'ocs' & !Organic Carbon                  + Sulfate
     ,'dbc' & !Black Carbon + Mineral Dust     + Sulfate
     ,'boc' & !Black Carbon + Organic Carbon   + Sulfate
     ,'bcs' & !Black Carbon                    + Sulfate
     ,'mxx' & !Black Carbon + Organic Carbon   + Sulfate   +  Mineral Dust + Sea Salt
           /)

  INTEGER,PARAMETER :: akk = 01 , &! aitken
                       acc = 02 , &!
                       dd1 = 03 , &
                       ds1 = 04 , &!
                       dd2 = 05 , &!
                       ds2 = 06 , &!
                       ssa = 07 , &!
                       ssc = 08 , &!
                       sss = 09 , &!
                       occ = 10 , &!
                       bc1 = 11 , &!
                       bc2 = 12 , &!
                       bc3 = 13 , &!
                       ocs = 14 , &!
                       dbc = 15 , &!
                       boc = 16 , &!
                       bcs = 17 , &!
                       mxx = 18

  !define if a specific mode will exist (=1), not (=0), No permission by mechanism (=X)
  !for  bins 1 to 10
  !Mechanism = 1
  INTEGER,PARAMETER,DIMENSION(nmodes,nspecies) :: mode_alloc=RESHAPE((/ &
!------------------------------------------------------------------------------------
!        akk acc dd1 ds1 dd2 ds2 ssa ssc sss occ bc1 bc2 bc3 ocs dbc boc bcs mxx
!         0   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1   1
!-mode    1   2   3   4   5   6   7   8   9   0   1   2   3   4   5   6   7   8
!------------------------------------------------------------------------------------
          1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , X , 1 , 1 , 1 , 1 , X , 1 , 1 , 1 , 1 , & ! sulf = 16
          X , X , X , X , X , X , X , X , X , X , 1 , 1 , 1 , X , 1 , 1 , 1 , 1 , & ! bcar =  7
          X , X , X , X , X , X , X , X , X , 1 , X , X , X , X , X , 1 , X , 1 , & ! ocar =  3
          X , X , 1 , 1 , 1 , 1 , X , X , X , X , X , X , X , X , 1 , X , X , 1 , & ! dust =  6
          X , X , X , X , X , X , 1 , 1 , X , X , X , X , X , X , X , X , X , 1   & ! seas =  3
!------------------------------------------------------------------------------------
          /),(/nmodes,nspecies/))
                                                                     !      Total:       = 35

     INTEGER,PARAMETER,DIMENSION(nmodes) :: numb_mod_alloc=(/ &
   !------------------------------------------------------------------------------------
   !        akk acc dd1 ds1 dd2 ds2 ssa ssc sss occ bc1 bc2 bc3 ocs dbc boc bcs mxx
   !         0   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   1   1
   !-mode    1   2   3   4   5   6   7   8   9   0   1   2   3   4   5   6   7   8
   !------------------------------------------------------------------------------------
             1 , 1 , 1 , 1 , 1 , 1 , X , X , X , 1 , 1 , 1 , 1 , X , 1 , 1 , 1 , 1/)! numb = 14 (specie = 0)


  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes,nspecies) :: aer_name=RESHAPE((/ &

      'akk_sulf','acc_sulf','dd1_sulf','ds1_sulf','dd2_sulf','ds2_sulf', &! 1
      'ssa_sulf','ssc_sulf','ssc_sulf','occ_sulf','bc1_sulf','bc2_sulf', &! 2
      'bc3_sulf','ocs_sulf','dbc_sulf','boc_sulf','bcs_sulf','mxx_sulf', &! 3
      'akk_bcar','acc_bcar','dd1_bcar','ds1_bcar','dd2_bcar','ds2_bcar', &! 4
      'ssa_bcar','ssc_bcar','ssc_bcar','occ_bcar','bc1_bcar','bc2_bcar', &! 5
      'bc3_bcar','ocs_bcar','dbc_bcar','boc_bcar','bcs_bcar','mxx_bcar', &! 6
      'akk_ocar','acc_ocar','dd1_ocar','ds1_ocar','dd2_ocar','ds2_ocar', &! 7
      'ssa_ocar','ssc_ocar','ssc_ocar','occ_ocar','bc1_ocar','bc2_ocar', &! 8
      'bc3_ocar','ocs_ocar','dbc_ocar','boc_ocar','bcs_ocar','mxx_ocar', &! 9
      'akk_dust','acc_dust','dd1_dust','ds1_dust','dd2_dust','ds2_dust', &! 10
      'ssa_dust','ssc_dust','ssc_dust','occ_dust','bc1_dust','bc2_dust', &! 11
      'bc3_dust','ocs_dust','dbc_dust','boc_dust','bcs_dust','mxx_dust', &! 12
      'akk_seas','acc_seas','dd1_seas','ds1_seas','dd2_seas','ds2_seas', &! 13
      'ssa_seas','ssc_seas','ssc_seas','occ_seas','bc1_seas','bc2_seas', &! 14
      'bc3_seas','ocs_seas','dbc_seas','boc_seas','bcs_seas','mxx_seas'  &! 15 
       /),(/nmodes,nspecies/))
!          1          2         3         4           5           6   => 90 scalares


  CHARACTER(LEN=8),PARAMETER,DIMENSION(nmodes) :: numb_name=        (/ &
    'numb_akk','numb_acc','numb_dd1','numb_ds1','numb_dd2','numb_ds2', &
    'numb_ssa','numb_ssc','numb_sss','numb_occ','numb_bc1','numb_bc2', &
    'numb_bc3','numb_ocs','numb_dbc','numb_boc','numb_bcs','numb_mxx' /)


  CHARACTER(LEN=13),PARAMETER,DIMENSION(ninorg) :: inorg_name=        (/ &
    'mass_nitrato ','mass_ammonium','mass_water   ' /)

  
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
				   !=2, emission will be calculated during the model simulation (on-line emission)
  INTEGER,PARAMETER :: transport = 6 ! transported species
                                   !=1, yes
				   !=0, not

  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda, 5=offline emission, 6=transport])
  ! attention : for aerosols,  mode_alloc(ispc) = spc_alloc(transport,imode,ispc)
  INTEGER,PARAMETER,DIMENSION(6,nmodes,nspecies) :: spc_alloc=RESHAPE((/ &
    X , 1 , 1 , X , X , 1 , & ! mass_akk_sulf  t1
    X , 1 , 1 , X , X , 1 , & ! mass_acc_sulf  t2
    X , 1 , 1 , X , X , 1 , & ! mass_dd1_sulf  t3
    X , 1 , 1 , X , X , 1 , & ! mass_ds1_sulf  t4
    X , 1 , 1 , X , X , 1 , & ! mass_dd2_sulf  t5
    X , 1 , 1 , X , X , 1 , & ! mass_ds2_sulf  t6
    X , 1 , 1 , X , X , 1 , & ! mass_ssa_sulf  t7
    X , 1 , 1 , X , X , 1 , & ! mass_ssc_sulf  
    X , X , X , X , X , X , & ! mass_sss_sulf  
    X , 1 , 1 , X , X , 1 , & ! mass_occ_sulf  t8
    X , 1 , 1 , X , X , 1 , & ! mass_bc1_sulf  t9
    X , 1 , 1 , X , X , 1 , & ! mass_bc2_sulf  t10
    X , 1 , 1 , X , X , 1 , & ! mass_bc3_sulf  t11
    X , X , X , X , X , X , & ! mass_ocs_sulf 
    X , 1 , 1 , X , X , 1 , & ! mass_dbc_sulf  t12
    X , 1 , 1 , X , X , 1 , & ! mass_boc_sulf  t13
    X , 1 , 1 , X , X , 1 , & ! mass_bcs_sulf  t14
    X , 1 , 1 , X , X , 1 , & ! mass_mxx_sulf  t15 
        
    X , X , X , X , X , X , & ! mass_akk_bcar
    X , X , X , X , X , X , & ! mass_acc_bcar
    X , X , X , X , X , X , & ! mass_dd1_bcar
    X , X , X , X , X , X , & ! mass_ds1_bcar
    X , X , X , X , X , X , & ! mass_dd2_bcar
    X , X , X , X , X , X , & ! mass_ds2_bcar
    X , X , X , X , X , X , & ! mass_ssa_bcar
    X , X , X , X , X , X , & ! mass_ssc_bcar
    X , X , X , X , X , X , & ! mass_sss_bcar
    X , X , X , X , X , X , & ! mass_occ_bcar
    1 , 1 , 1 , X , X , 1 , & ! mass_bc1_bcar  t16
    X , 1 , 1 , X , X , 1 , & ! mass_bc2_bcar  t17
    X , 1 , 1 , X , X , 1 , & ! mass_bc3_bcar  t18
    X , X , X , X , X , X , & ! mass_ocs_bcar
    X , 1 , 1 , X , X , 1 , & ! mass_dbc_bcar  t19
    1 , 1 , 1 , X , X , 1 , & ! mass_boc_bcar  t20
    X , 1 , 1 , X , X , 1 , & ! mass_bcs_bcar  t21
    X , 1 , 1 , X , X , 1 , & ! mass_mxx_bcar  t22

    X , X , X , X , X , X , & ! mass_akk_ocar
    X , X , X , X , X , X , & ! mass_acc_ocar
    X , X , X , X , X , X , & ! mass_dd1_ocar
    X , X , X , X , X , X , & ! mass_ds1_ocar
    X , X , X , X , X , X , & ! mass_dd2_ocar
    X , X , X , X , X , X , & ! mass_ds2_ocar
    X , X , X , X , X , X , & ! mass_ssa_ocar
    X , X , X , X , X , X , & ! mass_ssc_ocar
    X , X , X , X , X , X , & ! mass_sss_ocar
    1 , 1 , 1 , X , X , 1 , & ! mass_occ_ocar  t23
    X , X , X , X , X , X , & ! mass_bc1_ocar 
    X , X , X , X , X , X , & ! mass_bc2_ocar
    X , X , X , X , X , X , & ! mass_bc3_ocar
    X , X , X , X , X , X , & ! mass_ocs_ocar
    X , X , X , X , X , X , & ! mass_dbc_ocar
    1 , 1 , 1 , X , X , 1 , & ! mass_boc_ocar  t24
    X , X , X , X , X , X , & ! mass_bcs_ocar
    X , 1 , 1 , X , X , 1 , & ! mass_mxx_ocar  t25
   					 
    X , X , X , X , X , X , & ! mass_akk_dust
    X , X , X , X , X , X , & ! mass_acc_dust
    X , 1 , 1 , X , X , 1 , & ! mass_dd1_dust  t26
    X , 1 , 1 , X , X , 1 , & ! mass_ds1_dust  t27
    X , 1 , 1 , X , X , 1 , & ! mass_dd2_dust  t28
    X , 1 , X , X , X , 1 , & ! mass_ds2_dust  t29
    X , X , X , X , X , X , & ! mass_ssa_dust
    X , X , X , X , X , X , & ! mass_ssc_dust
    X , X , X , X , X , X , & ! mass_sss_dust
    X , X , X , X , X , X , & ! mass_occ_dust
    X , X , X , X , X , X , & ! mass_bc1_dust
    X , X , X , X , X , X , & ! mass_bc2_dust
    X , X , X , X , X , X , & ! mass_bc3_dust
    X , X , X , X , X , X , & ! mass_ocs_dust
    X , 1 , 1 , X , X , 1 , & ! mass_dbc_dust  t30
    X , X , X , X , X , X , & ! mass_boc_dust
    X , X , X , X , X , X , & ! mass_bcs_dust
    X , 1 , 1 , X , X , 1 , & ! mass_mxx_dust  t31	    
    
    X , X , X , X , X , X , & ! mass_akk_seas
    X , X , X , X , X , X , & ! mass_acc_seas
    X , X , X , X , X , X , & ! mass_dd1_seas
    X , X , X , X , X , X , & ! mass_ds1_seas
    X , X , X , X , X , X , & ! mass_dd2_seas
    X , X , X , X , X , X , & ! mass_ds2_seas
    1 , 1 , 1 , X , X , 1 , & ! mass_ssa_seas  t32
    1 , 1 , 1 , X , X , 1 , & ! mass_ssc_seas  t33
    X , X , X , X , X , X , & ! mass_sss_seas
    X , X , X , X , X , X , & ! mass_occ_seas
    X , X , X , X , X , X , & ! mass_bc1_seas
    X , X , X , X , X , X , & ! mass_bc2_seas
    X , X , X , X , X , X , & ! mass_bc3_seas
    X , X , X , X , X , X , & ! mass_ocs_seas
    X , X , X , X , X , X , & ! mass_dbc_seas
    X , X , X , X , X , X , & ! mass_boc_seas
    X , X , X , X , X , X , & ! mass_bcs_seas
    X , 1 , 1 , X , X , 1   & ! mass_mxx_seas  t34
						      
    /),(/6,nmodes,nspecies/))




  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda, 5=offline emission, 6=transport])
  ! attention : for aerosols,  mode_alloc(ispc) = spc_alloc(transport,imode,ispc)
  INTEGER,PARAMETER,DIMENSION(6,nmodes) :: numb_alloc=reshape((/ &
    1 , 1 , 1 , X , X , 1 , & ! aer numb akk   t35
    1 , 1 , 1 , X , X , 1 , & ! aer numb acc   t36
    1 , 1 , 1 , X , X , 1 , & ! aer numb dd1   t37
    X , 1 , 1 , X , X , 1 , & ! aer numb ds1   t38
    1 , 1 , 1 , X , X , 1 , & ! aer numb dd2   t39
    X , 1 , 1 , X , X , 1 , & ! aer numb ds2   t40
    X , X , X , X , X , X , & ! aer numb ssa 
    X , X , X , X , X , X , & ! aer numb ssc 
    X , X , X , X , X , X , & ! aer numb sss 
    1 , 1 , 1 , X , X , 1 , & ! aer numb occ   t41
    1 , 1 , 1 , X , X , 1 , & ! aer numb bc1   t42
    X , 1 , 1 , X , X , 1 , & ! aer numb bc2   t43
    X , 1 , 1 , X , X , 1 , & ! aer numb bc3   t44
    X , X , X , X , X , X , & ! aer numb ocs   
    X , 1 , 1 , X , X , 1 , & ! aer numb dbc   t45
    1 , 1 , 1 , X , X , 1 , & ! aer numb boc   t46
    X , 1 , 1 , X , X , 1 , & ! aer numb bcs   t47
    X , 1 , 1 , X , X , 1   & ! aer numb mxx   t48
    !
    /),(/6,nmodes/))

  INTEGER,PARAMETER,DIMENSION(ninorg) :: inorg_mod_alloc=(/ &
   !------------------------------------------------------------------------------------
   !        nitrato ammonium water 
   !------------------------------------------------------------------------------------
             1 , 1 , 1 /)!   = 3 

  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda, 5=offline emission, 6=transport])
  ! attention : for aerosols,  mode_alloc(ispc) = spc_alloc(transport,imode,ispc)
  INTEGER,PARAMETER,DIMENSION(6,ninorg) :: inorg_alloc=reshape((/ &
    X , 1 , 1 , X , X , 1 , & ! mass_nitrate   t49
    X , 1 , 1 , X , X , 1 , & ! mass_ammonium  t50
    X , 1 , 1 , X , X , 1   & ! mass_water     t51
    /),(/6,ninorg/))

   INTEGER,PARAMETER,DIMENSION(nmodes,nspecies) :: aer12matrix=RESHAPE((/ &
         ! 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18
           04,06,08,11,14,17,20,00,00,26,29,32,35,00,38,42,46,49,&! SULF
           00,00,00,00,00,00,00,00,00,00,30,33,36,00,39,43,47,50,&! BCAR
           00,00,00,00,00,00,00,00,00,27,00,00,00,00,00,44,00,51,&! OCAR
           00,00,09,12,15,18,00,00,00,00,00,00,00,00,40,00,00,52,&! DUST
           00,00,00,00,00,00,21,24,00,00,00,00,00,00,00,00,00,53 &! SEAS 
         /),(/nmodes,nspecies/))

   INTEGER,PARAMETER, DIMENSION(ninorg) :: aer1_inorg2matrix=(/1,2,3/)
   !                                                     01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18  
   INTEGER,PARAMETER, DIMENSION(nmodes) :: aer22matrix=(/05,07,10,13,16,19,00,00,00,28,31,34,37,00,41,45,48,54/)

!A total of 51 transported variables:
! 34 (aerosol mass)   + 
! 14 (aerosol number) + 
!  3 inorganic aerosols (nitrate, ammonium and water) 

! - end of MATRIX relevant parameters
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------
!----- the section below is DUMMY and only is used by the SIMPLE aerosol model.
!----- it must be keept in this aer1_list file for consistency with the code for the SIMPLE aerosol model.

  !Number of each specie
  INTEGER,PARAMETER :: sdust=-99
  INTEGER,PARAMETER :: bburn=-99
  INTEGER,PARAMETER :: urban=-99
  INTEGER,PARAMETER :: bioge=-99
  INTEGER,PARAMETER :: marin=-99
  INTEGER,PARAMETER :: v_ash=-99
  INTEGER,PARAMETER :: nucle = -99  
  INTEGER,PARAMETER :: accum = -99  
  INTEGER,PARAMETER :: coarse= -99

  ! effective particle radius (meter)
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_radius=RESHAPE((/ &
!------------------------------------------------------------------------------------------------------------------
!-bin size  1     2     3     4    5     6      7   8      9     10   11    12    13   14    15    16    17   18
!------------------------------------------------------------------------------------------------------------------
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! sdust
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! bburn 
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! urban
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! bioge
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! marin
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999.  & ! v_ash
!---------------------------------------------------------- ---------------------------- ----------------------------
    /),(/nmodes,nspecies/))

  ! particle density kg/m^3
  !srf-dez2013: changing density for ash from 2500 to 900 kg/m3
  REAL,PARAMETER,DIMENSION(nmodes,nspecies) :: part_dens=RESHAPE((/ &
!------------------------------------------------------------------------------------------------------------------
!-bin size  1     2     3     4    5     6      7   8      9     10   11    12    13   14    15    16    17   18
!------------------------------------------------------------------------------------------------------------------
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! sdust
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! bburn 
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! urban
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! bioge
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999., & ! marin
    999., 999., 999., 999., 999., 999., 999., 999., 999., 999.,999., 999., 999., 999., 999., 999., 999., 999.  & ! v_ash
!---------------------------------------------------------- ---------------------------- ----------------------------
    /),(/nmodes,nspecies/))


END MODULE aer1_list
