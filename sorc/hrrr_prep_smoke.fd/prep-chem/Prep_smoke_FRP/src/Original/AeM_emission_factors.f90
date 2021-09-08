!################################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                       !
!  Version 1.8.3: set/2017                                                       !
!  Coded by Saulo Freitas and Karla Longo; updated by Daniela Franca             !
!  Contact: brams support: brams_help@cptec.inpe.br - http://brams.cptec.inpe.br !
!################################################################################!

module AeM_emission_factors
! ------------------------------------------------
! Indices for the emission-factors matrix data
! ------------------------------------------------
! Column - Bioma
! ------------------------------------------------

integer	:: &
	TropFor       = 01, & ! TropFor
	ExtratropF    = 02, & ! ExtratropF 
	Savanna       = 03, & ! Savanna
	pasture       = 04, & ! pasture/cropland       
	Biofuel       = 05, & ! Biofuel       
	CharcMak      = 06, & ! CharcMak
	CharcBurn     = 07, & ! CharcBurn
	AgResid       = 08, & ! AgResid
	Laboratory    = 09, & ! Laboratory
        Molec_Weight  = 10, & ! Molec. Weight
	
! ------------------------------------------------
! Line - Gas
! ------------------------------------------------
	
	CO2    = 01, &        !CO2
	CO     = 02, &        !CO
	CH4    = 03, &        !CH4
	NHMC   = 04, &        !NHMC
	
	C2H2   = 05, &        !C2H2
	C2H4   = 06, &        !C2H4
	C2H6   = 07, &        !C2H6
	C3H4   = 08, &        !C3H4
	C3H6   = 09, &        !C3H6
	C3H8   = 10, &        !C3H8
	
    	butene_1     = 11, &  !1-butene
	butene_i     = 12, &  !i-butene
	butene_tr_2  = 13, &  !tr-2-butene
	butene_cis_2 = 14, &  !cis-2-butene
	butadiene    = 15, &  !butadiene
	butane_n     = 16, &  !n-butane
	butane_i     = 17, &  !i-butane
	
    	pentene_1    = 18, &  !1-pentene
	pentene_2    = 19, &  !2-pentene (cis&trans)
	pentane_n    = 20, &  !n-pentane
	Butene_2_Me  = 21, &  !2-Me-Butene
	butane_2_Me  = 22, &  !2-Me-butane
	pentadienes  = 23, &  !pentadienes
	Isoprene     = 24, &  !Isoprene
	
    	cyclopentene	= 25, & !cyclopentene
	cyclopentadiene = 26, & !cyclopentadiene
	pentene_4_me_1  = 27, & !4-me-1-pentene
	pentene_2_me_1  = 28, & !2-me-1-pentene
	
        hexene_1    = 29, &   !1-hexene
	hexadienes  = 30, &   !hexadienes
	hexane_n    = 31, &   !n-hexane
	isohexanes  = 32, &   !isohexanes
	
        heptane       = 33, & !heptane
	octenes       = 34, & !octenes
	terpenes      = 35, & !terpenes
	benzene       = 36, & !benzene
	toluene       = 37, & !toluene
	xylenes       = 38, & !xylenes
	ethylbenzene  = 39, & !ethylbenzene
	styrene       = 40, & !styrene
	PAH	      = 41, & !PAH
	
        Methanol       = 42, & !Methanol
	Ethanol        = 43, & !Ethanol
	Propanol_1     = 44, & !1-Propanol
	propanol_2     = 45, & !2-propanol
	Butanols       = 46, & !Butanols
	cyclopentanol  = 47, & !cyclopentanol
	phenol         = 48, & !phenol
	
        Formaldehyde	    = 49, &!Formaldehyde
	Acetald 	    = 50, &!Acetald
	Hydroxyacetaldehyde = 51, &!Hydroxyacetaldehyde
	Acrolein	    = 52, &!Acrolein
	
        Propanal  = 53, &     !Propanal
	Butanals  = 54, &     !Butanals
	Hexanals  = 55, &     !Hexanals
	Heptanals = 56, &     !Heptanals
	
        Acetone 	= 57, &  !Acetone
	Butanone_2	= 58, &  !2-Butanone
	Butanedione_2_3 = 59, &  !2,3-Butanedione
	Pentanones	= 60, &  !Pentanones
	Hexanones	= 61, &  !Hexanones
	Heptanones	= 62, &  !Heptanones
	Octanones	= 63, &  !Octanones
	
        Benzaldehyde	 = 64, &  !Benzaldehyde   
	Furan		 = 65, &  !Furan
	Me_Furan_2       = 66, &  !2-Me-Furan
	Me_Furan_3	 = 67, &  !3-Me-Furan
	ethylfuran_2     = 68, &  !2-ethylfuran
	dime_furan_2_4   = 69, &  !2,4-dime-furan
	Dime_furan_2_5   = 70, &  !2,5-Dime-furan
	Tetrahydrofuran  = 71, &  !Tetrahydrofuran
	dihydrofuran_2_3 = 72, &  !2,3-dihydrofuran
	benzofuran	 = 73, &  !benzofuran
	Furfural	 = 74, &  !Furfural
	
        Me_format        = 75, &  !Me-format
	Me_Acetate       = 76, &  !Me-Acetate
	Acetonitrile     = 77, &  !Acetonitrile
	Acrylonitrile    = 78, &  !Acrylonitrile
	Propionitrile    = 79, &  !Propionitrile
	
        pyrrole 	  = 80, & !pyrrole
	trimethylpyrazole = 81, & !trimethylpyrazole
	methylamine	  = 82, & !methylamine
	dimethylamine	  = 83, & !dimethylamine
	ethylamine	  = 84, & !ethylamine
	trimethylamine    = 85, & !trimethylamine
	n_pentylamine	  = 86, & !n-pentylamine
	me_1_butylamine_2 = 87, & !2-me-1-butylamine
	
        HFo	  = 88, &  !HFo
	HAc	  = 89, &  !HAc
	Propanoic = 90, &  !Propanoic acid
	H2	  = 91, &  !H2
	NOx	  = 92, &  !NOx
	NOy	  = 93, &  !NOy
	
        N2O       = 94, &  !N2O
	NH3       = 95, &  !NH3
	HCN       = 96, &  !HCN
	cyanogen  = 97, &  !cyanogen
	N2	  = 98, &  !N2
	SO2	  = 99, &  !SO2
	DMS	  =100, &  !DMS
	
        COS_	  =101, &  !COS
	CH3Cl	  =102, &  !CH3Cl
	CH3Br	  =103, &  !CH3Br
	CH3I	  =104, &  !CH3I   
        Hg	  =105, &  !Hg

	BBURN2	  =106, &  !PM2.5
		  
    BBURN3	  =107, &  !old - TPM

	TC	  =108, &  !TC
	OC	  =109, &  !OC
	BC	  =110	   !BC
!---------------------------------------------------------------------------------------------------
integer, parameter :: AeM_nspecies=110
character(LEN=20),dimension(AeM_nspecies),parameter :: AeM_spc_name= &
! avoid tab character
!'12345678901234567890'
(/                     &
 'CO2                 '&     
,'CO                  '&     
,'CH4                 '&     
,'NHMC                '&     
,'C2H2                '&     
,'C2H4                '&     
,'C2H6                '&     
,'C3H4                '&     
,'C3H6                '&     
,'C3H8                '&     
,'1_butene            '&  
,'i-butene            '&  
,'tr_2_butene         '&  
,'cis_2_butene        '&  
,'butadiene           '&  
,'n_butane            '&  
,'i-butane            '&  
,'1_pentene           '&  
,'2_pentene           '&  
,'n_pentane           '&  
,'2_Me_Butene         '&  
,'2_Me_butane         '&  
,'pentadienes         '&  
,'Isoprene            '&  
,'cyclopentene        '&   
,'cyclopentadiene     '&   
,'4_me_1_pentene      '&   
,'2_me_1_pentene      '&   
,'1_hexene            '&  
,'hexadienes          '&  
,'n_hexane            '&  
,'isohexanes          '&  
,'heptane             '&	 
,'octenes             '&	 
,'terpenes            '&	 
,'benzene             '&	 
,'toluene             '&	 
,'xylenes             '&	 
,'ethylbenzene        '&   
,'styrene             '&   
,'PAH                 '&   
,'Methanol            '&
,'Ethanol             '&
,'1_Propanol          '&
,'2_propanol          '&
,'Butanols            '&  
,'cyclopentanol       '&
,'phenol              '&
,'Formaldehyde        '& 
,'Acetald             '& 
,'Hydroxyacetaldehyde '& 
,'Acrolein            '& 
,'Propanal            '&    
,'Butanals            '&    
,'Hexanals            '&    
,'Heptanals           '&    
,'Acetone             '&
,'2_Butanone          '&
,'2_3_Butanedione     '&
,'Pentanones          '&
,'Hexanones           '&
,'Heptanones          '&
,'Octanones           '&
,'Benzaldehyde        '&
,'Furan               '&
,'2_Me_Furan          '&
,'3_Me_Furan          '&
,'2_ethylfuran        '&
,'2_4_dime_furan      '&
,'2_5_Dime_furan      '&
,'Tetrahydrofuran     '&
,'2_3_dihydrofuran    '&
,'benzofuran          '&
,'Furfural            '&
,'Me_format           '&
,'Me_Acetate          '&
,'Acetonitrile        '&
,'Acrylonitrile       '&
,'Propionitrile       '&
,'pyrrole             '&
,'trimethylpyrazole   '&
,'methylamine         '&
,'dimethylamine       '&
,'ethylamine          '&
,'trimethylamine      '&
,'n_pentylamine       '&
,'2_me_1_butylamine   '&
,'HFo                 '&
,'HAc                 '&
,'Propanoic           '&
,'H2                  '&
,'NOx                 '&
,'NOy                 '&
,'N2O                 '&
,'NH3                 '&
,'HCN                 '&
,'cyanogen            '&
,'N2                  '&
,'SO2                 '&
,'DMS                 '&
,'COS                 '&
,'CH3Cl               '&
,'CH3Br               '&
,'CH3I                '&
,'Hg                  '&
,'BBURN2              '&  !PM25
,'BBURN3              '&  !PM10
,'TC                  '&
,'OC                  '&
,'BC                  '&
/) 	  	      

!---------------------------------------------------------------------------------------------------
real,    dimension(10,AeM_nspecies) :: emission_factor

data emission_factor/  &
!---------------------------------------------------------------------------------------------------
! Emission Factor
!---------------------------------------------------------------------------------------------------
! TropFor! Extra !Savanna! pasture | Biofuel! CharcMak ! CharcBurn ! AgResid ! Laboratory ! Molec. !
!	 !tropF  !	 ! cropland|	    !  	       !           !         !            ! Weight !
!---------------------------------------------------------------------------------------------------
  1624.,  1576., 1679.,    1679.,  1464.,     478.,       2573.,      1405.,   1476.,  44.01, & ! CO2
  103.,   112.,  57.,	   57.,    84.,       84.,        220.,       109.,     89.,   28.01, & ! CO
  6.8,    5.1,   2.0,	   2.0,    6.6,       12.2,       6.7,        10.3,     4.5,   16.04, & ! CH4
  7.0,    8.7,   4.8,	   4.8,    7.7,       17.5,       4.6,        11.2,    8.7,    15.00, & ! NMHC  									     
  0.197,  0.289, 0.370,    0.370,  0.637,     0.350,      0.245,      0.200,   0.308,  26.04, & ! C2H2
  1.174,  1.570, 0.796,    0.796,  1.138,     1.092,      0.583,      1.140,   1.095,  28.05, & ! C2H4
  0.817,  0.813, 0.477,    0.477,  0.728,     2.137,      0.655,      1.800,   1.065,  30.07, & ! C2H6
  0.013,  0.051, 0.019,    0.019,  0.290,     -9999,      0.071,      0.030,   -9999,  40.06, & ! C3H4
  0.568,  0.699, 0.451,    0.451,  0.649,     1.012,      0.399,      0.840,   0.671,  42.08, & ! C3H6
  0.536,  0.310, 0.137,    0.137,  0.281,     0.350,      0.207,      0.403,   0.151,  44.10, & ! C3H8
  0.073,  0.125, 0.069,    0.069,  0.171,     -9999,      0.113,      0.106,   -9999,  56.11, & ! 1-butene     
  0.109,  0.096, 0.054,    0.054,  0.165,     -9999,      0.087,      0.070,   0.410,  56.11, & ! i-butene
  0.033,  0.041, 0.027,    0.027,  0.202,     -9999,      0.037,      0.033,   0.040,  56.11, &! tr-2-butene
  0.031,  0.041, 0.021,    0.021,  0.110,     -9999,      0.023,      0.039,   0.026,  56.11, &! cis-2-butene
  0.094,  0.111, 0.073,    0.073,  0.168,     -9999,      0.062,      0.077,   0.136,  54.09, &! butadiene
  0.041,  0.094, 0.020,    0.020,  0.038,     -9999,      0.066,      0.047,   0.040,  58.12, &! n-butane
  0.015,  0.036, 0.006,    0.006,  0.047,     -9999,      0.010,      0.013,   -9999,  58.12, &! i-butane    
  0.056,  0.042, 0.022,    0.022,  0.135,     -9999,      0.028,      0.007,   0.011,  70.13, &! 1-pentene
  0.012,  0.030, 0.007,    0.007,  0.008,     -9999,      0.023,      0.009,   0.014,  70.13, &! 2-pentene (cis&trans)
  0.014,  0.044, 0.006,    0.006,  0.020,     -9999,      0.096,      0.022,   0.030,  72.15, &! n-pentane
  0.074,  0.077, 0.033,    0.033,  0.096,     -9999,      0.015,      0.006,   0.013,  70.13, &! 2-Me-Butene
  0.008,  0.027, 0.004,    0.004,  0.041,     -9999,      0.071,      0.015,   0.004,  72.15, &! 2-Me-butane
  -9999,  0.024, -9999,    -9999,  0.019,     -9999,      -9999,      -9999,   0.024,  -9999, &! pentadienes
  0.218,  0.107, 0.010,    0.010,  0.099,     -9999,      0.017,      0.750,   0.044,  68.12, &! Isoprene
  0.019,  0.041, 0.012,    0.012,  0.308,     -9999,      0.035,      0.016,   0.006,  68.12, &! cyclopentene
  0.027,  0.027, 0.016,    0.016,  0.061,     -9999,      0.052,      0.022,   0.023,  66.11, &! cyclopentadiene
  0.048,  0.052, 0.050,    0.050,  0.015,     -9999,      0.098,      0.041,   -9999,  84.16, &! 4-me-1-pentene
  -9999,  0.072, 0.004,    0.004,  -9999,     -9999,      -9999,      -9999,   -9999,  84.16, &! 2-me-1-pentene
  0.063,  0.086, 0.037,    0.037,  0.020,     -9999,      0.141,      0.059,   0.026,  84.16, &! 1-hexene
  -9999,  -9999, -9999,    -9999,  -9999,     -9999,      -9999,      -9999,   0.065,  -9999, &! hexadienes
  0.048,  0.043, 0.027,    0.027,  0.008,     -9999,      0.063,      0.039,   0.007,  86.17, &! n-hexane
  0.076,  0.042, 0.046,    0.046,  0.161,     -9999,      0.149,      0.063,   -9999,  86.17, &! isohexanes
  0.032,  0.035, 0.019,    0.019,  0.006,     -9999,      0.063,      0.026,   -9999,  98.17, &! heptane
  0.012,  0.031, 0.006,    0.006,  0.007,     -9999,      0.017,      0.007,   0.018,  112.22,&! octenes
  0.150,  2.003, 0.015,    0.015,  0.114,     -9999,      0.000,      0.015,   1.400,  136.24,&! terpenes
  0.364,  0.476, 0.250,    0.250,  0.878,     -9999,      1.124,      0.530,   0.355,  78.11, &! benzene
  0.238,  0.319, 0.130,    0.130,  0.494,     -9999,      0.380,      0.390,   0.319,  90.10, &! toluene
  0.109,  0.177, 0.048,    0.048,  0.234,     -9999,      0.148,      0.260,   0.087,  106.17,&! xylenes
  0.057,  0.044, 0.022,    0.022,  0.124,     -9999,      0.048,      0.150,   0.026,  106.17,&! ethylbenzene
  0.028,  0.066, 0.023,    0.023,  0.224,     -9999,      0.133,      0.028,   0.018,  104.15,&! styrene
  0.100,  0.019, 0.0024,   0.0024, 0.151,     -9999,      0.025,      0.100,   0.0640, -9999, &! PAH
  2.950,  2.073, 1.470,    1.470,  3.493,     12.300,     1.240,      5.660,   1.359,  32.04, &! Methanol
  0.017,  0.053, 0.010,    0.010,  0.013,     -9999,      0.033,      0.014,   0.097,  46.07, &! Ethanol
  0.041,  0.042, 0.025,    0.025,  0.030,     -9999,      0.080,      0.034,   0.248,  60.10, &! 1-Propanol
  -9999,  -9999, -9999,    -9999,  -9999,     -9999,      -9999,      -9999,   0.210,  -9999, &! 2-propanol
  0.009,  0.012, 0.008,    0.008,  0.051,     -9999,      0.022,      0.009,   0.025,  74.12, &! Butanols
  0.031,  0.044, 0.033,    0.033,  0.032,     -9999,      0.084,      0.035,   -9999,  86.13, &! cyclopentanol
  0.450,  0.252, 0.003,    0.003,  0.932,     2.750,      1.659,      1.170,   1.912,  94.11, &! phenol
  1.670,  2.144, 0.458,    0.458,  0.864,     1.060,      0.733,      1.750,   1.590,  30.03, &! Formaldehyde
  1.550,  1.072, 0.500,    0.500,  0.362,     -9999,      1.278,      2.770,   0.831,  45.06, &! Acetald
  0.961,  0.390, 0.961,    0.961,  0.660,     -9999,      0.961,      0.961,   0.961,  60.05, &! Hydroxyacetaldehyde
  0.650,  0.349, 0.075,    0.075,  0.052,     -9999,      0.347,      1.160,   0.372,  56.06, &! Acrolein
  0.100,  0.122, 0.008,    0.008,  0.072,     -9999,      0.145,      0.180,   0.545,  58.08, &! Propanal
  0.180,  0.141, 0.055,    0.055,  0.030,     -9999,      0.202,      0.320,   0.021,  72.11, &! Butanals
  0.010,  0.055, 0.014,    0.014,  0.063,     -9999,      0.091,      0.030,   0.003,  102.17,&! Hexanals
  0.003,  0.004, 0.003,    0.003,  0.374,     -9999,      0.008,      0.003,   0.004,  114.19,&! Heptanals
  0.630,  0.655, 0.517,    0.517,  0.324,     0.175,      1.355,      1.130,   0.726,  64.13, &! Acetone
  0.500,  0.231, 0.250,    0.250,  0.084,     -9999,      0.832,      0.900,   0.285,  72.11, &! 2-Butanone
  0.730,  0.764, 0.544,    0.544,  0.013,     -9999,      1.750,      1.310,   1.033,  86.08, &! 2,3-Butanedione
  0.090,  0.054, 0.016,    0.016,  0.029,     -9999,      0.092,      0.200,   0.038,  86.13, &! Pentanones
  -9999,  -9999, -9999,    -9999,  -9999,     -9999,      -9999,      -9999,   0.007,  -9999, &! Hexanones
  0.002,  0.006, 0.006,    0.006,  0.004,     -9999,      0.011,      0.005,   0.117,  114.19,&! Heptanones
  0.019,  0.023, 0.015,    0.015,  0.016,     -9999,      0.043,      0.018,   0.005,  128.22,&! Octanones
  0.027,  0.132, 0.030,    0.030,  0.034,     -9999,      0.070,      0.029,   0.058,  106.12,&! Benzaldehyde									      
  0.329,  0.471, 0.371,    0.371,  0.414,     1.150,      0.863,      1.030,   0.934,  68.08, &! Furan
  0.327,  0.337, 0.248,    0.248,  0.177,     -9999,      0.460,      1.050,   0.550,  82.10, &! 2-Me-Furan
  0.080,  0.034, 0.008,    0.008,  0.013,     -9999,      0.061,      0.150,   0.063,  82.10, &! 3-Me-Furan
  0.003,  0.016, 0.001,    0.001,  0.003,     -9999,      0.007,      0.003,   -9999,  94.10, &! 2-ethylfuran
  0.024,  0.019, 0.008,    0.008,  0.003,     -9999,      0.037,      0.015,   -9999,  96.13, &! 2,4-dime-furan      
  0.028,  0.070, 0.002,    0.002,  0.035,     -9999,      0.054,      0.023,   0.289,  96.13, &! 2,5-Dime-furan
  0.016,  0.001, 0.016,    0.016,  0.016,     -9999,      0.042,      0.018,   0.002,  72.12, &! Tetrahydrofuran
  0.013,  0.018, 0.013,    0.013,  0.013,     -9999,      0.033,      0.014,   -9999,  70.10, &! 2,3-dihydrofuran    
  0.015,  0.094, 0.014,    0.014,  0.016,     -9999,      0.042,      0.017,   0.014,  118.14,&! benzofuran
  0.384,  0.616, 0.233,    0.233,  0.129,     1.049,      0.749,      0.315,   2.054,  96.09, &! Furfural
  0.024,  0.024, 0.014,    0.014,  0.018,     -9999,      0.047,      0.020,   0.043,  60.05, &! Me-format
  0.095,  0.094, 0.053,    0.053,  0.105,     -9999,      0.186,      0.078,   0.112,  74.05, &! Me-Acetate
  0.410,  0.230, 0.198,    0.198,  0.179,     -9999,      0.180,      0.740,   0.173,  41.05, &! Acetonitrile
  0.040,  0.034, -9999.,   -9999.,  0.030,    -9999,      0.000,       0.080,  0.040,  53.10, &! Acrylonitrile
  0.090, -9999., -9999.,   -9999., -9999.,    -9999,      0.000,       0.170,  0.020,  55.07, &! Propionitrile
  0.120,  0.012, -9999.,   -9999., -9999.,    -9999,      0.000,       0.220,  0.017,  67.09, &! pyrrole
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.124,  110.16,&! trimethylpyrazole
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.057,  31.06, &! methylamine  
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.062,  45.09, &! dimethylamine        
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.010,  45.09, &! ethylamine   
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.041,  59.11, &! trimethylamine       
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.439,  87.17, &! n-pentylamine        
  -9999., -9999.,-9999.,   -9999., -9999.,    -9999,      0.000,      -9999.,  0.140,  87.17, &! 2-me-1-butylamine    
  0.570,  1.545, 0.63,     0.63,   0.409,     0.450,      0.119,      0.460,   0.770,  47.02, &! HFo
  4.250,  3.369, 2.61,     2.61,   11.96,     8.920,      1.845,      9.950,   2.813,  60.05, &! HAc
  -9999,  -9999, -9999,    -9999,  -9999,     -9999,      -9999,      -9999,   0.560,  -9999, &! Propanoic acid
  3.30,   1.80,  0.75,     0.75,   1.762,     -9999,      4.670,      1.966,   -9999., 2.02,  &! H2
  1.67,   2.77,  1.07,     1.07,   1.74,      0.05,       2.32,       1.35,    1.87,   30.01, &! NOx
  -9999., 0.67,  2.20,     2.20,   -9999.,    0.00,       0.00,       -9999.,  0.00,   0.00,  &! NOy
  0.20,   0.25,  0.21,     0.21,   0.13,      0.02,       0.35,       0.10,    0.06,   44.01, &! N2O
  1.10,   1.31,  0.74,     0.74,   1.60,      0.23,       0.80,       1.30,    1.61,   18.02, &! NH3
  0.66,   0.77,  0.23,     0.23,   0.30,      0.30,       0.30,       0.54,    0.47,   27.02, &! HCN
  0.00,   -9999.,0.00,     0.00,   -9999.,    0.00,       0.00,       -9999.,  0.01,   52.04, &! cyanogen
  2.64,   2.64,  2.64,     2.64,   2.64,      -9999,      2.64,       2.64,    4.13,   28.01, &! N2
  0.58,   1.36,  0.44,     0.44,   0.29,      0.00,       0.05,       0.40,    0.87,   64.06, &! SO2
  0.0022, 0.0141, 0.0011,   0.0011, 0.100,    -9999,      -9999,      -9999,   -9999,  0.00,  &! DMS
  0.025,  0.045, 0.015,    0.015,  0.040,     0.040,      0.040,      0.060,   0.032,  60.07, &! COS
  0.020,  0.040, 0.055,    0.055,  0.039,     0.010,      0.012,      0.228,   0.158,  50.49, &! CH3Cl
  0.0053, 0.0029, 0.0021,  0.0021, 0.0030,    0.0030,     0.0030,     0.0030,  0.0008, 94.94, &! CH3Br
  0.0068, 0.0005, 0.0004,  0.0004, 0.0010,    -9999,      0.0010,     0.0010,  -9999., 141.94,&! CH3I
  5.E-05, 1.3E-04, 0.E+0,  0.E+0, 6.8E-06,    0.E-05,     5.E-05,     5.E-05,  3.7E-5, 200.59,&! Hg 
  9.4,    15.7,  4.0,	   4.0,    6.0,       2.1,        1.6,        10.7,    5.1,    -9999, &! PM2.5
  11.8,   17.6,  6.3,	   6.3,    6.9,       4.3,        2.4,        23.4,    7.2,    -9999, &! PM10 (old TPM)
  6.0,    8.3,   3.5,	   3.5,    3.4,       -9999,      3.3,        3.4,     3.6,    12.00, &! TC
  5.2,    7.7,   3.2,	   3.2,    3.1,       -9999,      4.8,        3.3,     2.5,    12.00, &! OC
  0.70,   0.58,  0.38,     0.38,   0.54,      -9999,      1.50,       0.69,    1.08,   12.00 / ! BC


!index_bioma =2
!index_gas=2
!print*, emission_factor(index_bioma, index_gas)
!print*,spc_name(index_gas)

end module AeM_emission_factors
