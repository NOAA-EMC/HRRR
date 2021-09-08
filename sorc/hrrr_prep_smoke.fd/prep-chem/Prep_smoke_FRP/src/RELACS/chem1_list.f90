MODULE chem1_list
  IMPLICIT NONE
  
  
  CHARACTER(LEN=24),PARAMETER :: chemical_mechanism='RELACS'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=047
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
   'O3  ' & !
     ,'H2O2' & !
     ,'NO  ' & !
     ,'NO2 ' & !
     ,'NO3 ' & !
     ,'N2O5' & !
     ,'HONO' & !
     ,'HNO3' & !
     ,'HNO4' & !
     ,'SO2 ' & !
     ,'SULF' & !
     ,'CO  ' & !
     ,'CO2 ' & !
     ,'N2  ' & !
     ,'O2  ' & !
     ,'H2O ' & !
     ,'H2  ' & !
     ,'O3P ' & !
     ,'O1D ' & !
     ,'HO  ' & !
     ,'HO2 ' & !
     ,'CH4 ' & !
     ,'ETH ' & !
     ,'ALKA' & !
     ,'ALKE' & !
     ,'BIO ' & !
     ,'ARO ' & !
     ,'HCHO' & !
     ,'ALD ' & !
     ,'KET ' & !
     ,'CRBO' & !
     ,'ONIT' & !
     ,'PAN ' & !
     ,'OP1 ' & !
     ,'OP2 ' & !
     ,'ORA1' & !
     ,'ORA2' & !
     ,'MO2 ' & !
     ,'AKAP' & !
     ,'AKEP' & !
     ,'BIOP' & !
     ,'PHO ' & !
     ,'ADD ' & !
     ,'AROP' & !
     ,'CBOP' & !
     ,'OLN ' & !
     ,'XO2 ' & !
   /)
  
  
  !Number of each specie   
  INTEGER,PARAMETER :: O3  =001
  INTEGER,PARAMETER :: H2O2=002
  INTEGER,PARAMETER :: NO  =003
  INTEGER,PARAMETER :: NO2 =004
  INTEGER,PARAMETER :: NO3 =005
  INTEGER,PARAMETER :: N2O5=006
  INTEGER,PARAMETER :: HONO=007
  INTEGER,PARAMETER :: HNO3=008
  INTEGER,PARAMETER :: HNO4=009
  INTEGER,PARAMETER :: SO2 =010
  INTEGER,PARAMETER :: SULF=011
  INTEGER,PARAMETER :: CO  =012
  INTEGER,PARAMETER :: CO2 =013
  INTEGER,PARAMETER :: N2  =014
  INTEGER,PARAMETER :: O2  =015
  INTEGER,PARAMETER :: H2O =016
  INTEGER,PARAMETER :: H2  =017
  INTEGER,PARAMETER :: O3P =018
  INTEGER,PARAMETER :: O1D =019
  INTEGER,PARAMETER :: HO  =020
  INTEGER,PARAMETER :: HO2 =021
  INTEGER,PARAMETER :: CH4 =022
  INTEGER,PARAMETER :: ETH =023
  INTEGER,PARAMETER :: ALKA=024
  INTEGER,PARAMETER :: ALKE=025
  INTEGER,PARAMETER :: BIO =026
  INTEGER,PARAMETER :: ARO =027
  INTEGER,PARAMETER :: HCHO=028
  INTEGER,PARAMETER :: ALD =029
  INTEGER,PARAMETER :: KET =030
  INTEGER,PARAMETER :: CRBO=031
  INTEGER,PARAMETER :: ONIT=032
  INTEGER,PARAMETER :: PAN =033
  INTEGER,PARAMETER :: OP1 =034
  INTEGER,PARAMETER :: OP2 =035
  INTEGER,PARAMETER :: ORA1=036
  INTEGER,PARAMETER :: ORA2=037
  INTEGER,PARAMETER :: MO2 =038
  INTEGER,PARAMETER :: AKAP=039
  INTEGER,PARAMETER :: AKEP=040
  INTEGER,PARAMETER :: BIOP=041
  INTEGER,PARAMETER :: PHO =042
  INTEGER,PARAMETER :: ADD =043
  INTEGER,PARAMETER :: AROP=044
  INTEGER,PARAMETER :: CBOP=045
  INTEGER,PARAMETER :: OLN =046
  INTEGER,PARAMETER :: XO2 =047
  
  
  !for memory allocattion: 
  !This parameters are use for documentation only. 
  !Use them in a program in substitution of numerical terms.
  INTEGER,PARAMETER :: src = 1 ! source term 
  INTEGER,PARAMETER :: ddp = 2 ! dry deposition 
  INTEGER,PARAMETER :: wdp = 3 ! wet deposition 
  INTEGER,PARAMETER :: fdda = 4! four-dim assimilation 
  INTEGER,PARAMETER :: offline = 5! ! off-line emissions: 
                                  !=1, emission will be read from file
                                  !=0, emission will be calculated during the model simulation (on-line emission)
  INTEGER,PARAMETER :: transport = 6! ! off-line emissions: 
                                  !=1, yes
                                  !=0, no transport
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0
  
  
  ! spaction(specie,[1=source,2=drydep,3=wetdep,4=fdda,5=offline emission,6=transport]) ]) 
  INTEGER,PARAMETER,DIMENSION(6,nspecies) :: spc_alloc=RESHAPE((/ &
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! O3 - 001
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! H2O2 - 002
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! NO - 003
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! NO2 - 004
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! NO3 - 005
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! N2O5 - 006
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HONO - 007
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! HNO3 - 008
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HNO4 - 009
    1 , 1 , 1 , 0 , 0 , 1 ,   & ! SO2 - 010
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! SULF - 011
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CO - 012
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! CO2 - 013
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! N2 - 014
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O2 - 015
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! H2O - 016
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! H2 - 017
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O3P - 018
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O1D - 019
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HO - 020
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HO2 - 021
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CH4 - 022
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ETH - 023
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ALKA - 024
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ALKE - 025
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! BIO - 026
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ARO - 027
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! HCHO - 028
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ALD - 029
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! KET - 030
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! CRBO - 031
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! ONIT - 032
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! PAN - 033
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! OP1 - 034
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! OP2 - 035
    1 , 1 , 1 , 0 , 1 , 1 ,   & ! ORA1 - 036
    1 , 1 , 1 , 0 , 1 , 1 ,   & ! ORA2 - 037
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! MO2 - 038
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! AKAP - 039
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! AKEP - 040
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! BIOP - 041
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! PHO - 042
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ADD - 043
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! AROP - 044
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! CBOP - 045
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! OLN - 046
    0 , 0 , 0 , 0 , 0 , 0    & ! XO2 - 047
    /),(/6,nspecies/))
  
  
  INTEGER,PARAMETER,DIMENSION(nspecies) :: spc_uveq=(/ &
   00, & !O3
   00, & !H2O2
   00, & !NO
   00, & !NO2
   00, & !NO3
   00, & !N2O5
   00, & !HONO
   00, & !HNO3
   00, & !HNO4
   00, & !SO2
   00, & !SULF
   00, & !CO
   00, & !CO2
   00, & !N2
   00, & !O2
   00, & !H2O
   00, & !H2
   00, & !O3P
   00, & !O1D
   00, & !HO
   00, & !HO2
   00, & !CH4
   00, & !ETH
   00, & !ALKA
   00, & !ALKE
   00, & !BIO
   00, & !ARO
   00, & !HCHO
   00, & !ALD
   00, & !KET
   00, & !CRBO
   00, & !ONIT
   00, & !PAN
   00, & !OP1
   00, & !OP2
   00, & !ORA1
   00, & !ORA2
   00, & !MO2
   00, & !AKAP
   00, & !AKEP
   00, & !BIOP
   00, & !PHO
   00, & !ADD
   00, & !AROP
   00, & !CBOP
   00, & !OLN
   00  & !XO2
   /)
  
  
!     HENRYS LAW COEFFICIENTS
!     Henrys law coefficient
!     [KH298]=mole/(l atm)
!     Referencias em R. Sander (1999)
!     Compilation of Henry Law Constants 
!     for Inorganic and Organic Species 
!     of Potential Importance in 
!     Environmental Chemistry (Version 3) 
!     http://www.henrys-law.org 
!     * indica artigos nao encontrados nesse endereço eletronico
  REAL,PARAMETER,DIMENSION(nspecies) :: hstar=(/&
    1.10E-2              ,   & ! O3 - 001
    8.30E+4              ,   & ! H2O2 - 002
    1.90E-3              ,   & ! NO - 003
    1.20E-2              ,   & ! NO2 - 004
    6.1E-01              ,   & ! NO3 - 005
    2.1E+00              ,   & ! N2O5 - 006
    5.00E+1              ,   & ! HONO - 007
    2.10E+5              ,   & ! HNO3 - 008
    1.20E+4              ,   & ! HNO4 - 009
    1.40E+0              ,   & ! SO2 - 010
    2.10E+5              ,   & ! SULF - 011
    9.90E-4              ,   & ! CO - 012
    3.6E-02              ,   & ! CO2 - 013
    6.1E-04              ,   & ! N2 - 014
    1.3E-03              ,   & ! O2 - 015
    0.0E+00              ,   & ! H2O - 016
    7.8E-04              ,   & ! H2 - 017
    0.00E+0              ,   & ! O3P - 018
    0.00E+0              ,   & ! O1D - 019
    3.00E+1              ,   & ! HO - 020
    5.70E+3              ,   & ! HO2 - 021
    1.40E-3              ,   & ! CH4 - 022
    1.90E-3              ,   & ! ETH - 023
    1.00E-3              ,   & ! ALKA - 024
    5.00E-3              ,   & ! ALKE - 025
    2.80E-2              ,   & ! BIO - 026
    1.73E-1              ,   & ! ARO - 027
    3.20E+3              ,   & ! HCHO - 028
    1.40E+1              ,   & ! ALD - 029
    3.00E+1              ,   & ! KET - 030
    2.1E+05              ,   & ! CRBO - 031
    1.00E+0              ,   & ! ONIT - 032
    3.60E+0              ,   & ! PAN - 033
    3.10E+2              ,   & ! OP1 - 034
    3.40E+2              ,   & ! OP2 - 035
    8.90E+3              ,   & ! ORA1 - 036
    4.10E+3              ,   & ! ORA2 - 037
    2.00E+3              ,   & ! MO2 - 038
    0.0E+00              ,   & ! AKAP - 039
    0.0E+00              ,   & ! AKEP - 040
    0.0E+00              ,   & ! BIOP - 041
    0.0E+00              ,   & ! PHO - 042
    0.0E+00              ,   & ! ADD - 043
    0.0E+00              ,   & ! AROP - 044
    1.14E+1              ,   & ! CBOP - 045
    0.0E+00              ,   & ! OLN - 046
    0.0E+00                  & ! XO2 - 047
    /)
  
! [1] Noziere B. et al. The uptake of methyl vinyl ketone, methacrolein,
! and 2-methyl-3-butene-2-olonto sulfuric acid solutions,Journal of Physical
! Chemistry A, Vol.110, No.7, 2387-2395, 2006.
! [2] Abraham M. H. et al. Partition of compounds from gas to water and
! from gas to physiological saline at 310K: Linear free energy relationships,
! elsevier, 2006.
  REAL,PARAMETER,DIMENSION(nspecies) :: f0=(/&
    1.0 ,   & ! O3 - 001
    1.0 ,   & ! H2O2 - 002
    0.0 ,   & ! NO - 003
    0.1 ,   & ! NO2 - 004
    0.0 ,   & ! NO3 - 005
    0.0 ,   & ! N2O5 - 006
    0.0 ,   & ! HONO - 007
    0.0 ,   & ! HNO3 - 008
    0.0 ,   & ! HNO4 - 009
    0.0 ,   & ! SO2 - 010
    0.0 ,   & ! SULF - 011
    0.0 ,   & ! CO - 012
    0.0 ,   & ! CO2 - 013
    0.0 ,   & ! N2 - 014
    0.0 ,   & ! O2 - 015
    0.0 ,   & ! H2O - 016
    0.0 ,   & ! H2 - 017
    0.0 ,   & ! O3P - 018
    0.0 ,   & ! O1D - 019
    0.0 ,   & ! HO - 020
    0.0 ,   & ! HO2 - 021
    0.0 ,   & ! CH4 - 022
    0.0 ,   & ! ETH - 023
    0.0 ,   & ! ALKA - 024
    0.0 ,   & ! ALKE - 025
    0.0 ,   & ! BIO - 026
    0.0 ,   & ! ARO - 027
    0.0 ,   & ! HCHO - 028
    0.0 ,   & ! ALD - 029
    0.0 ,   & ! KET - 030
    0.0 ,   & ! CRBO - 031
    0.0 ,   & ! ONIT - 032
    0.1 ,   & ! PAN - 033
    0.1 ,   & ! OP1 - 034
    0.1 ,   & ! OP2 - 035
    0.0 ,   & ! ORA1 - 036
    0.0 ,   & ! ORA2 - 037
    0.0 ,   & ! MO2 - 038
    0.0 ,   & ! AKAP - 039
    0.0 ,   & ! AKEP - 040
    0.0 ,   & ! BIOP - 041
    0.0 ,   & ! PHO - 042
    0.0 ,   & ! ADD - 043
    0.0 ,   & ! AROP - 044
    0.0 ,   & ! CBOP - 045
    0.0 ,   & ! OLN - 046
    0.0     & ! XO2 - 047
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: difrat=(/&
    1.6 ,   & ! O3 - 001
    1.4 ,   & ! H2O2 - 002
    1.3 ,   & ! NO - 003
    1.6 ,   & ! NO2 - 004
    1.9 ,   & ! NO3 - 005
    2.4 ,   & ! N2O5 - 006
    1.6 ,   & ! HONO - 007
    1.9 ,   & ! HNO3 - 008
    2.1 ,   & ! HNO4 - 009
    1.9 ,   & ! SO2 - 010
    2.3 ,   & ! SULF - 011
    1.2 ,   & ! CO - 012
    1.6 ,   & ! CO2 - 013
    1.2 ,   & ! N2 - 014
    1.3 ,   & ! O2 - 015
    1.0 ,   & ! H2O - 016
    0.3 ,   & ! H2 - 017
    0.9 ,   & ! O3P - 018
    0.9 ,   & ! O1D - 019
    1.0 ,   & ! HO - 020
    1.4 ,   & ! HO2 - 021
    0.9 ,   & ! CH4 - 022
    1.3 ,   & ! ETH - 023
    1.9 ,   & ! ALKA - 024
    1.4 ,   & ! ALKE - 025
    1.9 ,   & ! BIO - 026
    2.3 ,   & ! ARO - 027
    1.3 ,   & ! HCHO - 028
    1.6 ,   & ! ALD - 029
    2.0 ,   & ! KET - 030
    2.0 ,   & ! CRBO - 031
    2.6 ,   & ! ONIT - 032
    2.6 ,   & ! PAN - 033
    1.6 ,   & ! OP1 - 034
    1.9 ,   & ! OP2 - 035
    1.6 ,   & ! ORA1 - 036
    1.8 ,   & ! ORA2 - 037
    1.6 ,   & ! MO2 - 038
    2.4 ,   & ! AKAP - 039
    2.2 ,   & ! AKEP - 040
    2.5 ,   & ! BIOP - 041
    2.4 ,   & ! PHO - 042
    2.4 ,   & ! ADD - 043
    2.9 ,   & ! AROP - 044
    2.2 ,   & ! CBOP - 045
    2.7 ,   & ! OLN - 046
    1.6     & ! XO2 - 047
    /)
  
  
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)
  REAL,PARAMETER,DIMENSION(nspecies) :: dvj=(/&
    0.1443400     ,   & ! O3 - 001
    0.1715000     ,   & ! H2O2 - 002
    0.1825700     ,   & ! NO - 003
    0.1474400     ,   & ! NO2 - 004
    0.1270000     ,   & ! NO3 - 005
    0.0962300     ,   & ! N2O5 - 006
    0.1458600     ,   & ! HONO - 007
    0.1259900     ,   & ! HNO3 - 008
    0.1125100     ,   & ! HNO4 - 009
    0.1250000     ,   & ! SO2 - 010
    0.1010200     ,   & ! SULF - 011
    0.1889800     ,   & ! CO - 012
    0.1507600     ,   & ! CO2 - 013
    0.1889800     ,   & ! N2 - 014
    0.1767800     ,   & ! O2 - 015
    0.2357000     ,   & ! H2O - 016
    0.7071100     ,   & ! H2 - 017
    0.2500000     ,   & ! O3P - 018
    0.2500000     ,   & ! O1D - 019
    0.2425400     ,   & ! HO - 020
    0.1740800     ,   & ! HO2 - 021
    0.2500000     ,   & ! CH4 - 022
    0.1825700     ,   & ! ETH - 023
    0.1273600     ,   & ! ALKA - 024
    0.1739100     ,   & ! ALKE - 025
    0.1212700     ,   & ! BIO - 026
    0.1010700     ,   & ! ARO - 027
    0.1825700     ,   & ! HCHO - 028
    0.1507600     ,   & ! ALD - 029
    0.1178500     ,   & ! KET - 030
    0.1206700     ,   & ! CRBO - 031
    0.0916700     ,   & ! ONIT - 032
    0.0902300     ,   & ! PAN - 033
    0.1443400     ,   & ! OP1 - 034
    0.1258400     ,   & ! OP2 - 035
    0.1474400     ,   & ! ORA1 - 036
    0.1291000     ,   & ! ORA2 - 037
    0.1458600     ,   & ! MO2 - 038
    0.0986900     ,   & ! AKAP - 039
    0.1063200     ,   & ! AKEP - 040
    0.0924500     ,   & ! BIOP - 041
    0.0966700     ,   & ! PHO - 042
    0.0966300     ,   & ! ADD - 043
    0.0812100     ,   & ! AROP - 044
    0.1080100     ,   & ! CBOP - 045
    0.0857500     ,   & ! OLN - 046
    0.1507600         & ! XO2 - 047
    /)
  
  
!     -DH/R (for temperature correction)
!     [-DH/R]=K
!     Referencias em R. Sander (1999)
!     Compilation of Henry Law Constants
!     for Inorganic and Organic Species 
!     of Potential Importance in 
!     Environmental Chemistry (Version 3)
!     http://www.henrys-law.org 
  REAL,PARAMETER,DIMENSION(nspecies) :: dhr=(/&
    2400.         ,   & ! O3 - 001
    7400.         ,   & ! H2O2 - 002
    1400.         ,   & ! NO - 003
    2500.         ,   & ! NO2 - 004
    2000.         ,   & ! NO3 - 005
    3400.         ,   & ! N2O5 - 006
    4900.         ,   & ! HONO - 007
    8700.         ,   & ! HNO3 - 008
    6900.         ,   & ! HNO4 - 009
    2900.         ,   & ! SO2 - 010
    0.            ,   & ! SULF - 011
    1300.         ,   & ! CO - 012
    2200.         ,   & ! CO2 - 013
    1300.         ,   & ! N2 - 014
    1500.         ,   & ! O2 - 015
    0.            ,   & ! H2O - 016
    500.          ,   & ! H2 - 017
    0.            ,   & ! O3P - 018
    0.            ,   & ! O1D - 019
    4500.         ,   & ! HO - 020
    5900.         ,   & ! HO2 - 021
    1600.         ,   & ! CH4 - 022
    2300.         ,   & ! ETH - 023
    2700.         ,   & ! ALKA - 024
    3000.         ,   & ! ALKE - 025
    0.            ,   & ! BIO - 026
    4045.         ,   & ! ARO - 027
    6800.         ,   & ! HCHO - 028
    5600.         ,   & ! ALD - 029
    4600.         ,   & ! KET - 030
    5300.         ,   & ! CRBO - 031
    5800.         ,   & ! ONIT - 032
    6500.         ,   & ! PAN - 033
    5200.         ,   & ! OP1 - 034
    6000.         ,   & ! OP2 - 035
    5700.         ,   & ! ORA1 - 036
    6300.         ,   & ! ORA2 - 037
    6600.         ,   & ! MO2 - 038
    0.            ,   & ! AKAP - 039
    0.            ,   & ! AKEP - 040
    0.            ,   & ! BIOP - 041
    0.            ,   & ! PHO - 042
    0.            ,   & ! ADD - 043
    0.            ,   & ! AROP - 044
    0.            ,   & ! CBOP - 045
    0.            ,   & ! OLN - 046
    0.                & ! XO2 - 047
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: weight=(/&
    48.  ,   & ! O3 - 001
    34.  ,   & ! H2O2 - 002
    30.  ,   & ! NO - 003
    46.  ,   & ! NO2 - 004
    62.  ,   & ! NO3 - 005
    108. ,   & ! N2O5 - 006
    47.  ,   & ! HONO - 007
    63.  ,   & ! HNO3 - 008
    79.  ,   & ! HNO4 - 009
    64.  ,   & ! SO2 - 010
    98.  ,   & ! SULF - 011
    28.  ,   & ! CO - 012
    44.  ,   & ! CO2 - 013
    28.  ,   & ! N2 - 014
    32.  ,   & ! O2 - 015
    18.  ,   & ! H2O - 016
    2.   ,   & ! H2 - 017
    16.  ,   & ! O3P - 018
    16.  ,   & ! O1D - 019
    17.  ,   & ! HO - 020
    33.  ,   & ! HO2 - 021
    16.  ,   & ! CH4 - 022
    30.  ,   & ! ETH - 023
    61.6 ,   & ! ALKA - 024
    33.0 ,   & ! ALKE - 025
    68.  ,   & ! BIO - 026
    97.9 ,   & ! ARO - 027
    30.  ,   & ! HCHO - 028
    44.  ,   & ! ALD - 029
    72.  ,   & ! KET - 030
    68.6 ,   & ! CRBO - 031
    119. ,   & ! ONIT - 032
    122. ,   & ! PAN - 033
    48.  ,   & ! OP1 - 034
    62.  ,   & ! OP2 - 035
    46.  ,   & ! ORA1 - 036
    60.  ,   & ! ORA2 - 037
    47.  ,   & ! MO2 - 038
    102. ,   & ! AKAP - 039
    88.4 ,   & ! AKEP - 040
    117. ,   & ! BIOP - 041
    107. ,   & ! PHO - 042
    107. ,   & ! ADD - 043
    151. ,   & ! AROP - 044
    85.4 ,   & ! CBOP - 045
    136. ,   & ! OLN - 046
    44.      & ! XO2 - 047
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: init_ajust=(/&
    1.0 ,   & ! O3 - 001
    1.0 ,   & ! H2O2 - 002
    1.0 ,   & ! NO - 003
    1.0 ,   & ! NO2 - 004
    1.0 ,   & ! NO3 - 005
    1.0 ,   & ! N2O5 - 006
    1.0 ,   & ! HONO - 007
    1.0 ,   & ! HNO3 - 008
    1.0 ,   & ! HNO4 - 009
    1.0 ,   & ! SO2 - 010
    1.0 ,   & ! SULF - 011
    1.0 ,   & ! CO - 012
    1.0 ,   & ! CO2 - 013
    1.0 ,   & ! N2 - 014
    1.0 ,   & ! O2 - 015
    1.0 ,   & ! H2O - 016
    1.0 ,   & ! H2 - 017
    1.0 ,   & ! O3P - 018
    1.0 ,   & ! O1D - 019
    1.0 ,   & ! HO - 020
    1.0 ,   & ! HO2 - 021
    1.0 ,   & ! CH4 - 022
    1.0 ,   & ! ETH - 023
    1.0 ,   & ! ALKA - 024
    1.0 ,   & ! ALKE - 025
    1.0 ,   & ! BIO - 026
    1.0 ,   & ! ARO - 027
    1.0 ,   & ! HCHO - 028
    1.0 ,   & ! ALD - 029
    1.0 ,   & ! KET - 030
    1.0 ,   & ! CRBO - 031
    1.0 ,   & ! ONIT - 032
    1.0 ,   & ! PAN - 033
    1.0 ,   & ! OP1 - 034
    1.0 ,   & ! OP2 - 035
    1.0 ,   & ! ORA1 - 036
    1.0 ,   & ! ORA2 - 037
    1.0 ,   & ! MO2 - 038
    1.0 ,   & ! AKAP - 039
    1.0 ,   & ! AKEP - 040
    1.0 ,   & ! BIOP - 041
    1.0 ,   & ! PHO - 042
    1.0 ,   & ! ADD - 043
    1.0 ,   & ! AROP - 044
    1.0 ,   & ! CBOP - 045
    1.0 ,   & ! OLN - 046
    1.0     & ! XO2 - 047
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: emiss_ajust=(/&
    1.0 ,   & ! O3 - 001
    1.0 ,   & ! H2O2 - 002
    1.0 ,   & ! NO - 003
    1.0 ,   & ! NO2 - 004
    1.0 ,   & ! NO3 - 005
    1.0 ,   & ! N2O5 - 006
    1.0 ,   & ! HONO - 007
    1.0 ,   & ! HNO3 - 008
    1.0 ,   & ! HNO4 - 009
    1.0 ,   & ! SO2 - 010
    1.0 ,   & ! SULF - 011
    1.0 ,   & ! CO - 012
    1.0 ,   & ! CO2 - 013
    1.0 ,   & ! N2 - 014
    1.0 ,   & ! O2 - 015
    1.0 ,   & ! H2O - 016
    1.0 ,   & ! H2 - 017
    1.0 ,   & ! O3P - 018
    1.0 ,   & ! O1D - 019
    1.0 ,   & ! HO - 020
    1.0 ,   & ! HO2 - 021
    1.0 ,   & ! CH4 - 022
    1.0 ,   & ! ETH - 023
    1.0 ,   & ! ALKA - 024
    1.0 ,   & ! ALKE - 025
    1.0 ,   & ! BIO - 026
    1.0 ,   & ! ARO - 027
    1.0 ,   & ! HCHO - 028
    1.0 ,   & ! ALD - 029
    1.0 ,   & ! KET - 030
    1.0 ,   & ! CRBO - 031
    1.0 ,   & ! ONIT - 032
    1.0 ,   & ! PAN - 033
    1.0 ,   & ! OP1 - 034
    1.0 ,   & ! OP2 - 035
    1.0 ,   & ! ORA1 - 036
    1.0 ,   & ! ORA2 - 037
    1.0 ,   & ! MO2 - 038
    1.0 ,   & ! AKAP - 039
    1.0 ,   & ! AKEP - 040
    1.0 ,   & ! BIOP - 041
    1.0 ,   & ! PHO - 042
    1.0 ,   & ! ADD - 043
    1.0 ,   & ! AROP - 044
    1.0 ,   & ! CBOP - 045
    1.0 ,   & ! OLN - 046
    1.0     & ! XO2 - 047
   /)
  
  
!    ACID DISSOCIATION CONSTANT AT 298K 
!     [mole/liter of liquid water]
!     Referencias: Barth et al. JGR 112, D13310 2007
!     Martell and Smith, 1976, Critical stability
!     vol1-4 Plenum Press New York
  REAL,PARAMETER,DIMENSION(nspecies) :: ak0=(/&
    0.00E+00     ,   & ! O3 - 001
    2.20E-12     ,   & ! H2O2 - 002
    0.00E+00     ,   & ! NO - 003
    0.00E+00     ,   & ! NO2 - 004
    0.00E+00     ,   & ! NO3 - 005
    0.00E+00     ,   & ! N2O5 - 006
    7.10E-04     ,   & ! HONO - 007
    1.54E+01     ,   & ! HNO3 - 008
    0.00E+00     ,   & ! HNO4 - 009
    1.30E-02     ,   & ! SO2 - 010
    1.00E-02     ,   & ! SULF - 011
    0.00E+00     ,   & ! CO - 012
    4.50E-07     ,   & ! CO2 - 013
    0.00E+00     ,   & ! N2 - 014
    0.00E+00     ,   & ! O2 - 015
    0.00E+00     ,   & ! H2O - 016
    0.00E+00     ,   & ! H2 - 017
    0.00E+00     ,   & ! O3P - 018
    0.00E+00     ,   & ! O1D - 019
    0.00E+00     ,   & ! HO - 020
    3.50E-05     ,   & ! HO2 - 021
    0.00E+00     ,   & ! CH4 - 022
    0.00E+00     ,   & ! ETH - 023
    0.00E+00     ,   & ! ALKA - 024
    0.00E+00     ,   & ! ALKE - 025
    0.00E+00     ,   & ! BIO - 026
    0.00E+00     ,   & ! ARO - 027
    0.00E+00     ,   & ! HCHO - 028
    0.00E+00     ,   & ! ALD - 029
    0.00E+00     ,   & ! KET - 030
    0.00E+00     ,   & ! CRBO - 031
    0.00E+00     ,   & ! ONIT - 032
    0.00E+00     ,   & ! PAN - 033
    0.00E+00     ,   & ! OP1 - 034
    0.00E+00     ,   & ! OP2 - 035
    1.80E-04     ,   & ! ORA1 - 036
    1.75E-05     ,   & ! ORA2 - 037
    0.00E+00     ,   & ! MO2 - 038
    0.00E+00     ,   & ! AKAP - 039
    0.00E+00     ,   & ! AKEP - 040
    0.00E+00     ,   & ! BIOP - 041
    0.00E+00     ,   & ! PHO - 042
    0.00E+00     ,   & ! ADD - 043
    0.00E+00     ,   & ! AROP - 044
    0.00E+00     ,   & ! CBOP - 045
    0.00E+00     ,   & ! OLN - 046
    0.00E+00         & ! XO2 - 047
   /)
  
  
!     Temperature correction factor for
!     acid dissociation constants
!     [K]
!     Referencias: Barth et al. JGR 112, D13310 2007
  REAL,PARAMETER,DIMENSION(nspecies) :: dak=(/&
    0.         ,   & ! O3 - 001
    -3700.     ,   & ! H2O2 - 002
    0.         ,   & ! NO - 003
    0.         ,   & ! NO2 - 004
    0.         ,   & ! NO3 - 005
    0.         ,   & ! N2O5 - 006
    0.         ,   & ! HONO - 007
    0.         ,   & ! HNO3 - 008
    0.         ,   & ! HNO4 - 009
    2000.      ,   & ! SO2 - 010
    0.         ,   & ! SULF - 011
    0.         ,   & ! CO - 012
    -1000.     ,   & ! CO2 - 013
    0.         ,   & ! N2 - 014
    0.         ,   & ! O2 - 015
    0.         ,   & ! H2O - 016
    0.         ,   & ! H2 - 017
    0.         ,   & ! O3P - 018
    0.         ,   & ! O1D - 019
    0.         ,   & ! HO - 020
    0.         ,   & ! HO2 - 021
    0.         ,   & ! CH4 - 022
    0.         ,   & ! ETH - 023
    0.         ,   & ! ALKA - 024
    0.         ,   & ! ALKE - 025
    0.         ,   & ! BIO - 026
    0.         ,   & ! ARO - 027
    0.         ,   & ! HCHO - 028
    0.         ,   & ! ALD - 029
    0.         ,   & ! KET - 030
    0.         ,   & ! CRBO - 031
    0.         ,   & ! ONIT - 032
    0.         ,   & ! PAN - 033
    0.         ,   & ! OP1 - 034
    0.         ,   & ! OP2 - 035
    -1500.     ,   & ! ORA1 - 036
    0.         ,   & ! ORA2 - 037
    0.         ,   & ! MO2 - 038
    0.         ,   & ! AKAP - 039
    0.         ,   & ! AKEP - 040
    0.         ,   & ! BIOP - 041
    0.         ,   & ! PHO - 042
    0.         ,   & ! ADD - 043
    0.         ,   & ! AROP - 044
    0.         ,   & ! CBOP - 045
    0.         ,   & ! OLN - 046
    0.             & ! XO2 - 047
    /)
  
  
  INTEGER,PARAMETER :: nr   =128!Number of gas-phase reactions
  INTEGER,PARAMETER :: nrt  =128!Total Number of reactions
  INTEGER,PARAMETER :: nrh2o=000!Number of aqueous-phase reactions
  
 !------------------------------------------------------------------------------  
  
 ! Photolysis Rate Calculation: method used (LUT=look_up_table, FAST-JX= on-line)
  
  CHARACTER(LEN=10),PARAMETER :: PhotojMethod= 'FAST-TUV'
  INTEGER,PARAMETER :: maxJcomb=    5, nr_photo=   17
  INTEGER,PARAMETER,DIMENSION(nr_photo) :: nfactors=(/ &
                            1, &!  1
                            1, &!  2
                            1, &!  3
                            1, &!  4
                            1, &!  5
                            1, &!  6
                            1, &!  7
                            1, &!  8
                            1, &!  9
                            1, &! 10
                            1, &! 11
                            1, &! 12
                            1, &! 13
                            1, &! 14
                            1, &! 15
                            3, &! 16
                            1/)! 17
 
  DOUBLE PRECISION,PARAMETER,DIMENSION(maxJcomb,nr_photo) :: factor=RESHAPE((/ &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+00, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.900D+00, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.280D+00, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.500D+00, 0.500D+00, 0.500D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00/),(/maxJcomb,nr_photo/))
  CHARACTER(LEN=07 ),PARAMETER,DIMENSION(maxJcomb,nr_photo) :: JReactionComp=RESHAPE((/ &
                                               "NO2    ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "O3(1D) ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "O3     ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "HNO2   ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "HNO3   ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "HNO4   ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "NO3    ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "NO3    ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "H2O2   ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "H2COb  ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "H2COa  ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "ActAld ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "CH3OOH ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "CH3OOH ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "H2O2   ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "Acet-a ","Acet-b ","MEKeto ","NONE   ","NONE   ", &
                                               "Glyxlb ","NONE   ","NONE   ","NONE   ","NONE   "/),(/maxJcomb,nr_photo/))

  
END MODULE chem1_list
