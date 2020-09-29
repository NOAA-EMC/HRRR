MODULE chem1_list
  IMPLICIT NONE
  
  
  CHARACTER(LEN=24),PARAMETER :: chemical_mechanism='CB07'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=036
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
   'NO2 ' & !
     ,'NO  ' & !
     ,'O   ' & !
     ,'O3  ' & !
     ,'NO3 ' & !
     ,'O1D ' & !
     ,'OH  ' & !
     ,'HO2 ' & !
     ,'N2O5' & !
     ,'HNO3' & !
     ,'HONO' & !
     ,'PNA ' & !
     ,'H2O2' & !
     ,'CO  ' & !
     ,'FORM' & !
     ,'ALD2' & !
     ,'C2O3' & !
     ,'XO2 ' & !
     ,'PAN ' & !
     ,'PAR ' & !
     ,'XO2N' & !
     ,'ROR ' & !
     ,'OLE ' & !
     ,'ETH ' & !
     ,'TOL ' & !
     ,'CRES' & !
     ,'TO2 ' & !
     ,'OPEN' & !
     ,'CRO ' & !
     ,'MGLY' & !
     ,'XYL ' & !
     ,'ISOP' & !
     ,'ISPD' & !
     ,'CH4 ' & !
     ,'MEOH' & !
     ,'ETOH' & !
   /)
  
  
  !Number of each specie   
  INTEGER,PARAMETER :: NO2 =001
  INTEGER,PARAMETER :: NO  =002
  INTEGER,PARAMETER :: O   =003
  INTEGER,PARAMETER :: O3  =004
  INTEGER,PARAMETER :: NO3 =005
  INTEGER,PARAMETER :: O1D =006
  INTEGER,PARAMETER :: OH  =007
  INTEGER,PARAMETER :: HO2 =008
  INTEGER,PARAMETER :: N2O5=009
  INTEGER,PARAMETER :: HNO3=010
  INTEGER,PARAMETER :: HONO=011
  INTEGER,PARAMETER :: PNA =012
  INTEGER,PARAMETER :: H2O2=013
  INTEGER,PARAMETER :: CO  =014
  INTEGER,PARAMETER :: FORM=015
  INTEGER,PARAMETER :: ALD2=016
  INTEGER,PARAMETER :: C2O3=017
  INTEGER,PARAMETER :: XO2 =018
  INTEGER,PARAMETER :: PAN =019
  INTEGER,PARAMETER :: PAR =020
  INTEGER,PARAMETER :: XO2N=021
  INTEGER,PARAMETER :: ROR =022
  INTEGER,PARAMETER :: OLE =023
  INTEGER,PARAMETER :: ETH =024
  INTEGER,PARAMETER :: TOL =025
  INTEGER,PARAMETER :: CRES=026
  INTEGER,PARAMETER :: TO2 =027
  INTEGER,PARAMETER :: OPEN=028
  INTEGER,PARAMETER :: CRO =029
  INTEGER,PARAMETER :: MGLY=030
  INTEGER,PARAMETER :: XYL =031
  INTEGER,PARAMETER :: ISOP=032
  INTEGER,PARAMETER :: ISPD=033
  INTEGER,PARAMETER :: CH4 =034
  INTEGER,PARAMETER :: MEOH=035
  INTEGER,PARAMETER :: ETOH=036
  
  
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
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! NO2 - 001
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! NO - 002
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O - 003
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! O3 - 004
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! NO3 - 005
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O1D - 006
    0 , 1 , 1 , 0 , 0 , 0 ,   & ! OH - 007
    0 , 1 , 1 , 0 , 0 , 0 ,   & ! HO2 - 008
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! N2O5 - 009
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! HNO3 - 010
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HONO - 011
    0 , 1 , 0 , 0 , 0 , 1 ,   & ! PNA - 012
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! H2O2 - 013
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CO - 014
    1 , 1 , 0 , 1 , 0 , 1 ,   & ! FORM - 015
    1 , 1 , 0 , 1 , 0 , 1 ,   & ! ALD2 - 016
    0 , 0 , 0 , 0 , 0 , 1 ,   & ! C2O3 - 017
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! XO2 - 018
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! PAN - 019
    1 , 0 , 0 , 1 , 0 , 1 ,   & ! PAR - 020
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! XO2N - 021
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ROR - 022
    1 , 1 , 0 , 1 , 0 , 1 ,   & ! OLE - 023
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ETH - 024
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! TOL - 025
    1 , 0 , 0 , 1 , 0 , 1 ,   & ! CRES - 026
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! TO2 - 027
    0 , 0 , 0 , 0 , 0 , 1 ,   & ! OPEN - 028
    0 , 0 , 0 , 0 , 0 , 1 ,   & ! CRO - 029
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! MGLY - 030
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! XYL - 031
    1 , 0 , 0 , 1 , 0 , 1 ,   & ! ISOP - 032
    0 , 0 , 0 , 1 , 0 , 1 ,   & ! ISPD - 033
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CH4 - 034
    1 , 0 , 0 , 0 , 0 , 1 ,   & ! MEOH - 035
    1 , 0 , 0 , 0 , 0 , 1    & ! ETOH - 036
    /),(/6,nspecies/))
  
  
  INTEGER,PARAMETER,DIMENSION(nspecies) :: spc_uveq=(/ &
   09, & !NO2
   04, & !NO
   00, & !O
   02, & !O3
   10, & !NO3
   00, & !O1D
   00, & !OH
   00, & !HO2
   11, & !N2O5
   13, & !HNO3
   00, & !HONO
   14, & !PNA
   07, & !H2O2
   00, & !CO
   00, & !FORM
   00, & !ALD2
   00, & !C2O3
   00, & !XO2
   49, & !PAN
   49, & !PAR
   00, & !XO2N
   00, & !ROR
   00, & !OLE
   00, & !ETH
   00, & !TOL
   00, & !CRES
   00, & !TO2
   00, & !OPEN
   00, & !CRO
   00, & !MGLY
   00, & !XYL
   00, & !ISOP
   00, & !ISPD
   00, & !CH4
   00, & !MEOH
   00  & !ETOH
   /)
  
  
!     HENRYS LAW COEFFICIENTS
!     Henrys law coefficient
!     [KH298]=mole/(l atm)
!     Referencias em http://www.mpch-mainz.mpg.de/~sander/res/henry.html
!     * indica artigos nao encontrados nesse endereço eletronico
  REAL,PARAMETER,DIMENSION(nspecies) :: hstar=(/&
    1.20E-2              ,   & ! NO2 - 001
    1.90E-3              ,   & ! NO - 002
    0.0E+00              ,   & ! O - 003
    1.10E-2              ,   & ! O3 - 004
    1.8E+00              ,   & ! NO3 - 005
    0.00E+0              ,   & ! O1D - 006
    3.00E+1              ,   & ! OH - 007
    5.70E+3              ,   & ! HO2 - 008
    1.0E+10              ,   & ! N2O5 - 009
    2.10E+5              ,   & ! HNO3 - 010
    5.00E+1              ,   & ! HONO - 011
    0.0E+00              ,   & ! PNA - 012
    8.30E+4              ,   & ! H2O2 - 013
    9.90E-4              ,   & ! CO - 014
    6.0E+03              ,   & ! FORM - 015
    1.40E+1              ,   & ! ALD2 - 016
    0.0E+00              ,   & ! C2O3 - 017
    0.0E+00              ,   & ! XO2 - 018
    4.10E+0              ,   & ! PAN - 019
    3.50E+0              ,   & ! PAR - 020
    0.0E+00              ,   & ! XO2N - 021
    0.0E+00              ,   & ! ROR - 022
    0.0E+00              ,   & ! OLE - 023
    1.90E-3              ,   & ! ETH - 024
    1.50E-1              ,   & ! TOL - 025
    0.0E+00              ,   & ! CRES - 026
    0.0E+00              ,   & ! TO2 - 027
    0.0E+00              ,   & ! OPEN - 028
    0.0E+00              ,   & ! CRO - 029
    3.20E+4              ,   & ! MGLY - 030
    1.50E-1              ,   & ! XYL - 031
    0.0E+00              ,   & ! ISOP - 032
    0.0E+00              ,   & ! ISPD - 033
    1.40E-3              ,   & ! CH4 - 034
    0.0E+00              ,   & ! MEOH - 035
    0.0E+00                  & ! ETOH - 036
    /)
  
! [1] Noziere B. et al. The uptake of methyl vinyl ketone, methacrolein,
! and 2-methyl-3-butene-2-olonto sulfuric acid solutions,Journal of Physical
! Chemistry A, Vol.110, No.7, 2387-2395, 2006.
! [2] Abraham M. H. et al. Partition of compounds from gas to water and
! from gas to physiological saline at 310K: Linear free energy relationships,
! elsevier, 2006.
  REAL,PARAMETER,DIMENSION(nspecies) :: f0=(/&
    0.1 ,   & ! NO2 - 001
    0.0 ,   & ! NO - 002
    0.0 ,   & ! O - 003
    1.0 ,   & ! O3 - 004
    0.0 ,   & ! NO3 - 005
    0.0 ,   & ! O1D - 006
    0.0 ,   & ! OH - 007
    0.0 ,   & ! HO2 - 008
    0.0 ,   & ! N2O5 - 009
    0.0 ,   & ! HNO3 - 010
    0.0 ,   & ! HONO - 011
    0.0 ,   & ! PNA - 012
    1.0 ,   & ! H2O2 - 013
    0.0 ,   & ! CO - 014
    0.0 ,   & ! FORM - 015
    0.0 ,   & ! ALD2 - 016
    0.0 ,   & ! C2O3 - 017
    0.0 ,   & ! XO2 - 018
    0.1 ,   & ! PAN - 019
    0.1 ,   & ! PAR - 020
    0.0 ,   & ! XO2N - 021
    0.0 ,   & ! ROR - 022
    0.0 ,   & ! OLE - 023
    0.0 ,   & ! ETH - 024
    0.0 ,   & ! TOL - 025
    0.0 ,   & ! CRES - 026
    0.0 ,   & ! TO2 - 027
    0.0 ,   & ! OPEN - 028
    0.0 ,   & ! CRO - 029
    0.0 ,   & ! MGLY - 030
    0.0 ,   & ! XYL - 031
    0.0 ,   & ! ISOP - 032
    0.0 ,   & ! ISPD - 033
    0.0 ,   & ! CH4 - 034
    0.0 ,   & ! MEOH - 035
    0.0     & ! ETOH - 036
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: difrat=(/&
    1.6 ,   & ! NO2 - 001
    1.3 ,   & ! NO - 002
    0.0 ,   & ! O - 003
    1.6 ,   & ! O3 - 004
    0.0 ,   & ! NO3 - 005
    0.0 ,   & ! O1D - 006
    0.0 ,   & ! OH - 007
    0.0 ,   & ! HO2 - 008
    0.0 ,   & ! N2O5 - 009
    1.9 ,   & ! HNO3 - 010
    0.0 ,   & ! HONO - 011
    0.0 ,   & ! PNA - 012
    1.4 ,   & ! H2O2 - 013
    0.0 ,   & ! CO - 014
    1.3 ,   & ! FORM - 015
    1.6 ,   & ! ALD2 - 016
    0.0 ,   & ! C2O3 - 017
    0.0 ,   & ! XO2 - 018
    2.6 ,   & ! PAN - 019
    2.6 ,   & ! PAR - 020
    0.0 ,   & ! XO2N - 021
    0.0 ,   & ! ROR - 022
    0.0 ,   & ! OLE - 023
    0.0 ,   & ! ETH - 024
    0.0 ,   & ! TOL - 025
    0.0 ,   & ! CRES - 026
    0.0 ,   & ! TO2 - 027
    0.0 ,   & ! OPEN - 028
    0.0 ,   & ! CRO - 029
    0.0 ,   & ! MGLY - 030
    0.0 ,   & ! XYL - 031
    0.0 ,   & ! ISOP - 032
    0.0 ,   & ! ISPD - 033
    0.0 ,   & ! CH4 - 034
    0.0 ,   & ! MEOH - 035
    0.0     & ! ETOH - 036
    /)
  
  
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)
  REAL,PARAMETER,DIMENSION(nspecies) :: dvj=(/&
    0.1470000	    ,   & ! NO2 - 001
    0.1830000	    ,   & ! NO - 002
    0.2500000	    ,   & ! O - 003
    0.1750000	    ,   & ! O3 - 004
    0.1270000	    ,   & ! NO3 - 005
    0.2500000	    ,   & ! O1D - 006
    0.2430000	    ,   & ! OH - 007
    0.1740000	    ,   & ! HO2 - 008
    0.1100000	    ,   & ! N2O5 - 009
    0.1260000	    ,   & ! HNO3 - 010
    0.1530000	    ,   & ! HONO - 011
    0.1129000	    ,   & ! PNA - 012
    0.1710000	    ,   & ! H2O2 - 013
    0.1890000	    ,   & ! CO - 014
    0.1830000	    ,   & ! FORM - 015
    0.1510000	    ,   & ! ALD2 - 016
    0.1155000	    ,   & ! C2O3 - 017
    0.1507557	    ,   & ! XO2 - 018
    9.0999998E-02 ,   & ! PAN - 019
    9.0999998E-02 ,   & ! PAR - 020
    0.0917000	    ,   & ! XO2N - 021
    0.1491000	    ,   & ! ROR - 022
    0.1900000	    ,   & ! OLE - 023
    0.1830000	    ,   & ! ETH - 024
    0.1040000	    ,   & ! TOL - 025
    0.0960000	    ,   & ! CRES - 026
    0.0960000	    ,   & ! TO2 - 027
    0.0894400	    ,   & ! OPEN - 028
    0.0967000	    ,   & ! CRO - 029
    0.1180000	    ,   & ! MGLY - 030
    9.7000003E-02 ,   & ! XYL - 031
    9.2450030E-02 ,   & ! ISOP - 032
    0.1195000	    ,   & ! ISPD - 033
    0.2500000	    ,   & ! CH4 - 034
    0.1768000	    ,   & ! MEOH - 035
    0.1474000	        & ! ETOH - 036
    /)
  
  
!     -DH/R (for temperature correction)
!     [-DH/R]=K
  REAL,PARAMETER,DIMENSION(nspecies) :: dhr=(/&
    2500.         ,   & ! NO2 - 001
    1400.         ,   & ! NO - 002
    0.0           ,   & ! O - 003
    2400.         ,   & ! O3 - 004
    2000.         ,   & ! NO3 - 005
    0.            ,   & ! O1D - 006
    4500.         ,   & ! OH - 007
    5900.         ,   & ! HO2 - 008
    3400.         ,   & ! N2O5 - 009
    8700.         ,   & ! HNO3 - 010
    4900.         ,   & ! HONO - 011
    0.0           ,   & ! PNA - 012
    7400.         ,   & ! H2O2 - 013
    1300.         ,   & ! CO - 014
    7190.         ,   & ! FORM - 015
    5600.         ,   & ! ALD2 - 016
    0.0           ,   & ! C2O3 - 017
    0.            ,   & ! XO2 - 018
    5760.         ,   & ! PAN - 019
    6500.         ,   & ! PAR - 020
    0.0           ,   & ! XO2N - 021
    0.0           ,   & ! ROR - 022
    0.0           ,   & ! OLE - 023
    2300.         ,   & ! ETH - 024
    4000.         ,   & ! TOL - 025
    0.0           ,   & ! CRES - 026
    0.0           ,   & ! TO2 - 027
    0.0           ,   & ! OPEN - 028
    0.0           ,   & ! CRO - 029
    7500.         ,   & ! MGLY - 030
    4000.         ,   & ! XYL - 031
    0.            ,   & ! ISOP - 032
    0.0           ,   & ! ISPD - 033
    1600.         ,   & ! CH4 - 034
    0.0           ,   & ! MEOH - 035
    0.0               & ! ETOH - 036
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: weight=(/&
    46.  ,   & ! NO2 - 001
    30.  ,   & ! NO - 002
    16.  ,   & ! O - 003
    48.  ,   & ! O3 - 004
    62.  ,   & ! NO3 - 005
    16.  ,   & ! O1D - 006
    17.  ,   & ! OH - 007
    33.  ,   & ! HO2 - 008
    108. ,   & ! N2O5 - 009
    63.  ,   & ! HNO3 - 010
    47.  ,   & ! HONO - 011
    79.  ,   & ! PNA - 012
    34.  ,   & ! H2O2 - 013
    28.  ,   & ! CO - 014
    30.  ,   & ! FORM - 015
    44.  ,   & ! ALD2 - 016
    75.  ,   & ! C2O3 - 017
    44.  ,   & ! XO2 - 018
    121. ,   & ! PAN - 019
    14.  ,   & ! PAR - 020
    119. ,   & ! XO2N - 021
    45.  ,   & ! ROR - 022
    28.  ,   & ! OLE - 023
    28.  ,   & ! ETH - 024
    92.  ,   & ! TOL - 025
    108. ,   & ! CRES - 026
    109. ,   & ! TO2 - 027
    125. ,   & ! OPEN - 028
    107. ,   & ! CRO - 029
    72.  ,   & ! MGLY - 030
    106. ,   & ! XYL - 031
    68.  ,   & ! ISOP - 032
    70.  ,   & ! ISPD - 033
    16.  ,   & ! CH4 - 034
    32.  ,   & ! MEOH - 035
    46.      & ! ETOH - 036
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: init_ajust=(/&
    1.0 ,   & ! NO2 - 001
    1.0 ,   & ! NO - 002
    1.0 ,   & ! O - 003
    1.0 ,   & ! O3 - 004
    1.0 ,   & ! NO3 - 005
    1.0 ,   & ! O1D - 006
    1.0 ,   & ! OH - 007
    1.0 ,   & ! HO2 - 008
    1.0 ,   & ! N2O5 - 009
    1.0 ,   & ! HNO3 - 010
    1.0 ,   & ! HONO - 011
    1.0 ,   & ! PNA - 012
    1.0 ,   & ! H2O2 - 013
    1.0 ,   & ! CO - 014
    1.0 ,   & ! FORM - 015
    1.0 ,   & ! ALD2 - 016
    1.0 ,   & ! C2O3 - 017
    1.0 ,   & ! XO2 - 018
    1.0 ,   & ! PAN - 019
    1.0 ,   & ! PAR - 020
    1.0 ,   & ! XO2N - 021
    1.0 ,   & ! ROR - 022
    1.0 ,   & ! OLE - 023
    1.0 ,   & ! ETH - 024
    1.0 ,   & ! TOL - 025
    1.0 ,   & ! CRES - 026
    1.0 ,   & ! TO2 - 027
    1.0 ,   & ! OPEN - 028
    1.0 ,   & ! CRO - 029
    1.0 ,   & ! MGLY - 030
    1.0 ,   & ! XYL - 031
    1.0 ,   & ! ISOP - 032
    1.0 ,   & ! ISPD - 033
    1.0 ,   & ! CH4 - 034
    1.0 ,   & ! MEOH - 035
    1.0     & ! ETOH - 036
   /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: emiss_ajust=(/&
    1.0 ,   & ! NO2 - 001
    1.0 ,   & ! NO - 002
    1.0 ,   & ! O - 003
    1.0 ,   & ! O3 - 004
    1.0 ,   & ! NO3 - 005
    1.0 ,   & ! O1D - 006
    1.0 ,   & ! OH - 007
    1.0 ,   & ! HO2 - 008
    1.0 ,   & ! N2O5 - 009
    1.0 ,   & ! HNO3 - 010
    1.0 ,   & ! HONO - 011
    1.0 ,   & ! PNA - 012
    1.0 ,   & ! H2O2 - 013
    1.0 ,   & ! CO - 014
    1.0 ,   & ! FORM - 015
    1.0 ,   & ! ALD2 - 016
    1.0 ,   & ! C2O3 - 017
    1.0 ,   & ! XO2 - 018
    1.0 ,   & ! PAN - 019
    1.0 ,   & ! PAR - 020
    1.0 ,   & ! XO2N - 021
    1.0 ,   & ! ROR - 022
    1.0 ,   & ! OLE - 023
    1.0 ,   & ! ETH - 024
    1.0 ,   & ! TOL - 025
    1.0 ,   & ! CRES - 026
    1.0 ,   & ! TO2 - 027
    1.0 ,   & ! OPEN - 028
    1.0 ,   & ! CRO - 029
    1.0 ,   & ! MGLY - 030
    1.0 ,   & ! XYL - 031
    1.0 ,   & ! ISOP - 032
    1.0 ,   & ! ISPD - 033
    1.0 ,   & ! CH4 - 034
    1.0 ,   & ! MEOH - 035
    1.0     & ! ETOH - 036
   /)
  
  
!    ACID DISSOCIATION CONSTANT AT 298K 
!     [mole/liter of liquid water]
!     Referencias: Barth et al. JGR 112, D13310 2007
!     Martell and Smith, 1976, Critical stability
!     vol1-4 Plenum Press New York
  REAL,PARAMETER,DIMENSION(nspecies) :: ak0=(/&
    0.00E+00	    ,   & ! NO2 - 001
    0.00E+00	    ,   & ! NO - 002
    0.00E+00	    ,   & ! O - 003
    0.00E+00	    ,   & ! O3 - 004
    0.00E+00	    ,   & ! NO3 - 005
    0.00E+00	    ,   & ! O1D - 006
    0.00E+00	    ,   & ! OH - 007
    3.50E-05	    ,   & ! HO2 - 008
    0.00E+00	    ,   & ! N2O5 - 009
    1.54E+01	    ,   & ! HNO3 - 010
    7.10E-04	    ,   & ! HONO - 011
    0.00E+00	    ,   & ! PNA - 012
    2.20E-12     ,   & ! H2O2 - 013
    0.00E+00	    ,   & ! CO - 014
    0.00E+00	    ,   & ! FORM - 015
    0.00E+00	    ,   & ! ALD2 - 016
    0.00E+00	    ,   & ! C2O3 - 017
    0.00E+00	    ,   & ! XO2 - 018
    0.00E+00	    ,   & ! PAN - 019
    0.00E+00	    ,   & ! PAR - 020
    0.00E+00	    ,   & ! XO2N - 021
    0.00E+00	    ,   & ! ROR - 022
    0.00E+00	    ,   & ! OLE - 023
    0.00E+00	    ,   & ! ETH - 024
    0.00E+00	    ,   & ! TOL - 025
    0.00E+00	    ,   & ! CRES - 026
    0.00E+00	    ,   & ! TO2 - 027
    0.00E+00	    ,   & ! OPEN - 028
    0.00E+00	    ,   & ! CRO - 029
    0.00E+00	    ,   & ! MGLY - 030
    0.00E+00	    ,   & ! XYL - 031
    0.00E+00	    ,   & ! ISOP - 032
    0.00E+00	    ,   & ! ISPD - 033
    0.00E+00	    ,   & ! CH4 - 034
    0.00E+00	    ,   & ! MEOH - 035
    0.00E+00	        & ! ETOH - 036
   /)
  
  
!     Temperature correction factor for
!     acid dissociation constants
!     [K]
!     Referencias: Barth et al. JGR 112, D13310 2007
  REAL,PARAMETER,DIMENSION(nspecies) :: dak=(/&
    0.         ,   & ! NO2 - 001
    0.         ,   & ! NO - 002
    0.         ,   & ! O - 003
    0.         ,   & ! O3 - 004
    0.         ,   & ! NO3 - 005
    0.         ,   & ! O1D - 006
    0.         ,   & ! OH - 007
    0.         ,   & ! HO2 - 008
    0.         ,   & ! N2O5 - 009
    0.         ,   & ! HNO3 - 010
    0.         ,   & ! HONO - 011
    0.         ,   & ! PNA - 012
    -3700.     ,   & ! H2O2 - 013
    0.         ,   & ! CO - 014
    0.         ,   & ! FORM - 015
    0.         ,   & ! ALD2 - 016
    0.         ,   & ! C2O3 - 017
    0.         ,   & ! XO2 - 018
    0.         ,   & ! PAN - 019
    0.         ,   & ! PAR - 020
    0.         ,   & ! XO2N - 021
    0.         ,   & ! ROR - 022
    0.         ,   & ! OLE - 023
    0.         ,   & ! ETH - 024
    0.         ,   & ! TOL - 025
    0.         ,   & ! CRES - 026
    0.         ,   & ! TO2 - 027
    0.         ,   & ! OPEN - 028
    0.         ,   & ! CRO - 029
    0.         ,   & ! MGLY - 030
    0.         ,   & ! XYL - 031
    0.         ,   & ! ISOP - 032
    0.         ,   & ! ISPD - 033
    0.         ,   & ! CH4 - 034
    0.         ,   & ! MEOH - 035
    0.             & ! ETOH - 036
    /)
  
  
  INTEGER,PARAMETER :: nr   =103!Number of gas-phase reactions
  INTEGER,PARAMETER :: nrt  =103!Total Number of reactions
  INTEGER,PARAMETER :: nrh2o=000!Number of aqueous-phase reactions
  
 !------------------------------------------------------------------------------  
  
 ! Photolysis Rate Calculation: method used (LUT=look_up_table, FAST-JX= on-line)
  
  CHARACTER(LEN=10),PARAMETER :: PhotojMethod= 'LUT'
  INTEGER,PARAMETER :: maxJcomb=    5, nr_photo=    1
  INTEGER,PARAMETER,DIMENSION(nr_photo) :: nfactors=(/ &
                            0/)!  1
 
  DOUBLE PRECISION,PARAMETER,DIMENSION(maxJcomb,nr_photo) :: factor=RESHAPE((/ &
                                                0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00/),(/maxJcomb,nr_photo/))
  CHARACTER(LEN=07 ),PARAMETER,DIMENSION(maxJcomb,nr_photo) :: JReactionComp=RESHAPE((/ &
                                               "NONE   ","NONE   ","NONE   ","NONE   ","NONE   "/),(/maxJcomb,nr_photo/))

  
END MODULE chem1_list
