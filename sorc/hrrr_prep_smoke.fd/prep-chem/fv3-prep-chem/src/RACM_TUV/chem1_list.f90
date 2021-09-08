MODULE chem1_list
  IMPLICIT NONE
  
  
  CHARACTER(LEN=24),PARAMETER :: chemical_mechanism='RACM'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=072
  
  
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
     ,'O3P ' & !
     ,'O1D ' & !
     ,'HO  ' & !
     ,'HO2 ' & !
     ,'CH4 ' & !
     ,'ETH ' & !
     ,'HC3 ' & !
     ,'HC5 ' & !
     ,'HC8 ' & !
     ,'ETE ' & !
     ,'OLT ' & !
     ,'OLI ' & !
     ,'DIEN' & !
     ,'ISO ' & !
     ,'API ' & !
     ,'LIM ' & !
     ,'TOL ' & !
     ,'XYL ' & !
     ,'CSL ' & !
     ,'HCHO' & !
     ,'ALD ' & !
     ,'KET ' & !
     ,'GLY ' & !
     ,'MGLY' & !
     ,'DCB ' & !
     ,'MACR' & !
     ,'UDD ' & !
     ,'HKET' & !
     ,'ONIT' & !
     ,'PAN ' & !
     ,'TPAN' & !
     ,'OP1 ' & !
     ,'OP2 ' & !
     ,'PAA ' & !
     ,'ORA1' & !
     ,'ORA2' & !
     ,'MO2 ' & !
     ,'ETHP' & !
     ,'HC3P' & !
     ,'HC5P' & !
     ,'HC8P' & !
     ,'ETEP' & !
     ,'OLTP' & !
     ,'OLIP' & !
     ,'ISOP' & !
     ,'APIP' & !
     ,'LIMP' & !
     ,'PHO ' & !
     ,'ADDT' & !
     ,'ADDX' & !
     ,'ADDC' & !
     ,'TOLP' & !
     ,'XYLP' & !
     ,'CSLP' & !
     ,'ACO3' & !
     ,'TCO3' & !
     ,'KETP' & !
     ,'OLNN' & !
     ,'OLND' & !
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
  INTEGER,PARAMETER :: O3P =013
  INTEGER,PARAMETER :: O1D =014
  INTEGER,PARAMETER :: HO  =015
  INTEGER,PARAMETER :: HO2 =016
  INTEGER,PARAMETER :: CH4 =017
  INTEGER,PARAMETER :: ETH =018
  INTEGER,PARAMETER :: HC3 =019
  INTEGER,PARAMETER :: HC5 =020
  INTEGER,PARAMETER :: HC8 =021
  INTEGER,PARAMETER :: ETE =022
  INTEGER,PARAMETER :: OLT =023
  INTEGER,PARAMETER :: OLI =024
  INTEGER,PARAMETER :: DIEN=025
  INTEGER,PARAMETER :: ISO =026
  INTEGER,PARAMETER :: API =027
  INTEGER,PARAMETER :: LIM =028
  INTEGER,PARAMETER :: TOL =029
  INTEGER,PARAMETER :: XYL =030
  INTEGER,PARAMETER :: CSL =031
  INTEGER,PARAMETER :: HCHO=032
  INTEGER,PARAMETER :: ALD =033
  INTEGER,PARAMETER :: KET =034
  INTEGER,PARAMETER :: GLY =035
  INTEGER,PARAMETER :: MGLY=036
  INTEGER,PARAMETER :: DCB =037
  INTEGER,PARAMETER :: MACR=038
  INTEGER,PARAMETER :: UDD =039
  INTEGER,PARAMETER :: HKET=040
  INTEGER,PARAMETER :: ONIT=041
  INTEGER,PARAMETER :: PAN =042
  INTEGER,PARAMETER :: TPAN=043
  INTEGER,PARAMETER :: OP1 =044
  INTEGER,PARAMETER :: OP2 =045
  INTEGER,PARAMETER :: PAA =046
  INTEGER,PARAMETER :: ORA1=047
  INTEGER,PARAMETER :: ORA2=048
  INTEGER,PARAMETER :: MO2 =049
  INTEGER,PARAMETER :: ETHP=050
  INTEGER,PARAMETER :: HC3P=051
  INTEGER,PARAMETER :: HC5P=052
  INTEGER,PARAMETER :: HC8P=053
  INTEGER,PARAMETER :: ETEP=054
  INTEGER,PARAMETER :: OLTP=055
  INTEGER,PARAMETER :: OLIP=056
  INTEGER,PARAMETER :: ISOP=057
  INTEGER,PARAMETER :: APIP=058
  INTEGER,PARAMETER :: LIMP=059
  INTEGER,PARAMETER :: PHO =060
  INTEGER,PARAMETER :: ADDT=061
  INTEGER,PARAMETER :: ADDX=062
  INTEGER,PARAMETER :: ADDC=063
  INTEGER,PARAMETER :: TOLP=064
  INTEGER,PARAMETER :: XYLP=065
  INTEGER,PARAMETER :: CSLP=066
  INTEGER,PARAMETER :: ACO3=067
  INTEGER,PARAMETER :: TCO3=068
  INTEGER,PARAMETER :: KETP=069
  INTEGER,PARAMETER :: OLNN=070
  INTEGER,PARAMETER :: OLND=071
  INTEGER,PARAMETER :: XO2 =072
  
  
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
    1 , 1 , 1 , 1 , 0 , 1 ,   & ! NO2 - 004
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! NO3 - 005
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! N2O5 - 006
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HONO - 007
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! HNO3 - 008
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HNO4 - 009
    1 , 1 , 1 , 1 , 0 , 1 ,   & ! SO2 - 010
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! SULF - 011
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CO - 012
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O3P - 013
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! O1D - 014
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HO - 015
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HO2 - 016
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CH4 - 017
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ETH - 018
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! HC3 - 019
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! HC5 - 020
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! HC8 - 021
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ETE - 022
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! OLT - 023
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! OLI - 024
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! DIEN - 025
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ISO - 026
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! API - 027
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! LIM - 028
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! TOL - 029
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! XYL - 030
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! CSL - 031
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! HCHO - 032
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! ALD - 033
    1 , 1 , 1 , 1 , 1 , 1 ,   & ! KET - 034
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! GLY - 035
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! MGLY - 036
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! DCB - 037
    0 , 1 , 1 , 1 , 1 , 1 ,   & ! MACR - 038
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! UDD - 039
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! HKET - 040
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! ONIT - 041
    0 , 1 , 1 , 1 , 0 , 1 ,   & ! PAN - 042
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! TPAN - 043
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! OP1 - 044
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! OP2 - 045
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! PAA - 046
    1 , 1 , 1 , 0 , 1 , 1 ,   & ! ORA1 - 047
    1 , 1 , 1 , 0 , 1 , 1 ,   & ! ORA2 - 048
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! MO2 - 049
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ETHP - 050
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! HC3P - 051
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! HC5P - 052
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! HC8P - 053
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ETEP - 054
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! OLTP - 055
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! OLIP - 056
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ISOP - 057
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! APIP - 058
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! LIMP - 059
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! PHO - 060
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ADDT - 061
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ADDX - 062
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! ADDC - 063
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! TOLP - 064
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! XYLP - 065
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! CSLP - 066
    0 , 1 , 1 , 0 , 0 , 1 ,   & ! ACO3 - 067
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! TCO3 - 068
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! KETP - 069
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! OLNN - 070
    0 , 0 , 0 , 0 , 0 , 0 ,   & ! OLND - 071
    0 , 0 , 0 , 0 , 0 , 0    & ! XO2 - 072
    /),(/6,nspecies/))
  
  
  INTEGER,PARAMETER,DIMENSION(nspecies) :: spc_uveq=(/ &
   02, & !O3
   07, & !H2O2
   04, & !NO
   09, & !NO2
   10, & !NO3
   11, & !N2O5
   00, & !HONO
   13, & !HNO3
   14, & !HNO4
   00, & !SO2
   00, & !SULF
   00, & !CO
   00, & !O3P
   00, & !O1D
   00, & !HO
   00, & !HO2
   00, & !CH4
   00, & !ETH
   00, & !HC3
   00, & !HC5
   00, & !HC8
   00, & !ETE
   00, & !OLT
   00, & !OLI
   00, & !DIEN
   00, & !ISO
   00, & !API
   00, & !LIM
   00, & !TOL
   00, & !XYL
   00, & !CSL
   00, & !HCHO
   00, & !ALD
   00, & !KET
   00, & !GLY
   00, & !MGLY
   00, & !DCB
   00, & !MACR
   00, & !UDD
   00, & !HKET
   00, & !ONIT
   49, & !PAN
   00, & !TPAN
   00, & !OP1
   00, & !OP2
   00, & !PAA
   00, & !ORA1
   00, & !ORA2
   00, & !MO2
   00, & !ETHP
   00, & !HC3P
   00, & !HC5P
   00, & !HC8P
   00, & !ETEP
   00, & !OLTP
   00, & !OLIP
   00, & !ISOP
   00, & !APIP
   00, & !LIMP
   00, & !PHO
   00, & !ADDT
   00, & !ADDX
   00, & !ADDC
   00, & !TOLP
   00, & !XYLP
   00, & !CSLP
   00, & !ACO3
   00, & !TCO3
   00, & !KETP
   00, & !OLNN
   00, & !OLND
   00  & !XO2
   /)
  
  
!     HENRYS LAW COEFFICIENTS
!     Henrys law coefficient
!     [KH298]=mole/(l atm)
!     Referencias em http://www.mpch-mainz.mpg.de/~sander/res/henry.html
!     * indica artigos nao encontrados nesse endereço eletronico
  REAL,PARAMETER,DIMENSION(nspecies) :: hstar=(/&
    1.10E-2              ,   & ! O3 - 001
    8.30E+4              ,   & ! H2O2 - 002
    1.90E-3              ,   & ! NO - 003
    1.20E-2              ,   & ! NO2 - 004
    1.8E+00              ,   & ! NO3 - 005
    1.0E+10              ,   & ! N2O5 - 006
    5.00E+1              ,   & ! HONO - 007
    2.10E+5              ,   & ! HNO3 - 008
    1.20E+4              ,   & ! HNO4 - 009
    1.40E+0              ,   & ! SO2 - 010
    7.00E+1              ,   & ! SULF - 011
    9.90E-4              ,   & ! CO - 012
    0.00E+0              ,   & ! O3P - 013
    0.00E+0              ,   & ! O1D - 014
    3.00E+1              ,   & ! HO - 015
    5.70E+3              ,   & ! HO2 - 016
    1.40E-3              ,   & ! CH4 - 017
    1.90E-3              ,   & ! ETH - 018
    1.10E-3              ,   & ! HC3 - 019
    1.00E-3              ,   & ! HC5 - 020
    2.00E-3              ,   & ! HC8 - 021
    4.70E-3              ,   & ! ETE - 022
    8.50E-3              ,   & ! OLT - 023
    4.40E-3              ,   & ! OLI - 024
    1.40E-2              ,   & ! DIEN - 025
    2.80E-2              ,   & ! ISO - 026
    4.90E-2              ,   & ! API - 027
    1.13E-2              ,   & ! LIM - 028
    1.50E-1              ,   & ! TOL - 029
    1.50E-1              ,   & ! XYL - 030
    8.20E+2              ,   & ! CSL - 031
    3.20E+3              ,   & ! HCHO - 032
    1.40E+1              ,   & ! ALD - 033
    2.50E+1              ,   & ! KET - 034
    3.60E+5              ,   & ! GLY - 035
    3.20E+4              ,   & ! MGLY - 036
    1.40E+6              ,   & ! DCB - 037
    6.50E+0              ,   & ! MACR - 038
    0.00E+0              ,   & ! UDD - 039
    0.00E+0              ,   & ! HKET - 040
    1.00E+0              ,   & ! ONIT - 041
    3.50E+0              ,   & ! PAN - 042
    1.70E+0              ,   & ! TPAN - 043
    3.10E+2              ,   & ! OP1 - 044
    3.40E+2              ,   & ! OP2 - 045
    8.40E+2              ,   & ! PAA - 046
    5.40E+3              ,   & ! ORA1 - 047
    5.50E+3              ,   & ! ORA2 - 048
    2.00E+3              ,   & ! MO2 - 049
    0.0E+00              ,   & ! ETHP - 050
    0.0E+00              ,   & ! HC3P - 051
    0.0E+00              ,   & ! HC5P - 052
    0.0E+00              ,   & ! HC8P - 053
    0.0E+00              ,   & ! ETEP - 054
    0.0E+00              ,   & ! OLTP - 055
    0.0E+00              ,   & ! OLIP - 056
    0.0E+00              ,   & ! ISOP - 057
    0.0E+00              ,   & ! APIP - 058
    0.0E+00              ,   & ! LIMP - 059
    0.0E+00              ,   & ! PHO - 060
    0.0E+00              ,   & ! ADDT - 061
    0.0E+00              ,   & ! ADDX - 062
    0.0E+00              ,   & ! ADDC - 063
    0.0E+00              ,   & ! TOLP - 064
    0.0E+00              ,   & ! XYLP - 065
    0.0E+00              ,   & ! CSLP - 066
    1.14E+1              ,   & ! ACO3 - 067
    0.0E+00              ,   & ! TCO3 - 068
    0.0E+00              ,   & ! KETP - 069
    0.0E+00              ,   & ! OLNN - 070
    0.0E+00              ,   & ! OLND - 071
    0.0E+00                  & ! XO2 - 072
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
    0.0 ,   & ! O3P - 013
    0.0 ,   & ! O1D - 014
    0.0 ,   & ! HO - 015
    0.0 ,   & ! HO2 - 016
    0.0 ,   & ! CH4 - 017
    0.0 ,   & ! ETH - 018
    0.0 ,   & ! HC3 - 019
    0.0 ,   & ! HC5 - 020
    0.0 ,   & ! HC8 - 021
    0.0 ,   & ! ETE - 022
    0.0 ,   & ! OLT - 023
    0.0 ,   & ! OLI - 024
    0.0 ,   & ! DIEN - 025
    0.0 ,   & ! ISO - 026
    0.0 ,   & ! API - 027
    0.0 ,   & ! LIM - 028
    0.0 ,   & ! TOL - 029
    0.0 ,   & ! XYL - 030
    0.0 ,   & ! CSL - 031
    0.0 ,   & ! HCHO - 032
    0.0 ,   & ! ALD - 033
    0.0 ,   & ! KET - 034
    0.0 ,   & ! GLY - 035
    0.0 ,   & ! MGLY - 036
    0.0 ,   & ! DCB - 037
    0.0 ,   & ! MACR - 038
    0.0 ,   & ! UDD - 039
    0.0 ,   & ! HKET - 040
    0.0 ,   & ! ONIT - 041
    0.1 ,   & ! PAN - 042
    0.0 ,   & ! TPAN - 043
    0.1 ,   & ! OP1 - 044
    0.1 ,   & ! OP2 - 045
    0.1 ,   & ! PAA - 046
    0.0 ,   & ! ORA1 - 047
    0.0 ,   & ! ORA2 - 048
    0.0 ,   & ! MO2 - 049
    0.0 ,   & ! ETHP - 050
    0.0 ,   & ! HC3P - 051
    0.0 ,   & ! HC5P - 052
    0.0 ,   & ! HC8P - 053
    0.0 ,   & ! ETEP - 054
    0.0 ,   & ! OLTP - 055
    0.0 ,   & ! OLIP - 056
    0.0 ,   & ! ISOP - 057
    0.0 ,   & ! APIP - 058
    0.0 ,   & ! LIMP - 059
    0.0 ,   & ! PHO - 060
    0.0 ,   & ! ADDT - 061
    0.0 ,   & ! ADDX - 062
    0.0 ,   & ! ADDC - 063
    0.0 ,   & ! TOLP - 064
    0.0 ,   & ! XYLP - 065
    0.0 ,   & ! CSLP - 066
    0.0 ,   & ! ACO3 - 067
    0.0 ,   & ! TCO3 - 068
    0.0 ,   & ! KETP - 069
    0.0 ,   & ! OLNN - 070
    0.0 ,   & ! OLND - 071
    0.0     & ! XO2 - 072
    /)
  
  
  REAL,PARAMETER,DIMENSION(nspecies) :: difrat=(/&
    1.6 ,   & ! O3 - 001
    1.4 ,   & ! H2O2 - 002
    1.3 ,   & ! NO - 003
    1.6 ,   & ! NO2 - 004
    0.0 ,   & ! NO3 - 005
    0.0 ,   & ! N2O5 - 006
    0.0 ,   & ! HONO - 007
    1.9 ,   & ! HNO3 - 008
    0.0 ,   & ! HNO4 - 009
    1.9 ,   & ! SO2 - 010
    0.0 ,   & ! SULF - 011
    0.0 ,   & ! CO - 012
    0.0 ,   & ! O3P - 013
    0.0 ,   & ! O1D - 014
    0.0 ,   & ! HO - 015
    0.0 ,   & ! HO2 - 016
    0.0 ,   & ! CH4 - 017
    0.0 ,   & ! ETH - 018
    0.0 ,   & ! HC3 - 019
    0.0 ,   & ! HC5 - 020
    0.0 ,   & ! HC8 - 021
    0.0 ,   & ! ETE - 022
    0.0 ,   & ! OLT - 023
    0.0 ,   & ! OLI - 024
    0.0 ,   & ! DIEN - 025
    0.0 ,   & ! ISO - 026
    0.0 ,   & ! API - 027
    0.0 ,   & ! LIM - 028
    0.0 ,   & ! TOL - 029
    0.0 ,   & ! XYL - 030
    0.0 ,   & ! CSL - 031
    1.3 ,   & ! HCHO - 032
    1.6 ,   & ! ALD - 033
    0.0 ,   & ! KET - 034
    0.0 ,   & ! GLY - 035
    0.0 ,   & ! MGLY - 036
    0.0 ,   & ! DCB - 037
    0.0 ,   & ! MACR - 038
    0.0 ,   & ! UDD - 039
    0.0 ,   & ! HKET - 040
    0.0 ,   & ! ONIT - 041
    2.6 ,   & ! PAN - 042
    0.0 ,   & ! TPAN - 043
    1.6 ,   & ! OP1 - 044
    1.6 ,   & ! OP2 - 045
    2.0 ,   & ! PAA - 046
    1.6 ,   & ! ORA1 - 047
    1.6 ,   & ! ORA2 - 048
    0.0 ,   & ! MO2 - 049
    0.0 ,   & ! ETHP - 050
    0.0 ,   & ! HC3P - 051
    0.0 ,   & ! HC5P - 052
    0.0 ,   & ! HC8P - 053
    0.0 ,   & ! ETEP - 054
    0.0 ,   & ! OLTP - 055
    0.0 ,   & ! OLIP - 056
    0.0 ,   & ! ISOP - 057
    0.0 ,   & ! APIP - 058
    0.0 ,   & ! LIMP - 059
    0.0 ,   & ! PHO - 060
    0.0 ,   & ! ADDT - 061
    0.0 ,   & ! ADDX - 062
    0.0 ,   & ! ADDC - 063
    0.0 ,   & ! TOLP - 064
    0.0 ,   & ! XYLP - 065
    0.0 ,   & ! CSLP - 066
    0.0 ,   & ! ACO3 - 067
    0.0 ,   & ! TCO3 - 068
    0.0 ,   & ! KETP - 069
    0.0 ,   & ! OLNN - 070
    0.0 ,   & ! OLND - 071
    0.0     & ! XO2 - 072
    /)
  
  
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)
  REAL,PARAMETER,DIMENSION(nspecies) :: dvj=(/&
    0.1750000	    ,   & ! O3 - 001
    0.1710000	    ,   & ! H2O2 - 002
    0.1830000	    ,   & ! NO - 003
    0.1470000	    ,   & ! NO2 - 004
    0.1270000	    ,   & ! NO3 - 005
    0.1100000	    ,   & ! N2O5 - 006
    0.1530000	    ,   & ! HONO - 007
    0.1260000	    ,   & ! HNO3 - 008
    0.1130000	    ,   & ! HNO4 - 009
    0.1260000	    ,   & ! SO2 - 010
    0.1010153	    ,   & ! SULF - 011
    0.1890000	    ,   & ! CO - 012
    0.2500000	    ,   & ! O3P - 013
    0.2500000	    ,   & ! O1D - 014
    0.2430000	    ,   & ! HO - 015
    0.1740000	    ,   & ! HO2 - 016
    0.2500000	    ,   & ! CH4 - 017
    0.1830000	    ,   & ! ETH - 018
    0.1510000	    ,   & ! HC3 - 019
    0.1180000	    ,   & ! HC5 - 020
    9.3999997E-02 ,   & ! HC8 - 021
    0.1889822	    ,   & ! ETE - 022
    0.1540000	    ,   & ! OLT - 023
    0.1210000	    ,   & ! OLI - 024
    0.1360828	    ,   & ! DIEN - 025
    0.1210000	    ,   & ! ISO - 026
    8.5749298E-02 ,   & ! API - 027
    8.5749298E-02 ,   & ! LIM - 028
    0.1040000	    ,   & ! TOL - 029
    9.7000003E-02 ,   & ! XYL - 030
    9.6000001E-02 ,   & ! CSL - 031
    0.1830000	    ,   & ! HCHO - 032
    0.1510000	    ,   & ! ALD - 033
    0.1180000	    ,   & ! KET - 034
    0.1310000	    ,   & ! GLY - 035
    0.1180000	    ,   & ! MGLY - 036
    0.1070000	    ,   & ! DCB - 037
    0.1195229	    ,   & ! MACR - 038
    9.1669850E-02 ,   & ! UDD - 039
    0.1162476	    ,   & ! HKET - 040
    9.2000000E-02 ,   & ! ONIT - 041
    9.0999998E-02 ,   & ! PAN - 042
    8.2000002E-02 ,   & ! TPAN - 043
    0.1440000	    ,   & ! OP1 - 044
    0.1270000	    ,   & ! OP2 - 045
    0.1150000	    ,   & ! PAA - 046
    0.1530000	    ,   & ! ORA1 - 047
    0.1240000	    ,   & ! ORA2 - 048
    0.1458650	    ,   & ! MO2 - 049
    0.1280369	    ,   & ! ETHP - 050
    0.1154700	    ,   & ! HC3P - 051
    9.8532930E-02 ,   & ! HC5P - 052
    8.3045483E-02 ,   & ! HC8P - 053
    0.1139606	    ,   & ! ETEP - 054
    0.1048285	    ,   & ! OLTP - 055
    9.2450030E-02 ,   & ! OLIP - 056
    9.2450030E-02 ,   & ! ISOP - 057
    7.3521458E-02 ,   & ! APIP - 058
    7.3521458E-02 ,   & ! LIMP - 059
    9.6673653E-02 ,   & ! PHO - 060
    9.5782630E-02 ,   & ! ADDT - 061
    9.0166964E-02 ,   & ! ADDX - 062
    8.9442722E-02 ,   & ! ADDC - 063
    8.4215194E-02 ,   & ! TOLP - 064
    8.0321930E-02 ,   & ! XYLP - 065
    7.9808690E-02 ,   & ! CSLP - 066
    0.1150000	    ,   & ! ACO3 - 067
    9.3250483E-02 ,   & ! TCO3 - 068
    9.8532930E-02 ,   & ! KETP - 069
    8.5749298E-02 ,   & ! OLNN - 070
    8.5749298E-02 ,   & ! OLND - 071
    0.1507557	        & ! XO2 - 072
    /)
  
  
!     -DH/R (for temperature correction)
!     [-DH/R]=K
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
    0.            ,   & ! O3P - 013
    0.            ,   & ! O1D - 014
    4500.         ,   & ! HO - 015
    5900.         ,   & ! HO2 - 016
    1600.         ,   & ! CH4 - 017
    2300.         ,   & ! ETH - 018
    2800.         ,   & ! HC3 - 019
    7500.         ,   & ! HC5 - 020
    6250.         ,   & ! HC8 - 021
    1800.         ,   & ! ETE - 022
    4000.         ,   & ! OLT - 023
    4000.         ,   & ! OLI - 024
    4500.         ,   & ! DIEN - 025
    4500.         ,   & ! ISO - 026
    4500.         ,   & ! API - 027
    4500.         ,   & ! LIM - 028
    4000.         ,   & ! TOL - 029
    4000.         ,   & ! XYL - 030
    5800.         ,   & ! CSL - 031
    6800.         ,   & ! HCHO - 032
    5600.         ,   & ! ALD - 033
    4800.         ,   & ! KET - 034
    7500.         ,   & ! GLY - 035
    7500.         ,   & ! MGLY - 036
    0.            ,   & ! DCB - 037
    5300.         ,   & ! MACR - 038
    0.            ,   & ! UDD - 039
    0.            ,   & ! HKET - 040
    5800.         ,   & ! ONIT - 041
    6500.         ,   & ! PAN - 042
    6500.         ,   & ! TPAN - 043
    5200.         ,   & ! OP1 - 044
    6000.         ,   & ! OP2 - 045
    5300.         ,   & ! PAA - 046
    5700.         ,   & ! ORA1 - 047
    6300.         ,   & ! ORA2 - 048
    6600.         ,   & ! MO2 - 049
    0.            ,   & ! ETHP - 050
    0.            ,   & ! HC3P - 051
    0.            ,   & ! HC5P - 052
    0.            ,   & ! HC8P - 053
    0.            ,   & ! ETEP - 054
    0.            ,   & ! OLTP - 055
    0.            ,   & ! OLIP - 056
    0.            ,   & ! ISOP - 057
    0.            ,   & ! APIP - 058
    0.            ,   & ! LIMP - 059
    0.            ,   & ! PHO - 060
    0.            ,   & ! ADDT - 061
    0.            ,   & ! ADDX - 062
    0.            ,   & ! ADDC - 063
    0.            ,   & ! TOLP - 064
    0.            ,   & ! XYLP - 065
    0.            ,   & ! CSLP - 066
    6266.         ,   & ! ACO3 - 067
    0.            ,   & ! TCO3 - 068
    0.            ,   & ! KETP - 069
    0.            ,   & ! OLNN - 070
    0.            ,   & ! OLND - 071
    0.                & ! XO2 - 072
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
    16.  ,   & ! O3P - 013
    16.  ,   & ! O1D - 014
    17.  ,   & ! HO - 015
    33.  ,   & ! HO2 - 016
    16.  ,   & ! CH4 - 017
    30.  ,   & ! ETH - 018
    44.  ,   & ! HC3 - 019
    72.  ,   & ! HC5 - 020
    114. ,   & ! HC8 - 021
    28.  ,   & ! ETE - 022
    42.  ,   & ! OLT - 023
    68.  ,   & ! OLI - 024
    54.  ,   & ! DIEN - 025
    68.  ,   & ! ISO - 026
    136. ,   & ! API - 027
    136. ,   & ! LIM - 028
    92.  ,   & ! TOL - 029
    106. ,   & ! XYL - 030
    108. ,   & ! CSL - 031
    30.  ,   & ! HCHO - 032
    44.  ,   & ! ALD - 033
    72.  ,   & ! KET - 034
    58.  ,   & ! GLY - 035
    72.  ,   & ! MGLY - 036
    87.  ,   & ! DCB - 037
    70.  ,   & ! MACR - 038
    119. ,   & ! UDD - 039
    74.  ,   & ! HKET - 040
    119. ,   & ! ONIT - 041
    121. ,   & ! PAN - 042
    147. ,   & ! TPAN - 043
    48.  ,   & ! OP1 - 044
    62.  ,   & ! OP2 - 045
    76.  ,   & ! PAA - 046
    46.  ,   & ! ORA1 - 047
    60.  ,   & ! ORA2 - 048
    47.  ,   & ! MO2 - 049
    61.  ,   & ! ETHP - 050
    75.  ,   & ! HC3P - 051
    103. ,   & ! HC5P - 052
    145. ,   & ! HC8P - 053
    77.  ,   & ! ETEP - 054
    91.  ,   & ! OLTP - 055
    117. ,   & ! OLIP - 056
    117. ,   & ! ISOP - 057
    185. ,   & ! APIP - 058
    185. ,   & ! LIMP - 059
    107. ,   & ! PHO - 060
    109. ,   & ! ADDT - 061
    123. ,   & ! ADDX - 062
    125. ,   & ! ADDC - 063
    141. ,   & ! TOLP - 064
    155. ,   & ! XYLP - 065
    157. ,   & ! CSLP - 066
    75.  ,   & ! ACO3 - 067
    115. ,   & ! TCO3 - 068
    103. ,   & ! KETP - 069
    136. ,   & ! OLNN - 070
    136. ,   & ! OLND - 071
    44.      & ! XO2 - 072
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
    1.0 ,   & ! O3P - 013
    1.0 ,   & ! O1D - 014
    1.0 ,   & ! HO - 015
    1.0 ,   & ! HO2 - 016
    1.0 ,   & ! CH4 - 017
    1.0 ,   & ! ETH - 018
    1.0 ,   & ! HC3 - 019
    1.0 ,   & ! HC5 - 020
    1.0 ,   & ! HC8 - 021
    1.0 ,   & ! ETE - 022
    1.0 ,   & ! OLT - 023
    1.0 ,   & ! OLI - 024
    1.0 ,   & ! DIEN - 025
    1.0 ,   & ! ISO - 026
    1.0 ,   & ! API - 027
    1.0 ,   & ! LIM - 028
    1.0 ,   & ! TOL - 029
    1.0 ,   & ! XYL - 030
    1.0 ,   & ! CSL - 031
    1.0 ,   & ! HCHO - 032
    1.0 ,   & ! ALD - 033
    1.0 ,   & ! KET - 034
    1.0 ,   & ! GLY - 035
    1.0 ,   & ! MGLY - 036
    1.0 ,   & ! DCB - 037
    1.0 ,   & ! MACR - 038
    1.0 ,   & ! UDD - 039
    1.0 ,   & ! HKET - 040
    1.0 ,   & ! ONIT - 041
    1.0 ,   & ! PAN - 042
    1.0 ,   & ! TPAN - 043
    1.0 ,   & ! OP1 - 044
    1.0 ,   & ! OP2 - 045
    1.0 ,   & ! PAA - 046
    1.0 ,   & ! ORA1 - 047
    1.0 ,   & ! ORA2 - 048
    1.0 ,   & ! MO2 - 049
    1.0 ,   & ! ETHP - 050
    1.0 ,   & ! HC3P - 051
    1.0 ,   & ! HC5P - 052
    1.0 ,   & ! HC8P - 053
    1.0 ,   & ! ETEP - 054
    1.0 ,   & ! OLTP - 055
    1.0 ,   & ! OLIP - 056
    1.0 ,   & ! ISOP - 057
    1.0 ,   & ! APIP - 058
    1.0 ,   & ! LIMP - 059
    1.0 ,   & ! PHO - 060
    1.0 ,   & ! ADDT - 061
    1.0 ,   & ! ADDX - 062
    1.0 ,   & ! ADDC - 063
    1.0 ,   & ! TOLP - 064
    1.0 ,   & ! XYLP - 065
    1.0 ,   & ! CSLP - 066
    1.0 ,   & ! ACO3 - 067
    1.0 ,   & ! TCO3 - 068
    1.0 ,   & ! KETP - 069
    1.0 ,   & ! OLNN - 070
    1.0 ,   & ! OLND - 071
    1.0     & ! XO2 - 072
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
    1.0 ,   & ! O3P - 013
    1.0 ,   & ! O1D - 014
    1.0 ,   & ! HO - 015
    1.0 ,   & ! HO2 - 016
    1.0 ,   & ! CH4 - 017
    1.0 ,   & ! ETH - 018
    1.0 ,   & ! HC3 - 019
    1.0 ,   & ! HC5 - 020
    1.0 ,   & ! HC8 - 021
    1.0 ,   & ! ETE - 022
    1.0 ,   & ! OLT - 023
    1.0 ,   & ! OLI - 024
    1.0 ,   & ! DIEN - 025
    1.0 ,   & ! ISO - 026
    1.0 ,   & ! API - 027
    1.0 ,   & ! LIM - 028
    1.0 ,   & ! TOL - 029
    1.0 ,   & ! XYL - 030
    1.0 ,   & ! CSL - 031
    1.0 ,   & ! HCHO - 032
    1.0 ,   & ! ALD - 033
    1.0 ,   & ! KET - 034
    1.0 ,   & ! GLY - 035
    1.0 ,   & ! MGLY - 036
    1.0 ,   & ! DCB - 037
    1.0 ,   & ! MACR - 038
    1.0 ,   & ! UDD - 039
    1.0 ,   & ! HKET - 040
    1.0 ,   & ! ONIT - 041
    1.0 ,   & ! PAN - 042
    1.0 ,   & ! TPAN - 043
    1.0 ,   & ! OP1 - 044
    1.0 ,   & ! OP2 - 045
    1.0 ,   & ! PAA - 046
    1.0 ,   & ! ORA1 - 047
    1.0 ,   & ! ORA2 - 048
    1.0 ,   & ! MO2 - 049
    1.0 ,   & ! ETHP - 050
    1.0 ,   & ! HC3P - 051
    1.0 ,   & ! HC5P - 052
    1.0 ,   & ! HC8P - 053
    1.0 ,   & ! ETEP - 054
    1.0 ,   & ! OLTP - 055
    1.0 ,   & ! OLIP - 056
    1.0 ,   & ! ISOP - 057
    1.0 ,   & ! APIP - 058
    1.0 ,   & ! LIMP - 059
    1.0 ,   & ! PHO - 060
    1.0 ,   & ! ADDT - 061
    1.0 ,   & ! ADDX - 062
    1.0 ,   & ! ADDC - 063
    1.0 ,   & ! TOLP - 064
    1.0 ,   & ! XYLP - 065
    1.0 ,   & ! CSLP - 066
    1.0 ,   & ! ACO3 - 067
    1.0 ,   & ! TCO3 - 068
    1.0 ,   & ! KETP - 069
    1.0 ,   & ! OLNN - 070
    1.0 ,   & ! OLND - 071
    1.0     & ! XO2 - 072
   /)
  
  
!    ACID DISSOCIATION CONSTANT AT 298K 
!     [mole/liter of liquid water]
!     Referencias: Barth et al. JGR 112, D13310 2007
!     Martell and Smith, 1976, Critical stability
!     vol1-4 Plenum Press New York
  REAL,PARAMETER,DIMENSION(nspecies) :: ak0=(/&
    0.00E+00	    ,   & ! O3 - 001
    2.20E-12     ,   & ! H2O2 - 002
    0.00E+00	    ,   & ! NO - 003
    0.00E+00	    ,   & ! NO2 - 004
    0.00E+00	    ,   & ! NO3 - 005
    0.00E+00	    ,   & ! N2O5 - 006
    7.10E-04	    ,   & ! HONO - 007
    1.54E+01	    ,   & ! HNO3 - 008
    0.00E+00	    ,   & ! HNO4 - 009
    1.30E-02     ,   & ! SO2 - 010
    1.00E-02	    ,   & ! SULF - 011
    0.00E+00	    ,   & ! CO - 012
    0.00E+00	    ,   & ! O3P - 013
    0.00E+00	    ,   & ! O1D - 014
    0.00E+00	    ,   & ! HO - 015
    3.50E-05	    ,   & ! HO2 - 016
    0.00E+00	    ,   & ! CH4 - 017
    0.00E+00	    ,   & ! ETH - 018
    0.00E+00	    ,   & ! HC3 - 019
    0.00E+00	    ,   & ! HC5 - 020
    0.00E+00	    ,   & ! HC8 - 021
    0.00E+00	    ,   & ! ETE - 022
    0.00E+00	    ,   & ! OLT - 023
    0.00E+00	    ,   & ! OLI - 024
    0.00E+00	    ,   & ! DIEN - 025
    0.00E+00	    ,   & ! ISO - 026
    0.00E+00	    ,   & ! API - 027
    0.00E+00	    ,   & ! LIM - 028
    0.00E+00	    ,   & ! TOL - 029
    0.00E+00	    ,   & ! XYL - 030
    1.50E-10	    ,   & ! CSL - 031
    0.00E+00	    ,   & ! HCHO - 032
    0.00E+00	    ,   & ! ALD - 033
    0.00E+00	    ,   & ! KET - 034
    0.00E+00	    ,   & ! GLY - 035
    0.00E+00	    ,   & ! MGLY - 036
    0.00E+00	    ,   & ! DCB - 037
    0.00E+00	    ,   & ! MACR - 038
    0.00E+00	    ,   & ! UDD - 039
    0.00E+00	    ,   & ! HKET - 040
    0.00E+00	    ,   & ! ONIT - 041
    0.00E+00	    ,   & ! PAN - 042
    0.00E+00	    ,   & ! TPAN - 043
    0.00E+00	    ,   & ! OP1 - 044
    0.00E+00	    ,   & ! OP2 - 045
    0.00E+00	    ,   & ! PAA - 046
    1.80E-04     ,   & ! ORA1 - 047
    1.75E-05	    ,   & ! ORA2 - 048
    0.00E+00	    ,   & ! MO2 - 049
    0.00E+00	    ,   & ! ETHP - 050
    0.00E+00	    ,   & ! HC3P - 051
    0.00E+00	    ,   & ! HC5P - 052
    0.00E+00	    ,   & ! HC8P - 053
    0.00E+00	    ,   & ! ETEP - 054
    0.00E+00	    ,   & ! OLTP - 055
    0.00E+00	    ,   & ! OLIP - 056
    0.00E+00	    ,   & ! ISOP - 057
    0.00E+00	    ,   & ! APIP - 058
    0.00E+00	    ,   & ! LIMP - 059
    0.00E+00	    ,   & ! PHO - 060
    0.00E+00	    ,   & ! ADDT - 061
    0.00E+00	    ,   & ! ADDX - 062
    0.00E+00	    ,   & ! ADDC - 063
    0.00E+00	    ,   & ! TOLP - 064
    0.00E+00	    ,   & ! XYLP - 065
    0.00E+00	    ,   & ! CSLP - 066
    0.00E+00	    ,   & ! ACO3 - 067
    0.00E+00	    ,   & ! TCO3 - 068
    0.00E+00	    ,   & ! KETP - 069
    0.00E+00	    ,   & ! OLNN - 070
    0.00E+00	    ,   & ! OLND - 071
    0.00E+00	        & ! XO2 - 072
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
    0.         ,   & ! O3P - 013
    0.         ,   & ! O1D - 014
    0.         ,   & ! HO - 015
    0.         ,   & ! HO2 - 016
    0.         ,   & ! CH4 - 017
    0.         ,   & ! ETH - 018
    0.         ,   & ! HC3 - 019
    0.         ,   & ! HC5 - 020
    0.         ,   & ! HC8 - 021
    0.         ,   & ! ETE - 022
    0.         ,   & ! OLT - 023
    0.         ,   & ! OLI - 024
    0.         ,   & ! DIEN - 025
    0.         ,   & ! ISO - 026
    0.         ,   & ! API - 027
    0.         ,   & ! LIM - 028
    0.         ,   & ! TOL - 029
    0.         ,   & ! XYL - 030
    0.         ,   & ! CSL - 031
    0.         ,   & ! HCHO - 032
    0.         ,   & ! ALD - 033
    0.         ,   & ! KET - 034
    0.         ,   & ! GLY - 035
    0.         ,   & ! MGLY - 036
    0.         ,   & ! DCB - 037
    0.         ,   & ! MACR - 038
    0.         ,   & ! UDD - 039
    0.         ,   & ! HKET - 040
    0.         ,   & ! ONIT - 041
    0.         ,   & ! PAN - 042
    0.         ,   & ! TPAN - 043
    0.         ,   & ! OP1 - 044
    0.         ,   & ! OP2 - 045
    0.         ,   & ! PAA - 046
    -1500.     ,   & ! ORA1 - 047
    0.         ,   & ! ORA2 - 048
    0.         ,   & ! MO2 - 049
    0.         ,   & ! ETHP - 050
    0.         ,   & ! HC3P - 051
    0.         ,   & ! HC5P - 052
    0.         ,   & ! HC8P - 053
    0.         ,   & ! ETEP - 054
    0.         ,   & ! OLTP - 055
    0.         ,   & ! OLIP - 056
    0.         ,   & ! ISOP - 057
    0.         ,   & ! APIP - 058
    0.         ,   & ! LIMP - 059
    0.         ,   & ! PHO - 060
    0.         ,   & ! ADDT - 061
    0.         ,   & ! ADDX - 062
    0.         ,   & ! ADDC - 063
    0.         ,   & ! TOLP - 064
    0.         ,   & ! XYLP - 065
    0.         ,   & ! CSLP - 066
    0.         ,   & ! ACO3 - 067
    0.         ,   & ! TCO3 - 068
    0.         ,   & ! KETP - 069
    0.         ,   & ! OLNN - 070
    0.         ,   & ! OLND - 071
    0.             & ! XO2 - 072
    /)
  
  
  INTEGER,PARAMETER :: nr   =237!Number of gas-phase reactions
  INTEGER,PARAMETER :: nrt  =237!Total Number of reactions
  INTEGER,PARAMETER :: nrh2o=000!Number of aqueous-phase reactions
  
 !------------------------------------------------------------------------------  
  
 ! Photolysis Rate Calculation: method used (LUT=look_up_table, FAST-JX= on-line)
  
  CHARACTER(LEN=10),PARAMETER :: PhotojMethod= 'FAST-TUV'
  INTEGER,PARAMETER :: maxJcomb=    5, nr_photo=   23
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
                            1, &! 17
                            1, &! 18
                            1, &! 19
                            2, &! 20
                            2, &! 21
                            1, &! 22
                            3/)! 23
 
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
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+00, 0.285D-02, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.200D+00, 0.800D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.100D+01, 0.000D+00, 0.000D+00, 0.000D+00, 0.000D+00, &
                                                0.500D+00, 0.500D+00, 0.500D+00, 0.000D+00, 0.000D+00/),(/maxJcomb,nr_photo/))
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
                                               "Glyxlb ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "Glyxla ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "MGlyxl ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "NO2    ","NO2    ","NONE   ","NONE   ","NONE   ", &
                                               "n-pn   ","i-pn   ","NONE   ","NONE   ","NONE   ", &
                                               "MeAcr  ","NONE   ","NONE   ","NONE   ","NONE   ", &
                                               "Acet-a ","Acet-b ","MEKeto ","NONE   ","NONE   "/),(/maxJcomb,nr_photo/))

  
END MODULE chem1_list
