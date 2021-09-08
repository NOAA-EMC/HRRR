MODULE chem1_list
  IMPLICIT NONE
  
  CHARACTER(LEN=24),PARAMETER :: chemical_mechanism='RADM_WRF_FIM'
  INTEGER,PARAMETER :: maxnspecies= 200
  INTEGER,PARAMETER :: nspecies=086
  
  
  !Name of species 
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies) :: spc_name=(/ &
      'O3      ' & !
     ,'H2O2    ' & !
     ,'NO      ' & !
     ,'NO2     ' & !
     ,'NO3     ' & !
     ,'N2O5    ' & !
     ,'HONO    ' & !
     ,'HNO3    ' & !
     ,'HNO4    ' & !
     ,'SO2     ' & !
     ,'SO4     ' & !!srf - changed from SULF to SO4
     ,'CO      ' & !
     ,'CO2     ' & !
     ,'NN      ' & !
     ,'O2      ' & !
     ,'H2O     ' & !
     ,'H2      ' & !
     ,'O3P     ' & !
     ,'O1D     ' & !
     ,'HO      ' & !
     ,'HO2     ' & !
     ,'CH4     ' & !
     ,'ETH     ' & !
     ,'HC3     ' & !
     ,'HC5     ' & !
     ,'HC8     ' & !
     ,'ETE     ' & !
     ,'OLT     ' & !
     ,'OLI     ' & !
     ,'DIEN    ' & !
     ,'ISO     ' & !
     ,'API     ' & !
     ,'LIM     ' & !
     ,'TOL     ' & !
     ,'XYL     ' & !
     ,'CSL     ' & !
     ,'HCHO    ' & !
     ,'ALD     ' & !
     ,'KET     ' & !
     ,'GLY     ' & !
     ,'MGLY    ' & !
     ,'DCB     ' & !
     ,'MACR    ' & !
     ,'UDD     ' & !
     ,'HKET    ' & !
     ,'ONIT    ' & !
     ,'PAN     ' & !
     ,'TPAN    ' & !
     ,'OP1     ' & !
     ,'OP2     ' & !
     ,'PAA     ' & !
     ,'ORA1    ' & !
     ,'ORA2    ' & !
     ,'MO2     ' & !
     ,'ETHP    ' & !
     ,'HC3P    ' & !
     ,'HC5P    ' & !
     ,'HC8P    ' & !
     ,'ETEP    ' & !
     ,'OLTP    ' & !
     ,'OLIP    ' & !
     ,'ISOP    ' & !
     ,'APIP    ' & !
     ,'LIMP    ' & !
     ,'PHO     ' & !
     ,'ADDT    ' & !
     ,'ADDX    ' & !
     ,'ADDC    ' & !
     ,'TOLP    ' & !
     ,'XYLP    ' & !
     ,'CSLP    ' & !
     ,'ACO3    ' & !
     ,'TCO3    ' & !
     ,'KETP    ' & !
     ,'OLNN    ' & !
     ,'OLND    ' & !
     ,'XO2     ' & !
     ,'NH3     ' &
     ,'BBURN2  ' & !PM25
     ,'BBURN3  ' & !PM10     
     ,'OC      '&
     ,'BC      '&
     ,'DMS     '&
     ,'ASH     '& 
     ,'URBAN2  '&
     ,'URBAN3  '&    
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
  INTEGER,PARAMETER :: NN  =014
  INTEGER,PARAMETER :: O2  =015
  INTEGER,PARAMETER :: H2O =016
  INTEGER,PARAMETER :: H2  =017
  INTEGER,PARAMETER :: O3P =018
  INTEGER,PARAMETER :: O1D =019
  INTEGER,PARAMETER :: HO  =020
  INTEGER,PARAMETER :: HO2 =021
  INTEGER,PARAMETER :: CH4 =022
  INTEGER,PARAMETER :: ETH =023
  INTEGER,PARAMETER :: HC3 =024
  INTEGER,PARAMETER :: HC5 =025
  INTEGER,PARAMETER :: HC8 =026
  INTEGER,PARAMETER :: ETE =027
  INTEGER,PARAMETER :: OLT =028
  INTEGER,PARAMETER :: OLI =029
  INTEGER,PARAMETER :: DIEN=030
  INTEGER,PARAMETER :: ISO =031
  INTEGER,PARAMETER :: API =032
  INTEGER,PARAMETER :: LIM =033
  INTEGER,PARAMETER :: TOL =034
  INTEGER,PARAMETER :: XYL =035
  INTEGER,PARAMETER :: CSL =036
  INTEGER,PARAMETER :: HCHO=037
  INTEGER,PARAMETER :: ALD =038
  INTEGER,PARAMETER :: KET =039
  INTEGER,PARAMETER :: GLY =040
  INTEGER,PARAMETER :: MGLY=041
  INTEGER,PARAMETER :: DCB =042
  INTEGER,PARAMETER :: MACR=043
  INTEGER,PARAMETER :: UDD =044
  INTEGER,PARAMETER :: HKET=045
  INTEGER,PARAMETER :: ONIT=046
  INTEGER,PARAMETER :: PAN =047
  INTEGER,PARAMETER :: TPAN=048
  INTEGER,PARAMETER :: OP1 =049
  INTEGER,PARAMETER :: OP2 =050
  INTEGER,PARAMETER :: PAA =051
  INTEGER,PARAMETER :: ORA1=052
  INTEGER,PARAMETER :: ORA2=053
  INTEGER,PARAMETER :: MO2 =054
  INTEGER,PARAMETER :: ETHP=055
  INTEGER,PARAMETER :: HC3P=056
  INTEGER,PARAMETER :: HC5P=057
  INTEGER,PARAMETER :: HC8P=058
  INTEGER,PARAMETER :: ETEP=059
  INTEGER,PARAMETER :: OLTP=060
  INTEGER,PARAMETER :: OLIP=061
  INTEGER,PARAMETER :: ISOP=062
  INTEGER,PARAMETER :: APIP=063
  INTEGER,PARAMETER :: LIMP=064
  INTEGER,PARAMETER :: PHO =065
  INTEGER,PARAMETER :: ADDT=066
  INTEGER,PARAMETER :: ADDX=067
  INTEGER,PARAMETER :: ADDC=068
  INTEGER,PARAMETER :: TOLP=069
  INTEGER,PARAMETER :: XYLP=070
  INTEGER,PARAMETER :: CSLP=071
  INTEGER,PARAMETER :: ACO3=072
  INTEGER,PARAMETER :: TCO3=073
  INTEGER,PARAMETER :: KETP=074
  INTEGER,PARAMETER :: OLNN=075
  INTEGER,PARAMETER :: OLND=076
  INTEGER,PARAMETER :: XO2 =077
  INTEGER,PARAMETER :: NH3 =078
  INTEGER,PARAMETER :: BBURN2=079  !
  INTEGER,PARAMETER :: BBURN3=080  !pm10.
  INTEGER,PARAMETER :: OC  =081
  INTEGER,PARAMETER :: BC  =082
  INTEGER,PARAMETER :: DMS =083
  INTEGER,PARAMETER :: ASH =084
  INTEGER,PARAMETER :: URBAN2 =085 !pm2.5
  INTEGER,PARAMETER :: URBAN3 =086 !pm10
  
  
  INTEGER,PARAMETER :: on = 1
  INTEGER,PARAMETER :: off = 0
  
  
  INTEGER,PARAMETER :: src  = 1 ! source term 
				   
 ! spaction(specie,[1=source]) 
  INTEGER,PARAMETER,DIMENSION(1,nspecies) :: spc_alloc=RESHAPE((/ &
    0 , & ! O3 - 001
    0 , & ! H2O2 - 002
    1 , & ! NO - 003
    1 , & ! NO2 - 004
    0 , & ! NO3 - 005
    0 , & ! N2O5 - 006
    0 , & ! HONO - 007
    0 , & ! HNO3 - 008
    0 , & ! HNO4 - 009
    1 , & ! SO2 - 010
    1 , & ! SULF/SO4 - 011
    1 , & ! CO - 012
    1 , & ! CO2 - 013
    0 , & ! N2 - 014
    0 , & ! O2 - 015
    0 , & ! H2O - 016
    0 , & ! H2 - 017
    0 , & ! O3P - 018
    0 , & ! O1D - 019
    0 , & ! HO - 020
    0 , & ! HO2 - 021
    1 , & ! CH4 - 022
    1 , & ! ETH - 023
    1 , & ! HC3 - 024
    1 , & ! HC5 - 025
    1 , & ! HC8 - 026
    1 , & ! ETE - 027
    1 , & ! OLT - 028
    1 , & ! OLI - 029
    1 , & ! DIEN - 030
    1 , & ! ISO - 031
    1 , & ! API - 032
    1 , & ! LIM - 033
    1 , & ! TOL - 034
    1 , & ! XYL - 035
    1 , & ! CSL - 036
    1 , & ! HCHO - 037
    1 , & ! ALD - 038
    1 , & ! KET - 039
    0 , & ! GLY - 040
    0 , & ! MGLY - 041
    0 , & ! DCB - 042
    1 , & ! MACR - 043
    0 , & ! UDD - 044
    0 , & ! HKET - 045
    0 , & ! ONIT - 046
    0 , & ! PAN - 047
    0 , & ! TPAN - 048
    0 , & ! OP1 - 049
    0 , & ! OP2 - 050
    0 , & ! PAA - 051
    1 , & ! ORA1 - 052
    1 , & ! ORA2 - 053
    0 , & ! MO2 - 054
    0 , & ! ETHP - 055
    0 , & ! HC3P - 056
    0 , & ! HC5P - 057
    0 , & ! HC8P - 058
    0 , & ! ETEP - 059
    0 , & ! OLTP - 060
    0 , & ! OLIP - 061
    0 , & ! ISOP - 062
    0 , & ! APIP - 063
    0 , & ! LIMP - 064
    0 , & ! PHO - 065
    0 , & ! ADDT - 066
    0 , & ! ADDX - 067
    0 , & ! ADDC - 068
    0 , & ! TOLP - 069
    0 , & ! XYLP - 070
    0 , & ! CSLP - 071
    0 , & ! ACO3 - 072
    0 , & ! TCO3 - 073
    0 , & ! KETP - 074
    0 , & ! OLNN - 075
    0 , & ! OLND - 076
    0 , & ! XO2 - 077
    1 , & ! NH3 - 078
    1 , & ! PM25 - 078
    1 , & ! PM10 - 079
    1 , & ! OC - 080
    1 , & ! BC - 081
    1 , & ! DMS - 082
    1 , & ! ASH - 082
    1 , & ! DMS - 082
    1   & ! ASH - 082
   /),(/1,nspecies/))
  
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
    44.  ,   & ! HC3 - 024
    72.  ,   & ! HC5 - 025
    114. ,   & ! HC8 - 026
    28.  ,   & ! ETE - 027
    42.  ,   & ! OLT - 028
    68.  ,   & ! OLI - 029
    54.  ,   & ! DIEN - 030
    68.  ,   & ! ISO - 031
    136. ,   & ! API - 032
    136. ,   & ! LIM - 033
    92.  ,   & ! TOL - 034
    106. ,   & ! XYL - 035
    108. ,   & ! CSL - 036
    30.  ,   & ! HCHO - 037
    44.  ,   & ! ALD - 038
    72.  ,   & ! KET - 039
    58.  ,   & ! GLY - 040
    72.  ,   & ! MGLY - 041
    87.  ,   & ! DCB - 042
    70.  ,   & ! MACR - 043
    119. ,   & ! UDD - 044
    74.  ,   & ! HKET - 045
    119. ,   & ! ONIT - 046
    121. ,   & ! PAN - 047
    147. ,   & ! TPAN - 048
    48.  ,   & ! OP1 - 049
    62.  ,   & ! OP2 - 050
    76.  ,   & ! PAA - 051
    46.  ,   & ! ORA1 - 052
    60.  ,   & ! ORA2 - 053
    47.  ,   & ! MO2 - 054
    61.  ,   & ! ETHP - 055
    75.  ,   & ! HC3P - 056
    103. ,   & ! HC5P - 057
    145. ,   & ! HC8P - 058
    77.  ,   & ! ETEP - 059
    91.  ,   & ! OLTP - 060
    117. ,   & ! OLIP - 061
    117. ,   & ! ISOP - 062
    185. ,   & ! APIP - 063
    185. ,   & ! LIMP - 064
    107. ,   & ! PHO - 065
    109. ,   & ! ADDT - 066
    123. ,   & ! ADDX - 067
    125. ,   & ! ADDC - 068
    141. ,   & ! TOLP - 069
    155. ,   & ! XYLP - 070
    157. ,   & ! CSLP - 071
    75.  ,   & ! ACO3 - 072
    115. ,   & ! TCO3 - 073
    103. ,   & ! KETP - 074
    136. ,   & ! OLNN - 075
    136. ,   & ! OLND - 076
    44.  ,   & ! XO2 - 077
    18.02,   & ! NH3 - 077
    00.  ,   & ! PM25 - 078
    00.  ,   & ! PM10 - 079
    12.  ,   & ! BC - 080
    12.  ,   & ! OC - 081
    62.  ,   & ! DMS - 082
    -99. ,   & ! ASH - 082 !not used
    00.  ,   & ! PM25 - 078
    00.     & ! PM10 - 079
    /)
  
  
END MODULE chem1_list
