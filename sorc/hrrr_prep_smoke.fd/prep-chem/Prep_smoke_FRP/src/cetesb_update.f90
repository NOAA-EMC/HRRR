!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module cetesb_update

!---------------------------------------------------------------------------
integer, parameter ::  ncities = 4, cetesb_nspecies=2, n_data=3
	
integer	::  &	
CO_cetesb    = 01, &       
NOX_cetesb   = 02  !=> the last must be equal to "cetesb_nspecies"

integer :: ispc_cetesb

character(LEN=20),dimension(cetesb_nspecies),parameter :: cetesb_spc_name= &
! avoid tab character
!'12345678901234567890'
(/      &
 'CO '  &     
,'NOX'  /)     

integer, parameter ::         &
 SAOPAULO		  =1  &
,CUBATAO		  =2  &
,SAOJOSECAMPOS		  =3  &
,CAMPINAS		  =4  

!---------------------------------------------------------------------------
character(LEN=25),dimension(ncities),parameter :: city_name= &
! '1234567890123456789012345'
(/                           & 
  'SAO_PAULO                '&
, 'CUBATAO                  '&
, 'SAOJOSECAMPOS            '&
, 'CAMPINAS                 '/)

!---------------------------------------------------------------------------
real,    dimension(cetesb_nspecies,ncities) :: emission_city
data emission_city/ &   ! (kg/dia)
!  CO    ! NOX. !
4.000e+6, 0.869e+6, & ! SAO_PAULO
0.081e+6, 0.052e+6, & ! CUBATAO
0.121e+6, 0.035e+6, & ! SAOJOSECAMPOS
0.782e+6, 0.177e+6  / ! CAMPINAS

!---------------------------------------------------------------------------
real,    dimension(n_data,ncities) :: emission_geo_info
data emission_geo_info/ &
! area (m^2)  !  lat !  lon  
!----------------------
1.747e9,  -23.62, -46.65,  & ! SAO_PAULO
0.142e9,  -23.90, -46.40,  & ! CUBATAO
0.288e9,  -23.18, -45.89,  & ! SAOJOSECAMPOS
0.796e9,  -22.91, -47.06   / ! CAMPINAS


end module cetesb_update
!- correcao do CO/NOX para SAO PAULO de acordo com a CETESB/2005

! A Região Metropolitana de São Paulo - RMSP, está localizada a 23ºS e 46ºW, na porção sudeste do Brasil.
! Possui uma área de 8.051km² com uma população superior a 17 milhões de habitantes, distribuída em uma
! área urbanizada e de maneira desordenada em 1.747km² dessa área. O sítio urbano situa-se, praticamente,
! na Bacia Sedimentar de São Paulo, cujo principal vale é o do Rio Tietê, orientado no sentido leste-oeste,
! com uma altitude média de 720 metros e uma extensa planície de inundação. Essa bacia é cercada ao norte

! A região sofre todo tipo de problemas ambientais, entre os quais está a deterioração da qualidade do ar,
! devida às emissões atmosféricas de cerca de 2000 indústrias de alto potencial poluidor e por uma frota de
! aproximadamente 7,4 milhões de veículos, frota esta que representa 1/5 do total nacional. De acordo com as
! estimativas de 2005, essas fontes de poluição são responsáveis pelas emissões para a atmosfera, dos
! seguintes poluentes: 1,46 milhão de t/ano de monóxido de carbono (CO), 354 mil t/ano de hidrocarbonetos
! (HC), 317 mil t/ano de óxidos de nitrogênio (NOX), 28 mil t/ano de material particulado total (MP) e 12 mil
! t/ano de óxidos de enxofre (SOX). Desses totais os veículos são responsáveis por 97% das emissões de CO,
! 97% de HC, 96% NOX, 40% de MP e 42% de SOX.
!
! emissao de CO  por dia = 1.46 e6 x 1.e3 / 365 (kg/dia) = 4    10^6 kg/dia
! emissao de NOX por dia = 317  e3 x 1.e3 / 365 (kg/dia) = 0.869 10^6 kg/dia
! area efetiva = 1.747km^2 = 42 x 42 km^2
!
!     CO_SP = 4.e+6 
!    NOX_SP = 0.87*1.e+6 
!    name_city(1)='SAO_PAULO'
!    AREA_SP = 1.747 * 1.e9 ! m^2
!    rrlat= -23.62
!    rrlon= -46.65 
!
!- correcao do CO/NOX para Região Metropolitana da Baixada Santista de acordo com a CETESB/2005                                                                  
!A Região Metropolitana da Baixada Santista, com uma área de 2.372km e 1,5 milhão de habitantes, é
!composta por nove municípios, entre eles Cubatão, cuja importância industrial o faz singular na região em
!que está inserido. Cubatão, com uma área de 142km e aproximadamente 110 mil habitantes, dista cerca de
!40 km da cidade de São Paulo. Cubatão foi conhecida como uma área afetada por problemas sérios de
!poluição atmosférica, em função das grandes emissões de poluentes industriais e sua topografia acidentada
!associada às condições meteorológicas desfavoráveis à dispersão de poluentes.
!
! cubatao : lat -23.9   lon -46.4
! cubatao com 142 km^2 :                   CO     HC   NOx   SOx   MP
! cubatao com 142 km^2 : total (1000t/ano) 29.61 3.95 19.03 23.14 3.72
!     CO_CB = 0.08* 1.e6 ! = 29.61 * 1.e6 / 365.
!    NOX_CB = 0.05* 1.e6 ! = 19.03 * 1.e6 / 365.
!    name_city(2)='CUBATAO'
!    AREA_CB = 142 * 1.e6 ! m^2
!    rrlat= -23.9
!    rrlon= -46.4 
!
!--   São José dos Campos
!                                                             
!O município de São José dos Campos com área de 1.102km^2 conta com uma população em torno de 540
!mil habitantes. Atualmente, o parque industrial conta com cerca de 900 empresas e a frota é constituída de,
!aproximadamente, 190 mil veículos. Localizado na porção média do Rio Paraíba do Sul, distante cerca de 70
!quilômetros a nordeste da capital do Estado, ladeando a Rodovia Presidente Dutra, que liga os dois maiores
!centros produtores e consumidores do país que são a Região Metropolitana de São Paulo e a do Rio de
!Janeiro.
!
! São José dos Campos : lat -23.18   lon -45.89
! São José dos Campos com 288 km^2 :           CO     HC     NOx     SOx     MP
! São José dos Campos :total (1000t/ano)TOTAL 44.21 11.18   12.86    12.31   2.69
!     CO_SJC = 0.12 * 1.e6 ! = 44.21 * 1.e6 / 365.
!    NOX_SJC = 0.035* 1.e6 ! = 12.86 * 1.e6 / 365.
!    name_city(3)='SAOJOSECAMPOS'
!    AREA_SJC = 288 * 1.e6 ! m^2
!    rrlat= -23.18
!    rrlon= -45.89
!
!-- Região Metropolitana de Campinas
!A estimativa das emissões para a Região Metropolitana de Campinas considerou os seguintes municípios:
!Americana, Artur Nogueira, Campinas, Cosmópolis, Engenheiro Coelho, Estiva Gerbi, Holambra,
!Hortolândia, Indaiatuba, Itapira, Jaguariúna, Limeira, Mogi-Guaçu, Mogi-Mirim, Monte-Mor, Nova Odessa,
!Paulínia, Pedreira, Santa Bárbara do Oeste, Santo Antônio da Posse, Sumaré, Valinhos e Vinhedo. Muitos
!desses municípios possuem alto grau de industrialização, de serviços e desenvolvimento agrícola. Todas
!essas atividades trouxeram diversos problemas de ordem ambiental. Destacam-se a cidade de Campinas,
! com uma população em torno de 970 mil habitantes, considerada como a sede da região, e responsável por
!cerca de 17% da produção industrial do Estado, e o município de Paulínia, que conta com um grande parque
!industrial. Assim como na RMSP, a região conta ainda com uma frota de veículos que é responsável por
!uma parte significativa da poluição atmosférica.

!   EMISSÃO (1000 t/ano) CO  HC    NOX      SOX  MP
!                  TOTAL 285.36 65.91 64.69 23.84 10.15
!
!
! Campinas : lat -22.91   lon -47.06
! Campinas com 796 km^2 :	    CO     HC	  NOx	  SOx	  MP
! Campinas :total (1000t/ano)TOTAL 285.36 65.91 64.69 23.84 10.15
!    ispc= CO
!     CO_CP = 0.78 * 1.e6 ! =  285.36 * 1.e6 / 365.
!    NOX_CP = 0.177* 1.e6 ! =  64.69  * 1.e6 / 365.
!    name_city(4)='CAMPINAS'
!    AREA_CP = 796 * 1.e6 ! m^2
!    rrlat= -22.91
!     rrlon= -47.06
