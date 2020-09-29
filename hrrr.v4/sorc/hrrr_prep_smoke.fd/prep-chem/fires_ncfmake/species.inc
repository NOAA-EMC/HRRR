MODULE species
!
! 3-D species from input binary file - correspondence with output netcdf file done in manipufiles.f

     ! Output to NetCDF diagnostic file
     integer, parameter :: num3dnew = 7
!    integer, parameter :: num3dnew = 34
!    integer, parameter :: p_so2 = 1
!    integer, parameter :: p_no2 = 2
!    integer, parameter :: p_no = 3
     integer, parameter :: p_no = 1
!    integer, parameter :: p_ald = 4
!    integer, parameter :: p_hcho = 5
!    integer, parameter :: p_ora2 = 6
!    integer, parameter :: p_nh3 = 7
!    integer, parameter :: p_hc3 = 8
!    integer, parameter :: p_hc5 = 9
!    integer, parameter :: p_hc8 = 10
!    integer, parameter :: p_eth = 11
!    integer, parameter :: p_co = 12
     integer, parameter :: p_co = 2
!    integer, parameter :: p_olt = 13
!    integer, parameter :: p_oli = 14
!    integer, parameter :: p_tol = 15
!    integer, parameter :: p_xyl = 16
!    integer, parameter :: p_ket = 17
!    integer, parameter :: p_csl = 18
!    integer, parameter :: p_iso = 19
!    integer, parameter :: p_pm25 = 20  ! inert pm2.5 only
!    integer, parameter :: p_pm10 = 21
!    integer, parameter :: p_oc = 22
!    integer, parameter :: p_bc = 23
     integer, parameter :: p_pm25 = 3  ! inert pm2.5 only
     integer, parameter :: p_pm10 = 4
     integer, parameter :: p_oc = 5
     integer, parameter :: p_bc = 6
     integer, parameter :: p_smoke = 7
!    integer, parameter :: p_dms = 24
!    integer, parameter :: p_so4 = 25
!    integer, parameter :: p_ash = 26
!    integer, parameter :: p_ete = 27
!    integer, parameter :: p_ch4 = 28
!    integer, parameter :: p_co2 = 29
!    integer, parameter :: p_api = 30
!    integer, parameter :: p_lim = 31
!    integer, parameter :: p_ora1 = 32
!    integer, parameter :: p_dien = 33 ! Not in binary output file
!    integer, parameter :: p_macr = 34 ! Not in binary output file
     character (len=80), dimension(num3dnew),save    :: var3dnew,unit3dnew,desc3dnew
     integer, dimension(num3dnew)                    :: idvaro3dn

!
!    data var3dnew(p_so2),desc3dnew(p_so2),unit3dnew(p_so2)/ & ! so2 mole/km^2/hr
!     'ebu_in_so2','EMISSIONS','mol km^-2 hr^-1'/   ! so2 mole/km^2/hr
!    data var3dnew(p_no2),desc3dnew(p_no2),unit3dnew(p_no2)/ & ! no2 mole/km^2/hr
!     'ebu_in_no2','EMISSIONS','mol km^-2 hr^-1'/   ! no2 mole/km^2/hr
     data var3dnew(p_no),desc3dnew(p_no),unit3dnew(p_no)/ & ! no mole/km^2/hr
      'ebu_in_no','EMISSIONS','mol km^-2 hr^-1'/   ! no mole/km^2/hr
!    data var3dnew(p_ald),desc3dnew(p_ald),unit3dnew(p_ald)/ & ! ald mole/km^2/hr
!     'ebu_in_ald','EMISSIONS','mol km^-2 hr^-1'/   ! ald mole/km^2/hr
!    data var3dnew(p_hcho),desc3dnew(p_hcho),unit3dnew(p_hcho)/ & ! hcho mole/km^2/hr
!     'ebu_in_hcho','EMISSIONS','mol km^-2 hr^-1'/   ! hcho mole/km^2/hr
!    data var3dnew(p_ora2),desc3dnew(p_ora2),unit3dnew(p_ora2)/ & ! ora2 mole/km^2/hr
!     'ebu_in_ora2','EMISSIONS','mol km^-2 hr^-1'/   ! ora2 mole/km^2/hr
!    data var3dnew(p_nh3),desc3dnew(p_nh3),unit3dnew(p_nh3)/ & ! nh3 mole/km^2/hr
!     'ebu_in_nh3','EMISSIONS','mol km^-2 hr^-1'/   ! nh3 mole/km^2/hr
!    data var3dnew(p_hc3),desc3dnew(p_hc3),unit3dnew(p_hc3)/ & ! hc3 mole/km^2/hr
!     'ebu_in_hc3','EMISSIONS','mol km^-2 hr^-1'/   ! hc3 mole/km^2/hr
!    data var3dnew(p_hc5),desc3dnew(p_hc5),unit3dnew(p_hc5)/ & ! hc5 mole/km^2/hr
!     'ebu_in_hc5','EMISSIONS','mol km^-2 hr^-1'/   ! hc5 mole/km^2/hr
!    data var3dnew(p_hc8),desc3dnew(p_hc8),unit3dnew(p_hc8)/ & ! hc8 mole/km^2/hr
!     'ebu_in_hc8','EMISSIONS','mol km^-2 hr^-1'/   ! hc8 mole/km^2/hr
!    data var3dnew(p_eth),desc3dnew(p_eth),unit3dnew(p_eth)/ & ! eth mole/km^2/hr
!     'ebu_in_eth','EMISSIONS','mol km^-2 hr^-1'/   ! eth mole/km^2/hr
     data var3dnew(p_co),desc3dnew(p_co),unit3dnew(p_co)/ & ! co mole/km^2/hr
      'ebu_in_co','EMISSIONS','mol km^-2 hr^-1'/   ! co mole/km^2/hr
!    data var3dnew(p_olt),desc3dnew(p_olt),unit3dnew(p_olt)/ & ! olt mole/km^2/hr
!     'ebu_in_olt','EMISSIONS','mol km^-2 hr^-1'/   ! olt mole/km^2/hr
!    data var3dnew(p_oli),desc3dnew(p_oli),unit3dnew(p_oli)/ & ! oli mole/km^2/hr
!     'ebu_in_oli','EMISSIONS','mol km^-2 hr^-1'/   ! oli mole/km^2/hr
!    data var3dnew(p_tol),desc3dnew(p_tol),unit3dnew(p_tol)/ & ! tol mole/km^2/hr
!     'ebu_in_tol','EMISSIONS','mol km^-2 hr^-1'/   ! tol mole/km^2/hr
!    data var3dnew(p_xyl),desc3dnew(p_xyl),unit3dnew(p_xyl)/ & ! xyl mole/km^2/hr
!     'ebu_in_xyl','EMISSIONS','mol km^-2 hr^-1'/   ! xyl mole/km^2/hr
!    data var3dnew(p_ket),desc3dnew(p_ket),unit3dnew(p_ket)/ & ! ket mole/km^2/hr
!     'ebu_in_ket','EMISSIONS','mol km^-2 hr^-1'/   ! ket mole/km^2/hr
!    data var3dnew(p_csl),desc3dnew(p_csl),unit3dnew(p_csl)/ & ! csl mole/km^2/hr
!     'ebu_in_csl','EMISSIONS','mol km^-2 hr^-1'/   ! csl mole/km^2/hr
!    data var3dnew(p_iso),desc3dnew(p_iso),unit3dnew(p_iso)/ & ! iso mole/km^2/hr
!     'ebu_in_iso','EMISSIONS','mol km^-2 hr^-1'/   ! iso mole/km^2/hr
     data var3dnew(p_pm25),desc3dnew(p_pm25),unit3dnew(p_pm25)/ & ! INERT pm25 ug/m^2/sec
      'ebu_in_pm25','EMISSIONS','ug m^-2 s^-1'/   ! INERT pm25 ug/m^2/sec
     data var3dnew(p_pm10),desc3dnew(p_pm10),unit3dnew(p_pm10)/ & ! pm10 ug/m^2/sec
      'ebu_in_pm10','EMISSIONS','ug m^-2 s^-1'/   ! pm10 ug/m^2/sec
     data var3dnew(p_oc),desc3dnew(p_oc),unit3dnew(p_oc)/ & ! oc ug/m^2/sec
      'ebu_in_oc','EMISSIONS','ug m^-2 s^-1'/   ! oc ug/m^2/sec
     data var3dnew(p_bc),desc3dnew(p_bc),unit3dnew(p_bc)/ & ! bc ug/m^2/sec
      'ebu_in_bc','EMISSIONS','ug m^-2 s^-1'/   ! bc ug/m^2/sec
     data var3dnew(p_smoke),desc3dnew(p_smoke),unit3dnew(p_smoke)/ & ! oc+bc+pm25 ug/m^2/sec
      'ebu_in_smoke','EMISSIONS','ug m^-2 s^-1'/   ! oc+bc+pm25 ug/m^2/sec
!    data var3dnew(p_dms),desc3dnew(p_dms),unit3dnew(p_dms)/ & ! dms mole/km^2/hr
!     'ebu_in_dms','EMISSIONS','mol km^-2 hr^-1'/   ! dms mole/km^2/hr
!    data var3dnew(p_so4),desc3dnew(p_so4),unit3dnew(p_so4)/ & ! so4 ug/m^2/sec
!     'ebu_in_so4','EMISSIONS','ug m^-2 s^-1'/   ! so4 ug/m^2/sec
!    data var3dnew(p_ash),desc3dnew(p_ash),unit3dnew(p_ash)/ & ! ash ug/m^2/sec
!     'ebu_in_ash','EMISSIONS','ug m^-2 s^-1'/   ! ash ug/m^2/sec
!    data var3dnew(p_ete),desc3dnew(p_ete),unit3dnew(p_ete)/ & ! ete mole/km^2/hr
!     'ebu_in_ete','EMISSIONS','mol km^-2 hr^-1'/   ! ete mole/km^2/hr
!    data var3dnew(p_ch4),desc3dnew(p_ch4),unit3dnew(p_ch4)/ & ! ch4 mole/km^2/hr
!     'ebu_in_ch4','EMISSIONS','mol km^-2 hr^-1'/   ! ch4 mole/km^2/hr
!    data var3dnew(p_co2),desc3dnew(p_co2),unit3dnew(p_co2)/ & ! co2 mole/km^2/hr
!     'ebu_in_co2','EMISSIONS','mol km^-2 hr^-1'/   ! co2 mole/km^2/hr
!    data var3dnew(p_api),desc3dnew(p_api),unit3dnew(p_api)/ & ! api mole/km^2/hr
!     'ebu_in_api','EMISSIONS','mol km^-2 hr^-1'/   ! api mole/km^2/hr
!    data var3dnew(p_lim),desc3dnew(p_lim),unit3dnew(p_lim)/ & ! lim mole/km^2/hr
!     'ebu_in_lim','EMISSIONS','mol km^-2 hr^-1'/   ! lim mole/km^2/hr
!    data var3dnew(p_ora1),desc3dnew(p_ora1),unit3dnew(p_ora1)/ & ! ora1 mole/km^2/hr
!     'ebu_in_ora1','EMISSIONS','mol km^-2 hr^-1'/   ! ora1 mole/km^2/hr
!    data var3dnew(p_dien),desc3dnew(p_dien),unit3dnew(p_dien)/ & ! dien mole/km^2/hr
!     'ebu_in_dien','EMISSIONS','mol km^-2 hr^-1'/   ! dien mole/km^2/hr
!    data var3dnew(p_macr),desc3dnew(p_macr),unit3dnew(p_macr)/ & ! macr mole/km^2/hr
!     'ebu_in_macr','EMISSIONS','mol km^-2 hr^-1'/   ! macr mole/km^2/hr
!
! 2-D species from input binary file - correspondence with output netcdf file done in manipufiles.f
     integer, parameter :: num2dnew = 6
     integer, parameter :: p_flmfrc = 1
     integer, parameter :: p_meanfrp = 2
     integer, parameter :: p_stdfrp = 3
     integer, parameter :: p_meansz = 4
     integer, parameter :: p_stdsz = 5
     integer, parameter :: ebb_smk = 6
     character (len=80), dimension(num2dnew),save    :: var2dnew,unit2dnew,desc2dnew
     integer, dimension(num2dnew)                    :: idvaro2dn,wrfinput,varid_wrfinput
!
     data wrfinput(p_flmfrc),var2dnew(p_flmfrc),desc2dnew(p_flmfrc),unit2dnew(p_flmfrc)/ & ! Tropical Forest veg frac
      0,'FLAM_FRAC','bbem2 flame fraction','?'/   ! flam_frac , bbem=2, from prep_chem_sour
     data wrfinput(p_meanfrp),var2dnew(p_meanfrp),desc2dnew(p_meanfrp),unit2dnew(p_meanfrp)/ & ! Extratropical Forest veg frac
      1,'MEAN_FRP','bbem2 mean FRP','?'/   ! mean_frp , bbem=2, from prep_chem_sour
     data wrfinput(p_stdfrp),var2dnew(p_stdfrp),desc2dnew(p_stdfrp),unit2dnew(p_stdfrp)/ & ! Savanna veg frac
      2,'STD_FRP','bbem2 FRP standard deviation','?'/   ! std_frp , bbem=2, from prep_chem_sour
     data wrfinput(p_meansz),var2dnew(p_meansz),desc2dnew(p_meansz),unit2dnew(p_meansz)/ & ! Grassland veg frac
      3,'MEAN_FSIZE','bbem2 mean Size','?'/   ! mean_size , bbem=2, from prep_chem_sour
     data wrfinput(p_stdsz),var2dnew(p_stdsz),desc2dnew(p_stdsz),unit2dnew(p_stdsz)/ & ! Tropical Forest veg frac
      4,'STD_FSIZE','bbem2 Size standard deviation','?'/   ! std_size , bbem=2, from prep_chem_sour
     data wrfinput(ebb_smk),var2dnew(ebb_smk),desc2dnew(ebb_smk),unit2dnew(ebb_smk)/ & ! surface smoke
      5,'EBB_SMOKE','ebb smoke','ug m^-2 s^-1'/   
!
! variables to be read from input netcdf and written to output netcdf, only real 3-D and 2-D variables allowed so far
     integer, parameter :: num_rw = 1
     character (len=20), dimension(num_rw),save    :: var_rw
     data var_rw/'BLANK'/  ! Variable list to trasfer directly from input to output

! Angles and Variables for Phase Functions (SAM 2/27/12)
!    integer, parameter :: NANG = 100, NANG2=2*NANG-1  ! NANG=Number of scattering angles between 0 and 90 degrees, inclusive
!    REAL*8  THETA(NANG), COSPHI(NANG2)
 
END MODULE species

