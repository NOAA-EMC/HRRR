#!/bin/csh -x
#set NCEPLIBS="/usrx/local/nceplibs/"
ifort -o exe-4 gausslat_driver.f90 gausslat.f -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 ${NCEPLIBS}/libsp_v2.0.2_4.a
ifort -o exe-8 gausslat_driver.f90 gausslat.f  -openmp -convert big_endian -assume byterecl -fp-model strict -i8 -r8 ${NCEPLIBS}/libsp_v2.0.2_8.a
ifort  -o exe-d gausslat_driver.f90 gausslat.f  -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -r8 ${NCEPLIBS}/libsp_v2.0.2_d.a
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d
