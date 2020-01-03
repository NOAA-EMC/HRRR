#!/bin/csh -x
#set C="/usrx/local/nceplibs/"
ifort -o exe-4 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 -fpp -DLINUX ${NCEPLIBS}/libsp_v2.0.2_4.a 
ifort -o exe-d unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -r8 -fpp -DLINUX ${NCEPLIBS}/libsp_v2.0.2_d.a
ifort -o exe-8 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -fpp -DLINUX -i8 -r8 ${NCEPLIBS}/libsp_v2.0.2_8.a
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d

