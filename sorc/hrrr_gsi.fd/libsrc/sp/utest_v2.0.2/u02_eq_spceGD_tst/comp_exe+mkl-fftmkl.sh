ifort -o exe-4 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 -mkl /gpfs/thm/u/Eugene.Mirvis/WCOSS/lib_mkl-fftmkl/libsp_4.a
ifort -o exe-8 unittest01.f90 -auto -openmp -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /gpfs/thm/u/Eugene.Mirvis/WCOSS/lib_mkl-fftmkl/libsp_8.a -mkl 
ifort  -o exe-d unittest01.f90 -auto -openmp  -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /gpfs/thm/u/Eugene.Mirvis/WCOSS/lib_mkl-fftmkl/libsp_d.a -mkl
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d

