ifort -o exe-4 gausslat_driver.f90 gausslat.f -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -real-size 32 /global/noscrub/Shrinivas.Moorthi/para/lib/libsp_4.a
ifort -o exe-8 gausslat_driver.f90 gausslat.f  -openmp -convert big_endian -assume byterecl -fp-model strict -i8 -r8 /global/noscrub/Shrinivas.Moorthi/para/lib/libsp_8.a
ifort  -o exe-d gausslat_driver.f90 gausslat.f  -openmp -convert big_endian -assume byterecl -fp-model strict -i4 -r8 /global/noscrub/Shrinivas.Moorthi/para/lib/libsp_d.a
echo "./exe-4__________________________________________________________________________________________"
./exe-4
echo "./exe-8__________________________________________________________________________________________"
./exe-8
echo "./exe-d___________________________________________________________________________________________"
./exe-d
