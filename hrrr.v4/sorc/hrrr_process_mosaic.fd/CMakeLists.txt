cmake_minimum_required(VERSION 2.8)

file(GLOB src_all  netCDFsub.f90 netCDFsub_geo.f90 process_NSSL_mosaic.f90 read_ncep_binary.f90 read_nssl_binary.f90 read_NSSL_mosaic.f90 write_bufr_ref.f90 read_grib2_mod.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} ${G2INC} )

set(EXE "process_NSSL_mosaic.exe")
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2_WPS} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${MPILIB} ${LIBBUFR} ${LIBBACIO} ${LIBSP} ${LIBIOMP5} )

# set(EXE "read_bufr.exe")
# add_executable(${EXE} readbufr.f90)
# set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
# target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${LIBBUFR} )

# set(EXE "read_grib2.exe")
# add_executable(${EXE} read_grib2.f90)
# set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
# target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${LIBBACIO} )

# set(EXE "write_bufr.exe")
# add_executable(${EXE} write_bufr_ref.f90 write_bufr_test.f90)
# set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
# target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${LIBBUFR} )


