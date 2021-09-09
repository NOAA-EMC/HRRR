cmake_minimum_required(VERSION 2.8)

file(GLOB src_all nc_readwrt_mod.f90 grib2_read_mod.f90)
#set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} ${G2INC} )

set(EXE "update_GVF.exe")
add_executable(${EXE} ${src_all} update_GVF.f90  )
#set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${MPILIB} ${LIBBUFR} ${LIBBACIO} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP})


set(EXE "gen_annual_maxmin_GVF.exe")
add_executable(${EXE} ${src_all} gen_annual_maxmin_GVF.f90)
#set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${MPILIB} ${LIBBUFR} ${LIBBACIO} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP})
#target_link_libraries( ${EXE} ${NETCDFLIB} ${GSILIB} ${LIBG2} ${LIBG2TMPL} ${LIBZ} ${LIBPNG} ${LIBJASPER} ${MPILIB} ${LIBBUFR} ${LIBBACIO} ${LIBW3EMC} ${LIBW3NCO} )
