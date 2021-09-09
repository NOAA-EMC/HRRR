cmake_minimum_required(VERSION 2.8)

file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${G2INC} ${NETCDFINC} )

set(EXE ${EXEsst})
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${NETCDFLIB} ${LIBG2_WPS} ${LIBJASPER} ${LIBPNG} ${LIBZ} ${LIBBACIO_UPP} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO_UPP})



