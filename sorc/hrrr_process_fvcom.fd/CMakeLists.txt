cmake_minimum_required (VERSION 2.8)

file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE ${EXEfvcom})
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ncio ${NETCDFLIB} ${GSILIB} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO} ${LIBBUFR})
