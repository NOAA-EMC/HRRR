cmake_minimum_required(VERSION 2.8)

file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
file(GLOB src_c ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
set_source_files_properties(${src_c} PROPERTIES COMPILE_FLAGS ${C_FLAGS})

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE ${EXEcloud})
add_executable(${EXE} ${src_c} ${src_all} )
#set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${wrfframe} ${NETCDFLIB} ${GSILIB} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO} ${LIBBUFR} ${MPILIB} )



