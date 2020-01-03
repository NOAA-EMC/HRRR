cmake_minimum_required(VERSION 2.8)

file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
#file(GLOB src_all . *.90)
#set_source_files_properties(${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
#file(GLOB src_f . *.f)
#set_source_files_properties(${src_f} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
#file(GLOB src_c . *.c)
#set_source_files_properties(${src_c} PROPERTIES COMPILE_FLAGS ${C_FLAGS})
#list( APPEND src_all ${src_f} )
#list( APPEND src_all ${src_c} )

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE ${EXEref2tten})
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${NETCDFLIB} ${MPILIB} ${GSILIB} )



