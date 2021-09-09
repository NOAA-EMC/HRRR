cmake_minimum_required(VERSION 2.8)
file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
file(GLOB src_c ${CMAKE_CURRENT_SOURCE_DIR}/*.c)
set_source_files_properties(${src_c} PROPERTIES COMPILE_FLAGS ${C_FLAGS})

include_directories( ${GSIINC} ${NETCDFINC} )

ADD_EXECUTABLE(process_Lightning_bufr.exe ${src_c} ${src_all} )
TARGET_LINK_LIBRARIES(process_Lightning_bufr.exe ${NETCDFLIB} ${LIBBUFR} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${LIBBACIO} ${MPILIB} ${CONSTANTS})
