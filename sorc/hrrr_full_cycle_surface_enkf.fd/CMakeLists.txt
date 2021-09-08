cmake_minimum_required(VERSION 2.8)

file(GLOB src_all ${CMAKE_CURRENT_SOURCE_DIR}/*.f90)
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE "full_cycle_surface_enkfd02.exe")
add_executable(${EXE} update_netcdf_mass.f90 full_cycle_surface_netcdf_mass.f90)
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${NETCDFLIB} ${GSILIB} ${LIBW3EMC_UPP} ${LIBW3NCO_UPP} ${MPILIB} )

