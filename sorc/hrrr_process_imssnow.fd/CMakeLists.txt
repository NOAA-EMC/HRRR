cmake_minimum_required(VERSION 2.8)

file(GLOB src_all  netCDFsub_geo.f90 process_NESDIS_imssnow.f90  update_SNOWICE_netcdf_mass.f90 module_wrf_error.f90 )
set_source_files_properties( ${src_all} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )

include_directories( ${GSIINC} ${NETCDFINC} )

set(EXE ${EXEsnow})
add_executable(${EXE} ${src_all})
set_target_properties( ${EXE} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS} )
target_link_libraries( ${EXE} ${wrflib} ${NETCDFLIB} ${LIBBACIO_UPP} ${LIBIP} ${LIBSP} ${LIBW3NCO_UPP} ${LIBW3EMC_UPP} )

