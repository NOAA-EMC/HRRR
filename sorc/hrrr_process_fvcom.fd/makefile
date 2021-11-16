# makefile for porcess_fvcom
#!/bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${BUFR_LIBd} ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${BACIO_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4}
HDF5 = /apps/prod/hdf5-parallel/1.10.6/intel/19.1.3.304/cray-mpich/8.1.4

INCLD= -I${NETCDF}/include
LIBS =  ${NWPLIBS} \
        ${NETCDF}/lib/libnetcdff.a ${NETCDF}/lib/libnetcdf.a \
        -L${HDF5}/lib -lhdf5_hl -lhdf5 -lz ${Z_LIB}

OBJS = kinds.o module_map_utils.o module_ncio.o  module_nwp_base.o  module_nwp.o process_FVCOM.o

all: process_FVCOM

process_FVCOM: ${OBJS}
	${FC} -o ../../exec/hrrr_process_fvcom ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<


clean:
	/bin/rm -f *.o *.exe *.mod hrrr_process_fvcom
