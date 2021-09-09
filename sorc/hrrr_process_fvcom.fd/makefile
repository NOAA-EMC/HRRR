# makefile for porcess_fvcom
#!/bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
#NWPLIBS = ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}

W3FLAGS = -I/${W3EMC_INC4}
INCLD= -I${NETCDF}/include
LIBS =  ${NWPLIBS} \
        ${NETCDF}/lib/libnetcdff.a ${NETCDF}/lib/libnetcdf.a \
        ${HDF5}/lib/libhdf5hl_fortran.a ${HDF5}/lib/libhdf5_hl.a ${HDF5}/lib/libhdf5_fortran.a ${HDF5}/lib/libhdf5.a /usr/lib64/libz.a


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
