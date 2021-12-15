# makefile for lightning_porcess
#! /bin/sh -v

# Define the fortran compiler and options
FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${BACIO_LIB4} ${W3NCO_LIB4} ${BUFR_LIBd}

INCLD=  -I${NETCDF}/include
LIBS =  -L${NWPLIBS} -L${NETCDF}/lib -lnetcdf -lnetcdff

OBJS = kinds.o constants.o Check_Lightning_QC.o netCDFsub_geo.o cio.o \
       read_lightning_bufr.o module_debug.o misc_definitions_module.o \
       constants_module.o module_map_utils.o gridmod_gsi_map_utils.o \
       write_bufr_lght.o process_Lightning.o

all: hrrr_process_lightning

hrrr_process_lightning: ${OBJS}
	${FC} -o ../../exec/hrrr_process_lightning ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.mod hrrr_process_lightning
