# makefile for process_imssnow
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
#ARCH     =
FLAGS = -qopenmp
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${IP_LIB4} ${SP_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}

INCLD=  -I${NETCDF}/include -I${WRF_IO_INC} -I${G2_INC4} -I${G2TMPL_INC} -I${W3NCO_INC4} -I${IP_INC4}
LIBS =  ${WRF_IO_LIB} ${NWPLIBS} -L${NETCDF}/lib -lnetcdf -lnetcdff

OBJS = kinds.o grib2_read_mod.o process_NESDIS_imssnow.o netCDFsub_geo.o update_SNOWICE_netcdf_mass.o

all: process_NESDIS_imssnow

process_NESDIS_imssnow: ${OBJS} process_NESDIS_imssnow.o
	${FC} -o ../../exec/hrrr_process_imssnow ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_process_imssnow
