# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ifort
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
OPTS     = -O3 -assume byterecl -convert big_endian
LIST     =
FREE     = -FR
TRAPS    =
PROFILE  =
DEBUG    = # -g # -traceback -fpe0 -ftrapuv # -check all
MEM      =
MAP      =
ESSL     =
MASS     =

WRFPATH    = ${BASE}/hrrr_wrf.fd/WRFV3.6_serialnetcdf_noomp

INCLD = ${NETCDF_INCLUDE}

NETCDF_LIB = ${NETCDF}/lib/libnetcdf.a

LIBS =  ${NETCDF_LIB} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a

OBJS = da_module_couple_uv.o da_netcdf_interface.o

all: da_update_bc

da_update_bc: ${OBJS} da_update_bc.o
	${FC} -o ../../exec/hrrr_update_bc ${FLAGS} da_update_bc.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_update_bc
