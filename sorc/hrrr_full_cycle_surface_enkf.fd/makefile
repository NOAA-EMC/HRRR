# makefile for full_cycle_surface 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
#CPP      = /lib/cpp -P
CC       = cc
ARCH     =
FLAGS = -qopenmp
CPPFLAGS = # -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE

NWPLIBS = ${W3NCO_LIB4} ${BACIO_LIB4}
INCLD = -I${NETCDF}/include -I${WRF_IO_INC}
LIBS =  ${NWPLIBS} $(WRF_IO_LIB) -L${NETCDF}/lib -lnetcdf -lnetcdff

OBJS = kinds.o update_netcdf_mass.o

all: hrrr_full_cycle_surface_enkf

hrrr_full_cycle_surface_enkf: ${OBJS} full_cycle_surface_netcdf_mass.o
	${FC} -o ../../exec/hrrr_full_cycle_surface_enkf ${FLAGS} full_cycle_surface_netcdf_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_full_cycle_surface
