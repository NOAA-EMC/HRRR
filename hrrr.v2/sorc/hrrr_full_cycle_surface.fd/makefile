# makefile for full_cycle_surface 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE

WRFPATH  = /nwprod/sorc/wrf_shared.v1.1.0
INCLD = ${NETCDF_INCLUDE} -I$(WRFPATH)/frame
NWPLIBS = ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}
LIBS =  ${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        ${NETCDF_LDFLAGS_F}

OBJS = kinds.o update_netcdf_mass.o

all: hrrr_full_cycle_surface

hrrr_full_cycle_surface: ${OBJS} full_cycle_surface_netcdf_mass.o
	${FC} -o ../../exec/hrrr_full_cycle_surface ${FLAGS} full_cycle_surface_netcdf_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_full_cycle_surface
