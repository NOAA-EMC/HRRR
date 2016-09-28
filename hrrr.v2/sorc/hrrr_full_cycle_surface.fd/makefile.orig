# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I${NETCDF}/include -I../../../include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf -L../../../lib -lw3emc_i4r8 -lw3nco_i4r8

OBJS = update_netcdf_mass.o update_netcdf_mass_snowT.o

all: full_cycle_surface 

full_cycle_surface: full_cycle_surface_netcdf_mass.o ${OBJS}
	${FC} -o full_cycle_surface.exe ${FLAGS} full_cycle_surface_netcdf_mass.o ${OBJS} ${LIBS} 

full_cycle_snow: full_cycle_netcdf_mass.o ${OBJS}
	${FC} -o full_cycle_snow.exe ${FLAGS} full_cycle_netcdf_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
