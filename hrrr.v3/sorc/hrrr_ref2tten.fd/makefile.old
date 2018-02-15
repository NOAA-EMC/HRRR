# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFDIR=/scratch1/portfolios/BMC/wrfruc/mhu/code/WRF_release/WRFV3.4_ARW
GSIDIR=/scratch1/portfolios/BMC/wrfruc/mhu/rapid_refresh/GSI_r845
FC   =  mpif90 -free
CC  = cc
FLAGS= -O3 -fp-model precise -assume byterecl -fpe0 -DLINUX #-g -traceback # -convert big_endian
CPPFLAGS =  -D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
GSILIBS =  ${GSIDIR}/src/main -lgsi #../../../lib -lw3emc_i4r8 -lw3nco_i4r8 -lbacio -lbufr_i4r8

INCLD=  -I${NETCDF}/include -I${GSIDIR}/include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf -L${GSILIBS}

OBJS = read_netcdf_mass.o BackgroundCld.o pbl_height.o vinterp_radar_ref.o \
       build_missing_REFcone.o radar_ref2tten.o smooth.o

all: ref2tten

ref2tten: ${OBJS} gsdcloudanalysis_ref2tten.o
	${FC} -o ref2tten.exe ${FLAGS} gsdcloudanalysis_ref2tten.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
