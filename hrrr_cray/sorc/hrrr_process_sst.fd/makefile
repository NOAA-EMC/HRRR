# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}

WRFPATH  = /gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel
INCLD= -I${NETCDF}/include -I$(WRFPATH)/frame
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        -L${NETCDF}/lib -lnetcdf

OBJS = kinds.o process_SST.o sst14k.o sstGlobal2RR.o netCDFsub_geo.o update_SST_netcdf_mass.o

all: process_SST

process_SST: ${OBJS}
	${FC} -o ../../exec/hrrr_process_sst ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<


clean:
	/bin/rm -f *.o *.exe *.mod hrrr_process_sst
