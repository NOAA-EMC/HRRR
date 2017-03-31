# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFPATH    = /nwprod/sorc/wrf_shared.fd
FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS =/nwprod/lib -lbufr_d_64

INCLD=  -I${NETCDF}/include -I$(WRFPATH)/frame
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        -L${NETCDF}/lib -lnetcdf
# cio
OBJS = kinds.o constants.o module_debug.o gridmod_gsi_map_utils.o \
       misc_definitions_module.o constants_module.o module_map_utils.o \
       write_bufr_NASALaRC.o process_NASALaRC_cloud.o \
       netCDFsub.o netCDFsub_geo.o cio.o

all: process_NASALaRC_cloud

process_NASALaRC_cloud: ${OBJS}
	${FC} -o ../../exec/hrrr_process_cloud ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_process_cloud
