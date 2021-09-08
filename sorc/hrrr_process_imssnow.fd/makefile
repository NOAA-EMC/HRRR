# makefile for process_imssnow
#! /bin/sh -v

# Define the fortran compiler and options

#WRFPATH=/nwprod/sorc/wrf_shared.v1.1.0
WRFPATH    = /gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel
FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${IP_LIB4} ${SP_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}
NETCDF_LIB = ${NETCDF}/lib/libnetcdf.a

INCLD=  ${NETCDF_INCLUDE} -I$(WRFPATH)/frame -I${G2_INC4}
LIBS =  ${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        ${NETCDF_LDFLAGS_F} 

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
