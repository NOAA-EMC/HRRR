# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFDIR=${BASE}/hrrr_wrfarw.fd/WRFV3.9
FC   = ftn
CC  = 
FLAGS= -assume byterecl
CPPFLAGS = -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -DNOUNDERSCORE
NWPLIBS = ${BUFR_LIBd} ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${BACIO_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4}

INCLD= -I${NETCDF}/include  -I${WRFDIR}/inc -I${WRFDIR}/frame
LIBS = -L${NWPLIBS} \
        $(WRFDIR)/external/io_int/libwrfio_int.a \
        $(WRFDIR)/external/io_netcdf/libwrfio_nf.a \
        $(WRFDIR)/frame/module_machine.o \
        ${WRFDIR}/frame/pack_utils.o \
	-L${NETCDF}/lib -lnetcdf -lnetcdff

OBJS = kinds.o constants.o read_netcdf_mass.o BackgroundCld.o pbl_height.o vinterp_radar_ref.o \
       build_missing_REFcone.o radar_ref2tten.o smooth.o convert_stcst2ref.o convert_lghtn2ref.o read_Lightning_cld.o

all: hrrr_ref2tten

hrrr_ref2tten: ${OBJS} gsdcloudanalysis_ref2tten.o
	${FC} -qopenmp -o ../../exec/hrrr_ref2tten ${FLAGS} gsdcloudanalysis_ref2tten.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o hrrr_ref2tten *.mod
