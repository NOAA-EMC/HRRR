# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I${NETCDF}/include -I../../../include
LIBS =  -L${WRFDIR}/external/io_netcdf -lwrfio_nf -L${NETCDF}/lib -lnetcdf \
        -L${UPPLIBDIR}  -lw3 -lip_4 -lsp_d

OBJS = process_NESDIS_imssnow.o netCDFsub_geo.o update_SNOWICE_netcdf_mass.o

all: process_NESDIS_imssnow

process_NESDIS_imssnow: ${OBJS}
	${FC} -o process_NESDIS_imssnow.exe ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
