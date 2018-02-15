# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

FC  =  mpif90 -f90=ifort -free
CC  = cc
FLAGS= -fp-model precise -assume byterecl -g -traceback $(LDFLAGS) -DLINUX ## -convert big_endian
CPPFLAGS =  -D_UNDERSCORE -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
GSILIBS =../../../lib -lw3emc_i4r8 -lw3nco_i4r8 -lbacio -lbufr_i4r8

UPPDIR=/mnt/lfs3/projects/rtwbl/mhu/rapcode/git_local/rapid-refresh/UPP_2015/comupp

G2_INC4     = -I${UPPDIR}/include
G2TMPL_INC  = -I${UPPDIR}/include
G2_LIB4     = ${UPPDIR}/lib/libg2.a
G2TMPL_LIB  = ${UPPDIR}/lib/libg2tmpl.a
JASPER_LIB  = -L/usr/lib64 -ljasper
PNG_LIB     = -L/usr/lib64 -lpng
Z_LIB       = -L/usr/lib64 -lz
NETCDF      = /apps/netcdf/3.6.3-intel

INCLD=  -I${NETCDF}/include -I../../../include ${G2_INC4}
LIBS =  \
        ${G2_LIB4} ${G2TMPL_LIB} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
        -L${NETCDF}/lib -lnetcdf -L${GSILIBS}

OBJS = netCDFsub.o netCDFsub_geo.o write_bufr_ref.o process_NSSL_mosaic.o \
       read_nssl_binary.o read_grib2_mod.o read_ncep_binary.o # read_grib2.o

# all: process_NSSL_mosaic read_grib2
all: process_NSSL_mosaic

process_NSSL_mosaic: ${OBJS}
	${FC} -o process_NSSL_mosaic.exe ${FLAGS} ${OBJS} ${LIBS} 

read_bufr: readbufr.o
	${FC} -o read_bufr.exe ${FLAGS} readbufr.o ${LIBS} 

read_grib2: read_grib2.o
	${FC} -o read_grib2.exe ${FLAGS} read_grib2.o ${LIBS} 

write_bufr: write_bufr_ref.o write_bufr_test.o
	${FC} -o write_bufr.exe ${FLAGS} write_bufr_ref.o write_bufr_test.o  ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
