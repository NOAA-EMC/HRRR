# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
FLAGS    = -O3 -fp-model precise -assume byterecl -fpe0 -DLINUX
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${BUFR_LIBd} ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${BACIO_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4}

INCLD=  -I${NETCDF_INCLUDE} -I${WRF_IO_INC} -I${G2_INC4} -I${G2TMPL_INC}
LIBS =  ${WRF_IO_LIB} -L${NETCDF}/lib -lnetcdf -lnetcdff ${NWPLIBS}

OBJS = kinds.o constants.o write_bufr_ref.o process_NSSL_mosaic.o \
          netCDFsub.o netCDFsub_geo.o read_nssl_binary.o \
          read_grib2_mod.o read_ncep_binary.o

OBJSENKF = kinds.o constants.o DART.o write_bufr_ref.o process_NSSL_mosaic_to_dart_and_netcdf.o \
          netCDFsub.o netCDFsub_geo.o read_nssl_binary.o write_netcdf_ref.o \
          read_grib2_mod.o read_ncep_binary.o

all: process_mosaic process_mosaic_enkf

process_mosaic: ${OBJS}
	${FC} -o ../../exec/hrrr_process_mosaic ${FLAGS} ${OBJS} ${LIBS} 

process_mosaic_enkf: ${OBJSENKF}
	${FC} -o ../../exec/hrrr_process_mosaic_enkf ${FLAGS} ${OBJSENKF} ${LIBS} 

read_bufr: readbufr.o
	${FC} -o read_bufr.exe ${FLAGS} readbufr.o ${LIBS} 

write_bufr: write_bufr_ref.o write_bufr_test.o
	${FC} -o write_bufr.exe ${FLAGS} write_bufr_ref.o write_bufr_test.o  ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_process_mosaic hrrr_process_mosaic_enkf
