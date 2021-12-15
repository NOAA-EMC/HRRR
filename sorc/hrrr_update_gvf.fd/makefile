# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
FLAGS    = -O0 -DLINUX
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${BACIO_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4}

INCLD=  ${NETCDF_INCLUDE} -I${WRF_IO_INC} -I${G2_INC4} -I${G2TMPL_INC} 
LIBS =  -L${NWPLIBS} ${WRF_IO_LIB} \
        -L${NETCDF}/lib -lnetcdf -lnetcdff

OBJS = nc_readwrt_mod.o grib2_read_mod.o

all: update_GVF gen_annual_maxmin_GVF

update_GVF: ${OBJS} update_GVF.o
	${FC} -o ../../exec/hrrr_update_gvf ${FLAGS} update_GVF.o ${OBJS} ${LIBS} 

gen_annual_maxmin_GVF: ${OBJS} gen_annual_maxmin_GVF.o
	${FC} -o gen_annual_maxmin_GVF.exe ${FLAGS} gen_annual_maxmin_GVF.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod hrrr_update_gvf
