# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

#JASPER_LIB   = /gpfs/hps/nco/ops/nwtest/lib/crtm/v2.1.3/intel/libcrtm_v2.1.3.a
#PNG_LIB   = /gpfs/hps/nco/ops/nwtest/lib/crtm/v2.1.3/intel/libcrtm_v2.1.3.a
#WRFPATH    = /nwprod/sorc/wrf_shared.v1.1.0
WRFPATH    = /gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel
FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
FLAGS    = -O0 -DLINUX
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${BACIO_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4}

INCLD=  ${NETCDF_INCLUDE} -I$(WRFPATH)/frame -I${G2_INC4} -I${G2TMPL_INC} 
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        ${NETCDF_LDFLAGS_F} 

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
