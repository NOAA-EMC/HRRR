# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options
include configure.wrf


OBJS =  \
       da_module_couple_uv.o \
       da_netcdf_interface.o  \


all: da_update_bc.exe

#$(WRF_SRC_ROOT_DIR)/external/io_netcdf/libwrfio_nf.a
da_update_bc.exe : ${OBJS} da_update_bc.o
	$(SFC) $(LDFLAGS) -o $@ da_update_bc.o \
           da_netcdf_interface.o \
           da_module_couple_uv.o $(LIB_EXTERNAL)

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${DM_FC} ${FLAGS} ${INCLD} -c $<

.f90.o:
	$(RM) $@
	$(SED_FTN) $*.f90 > $*.b
	$(CPP) $(CPPFLAGS) $(OMPCPP) $(FPPFLAGS) -I${WRF_SRC_ROOT_DIR}/inc  -I${NETCDFPATH}/include $*.b  > $*.f
	$(RM) $*.b
	$(DM_FC) -o $@ -c $(FCFLAGS) $(PROMOTION) -I${NETCDFPATH}/include $*.f

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod *.f
