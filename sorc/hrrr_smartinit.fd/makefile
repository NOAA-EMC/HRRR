SHELL=/bin/sh
#

OBJS1= aset3d.o aset2d.o grddef.o constants.o asetdown.o rdgrib.o GETGRIB.o MAIN.o NDFDgrid.o BOUND.o WRITE_GRIB.o CALC_FLDS.o

FC =   ftn -convert big_endian -g -traceback 

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
       ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

INCG2=-I${G2_INC4} -I${G2TMPL_INC}

NCEP_LIB_DIR =

FFLAGS = -O2   ## -debug all  ## -fp-model strict -list
#LDFLAGS =-FR -assume byterecl # -check bounds -g # -check all -debug all -warn nounused  
LDFLAGS =-FR -assume byterecl

CMD =  ../../exec/hrrr_smartinit

PROFLIB =	-lprof

all:		$(CMD)

$(OBJS1): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90

$(CMD):		$(OBJS1)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS1) $(LIBS)

clean:
	-rm -f $(OBJS1) *.mod
