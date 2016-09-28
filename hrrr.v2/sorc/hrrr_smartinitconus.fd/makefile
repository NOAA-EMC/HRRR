SHELL=/bin/sh
#
# This makefile was produced by /usr/bin/fmgen at 10:56:31 AM on 11/10/98
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=   GETGRIB.f MAIN.f NDFDgrid.f BOUND.f GRIBIT.f GET_BITS.f w3fi71.f 
OBJS=   GETGRIB.o MAIN.o NDFDgrid.o BOUND.o GRIBIT.o GET_BITS.o w3fi71.o
# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC       = ifort -openmp -convert big_endian 
CPP      = /lib/cpp -P

FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS)
FFLAGST  = $(OPTS) $(LIST) $(FREE) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS)
LDFLAGS  = $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}
INC = -I${G2_INC4} -I${G2TMPL_INC}
CMD =	        ../../exec/hrrr_smartinit_conus
PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
# add -C after qsource
# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
