SHELL=/bin/sh
#
SRCS=	BFRHDR.f BFRIZE.f CALHEL.f CALWXT.f SMIX.f SNDPST_EM.f VAP.f \
        WETBLB.f CALVIS.f CALWXT_RAMER.f CALWXT_EXPLICIT.f \
        CALWXT_REVISED.f CALWXT_BOURG.f CALWXT_DOMINANT.f

OBJS=	BFRHDR.o BFRIZE.o CALHEL.o CALWXT.o SMIX.o SNDPST_EM.o VAP.o \
        WETBLB.o CALVIS.o CALWXT_RAMER.o CALWXT_EXPLICIT.o \
        CALWXT_REVISED.o CALWXT_BOURG.o CALWXT_DOMINANT.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =            ftn -qopenmp -convert big_endian -g -traceback
LDFLAGS = 
CMD = ../../exec/hrrr_sndp
LIBS = ${BUFR_LIB4}

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:	 $(CMD) delete

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

delete:
	@rm  *.o
