SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libgsdcloud.a
INCMOD  = ../../../include

include make.filelist

.SUFFIXES: .f90 .o

.f90.o:
	$(SF90) $(FFLAGS_DEFAULT) -c $<


all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) -ruv $(LIB) $(OBJ_FILES)

.f90.a:
	$(SF90) -c $(FFLAGS_CLOUD) $<

.c.a:
	$(SCC) -c $(CFLAGS) $<

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	$(RM) -f *.o *.mod $(LIB)

