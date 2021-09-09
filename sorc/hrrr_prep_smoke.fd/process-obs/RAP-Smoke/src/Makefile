SHELL=/bin/sh
EXES=\
FRE_BBM_RAP_v4.exe \
merge_FRP_RAP_v3.exe \
proc_J01_FRP_RAP_v3.exe \
proc_MODIS_FRP_RAP_v3.exe \
proc_NPP_FRP_RAP_v3.exe

# These four will be replaced by the caller:
FC=ifort
FFLAGS=-g -traceback
LDFLAGS=
LIBS=

all: $(EXES)

clean:
	rm -f *.o *.mod *.a *.exe $(EXES) *~

.SUFFIXES: .f90 .exe

%.exe : %.f90 Makefile
	$(FC) $(FFLAGS) $*.f90 -o $*.exe $(LDFLAGS) $(LIBS)
