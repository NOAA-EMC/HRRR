SHELL=/bin/sh
EXES=qc_modis.exe qc_viirs.exe

# These four will be replaced by the caller:
FC=ifort
FFLAGS=-traceback -g
LDFLAGS=
LIBS=

all: $(EXES)

clean:
	rm -f *.o *.mod *.a *.exe $(EXES) *~

.SUFFIXES: .f90 .exe

%.exe : %.f90 Makefile
	$(FC) $(FFLAGS) $*.f90 -o $*.exe $(LDFLAGS) $(LIBS)
