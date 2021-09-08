SHELL=/bin/sh
EXES= FRE_BBM_HRRR_AK.exe        \
      merge_FRP_HRRR_AK.exe      \
      proc_J01_FRP_HRRR_AK.exe   \
      proc_MODIS_FRP_HRRR-AK.exe \
      proc_NPP_FRP_HRRR-AK_v5.exe

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
