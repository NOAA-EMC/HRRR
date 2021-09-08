SHELL=/bin/sh
TARGET=fires_ncfmake.x

# These four will be overridden by the caller:
FFLAGS=
LDFLAGS=
LIBS=
FC=fc

all: $(TARGET)

clean:
	rm -f $(TARGET) *.o *.mod *.a *~

$(TARGET): fires_ncfmake.f90  manipufiles.inc  netcdf_writroutines.inc  species.inc
	$(FC) $(FFLAGS) fires_ncfmake.f90 -o $(TARGET) $(LDFLAGS) $(LIBS)
