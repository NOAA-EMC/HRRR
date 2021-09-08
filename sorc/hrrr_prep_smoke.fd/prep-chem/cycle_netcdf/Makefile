SHELL=/bin/sh
TARGET=cycle_netcdf.x

# These four will be overridden by the caller:
FFLAGS=
LDFLAGS=
LIBS=
FC=fc

SOURCES=module_error.f90 module_types.f90 module_att_info.f90 \
        module_var_info.f90 module_io.f90 cycle_netcdf.f90

all: $(TARGET)

clean:
	rm -f $(TARGET) *.o *.mod *.a *~

$(TARGET): $(SOURCES)
	$(FC) $(FFLAGS) $(SOURCES) -o $(TARGET) $(LDFLAGS) $(LIBS)
