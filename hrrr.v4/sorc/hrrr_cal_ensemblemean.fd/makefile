# makefile for full_cycle_surface 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
DEBUG    = -O0 -debug all -check all -traceback
OPTIM    = -O3
FFLAGS   = -I$(NETCDF)/include -I$(PNETCDF)/include -I. $(OPTIM)
LIBS = -L$(NETCDF)/lib -lnetcdff -lnetcdf -L$(PNETCDF)/lib -lpnetcdf -L$(HDF5)/lib \
	-L$(HDF5)/lib -lhdf5_hl -lhdf5 -lz
# LIBS =  $(NETCDF)/lib/libnetcdff.a $(NETCDF)/lib/libnetcdf.a  $(PNETCDF)/lib/libpnetcdf.a \
#         $(HDF5)/lib/libhdf5hl_fortran.a $(HDF5)/lib/libhdf5_hl.a $(HDF5)/lib/libhdf5_fortran.a $(HDF5)/lib/libhdf5.a /usr/lib64/libz.a

all: ../../exec/hrrr_cal_ensmean
clean:
	rm -f hrrr_cal_ensmean *.o *~

average_netcdf_files_parallel.o: average_netcdf_files_parallel.f90
	$(FC) $(FFLAGS) -c -o average_netcdf_files_parallel.o average_netcdf_files_parallel.f90

../../exec/hrrr_cal_ensmean: average_netcdf_files_parallel.o makefile
	$(FC) $(FFLAGS) -o ../../exec/hrrr_cal_ensmean average_netcdf_files_parallel.o $(LIBS) 
