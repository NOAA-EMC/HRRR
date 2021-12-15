# makefile for full_cycle_surface 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
DEBUG    = -O0 -debug all -check all -traceback
OPTIM    = -O3
HDF5 = /apps/prod/hdf5-parallel/1.10.6/intel/19.1.3.304/cray-mpich/8.1.4
PNETCDF = /apps/prod/pnetcdf/1.12.2/intel/19.1.3.304/cray-mpich/8.1.4
FFLAGS   = -I$(NETCDF)/include -I$(PNETCDF)/include -I. $(OPTIM)
LIBS = -L$(NETCDF)/lib -lnetcdff -lnetcdf -L$(PNETCDF)/lib -lpnetcdf \
	-L$(HDF5)/lib -lhdf5_hl -lhdf5 -lz

all: ../../exec/hrrr_cal_ensmean
clean:
	rm -f hrrr_cal_ensmean *.o *~

average_netcdf_files_parallel.o: average_netcdf_files_parallel.f90
	$(FC) $(FFLAGS) -c -o average_netcdf_files_parallel.o average_netcdf_files_parallel.f90

../../exec/hrrr_cal_ensmean: average_netcdf_files_parallel.o makefile
	$(FC) $(FFLAGS) -o ../../exec/hrrr_cal_ensmean average_netcdf_files_parallel.o $(LIBS) 
