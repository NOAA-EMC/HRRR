#! /bin/sh

# Most of this is stolen from the HWRF v12.0.2

module purge

# These get unset by the module purge:
module use /opt/cray/ari/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/modulefiles/

# System and compiler prereqs:
module load modules/3.2.6.7
module load switch/1.0-1.0502.57058.1.58.ari
module load craype-network-aries
module load ncep/1.0
module load xt-lsfhpc/9.1.3
module load craype/2.3.0
module load PrgEnv-intel/5.2.56
module load craype-haswell
module switch intel intel/15.0.3.187
module load hpss/4.1.0.3
module load dvs
module load eswrap

# Load iobuf module to add buffering to unbuffered I/O in
# applications:
module load iobuf/2.0.6

#PNetCDF:
module load PNetCDF-intel-haswell/1.5.0
#export PNETCDF=/gpfs/hps/usrx/local/prod/PNetCDF/1.5.0/intel/haswell/

#NetCDF:
module load HDF5-serial-intel-haswell/1.8.9
module load NetCDF-intel-haswell/4.2
#export NETCDF=/gpfs/hps/usrx/local/prod/NetCDF/4.2/intel/haswell/

#Huge pages
module load craype-hugepages256M

export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_DFI_RADAR=1
export WRF_SMOKE=1

#export NETCDF=/gpfs/hps/usrx/local/prod/NetCDF/4.2/intel/haswell/
#export PNETCDF=/gpfs/hps/usrx/local/prod/PNetCDF/1.5.0/intel/haswell/
#export HDF5=/gpfs/hps/usrx/local/prod/HDF5/1.8.9/serial/intel/haswell
#export HDF5_LDFLAGS="-L${HDF5}/lib -lhdf5_hl -lhdf5hl_fortran -lhdf5 -lhdf5_fortran -lz";
export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdff -lnetcdf -lz ${HDF5_LDFLAGS}";

set -x
which ifort
which icc
which ftn
which cc
ifort --version
icc --version
ftn --version
cc --version
set +x

cd WRFV3.9

if [[ "$1" == configure ]] ; then
    ./clean -aa
    ./clean -a
    ./clean
    find . -name '*.o' -o -name '*.a' -o -name '*.mod' | xargs rm -f
    ( echo 51 ; echo 1 ) | ./configure -hyb
    cat configure.wrf.useme > configure.wrf
    set -e
    test -s configure.wrf
elif [[ "$1" == compile ]] ; then
    rm -f main/*.exe
    ./compile -j 16 em_real
    if [[ ! -s main/wrf.exe ]] ; then
        ./compile -j 1 em_real
    fi
    set -e
    test -s main/wrf.exe
else
    echo Do you want to configure or compile \?
fi

