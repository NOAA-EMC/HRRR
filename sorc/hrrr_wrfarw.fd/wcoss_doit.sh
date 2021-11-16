#! /bin/sh

# Most of this is stolen from the HWRF v12.0.2

module purge

# System and compiler prereqs:
module load envvar/1.0
module load PrgEnv-intel/8.1.0
module load craype
module load intel/19.1.3.304
module load cray-mpich/8.1.7
module load hdf5/1.10.6
module load netcdf/4.7.4
module load pnetcdf/1.12.2

#Huge pages
#module load craype-hugepages256M


export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_DFI_RADAR=1
export WRF_SMOKE=1

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

