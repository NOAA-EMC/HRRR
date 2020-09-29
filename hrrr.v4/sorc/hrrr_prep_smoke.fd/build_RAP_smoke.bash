#! /bin/bash

EXECrap="$1"
if [[ -z "$1" ]] ; then
    echo 'Syntax: build_RAP_smoke.sh /path/to/RAP/exec' 1>&2
    exit 1
fi

set -xue

mkdir -p "$EXECrap"
pushd "$EXECrap"
EXECrap=$( pwd -P )
popd

set +xu
if [[ -d /lfs1 ]] ; then
    where=jet
    module purge
    module load intel
    module load szip
    module load hdf5
    module load netcdf
else
    # Assume WCOSS Cray
    where=wcoss_cray
    module purge
    module load intel
    module load NetCDF-intel-haswell/4.2
    module load HDF5-serial-intel-haswell/1.8.9
fi
set -xu

pushd prep-chem/fires_ncfmake/
make clean
if [[ "$where" == jet ]] ; then
    ./mk-fv3-jet
else
    ./mk-wrf-wcoss-cray
fi
cp -fp "fires_ncfmake.x" $EXECrap/rap_fires_ncfmake
popd

pushd prep-chem/Prep_smoke_FRP/bin/build/
make clean
if [[ "$where" == jet ]] ; then
    ./mk-fv3
else
    ./mk-wrf-wcoss
fi
cp -fp ../prep_chem_sources_RADM_WRF_FIM_.exe $EXECrap/rap_prep_chem_sources
popd

pushd process-obs/RAP-Smoke/src/
make clean
make
cp -fp FRE_BBM_RAP_v4.exe "$EXECrap/rap_smoke_FRE_BBM_RAP"
cp -fp merge_FRP_RAP_v3.exe "$EXECrap/rap_smoke_merge_FRP_RAP"
cp -fp proc_J01_FRP_RAP_v3.exe "$EXECrap/rap_smoke_proc_J01_FRP_RAP"
cp -fp proc_MODIS_FRP_RAP_v3.exe "$EXECrap/rap_smoke_proc_MODIS_FRP_RAP"
cp -fp proc_NPP_FRP_RAP_v3.exe "$EXECrap/rap_smoke_proc_NPP_FRP_RAP"

set +xue

echo Success.  Please enjoy your new executables in $EXECrap
exit 0
