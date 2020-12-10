set -x

##############################

export BASE=`pwd`
cd $BASE

. /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/HRRR/v4.0.0.da
module load HDF5-serial-intel-haswell/1.8.9
module unload NetCDF-intel-haswell/3.6.3
module load NetCDF-intel-haswell/4.2
module list

cd ${BASE}/hrrr_process_fvcom.fd
make clean
make

##############################
