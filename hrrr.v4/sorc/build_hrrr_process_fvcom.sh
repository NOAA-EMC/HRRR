set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module use -a /opt/cray/craype/default/modulefiles
module use -a /opt/cray/ari/modulefiles
module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
module load PrgEnv-intel
module load cray-mpich
module load zlib-intel-haswell/1.2.7
module load HDF5-serial-intel-haswell/1.8.9
module load NetCDF-intel-haswell/4.2
module load grib_util/1.0.3
module load prod_util
module load prod_envir

module use -a /usrx/local/dev/modulefiles
module list

cd ${BASE}/hrrr_process_fvcom.fd
make clean
make

##############################
