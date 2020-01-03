set -e

##############################

export BASE=`pwd`
 
 . /opt/modules/default/init/ksh
module purge
# Load modules
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
module load cmake

module load craype-hugepages32M

which cmake
cmake --version

set -xu
export CRAYPE_LINK_TYPE=dynamic

cd ${BASE}/hrrr_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/hrrr_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make

set -e

cp bin/gsi.x        ${BASE}/../exec/hrrr_gsi
cp bin/enkf_wrf.x   ${BASE}/../exec/hrrr_enkf
cp bin/enspreproc.x ${BASE}/../exec/hrrr_process_enkf

##############################
