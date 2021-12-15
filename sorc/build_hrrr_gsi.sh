set -x

##############################

export BASE=`pwd`
cd $BASE

module purge

# Load modules
export COMP=ftn
export COMP_MP=ftn
export COMP_MPI=ftn

export C_COMP=cc
export C_COMP_MP=cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

module load envvar/1.0
module load PrgEnv-intel/8.0.0
module load intel/19.1.3.304
module load craype
module load cray-mpich/8.1.4
module load bacio/2.4.1
module load bufr/11.4.0
module load crtm/2.3.0
module load ip/3.3.3
module load nemsio/2.5.2
module load prod_util/2.0.8
module load sfcio/1.4.1
module load sigio/2.3.2
module load sp/2.3.3
module load w3emc/2.7.3
module load w3nco/2.4.1
module load wrf_io/1.1.1

module load cmake/3.18.4
module load git/2.29.0
module load hdf5/1.10.6
module load netcdf/4.7.4
module load pnetcdf/1.12.2

module list

cd ${BASE}/hrrr_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/hrrr_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make -j1

cp bin/gsi.x        ${BASE}/../exec/hrrr_gsi
cp bin/enkf_wrf.x   ${BASE}/../exec/hrrr_enkf
cp bin/enspreproc.x ${BASE}/../exec/hrrr_process_enkf
cp bin/initialens.x ${BASE}/../exec/hrrr_initialens

##############################
