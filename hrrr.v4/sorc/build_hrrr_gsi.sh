set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
# Load modules
setenv COMP ftn
setenv COMP_MP ftn
setenv COMP_MPI ftn

setenv C_COMP cc
setenv C_COMP_MP cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

set WRF_SHARED_VER v1.1.0
set WRF_SHARED_ROOT /gpfs/hps/nco/ops/nwprod/wrf_shared
setenv WRF_SHARED_PATH ${WRF_SHARED_ROOT}.${WRF_SHARED_VER}-${COMPILER}

# Known conflicts

# Loading ncep environment
module load ncep/1.0
module load prod_util/1.0.29

# Loading Intel Compiler Suite
module load PrgEnv-intel/5.2.56
module unload intel/15.0.3.187
module load intel/18.1.163

# Loading pe environment
module load craype-sandybridge
module load cray-mpich/7.2.0

#module use /usrx/local/dev/modulefiles
module load cmake/3.6.2
#module unuse /usrx/local/dev/modulefiles

# Loading netcdf modules
#module use /usrx/local/dev/modulefiles
module load NetCDF-intel-sandybridge/4.7.4
module load HDF5-parallel-intel-sandybridge/1.10.6

# Loading nceplibs modules
#module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles
module load bufr-intel/11.3.0
module load ip-intel/3.0.0
module load nemsio-intel/2.2.2
module load sfcio-intel/1.0.0
module load sigio-intel/2.1.0
module load sp-intel/2.0.2
module load w3nco-intel/2.0.6
module load w3emc-intel/2.2.0

#module use /usrx/local/nceplibs/modulefiles
module load bacio-intel/2.0.2

#module use -a /usrx/local/nceplibs/NCEPLIBS/modulefiles
#module load crtm/2.3.0

# Loading python
module load python/3.6.3

module list

export BUFR_LIBd=/gpfs/hps/nco/ops/nwprod/lib/bufr/v11.3.0/intel/libbufr_v11.3.0_4_64.a

cd ${BASE}/hrrr_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/hrrr_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make -j8

set -e

cp bin/gsi.x        ${BASE}/../exec/hrrr_gsi
cp bin/enkf_wrf.x   ${BASE}/../exec/hrrr_enkf
cp bin/enspreproc.x ${BASE}/../exec/hrrr_process_enkf
cp bin/initialens.x ${BASE}/../exec/hrrr_initialens

##############################
