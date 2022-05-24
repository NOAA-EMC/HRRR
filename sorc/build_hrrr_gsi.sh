set -x

##############################

export BASE=`pwd`
cd $BASE

module reset

# Load modules
export COMP=ftn
export COMP_MP=ftn
export COMP_MPI=ftn

export C_COMP=cc
export C_COMP_MP=cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0

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
