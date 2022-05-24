set -x

##############################

export BASE=`pwd`
cd $BASE

module reset

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/hrrr_process_fvcom.fd
make clean
make

##############################
