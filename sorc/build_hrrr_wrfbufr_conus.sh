set -x

##############################

export BASE=`pwd`

module reset
module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/hrrr_wrfbufr_conus.fd
make clean
make

##############################
