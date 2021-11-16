set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load HRRR/v4.0.0.da
module list

cd ${BASE}/hrrr_smartinit.fd
make clean
make

##############################
