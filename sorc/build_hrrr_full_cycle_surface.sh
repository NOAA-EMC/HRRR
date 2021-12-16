set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/hrrr_full_cycle_surface.fd
make clean
make

##############################
