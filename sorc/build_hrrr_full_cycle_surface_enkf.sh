set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0

module use $BASE/../modulefiles
module load HRRR/v4.0.0.da
module list

cd ${BASE}/hrrr_full_cycle_surface_enkf.fd
make clean
make

##############################
