set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/HRRR/v4.0.0.da
module list

cd ${BASE}/hrrr_full_cycle_surface_enkf.fd
make clean
make

##############################
