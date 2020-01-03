set -x

##############################

export BASE=`pwd`

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/HRRR/v4.0.0.da
module list

cd ${BASE}/hrrr_process_sst.fd
make clean
make

##############################
