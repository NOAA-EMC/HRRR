set -x

##############################

export BASE=`pwd`

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/HRRR/v3.0.0.da
module list

cd ${BASE}/hrrr_stnmlist.fd
make clean
make

##############################
