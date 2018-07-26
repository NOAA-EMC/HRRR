set -x

##############################

export BASE=`pwd`
 
 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/HRRR/v3.0.0.da
module list

export CRAYPE_LINK_TYPE=dynamic

cd ${BASE}/hrrr_gsi.fd
make clean
cd ${BASE}/hrrr_gsi.fd/gsdcloud
make clean
make
cd ${BASE}/hrrr_gsi.fd
make
make install
make library

module unload craype-haswell
module load craype-sandybridge

##############################
