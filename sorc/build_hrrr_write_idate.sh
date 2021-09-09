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

cd ${BASE}/hrrr_write_idate.fd
make clean
make

cp hrrr_write_idate  ${BASE}/../exec/hrrr_write_idate

##############################
