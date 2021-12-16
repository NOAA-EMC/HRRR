set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

cd ${BASE}/hrrr_write_idate.fd
make clean
make

cp hrrr_write_idate  ${BASE}/../exec/hrrr_write_idate

##############################
