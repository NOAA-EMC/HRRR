set -x

##############################

export BASE=`pwd`
cd $BASE

module reset
module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

#Huge pages
#module load craype-hugepages256M

cd ${BASE}/hrrr_cal_bcpert.fd
make clean
make

##############################
