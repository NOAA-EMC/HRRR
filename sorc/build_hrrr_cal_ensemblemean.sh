set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load HRRR/v4.0.0.da
module list

#Huge pages
#module load craype-hugepages256M

cd ${BASE}/hrrr_cal_ensemblemean.fd
make clean
make

##############################
