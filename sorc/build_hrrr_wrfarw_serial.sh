set -x

export BASE=`pwd`

module purge
module load envvar/1.0
module load $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0
module list

##############################

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.9
 ./clean -aa
 ./clean -a
 ./clean
 cp configure.wrf.serial configure.wrf
./compile em_real

##############################


