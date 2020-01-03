set -x

export BASE=`pwd`

 . /opt/modules/default/init/ksh

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/HRRR/v4.0.0
module unload PNetCDF-intel-sandybridge/1.5.0
module list

##############################

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.9
 ./clean -aa
 ./clean -a
 ./clean
 cp configure.wrf.serial configure.wrf
./compile em_real

##############################


