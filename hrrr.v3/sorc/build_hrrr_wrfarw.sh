set -x

export BASE=`pwd`

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/HRRR/v3.0.0

echo "modules for wrf"
module list

sleep 1

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.8.1
./clean -aa
./clean -a
./clean
cp configure.wrf.optim configure.wrf
./compile em_real
cp main/real.exe ${BASE}/../exec/hrrr_wrfarw_real
cp main/wrf.exe ${BASE}/../exec/hrrr_wrfarw_fcst

module unload craype-haswell
module load craype-sandybridge

##############################
