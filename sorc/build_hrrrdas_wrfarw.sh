#! /bin/ksh --login

set -x

export BASE=`pwd`

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/HRRR/v4.0.0

echo "modules for hrrrdas wrf"
module list

sleep 1

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp configure.wrf.wcoss_hrrrdas configure.wrf

export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
unset WRF_DFI_RADAR
export WRF_SMOKE=1

set -e
./compile -j 1 em_real

cp main/real.exe ${BASE}/../exec/hrrrdas_wrfarw_real
cp main/wrf.exe ${BASE}/../exec/hrrrdas_wrfarw_fcst
set +e

##############################
