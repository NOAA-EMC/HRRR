set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0

#module load craype-hugepages256M

echo "modules for hrrr wrf"
module list

sleep 1

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp configure.wrf.useme configure.wrf

export PNETCDF_QUILT=1
export WRFIO_NCD_LARGE_FILE_SUPPORT=1
export WRF_DFI_RADAR=1
export WRF_SMOKE=1

./compile -j 1 em_real

cp main/real.exe ${BASE}/../exec/hrrr_wrfarw_real
cp main/wrf.exe ${BASE}/../exec/hrrr_wrfarw_fcst

##############################
