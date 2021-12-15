set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load HRRR/v4.0.0
module list

cd ${BASE}/hrrr_wps.fd/WPSV3.9.1
./clean -aa
./clean -a
./clean
cp configure.wps.optim configure.wps
./compile
cp ungrib/src/ungrib.exe ${BASE}/../exec/hrrr_wps_ungrib
cp metgrid/src/metgrid.exe ${BASE}/../exec/hrrr_wps_metgrid

##############################
