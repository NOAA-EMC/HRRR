set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_hrrr_wrfarw_serial=yes
export BUILD_hrrr_update_bc=yes
export BUILD_hrrr_wps=yes
export BUILD_hrrr_wrfpost=yes
export BUILD_hrrr_wrfarw=yes
export BUILD_hrrr_gsi=yes
export BUILD_hrrrdas_wrfarw=yes
export BUILD_hrrr_write_idate=yes
export BUILD_hrrr_cal_bcpert=yes
export BUILD_hrrr_cal_ensemblemean=yes
export BUILD_hrrr_copy_hrrrdas=yes
export BUILD_hrrr_full_cycle_surface=yes
export BUILD_hrrr_full_cycle_surface_enkf=yes
export BUILD_hrrr_process_cloud=yes
export BUILD_hrrr_process_imssnow=yes
export BUILD_hrrr_process_mosaic=yes
export BUILD_hrrr_process_sst=yes
export BUILD_hrrr_process_lightning=yes
export BUILD_hrrr_prep_smoke=yes
export BUILD_hrrr_process_fvcom=yes
export BUILD_hrrr_update_gvf=yes
export BUILD_hrrr_ref2tten=yes
export BUILD_hrrr_sndp=yes
export BUILD_hrrr_wrfbufr_conus=yes
export BUILD_hrrr_wrfbufr_alaska=yes
export BUILD_hrrr_stnmlist=yes
export BUILD_hrrr_smartinit=yes


module purge
module load envvar/1.0

module use $BASE/../modulefiles
source $BASE/../modulefiles/HRRR/v4.0.0

module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

cd ${BASE}/hrrr_cal_bcpert.fd
make clean

cd ${BASE}/hrrr_cal_ensemblemean.fd
make clean

cd ${BASE}/hrrr_copy_hrrrdas.fd
make clean

cd ${BASE}/hrrr_full_cycle_surface_enkf.fd
make clean

cd ${BASE}/hrrr_full_cycle_surface.fd
make clean

cd ${BASE}/hrrr_prep_smoke.fd/process-obs/QC
make clean

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/cycle_netcdf
make clean

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/fires_ncfmake
make clean

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/Prep_smoke_FRP/bin/build
make clean

cd ${BASE}/hrrr_prep_smoke.fd/process-obs/HRRR-Smoke/src
make clean


cd ${BASE}/hrrr_prep_smoke.fd/process-obs/HRRR-AK-Smoke/src
make clean

cd ${BASE}/hrrr_process_cloud.fd
make clean

cd ${BASE}/hrrr_process_fvcom.fd
make clean

cd ${BASE}/hrrr_process_imssnow.fd
make clean

cd ${BASE}/hrrr_process_lightning.fd
make clean

cd ${BASE}/hrrr_process_mosaic.fd
make clean

cd ${BASE}/hrrr_process_sst.fd
make clean

cd ${BASE}/hrrr_ref2tten.fd
make clean

cd ${BASE}/hrrr_smartinit_ak.fd
make clean

cd ${BASE}/hrrr_smartinit_conus.fd
make clean

cd ${BASE}/hrrr_smartinit.fd
make clean

cd ${BASE}/hrrr_sndp.fd
make clean

cd ${BASE}/hrrr_stnmlist.fd
make clean

cd ${BASE}/hrrr_update_bc.fd
make clean

cd ${BASE}/hrrr_update_gvf.fd
make clean

cd ${BASE}/hrrr_wps.fd/WPSV3.9.1
./clean -aa
./clean -a
./clean

cd ${BASE}/hrrr_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean

cd ${BASE}/hrrr_wrfbufr_alaska.fd
make clean

cd ${BASE}/hrrr_wrfbufr_conus.fd
make clean

cd ${BASE}/hrrr_wrfpost.fd
make clean

cd ${BASE}/hrrr_write_idate.fd
make clean

