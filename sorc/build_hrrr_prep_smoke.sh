##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load HRRR/v4.0.0
module list

cd ${BASE}/hrrr_prep_smoke.fd/process-obs/QC
make clean
make 
cp -fp qc_modis.exe ${BASE}/../exec/hrrr_smoke_qc_modis
cp -fp qc_viirs.exe ${BASE}/../exec/hrrr_smoke_qc_viirs

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/cycle_netcdf
make clean
./mk-wrf-wcoss2
cp -fp "cycle_netcdf.x" ${BASE}/../exec/hrrr_cycle_netcdf

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/fires_ncfmake
make clean
./mk-wrf-wcoss2
cp -fp "fires_ncfmake.x" ${BASE}/../exec/hrrr_fires_ncfmake

cd ${BASE}/hrrr_prep_smoke.fd/prep-chem/Prep_smoke_FRP/bin/build
make clean
./mk-wrf-wcoss
cp -fp ../prep_chem_sources_RADM_WRF_FIM_.exe ${BASE}/../exec/hrrr_prep_chem_sources

cd ${BASE}/hrrr_prep_smoke.fd/process-obs/HRRR-Smoke/src
make clean
make
cp -fp FRE_BBM_HRRR_v4.exe ${BASE}/../exec/hrrr_smoke_fre_bbm_conus
cp -fp merge_FRP_HRRR_v3.exe ${BASE}/../exec/hrrr_smoke_merge_frp_conus
cp -fp proc_J01_FRP_HRRR_v3.exe ${BASE}/../exec/hrrr_smoke_proc_j01_frp_conus
cp -fp proc_MODIS_FRP_HRRR_v3.exe ${BASE}/../exec/hrrr_smoke_proc_modis_frp_conus
cp -fp proc_NPP_FRP_HRRR_v3.exe ${BASE}/../exec/hrrr_smoke_proc_npp_frp_conus

cd ${BASE}/hrrr_prep_smoke.fd/process-obs/HRRR-AK-Smoke/src
make clean
make
cp -fp FRE_BBM_HRRR_AK.exe ${BASE}/../exec/hrrr_smoke_fre_bbm_alaska
cp -fp merge_FRP_HRRR_AK.exe ${BASE}/../exec/hrrr_smoke_merge_frp_alaska
cp -fp proc_J01_FRP_HRRR_AK.exe ${BASE}/../exec/hrrr_smoke_proc_j01_frp_alaska
cp -fp proc_MODIS_FRP_HRRR-AK.exe ${BASE}/../exec/hrrr_smoke_proc_modis_frp_alaska
cp -fp proc_NPP_FRP_HRRR-AK_v5.exe ${BASE}/../exec/hrrr_smoke_proc_npp_frp_alaska

##############################
