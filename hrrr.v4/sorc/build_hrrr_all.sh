set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_hrrr_wrfarw_serial=no
export BUILD_hrrr_update_bc=no
export BUILD_hrrr_wps=no
export BUILD_hrrr_wrfpost=no
export BUILD_hrrr_wrfarw=yes
export BUILD_hrrr_gsi=no
export BUILD_hrrrdas_wrfarw=no
export BUILD_hrrr_write_idate=no
export BUILD_hrrr_cal_bcpert=no
export BUILD_hrrr_cal_ensemblemean=no
export BUILD_hrrr_copy_hrrrdas=no
export BUILD_hrrr_full_cycle_surface=no
export BUILD_hrrr_full_cycle_surface_enkf=no
export BUILD_hrrr_process_cloud=no
export BUILD_hrrr_process_imssnow=no
export BUILD_hrrr_process_mosaic=no
export BUILD_hrrr_process_sst=no
export BUILD_hrrr_process_lightning=no
export BUILD_hrrr_prep_smoke=no
export BUILD_hrrr_process_fvcom=no
export BUILD_hrrr_update_gvf=no
export BUILD_hrrr_ref2tten=no
export BUILD_hrrr_sndp=no
export BUILD_hrrr_wrfbufr_conus=no
export BUILD_hrrr_wrfbufr_alaska=no
export BUILD_hrrr_stnmlist=no
export BUILD_hrrr_smartinit=no

 . /opt/modules/default/init/ksh

module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/HRRR/v4.0.0
#module unload PNetCDF-intel-sandybridge/1.5.0

module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1


##############################

if [ $BUILD_hrrr_wrfarw_serial = yes ] ; then

echo " .... Building hrrr_wrfarw_serial .... "
$BASE/build_hrrr_wrfarw_serial.sh > $logs_dir/build_hrrr_wrfarw_serial.log 2>&1

fi

##############################

if [ $BUILD_hrrr_update_bc = yes ] ; then

echo " .... Building hrrr_update_bc .... "
$BASE/build_hrrr_update_bc.sh > $logs_dir/build_hrrr_update_bc.log 2>&1

fi

##############################

if [ $BUILD_hrrr_wps = yes ] ; then

echo " .... Building hrrr_wps .... "
$BASE/build_hrrr_wps.sh > $logs_dir/build_hrrr_wps.log 2>&1

fi

##############################

if [ $BUILD_hrrr_wrfpost = yes ] ; then

echo " .... Building hrrr_wrfpost .... "
$BASE/build_hrrr_wrfpost.sh > $logs_dir/build_hrrr_wrfpost.log 2>&1

fi

##############################

if [ $BUILD_hrrr_wrfarw = yes ] ; then

echo " .... Building hrrr_wrfarw .... "
$BASE/build_hrrr_wrfarw.sh > $logs_dir/build_hrrr_wrfarw.log 2>&1

fi

##############################

if [ $BUILD_hrrr_gsi = yes ] ; then

echo " .... Building hrrr_gsi .... "
$BASE/build_hrrr_gsi.sh > $logs_dir/build_hrrr_gsi.log 2>&1

fi

##############################

if [ $BUILD_hrrrdas_wrfarw = yes ] ; then

echo " .... Building hrrrdas_wrfarw .... "
$BASE/build_hrrrdas_wrfarw.sh > $logs_dir/build_hrrrdas_wrfarw.log 2>&1

fi

##############################

if [ $BUILD_hrrr_write_idate = yes ] ; then

echo " .... Building hrrr_write_idate .... "
$BASE/build_hrrr_write_idate.sh > $logs_dir/build_hrrr_write_idate.log 2>&1

fi

##############################

if [ $BUILD_hrrr_cal_bcpert = yes ] ; then

echo " .... Building hrrr_cal_bcpert .... "
$BASE/build_hrrr_cal_bcpert.sh > $logs_dir/build_hrrr_cal_bcpert.log 2>&1

fi

##############################

if [ $BUILD_hrrr_cal_ensemblemean = yes ] ; then

echo " .... Building hrrr_cal_ensemblemean .... "
$BASE/build_hrrr_cal_ensemblemean.sh > $logs_dir/build_hrrr_cal_ensemblemean.log 2>&1

fi

##############################

if [ $BUILD_hrrr_copy_hrrrdas = yes ] ; then

echo " .... Building hrrr_copy_hrrrdas .... "
$BASE/build_hrrr_copy_hrrrdas.sh > $logs_dir/build_hrrr_copy_hrrrdas.log 2>&1

fi

##############################

if [ $BUILD_hrrr_full_cycle_surface = yes ] ; then

echo " .... Building hrrr_full_cycle_surface .... "
$BASE/build_hrrr_full_cycle_surface.sh > $logs_dir/build_full_cycle_surface.log 2>&1

fi

##############################

if [ $BUILD_hrrr_full_cycle_surface_enkf = yes ] ; then

echo " .... Building hrrr_full_cycle_surface_enkf .... "
$BASE/build_hrrr_full_cycle_surface_enkf.sh > $logs_dir/build_full_cycle_surface_enkf.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_cloud = yes ] ; then

echo " .... Building hrrr_process_cloud .... "
$BASE/build_hrrr_process_cloud.sh > $logs_dir/build_process_cloud.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_imssnow = yes ] ; then

echo " .... Building hrrr_process_imssnow .... "
$BASE/build_hrrr_process_imssnow.sh > $logs_dir/build_process_imssnow.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_mosaic = yes ] ; then

echo " .... Building hrrr_process_mosaic .... "
$BASE/build_hrrr_process_mosaic.sh > $logs_dir/build_process_mosaic.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_sst = yes ] ; then

echo " .... Building hrrr_process_sst .... "
$BASE/build_hrrr_process_sst.sh > $logs_dir/build_process_sst.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_lightning = yes ] ; then

echo " .... Building hrrr_process_lightning .... "
$BASE/build_hrrr_process_lightning.sh > $logs_dir/build_process_lightning.log 2>&1

fi

##############################

if [ $BUILD_hrrr_prep_smoke = yes ] ; then

echo " .... Building hrrr_prep_smoke .... "
$BASE/build_hrrr_prep_smoke.sh > $logs_dir/build_hrrr_prep_smoke.log 2>&1

fi

##############################

if [ $BUILD_hrrr_process_fvcom = yes ] ; then

echo " .... Building hrrr_process_fvcom .... "
$BASE/build_hrrr_process_fvcom.sh > $logs_dir/build_process_fvcom.log 2>&1

fi

##############################

if [ $BUILD_hrrr_update_gvf = yes ] ; then

echo " .... Building hrrr_update_gvf .... "
$BASE/build_hrrr_update_gvf.sh > $logs_dir/build_update_gvf.log 2>&1

fi

##############################

if [ $BUILD_hrrr_ref2tten = yes ] ; then

echo " .... Building hrrr_ref2tten .... "
$BASE/build_hrrr_ref2tten.sh > $logs_dir/build_ref2tten.log 2>&1

fi

##############################

if [ $BUILD_hrrr_sndp = yes ] ; then

echo " .... Building hrrr_sndp .... "
$BASE/build_hrrr_sndp.sh > $logs_dir/build_sndp.log 2>&1

fi

##############################

if [ $BUILD_hrrr_wrfbufr_conus = yes ] ; then

echo " .... Building hrrr_wrfbufr_conus .... "
$BASE/build_hrrr_wrfbufr_conus.sh > $logs_dir/build_wrfbufr_conus.log 2>&1

fi

##############################

if [ $BUILD_hrrr_wrfbufr_alaska = yes ] ; then

echo " .... Building hrrr_wrfbufr_alaska .... "
$BASE/build_hrrr_wrfbufr_alaska.sh > $logs_dir/build_wrfbufr_alaska.log 2>&1

fi

##############################

if [ $BUILD_hrrr_stnmlist = yes ] ; then

echo " .... Building hrrr_stnmlist .... "
$BASE/build_hrrr_stnmlist.sh > $logs_dir/build_stnmlist.log 2>&1

fi

##############################

if [ $BUILD_hrrr_smartinit = yes ] ; then

echo " .... Building hrrr_smartinit .... "
$BASE/build_hrrr_smartinit.sh > $logs_dir/build_smartinit.log 2>&1

fi

##############################

cd $BASE
