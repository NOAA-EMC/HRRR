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
export BUILD_hrrr_process_enkf=yes
export BUILD_hrrr_full_cycle_surface=yes
export BUILD_hrrr_process_cloud=yes
export BUILD_hrrr_process_imssnow=yes
export BUILD_hrrr_process_mosaic=yes
export BUILD_hrrr_process_sst=yes
export BUILD_hrrr_process_lightning=yes
export BUILD_hrrr_update_gvf=yes
export BUILD_hrrr_ref2tten=yes
export BUILD_hrrr_sndp=yes
export BUILD_hrrr_wrfbufr=yes
export BUILD_hrrr_stnmlist=yes
export BUILD_hrrr_smartinit_conus=yes
export BUILD_hrrr_smartinit_ak=yes

 . /opt/modules/default/init/ksh

module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/HRRR/v3.0.0
module unload PNetCDF-intel-sandybridge/1.5.0

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

if [ $BUILD_hrrr_process_enkf = yes ] ; then

echo " .... Building hrrr_process_enkf .... "
$BASE/build_hrrr_process_enkf.sh > $logs_dir/build_hrrr_process_enkf.log 2>&1

fi

##############################

if [ $BUILD_hrrr_full_cycle_surface = yes ] ; then

echo " .... Building hrrr_full_cycle_surface .... "
$BASE/build_hrrr_full_cycle_surface.sh > $logs_dir/build_full_cycle_surface.log 2>&1

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

if [ $BUILD_hrrr_wrfbufr = yes ] ; then

echo " .... Building hrrr_wrfbufr .... "
$BASE/build_hrrr_wrfbufr.sh > $logs_dir/build_wrfbufr.log 2>&1

fi

##############################

if [ $BUILD_hrrr_stnmlist = yes ] ; then

echo " .... Building hrrr_stnmlist .... "
$BASE/build_hrrr_stnmlist.sh > $logs_dir/build_stnmlist.log 2>&1

fi

##############################

if [ $BUILD_hrrr_smartinit_conus = yes ] ; then

echo " .... Building hrrr_smartinit_conus .... "
$BASE/build_hrrr_smartinit_conus.sh > $logs_dir/build_smartinit_conus.log 2>&1

fi

##############################

if [ $BUILD_hrrr_smartinit_ak = yes ] ; then

echo " .... Building hrrr_smartinit_ak .... "
$BASE/build_hrrr_smartinit_ak.sh > $logs_dir/build_smartinit_ak.log 2>&1

fi

##############################

cd $BASE
