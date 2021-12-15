#!/bin/ksh --login
############################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_prep_ref2tten.sh.sms
# Script description:  runs the boundary generation job for the HRRR
#
# Author:  Curtis Alexander / Geoffrey Manikin   Org: EMC   Date: 2011-08-24
#
# Script history log:
# 2011-08-24  M Hu / G Manikin / J Zhu
# 2013-02-27  C Alexander
# 2015-08-01  C Alexander / G Manikin - HRRRv1
# 2016-02-05  C Alexander / G Manikin - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

set -x
# perform tten section

export MPI_VERBOSE=1
export MPI_DISPLAY_SETTINGS=1
export MPI_BUFS_PER_PROC=128
export MPI_BUFS_PER_HOST=128
export MPI_IB_RAILS=2
export MPI_GROUP_MAX=128
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

export HRRRDAS_MEAN=${COMOUTm1}/../hrrrdas/hrrr.t${cycm1}z.analysis_d02.mean

rm $DATA/*
cd $DATA

# Bring over background field (it's modified so we can't link to it)
if [ -r ${COMIN}/"hrrr.t${cyc}z.wrfguess_rap" ]; then
  cp ${COMIN}/hrrr.t${cyc}z.wrfguess_rap ./wrf_inout
  echo " Cycle ${PDY}${cyc}: TTEN background=${COMIN}/hrrr.t${cyc}z.wrfguess_pre"

  if [ ! -r "${HRRRDAS_MEAN}" ]; then
    echo " Cycle ${PDY}${cyc}: TTEN background is from RAP "

  else ## use HRRRDAS mean
    if [ ! -s "${HRRRDAS_MEAN}" ]; then
      echo "ERROR: HRRRDAS_MEAN '${HRRRDAS_MEAN}' has zero size!"
      exit 1
    fi

    cp ${EXEChrrr}/hrrr_copy_hrrrdas .
    runline="mpiexec -n 4 -ppn 4 ./hrrr_copy_hrrrdas ${HRRRDAS_MEAN} ./wrf_inout"
    $runline >> copy_hrrrdas.out
    export err=$?; err_chk
    echo " Cycle ${PDY}${cyc}: TTEN background is from HRRRDAS: ${HRRRDAS_MEAN}"
  fi

# No background available so abort
else
  echo "${COMIN}/hrrr.t${cyc}z.wrfguess_rap does not exist"
  echo "FATAL ERROR: No background file for analysis at ${PDY}${cyc}!!!!"
  echo " Cycle ${PDY}${cyc}: TTEN failed because of no background"
  err_exit 1
fi

# Insert land surface variables into the wrf_inout file
if [ $cyc -eq 0 ]; then
START_TIME=$PDYm1$cycm1
else
START_TIME=$PDY$cycm1
fi
counter=1
targetsize=16061363612
targetsize2=16073946524  ## HRRRv4 working dir on dell2
targetsize3=16057259932  ## HRRRv4 on WCOSS2
while [[ $counter -lt 36 ]]; do
  counterhr=$counter
  typeset -Z2 counterhr
  CYC_TIME=`$NDATE -${counter} $START_TIME`
  if [ -r ${HRRRGES_SFC}/hrrr_${CYC_TIME}f0$counterhr ]; then
    filesize=$(stat -c%s ${HRRRGES_SFC}/hrrr_${CYC_TIME}f0$counterhr)
    echo $filesize
#   if [ $filesize -eq $targetsize ]; then
    if [[ $filesize -eq $targetsize || $filesize -eq $targetsize2 || $filesize -eq $targetsize3 ]]; then
      echo " Cycle surface fields based on hrrr_${CYC_TIME}f0$counterhr"
      cp ${HRRRGES_SFC}/hrrr_${CYC_TIME}f0$counterhr ./wrfout_d01_save
      cp ${EXEChrrr}/hrrr_full_cycle_surface .
      runline="mpiexec -n 4 -ppn 4 ./hrrr_full_cycle_surface"
      $runline >> full_cycle_surface.out
      export err=$?; err_chk
      break
    fi
  fi
  counter=` expr $counter + 1 `
done

if [[ $counter -eq 48 ]]; then
  echo "WARNING: No land surface data cycled for background at ${PDY}${cyc}!!!!"
fi  

# Update GVF with real-time data
latestGVF=`ls ${GVF}/GVF-WKL-GLB_v2r3_npp_s*_e${PDYm2}_c${PDYm1}*.grib2`
latestGVF2=`ls ${GVF}/GVF-WKL-GLB_v2r3_npp_s*_e${PDYm3}_c${PDYm2}*.grib2`
if [ -r "$latestGVF" ]; then
   cp ${EXEChrrr}/hrrr_update_gvf .
   cp ${FIXhrrr}/hrrr_gvf_VIIRS_4KM.MIN.1gd4r.new gvf_VIIRS_4KM.MIN.1gd4r.new
   cp ${FIXhrrr}/hrrr_gvf_VIIRS_4KM.MAX.1gd4r.new gvf_VIIRS_4KM.MAX.1gd4r.new
   cp $latestGVF GVF-WKL-GLB.grib2
   runline="mpiexec -n 4 -ppn 4 ./hrrr_update_gvf"
   $runline >> update_gvf.out
elif [ -r "$latestGVF2" ]; then
   cp ${EXEChrrr}/hrrr_update_gvf .
   cp ${FIXhrrr}/hrrr_gvf_VIIRS_4KM.MIN.1gd4r.new gvf_VIIRS_4KM.MIN.1gd4r.new 
   cp ${FIXhrrr}/hrrr_gvf_VIIRS_4KM.MAX.1gd4r.new gvf_VIIRS_4KM.MAX.1gd4r.new 
   cp $latestGVF2 GVF-WKL-GLB.grib2
   runline="mpiexec -n 4 -ppn 4 ./hrrr_update_gvf"
   $runline >> update_gvf.out
else
   echo "$latestGVF and $latestGVF2 do not exist!!"
   echo "Warning: No GVF real-time data available for background at ${PDY}${cyc}!!!!"
fi  

# Link to the radar binary data
subhtimes="16 30 46 60"
count=1
for subhtime in ${subhtimes}; do
  if [ -r "${DATA_RADAR}/${subhtime}/RefInGSI.dat" ]; then
    ln -sf ${DATA_RADAR}/${subhtime}/RefInGSI3D.dat ./RefInGSI3D.dat_0${count}
  else
    echo "FATAL ERROR ${DATA_RADAR}/${subhtime}/RefInGSI3D.dat does not exist!"
    err_exit 1
  fi
  count=$((count + 1))
done

# Link to the lightning binary data
subhtimes="16 30 46 60"
count=1
for subhtime in ${subhtimes}; do
  if [ -r "${DATA_RADAR}/${subhtime}/LightningInGSI.dat" ]; then
    ln -sf ${DATA_RADAR}/${subhtime}/LightningInGSI.dat ./LightningInGSI.dat_0${count}
  else
    echo "ERROR ${DATA_RADAR}/${subhtime}/LightningInGSI.dat does not exist!"
#    err_exit 1
  fi
  count=$((count + 1))
done


# Run radar to tten
cp ${EXEChrrr}/hrrr_ref2tten .
runline="mpiexec -n 4 -ppn 4 ./hrrr_ref2tten"
$runline
export err=$?; err_chk

cp wrf_inout ${COMOUT}/hrrr.t${cyc}z.wrfguess_pre
exit 0
