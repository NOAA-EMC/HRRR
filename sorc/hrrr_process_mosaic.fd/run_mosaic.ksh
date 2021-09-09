#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_process_hydro.sh.sms
# Script description:  runs radar mosaic and Langley cloud processing 
#
# Author:        Ming Hu / Geoff Manikin   Org: EMC          Date: 2011-08-24
#
# Script history log:
# 2011-08-24  M Hu / G Manikin
#

# Set the queueing options 
#PBS -l procs=33
#PBS -l walltime=0:20:00
#PBS -A comgsi
#PBS -N wrf_gsi
#PBS -q debug
#PBS -j oe

set -x
np=$PBS_NP

module load intel
module load mvapich2


START_TIME=2015121423
workdir=/scratch3/BMC/wrfruc/mhu/rapcode/GSI_r1216/data_process/mosaic/test
pgm=/scratch3/BMC/wrfruc/mhu/rapcode/GSI_r1216/data_process/mosaic/all/process_NSSL_mosaic.exe
export MOSAICTILENUM=4

mkdir -p ${workdir}
cd ${workdir}

# NSSL MOSAIC Data
export COM_MOSAIC=/scratch4/BMC/public/data/radar

### Process Mosaic
numtiles=${MOSAICTILENUM}

# Directory for the Radar Mosaic input files

echo $START_TIME >STARTTIME

# Compute date & time components for the analysis time
ymd=`echo ${START_TIME} | cut -c1-8`
ymdh=`echo ${START_TIME} | cut -c1-10`
hh=`echo ${START_TIME} | cut -c9-10`
YYYYJJJHH00=`date +"%Y%j%H00" -d "${ymd} ${hh}"`
YYYYMMDDHH=`date +"%Y%m%d%H" -d "${ymd} ${hh}"`
YYYY=`date +"%Y" -d "${ymd} ${hh}"`
MM=`date +"%m" -d "${ymd} ${hh}"`
DD=`date +"%d" -d "${ymd} ${hh}"`
HH=`date +"%H" -d "${ymd} ${hh}"`
mm=`date +"%M" -d "${ymd} ${hh}"`

cp /scratch3/BMC/wrfruc/mhu/rapcode/GSI_r1216/data_process/mosaic/prepobs_prep.bufrtable  ./prepobs_prep.bufrtable
cp /home/Ming.Hu/RR/RAP2014jun/static/WPS/geo_em.d01.nc ./geo_em.d01.nc

# find NSSL grib2 mosaic files
COM_MOSAIC_GRIB2=${COM_MOSAIC}/mrms/conus
numgrib2_00=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}00??.grib2 | wc -l`
numgrib2_01=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}01??.grib2 | wc -l`
numgrib2_02=`ls ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}02??.grib2 | wc -l`
if [ ${numgrib2_00} -eq 33 ]; then
   cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}00??.grib2 .
   ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
else
   if [ ${numgrib2_01} -eq 33 ]; then
      cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}01??.grib2 .
      ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
   else
      if [ ${numgrib2_02} -eq 33 ]; then
         cp ${COM_MOSAIC_GRIB2}/*_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}02??.grib2 .
         ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
      else
         echo " No NSSL gribs data available, use NCEP 8 tiles binary"
         if [ -s filelist_mrms ]; then
            rm -f filelist_mrms
         fi
      fi
   fi
fi

if [ -s filelist_mrms ]; then
   numgrib2=`more filelist_mrms | wc -l`
   echo "NSSL grib2 file level number = $numgrib2"
else
   numgrib2=0
fi

# Link to the radar data
if [ $numgrib2 -eq 36 ]; then 
   gzip -d *.gz
   numtiles=1
   rm -f filelist_mrms
   ls *_MergedReflectivityQC_*_${YYYY}${MM}${DD}-${HH}????.grib2 > filelist_mrms
else
   if [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile1/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile2/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile3/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ] && \
      [ -s ${COM_MOSAIC}/nssl/mrms_binary/tile4/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ]; then
      numtiles=4
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile1/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t1.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile2/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t2.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile3/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t3.gz
      cp ${COM_MOSAIC}/nssl/mrms_binary/tile4/mrefl/MREF3D33L.${YYYY}${MM}${DD}.${HH}${mm}00.gz ./mosaic_t4.gz
      gzip -d *.gz
   else
      numtiles=81
      export MOSAICdir=${COM_MOSAIC}/nssl/mosaic3d_nc
      ln -s ${MOSAICdir}/tile1/${ymd}_${hh}00.mosaic ./mosaic_t1
      ln -s ${MOSAICdir}/tile2/${ymd}_${hh}00.mosaic ./mosaic_t2
      ln -s ${MOSAICdir}/tile3/${ymd}_${hh}00.mosaic ./mosaic_t3
      ln -s ${MOSAICdir}/tile4/${ymd}_${hh}00.mosaic ./mosaic_t4
      ln -s ${MOSAICdir}/tile5/${ymd}_${hh}00.mosaic ./mosaic_t5
      ln -s ${MOSAICdir}/tile6/${ymd}_${hh}00.mosaic ./mosaic_t6
      ln -s ${MOSAICdir}/tile7/${ymd}_${hh}00.mosaic ./mosaic_t7
      ln -s ${MOSAICdir}/tile8/${ymd}_${hh}00.mosaic ./mosaic_t8
   fi
fi

echo ${ymdh} > ./mosaic_cycle_date

cat << EOF > mosaic.namelist
 &setup
  tversion=${numtiles},
  analysis_time = ${YYYYMMDDHH},
  dataPath = './',
 /

EOF

#startmsg
mpiexec -np $np $pgm > process_mosaic.out

msg="JOB $job FOR RAP_PREP HAS COMPLETED NORMALLY"

exit 0

