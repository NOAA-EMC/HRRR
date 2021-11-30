#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_prep_cloud.sh.ecf
# Script description:  This script processes the Cloud input data for the HRRR
#
# Author:  Curtis Alexander / Geoffrey.Manikin   Org: EMC     Date: 2011-08-24
#
# Script history log:
# 2011-08-24  M Hu / G Manikin / J Zhu - RAP
# 2014-08-01  C Alexander / G Manikin - HRRRv1
# 2016-02-01  C Alexander / G Manikin - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

set -x
DATE=/bin/date
AWK="/bin/gawk --posix"

cd ${DATA}

# Set endian conversion options for use with Intel compilers
export F_UFMTENDIAN="big;little:10,15,66"
export GMPIENVVAR=F_UFMTENDIAN
export MV2_ON_DEMAND_THRESHOLD=256
export START_TIME=${PDY}${cyc}
echo $START_TIME

# Make sure START_TIME is defined and in the correct format
if [ `echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{10}$/'` ]; then
    START_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
  elif [ ! "`echo "${START_TIME}" | ${AWK} '/^[[:digit:]]{8}[[:blank:]]{1}[[:digit:]]{2}$/'`" ]; then
    echo "ERROR: start time, '${START_TIME}', is not in 'yyyymmddhh' or 'yyyymmdd hh' format"
    exit 1
fi
  START_TIME=`${DATE} -d "${START_TIME}"`

echo ${PDY}${cyc}
cp ${PARMhrrr}/hrrr_prepobs_prep.bufrtable ./prepobs_prep.bufrtable
ln -sf ${FIXhrrr}/hrrrdas_geo_em.d01.nc geo_em.d01.nc 

# Link to the prepbufr data
if [ -r "${COMINnasalarc}/${RAP_RUN}.t${cyc}z.lgycld.tm00.bufr_d" ]; then
  cp ${COMINnasalarc}/${RAP_RUN}.t${cyc}z.lgycld.tm00.bufr_d . 
  ln -sf ${RAP_RUN}.t${cyc}z.lgycld.tm00.bufr_d NASA_LaRC_cloud.bufr
else
  echo "WARNING: ${COMINnasalarc}/${RAP_RUN}.t${cyc}z.lgycld.tm00.bufr_d does not exist!"
  echo "cloud processing will run with the NASALaRC data missing"
fi

echo ${PDY}${cyc} > nasaLaRC_cycle_date

cat << EOF > namelist_nasalarc
&SETUP
  analysis_time = ${PDY}${cyc},
  bufrfile='NASALaRCCloudInGSI.bufr',
  npts_rad=1,
  ioption = 2,
/
EOF

#  Run obs pre-processor
cp ${EXEChrrr}/hrrr_process_cloud .
runline="mpiexec -n 4 -ppn 4 ./hrrr_process_cloud"
$runline
export err=$?; err_chk

# Copy the output data to /com
cp NASALaRCCloudInGSI.bufr $COMOUT/hrrr.t${cyc}z.NASALaRCCloudInGSI_d01.bufr

# for domain 2
cat << EOF > namelist_nasalarc
&SETUP
  analysis_time = ${PDY}${cyc},
  bufrfile='NASALaRCCloudInGSI.bufr',
  npts_rad=3,
  ioption = 2,
/
EOF

rm geo_em.d01.nc
ln -sf ${FIXhrrr}/hrrrdas_geo_em.d02.nc geo_em.d01.nc 
runline="mpiexec -n 4 -ppn 4 ./hrrr_process_cloud"
$runline
export err=$?; err_chk

# Copy the output data to /com
cp NASALaRCCloudInGSI.bufr $COMOUT/hrrr.t${cyc}z.NASALaRCCloudInGSI_d02.bufr

echo "End of Cloud Processing"
exit 0
