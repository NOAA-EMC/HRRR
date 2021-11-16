#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_wrfbufr.sh
# Script description:  Run hrrr wrfbufr jobs
#
# Author:      G Manikin   EMC         Date: 2014-08-01
#
# Abstract: This script runs the HRRR wrfbufr jobs for the 18-h HRRR forecast
#
# Script history log:
# 2014-08-01  G Manikin - new script 
# 2018-01-24  B Blake / G Manikin - HRRRv3
#

set -xa
mkdir $DATA
cd $DATA

msg="$job HAS BEGUN"
postmsg $jlogfile "$msg"

# fhr is passed from the SMS script
fhr=$fhr

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export CORE=RAPR
export OUTTYP=binarympiio
DATE=/bin/date

START_TIME=$PDY$cyc
echo $START_TIME

cp ${FIXhrrr}/hrrrak_profdat hrrr_profdat 
OUTTYP=netcdf
model=RAPR
NFILE=1
INCR=01
CYCLE1=`$NDATE -1 $START_TIME`
date=`$NDATE $fhr $START_TIME`

wyr=`echo $date | cut -c1-4`
wmn=`echo $date | cut -c5-6`
wdy=`echo $date | cut -c7-8`
whr=`echo $date | cut -c9-10`

echo $wyr $wmn $wdy $whr
let fhrold="$fhr - 1"
dateold=`$NDATE $fhrold $START_TIME`

oyr=`echo $dateold | cut -c1-4`
omn=`echo $dateold | cut -c5-6`
ody=`echo $dateold | cut -c7-8`
ohr=`echo $dateold | cut -c9-10`

timeform=${wyr}"-"${wmn}"-"${wdy}"_"${whr}"_00_00"
timeformold=${oyr}"-"${omn}"-"${ody}"_"${ohr}"_00_00"

cp ${INPUT_DATA}/wrfout_d01_${timeform}  wrfoutd01_${timeform}

if [ $fhr -eq 0 ]; then
# Look back 2 cycles for relevant file - if not found, we do not want to
# utilize information from an old cycle for hourly average fields
  counter=1
  while [[ $counter -lt 06 ]]; do
    counterhr=$counter
    typeset -Z2 counterhr
    CYC_TIME=`$NDATE -${counter} $dateold`
    if [ -r ${HRRRGES_SFC}/hrrr_${CYC_TIME}f0$counterhr ]; then
      echo "Found hrrr_${CYC_TIME}f0$counterhr for $dateold"
      cp ${HRRRGES_SFC}/hrrr_${CYC_TIME}f0$counterhr ./wrfoutd01_${timeformold}
      break
    fi
    counter=` expr $counter + 1 `
  done
  if [ $counter -eq 06 ]; then
    echo "No file found for $dateold"
  fi
else
  cp ${INPUT_DATA}/wrfout_d01_${timeformold} wrfoutd01_${timeformold}
fi

OUTFIL=wrfoutd01_${timeform}
OLDOUTFIL=wrfoutd01_${timeformold}

START_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
START_TIME=`${DATE} -d "${START_TIME}"`
END_TIME=`${DATE} -d "${START_TIME} $fhr hours"`
echo $END_TIME

YYYY=`${DATE} +"%Y" -d "${START_TIME}"`
MM=`${DATE} +"%m" -d "${START_TIME}"`
DD=`${DATE} +"%d" -d "${START_TIME}"`
HH=`${DATE} +"%h" -d "${START_TIME}"`

VALIDTIME=${YYYY}'-'${MM}'-'${DD}'_'${whr}':00:00'
START_TIME=${YYYY}'-'${MM}'-'${DD}'_'${cyc}':00:00'
VTIME=${YYYY}${MM}${DD}${whr}
echo $VTIME 

cat > itag <<EOF
$OUTFIL
$model
$OUTTYP
$START_TIME
$NFILE
$INCR
${fhr}
$OLDOUTFIL
EOF

ln -sf itag              fort.11
ln -sf $DATA/hrrr_profdat  fort.19
ln -sf $DATA/profilm.c1.tm00 fort.79

datestr=`date`
echo about to run program at $datestr

#startmsg
cp ${EXEChrrr}/hrrr_wrfbufr_${dom} hrrr_wrfbufr
runline="mpiexec -n 1 -ppn 1 ./hrrr_wrfbufr"
$runline
export err=$?; err_chk

mv profilm.c1.tm00 profilm.c1.f${fhr}

postmsg $jlogfile "HRRR WRFBUFR done for F${fhr}"

echo EXITING $0
exit
