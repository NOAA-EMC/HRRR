#! /bin/ksh -l
###################################################
#  UNIX Script Documentation Block 
#
# Script name:         exhrrr_postmgr_subh.sh.sms
# Script description:  post manager for sub-hourly output
#
# 2014-08-01  G Manikin - new script
# 2016-02-01  G Manikin - HRRRv2
# 2018-01-24  B Blake - HRRRv3
####################################################

set -x

cd $DATA

export PS4='$SECONDS + '

hour=00
minutes="00 15 30 45"

typeset -Z2 hour
TEND=18
TCP=18

if [ -e posthours ]; then
   rm -f posthours
fi

while [ $hour -lt $TCP ]; 
do
  for min in $minutes; do 
    echo $hour$min >>posthours
  done
     let "hour=hour+1"
done
echo $TEND'00' >>posthours
postjobs=`cat posthours`

# Compute date & time components for the forecast
START_TIME=${START_TIME:-$PDY$cyc}
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`

#
# Wait for all fcst hours to finish 
#
icnt=1
while [ $icnt -lt 1000 ]
do
  for ftime in $postjobs
  do
    if [ -s $INPUT_DATA/fcstdone${ftime}.${shh} ]
    then 
#      qsub ${HOMEhrrr}/sms/post/post_subh/jhrrr_post_f${ftime}_${cyc}.qsub
      ecflow_client --event release_post${ftime}
      # Remove current fhr from list
      postjobs=`echo $postjobs | sed s/${ftime}//g`
    fi
  done
  
  result_check=`echo $postjobs | wc -w`
  if [ $result_check -eq 0 ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 360 ]
  then
    msg="FATAL ERROR: ABORTING after 60 minutes of waiting for HRRR FCST hours $postjobs."
    err_exit $msg
  fi

done

echo Exiting $0

exit