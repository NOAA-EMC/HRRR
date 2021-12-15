#! /bin/ksh -l
#################################################
#  UNIX Script Documentation Block
#
# Script name:         exhrrr_postmgr.sh.sms
# Script description:  post manager for hourly output
#
# 2014-08-01  G Manikin - new script
# 2016-02-01  G Manikin - HRRRv2
# 2018-01-24  B Blake / G Manikin - HRRRv3
#################################################

set -x

cd $DATA

export PS4='$SECONDS + '

hour=00
typeset -Z2 hour
if [ $cyc -eq 00 -o $cyc -eq 06 -o $cyc -eq 12 -o $cyc -eq 18 ]; then
  TEND=48
  TCP=49
else
  TEND=18
  TCP=19
fi

if [ -e posthours ]; then
   rm -f posthours
fi

while [ $hour -lt $TCP ]; 
do
  echo $hour >>posthours
     let "hour=hour+1"
done
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
ls -lrt $INPUT_DATA/fcstdone*00.$shh
icnt=1
while [ $icnt -lt 1000 ]
do
  for fhr in $postjobs
  do
    if [ -s $INPUT_DATA/fcstdone${fhr}00.${shh} ]
    then 
#      qsub ${HOMEhrrr}/smsak/post/post/jhrrr_post_f${fhr}_${cyc}.qsub
#      qsub ${HOMEhrrr}/smsak/post/wrfbufr/jhrrr_wrfbufr_f${fhr}_${cyc}.qsub
      ecflow_client --event release_post${fhr}

      # Remove current fhr from list
      postjobs=`echo $postjobs | sed s/${fhr}//g`
    fi
    if [ $fhr -eq 17 ]
    then
#  need to copy F17 fcstdone file to /com so that next hour's cycle
#    can know the status of the previous hour's forecast job
      cp $INPUT_DATA/fcstdone${fhr}00.${shh} $COMOUT/fcstdone${fhr}00.${shh}
    fi
  done
  
  result_check=`echo $postjobs | wc -w`
  if [ $result_check -eq 0 ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 900 ]
  then
    msg="ABORTING after 150 minutes of waiting for HRRR FCST hours $postjobs."
    err_exit $msg
  fi

done

echo Exiting $0

exit
