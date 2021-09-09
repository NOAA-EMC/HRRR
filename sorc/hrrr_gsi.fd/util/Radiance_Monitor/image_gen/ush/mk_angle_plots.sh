#!/bin/ksh

#------------------------------------------------------------------
#
#  mk_angle_plots.sh
#
#  submit the plot jobs to create the angle images.
#
#  Log:
#   08/2010  safford  initial coding (adapted from angle.sh).
#------------------------------------------------------------------

set -ax
date

echo "Begin mk_angle_plots.sh"

export NUM_CYCLES=${NUM_CYCLES:-121}
export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}

imgndir=${IMGNDIR}/angle
tankdir=${TANKDIR}/angle

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#echo Z = $Z

#-------------------------------------------------------------------
#  Locate/update the control files in $TANKDIR/radmon.$PDY.  $PDY 
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to 
#  the log file and exit if no ctl files are found. 
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`

cycdy=$((24/$CYCLE_INTERVAL))           # number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))		# number of days in plot period

echo SATYPE=$SATYPE

for type in ${SATYPE}; do
   found=0
   finished=0
   ctr=$ndays
   test_day=$PDATE

   while [[ $found -eq 0 && $finished -ne 1 ]]; do

      if [[ $REGIONAL_RR -eq 1 ]]; then         # REGIONAL_RR stores hrs 18-23 in next 
         tdate=`$NDATE +6 ${test_day}`          # day's radmon.yyymmdd directory
         pdy=`echo $test_day|cut -c1-8`
      else
         pdy=`echo $test_day|cut -c1-8`    
      fi
      echo "testing with pdy = $pdy"

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}
      else
         ieee_src=${TANKverf}/${MONITOR}.${PDY}
      fi

      if [[ -s ${ieee_src}/angle.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/angle.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/angle.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/angle.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi 
         found=1

      elif [[ -s ${ieee_src}/angle.${type}.ctl ]]; then
         $NCP ${ieee_src}/angle.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/angle.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/angle.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
         fi 
         found=1
      fi
 
      if [[ $found -eq 0 ]]; then
         if [[ $ctr -gt 0 ]]; then
            test_day=`$NDATE -24 ${pdy}00`
            ctr=$(($ctr-1)) 
         else
            finished=1
         fi
      fi
   done

   if [[ -s ${imgndir}/${type}.ctl.${Z} || -s ${imgndir}/${type}.ctl ]]; then
      allmissing=0
      found=1
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All angle control files are missing from ${TANKDIR} for requested date range.
   exit
fi

#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the angle control 
#   files. Conditionally rm "cray_32bit_ieee" from the options line.
 
for type in ${SATYPE}; do
   if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
     ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
   fi
   ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

#   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
#      sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
#      mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
#   fi

done


for sat in ${SATYPE}; do
   nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'` 
   if [[ $nchanl -lt 100 ]]; then
      SATLIST=" $sat $SATLIST "
   else
      bigSATLIST=" $sat $bigSATLIST "
   fi
done

${COMPRESS} -f ${imgndir}/*.ctl


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to angle subdir.
#
export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plotangle_${RADMON_SUFFIX}"

if [[ -d $PLOT_WORK_DIR ]]; then
   rm -f $PLOT_WORK_DIR
fi
mkdir -p $PLOT_WORK_DIR
cd $PLOT_WORK_DIR


  #-----------------------------------------------------------------
  # Loop over satellite types.  Submit job to make plots.
  #

list="count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1"

  if [[ ${MY_MACHINE} = "wcoss" || ${MY_MACHINE} = "cray" ]]; then
     suffix=a
     cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
     jobname=plot_${RADMON_SUFFIX}_ang_${suffix}
     logfile=$LOGdir/plot_angle_${suffix}.log

     rm -f $cmdfile
     rm -f $logfile

     rm $LOGdir/plot_angle_${suffix}.log

     for type in ${SATLIST}; do
       echo "$IG_SCRIPTS/plot_angle.sh $type $suffix '$list'" >> $cmdfile
     done
     chmod 755 $cmdfile
     echo "CMDFILE:  $cmdfile"

     ntasks=`cat $cmdfile|wc -l `

     if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
        wall_tm="2:30"
     else
        wall_tm="1:45"
     fi

     if [[ ${MY_MACHINE} = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 20000 -W ${wall_tm} \
             -R affinity[core] -J ${jobname} -cwd ${PWD} $cmdfile
     else	# cray
        $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 600 -W ${wall_tm} \
             -J ${jobname} -cwd ${PWD} $cmdfile
     fi
  else				# Zeus/theia platform
     for sat in ${SATLIST}; do
        suffix=${sat} 
        cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
        jobname=plot_${RADMON_SUFFIX}_ang_${suffix}
        logfile=${LOGdir}/plot_angle_${suffix}.log

        rm -f $cmdfile
        rm -f $logfile

        echo "$IG_SCRIPTS/plot_angle.sh $sat $suffix '$list'" >> $cmdfile

        if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
           wall_tm="5:00:00"
        else
           wall_tm="2:30:00"
        fi

        $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
             -V -j oe -o ${logfile} ${cmdfile}
     done
  fi



  #----------------------------------------------------------------------------
  #  bigSATLIST
  #   
  #    There is so much data for some sat/instrument sources that a separate 
  #    job for each is necessary.
  #   

echo "starting $bigSATLIST"

set -A list count penalty omgnbc total omgbc fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1

for sat in ${bigSATLIST}; do
   echo processing $sat in $bigSATLIST

   #
   #  wcoss submit 4 jobs for each $sat
   #
   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then 	
      batch=1
      ii=0

      suffix="${sat}_${batch}"
      cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
      rm -f $cmdfile
      jobname=plot_${RADMON_SUFFIX}_ang_${suffix}
      logfile=${LOGdir}/plot_angle_${suffix}.log

      while [[ $ii -le ${#list[@]}-1 ]]; do

         echo "$IG_SCRIPTS/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile
         ntasks=`cat $cmdfile|wc -l `
         chmod 755 $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="3:00"
         else
            wall_tm="1:00"
         fi

        
#         mem="6000"
#         if [[ $batch -eq 1 ]]; then
            mem="24000"
#         fi

         if [[ $MY_MACHINE = "wcoss" ]]; then
            $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M ${mem} -W ${wall_tm} \
                 -R affinity[core] -J ${jobname} -cwd ${PWD} $cmdfile
         else
            $SUB -q $JOB_QUEUE -P $PROJECT -o ${logfile} -M 600 -W ${wall_tm} \
                 -J ${jobname} -cwd ${PWD} $cmdfile
         fi

         (( batch=batch+1 ))

         suffix="${sat}_${batch}"
         cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}
         rm -f $cmdfile
         jobname=plot_${RADMON_SUFFIX}_ang_${suffix}
         logfile=${LOGdir}/plot_angle_${suffix}.log

         (( ii=ii+1 ))
      done

   else					# Zeus, submit 1 job for each sat/list item

      ii=0
      suffix="${sat}"

      while [[ $ii -le ${#list[@]}-1 ]]; do
         cmdfile=${PLOT_WORK_DIR}/cmdfile_pangle_${suffix}_${list[$ii]}
         rm -f $cmdfile
         logfile=${LOGdir}/plot_angle_${suffix}_${list[$ii]}.log
         jobname=plot_${RADMON_SUFFIX}_ang_${suffix}_${list[$ii]}

         echo "${IG_SCRIPTS}/plot_angle.sh $sat $suffix ${list[$ii]}" >> $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="5:00:00"
         else
            wall_tm="2:30:00"
         fi

         $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
              -V -j oe -o ${logfile} ${cmdfile}

         (( ii=ii+1 ))
      done
  fi

done


echo "End mk_angle_plots.sh"
exit
