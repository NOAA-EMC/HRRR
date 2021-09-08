#!/bin/ksh

#-------------------------------------------------------------------
#
#  script:  mk_time_plots.sh
#
#  submit plot jobs to make the time images.
#  
#-------------------------------------------------------------------

set -ax
date

echo Start mk_time_plots.sh
echo USE_ANL = $USE_ANL

export NUM_CYCLES=${NUM_CYCLES:-121}
export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}

imgndir=${IMGNDIR}/time
tankdir=${TANKDIR}/time

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi

#-------------------------------------------------------------------
#  Locate/update the control files.  If no ctl file is available
#  report a warning to the log file.  Search order is $imgndir,
#  the $TANKDIR/radmon.$PDY, then $tankdir.
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`

cycdy=$((24/$CYCLE_INTERVAL))		# number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))		# number days in plot period

test_day=$PDATE

for type in ${SATYPE}; do
   found=0
   finished=0
   test_day=$PDATE
   ctr=$ndays

   while [[ found -eq 0 && $finished -ne 1 ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then		# REGIONAL_RR stores hrs 18-23 in next 
         tdate=`$NDATE +6 ${test_day}`		# day's radmon.yyymmdd directory
         pdy=`echo $test_day|cut -c1-8`
      else
         pdy=`echo $test_day|cut -c1-8`
      fi

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}
      else
         ieee_src=${TANKverf}/${MONITOR}.${PDY}
      fi

      if [[ -s ${ieee_src}/time.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/time.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/time.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/time.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi
         found=1
      elif [[ -s ${ieee_src}/time.${type}.ctl ]]; then
         $NCP ${ieee_src}/time.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/time.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/time.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
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
   echo ERROR:  Unable to plot.  All time control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the time control
#   files.  Conditionally remove cray_32bit_ieee from the options line.


   for type in ${SATYPE}; do
      if [[ -s ${imgndir}/${type}.ctl.${Z} ]]; then
        ${UNCOMPRESS} ${imgndir}/${type}.ctl.${Z}
      fi
      ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}.ctl ${START_DATE} ${NUM_CYCLES}

      if [[ -s ${imgndir}/${type}_anl.ctl ]]; then
         ${IG_SCRIPTS}/update_ctl_tdef.sh ${imgndir}/${type}_anl.ctl ${START_DATE} ${NUM_CYCLES}
      fi
 
#      if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
#         sed -e 's/cray_32bit_ieee/ /' ${imgndir}/${type}.ctl > tmp_${type}.ctl
#         mv -f tmp_${type}.ctl ${imgndir}/${type}.ctl
#      fi
      
   done

   for sat in ${SATYPE}; do
      nchanl=`cat ${imgndir}/${sat}.ctl | gawk '/title/{print $NF}'`
      if [[ $nchanl -ge 100 ]]; then
         bigSATLIST=" $sat $bigSATLIST "
      else
         SATLIST=" $sat $SATLIST "
      fi
   done

   ${COMPRESS} ${imgndir}/*.ctl


#-------------------------------------------------------------------
#  Summary plots
#
#    Submit the summary plot job.
#
#-------------------------------------------------------------------

   jobname=plot_${RADMON_SUFFIX}_sum
   logfile=${LOGdir}/plot_summary.log
   rm ${logfile}

   if [[ $MY_MACHINE = "wcoss" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 100 -R affinity[core] -o ${logfile} \
           -W 1:00 -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_summary.sh
   elif [[ $MY_MACHINE = "cray" ]]; then
      $SUB -q $JOB_QUEUE -P $PROJECT -M 100 -o ${logfile} -W 1:00 \
           -J ${jobname} -cwd ${PWD} $IG_SCRIPTS/plot_summary.sh
   elif [[ $MY_MACHINE = "zeus" || $MY_MACHINE = "theia" ]]; then
      $SUB -A $ACCOUNT -l procs=1,walltime=1:00:00 -N ${jobname} \
           -V -j oe -o ${logfile} $IG_SCRIPTS/plot_summary.sh
   fi

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#  Time plots
#
#    Submit the time plot jobs.
#-------------------------------------------------------------------
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#   Rename PLOT_WORK_DIR to time subdir.
#
  export PLOT_WORK_DIR="${PLOT_WORK_DIR}/plottime_${RADMON_SUFFIX}"
  if [ -d $PLOT_WORK_DIR ] ; then
     rm -f $PLOT_WORK_DIR
  fi
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


#-------------------------------------------------------------------
#  Look over satellite types.  Submit plot job for each type.
#

   list="count penalty omgnbc total omgbc"


   if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then	
      suffix=a
      cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${suffix}
      jobname=plot_${RADMON_SUFFIX}_tm_${suffix}
      logfile=${LOGdir}/plot_time_${suffix}.log

      rm -f $cmdfile
      rm ${logfile}

>$cmdfile

      for sat in ${SATLIST}; do
         echo "$IG_SCRIPTS/plot_time.sh $sat $suffix '$list'" >> $cmdfile
      done
      chmod 755 $cmdfile

      if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
         wall_tm="2:30"
      else
         wall_tm="0:45"
      fi

      if [[ $MY_MACHINE = "wcoss" ]]; then
         $SUB -q $JOB_QUEUE -P $PROJECT -M 500 -R affinity[core] -o ${logfile} \
              -W ${wall_tm} -J ${jobname} -cwd ${PWD} ${cmdfile}
      else
         $SUB -q $JOB_QUEUE -P $PROJECT -M 500 -o ${logfile} -W ${wall_tm} \
              -J ${jobname} -cwd ${PWD} ${cmdfile}
      fi
      
   else							# zeus||theia
      for sat in ${SATLIST}; do
         cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
         jobname=plot_${RADMON_SUFFIX}_tm_${sat}
         logfile=${LOGdir}/plot_time_${sat}

         rm -f ${cmdfile}
         rm -f ${logfile}

         echo "$IG_SCRIPTS/plot_time.sh $sat $sat '$list'" >> $cmdfile

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="1:30:00"
         else
            wall_tm="0:40:00"
         fi

         $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
              -V -j oe -o ${logfile} $cmdfile
      done
   fi


#---------------------------------------------------------------------------
#  bigSATLIST
#
#    For some sat/instrument sources (airs_aqua, iasi, etc) there is so much 
#    data that a separate job for each provides a faster solution.
#   
#---------------------------------------------------------------------------
   for sat in ${bigSATLIST}; do 

      if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then	
         cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}
         jobname=plot_${RADMON_SUFFIX}_tm_${sat}
         logfile=${LOGdir}/plot_time_${sat}.log

         rm -f ${logfile}
         rm -f ${cmdfile}
 
         list="penalty count omgnbc total omgbc"
         for var in $list; do
            echo "$IG_SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile
         done
         chmod 755 $cmdfile

#         ntasks=`cat $cmdfile|wc -l `

         if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
            wall_tm="2:30"
         else
            wall_tm="1:00"
         fi
         if [[ $MY_MACHINE = "wcoss" ]]; then
            $SUB -q $JOB_QUEUE -P $PROJECT -M 500  -R affinity[core] -o ${logfile} \
                 -W ${wall_tm} -J ${jobname} -cwd ${PWD} ${cmdfile}
         else
            $SUB -q $JOB_QUEUE -P $PROJECT -M 500  -o ${logfile} -W ${wall_tm} \
                 -J ${jobname} -cwd ${PWD} ${cmdfile}
         fi
      else						# zeus||theia
         for var in $list; do
            cmdfile=${PLOT_WORK_DIR}/cmdfile_ptime_${sat}_${var}
            jobname=plot_${RADMON_SUFFIX}_tm_${sat}_${var}
            logfile=${LOGdir}/plot_time_${sat}_${var}.log
            rm -f ${logfile}
            rm -f ${cmdfile}

            if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
               wall_tm="2:00:00"
            else
               wall_tm="1:00:00"
            fi

            echo "$IG_SCRIPTS/plot_time.sh $sat $var $var" >> $cmdfile

            $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
                 -V -j oe -o ${logfile} $cmdfile
         done
      fi
   done

echo End mk_time_plots.sh
exit
