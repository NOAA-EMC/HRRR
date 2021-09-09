#!/bin/ksh

#------------------------------------------------------------------
#
#  mk_bcor_plots.sh
#
#  Submit plot jobs to make the bcor images.
#
#  Log:
#   08/2010  safford  initial coding (adapted from bcor.sh).
#------------------------------------------------------------------

set -ax
date

export NUM_CYCLES=${NUM_CYCLES:-121}
export CYCLE_INTERVAL=${CYCLE_INTERVAL:-6}

imgndir=${IMGNDIR}/bcor
tankdir=${TANKDIR}/bcor

if [[ ! -d ${imgndir} ]]; then
   mkdir -p ${imgndir}
fi


#-------------------------------------------------------------------
#  Locate/update the control files in $TANKDIR/radmon.$PDY.  $PDY
#  starts at END_DATE and walks back to START_DATE until ctl files
#  are found or we run out of dates to check.  Report an error to
#  the log file and exit if no ctl files are found.
#
allmissing=1
PDY=`echo $PDATE|cut -c1-8`

cycdy=$((24/$CYCLE_INTERVAL))           # number cycles per day
ndays=$(($NUM_CYCLES/$cycdy))           # number days in plot period

test_day=$PDATE

for type in ${SATYPE}; do
   found=0
   finished=0
   test_day=$PDATE
   ctr=$ndays

   while [[ $found -eq 0 && $finished -ne 1 ]]; do
      if [[ $REGIONAL_RR -eq 1 ]]; then         # REGIONAL_RR stores hrs 18-23 in next 
         tdate=`$NDATE +6 ${test_day}`          # day's radmon.yyymmdd directory
         pdy=`echo $test_day|cut -c1-8`
      else
         pdy=`echo $test_day|cut -c1-8`
      fi

      if [[ $TANK_USE_RUN -eq 1 ]]; then
         ieee_src=${TANKverf}/${RUN}.${PDY}/${MONITOR}
      else
         ieee_src=${TANKverf}/${MONITOR}.${PDY}
      fi

      if [[ -s ${ieee_src}/bcor.${type}.ctl.${Z} ]]; then
         $NCP ${ieee_src}/bcor.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
         if [[ -s ${ieee_src}/bcor.${type}_anl.ctl.${Z} ]]; then
            $NCP ${ieee_src}/bcor.${type}_anl.ctl.${Z} ${imgndir}/${type}_anl.ctl.${Z}
         fi
         found=1
      elif [[ -s ${ieee_src}/bcor.${type}.ctl ]]; then
         $NCP ${ieee_src}/bcor.${type}.ctl ${imgndir}/${type}.ctl
         if [[ -s ${ieee_src}/bcor.${type}_anl.ctl ]]; then
            $NCP ${ieee_src}/bcor.${type}_anl.ctl ${imgndir}/${type}_anl.ctl
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

#   elif [[ -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl || -s ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.${Z} ]]; then
#      $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl.${Z} ${imgndir}/${type}.ctl.${Z}
#      if [[ ! -s ${imgndir}/${type}.ctl.${Z} ]]; then
#         $NCP ${TANKDIR}/radmon.${PDY}/bcor.${type}.ctl ${imgndir}/${type}.ctl
#      fi
#      allmissing=0
#      found=1
#
#   elif [[ -s ${tankdir}/${type}.ctl.${Z} || -s ${tankdir}/${type}.ctl  ]]; then
#      $NCP ${tankdir}/${type}.ctl* ${imgndir}/.
#      allmissing=0
#      found=1
#
#   else
#      echo WARNING:  unable to locate ${type}.ctl
   fi
done

if [[ $allmissing = 1 ]]; then
   echo ERROR:  Unable to plot.  All bcor control files are missing.
   exit
fi


#-------------------------------------------------------------------
#   Update the time definition (tdef) line in the bcor control
#   files. Conditionally rm cray_32bit_ieee from options line.

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
   if [[ $nchanl -ge 100 ]]; then
      bigSATLIST=" $sat $bigSATLIST "      
   else         
      SATLIST=" $sat $SATLIST "
   fi
done

${COMPRESS} ${imgndir}/*.ctl


#------------------------------------------------------------------
#   Submit plot jobs
#

  plot_list="count total fixang lapse lapse2 const scangl clw cos sin emiss ordang4 ordang3 ordang2 ordang1"

  export PLOT_WORK_DIR=${PLOT_WORK_DIR}/plotbcor_${RADMON_SUFFIX}
  if [[ -d ${PLOT_WORK_DIR} ]]; then 
     rm -f ${PLOT_WORK_DIR}
  fi
  mkdir -p ${PLOT_WORK_DIR}
  cd ${PLOT_WORK_DIR}


  #-------------------------------------------------------------------------
  # Loop over satellite/instruments.  Submit poe job to make plots.  Each task handles
  # a single satellite/insrument.

  if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then	
     suffix=a
     cmdfile=cmdfile_pbcor_${suffix}
     jobname=plot_${RADMON_SUFFIX}_bcor_${suffix}
     logfile=${LOGdir}/plot_bcor_${suffix}.log

     rm -f ${cmdfile}
     rm -f ${logfile}

>$cmdfile
     for sat in ${SATLIST}; do
        echo "$IG_SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile
     done
     chmod 755 $cmdfile

     ntasks=`cat $cmdfile|wc -l `

     if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
        wall_tm="2:30"
     else
        wall_tm="0:45"
     fi

     if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core] -o ${logfile} \
             -W ${wall_tm} -J ${jobname} -cwd ${PWD} ./$cmdfile
     else
        $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -o ${logfile} -W ${wall_tm} \
             -J ${jobname} -cwd ${PWD} ./$cmdfile
     fi
  else					#Zeus/linux
     for sat in ${SATLIST}; do
        suffix=${sat}
        cmdfile=cmdfile_pbcor_${sat}
        jobname=plot_${RADMON_SUFFIX}_bcor_${sat}
        logfile=${LOGdir}/plot_bcor_${sat}.log

        rm -f $cmdfile
        rm -f $logfile

        echo "$IG_SCRIPTS/plot_bcor.sh $sat $suffix '$plot_list'" >> $cmdfile

        if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
           wall_tm="1:30:00"
        else
           wall_tm="0:25:00"
        fi

        $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
             -V -j oe -o ${logfile} $cmdfile
     done
  fi

  #--------------------------------------------------------------------------
  #  bigSATLIST
  #  
  #    Some satellite/instrument sources have so many channels that a separate
  #    job to handle each plot type is the fastest solution.
  #
  #--------------------------------------------------------------------------
  for sat in ${bigSATLIST}; do
     suffix=$sat

     if [[ $MY_MACHINE = "wcoss" || $MY_MACHINE = "cray" ]]; then

        cmdfile=cmdfile_pbcor_${suffix}
        jobname=plot_${RADMON_SUFFIX}_bcor_${suffix}
        logfile=${LOGdir}/plot_bcor_${suffix}.log

        rm -f $cmdfile
        rm ${logfile}

>$cmdfile
        for var in $plot_list; do
           echo "$IG_SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile
        done
        chmod 755 $cmdfile
        ntasks=`cat $cmdfile|wc -l `

        if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
           wall_tm="2:30"
        else
           wall_tm="1:00"
        fi

        if [[ $MY_MACHINE = "wcoss" ]]; then
           $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core] -o ${logfile} \
                -W ${wall_tm} -J ${jobname} -cwd ${PWD} ./$cmdfile
        else      
           $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -o ${logfile} -W ${wall_tm} \
                -J ${jobname} -cwd ${PWD} ./$cmdfile
        fi
     else					# zeus/linux
        for var in $plot_list; do
           cmdfile=cmdfile_pbcor_${suffix}_${var}
           jobname=plot_${RADMON_SUFFIX}_bcor_${suffix}_${var}
           logfile=${LOGdir}/plot_bcor_${suffix}_${var}.log

           rm -f ${cmdfile}
           rm -f ${logfile}

           echo "$IG_SCRIPTS/plot_bcor.sh $sat $var $var" >> $cmdfile

           if [[ $PLOT_ALL_REGIONS -eq 1 || $ndays -gt 30 ]]; then
              wall_tm="4:00:00"
           else
              wall_tm="2:00:00"
           fi

           $SUB -A $ACCOUNT -l procs=1,walltime=${wall_tm} -N ${jobname} \
                -V -j oe -o ${logfile} $cmdfile

        done
     fi
  done

exit
