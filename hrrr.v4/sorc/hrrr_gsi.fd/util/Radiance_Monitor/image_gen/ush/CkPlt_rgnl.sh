#!/bin/sh
#--------------------------------------------------------------------
#
#  CkPlt_rgnl.sh
#
#  Check for new data and if found call scripts to plot image
#  files and transfer new images to web server.
#
#  Note:  this does not generate any data files.  Those must be
#  already created for this script to function correctly.
#
#--------------------------------------------------------------------

function usage {
  echo "Usage:  CkPlt_rgnl.sh suffix [plot_date]"
  echo "            Suffix is data source identifier that matches data in"
  echo "              TANKDIR/stats/regional directory."
  echo "            Plot_date, format YYYYMMDDHH is optional.  If included the plot"
  echo "              will be for the specified cycle, provided data files are available."
  echo "              If not included, the plot cycle will be the last processed cycle."
}


set -ax
echo start CkPlt_rgnl.sh



nargs=$#
if [[ $nargs -lt 1 || $nargs -gt 2 ]]; then
   usage
   exit 1
fi


this_file=`basename $0`
this_dir=`dirname $0`

RADMON_SUFFIX=$1
echo RADMON_SUFFIX    = ${RADMON_SUFFIX}

#--------------------------------------------------------------------
#  Set plot_time if it is included as an argument.
#--------------------------------------------------------------------
plot_time=
if [[ $nargs -eq 2 ]]; then
   plot_time=$2
fi


#--------------------------------------------------------------------
# Set environment variables
#--------------------------------------------------------------------
export RAD_AREA=rgn
export PLOT_ALL_REGIONS=
export REGIONAL_RR=${REGIONAL_RR:-0}
echo REGIONAL_RR      = ${REGIONAL_RR}

top_parm=${this_dir}/../../parm
export RADMON_VERSION=${RADMON_VERSION:-${top_parm}/radmon.ver}
if [[ -s ${RADMON_VERSION} ]]; then
   . ${RADMON_VERSION}
else
   echo "Unable to source ${RADMON_VERSION} file"
   exit 2
fi

export RADMON_CONFIG=${RADMON_CONFIG:-${top_parm}/RadMon_config}

if [[ -s ${RADMON_CONFIG} ]]; then
   . ${RADMON_CONFIG}
else
   echo "Unable to source ${RADMON_CONFIG}"
   exit
fi

if [[ -s ${RADMON_USER_SETTINGS} ]]; then
   . ${RADMON_USER_SETTINGS}
else
   echo "Unable to source ${RADMON_USER_SETTINGS}"
   exit
fi

. ${IG_PARM}/plot_rad_conf
. ${IG_PARM}/rgnl_conf


#--------------------------------------------------------------------
#  Check setting of RUN_ONLY_ON_DEV and possible abort if on prod and
#  not permitted to run there.
#--------------------------------------------------------------------

if [[ RUN_ONLY_ON_DEV -eq 1 ]]; then
   is_prod=`${IG_SCRIPTS}/onprod.sh`
   if [[ $is_prod = 1 ]]; then
      exit 10
   fi
fi

#--------------------------------------------------------------------


tmpdir=${STMP_USER}/plot_rgnl_rad${RADMON_SUFFIX}
rm -rf $tmpdir
mkdir -p $tmpdir
cd $tmpdir

export PLOT=0

mkdir -p $LOGdir


#--------------------------------------------------------------------
# Check status of plot jobs.  If any are still running then exit
# this script.  If none are running then remove any old job records
# in the $LOADLQ directory.
#--------------------------------------------------------------------

running=0
if [[ $MY_MACHINE = "wcoss" ]]; then
   running=`bjobs -l | grep plot_${RADMON_SUFFIX} | wc -l`
else
   running=`showq -n -u ${LOGNAME} | grep plot_${RADMON_SUFFIX} | wc -l`
fi

if [[ $running -ne 0 ]]; then
   echo plot jobs still running for $RADMON_SUFFIX, must exit
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi



#--------------------------------------------------------------------
# Get date of cycle to process.  Exit if images are up to current
# $PRODATE.
#
# If plot_time has been specified via command line argument, then
# set PDATE to it.  Otherwise, determine the last date processed 
# (into *.ieee_d files) and use that as the PDATE.
#--------------------------------------------------------------------
export PRODATE=`${IG_SCRIPTS}/find_cycle.pl --cyc 1 --dir ${TANKDIR}`

if [[ $plot_time != "" ]]; then
   export PDATE=$plot_time
else
   export PDATE=$PRODATE
fi
export START_DATE=`$NDATE -720 $PDATE`
echo "START_DATE, PRODATE, PDATE =  $START_DATE $PRODATE  $PDATE"


#--------------------------------------------------------------------
#  Note:  for REGIONAL_RR cases the 19z-00z data files are stored in
#         the next day's radmon.yyyymmdd file.  So for those cases
#         add 6 hrs to pdate and then set the $PDY value.
#
sdate=`echo $PDATE|cut -c1-8`
export CYA=`echo $PDATE|cut -c9-10`
echo "sdate, CYA = $sdate, $CYA"

if [[ $REGIONAL_RR -eq 1 ]]; then
   echo "getting date for REGIONAL_RR model"
   tdate=`$NDATE +6 $PDATE`
   export PDY=`echo $tdate|cut -c1-8`
else 
   export PDY=`echo $PDATE|cut -c1-8`
fi

#--------------------------------------------------------------------
#  exit if no new data is available

if [[ $PDATE -gt $PRODATE ]]; then
   cd $tmpdir
   cd ../
   rm -rf $tmpdir
   exit
fi


#--------------------------------------------------------------------
#  Plot all but horizontal data with each cycle.  Plot horizontal
#  data on 00z cycle. 

export PLOT=1

#if [[ "$CYA" = "00" ]];then
#   export PLOT_HORIZ=1
#fi


#--------------------------------------------------------------------
# If data is available, export variables, and submit driver for
# radiance monitoring jobs.

if [[ $PLOT -eq 1 ]]; then

   if [[ $USE_STATIC_SATYPE -eq 0 ]]; then

      if [[ -d ${TANKDIR}/radmon.${PDY} ]]; then
         test_list=`ls ${TANKDIR}/radmon.${PDY}/*angle.*${PDATE}.ieee_d.*`
      fi

      cut_grp=2
      if [[ $REGIONAL_RR -eq 1 ]]; then
         cut_grp=3
      fi

      for test in ${test_list}; do
         this_file=`basename $test`
         test_anl=`echo $this_file | grep "_anl"`
         if [[ $test_anl = "" ]]; then
            tmp=`echo "$this_file" | cut -d. -f${cut_grp}`
            echo $tmp
            SATYPE_LIST="$SATYPE_LIST $tmp"
         fi
      done

      SATYPE=$SATYPE_LIST

   else
      TANKDIR_INFO=${TANKDIR}/info
      export STATIC_SATYPE_FILE=${STATIC_SATYPE_FILE:-${TANKDIR_INFO}/SATYPE.txt}

      if [[ ! -e $STATIC_SATYPE_FILE ]]; then
         export STATIC_SATYPE_FILE=${HOMEnam}/fix/nam_radmon_satype.txt
      fi
      echo "STATIC_SATYPE_FILE = $STATIC_SATYPE_FILE"

      #-------------------------------------------------------------
      #  Load the SATYPE list from the STATIC_SATYPE_FILE or exit
      #  if unable to locate it.
      #-------------------------------------------------------------
      if [[ -s $STATIC_SATYPE_FILE ]]; then
         SATYPE=""
         SATYPE=`cat ${STATIC_SATYPE_FILE}`
         echo $SATYPE
      else
         echo Unable to locate $STATIC_SATYPE_FILE, must exit.
         cd $tmpdir
         cd ../
         rm -rf $tmpdir
         exit
      fi
 
   fi

 
  #------------------------------------------------------------------
  # Set environment variables.

  pid=${pid:-$$}
  export PLOT_WORK_DIR=${STMP_USER}/plotjobs_${RADMON_SUFFIX}.${pid}
  mkdir -p $PLOT_WORK_DIR
  cd $PLOT_WORK_DIR


  #--------------------------------------------------------------------
  #   Set environment variables to export to subsequent scripts
  if [[ $MY_MACHINE = "wcoss" ]]; then
     `module load GrADS/2.0.1`
     echo GADDIR = $GADDIR
  fi
  export datdir=$RADSTAT_LOCATION


  #------------------------------------------------------------------
  #   Submit plot jobs.

  if [[ $PLOT_HORIZ -eq 1 ]]; then
     logfile=${LOGdir}/mk_horiz_plots.log
     rm ${logfile}

     jobname=mk_plot_horiz_${RADMON_SUFFIX}
     if [[ $MY_MACHINE = "wcoss" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -R affinity[core]  -cwd ${PLOT_WORK_DIR} \
             -o ${logfile} -W 0:45 -J ${jobname} ${IG_SCRIPTS}/mk_horiz_plots.sh
     elif [[ $MY_MACHINE = "cray" ]]; then
        $SUB -q $JOB_QUEUE -P $PROJECT -M 80 -o ${logfile} -W 0:45 -cwd ${PLOT_WORK_DIR} \
             -J ${jobname} ${IG_SCRIPTS}/mk_horiz_plots.sh
     else
        $SUB -A $ACCOUNT -l procs=1,walltime=0:20:00 -N ${jobname} \
             -V -j oe -o $LOGdir/mk_horiz_plots.log $IG_SCRIPTS/mk_horiz_plots.sh
     fi
  fi

   ${IG_SCRIPTS}/mk_angle_plots.sh

   ${IG_SCRIPTS}/mk_bcoef_plots.sh

   if [[ ${PLOT_STATIC_IMGS} -eq 1 ]]; then
      ${IG_SCRIPTS}/mk_bcor_plots.sh
   fi

   ${IG_SCRIPTS}/mk_time_plots.sh

  #------------------------------------------------------------------
  #  Run the make_archive.sh script if $DO_ARCHIVE is switched on.
  #------------------------------------------------------------------
  if [[ $DO_ARCHIVE = 1 ]]; then
     ${IG_SCRIPTS}/nu_make_archive.sh
  fi

fi

#--------------------------------------------------------------------
#  Check for log file and extract data for error report there
#--------------------------------------------------------------------
if [[ $DO_DATA_RPT -eq 1 || $DO_DIAG_RPT -eq 1 ]]; then

   logfile=${LOGdir}/data_extract.${RADMON_SUFFIX}.${sdate}.${CYA}.log

   if [[ -s $logfile ]]; then
      ${IG_SCRIPTS}/extract_err_rpts.sh $sdate $CYA $logfile
   fi
fi

#--------------------------------------------------------------------
# Clean up and exit
cd $tmpdir
cd ../
rm -rf $tmpdir

exit
