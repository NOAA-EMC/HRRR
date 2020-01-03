#!/bin/sh
set -ax

#
# extract_err_rpt.sh
#
# Given a day and cycle and log file, extract the diagnostic error
# report and mail it to the recipient list.
#

function usage {
  echo "Usage:  extract_err_rpt.sh day cycle logfile"
  echo "            day is in 8 digit format [YYYYMMDD]"
  echo "            cycle is in 2 digit format [CC]"
  echo "            logfile is the relative or absolute path to the log file"
}


nargs=$#
if [[ $nargs -ne 3 ]]; then
   usage
   exit 1
fi

echo MAIL_TO = $MAIL_TO
echo MAIL_CC = $MAIL_CC


day=$1
cyc=$2
file=$3

diag_start=0
diag_end=0
data_start=0
data_end=0

err_rpt=error_report.txt

if [[ -s $err_rpt ]]; then
  rm $err_rpt
fi

nodiag=`grep 'NO DIAG REPORT' $file` 
if [[ $nodiag = "" ]]; then
  
   start=`grep -n 'cat diag_report.txt' $file`
   if [[ ! $start = "" ]]; then
      diag_start=`echo $start | sed 's/:/ /' | gawk '{print $1}'`
      diag_start=`expr $diag_start + 1`

      end=`grep -n 'End Problem Reading Diagnostic File' $file`
      diag_end=`echo $end | sed 's/:/ /2' | gawk '{print $9}'`
      diag_end=`expr $diag_end - 1`

      echo "Source = ${RADMON_SUFFIX}" >> $err_rpt
      echo " " >> $err_rpt
      gawk "NR>=$diag_start && NR<=$diag_end" $file >> $err_rpt
      echo " " >> $err_rpt
      echo " " >> $err_rpt
      echo " " >> $err_rpt
      echo " " >> $err_rpt
   fi
fi

missing_diag_file="./missing_diags.txt"
if [[ -s $missing_diag_file ]]; then
   echo "processing potential missing_diags"
   missing_diags=`cat $missing_diag_file`
   for var in ${missing_diags}; do
      test=`cat $err_rpt | grep $var`

      if [[ $test = "" ]]; then
         #-------------------------------------------------------------
         #  add this to the existing $err_rpt, or start a new $err_rpt
         #          
         if [[ ! -s $err_rpt ]]; then
            echo "Source = ${RADMON_SUFFIX}" >> $err_rpt
            echo " " >> $err_rpt
            echo "   WARNING:  NO DATA FOUND in cycle $PDATE for these sources:" >> $err_rpt
         fi
         echo "         $var" >> $err_rpt
      fi
   done
   echo " " >> $err_rpt
fi

noerr=`grep 'NO ERROR REPORT' $file`
if [[ $noerr = "" ]]; then
   start=`grep -n 'Begin Cycle Data Integrity Report' $file | tail -1`
   if [[ ! $start = "" ]]; then
      data_start=`echo $start | sed 's/:/ /' | gawk '{print $1}'`
      data_start=`expr $data_start + 1`

      end=`grep -n 'End Cycle Data Integrity Report' $file | tail -1`
      data_end=`echo $end | sed 's/:/ /' | gawk '{print $1}'`
      data_end=`expr $data_end - 1`

      if [[ ! data_start = "" && ! data_end = "" ]]; then
         echo "Source = ${RADMON_SUFFIX}" >> $err_rpt
         gawk "NR>=$data_start && NR<=$data_end" $file >> $err_rpt
      fi
   fi

   #-------------------------------------------------------------------
   #  change the links in $err_rpt to point to the correct suffix
   #  (opr) is hard-coded in the links in the error report at the moment
   if [[ $RAD_AREA = "rgn" ]]; then
      sed "s/\/opr\//\/regional\/${RADMON_SUFFIX}\//g"  $err_rpt > tmp.txt
   else
      sed "s/\/opr\//\/${RADMON_SUFFIX}\//g"  $err_rpt > tmp.txt
   fi
   rm -f $err_rpt
   mv -f tmp.txt $err_rpt

fi

#-------------------------------------------------------------------
#  mail error notifications

   if [[ -s ${err_rpt} ]]; then
      lines=`wc -l <${err_rpt}`
      if [[ $lines -gt 2 ]]; then
echo "" >> $err_rpt
echo "" >> $err_rpt
echo "" >> $err_rpt
echo "*********************** WARNING ***************************" >> $err_rpt
echo "THIS IS AN AUTOMATED EMAIL.  REPLIES TO SENDER WILL NOT BE"  >> $err_rpt
echo "RECEIVED.  PLEASE DIRECT REPLIES TO edward.safford@noaa.gov" >> $err_rpt
echo "*********************** WARNING ***************************" >> $err_rpt

         if [[ $MAIL_CC == "" ]]; then
            /bin/mail -s RadMon_error_report ${MAIL_TO}< ${err_rpt}
         else
            /bin/mail -s RadMon_error_report -c "${MAIL_CC}" ${MAIL_TO}< ${err_rpt}
         fi
      fi
   fi

exit
