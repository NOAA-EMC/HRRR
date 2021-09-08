#!/bin/ksh

set -A WRF_DAT_FILES `ls alaska/hrrr_run_*`

for file in ${WRF_DAT_FILES[@]}; do
  if [ -s ${file} ]; then
    echo "${file}"
    newfile=`echo ${file} | sed 's/hrrr_run/hrrrak_run/g'`
    echo ${newfile}
    mv ${file} ${newfile}
  fi
done

