################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hrrr_mkawp.sh
# Script description:  To generate the AWIPS products for the HRRRR
#
# Author:      G Manikin /  EMC         Date: 2014-06-30
#
# Script history log:
# 2014-06-30  G Manikin  - adapted for HRRR 
#################################################################################

set -xa

fhr=$1

fhr=$(printf "%02d" $fhr)

run184="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18"
if  echo $run184 |grep $fhr;
then
  # Processing AWIPS grid 184 
  $GRBINDEX hrrr.t${cyc}z.ndfdf${fhr}.grib2 hrrr.t${cyc}z.ndfdf${fhr}.grib2i 
  export pgm=tocgrib2
  . prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes 
  export FORT11=hrrr.t${cyc}z.ndfdf${fhr}.grib2
  export FORT12=hrrr.t${cyc}z.ndfdf${fhr}.grib2i
  export FORT51=xtrn.${cycle}.hrrr${fhr}

  $TOCGRIB2 <$PARMutil/grib2_awips_hrrrf${fhr}.184  parm='KWBY' >> $pgmout 2> errfile
  err=$?;export err ;err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.hrrr${fhr} $PCOM/grib2.t${cyc}z.awphrrr184_f${fhr}_${cyc}
  fi

  if test "$SENDDBN" = 'YES'
  then
#    $DBNROOT/bin/dbn_alert MODEL NTC_LOW${ALERT_EXT} $job $PCOM/grib2.${cycle}.awphrrr184_f${fhr}_${cyc} 
    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $PCOM/grib2.${cycle}.awphrrr184_f${fhr}_${cyc} 
  fi
fi
