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
# 2018-01-24  B Blake - HRRRv3
#################################################################################

set -xa

fhr=$1

fhr=$(printf "%02d" $fhr)

run184="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36"
if  echo $run184 |grep $fhr;
then
  # Processing AWIPS grid 184 
  $GRB2INDEX hrrr.t${cyc}z.ndfdf${fhr}.grib2 hrrr.t${cyc}z.ndfdf${fhr}.grib2i 
  export pgm=tocgrib2
  . prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes 
  export FORT11=hrrr.t${cyc}z.ndfdf${fhr}.grib2
  export FORT12=hrrr.t${cyc}z.ndfdf${fhr}.grib2i
  export FORT51=xtrn.${cycle}.hrrr${fhr}

  $TOCGRIB2 <$PARMhrrr/wmo/grib2_awips_hrrrf${fhr}.184  parm='KWBY' >> $pgmout 2> errfile
# err=$?;export err ;err_chk
  err=$?;export err ;echo "err=$err "

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.hrrr${fhr} $WMO/grib2.t${cyc}z.awphrrr184_f${fhr}_${cyc}
  fi

  if [ $SENDDBN_NTC = YES -a $fhr -le 18 ]
  then
#    $DBNROOT/bin/dbn_alert MODEL NTC_LOW${ALERT_EXT} $job $WMO/grib2.${cycle}.awphrrr184_f${fhr}_${cyc} 
    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $WMO/grib2.${cycle}.awphrrr184_f${fhr}_${cyc} 
  fi
fi
