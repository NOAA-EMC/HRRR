#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_postsubh.sh.ecf
# Script description:  This script processes the Cloud input data for the HRRR
#
# Author:  Curtis Alexander / Geoffrey.Manikin   Org: EMC     Date: 2011-08-24
#
# Script history log:
# 2014-08-01  C Alexander / G Manikin - HRRRv1
# 2016-02-01  G Manikin / C Alexander - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

set -x

# Set up paths to shell commands
AWK="/bin/gawk --posix"
DATE=/bin/date
export CORE=RAPR
export VALIDTIMEUNITS=FMIN
set -x

cd ${DATA}
START_TIME=${PDY}' '${cyc}
START_TIME=`${DATE} -d "${START_TIME}"`
# fhr and fmin are passed from the SMS script
fhr=$fhr
fmin=$fmin

# Print out times
echo "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
echo "    fhr = ${fhr}"

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export tmmark=tm00

export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2

timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME} ${fhr} hours ${fmin} min"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME} ${fhr} hours ${fmin} min"`
echo $timestr

cat > itag <<EOF
${INPUT_DATA}/wrfout_d01_${timestr}
netcdf
grib2
${timestr2}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

#cat > input${fhr}.prd <<EOF
#WRFTWO.GrbF${fhr}${fmin}
#EOF

rm -f fort.*
ln -sf ${PARMhrrr}/hrrr_post_avblflds.xml post_avblflds.xml
ln -sf ${PARMhrrr}/hrrr_params_grib2_tbl_new params_grib2_tbl_new
ln -sf ${PARMhrrr}/hrrr_postcntrl_subh.xml postcntrl.xml
ln -sf ${PARMhrrr}/hrrr_postxconfig_subh-NT.txt postxconfig-NT.txt
ln -sf ${PARMhrrr}/hrrr_run_ETAMPNEW_DATA eta_micro_lookup.dat

ln -sf ${FIXcrtm}/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
ln -sf ${FIXcrtm}/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
ln -sf ${FIXcrtm}/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
ln -sf ${FIXcrtm}/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
#ln -sf ${FIXhrrr}/hrrr_imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin
ln -sf ${FIXcrtm}/imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin

ln -sf ${FIXcrtm}/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
ln -sf ${FIXcrtm}/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
ln -sf ${FIXcrtm}/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
ln -sf ${FIXcrtm}/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
ln -sf ${FIXcrtm}/seviri_m10.TauCoeff.bin v.seviri_m10.TauCoeff.bin
#ln -sf ${FIXhrrr}/hrrr_imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin
ln -sf ${FIXcrtm}/imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin

ln -sf ${FIXcrtm}/NPOESS.IRice.EmisCoeff.bin NPOESS.IRice.EmisCoeff.bin
ln -sf ${FIXcrtm}/NPOESS.IRland.EmisCoeff.bin NPOESS.IRland.EmisCoeff.bin
ln -sf ${FIXcrtm}/NPOESS.IRsnow.EmisCoeff.bin NPOESS.IRsnow.EmisCoeff.bin
ln -sf ${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin Nalli.IRwater.EmisCoeff.bin

ln -sf ${FIXcrtm}/FASTEM6.MWwater.EmisCoeff.bin FASTEM6.MWwater.EmisCoeff.bin

ln -sf ${FIXcrtm}/CloudCoeff.bin CloudCoeff.bin
ln -sf ${FIXcrtm}/AerosolCoeff.bin AerosolCoeff.bin
#ln -sf ${FIXcrtm}/Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin
ln -sf ${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin EmisCoeff.bin

# Run unipost
startmsg
cp ${EXEChrrr}/hrrr_wrfpost .
runline="mpiexec -n 128 -ppn 128 ./hrrr_wrfpost"
$runline
err=$?;export err ;err_chk

cp WRFTWO* ${POSTMGRDIR}/wrfsfc_${fhr}${fmin}.grib2
${WGRIB2} ${POSTMGRDIR}/wrfsfc_${fhr}${fmin}.grib2 -s >${POSTMGRDIR}/wrfsfc_${fhr}${fmin}.grib2.idx
export err=$?;err_chk

if [ $fhr -eq 00 -a $fmin -eq 00 ]; then
if [ $SENDCOM = YES ]
then
   cp ${POSTMGRDIR}/wrfsfc_${fhr}${fmin}.grib2 ${COMOUT}/hrrr.t${cyc}z.wrfsubhf00.grib2
   $WGRIB2 ${COMOUT}/hrrr.t${cyc}z.wrfsubhf00.grib2 -s > ${COMOUT}/hrrr.t${cyc}z.wrfsubhf00.grib2.idx
   export err=$?;err_chk
fi

if [ $SENDDBN = YES ]
then
   $DBNROOT/bin/dbn_alert MODEL HRRR_SUBH${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL HRRR_SUBH_WIDX${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2.idx
fi
fi

if [ $fmin -eq 00 -a $fhr -ne 00 ]; then
# Append 15, 30, 45 and 60 min GRIB2 files together
  let checkCount=0
  while [ $checkCount -le 20 ]; do
    # Append 15, 30, 45 and 60 min GRIB2 files together
    if [ ! -r ${POSTMGRDIR}/wrfsfc_${fhrm1}15.grib2.idx -o ! -r ${POSTMGRDIR}/wrfsfc_${fhrm1}30.grib2.idx -o ! -r ${POSTMGRDIR}/wrfsfc_${fhrm1}45.grib2.idx ]; then
      sleep 45
      ((checkCount++))
      if [ $checkCount -gt 16 ]; then
        echo "FATAL ERROR: Waiting for previous ${POSTMGRDIR}/wrfsfc_${fhrm1}15.grib2 ${POSTMGRDIR}/wrfsfc_${fhrm1}30.grib2 \
              ${POSTMGRDIR}/wrfsfc_${fhrm1}45.grib2 for 10 minutes. Please check previous three post_subh jobs"
      fi
    else
      break #all files exist
    fi
  done

if [ $SENDCOM = YES ]
then
cat ${POSTMGRDIR}/wrfsfc_${fhrm1}15.grib2 ${POSTMGRDIR}/wrfsfc_${fhrm1}30.grib2 ${POSTMGRDIR}/wrfsfc_${fhrm1}45.grib2 ${POSTMGRDIR}/wrfsfc_${fhr}00.grib2 > ${COMOUT}/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2
export err=$?;err_chk

$WGRIB2 ${COMOUT}/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2 -s > ${COMOUT}/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2.idx
export err=$?;err_chk
fi

if [ $SENDDBN = YES ]
 then
   $DBNROOT/bin/dbn_alert MODEL HRRR_SUBH${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL HRRR_SUBH_WIDX${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsubhf${fhr}.grib2.idx
fi

fi

# make nawips file
# settings for nagrib2
cpyfil=gds
garea=dset
gbtbls=
maxgrd=4999
kxky=
grdarea=
proj=
output=T
pdsext=no
NAGRIB=nagrib2
cp WRFTWO* grib2f${fhr}${fmin}
cp $GEMPAKhrrr/fix/hrrr_g2varsncep1.tbl g2varsncep1.tbl
cp $GEMPAKhrrr/fix/hrrr_g2varswmo2.tbl g2varswmo2.tbl
cp $GEMPAKhrrr/fix/hrrr_g2vcrdncep1.tbl g2vcrdncep1.tbl
cp $GEMPAKhrrr/fix/hrrr_g2vcrdwmo2.tbl g2vcrdwmo2.tbl
fhr3=$fhr
typeset -Z3 fhr3

 $GEMEXE/$NAGRIB << EOF
   GBFILE   = grib2f${fhr}${fmin}
   INDXFL   = 
   GDOUTF   = ${RUN}subh_${PDY}${cyc}f${fhr3}${fmin} 
   PROJ     = $proj
   GRDAREA  = $grdarea
   KXKY     = $kxky
   MAXGRD   = $maxgrd
   CPYFIL   = $cpyfil
   GAREA    = $garea
   OUTPUT   = $output
   GBTBLS   = $gbtbls
   GBDIAG   = 
   PDSEXT   = $pdsext
  l
  r
EOF
gpend

if [ $SENDCOM = "YES" ] ; then
 mv ${RUN}subh_${PDY}${cyc}f${fhr3}${fmin} ${COMNAWP}/.${RUN}subh_${PDY}${cyc}f${fhr3}${fmin}
 mv ${COMNAWP}/.${RUN}subh_${PDY}${cyc}f${fhr3}${fmin} ${COMNAWP}/${RUN}subh_${PDY}${cyc}f${fhr3}${fmin}
if [ $SENDDBN = "YES" ] ; then
       $DBNROOT/bin/dbn_alert MODEL HRRR_GEMPAK_SUBH${ALERT_EXT} $job \
           $COMNAWP/${RUN}subh_${PDY}${cyc}f${fhr3}${fmin}
fi
fi

# processing sub-hourly data for AWIPS
if [ $fmin -eq 00 ]; then

run184="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18"
if  echo $run184 |grep $fhr;
then
  # Processing AWIPS grid 184 

export wgrib2def="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"
${WGRIB2} ${COMOUT}/hrrr.${cycle}.wrfsubhf${fhr}.grib2 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} hrrr.${cycle}.ndfdsubhf${fhr}.grib2
export err=$?;err_chk

$GRB2INDEX hrrr.${cycle}.ndfdsubhf${fhr}.grib2 hrrr.${cycle}.ndfdsubhf${fhr}.grib2i

  export FORTREPORTS=unit_vars=yes
  export FORT11=hrrr.${cycle}.ndfdsubhf${fhr}.grib2
  export FORT12=hrrr.${cycle}.ndfdsubhf${fhr}.grib2i
  export FORT51=xtrn_subh.${cycle}.hrrr${fhr}

  $TOCGRIB2 <$PARMhrrr/wmo/grib2_awips_hrrr_subhf${fhr}.184  parm='KWBY' >> $pgmout 2> errfile 

  cp xtrn_subh.${cycle}.hrrr${fhr} ${WMO}/grib2.${cycle}.awphrrr_subh_184_f${fhr}_${cyc}
  if [ $SENDDBN_NTC = YES ]; then
    $DBNROOT/bin/dbn_alert NTC_LOW $NET $job ${WMO}/grib2.${cycle}.awphrrr_subh_184_f${fhr}_${cyc}
  fi
fi
fi # end AWIPS processing
echo "post subh completed at `${DATE}`"
exit 0
