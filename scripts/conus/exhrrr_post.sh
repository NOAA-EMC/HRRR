#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_post.sh.sms
# Script description:  This script processes the Cloud input data for the HRRR
#
# Author:  Curtis Alexander / Geoffrey Manikin   Org: EMC     Date: 2011-08-24
#
# Script history log:
# 2014-08-01  C Alexander / G Manikin - HRRRv1
# 2016-02-01  G Manikin / C Alexander - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

# Set up paths to shell commands
AWK="/bin/gawk --posix"
DATE=/bin/date
CNVOPTS='-g12 -p32'
set -x

cd ${DATA}
START_TIME=${PDY}' '${cyc}
START_TIME=`${DATE} -d "${START_TIME}"`
# fhr is passed from the SMS script
fhr=$fhr

# Print out times
echo "   START TIME = "`${DATE} +%Y%m%d%H -d "${START_TIME}"`
echo "    fhr = ${fhr}"

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
export tmmark=tm00
export RSTFNL=${DATA}/
export CORE=RAPR
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2


timestr=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${START_TIME}  ${fhr} hours"`
timestr2=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${fhr} hours"`

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

rm -f fort.*
ln -sf ${PARMhrrr}/hrrr_post_avblflds.xml post_avblflds.xml
ln -sf ${PARMhrrr}/hrrr_params_grib2_tbl_new params_grib2_tbl_new

# use special postcntrl for fhr=0,1 to eliminate duplicate precip records
if [ $fhr -eq 0 -o $fhr -eq 1 ]
then
ln -sf ${PARMhrrr}/hrrr_postcntrl_anl.xml postcntrl.xml
ln -sf ${PARMhrrr}/hrrr_postxconfig_anl-NT.txt postxconfig-NT.txt
else
ln -sf ${PARMhrrr}/hrrr_postcntrl.xml postcntrl.xml
ln -sf ${PARMhrrr}/hrrr_postxconfig-NT.txt postxconfig-NT.txt
fi

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
$runline >> wrfpost.out
err=$?;export err ;err_chk

# create alternate version of sfc file to avoid duplicate parameters
#   when tacking it on to end of pressure file;  it also keeps the
#   pressure level records out of the native level file

cp WRFTWO${fhr}.tm00 hrrrsfc.1
cp ${PARMhrrr}/hrrr_remove_duplicates .
${WGRIB2} hrrrsfc.1 | grep -F -f hrrr_remove_duplicates | ${WGRIB2} -i -grib WRFTWOlite${fhr}.tm00 hrrrsfc.1

# Append entire wrfsfc to wrfprs and copy to COM
if [ $SENDCOM = YES ]
then
cat WRFPRS${fhr}.tm00 WRFTWOlite${fhr}.tm00 > WRFPRS${fhr}.tm00.new
mv WRFPRS${fhr}.tm00.new ${COMOUT}/hrrr.t${cyc}z.wrfprsf${fhr}.grib2

# Append entire wrfsfc to wrfnat and copy both to COM
cat WRFNAT${fhr}.tm00 WRFTWOlite${fhr}.tm00 > WRFNAT${fhr}.tm00.new
mv WRFNAT${fhr}.tm00.new ${COMOUT}/hrrr.t${cyc}z.wrfnatf${fhr}.grib2
cp WRFTWO${fhr}.tm00 ${COMOUT}/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2

# make index files
$WGRIB2 ${COMOUT}/hrrr.t${cyc}z.wrfprsf${fhr}.grib2 -s > ${COMOUT}/hrrr.t${cyc}z.wrfprsf${fhr}.grib2.idx
$WGRIB2 ${COMOUT}/hrrr.t${cyc}z.wrfnatf${fhr}.grib2 -s > ${COMOUT}/hrrr.t${cyc}z.wrfnatf${fhr}.grib2.idx
$WGRIB2 ${COMOUT}/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2 -s > ${COMOUT}/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2.idx
fi

if [ $SENDDBN = YES ]
 then
   $DBNROOT/bin/dbn_alert MODEL HRRR_PRS${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfprsf${fhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL HRRR_PRS_WIDX${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfprsf${fhr}.grib2.idx

   $DBNROOT/bin/dbn_alert MODEL HRRR_NAT${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfnatf${fhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL HRRR_NAT_WIDX${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfnatf${fhr}.grib2.idx

   $DBNROOT/bin/dbn_alert MODEL HRRR_SFC${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2
   $DBNROOT/bin/dbn_alert MODEL HRRR_SFC_WIDX${ALERT_EXT} $job $COMOUT/hrrr.t${cyc}z.wrfsfcf${fhr}.grib2.idx
fi

# at f00, save history file for next hour's f00 wrfbufr job which needs to compute 1-hr trends
if [ $fhr -eq 0 ]; then
 cp ${INPUT_DATA}/wrfout_d01_${timestr} ${COMOUT}/hrrr.t${cyc}z.wrfhistory00
fi

postdate=${PDY}${cyc}
echo "DATE  ${postdate}0000WASHINGTON" > DATE_SMARTINIT 
${USHhrrr}/hrrr_smartinit.sh $fhr

# Processing for AWIPS
export wgrib2def="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"

fhr1=`expr $fhr - 1`
if [ $fhr -gt 0 -a $fhr -lt 10 ]; then
typeset -Z1 fhr1 fhr
elif [ $fhr -eq 10 ]; then
typeset -Z1 fhr1
typeset -Z2 fhr
elif [ $fhr -ge 11 ]; then
typeset -Z2 fhr1 fhr
fi
echo $fhr1 $fhr

cp ${PARMhrrr}/hrrr_ndfdparams .
cp ${PARMhrrr}/hrrr_ndfdparams_fromsmart .
if [ $fhr -eq 0 ];then
 echo "APCP:surface:0-0 day acc fcst" >> pcpsnow.fil
 echo "WEASD:surface:0-0 day acc fcst" >> pcpsnow.fil
else
 echo "APCP:surface:${fhr1}-${fhr} hour acc fcst" >> pcpsnow.fil
 echo "WEASD:surface:${fhr1}-${fhr} hour acc fcst" >> pcpsnow.fil
fi

# strip out records not needed for awips
typeset -Z2 fhr
${WGRIB2} hrrrsfc.1 | grep -F -f hrrr_ndfdparams | ${WGRIB2} -i -grib tmpfile hrrrsfc.1
${WGRIB2} hrrrsfc.1 | grep -F -f pcpsnow.fil | ${WGRIB2} -i -grib tmpfile2 hrrrsfc.1
${WGRIB2} HRRRCS${fhr}.tm00.grib2 | grep -F -f hrrr_ndfdparams_fromsmart | ${WGRIB2} -i -grib tmpfile3 HRRRCS${fhr}.tm00.grib2 
cat tmpfile tmpfile2 > tmpfile2a
${GRB2INDEX} tmpfile2a grib2file_index
${WGRIB2} tmpfile2a -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} tmpfile2b 
${WGRIB2} tmpfile3  -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} tmpfile3a

cat tmpfile2b tmpfile3a > hrrr.t${cyc}z.ndfdf${fhr}.grib2

$USHhrrr/hrrr_mkawp.sh $fhr

# Save files for surface cycle
targetsize=16061363612
targetsize2=16073946524   ## HRRRv4 working dir on dell2
targetsize3=16057259932   ## HRRRv4 on WCOSS2
if [ $fhr -lt 12 ]
then
  counter=1
  while [[ $counter -lt 36 ]]; do
  filesize=$(stat -c%s ${INPUT_DATA}/wrfout_d01_${timestr})
  echo $filesize
# if [ $filesize -eq $targetsize ]; then
  if [[ $filesize -eq $targetsize || $filesize -eq $targetsize2 || $filesize -eq $targetsize3 ]]; then
    cp ${INPUT_DATA}/wrfout_d01_${timestr} ${HRRRGES_SFC}/hrrr_${PDY}${cyc}f0${fhr}
    break
  else
    sleep 5
    counter=` expr $counter + 1 `
  fi
  done
  if [ $counter -eq 36 ]; then
    echo "WARNING: Forecast Hour f0${fhr} still short on node! Copy anyway."
    cp ${INPUT_DATA}/wrfout_d01_${timestr} ${HRRRGES_SFC}/hrrr_${PDY}${cyc}f0${fhr}
  fi
fi

# Save 1-hr forecast file to NOMADS
if [ $fhr -eq 01 ]; then
  if [ $cyc -eq 02 -o $cyc -eq 05 -o $cyc -eq 08 -o $cyc -eq 11 -o $cyc -eq 14 -o $cyc -eq 17 -o $cyc -eq 20 -o $cyc -eq 23 ]; then
    if [ $SENDDBN = YES ]
     then
       $DBNROOT/bin/dbn_alert MODEL HRRR_F01${ALERT_EXT} $job ${HRRRGES_SFC}/hrrr_${PDY}${cyc}f0${fhr}
    fi
  fi
fi

echo "HRRR fcst hour $fhr post completed at `${DATE}`"

exit 0
