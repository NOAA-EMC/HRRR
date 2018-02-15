################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hrrrak_smartinit.sh
# Script description:  To generate the smartinit products for the Hi-Res Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2014-04-15
#
# Script history log:
# 2014-04-15  G Manikin  -- new script
# 2017-04-11  B Blake    -- grib 2 version of script
# 2017-09-29  A Gibbs    -- Alaska version of the HRRR smartinit

set -xa

fhr=$1
typeset -Z2 fhr

mkdir -p $DATA/smart
cd $DATA/smart

cp ${COMIN}/hrrr.t${cyc}z.wrfnatf${fhr}.ak.grib2 WRFNAT${fhr}.tm00
${GRB2INDEX} WRFNAT${fhr}.tm00 WRFNAT${fhr}i.tm00

cp ${FIXhrrr}/hrrrak_smartmaskak3.grb2 LANDNDFDAK
cp ${FIXhrrr}/hrrrak_smarttopoak3.grb2 TOPONDFDAK
${GRB2INDEX} TOPONDFDAK TOPONDFDAKI
${GRB2INDEX} LANDNDFDAK LANDNDFDAKI

# strip out records not needed for awips
cp ${PARMhrrr}/hrrrak_smartnatparams .
${WGRIB2} WRFNAT${fhr}.tm00 | grep -F -f hrrrak_smartnatparams | ${WGRIB2} -i -grib hrrr_natgrd.tm00 WRFNAT${fhr}.tm00

${GRB2INDEX} hrrr_natgrd.tm00 grib2file_index 

#export  wgrib2def="nps:210:60 181.429:1649:2976.563 40.530101:1105:2976.563"
export wgrib2def="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"

${WGRIB2} hrrr_natgrd.tm00 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} hrrr.NDFDAKf${fhr}.grib2

mv hrrr.NDFDAKf${fhr}.grib2 hrrr.NDFDAKf${fhr}
${GRB2INDEX} hrrr.NDFDAKf${fhr} hrrr.NDFDAKf${fhr}I

cp ${COMROOT}/date/t${cyc}z DATE

export pgm=hrrr_smartinit_ak
. prep_step

ln -sf hrrr.NDFDAKf${fhr}     fort.11
ln -sf hrrr.NDFDAKf${fhr}I    fort.12
ln -sf TOPONDFDAK             fort.46
ln -sf TOPONDFDAKI            fort.47
ln -sf LANDNDFDAK             fort.48
ln -sf LANDNDFDAKI            fort.49
ln -sf HRRRAK${fhr}.tm00      fort.71

rm -rf smart.ksh
varEOF=EOF
cat > smart.ksh <<EOF
#!/bin/ksh -l
#/gpfs/hps3/emc/meso/save/$USER/nwprod2/hrrr.v3.0.0/exec/hrrr_smartinit_ak <<EOF >> smartinitak.out${fhr}
$EXEChrrr/hrrr_smartinit_ak <<EOF >> smartinitak.out${fhr}
$fhr
$cyc
$varEOF
EOF
chmod 755 smart.ksh
aprun -n 1 smart.ksh

export err=$?; err_chk

mv HRRRAK${fhr}.tm00 HRRRAK${fhr}.tm00.grib2
cp HRRRAK${fhr}.tm00.grib2 $DATA/.
cp HRRRAK${fhr}.tm00.grib2 $COMOUT/hrrr.t${cyc}z.smarthrrrakf${fhr}.grib2
cd $DATA
exit
