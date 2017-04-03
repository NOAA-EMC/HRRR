################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         hrrr_smartinit.sh
# Script description:  To generate the smartinit products for the Hi-Res Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2014-04-15
#
# Script history log:
# 2014-04-15  G Manikin  -- new script
#

set -xa

fhr=$1
typeset -Z2 fhr

mkdir -p $DATA/smart
cd $DATA/smart

export tmmark=tm00
WGRIB2=$EXEChrrr/hrrr_wgrib2

cp ${COMIN}/hrrr.t${cyc}z.wrfnatf${fhr}.grib2 WRFNAT${fhr}.tm00
${GRB2INDEX} WRFNAT${fhr}.tm00 WRFNAT${fhr}i.tm00

cp ${FIXhrrr}/hrrr_terrain_consensus.grb TOPONDFDCS
cp ${FIXhrrr}/hrrr_smartmask_consensus.grb LANDNDFDCS

$GRBINDEX TOPONDFDCS TOPONDFDCSI
$GRBINDEX LANDNDFDCS LANDNDFDCSI

# strip out records not needed for awips
cp ${PARMhrrr}/hrrr_smartnatparams .
${WGRIB2} WRFNAT${fhr}.tm00 | grep -F -f hrrr_smartnatparams | ${WGRIB2} -i -grib hrrr_natgrd.tm00 WRFNAT${fhr}.tm00

${GRB2INDEX} hrrr_natgrd.tm00 grib2file_index 

export wgrib2def="lambert:265:25.0:25.0 238.445999:2145:2539.703 20.191999:1377:2539.703"
${WGRIB2} hrrr_natgrd.tm00 -set_grib_type c3 -set_bitmap 1 -new_grid_winds grid -new_grid_interpolation bilinear -new_grid ${wgrib2def} hrrr.NDFDCSf${fhr}.grib2

mv hrrr.NDFDCSf${fhr}.grib2 hrrr.NDFDCSf${fhr}
${GRB2INDEX} hrrr.NDFDCSf${fhr} hrrr.NDFDCSf${fhr}I

cp /gpfs/hps/nco/ops/com/date/t${cyc}z DATE

export pgm=hrrr_smartinit_conus
. prep_step

ln -sf hrrr.NDFDCSf${fhr}     fort.11
ln -sf hrrr.NDFDCSf${fhr}I    fort.12
ln -sf TOPONDFDCS             fort.46
ln -sf TOPONDFDCSI            fort.47
ln -sf LANDNDFDCS             fort.48
ln -sf LANDNDFDCSI            fort.49
ln -sf HRRRCS${fhr}.tm00      fort.71

rm -rf smart.ksh
varEOF=EOF
cat > smart.ksh <<EOF
#!/bin/ksh -l
/gpfs/hps/emc/meso/noscrub/Benjamin.Blake/nwprod/hrrr.v2.0.10/exec/hrrr_smartinit_conus <<EOF >> smartinitcs.out${fhr}
$fhr
$cyc
$varEOF
EOF
chmod 755 smart.ksh
aprun -n 1 smart.ksh

export err=$?; err_chk

mv HRRRCS${fhr}.tm00 HRRRCS${fhr}.tm00.grib2
cp HRRRCS${fhr}.tm00.grib2 $DATA/.
cp HRRRCS${fhr}.tm00.grib2 $COMOUT/hrrr.t${cyc}z.smarthrrrconusf${fhr}.grib2
cd $DATA
exit
