#!/bin/ksh --login

set -x 

location=/gpfs/hps3/ptmp/Benjamin.Blake
savelocation=/gpfs/hps3/ptmp/Benjamin.Blake
cd ${savelocation}

timestring=2019-11-05_09_00_00
timestring2=2019-11-05_10_00_00
cyc=09

mkdir -p save_hrrr_fire_${cyc}
cp $location/hrrr_alaska_fcst_${cyc}/wrfbdy_d01 save_hrrr_fire_${cyc}/.
cp $location/hrrr_alaska_fcst_${cyc}/wrfinput_d01 save_hrrr_fire_${cyc}/.
cp $location/hrrr_alaska_fcst_${cyc}/namelist.input  save_hrrr_fire_${cyc}/.
cp $location/hrrr_alaska_fcst_${cyc}/wrfout_d01_${timestring}   save_hrrr_fire_${cyc}/.
cp $location/hrrr_alaska_fcst_${cyc}/wrfout_d01_${timestring2}   save_hrrr_fire_${cyc}/.
cp -r $location/hrrr_alaska_prep_smoke_${cyc} save_hrrr_fire_${cyc}/.
smokefile=smoke_file_hrrr$cyc.tar

tar -cvf $smokefile save_hrrr_fire_${cyc}

#rm -fr save_hrrr_fire_${cyc}

/usrx/local/prod/hpss/hsi put $smokefile : 1year/NCEPDEV/emc-meso/Benjamin.Blake/temp/$smokefile

exit
