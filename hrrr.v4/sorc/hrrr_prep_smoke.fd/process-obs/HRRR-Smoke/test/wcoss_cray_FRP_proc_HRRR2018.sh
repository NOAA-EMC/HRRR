#!/bin/bash
# ==============================================================================   
# title           : run_frp.sh
# description     : This script will get all files from MODIS and VIIRS and convert into FRE
# authors		  : GMAI Team, modified by R.Ahmadov (NOAA/ESRL)
# date            : 20170201
# version         : 1.0    
# usage		 	  : bash create_emissions_frp_v.1.0.sh
# notes			  :
# ==============================================================================
# DATE IN FORMAT =  YYYY-MM-DD
# YYYY = YEAR (2000 2001 ...)
# MM = MONTH (01 02 03 04 05 06 07 08 09 10 11 12)
# DD = DAY (01 02 ... 30/31)
# VIIRS data files: AF_v1r1_npp_s201805081032451_e201805081034093_c201805081114070.txt  (~100 every)
# MODIS data files: MODIS_C6_Global_MCD14DL_NRT_2018127.txt     (daily)

set -xue

# Set up paths to shell commands
LS=/bin/ls
LN=/bin/ln
RM=/bin/rm
MKDIR=/bin/mkdir
CP=/bin/cp
MV=/bin/mv
ECHO=/bin/echo
CAT=/bin/cat
GREP=/bin/grep
CUT=/bin/cut
AWK="/bin/gawk --posix"
SED=/bin/sed
DATE=/bin/date

base_data=/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/ap-fc/Ravan/HRRR_smoke
path_work="/gpfs/dell2/stmp/$USER"

here=$( cd $( dirname $0 ) ; pwd -P )
process_obs_codes=$here/../src/
prep_chem_codes=$here/../../../prep-chem/Prep_smoke_FRP/bin/
fires_ncfmake_codes=$here/../../../prep-chem/bin2wrfinput/

if [ ! test -d "$base_data" ] ; then
    echo "$base_data: missing or not a directory" 1>&2
    exit 1
fi

# LOCAL TO FRP CODE AND TEMP AND OUTPUT FOLDERS
path_stat="$base_data/Fire_emiss3.0/LU_data/"		#$base_data/Fire_emiss3.0/Work_dir/Debugging/Code2.1

workdir="${path_work}/Work_dir/Temp.$$.$RANDOM/"
mkdir -p "$workdir"
cd "$workdir"

#VIIRS_NPP_DIR=/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/dcom_af_viirs/us007003/af_viirs
#VIIRS_J01_DIR=/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/dcom_af_viirs/us007003/af_viirs
VIIRS_NPP_DIR=/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/GSD-prep-chem/process-obs/HRRR-Smoke/test/frp_raw
VIIRS_J01_DIR=/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/GSD-prep-chem/process-obs/HRRR-Smoke/test/frp_raw
MODIS_FIRE_DIR=/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/MODISfiredata/datafiles/FIRMS/c6/Global
HRRR_DATABASE=/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/wrfinput/HRRRX-CONUS

# INPUT FIRE FOLDERS TO MODIS abd VIIRS
#path_sat="$base_data/BBemiss_pre4.0/Input_data/2018/"
path_sat="$base_data/BBemiss_pre4.0/Input_data/2018/"
path_proc="$base_data/BBemiss_pre4.0/Output/HRRR-Smoke/"

# LANDUSE DATA for HRRR-Smoke
igbp=${path_stat}"MCD12_2013_NA_3KM.bin"
biom=${path_stat}"BIOME_NA_3km.bin"

# LOCAL TO CODES 
r_npp=${process_obs_codes}"proc_NPP_FRP_HRRR_v3.exe"
r_j01=${process_obs_codes}"proc_J01_FRP_HRRR_v3.exe"
r_modis=${process_obs_codes}"proc_MODIS_FRP_HRRR_v3.exe"
r_frp=${process_obs_codes}"merge_FRP_HRRR_v3.exe"
r_bbm=${process_obs_codes}"FRE_BBM_HRRR_v4.exe"
r_prep_chem=${prep_chem_codes}"prep_chem_sources_RADM_WRF_FIM_.exe"
r_fires_ncfmake=${fires_ncfmake_codes}"fires_ncfmake.x"

# Namelists:
prep_chem_sources_inp_in=${prep_chem_codes}"HRRR_CONUS_prep_chem_sources.inp.in"

test -x "$r_prep_chem"

###################################################################
############## DO NOT CHANGE FROM HERE  ###########################
###################################################################

# Start of forecast, number of days
fcst_start=2019050700       # Start of forecast in 'YYYYMMDDHH' format
fcst_start_cannonical="${fcst_start:0:4}-${fcst_start:4:2}-${fcst_start:6:2} ${fcst_start:8:2}:00:00 +0000"

time_start="${time_start:-$fcst_start}"
time_start_cannonical="${time_start:0:4}-${time_start:4:2}-${time_start:6:2} ${time_start:8:2}:00:00 +0000"

juld=$(date -d "$fcst_start_cannonical" +%j)
juld0=$[$juld-1]
date0=$(date -d "$fcst_start_cannonical - 1 day" +%Y%m%d)
hh=$(date -d "$fcst_start_cannonical" +%H)
dd=$(date -d "$fcst_start_cannonical" +%d)
mm=$(date -d "$fcst_start_cannonical" +%m)
yyyy=$(date -d "$fcst_start_cannonical" +%Y)
#ndays=1

echo $date0

YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${time_start_cannonical}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${time_start_cannonical}"`
OLD_DAY=`${DATE} +"%Y%m%d" -d "${time_start_cannonical} 1 day ago"`
OLD_JULIAN=`${DATE} +"%Y%j" -d "${time_start_cannonical} 1 day ago"`
#HH=`${DATE} +"%H" -d "${time_start_cannonical}"`

# Process each individual VIIRS hour, then copy into second working directory
evening_run=00     # in UTC
midnight_run=06
morning_run=12
afternoon_run=18

########################################################################
# Make a temporary directory for individual observations:

if [ -e parts ] ; then
    rm -rf parts
fi

mkdir parts
cd parts

########################################################################

echo "Start processing, S-NPP FRP files"
#    cd $path_sat
if [ ${hh} -eq ${evening_run} ]; then
    for file in $VIIRS_NPP_DIR/AF_v1r1_npp_s${date0}*.txt
    do
	lines=$( cat "$file" | wc -l )
	if [[ "$lines" -lt 2 ]] ; then
	    echo "No data in $file"
	elif ( echo "$file" | "$GREP" -E '\*' ) ; then
	    echo "No npp files for this date in $VIIRS_NPP_DIR"
	    exit 99
	else
	    head "$file"
	    hourv=$(basename ${file} | cut -c22-25 | $SED s,/,-,g )
	    echo ${file}
	    ${r_npp} ${file} ./${yyyy}${juld0}${hourv}_npp_3km.txt  ${igbp}
	    if [ -e fort.22 ] ; then
		echo "ABORT: did not open output file in $r_npp"
		exit 9
	    fi
	fi
    done
fi

########################################################################

echo "NOAA-20 (JPSS-1) data"
if [ ${hh} -eq ${evening_run} ]; then
    for file in $VIIRS_J01_DIR/AF_v1r1_j01_s${date0}*.txt
    do
	lines=$( cat "$file" | wc -l )
	if [[ "$lines" -lt 2 ]] ; then
	    echo "No data in $file"
	elif ( echo "$file" | "$GREP" -E '\*' ) ; then
	    echo "No npp files for this date in $VIIRS_J01_DIR"
	    exit 99
	else
	    head "$file"
	    hourv=$(basename ${file} | cut -c22-25 | $SED s,/,-,g )
	    echo ${file}
	    ${r_j01} ${file} ./${yyyy}${juld0}${hourv}_j01_3km.txt  ${igbp}
	    if [ -e fort.22 ] ; then
		echo "ABORT: did not open output file in $r_j01"
		exit 9
	    fi
	fi
    done
fi

########################################################################

echo "MODIS (Aqua and Terra) data"
if [ ${hh} -eq ${evening_run} ]; then
    modis_file=$MODIS_FIRE_DIR/MODIS_C6_Global_MCD14DL_NRT_${yyyy}${juld0}.txt
    if [ -s "$modis_file" ] ; then
        ${r_modis} "$modis_file" ${igbp} 00 24
	if [ -e fort.22 ] ; then
	    echo "ABORT: did not open output file in $r_modis"
	    exit 9
	fi
    fi
fi

########################################################################

echo "MERGE ALL OBS FILES INTO ONE"
cd ..
if [ -e whole ] ; then
    rm -rf whole
fi
mkdir whole
cd whole

cat $( ls -1 ../parts/*txt | sort ) > $yyyy$juld${hh}_daily3km.txt

${r_frp}  $yyyy$juld${hh}_daily3km.txt  $yyyy$juld${hh}_frp3km.txt

${r_bbm}  $yyyy$juld${hh}_frp3km.txt f$yyyy$juld${hh}_bbm3km_v3.txt   ${igbp}  ${biom}

########################################################################

echo "CONVERT TO BINARY"
cd ..
if [ -e bin ] ; then
    rm -rf bin
fi
mkdir bin
cd bin

cp -fp ../whole/f$yyyy$juld${hh}_bbm3km_v3.txt .
cat "$prep_chem_sources_inp_in" | $SED "s,%hh%,$hh,g ; s,%dd%,$dd,g ; s,%mm%,$mm,g ; s,%yyyy%,$yyyy,g" > prep_chem_sources.inp
"$r_prep_chem"

########################################################################

echo "CONVERT TO NETCDF"
cd ..
if [ -e input ] ; then
    rm -rf input
fi
mkdir input
cd input

cp -fp $( ls -1tr ../bin/*bin |tail -1 ) ./sources.bin
wrfinput_file=$HRRR_DATABASE/run/$fcst_start/wrfprd/wrfinput_d01
cp -fp "$wrfinput_file" wrfinput_d01
"$r_fires_ncfmake" ./wrfinput_d01 ./sources.bin

########################################################################

echo Done.