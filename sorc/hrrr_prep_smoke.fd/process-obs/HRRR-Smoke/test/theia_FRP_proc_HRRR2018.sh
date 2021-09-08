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

# LOCAL TO FRP CODE AND TEMP AND OUTPUT FOLDERS
path_stat="/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/Fire_emiss3.0/LU_data/"		#/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/Fire_emiss3.0/Work_dir/Debugging/Code2.1

path_work="/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/BBemiss_pre4.0/"
temp=${path_work}"Work_dir/Temp/"

# INPUT FIRE FOLDERS TO MODIS abd VIIRS
#path_sat="/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/BBemiss_pre4.0/Input_data/2018/"
path_sat="/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/BBemiss_pre4.0/Input_data/2018/"
path_proc="/scratch3/BMC/ap-fc/Ravan/HRRR_smoke/BBemiss_pre4.0/Output/HRRR-Smoke/"

# LANDUSE DATA for HRRR-Smoke
igbp=${path_stat}"MCD12_2013_NA_3KM.bin"
biom=${path_stat}"BIOME_NA_3km.bin"

# LOCAL TO CODES 
codes=${path_work}"HRRR-Smoke_programs/"
r_npp=${codes}"proc_NPP_FRP_HRRR_v3.exe"
r_j01=${codes}"proc_J01_FRP_HRRR_v3.exe"
r_modis=${codes}"proc_MODIS_FRP_HRRR_v3.exe"
r_frp=${codes}"merge_FRP_HRRR_v3.exe"
r_bbm=${codes}"FRE_BBM_HRRR_v3.exe"

###################################################################
############## DO NOT CHANGE FROM HERE  ###########################
###################################################################

# Start of forecast, number of days
fcst_start=2018101800       # Start of forecast in 'YYYYMMDDHH' format

yy=$(echo ${fcst_start} | cut -c1-4)
mm=$(echo ${fcst_start} | cut -c5-6)
dd=$(echo ${fcst_start} | cut -c7-8)
hh=$(echo ${fcst_start} | cut -c9-10)
juld=$(date -d "${yy}${mm}${dd}" +%j)
juld0=$[$juld-1]
date0=$(date -d "`date +%Y`-01-01 +$(( ${juld0} - 1 ))days" +%Y%m%d)
#ndays=1

echo $date0

YYYYMMDDHH=`${DATE} +"%Y%m%d%H" -d "${time_start}"`
YYYYMMDD=`${DATE} +"%Y%m%d" -d "${time_start}"`
OLD_DAY=`${DATE} +"%Y%m%d" -d "${time_start} 1 day ago"`
OLD_JULIAN=`${DATE} +"%Y%j" -d "${time_start} 1 day ago"`
#HH=`${DATE} +"%H" -d "${time_start}"`

# Process each individual VIIRS hour, then copy into second working directory
evening_run=00     # in UTC
midnight_run=06
morning_run=12
afternoon_run=18

cd $path_proc
rm -f *.txt

# Start processing, S-NPP FRP files
    cd $path_sat
    if [ ${hh} -eq ${evening_run} ]; then
       for file in AF_v1r1_npp_s${date0}*.txt
       do
         hourv=$(echo ${file} | cut -c22-25)
         echo ${file}
         ${r_npp} ${file} $path_proc${yy}${juld0}${hourv}_npp_3km.txt  ${igbp}
       done
    fi

# NOAA-20 (JPSS-1) data
    if [ ${hh} -eq ${evening_run} ]; then
       for file in AF_v1r1_j01_s${date0}*.txt
       do
         hourv=$(echo ${file} | cut -c22-25)
         echo ${file}
         ${r_j01} ${file} $path_proc${yy}${juld0}${hourv}_j01_3km.txt  ${igbp}
       done
    fi

# MODIS (Aqua and Terra) data
    if [ ${hh} -eq ${evening_run} ]; then
	 echo MODIS_C6_Global_MCD14DL_NRT_${yy}${juld0}.txt

         ${r_modis} MODIS_C6_Global_MCD14DL_NRT_${yy}${juld0}.txt ${igbp} 00 24
         mv *mod_3km.txt $path_proc
    fi

    cd  $path_proc
    cat * > $yy$juld${hh}_daily3km.txt

    ${r_frp}  $yy$juld${hh}_daily3km.txt  $yy$juld${hh}_frp3km.txt

    ${r_bbm}  $yy$juld${hh}_frp3km.txt f$yy$juld${hh}_bbm3km_v3.txt   ${igbp}  ${biom}




