#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
#                      .                                             .
# Script name:         exhrrr_fcstpre.sh.ecf
# Script description:  runs the 1-hr forecast job for the HRRR spinup
#
# Author:    Curtis Alexander / Geoff Manikin    Org: EMC     Date: 2014-05-02
#
# Script history log:
# 2014-05-02  C Alexander / G Manikin - HRRRv1
# 2016-02-05  C Alexander - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

ulimit -c 0

set -x

AWK="/bin/gawk --posix"
DATE=/bin/date
export FCST_INTERVAL=3
export FCST_LENGTH=1
export OMP_NUM_THREADS=1

# Set up some constants
export WRF_NAMELIST=namelist.input

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${PARMhrrr}/hrrrak_run_LANDUSE.TBL          \
                     ${PARMhrrr}/hrrrak_run_RRTM_DATA            \
                     ${PARMhrrr}/hrrrak_run_RRTM_DATA_DBL        \
                     ${PARMhrrr}/hrrrak_run_RRTMG_LW_DATA        \
                     ${PARMhrrr}/hrrrak_run_RRTMG_LW_DATA_DBL    \
                     ${PARMhrrr}/hrrrak_run_RRTMG_SW_DATA        \
                     ${PARMhrrr}/hrrrak_run_RRTMG_SW_DATA_DBL    \
                     ${PARMhrrr}/hrrrak_run_VEGPARM.TBL          \
                     ${PARMhrrr}/hrrrak_run_GENPARM.TBL          \
                     ${PARMhrrr}/hrrrak_run_SOILPARM.TBL         \
                     ${PARMhrrr}/hrrrak_run_MPTABLE.TBL          \
                     ${PARMhrrr}/hrrrak_run_URBPARM.TBL          \
                     ${PARMhrrr}/hrrrak_run_URBPARM_UZE.TBL      \
                     ${PARMhrrr}/hrrrak_run_ETAMPNEW_DATA        \
                     ${PARMhrrr}/hrrrak_run_ETAMPNEW_DATA.expanded_rain        \
                     ${PARMhrrr}/hrrrak_run_ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${PARMhrrr}/hrrrak_run_ETAMPNEW_DATA_DBL    \
                     ${PARMhrrr}/hrrrak_run_co2_trans            \
                     ${PARMhrrr}/hrrrak_run_ozone.formatted      \
                     ${PARMhrrr}/hrrrak_run_ozone_lat.formatted  \
                     ${PARMhrrr}/hrrrak_run_ozone_plev.formatted \
                     ${PARMhrrr}/hrrrak_run_tr49t85              \
                     ${PARMhrrr}/hrrrak_run_tr49t67              \
                     ${PARMhrrr}/hrrrak_run_tr67t85              \
                     ${PARMhrrr}/hrrrak_run_grib2map.tbl         \
                     ${PARMhrrr}/hrrrak_run_gribmap.txt          \
                     ${PARMhrrr}/hrrrak_run_aerosol.formatted      \
                     ${PARMhrrr}/hrrrak_run_aerosol_lat.formatted  \
                     ${PARMhrrr}/hrrrak_run_aerosol_lon.formatted  \
                     ${PARMhrrr}/hrrrak_run_aerosol_plev.formatted \
                     ${PARMhrrr}/hrrrak_run_bulkdens.asc_s_0_03_0_9  \
                     ${PARMhrrr}/hrrrak_run_bulkradii.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrrak_run_capacity.asc           \
                     ${PARMhrrr}/hrrrak_run_CCN_ACTIVATE.BIN       \
                     ${PARMhrrr}/hrrrak_run_coeff_p.asc            \
                     ${PARMhrrr}/hrrrak_run_coeff_q.asc            \
                     ${PARMhrrr}/hrrrak_run_constants.asc          \
                     ${PARMhrrr}/hrrrak_run_kernels.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrrak_run_kernels_z.asc          \
                     ${PARMhrrr}/hrrrak_run_masses.asc             \
                     ${PARMhrrr}/hrrrak_run_termvels.asc           \
                     ${PARMhrrr}/hrrrak_run_wind-turbine-1.tbl     \
                     ${PARMhrrr}/hrrrak_run_freezeH2O.dat          \
                     ${PARMhrrr}/hrrrak_run_qr_acr_qg.dat          \
                     ${PARMhrrr}/hrrrak_run_qr_acr_qs.dat          \
                     ${PARMhrrr}/hrrrak_run_eclipse_besselian_elements.dat


# Check to make sure WRF DAT files exist
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    echo "FATAL ERROR: ${file} either does not exist or is empty"
    exit 1
  fi
done

if [ $cyc  -eq 00 ]; then
START_TIME=${PDYm1}' '${cycm1}
else
START_TIME=${PDY}' '${cycm1}
fi
START_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
START_TIME=`${DATE} -d "${START_TIME}"`

# Get the end time string
END_TIME=`${DATE} -d "${START_TIME}  ${FCST_LENGTH} hours"`

# Print run parameters
echo
echo "hrrr_fcstpre started at `${DATE}`"
echo
echo "FCST LENGTH   = ${FCST_LENGTH}"
echo "FCST INTERVAL = ${FCST_INTERVAL}"
echo
echo "START TIME = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${START_TIME}"`
echo "END TIME = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${END_TIME}"`
echo

# Set up the work directory and cd into it
cd ${DATA}

# Bring in initial condition file 
   if [ -s ${COMIN}/hrrrak.t${cyc}z.wrfguess_pre ]; then
     echo " Initial condition ${COMIN}/hrrrak.t${cyc}z.wrfguess_pre "
     ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess_pre wrfinput_d01
     ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess_pre wrfvar_output
     cp ${PARMhrrr}/hrrrak_wrfpre.nl ${WRF_NAMELIST}
   elif [ -s ${COMIN}/hrrrak.t${cyc}z.wrfguess_rap ]; then
     echo "WARNING: No ${COMIN}/hrrr.t${cyc}z.wrfguess_pre available, Using initial condition ${COMIN}/hrrr.t${cyc}z.wrfguess_rap"
     ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess_rap wrfinput_d01
     ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess_rap wrfvar_output
     cp ${PARMhrrr}/hrrrak_wrfpre_norad.nl ${WRF_NAMELIST}
   else
     echo "FATAL ERROR: No initial condition available for Pre-Forecast"
     export err=9; err_chk
   fi

# Add smoke fields to wrfinput_d01
${USHhrrr}/hrrr_smoke_wrfinput_pre.ksh wrfinput_d01
## cycle smoke
if [ $cyc -eq 00 ]; then
${USHhrrr}/hrrr_cycle_smoke.ksh wrfinput_d01 ${PDYm1}${cycm1}
else
${USHhrrr}/hrrr_cycle_smoke.ksh wrfinput_d01 ${PDY}${cycm1}
fi

# Bring in boundary file with data for previous hour
# 
if [ $cyc  -eq 00 ]; then
cp ${COMINm1}/hrrrak.t${cycm1}z.wrfbdy wrfbdy_d01
else
cp ${COMIN}/hrrrak.t${cycm1}z.wrfbdy wrfbdy_d01
fi

if [ ! -r wrfbdy_d01 ]; then
 if [ -r ${COMIN4}/hrrrak.t${cycm4}z.wrfbdy ];then
   cp ${COMIN4}/hrrrak.t${cycm4}z.wrfbdy wrfbdy_d01
 else
   echo "FATAL ERROR - no boundary condition available for Pre-Forecast"
   err=9; err_chk
 fi
fi
 
# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  tempfile=`basename ${file}`
  tempname=`echo ${tempfile} | sed s/hrrrak_run_//`
  rm -f ${tempname}
  ln -sf ${file} ${tempname}
done

# Get the start and end time components
start_year=`${DATE} +%Y -d "${START_TIME}"`
start_month=`${DATE} +%m -d "${START_TIME}"`
start_day=`${DATE} +%d -d "${START_TIME}"`
start_hour=`${DATE} +%H -d "${START_TIME}"`
start_minute=`${DATE} +%M -d "${START_TIME}"`
start_second=`${DATE} +%S -d "${START_TIME}"`
end_year=`${DATE} +%Y -d "${END_TIME}"`
end_month=`${DATE} +%m -d "${END_TIME}"`
end_day=`${DATE} +%d -d "${END_TIME}"`
end_hour=`${DATE} +%H -d "${END_TIME}"`
end_minute=`${DATE} +%M -d "${END_TIME}"`
end_second=`${DATE} +%S -d "${END_TIME}"`

# Compute number of days and hours for the run
(( run_days = 0 ))
(( run_hours = 0 ))

# Create patterns for updating the wrf namelist
run=[Rr][Uu][Nn]
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
year=[Yy][Ee][Aa][Rr]
month=[Mm][Oo][Nn][Tt][Hh]
day=[Dd][Aa][Yy]
hour=[Hh][Oo][Uu][Rr]
minute=[Mm][Ii][Nn][Uu][Tt][Ee]
second=[Ss][Ee][Cc][Oo][Nn][Dd]
interval=[Ii][Nn][Tt][Ee][Rr][Vv][Aa][Ll]

# Update the run_days in wrf namelist.input
cat ${WRF_NAMELIST} | sed "s/\(${run}_${day}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_days}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the run_hours in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${run}_${hour}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_hours}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the start time in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/" \
   | sed "s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/"                   \
   | sed "s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/"                       \
   | sed "s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/"                     \
   | sed "s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/"                 \
   | sed "s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update end time in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/" \
   | sed "s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/"                   \
   | sed "s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/"                       \
   | sed "s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/"                     \
   | sed "s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/"                 \
   | sed "s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update interval in namelist
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))
cat ${WRF_NAMELIST} | sed "s/\(${interval}_${second}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
   > ${WRF_NAMELIST}.new 
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# update bc
cp ${PARMhrrr}/hrrrak_update_bc_parame.in parame.in
startmsg
cp ${EXEChrrr}/hrrr_update_bc .
runline="mpiexec -n 1 -ppn 1 ./hrrr_update_bc"
$runline >> $DATA/$pgmout 2>errfile
export err=$?; err_chk

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`

# Run wrf
startmsg
cp ${EXEChrrr}/hrrr_wrfarw_fcst . 
#runline="aprun -n 1200 -N 24 ./hrrr_wrfarw_fcst"
runline="mpiexec -n 1200 -ppn 128 --cpu-bind core ./hrrr_wrfarw_fcst"
$runline
export err=$?; err_chk

YYYY=`${DATE} +"%Y" -d "${END_TIME}"`
MM=`${DATE} +"%m" -d "${END_TIME}"`
DD=`${DATE} +"%d" -d "${END_TIME}"`
cp wrfout_d01_${YYYY}-${MM}-${DD}_${cyc}_00_00 $COMOUT/hrrrak.t${cyc}z.wrfguess
err_chk

cp rsl.out.0000 ${COMOUT}/hrrrak.t${cyc}z.rslpreout
msg="JOB $job FOR RAP_PRE-FORECAST HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
