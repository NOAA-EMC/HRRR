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
if [ $cyc -eq 00 -o $cyc -eq 06 -o $cyc -eq 12 -o $cyc -eq 18 ]; then
  export FCST_LENGTH=48
  export NODES=36
else
  export FCST_LENGTH=18
  export NODES=18
fi

export OMP_NUM_THREADS=2

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
    err_exit 
  fi
done

START_TIME=${PDY}' '${cyc}
START_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
START_TIME=`${DATE} -d "${START_TIME}"`

# Get the end time string
END_TIME=`${DATE} -d "${START_TIME}  ${FCST_LENGTH} hours"`

# Print run parameters
echo
echo "hrrr forecast started at `${DATE}`"
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
if [ -s ${COMIN}/hrrrak.t${cyc}z.wrf_inout ]; then
   echo " Initial condition ${COMIN}/hrrrak.t${cyc}z.wrf_inout "
   ln -sf ${COMIN}/hrrrak.t${cyc}z.wrf_inout wrfinput_d01
   ln -sf ${COMIN}/hrrrak.t${cyc}z.wrf_inout wrfvar_output
elif [ -s $COMOUT/hrrrak.t${cyc}z.wrfguess ]; then
   echo " Could not find wrf_inout file"
   echo " Initial condition ${COMIN}/hrrrak.t${cyc}z.wrfguess "
   ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess wrfinput_d01
   ln -sf ${COMIN}/hrrrak.t${cyc}z.wrfguess wrfvar_output
else
   errmsg="FATAL ERROR: No initial condition available for forecast - exit"
   echo $errmsg
   err_exit $errmsg
fi

# Add smoke fields to wrfinput_d01
$USHhrrr/hrrr_smoke_wrfinput.ksh wrfinput_d01

# Bring in boundary file and namelist
if [ -r ${COMIN}/hrrrak.t${cyc}z.wrfbdy wrfbdy_d01 ]; then
 cp ${COMIN}/hrrrak.t${cyc}z.wrfbdy wrfbdy_d01
else
 errmsg="FATAL ERROR: cannot find boundary conditions - must exit !!!"
 echo $errmsg
 err_exit $errmsg
fi

cp ${PARMhrrr}/hrrrak_wrf.nl ${WRF_NAMELIST}

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

############################################################
# Run wrf

cp ${EXEChrrr}/hrrr_wrfarw_fcst .

if [[ "${NODES:-0}" -lt 1 ]] ; then
    echo "WARNING: \$NODES variable is unset.  Will use \$LSB_MAX_NUM_PROCESSORS and assume WCOSS Cray."
    provided_nodes=$(( LSB_MAX_NUM_PROCESSORS / 24 ))
else
    provided_nodes="$NODES"
fi

if [[ "$provided_nodes" == 18 ]] ; then
    # This is the operational configuration for the 18-hour forecasts.
cat<<EOF > run_wrf_config
omp_num_threads=2 # compute only; quilt is always 1
hyperthreads=1    # compute only
io_hyperthreads=2 # quilt only
turbo_mode=YES    # compute only; YES or NO
reorder_ranks=grid_order # grid_order or NO
mpich_tuning=cb_nodes # cb_nodes or NO

# Distribution of compute nodes in grid:
#nnode_x=7
#nnode_y=10
nnode_x=8
nnode_y=8

# Distribution of MPI ranks within each compute node:
#nrank_x=2
#nrank_y=6
nrank_x=2
nrank_y=8

# Quilt server configuration:
#nio_tasks_per_group=60
#nio_groups=4
nio_tasks_per_group=64
nio_groups=2
nio_ppn=30
EOF
else
    # Operational settings for 48-hour forecasts (138 nodes)
    cat<<EOF > run_wrf_config
omp_num_threads=2 # compute only; quilt is always 1
hyperthreads=1    # compute only
io_hyperthreads=2 # quilt only
turbo_mode=YES    # compute only; YES or NO
reorder_ranks=grid_order # grid_order or NO
mpich_tuning=cb_nodes # cb_nodes or NO

# Distribution of compute nodes in grid:
nnode_x=8
nnode_y=16

# Distribution of MPI ranks within each compute node:
nrank_x=2
nrank_y=8

# Quilt server configuration:
nio_tasks_per_group=128
nio_groups=2
nio_ppn=48
cb_nodes=32
EOF
fi

if ( ! $USHhrrr/hrrr_run_wrf.sh hrrr_wrfarw_fcst run_wrf_config ) ; then
    err_exit "WRF-ARW execution wrapper script failed."
fi
############################################################

err_chk
cp rsl.out.0000 ${COMOUT}/hrrrak.t${cyc}z.rslout
msg="JOB $job FOR HRRR_FORECAST HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"
exit 0
