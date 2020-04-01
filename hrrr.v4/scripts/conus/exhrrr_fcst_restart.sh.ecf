#!/bin/ksh --login

################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
#                      .                                             .
# Script name:         exhrrr_fcst.sh.ecf
# Script description:  runs the free forecast job for the HRRR after job has stalled
#
# Author:    Curtis Alexander / Geoff Manikin    Org: EMC     Date: 2014-05-02
#
# Script history log:
# 2014-05-02  C Alexander / G Manikin 
# 2018-02-05  B Blake - HRRRv3

export OMP_NUM_THREADS=2

ulimit -c 0

cd ${DATA}
echo 'in data '
# Copy boundary file into DATA
cp -pr ${FCSTPROD}/wrfbdy_d01 ${DATA}

echo 'past copy'
# Determine original START_TIME
firstfile=`ls ${FCSTPROD}/wrfout* | head -n 1`
echo 'past setting firstfile'
firstname=`basename ${firstfile}`
YYYY=`echo ${firstname} | cut -c 12-15`
MM=`echo ${firstname} | cut -c 17-18`
DD=`echo ${firstname} | cut -c 20-21`
HH=`echo ${firstname}  | cut -c 23-24` 

# Determine last forecast hour completed that is divisible by three and number of files to copy
lastfile=`ls ${FCSTPROD}/fcstdone[0-1][0-9]00.* | tail -n 1`
lastname=`basename ${lastfile}`
lasthour=`echo ${lastname} | cut -c 9-10`
lasthour=$(($lasthour - $lasthour%3))
filecount=$((4*$lasthour + 1))

# Copy fcstdone and wrfout files already completed
fcstdonelist=`ls ${FCSTPROD}/fcstdone* | head -n ${filecount}`
wrfoutlist=`ls ${FCSTPROD}/wrfout* | head -n ${filecount}`
lastwrfout=`ls ${wrfoutlist} | tail -n 1`
lastwrfoutname=`basename ${lastwrfout}`
cp -pr ${fcstdonelist} ${DATA}
cp ${FCSTPROD}/${lastwrfoutname} ${DATA}/.

# Set softlink to wrfinput
mv ${DATA}/${lastwrfoutname} ${DATA}/${lastwrfoutname}_output
ln -sf ${DATA}/${lastwrfoutname}_output ${DATA}/wrfinput_d01

# Set new START_TIME
DATE=/bin/date
ORIG_START_TIME="${YYYY}${MM}${DD} ${HH}"
ORIG_START_TIME_STRING="${YYYY}-${MM}-${DD}_${HH}:00:00"
START_TIME=`${DATE} +"%Y%m%d %H" -d "${ORIG_START_TIME} ${lasthour} hours"`
NEWHH=`${DATE} +"%H" -d "${START_TIME}"`

# Adjust FCST_LENGTH
if [ $cyc -eq 00 -o $cyc -eq 06 -o $cyc -eq 12 -o $cyc -eq 18 ]; then
  export FCST_LENGTH=$((36 - $lasthour))
else
  export FCST_LENGTH=$((18 - $lasthour))
fi

set -x

AWK="/bin/gawk --posix"
export FCST_INTERVAL=3

# Set up some constants
export WRF_NAMELIST=namelist.input

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${PARMhrrr}/hrrr_run_LANDUSE.TBL          \
                     ${PARMhrrr}/hrrr_run_RRTM_DATA            \
                     ${PARMhrrr}/hrrr_run_RRTM_DATA_DBL        \
                     ${PARMhrrr}/hrrr_run_RRTMG_LW_DATA        \
                     ${PARMhrrr}/hrrr_run_RRTMG_LW_DATA_DBL    \
                     ${PARMhrrr}/hrrr_run_RRTMG_SW_DATA        \
                     ${PARMhrrr}/hrrr_run_RRTMG_SW_DATA_DBL    \
                     ${PARMhrrr}/hrrr_run_VEGPARM.TBL          \
                     ${PARMhrrr}/hrrr_run_GENPARM.TBL          \
                     ${PARMhrrr}/hrrr_run_SOILPARM.TBL         \
                     ${PARMhrrr}/hrrr_run_MPTABLE.TBL          \
                     ${PARMhrrr}/hrrr_run_URBPARM.TBL          \
                     ${PARMhrrr}/hrrr_run_URBPARM_UZE.TBL      \
                     ${PARMhrrr}/hrrr_run_ETAMPNEW_DATA        \
                     ${PARMhrrr}/hrrr_run_ETAMPNEW_DATA.expanded_rain        \
                     ${PARMhrrr}/hrrr_run_ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${PARMhrrr}/hrrr_run_ETAMPNEW_DATA_DBL    \
                     ${PARMhrrr}/hrrr_run_co2_trans            \
                     ${PARMhrrr}/hrrr_run_ozone.formatted      \
                     ${PARMhrrr}/hrrr_run_ozone_lat.formatted  \
                     ${PARMhrrr}/hrrr_run_ozone_plev.formatted \
                     ${PARMhrrr}/hrrr_run_tr49t85              \
                     ${PARMhrrr}/hrrr_run_tr49t67              \
                     ${PARMhrrr}/hrrr_run_tr67t85              \
                     ${PARMhrrr}/hrrr_run_grib2map.tbl         \
                     ${PARMhrrr}/hrrr_run_gribmap.txt          \
                     ${PARMhrrr}/hrrr_run_aerosol.formatted      \
                     ${PARMhrrr}/hrrr_run_aerosol_lat.formatted  \
                     ${PARMhrrr}/hrrr_run_aerosol_lon.formatted  \
                     ${PARMhrrr}/hrrr_run_aerosol_plev.formatted \
                     ${PARMhrrr}/hrrr_run_bulkdens.asc_s_0_03_0_9  \
                     ${PARMhrrr}/hrrr_run_bulkradii.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrr_run_capacity.asc           \
                     ${PARMhrrr}/hrrr_run_CCN_ACTIVATE.BIN       \
                     ${PARMhrrr}/hrrr_run_coeff_p.asc            \
                     ${PARMhrrr}/hrrr_run_coeff_q.asc            \
                     ${PARMhrrr}/hrrr_run_constants.asc          \
                     ${PARMhrrr}/hrrr_run_kernels.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrr_run_kernels_z.asc          \
                     ${PARMhrrr}/hrrr_run_masses.asc             \
                     ${PARMhrrr}/hrrr_run_termvels.asc           \
                     ${PARMhrrr}/hrrr_run_wind-turbine-1.tbl     \
                     ${PARMhrrr}/hrrr_run_freezeH2O.dat          \
                     ${PARMhrrr}/hrrr_run_qr_acr_qg.dat          \
                     ${PARMhrrr}/hrrr_run_qr_acr_qs.dat

# Check to make sure WRF DAT files exist
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    echo "ERROR: ${file} either does not exist or is empty"
    err_exit 
  fi
done

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

# Bring in namelist
cp ${PARMhrrr}/hrrr_wrf.nl ${WRF_NAMELIST}

# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  tempfile=`basename ${file}`
  tempname=`echo ${tempfile} | sed s/hrrr_run_//`
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

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`


cp ${EXEChrrr}/hrrr_wrfarw_fcst .

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
nrank_y=6

# Quilt server configuration:
nio_tasks_per_group=96
nio_groups=5
nio_ppn=48
cb_nodes=32
EOF

$USHhrrr/hrrr_run_wrf.sh hrrr_wrfarw_fcst run_wrf_config

############################################################
# Run wrf

cp ${EXEChrrr}/hrrr_wrfarw_fcst .

module unload craype-hugepages2M
module unload craype-hugepages4M
module unload craype-hugepages8M
module unload craype-hugepages16M
module unload craype-hugepages32M
module unload craype-hugepages64M
module unload craype-hugepages128M
module unload craype-hugepages512M

set -e
module load craype-hugepages256M
set +e

if [[ "${NODES:-0}" -lt 1 ]] ; then
    echo "WARNING: \$NODES variable is unset.  Will use \$LSB_MAX_NUM_PROCESSORS and assume WCOSS Cray."
    provided_nodes=$(( LSB_MAX_NUM_PROCESSORS / 24 ))
else
    provided_nodes="$NODES"
fi

if [[ "$provided_nodes" == 78 ]] ; then
    # 78 node configuration for EMC/GSD HRRR parallel, running a
    # forecast every 3 hours:
    cat<<EOF > run_wrf_config
omp_num_threads=2 # compute only; quilt is always 1
hyperthreads=1    # compute only
io_hyperthreads=2 # quilt only
turbo_mode=YES    # compute only; YES or NO
reorder_ranks=grid_order # grid_order or NO
mpich_tuning=cb_nodes # cb_nodes or NO

# Distribution of compute nodes in grid:
nnode_x=6
nnode_y=12

# Distribution of MPI ranks within each compute node:
nrank_x=2
nrank_y=6

# Quilt server configuration:
nio_tasks_per_group=96
nio_groups=5
nio_ppn=48
cb_nodes=24
EOF
elif [[ "$provided_nodes" == 13 ]] ; then
    # 13 node configuration for EMC/GSD HRRR parallel run two out of
    # every three hours (hrs 1, 2, 4, 5, etc.) to maintain cycling.
    cat<<EOF > run_wrf_config
omp_num_threads=2 # compute only; io is always 1
mkl_num_threads=1 # compute only; io is always 1
hyperthreads=2 # compute only
io_hyperthreads=2 # io only
turbo_mode=YES # YES or NO
reorder_ranks=grid_order # LSF, MPICH, or NO

# Distribution of compute nodes in grid:
nnode_x=6
nnode_y=2

# Distribution of MPI ranks within each compute node:
nrank_x=2
nrank_y=12

# Quilt server configuration:
nio_tasks_per_group=24
nio_groups=2
nio_ppn=48
cb_nodes=4
EOF
else
    # Operational settings (138 nodes)
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
nrank_y=6

# Quilt server configuration:
nio_tasks_per_group=96
nio_groups=5
nio_ppn=48
cb_nodes=32
EOF
fi

if ( ! $USHhrrr/hrrr_run_wrf.sh hrrr_wrfarw_fcst run_wrf_config ) ; then
    err_exit "WRF-ARW execution wrapper script failed."
fi
############################################################

# Change wrfout metadata to original START_TIME
for wrfout in `ls wrfout_d01_*`
do
  ncatted -O -h -a START_DATE,global,o,c,"${ORIG_START_TIME_STRING}" ${wrfout}
  ncatted -O -h -a SIMULATION_START_DATE,global,o,c,"${ORIG_START_TIME_STRING}" ${wrfout}
  mv ${wrfout} ${FCSTPROD}/.
done

# Rename fcstdone files to format from original START_TIME
for fcstdone in `ls fcstdone*.${NEWHH}`
do
   NEWFF=`echo ${fcstdone} | cut -c 9-10`
   NEWMM=`echo ${fcstdone} | cut -c 11-12`
   typeset -Z2 FF=$(($NEWFF + $lasthour))
   mv ${fcstdone} ${FCSTPROD}/fcstdone${FF}${NEWMM}.${HH}
done

err_chk
cp rsl.out.0000 ${COMOUT}/hrrr.t${cyc}z.rslout
msg="JOB $job FOR HRRR_FORECAST_RESTART HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
