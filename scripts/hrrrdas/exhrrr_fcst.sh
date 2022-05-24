#!/bin/ksh --login
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
#                      .                                             .
# Script name:         exhrrr_fcst.sh.ecf
# Script description:  runs the free forecast job for the HRRR 
#
# Author:    Curtis Alexander / Geoff Manikin    Org: EMC     Date: 2014-05-02
#
# Script history log:
# 2014-05-02  C Alexander / G Manikin - HRRRv1
# 2016-02-05  C Alexander - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

ulimit -c 0

set -x

# Submit post manager jobs
#bsub < ${HOMEhrrr}/sms/post/jhrrr_postmgr_${cyc}.bsub 
#bsub < ${HOMEhrrr}/sms/post/jhrrr_postmgr_subh_${cyc}.bsub 

AWK="/bin/gawk --posix"
DATE=/bin/date
export FCST_INTERVAL=3
export FCST_LENGTH=1
export NODES=5
export OMP_NUM_THREADS=2

echo "run forecst for member =",$ensmem
ensmemid=`printf %4.4i $ensmem`

# Set up some constants
export WRF_NAMELIST=namelist.input

# Initialize an array of WRF input dat files that need to be linked
set -A WRF_DAT_FILES ${PARMhrrr}/hrrrdas_run_LANDUSE.TBL          \
                     ${PARMhrrr}/hrrrdas_run_RRTM_DATA            \
                     ${PARMhrrr}/hrrrdas_run_RRTM_DATA_DBL        \
                     ${PARMhrrr}/hrrrdas_run_RRTMG_LW_DATA        \
                     ${PARMhrrr}/hrrrdas_run_RRTMG_LW_DATA_DBL    \
                     ${PARMhrrr}/hrrrdas_run_RRTMG_SW_DATA        \
                     ${PARMhrrr}/hrrrdas_run_RRTMG_SW_DATA_DBL    \
                     ${PARMhrrr}/hrrrdas_run_VEGPARM.TBL          \
                     ${PARMhrrr}/hrrrdas_run_GENPARM.TBL          \
                     ${PARMhrrr}/hrrrdas_run_SOILPARM.TBL         \
                     ${PARMhrrr}/hrrrdas_run_MPTABLE.TBL          \
                     ${PARMhrrr}/hrrrdas_run_URBPARM.TBL          \
                     ${PARMhrrr}/hrrrdas_run_URBPARM_UZE.TBL      \
                     ${PARMhrrr}/hrrrdas_run_ETAMPNEW_DATA        \
                     ${PARMhrrr}/hrrrdas_run_ETAMPNEW_DATA.expanded_rain        \
                     ${PARMhrrr}/hrrrdas_run_ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${PARMhrrr}/hrrrdas_run_ETAMPNEW_DATA_DBL    \
                     ${PARMhrrr}/hrrrdas_run_co2_trans            \
                     ${PARMhrrr}/hrrrdas_run_ozone.formatted      \
                     ${PARMhrrr}/hrrrdas_run_ozone_lat.formatted  \
                     ${PARMhrrr}/hrrrdas_run_ozone_plev.formatted \
                     ${PARMhrrr}/hrrrdas_run_tr49t85              \
                     ${PARMhrrr}/hrrrdas_run_tr49t67              \
                     ${PARMhrrr}/hrrrdas_run_tr67t85              \
                     ${PARMhrrr}/hrrrdas_run_grib2map.tbl         \
                     ${PARMhrrr}/hrrrdas_run_gribmap.txt          \
                     ${PARMhrrr}/hrrrdas_run_aerosol.formatted      \
                     ${PARMhrrr}/hrrrdas_run_aerosol_lat.formatted  \
                     ${PARMhrrr}/hrrrdas_run_aerosol_lon.formatted  \
                     ${PARMhrrr}/hrrrdas_run_aerosol_plev.formatted \
                     ${PARMhrrr}/hrrrdas_run_bulkdens.asc_s_0_03_0_9  \
                     ${PARMhrrr}/hrrrdas_run_bulkradii.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrrdas_run_capacity.asc           \
                     ${PARMhrrr}/hrrrdas_run_CCN_ACTIVATE.BIN       \
                     ${PARMhrrr}/hrrrdas_run_coeff_p.asc            \
                     ${PARMhrrr}/hrrrdas_run_coeff_q.asc            \
                     ${PARMhrrr}/hrrrdas_run_constants.asc          \
                     ${PARMhrrr}/hrrrdas_run_kernels.asc_s_0_03_0_9 \
                     ${PARMhrrr}/hrrrdas_run_kernels_z.asc          \
                     ${PARMhrrr}/hrrrdas_run_masses.asc             \
                     ${PARMhrrr}/hrrrdas_run_termvels.asc           \
                     ${PARMhrrr}/hrrrdas_run_wind-turbine-1.tbl     \
                     ${PARMhrrr}/hrrrdas_run_freezeH2O.dat          \
                     ${PARMhrrr}/hrrrdas_run_qr_acr_qg.dat          \
                     ${PARMhrrr}/hrrrdas_run_qr_acr_qs.dat          \
                     ${PARMhrrr}/hrrrdas_run_eclipse_besselian_elements.dat

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
echo "MEMBER ID" = ensmemid
echo

# to deal with diag and EnKF failure only, directly link 
if [ ${HRRRDAS_CONTINUE_FCST} = YES ]; then
   timestr1=`${DATE} +%Y%m%d%H%M -d "${START_TIME} 1 hours ago"`
   bkfile=${HRRRDASGES}/hrrrdas_d01_${timestr1}f01_mem${ensmemid}
   inputfile=${COMIN}/hrrr.t${cyc}z.wrfinput_d01.mem${ensmemid}
   if [ -s ${inputfile} ]; then
     mv ${inputfile} ${inputfile}.bk
   fi
   ln -s ${bkfile} ${inputfile}

   bkfile=${HRRRDASGES}/hrrrdas_d02_${timestr1}f01_mem${ensmemid}
   inputfile=${COMIN}/hrrr.t${cyc}z.wrfinput_d02.mem${ensmemid}
   if [ -s ${inputfile} ]; then
     mv ${inputfile} ${inputfile}.bk
   fi
   ln -s ${bkfile} ${inputfile}

   echo "Warning: HRRRDAS_CONTINUE_FCST=YES, will move COMIN/hrrr.t${cyc}z.wrfinput_d01.mem${ensmemid} to ...bk" > mailmsg
   echo "         and link to  previous cycle 1-h forecast as the current initial condition, HRRRDASGES/hrrrdas_d01_${timestr1}f01_mem${ensmemid}" >> mailmsg
   echo "         same as d02 files" >> mailmsg
   cat mailmsg
   export subject="$PDY ${cyc}z mem${ensmemid} HRRRDAS FCST skip diag/Enkf data"
   export maillist=${maillist:-'nco.spa@noaa.gov,geoffrey.manikin@noaa.gov,benjamin.blake@noaa.gov,ming.hu@noaa.gov'}
   cat mailmsg |mail.py -s "$subject" $maillist -v
fi

# Set up the work directory and cd into it
cd ${DATA}/mem${ensmemid}
#rm -fr mem${ensmemid}
#mkdir mem${ensmemid}
#cd mem${ensmemid}

list09="0001 0003 0005 0007 0009 0011 0013 0015 0017 0019 0021 0023 0025 0027 0029 0031 0033 0035"
list21="0002 0004 0006 0008 0010 0012 0014 0016 0018 0020 0022 0024 0026 0028 0030 0032 0034 0036"
# Bring in initial condition file 
if [ ${cyc} -eq 09 -o ${cyc} -eq 21 ]; then
   coldstartmember=0
   if [ ${cyc} -eq 09 ]; then
     for thismem in $list09; do
        if [ ${ensmemid} -eq ${thismem} ]; then
           coldstartmember=1
        fi
     done
   fi
   if [ ${cyc} -eq 21 ]; then
     for thismem in $list21; do
        if [ ${ensmemid} -eq ${thismem} ]; then
           coldstartmember=1
        fi
     done
   fi

   echo "coldstartmember=",$coldstartmember

   if [ $coldstartmember -eq 0 ]; then
      filein=${COMIN}/hrrr.t${cyc}z.wrfinput_d01.mem${ensmemid}
      fileguessin=${COMIN}/hrrr.t${cyc}z.wrfguess_d01.mem${ensmemid}
      if [ -s ${filein} ]; then
         echo " Initial condition ${filein} "
         ln -sf ${filein} wrfinput_d01
         ln -sf ${filein} wrfvar_output 
      else
         echo "initial file does not exist= ${filein} try=${fileguessin}"
         if [ -s ${fileguessin} ];then
            echo " Initial condition ${fileguessin} "
            ln -sf ${fileguessin} wrfinput_d01
            ln -sf ${fileguessin} wrfvar_output 
         else
            errmsg="FATAL ERROR: No initial condition available for forecast - exit"
            echo $errmsg
            err_exit $errmsg
         fi
      fi

      filein=${COMIN}/hrrr.t${cyc}z.wrfinput_d02.mem${ensmemid}
      fileguessin=${COMIN}/hrrr.t${cyc}z.wrfguess_d02.mem${ensmemid}
      if [ -s ${filein} ]; then
         echo " Initial condition ${filein} "
         ln -sf ${filein} wrfinput_d02
      else
         echo "initial file does not exist= ${filein} try=${fileguessin}"
         if [ -s ${fileguessin} ]; then
            echo " Initial condition ${fileguessin} "
            ln -sf ${fileguessin} wrfinput_d02
         else
            errmsg="FATAL ERROR: No initial condition available for forecast - exit"
            echo $errmsg
            err_exit $errmsg
         fi
      fi
   else
      fileguessin=${COMIN}/hrrr.t${cyc}z.wrfinput_d01.mem${ensmemid}
      filein=${COMIN}/hrrr.t${cyc}z.wrfguess_d01.mem${ensmemid}
      if [ -s ${filein} ]; then
         echo " Initial condition ${filein} "
         ln -sf ${filein} wrfinput_d01
         ln -sf ${filein} wrfvar_output 
      else
         echo "initial file does not exist= ${filein} try=${fileguessin}"
         if [ -s ${fileguessin} ];then
            echo " Initial condition ${fileguessin} "
            ln -sf ${fileguessin} wrfinput_d01
            ln -sf ${fileguessin} wrfvar_output 
         else
            errmsg="FATAL ERROR: No initial condition available for forecast - exit"
            echo $errmsg
            err_exit $errmsg
         fi
      fi

      fileguessin=${COMIN}/hrrr.t${cyc}z.wrfinput_d02.mem${ensmemid}
      filein=${COMIN}/hrrr.t${cyc}z.wrfguess_d02.mem${ensmemid}
      if [ -s ${filein} ]; then
         echo " Initial condition ${filein} "
         ln -sf ${filein} wrfinput_d02
      else
         echo "initial file does not exist= ${filein} try=${fileguessin}"
         if [ -s ${fileguessin} ]; then
            echo " Initial condition ${fileguessin} "
            ln -sf ${fileguessin} wrfinput_d02
         else
            errmsg="FATAL ERROR: No initial condition available for forecast - exit"
            echo $errmsg
            err_exit $errmsg
         fi
      fi

   fi
else
   filein=${COMIN}/hrrr.t${cyc}z.wrfinput_d01.mem${ensmemid}
   if [ -s ${filein} ]; then
      echo " Initial condition ${filein} "
      ln -sf ${filein} wrfinput_d01
      ln -sf ${filein} wrfvar_output 
   else
      echo "initial file does not exist=${filein}"
      errmsg="FATAL ERROR: No initial condition available for forecast - exit"
      echo $errmsg
      err_exit $errmsg
   fi

   filein=${COMIN}/hrrr.t${cyc}z.wrfinput_d02.mem${ensmemid}
   if [ -s ${filein} ]; then
      echo " Initial condition ${filein} "
      ln -sf ${filein} wrfinput_d02
   else
      echo "initial file does not exist=${filein}"
      errmsg="FATAL ERROR: No initial condition available for forecast - exit"
      echo $errmsg
      err_exit $errmsg
   fi
fi

## boundary condition searching
shh=`${DATE} +"%H" -d "${START_TIME}"`
yyyymmdd=`${DATE} +"%Y%m%d" -d "${START_TIME}"`
typeset -Z2 bdhh
let bdhh=${shh}/6*6
bdytime=${yyyymmdd}${bdhh}00
bdyfile=${HRRRDASBC}/wrfbdy_d01.${bdytime}.mem${ensmemid}
readyfile=${HRRRDASBC}/wrfbdy.ready.${bdytime}
if [ -s ${bdyfile} ]; then
   echo "check member boundary =",$bdyfile
else
   bdyfile=${HRRRDASBC}/wrfbdy_d01.${bdytime}
fi
if [[ -r ${bdyfile} && -r ${readyfile} ]]; then
  cp ${bdyfile} wrfbdy_d01
else
# Look for boundary conditions which are 6-h older
  if [ $bdhh -eq 00 ]; then
    bdhhm1=18
    yyyymmdd=$PDYm1
  elif [ $bdhh -eq 06 ]; then
    bdhhm1=00
  elif [ $bdhh -eq 12 ]; then
    bdhhm1=06
  elif [ $bdhh -eq 18 ]; then
    bdhhm1=12
  fi
  bdytimem1=${yyyymmdd}${bdhhm1}00
  bdyfilem1=${HRRRDASBC}/wrfbdy_d01.${bdytimem1}.mem${ensmemid}
  if [ -s ${bdyfilem1} ]; then
     echo "check member boundary =",$bdyfilem1
  else
     bdyfilem1=${HRRRDASBC}/wrfbdy_d01.${bdytimem1}
  fi
  if [ -r ${bdyfilem1} ]; then
    cp ${bdyfilem1} wrfbdy_d01
  else
    echo "FATAL ERROR: can not find boundary conditions for ${shh} !!!"
    err_exit
  fi
fi

cp ${PARMhrrr}/hrrrdas_wrf.nl ${WRF_NAMELIST}

# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  tempfile=`basename ${file}`
  tempname=`echo ${tempfile} | sed s/hrrrdas_run_//`
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
cat ${WRF_NAMELIST} | sed "s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year},${start_year}/" \
   | sed "s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month},${start_month}/"                   \
   | sed "s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day},${start_day}/"                       \
   | sed "s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour},${start_hour}/"                     \
   | sed "s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute},${start_minute}/"                 \
   | sed "s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second},${start_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update end time in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year},${end_year}/" \
   | sed "s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month},${end_month}/"                   \
   | sed "s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day},${end_day}/"                       \
   | sed "s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour},${end_hour}/"                     \
   | sed "s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute},${end_minute}/"                 \
   | sed "s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second},${end_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update interval in namelist
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))
cat ${WRF_NAMELIST} | sed "s/\(${interval}_${second}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
   > ${WRF_NAMELIST}.new 
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# update bc
cp ${PARMhrrr}/hrrr_update_bc_parame.in parame.in
startmsg
cp ${EXEChrrr}/hrrr_update_bc .
runline="./hrrr_update_bc"
$runline >> $DATA/$pgmout 2>errfile
export err=$?; err_chk

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`

###############################################################
# Run WRF.

cp ${EXEChrrr}/hrrrdas_wrfarw_fcst .

cat<<EOF > run_wrf_config
omp_num_threads=2 # compute only; io is always 1
mkl_num_threads=1 # compute only; io is always 1
hyperthreads=2 # compute only
io_hyperthreads=2 # io only
turbo_mode=YES # YES or NO
reorder_ranks=grid_order # LSF, MPICH, or NO

# Distribution of compute nodes in grid:
#nnode_x=6
#nnode_y=2
nnode_x=5
nnode_y=2

# Distribution of MPI ranks within each compute node:
#nrank_x=2
#nrank_y=12
nrank_x=2
nrank_y=12

# Quilt server configuration:
nio_tasks_per_group=24
nio_groups=2
nio_ppn=48
cb_nodes=4
EOF

if ( ! $USHhrrr/hrrr_run_wrf.sh hrrrdas_wrfarw_fcst run_wrf_config ) ; then
    err_exit "WRF-ARW execution wrapper script failed."
fi

###############################################################
err_chk
cp rsl.out.0000 ${COMOUT}/hrrr.t${cyc}z.rslout_mem${ensmemid}

timestr1=`${DATE} +%Y%m%d%H%M -d "${START_TIME}"`
timestr2=`${DATE} +%Y-%m-%d_%H_%M_%S -d "${END_TIME}"`

# Remove northern row and eastern column from HRRRDAS wrfout file so
# that it matches the HRRR domain, and reduce number of fields to
# minimum needed by GSI.

if [ -s wrfout_small_d02_${timestr2} ]; then
  echo "removing existing wrfout_small_d02_${timestr2}"
  rm -f wrfout_small_d02_${timestr2}
fi

echo "running ncks"

ncks -d west_east,0,1798 -d west_east_stag,0,1799 -d south_north,0,1058 -d south_north_stag,0,1059 -v "T,QVAPOR,SMOIS,P_TOP,ZNU,ZNW,RDX,RDY,MAPFAC_M,XLAT,XLONG,MUB,MU,P,PH,PHB,U,V,XLAND,SEAICE,SST,IVGTYP,ISLTYP,VEGFRA,SNOW,U10,V10,TSLB,TSK,Q2,SOILT1,TH2,QCLOUD,QRAIN,QSNOW,QICE,QGRAUP,QNRAIN,QNICE,QNCLOUD,REFL_10CM,W,C3H,C4H,C3F,C4F,Times" wrfout_d02_${timestr2} ./wrfout_small_d02_${timestr2}

# Get MPISERIAL; section this is copied from exhrrr_prep_smoke:
# ----------------------------------------------------------------------
# need this because have trouble load module now:
#MPISER=/gpfs/hps/nco/ops/nwprod/mpiserial.v3.0.0/exec/mpiserial
export MPISER=${MPISER:-${NWROOThps}/mpiserial.${mpiserial_ver}/exec/mpiserial}
LSB_MAX_NUM_PROCESSORS="${LSB_MAX_NUM_PROCESSORS:-1}"
if (( LSB_MAX_NUM_PROCESSORS > 1 )) ; then
    # Command to run mpiserial:
    np=$(( LSB_MAX_NUM_PROCESSORS - 1 )) # -1 for MAMU node processor
    MPISERIAL="mpiexec -n $np -ppn 24 $MPISER -m"
else
    fake_mpiserial() {
        chmod 755 "$1"
        set -xue
        source "$1"
    }
    MPISERIAL=fake_mpiserial
fi
# ----------------------------------------------------------------------

CPREQ=$( which cpreq )

cat> command_file<<EOF
#!/bin/bash
$CPREQ wrfout_d01_${timestr2} ${HRRRDASGES}/hrrrdas_d01_${timestr1}f01_mem${ensmemid}
$CPREQ wrfout_d02_${timestr2} ${HRRRDASGES}/hrrrdas_d02_${timestr1}f01_mem${ensmemid}
$CPREQ wrfout_small_d02_${timestr2} ${HRRRDASGES}/hrrrdas_small_d02_${timestr1}f01_mem${ensmemid}
EOF

#$MPISERIAL -a ./command_file
chmod 755 command_file
mpiexec -n 1 -ppn 1 command_file


msg="JOB $job FOR HRRR_FORECAST HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
