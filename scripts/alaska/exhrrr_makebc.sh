#!/bin/ksh --login
############################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exhrrr_makebc.sh.ecf
# Script description:  runs the job to generate the boundary conditions for
#                         the HRRR by processing the previous hour's RAP
#
# Author:  Curtis Alexander / Geoff Manikin   Org: EMC     Date: 2014-04-10
#
# Script history log:
# 2014-04-10  C Alexander / G Manikin - HRRRv1
# 2016-02-05  C Alexander / G Manikin - HRRRv2
# 2018-01-24  B Blake / G Manikin / C Alexander - HRRRv3

ulimit -s 512000
set -x

# Set up paths to shell commands
AWK="/bin/gawk --posix"
DATE=/bin/date

ulimit -s 512000

cd $DATA

# Set up some constants
export WPSNAMELIST=namelist.wps
export FCST_INTERVAL=3
if [ $cyc -eq 00 -o $cyc -eq 06 -o $cyc -eq 12 -o $cyc -eq 18 ]; then
  export FCST_LENGTH=48
  export FCST_LENGTH_RAP=51
else
  export FCST_LENGTH=18
  export FCST_LENGTH_RAP=21
fi
export LENGTH_DIFF=`expr $FCST_LENGTH_RAP - $FCST_LENGTH`
export LENDIVINT=`expr $FCST_LENGTH / $FCST_INTERVAL` 
export FILES_NEEDED=`expr $LENDIVINT + 1`
export DELAY_EXIT=`expr $LENGTH_DIFF + 1`
export START_TIME=${PDY}' '${cyc}
export FORMAT=rap.t%Hz.awp242bgrbf%f.grib2
export FORMAT2=rap.t[0-9][0-9]z.awp242bgrbf[0-9][0-9].grib2
export SOURCE=RAP
export WRF_CORE=ARW
export DATA_OUT=$DATA/files
export DATA_MET=$DATA/metfiles

if [ $cyc -eq 01 -o $cyc -eq 13 ]; then
  DELAY=2
else
  DELAY=1
fi

echo $FCST_LENGTH
echo $LENDIVINT
echo $LENGTH_DIFF
echo $FILES_NEEDED
echo $LENGTH_DIFF
echo $DELAY_EXIT

if [ $cyc -eq 01 -o $cyc -eq 00 ]; then
  export SOURCE_PATH=${SOURCE_PATH:-${COMRAP}/rap.${PDYm1}}
else
  export SOURCE_PATH=${SOURCE_PATH:-${COMRAP}/rap.${PDY}}
fi

# Make working directory
if [ ! -d "${DATA_OUT}" ]; then
  mkdir -p ${DATA_OUT}
fi

if [ ! -d "${DATA_MET}" ]; then
  mkdir -p ${DATA_MET}
fi

# Calculate start and end time date strings
START_TIME=`${DATE} -d "${START_TIME}"`
END_TIME=`${DATE} -d "${START_TIME}  ${FCST_LENGTH} hours"`
# Print run parameters
echo "ungrib started at `${DATE}`"
echo
echo "SOURCE         = ${SOURCE}"
echo "SOURCE_PATH    = ${SOURCE_PATH}"
echo "START_TIME     = ${START_TIME}"
echo "FCST_LENGTH    = ${FCST_LENGTH}"
echo "FCST_INTERVAL  = ${FCST_INTERVAL}"
echo

start_yyyymmdd_hhmmss=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}"`
end_yyyymmdd_hhmmss=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${END_TIME}"`

echo $start_yyyymmdd_hhmmss
echo $end_yyyymmdd_hhmmss

# Get the forecast interval in seconds
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))

# Link the Vtable into the work directory
rm -f Vtable
ln -sf ${PARMhrrr}/hrrrak_vtable Vtable

# Copy the namelist into the work directory
cp ${PARMhrrr}/hrrrak_namelist.wps ${WPSNAMELIST} 
WPSNAMELIST=`basename ${WPSNAMELIST}`

# Create a poor man's namelist hash
#
# Strip off comments, section names, slashes, and remove white space.
# Then loop over each line to create an array of names, an array of
# values, and variables that contain the index of the names in the
# array of names.  Each variable that contains an index of a namelist
# name is named $_NAME_ where 'NAME' is one of the names in the namelist
# The $_NAME_ vars are always capitalized even if the names in the namelist
# are not.
i=-1
for name in `sed 's/[[:blank:]]//g' ${WPSNAMELIST} | ${AWK} '/^[^#&\/]/' | sed 's/[[:cntrl:]]//g'`
do
  # If there's an = in the line
  if [ `echo ${name} | ${AWK} /=/` ]; then
    (( i=i+1 ))
    left=`echo ${name} | cut -d"=" -f 1 | ${AWK} '{print toupper($0)}'`
    right=`echo ${name} | cut -d"=" -f 2`
#    var[${i}]=${left}
    val[${i}]=${right}
    (( _${left}_=${i} ))
  else
    val[${i}]=${val[${i}]}${name}
  fi
done

# Create patterns for updating the namelist
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
date=[Dd][Aa][Tt][Ee]
interval=[Ii][Nn][Tt][Ee][Rr][Vv][Aa][Ll]
seconds=[Ss][Ee][Cc][Oo][Nn][Dd][Ss]
prefix=[Pp][Rr][Ee][Ff][Ii][Xx]
yyyymmdd_hhmmss='[[:digit:]]\{4\}-[[:digit:]]\{2\}-[[:digit:]]\{2\}_[[:digit:]]\{2\}:[[:digit:]]\{2\}:[[:digit:]]\{2\}'

# Update the start and end date in namelist
cat ${WPSNAMELIST} | sed "s/\(${start}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${start_yyyymmdd_hhmmss}'/" \
                      | sed "s/\(${end}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${end_yyyymmdd_hhmmss}'/"     \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update interval in namelist
cat ${WPSNAMELIST} | sed "s/\(${interval}_${seconds}\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
                      > ${WPSNAMELIST}.new 
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update the prefix in the namelist
cat ${WPSNAMELIST} | sed "s/\(${prefix}\)${equal}'[[:alnum:]]\{1,\}'/\1 = '${SOURCE}'/" \
                      > ${WPSNAMELIST}.new 
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Get a list of files in the SRCPATH directory
grib_files=`ls -1 ${SOURCE_PATH} | grep ${FORMAT2}`
ngribfiles=0

# Loop over progressively older RAP cycles for matching output times until found or RAP forecast length is exceeded
while [[ ${ngribfiles} -ne $FILES_NEEDED && ${DELAY} -le $LENGTH_DIFF ]]
do

  ngribfiles=0   
  # Get start time components to use for matching with grib files
  start_year=`${DATE} +%Y -d "${START_TIME} ${DELAY} hours ago"`
  start_yr=`${DATE} +%y -d "${START_TIME} ${DELAY} hours ago"`
  start_month=`${DATE} +%m -d "${START_TIME} ${DELAY} hours ago"`
  start_day=`${DATE} +%d -d "${START_TIME} ${DELAY} hours ago"`
  start_jday=`${DATE} +%j -d "${START_TIME} ${DELAY} hours ago"`
  start_hour=`${DATE} +%H -d "${START_TIME} ${DELAY} hours ago"`

  # Select files to create links to based on a file name format.
  # The format is a string optionally containing the following:
  #
  #  %y - Two digit year (e.g. 03)
  #  %Y - Four digit year (e.g. 2003)
  #  %m - Two digit month (i.e. 01, 02,...,12)
  #  %d - Two digit day of month (i.e. 01,02,...31)
  #  %j - Three digit julian day (i.e. 01,02,...366)
  #  %H - Two digit hour (i.e. 00,01,...23)
  #  %f - Two digit forecast hour (e.g. 00, 01, 12, 24, 48, etc.)
  #  %F - Four digit forecast hour (e.g. 0000, 0001, 0012, 0048, 0096, 0144, etc.)
  #
    if [ ${FORMAT} ]; then
      echo "Using format: '${FORMAT}'"
      set -A flags H j d m y Y F f
      for file in ${grib_files}; do

      # Check to see if the file conforms to the specified format
      filter=`echo ${FORMAT} | sed 's/%Y/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]/' \
                             | sed 's/%y/[[:digit:]][[:digit:]]/'                       \
                             | sed 's/%j/[[:digit:]][[:digit:]][[:digit:]]/'            \
                             | sed 's/%m/[[:digit:]][[:digit:]]/'                       \
                             | sed 's/%d/[[:digit:]][[:digit:]]/'                       \
                             | sed 's/%H/[[:digit:]][[:digit:]]/'                       \
                             | sed 's/%F/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]/' \
                             | sed 's/%f/[[:digit:]][[:digit:]]/'`

      if [ ! "`echo ${file} | ${AWK} "/^${filter}$/"`" ]; then
        continue
      fi

      # Clear any previous values for the flags
      for flag in ${flags[*]}; do
        eval unset _${flag}_
      done

      # The file conforms to the format, extract the values for each flag
      for flag in ${flags[*]}; do

        # If the flag is used, get its value
        if [ "`echo ${FORMAT} | ${AWK} "/%${flag}/"`" ]; then
          flagstr="\\\(%${flag}\\\)"
          format=`echo ${FORMAT} | sed "s/%${flag}/${flagstr}/"`
          filter=`echo ${format} | sed 's/%Y/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]/' \
                                 | sed 's/%y/[[:digit:]][[:digit:]]/'                       \
                                 | sed 's/%j/[[:digit:]][[:digit:]][[:digit:]]/'            \
                                 | sed 's/%m/[[:digit:]][[:digit:]]/'                       \
                                 | sed 's/%d/[[:digit:]][[:digit:]]/'                       \
                                 | sed 's/%H/[[:digit:]][[:digit:]]/'                       \
                                 | sed 's/%F/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]/' \
                                 | sed 's/%f/[[:digit:]][[:digit:]]/'`
          val=`echo ${file}| sed "s/${filter}/\1/"`
          eval _${flag}_="${val}"

          # Improve performance by early rejection of the files that do not match the times we want
          if [ -n "${_H_}" -a "${_H_}" -ne "${start_hour}" ]; then
            break
          elif [ -n "${_j_}" -a "${_j_}" -ne "${start_jday}" ]; then
            break
          elif [ -n "${_d_}" -a "${_d_}" -ne "${start_day}" ]; then
            break
          fi
        fi

      done

      # Check the value of each flag against the start time, fcst_length, and fcst_interval
      if [ -z "${_H_}" -o "${_H_}" -eq "${start_hour}" ]; then
        if [ -z "${_j_}" -o "${_j_}" -eq "${start_jday}" ]; then
          if [ -z "${_d_}" -o "${_d_}" -eq "${start_day}" ]; then
            if [ -z "${_m_}" -o "${_m_}" -eq "${start_month}" ]; then
              if [ -z "${_y_}" -o "${_y_}" -eq "${start_yr}" ]; then
                if [ -z "${_Y_}" -o "${_Y_}" -eq "${start_year}" ]; then
                  if [ -z "${_f_}" -a -z "${_F_}" ]; then
                    gribfiles[${ngribfiles}]=${file}
                    (( ngribfiles=ngribfiles + 1 ))
                  else
                    if [ -n "${_F_}" ]; then
                      fhour=${_F_}
                    elif [ -n "${_f_}" ]; then
                      fhour=${_f_}
                    fi
                    if (( (fhour >= DELAY) && (fhour <= FCST_LENGTH+DELAY) && ((fhour-DELAY) % FCST_INTERVAL==0) )) then
                      gribfiles[${ngribfiles}]=${file}
                      (( ngribfiles=ngribfiles + 1 ))
                    fi
                  fi
                fi
              fi
            fi
          fi
        fi
      fi

    done

  fi

  (( DELAY=DELAY + 1 ))
done

# Check to see if sufficent grib files found or exit
if [[ ${ngribfiles} -ne $FILES_NEEDED && ${DELAY} -eq $DELAY_EXIT ]]; then
  echo "FATAL ERROR: Insufficient grib2 files for linking.  Need one of following full set of RAP files: \
       rap.t${cycm1}z.awp242bgrbfHH.grib2(.idx) where HH=01,04,07,10,13,16,19 \
       rap.t${cycm2}z.awp242bgrbfHH.grib2(.idx) where HH=02,05,08,11,14,17,20 \
       rap.t${cycm3}z.awp242bgrbfHH.grib2(.idx) where HH=03,06,09,12,15,18,21"
  err_exit
fi

# If a filter is provided, try matching grib files using the filter
if [ ${ngribfiles} -eq 0 ]; then
  if [ ${FILTER} ]; then
    echo "TRYING FILTER = ${FILTER}"
    for file in ${grib_files}; do
      if [ `echo ${file} | ${AWK} "/${FILTER}/"` ]; then
        gribfiles[${ngribfiles}]=${file}
        (( ngribfiles=ngribfiles + 1 ))
      fi
    done
  fi
fi

# Try linking to all (non dot) files in the srcpath
if [ ${ngribfiles} -eq 0 ]; then
  filter="^[^.].*"
  echo "Filters found no grib files; linking to all files"
  for file in ${grib_files}; do
    if [ "`echo ${file} | ${AWK} "/${filter}/"`" ]; then
      gribfiles[${ngribfiles}]=${file}
      (( ngribfiles=ngribfiles + 1 ))      
    fi
  done
fi

# Check to make sure we linked to some grib files
if [ ${ngribfiles} -eq 0 ]; then
  echo "${SOURCE_PATH} appears to be empty"
  echo "FATAL ERROR: No grib files could be linked to"
  err_exit 
fi

# Create a set of id's for use in naming the links
set -A alphabet A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
i=0
j=0
k=0
n=0
while [ ${i} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
  while [ ${j} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
    while [ ${k} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
      id="${alphabet[${i}]}${alphabet[${j}]}${alphabet[${k}]}"
      cp ${SOURCE_PATH}/${gribfiles[${n}]} GRIBFILE_COMPLEX.${id} 
      $WGRIB2 GRIBFILE_COMPLEX.${id} -set_grib_type s -grib_out GRIBFILE_SIMPLE.${id} 
      ln -sf GRIBFILE_SIMPLE.${id} GRIBFILE.${id}
      (( k=k+1 ))
      (( n=n+1 ))
    done
    k=0
    (( j=j+1 ))
  done
  j=0
  (( i=i+1 ))
done

# Run ungrib
startmsg
${EXEChrrr}/hrrr_wps_ungrib >> $DATA/$pgmout 2>errfile
export err=$?; err_chk

# Check to see if we've got all the files we're expecting
fcst=0
while [ ${fcst} -le ${FCST_LENGTH} ]; do
  filename=${SOURCE}:`${DATE} +%Y-%m-%d_%H -d "${START_TIME}  ${fcst} hours"`
  if [ ! -s ${filename} ]; then
    echo "ERROR: ${filename} is missing"
    export err=1; err_chk
  fi
  (( fcst=fcst+FCST_INTERVAL ))
done

# Move the output files to the extprd directory
mkdir -p ${DATA_OUT}
for file in `ls -1 ${DATA} | grep ^${SOURCE}:`; do
  mv ${file} ${DATA_OUT}
done

echo "ungrib completed at `${DATE}`"

ls -lrt ${DATA_OUT}

# begin metgrid section
#export LSB_PJL_TASK_GEOMETRY="`/usrx/local/bin/mktjv 1/1 144/24`" 
export MP_EUIDEVICE=sn_all
export MP_EUILIB=us
export SOURCE_PATH=$DATA/files

# Print run parameters
echo "metgrid started at `${DATE}`"

# Print the forecast length, interval, start time, and end time
echo "START TIME    = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${START_TIME}"`
echo "END TIME      = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${END_TIME}"`
echo "FCST LENGTH   = ${FCST_LENGTH}"
echo "FCST INTERVAL = ${FCST_INTERVAL}"

# Link to geogrid static files
rm -f geo_*.d01.nc
ln -sf ${FIXhrrr}/hrrrak_geo_em.d01.nc geo_em.d01.nc 
ln -sf ${PARMhrrr}/hrrrak_METGRID.TBL METGRID.TBL

# Create patterns for updating the namelist
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
date=[Dd][Aa][Tt][Ee]
interval=[Ii][Nn][Tt][Ee][Rr][Vv][Aa][Ll]
seconds=[Ss][Ee][Cc][Oo][Nn][Dd][Ss]
prefix=[Pp][Rr][Ee][Ff][Ii][Xx]
fg_name=[Ff][Gg][_][Nn][Aa][Mm][Ee]
constants_name=[Cc][Oo][Nn][Ss][Tt][Aa][Nn][Tt][Ss][_][Nn][Aa][Mm][Ee]
yyyymmdd_hhmmss='[[:digit:]]\{4\}-[[:digit:]]\{2\}-[[:digit:]]\{2\}_[[:digit:]]\{2\}:[[:digit:]]\{2\}:[[:digit:]]\{2\}'

# Update the start and end date in namelist
cat ${WPSNAMELIST} | sed "s/\(${start}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${start_yyyymmdd_hhmmss}'/" \
                      | sed "s/\(${end}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${end_yyyymmdd_hhmmss}'/"     \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update interval in namelist
cat ${WPSNAMELIST} | sed "s/\(${interval}_${seconds}\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
                      > ${WPSNAMELIST}.new 
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update fg_name if SOURCE is defined
if [ "${SOURCE}" ]; then

  # Format the SOURCE string so it looks like: 'xxx','yyy',...,'zzz',
  source_str=`echo ${SOURCE} | sed "s/\([^',]*\),*/'\1',/g"`

  # Update fg_name
  cat ${WPSNAMELIST} | sed "s/\(${fg_name}\)${equal}.*/\1 = ${source_str}/" \
                        > ${WPSNAMELIST}.new
  mv ${WPSNAMELIST}.new ${WPSNAMELIST}

fi

# Update constants_name if CONSTANTS is defined
if [ "${CONSTANTS}" ]; then

  # Format the CONSTANTS string so it looks like: 'xxx','yyy',...,'zzz',
  constants_str=`echo ${CONSTANTS} | sed "s/\([^',]*\),*/'\1',/g"`

  # Update constants_name
  cat ${WPSNAMELIST} | sed "s/\(${constants_name}\)${equal}.*/\1 = ${constants_str}/" \
                        > ${WPSNAMELIST}.new
  mv ${WPSNAMELIST}.new ${WPSNAMELIST}

fi

# Create a poor man's namelist hash
#
# Strip off comments, section names, slashes, and remove white space.
# Then loop over each line to create an array of names, an array of
# values, and variables that contain the index of the names in the
# array of names.  Each variable that contains an index of a namelist
# name is named $_NAME_ where 'NAME' is one of the names in the namelist
# The $_NAME_ vars are always capitalized even if the names in the namelist
# are not.
i=-1
for name in `sed 's/[[:blank:]]//g' ${WPSNAMELIST} | ${AWK} '/^[^#&\/]/'`
do
  # If there's an = in the line
  if [ `echo ${name} | ${AWK} /=/` ]; then
    (( i=i+1 ))
    left=`echo ${name} | cut -d"=" -f 1 | ${AWK} '{print toupper($0)}'`
    right=`echo ${name} | cut -d"=" -f 2`
    val[${i}]=${right}
    (( _${left}_=${i} ))
  else
    val[${i}]=${val[${i}]}${name}
  fi
done

# Get an array of fg_names from the namelist
set -A source_list `echo ${val[${_FG_NAME_}]} | sed "s/[',]\{1,\}/ /g"`

# Get an array of constants_names from the namelist
set -A constant_list `echo ${val[${_CONSTANTS_NAME_}]} | sed "s/[',]\{1,\}/ /g"`

# Make sure SOURCE_PATH is defined if source_list is not empty
if [ ${#source_list[*]} -gt 0 ]; then
  if [ ! "${SOURCE_PATH}" ]; then
    echo "FATAL ERROR: fg_name is not empty, but \$SOURCE_PATH is not defined!"
    err_exit 
  fi

  # Create an array of SOURCE PATHS
  set -A source_path_list `echo ${SOURCE_PATH} | sed "s/[',]\{1,\}/ /g"`

  # Make sure source_list and source_path_list are the same length
  if [ ${#source_list[*]} -ne ${#source_path_list[*]} ]; then
    echo "FATAL ERROR: The number of paths in \$SOURCE_PATH does not match the number of sources in fg_name"
    err_exit 
  fi  

fi

# Create links to all the fg_name sources
i=0
for src in ${source_list[*]}; do
  fcst=0
  while [ ${fcst} -le ${FCST_LENGTH} ]; do
    datestr=`${DATE} +"%Y-%m-%d_%H" -d "${START_TIME}  ${fcst} hours"`
    rm -f ${src}:${datestr}
    if [ -e ${source_path_list[${i}]}/${src}:${datestr} ]; then
      ln -sf ${source_path_list[${i}]}/${src}:${datestr}
    fi
    (( fcst=fcst+${FCST_INTERVAL} ))
  done
  (( i=i+1 ))
done

# Get the WRF core
core=`echo ${val[${_WRF_CORE_}]} | sed "s/[',]\{1,\}//g"`

# Get the metgrid output format
output_format=`echo ${val[${_IO_FORM_METGRID_}]} | sed "s/[',]\{1,\}//g"`

# Set core specific variables
if [ "${core}" == "ARW" ]; then
  metgrid_prefix="met_em"
elif [ "${core}" == "NMM" ]; then
  metgrid_prefix="met_nmm"
else
  echo "FATAL ERROR: WRF Core, ${core}, is not supported!"
  err_exit 
fi

# Set the output file suffix
if [ ${output_format} -eq 2 ]; then
  metgrid_suffix=".nc"
else
  metgrid_suffix=""
fi

# Remove pre-existing metgrid files
fcst=0
while [ ${fcst} -le ${FCST_LENGTH} ]; do
  time_str=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${fcst} hours"`
  rm -f ${metgrid_prefix}.d01.${time_str}${metgrid_suffix}
  (( fcst=${fcst} + ${FCST_INTERVAL} ))
done

# Run metgrid
startmsg
cp ${EXEChrrr}/hrrr_wps_metgrid hrrr_metgrid
#runline="aprun -n 168 -N 24 ./hrrr_metgrid"
runline="mpiexec -n 128 -ppn 128 ./hrrr_metgrid"
$runline
export err=$?; err_chk

# Check to see if the output is there:
fcst=0
while [ ${fcst} -le ${FCST_LENGTH} ]; do
    time_str=`${DATE} +%Y-%m-%d_%H:%M:%S -d "${START_TIME}  ${fcst} hours"`
    if [ ! -e "${metgrid_prefix}.d01.${time_str}${metgrid_suffix}" ]; then
      echo "FATAL ERROR: hrrr_metgrid failed to complete"
      err_exit 
    fi
    (( fcst=${fcst} + ${FCST_INTERVAL} ))
done
echo "hrrr_metgrid completed successfully at `${DATE}`"
   
 mv met_em* ${DATA_MET}/.
 echo "about to start real section"

# Set MPI options
#export MP_SINGLE_THREAD=yes
#export MP_EAGER_LIMIT=65536
export OMP_NUM_THREADS=1
#export MP_MPILIB=pempi
#export MP_EUILIB=us
#export MP_LABELIO=yes
#export MP_SHARED_MEMORY=yes
#export MP_USE_BULK_XFER=yes
#export I_MPI_FABRICS=shm:ofa
real_prefix="met_em"

# Set the pathname of the WRF namelist
WRF_NAMELIST=${DATA}/namelist.input

# Set the input format
if [ ! "${INPUT_FORMAT}" ]; then
  INPUT_FORMAT=NETCDF
fi
if [ "${INPUT_FORMAT}" == "NETCDF" ]; then
  real_suffix=".nc"
elif [ "${INPUT_FORMAT}" == "BINARY" ]; then :
  real_suffix=""
else
  echo "FATAL ERROR: Unsupported INPUT_FORMAT, '${INPUT_FORMAT}'"
  err_exit 
fi

# Initialize an array of WRF DAT files that need to be linked
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
                     ${PARMhrrr}/hrrrak_run_freezeH2O.dat              \
                     ${PARMhrrr}/hrrrak_run_qr_acr_qg.dat              \
                     ${PARMhrrr}/hrrrak_run_qr_acr_qs.dat

# Check to make sure WRF DAT files exist
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    echo "FATAL ERROR: ${file} either does not exist or is empty"
    err_exit 
  fi
done

# Print run parameters
echo "real started at `${DATE}`"
echo "FCST_LENGTH    = ${FCST_LENGTH}"
echo "FCST_INTERVAL  = ${FCST_INTERVAL}"
echo "START_TIME     = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${START_TIME}"`
echo "END_TIME       = "`${DATE} +"%Y/%m/%d %H:%M:%S" -d "${END_TIME}"`

# Check to make sure the real input files (e.g. met_em.d01.*) are available
# and make links to them
fcst=0
while [ ${fcst} -le ${FCST_LENGTH} ]; do
  time_str=`${DATE} "+%Y-%m-%d_%H:%M:%S" -d "${START_TIME}  ${fcst} hours"`
  if [ ! -r "${DATA_MET}/${real_prefix}.d01.${time_str}${real_suffix}" ]; then
    echo "FATALERROR: Input file '${DATA_MET}/${real_prefix}.d01.${time_str}${real_suffix}' is missing"
    err_exit 
  fi
  rm -f ${real_prefix}.d01.${time_str}${real_suffix}
  ln -sf ${DATA_MET}/${real_prefix}.d01.${time_str}${real_suffix}
  (( fcst = fcst + ${FCST_INTERVAL} ))
done

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

# Copy the wrf namelist to the workdir as namelist.input
cp ${PARMhrrr}/hrrrak_real.nl ${WRF_NAMELIST}

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
cat ${WRF_NAMELIST} | sed "s/\(${interval}${second}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
   > ${WRF_NAMELIST}.new 
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Move existing rsl files to a subdir if there are any
echo "Checking for pre-existing rsl files"
if [ -f "rsl.out.0000" ]; then
  rsldir=rsl.`ls -l --time-style=+%Y%m%d%H%M%S rsl.out.0000 | cut -d" " -f 7`
  mkdir ${rsldir}
  echo "Moving pre-existing rsl files to ${rsldir}"
  mv rsl.out.* ${rsldir}
  mv rsl.error.* ${rsldir}
else
  echo "No pre-existing rsl files were found"
fi

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`

# Run real
startmsg
cp ${EXEChrrr}/hrrr_wrfarw_real .
#runline="aprun -n 168 -N 24 ./hrrr_wrfarw_real"
runline="mpiexec -n 128 -ppn 128 ./hrrr_wrfarw_real"
$runline
export err=$?; err_chk

cp wrfbdy_d01 $COMOUT/hrrrak.t${cyc}z.wrfbdy
cp rsl.out.0000 ${COMOUT}/hrrrak.t${cyc}z.rslbcout
echo "real completed successfully at `${DATE}`"

exit 0
