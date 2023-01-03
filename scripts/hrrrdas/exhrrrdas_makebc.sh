#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:        exhrrrdas_makebc.sh.sms 
# Script description:  Runs ungrib and metgrid
#
# Authors:        Ming Hu/Geoff Manikin         Date: 2011-08-24
#
# Abstract: run ungrid and metgrid
#
# Script history log:
# 2011-08-24  G Manikin - new script adapted from M. Hu's scripts
# 2018-01-25  B Blake / C Guastini - RAPv4

set -x

cd $DATA

###############################
# Run ungrib on GFS input files
###############################
export workdir=$DATA
START_TIME=$PDY$cyc
export FCST_LENGTH=${FCST_LENGTH:-24}
export FCST_INTERVAL=${FCST_INTERVAL:-3}
export SOURCE=${SOURCE:-gfs}
export SOURCE2=${SOURCE2:-sst}
export SOURCE_PATH=$COMINgfs
export DATE=${DATE:-/bin/date}

echo ${START_TIME} > STARTTIME

# Calculate start and end time date strings
echo ${START_TIME}
END_TIME=`$NDATE +${FCST_LENGTH} $START_TIME`
echo ${END_TIME}
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`
eyyyy=`echo ${END_TIME} | cut -c1-4`
emm=`echo ${END_TIME} | cut -c5-6`
edd=`echo ${END_TIME} | cut -c7-8`
ehh=`echo ${END_TIME} | cut -c9-10`
start_yyyymmdd_hhmmss=${syyyy}-${smm}-${sdd}_${shh}:00:00
end_yyyymmdd_hhmmss=${eyyyy}-${emm}-${edd}_${ehh}:00:00
start_YYYYMMDD=${syyyy}${smm}${sdd}

echo $FCST_LENGTH
echo $start_yyyymmdd_hhmmss
echo $end_yyyymmdd_hhmmss

# Get the forecast interval in seconds
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))

# Link the Vtable into the work directory
ln -sf $PARMhrrrdas/hrrrdas_Vtable.${SOURCE} Vtable

# Copy the namelist into the work directory
cp $PARMhrrrdas/hrrrdas_namelist.wps namelist.wps
WPSNAMELIST=namelist.wps

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


# Get start time components to use for matching with grib files
start_year=`echo ${START_TIME} | cut -c1-4`
start_yr=`echo ${START_TIME} | cut -c3-4`
start_month=`echo ${START_TIME} | cut -c5-6`
start_day=`echo ${START_TIME} | cut -c7-8`
start_hour=`echo ${START_TIME} | cut -c9-10`
start_ymd=${start_year}${start_month}${start_day}
YYYYMMDDHH=${start_year}-${start_month}-${start_day}_00

start_gfs_fcst_time=00
end_gfs_fcst_time=${FCST_LENGTH}

cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f000 gfs.t${start_hour}z.pgrb2a.0p25.f000
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f003 gfs.t${start_hour}z.pgrb2a.0p25.f003
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f006 gfs.t${start_hour}z.pgrb2a.0p25.f006
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f009 gfs.t${start_hour}z.pgrb2a.0p25.f009
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f012 gfs.t${start_hour}z.pgrb2a.0p25.f012
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f015 gfs.t${start_hour}z.pgrb2a.0p25.f015
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f018 gfs.t${start_hour}z.pgrb2a.0p25.f018
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f021 gfs.t${start_hour}z.pgrb2a.0p25.f021
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2.0p25.f024 gfs.t${start_hour}z.pgrb2a.0p25.f024

cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f000 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f003 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f006 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f009 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f012 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f015 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f018 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f021 .
cpreq ${COMINgfs}/gfs.${start_ymd}/${start_hour}/atmos/gfs.t${start_hour}z.pgrb2b.0p25.f024 .

cat gfs.t${start_hour}z.pgrb2a.0p25.f000 gfs.t${start_hour}z.pgrb2b.0p25.f000 > gfs.t${start_hour}z.pgrb2.0p25.f000
cat gfs.t${start_hour}z.pgrb2a.0p25.f003 gfs.t${start_hour}z.pgrb2b.0p25.f003 > gfs.t${start_hour}z.pgrb2.0p25.f003
cat gfs.t${start_hour}z.pgrb2a.0p25.f006 gfs.t${start_hour}z.pgrb2b.0p25.f006 > gfs.t${start_hour}z.pgrb2.0p25.f006
cat gfs.t${start_hour}z.pgrb2a.0p25.f009 gfs.t${start_hour}z.pgrb2b.0p25.f009 > gfs.t${start_hour}z.pgrb2.0p25.f009
cat gfs.t${start_hour}z.pgrb2a.0p25.f012 gfs.t${start_hour}z.pgrb2b.0p25.f012 > gfs.t${start_hour}z.pgrb2.0p25.f012
cat gfs.t${start_hour}z.pgrb2a.0p25.f015 gfs.t${start_hour}z.pgrb2b.0p25.f015 > gfs.t${start_hour}z.pgrb2.0p25.f015
cat gfs.t${start_hour}z.pgrb2a.0p25.f018 gfs.t${start_hour}z.pgrb2b.0p25.f018 > gfs.t${start_hour}z.pgrb2.0p25.f018
cat gfs.t${start_hour}z.pgrb2a.0p25.f021 gfs.t${start_hour}z.pgrb2b.0p25.f021 > gfs.t${start_hour}z.pgrb2.0p25.f021
cat gfs.t${start_hour}z.pgrb2a.0p25.f024 gfs.t${start_hour}z.pgrb2b.0p25.f024 > gfs.t${start_hour}z.pgrb2.0p25.f024


grib_files_temp=`ls -1 gfs.t${start_hour}z.pgrb2.0p25.f0[0-5]? | sort`
grib_files=`ls -1 gfs.t${start_hour}z.pgrb2.0p25.f0[0-5]? | sort`
ngribfiles=0

for file in ${grib_files_temp}; do
    file_time=`echo ${file} | cut -c23-24`
    if [[ "${file_time}" -ge "${start_gfs_fcst_time}" ]]; then
    if [[ "${file_time}" -le "${end_gfs_fcst_time}" ]]; then
        gribfiles[${ngribfiles}]=${file}
        (( ngribfiles=ngribfiles + 1 ))
    fi
    fi
done

echo  ${gribfiles[*]}

# Create a set of id's for use in naming the links
set -A alphabet A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
echo  ${alphabet[*]}
i=0
j=0
k=0
n=0
while [ ${i} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
  while [ ${j} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
    while [ ${k} -lt ${#alphabet[*]} -a ${n} -lt ${#gribfiles[*]} ]; do
      id="${alphabet[${i}]}${alphabet[${j}]}${alphabet[${k}]}"
      echo ${gribfiles[${n}]}, GRIBFILE.${id}
      ln -sf ${gribfiles[${n}]} GRIBFILE.${id}
      (( k=k+1 ))
      (( n=n+1 ))
    done
    k=0
    (( j=j+1 ))
  done
  j=0
  (( i=i+1 ))
done

# Run the GFS ungrib
export pgm=hrrr_wps_ungrib
. prep_step

#startmsg
$EXEChrrrdas/hrrr_wps_ungrib > ungribgfs.out
export err=$?;err_chk

echo "GFS ungrib completed at `${DATE}`"

###################################
# Now do the SST ungrid
###################################

# Link the Vtable into the work directory
rm -f Vtable
rm -f GRIBFILE*
ln -sf $PARMhrrrdas/hrrrdas_Vtable.${SOURCE2} Vtable

FCST_LENGTH="0"
END_TIME=`$NDATE +${FCST_LENGTH} $START_TIME`
PREDAY_TIME=`$NDATE -24 $START_TIME`
preday_year=`echo ${PREDAY_TIME} | cut -c1-4`
preday_month=`echo ${PREDAY_TIME} | cut -c5-6`
preday_day=`echo ${PREDAY_TIME} | cut -c7-8`
preday_hour=`echo ${PREDAY_TIME} | cut -c9-10`
preday_YYYYMMHH=${preday_year}${preday_month}${preday_day}
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`
eyyyy=`echo ${END_TIME} | cut -c1-4`
emm=`echo ${END_TIME} | cut -c5-6`
edd=`echo ${END_TIME} | cut -c7-8`
ehh=`echo ${END_TIME} | cut -c9-10`
start_gfs_fcst_time=00
end_gfs_fcst_time=${FCST_LENGTH}

# Get a list of files in the SRCPATH directory
echo COMINgfs ${COMINgfs}

if [ -r ${COMINsst}/nsst.${start_ymd}/sstoi_grb.grib2 ]; then
  gribfiles=`ls -1 ${COMINsst}/nsst.${start_ymd}/sstoi_grb `
else
  gribfiles=`ls -1 ${COMINsst}/nsst.${preday_YYYYMMHH}/sstoi_grb `
  start_yyyymmdd_hhmmss=${preday_year}-${preday_month}-${preday_day}_00:00:00
  end_yyyymmdd_hhmmss=${start_yyyymmdd_hhmmss}
  YYYYMMDDHH=${preday_year}-${preday_month}-${preday_day}_00
fi

# Copy the namelist into the work directory
cp $PARMhrrrdas/hrrrdas_namelist.wps namelist.wps
WPSNAMELIST=namelist.wps

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
                      | sed "s/\(${end}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${start_yyyymmdd_hhmmss}'/"     \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update interval in namelist
cat ${WPSNAMELIST} | sed "s/\(${interval}_${seconds}\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update the prefix in the namelist
cat ${WPSNAMELIST} | sed "s/\(${prefix}\)${equal}'[[:alnum:]]\{1,\}'/\1 = '${SOURCE2}'/" \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

ngribfiles=1

echo  ${gribfiles}
ln -sf ${gribfiles} GRIBFILE.AAA

# Run ungrib
export pgm=hrrr_wps_ungrib
. prep_step

$EXEChrrrdas/hrrr_wps_ungrib > ungribsst.out
export err=$?;err_chk

rm SST
ln -sf sst:${YYYYMMDDHH}  SST

echo "SST ungrib completed at `${DATE}`"

###################
# Run metgrid
###################
export CONSTANTS=SST,QNWFA_QNIFA_Monthly_GFS
export CONSTANT_PATH=$DATA,$PARMhrrrdas
export SOURCE_PATH=$DATA
export METGRIDPROC=METGRID_PROC

export WPSNAMELIST=namelist.wps
START_TIME=`cat STARTTIME`
echo $START_TIME

# Calculate start and end time date strings
FCST_LENGTH="24"
END_TIME=`$NDATE +${FCST_LENGTH} $START_TIME`
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`
eyyyy=`echo ${END_TIME} | cut -c1-4`
emm=`echo ${END_TIME} | cut -c5-6`
edd=`echo ${END_TIME} | cut -c7-8`
ehh=`echo ${END_TIME} | cut -c9-10`
start_yyyymmdd_hhmmss=${syyyy}-${smm}-${sdd}_${shh}:00:00
end_yyyymmdd_hhmmss=${eyyyy}-${emm}-${edd}_${ehh}:00:00

# Calculate the forecast interval in seconds
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))

# Copy the namelist to the work dir
cp ${PARMhrrrdas}/hrrrdas_namelist.wps ${WPSNAMELIST}

# Copy the METGRID.TBL to the work dir
cp ${PARMhrrrdas}/hrrrdas_metgrid.tbl METGRID.TBL

# Link to geogrid static files
rm -f geo_em.d01.nc
rm -f geo_em.d02.nc
cp $FIXhrrrdas/hrrrdas_geo_em.d01.nc geo_em.d01.nc
cp $FIXhrrrdas/hrrrdas_geo_em.d02.nc geo_em.d02.nc

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
metgrid_tbl_path=[Oo][Pp][Tt][_][Mm][Ee][Tt][Gg][Rr][Ii][Dd][_][Tt][Bb][Ll][_][Pp][Aa][Tt][Hh]
yyyymmdd_hhmmss='[[:digit:]]\{4\}-[[:digit:]]\{2\}-[[:digit:]]\{2\}_[[:digit:]]\{2\}:[[:digit:]]\{2\}:[[:digit:]]\{2\}'

# Update the start and end date in namelist
cat ${WPSNAMELIST} | sed "s/\(${start}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 = '${start_yyyymmdd_hhmmss}','${start_yyyymmdd_hhmmss}'/" \
                      | sed "s/\(${end}_${date}\)${equal}'${yyyymmdd_hhmmss}'/\1 ='${end_yyyymmdd_hhmmss}','${start_yyyymmdd_hhmmss}'/"     \
                      > ${WPSNAMELIST}.new
mv ${WPSNAMELIST}.new ${WPSNAMELIST}

# Update interval in namelist
cat ${WPSNAMELIST} | sed "s/\(${interval}_${seconds}\)${equal}[[:digit:]]\{1,\}/\1= ${fcst_interval_sec}/" \
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
  cat ${WPSNAMELIST} | sed "s/\(${constants_name}\)${equal}.*/\1 = ${constants_str} /" \
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
nameall=`sed 's/[[:blank:]]//g' ${WPSNAMELIST} | awk '/^[^#&\/]/'`
for name in ${nameall}
do
  if [ 1 ]; then
    (( i=i+1 ))
    left=`echo ${name} | cut -d"=" -f 1 | awk '{print toupper($0)}'`
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

 echo ${source_list[*]}
 echo ${constant_list[*]}
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
  # Create an array of CONSTANT PATHS
  set -A constant_path_list `echo ${CONSTANT_PATH} | sed "s/[',]\{1,\}/ /g"`

  # Make sure constant_list and constant_path_list are the same length
  if [ ${#constant_list[*]} -ne ${#constant_path_list[*]} ]; then
    echo "FATAL ERROR: The number of paths in \$CONSTANT_PATH does not match the number of constants in constants_name"
    err_exit
  fi

# Create links to all the fg_name sources
i=0
for src in ${source_list[*]}; do
  fcst=0
  while [ ${fcst} -le ${FCST_LENGTH} ]; do
    datestr_temp=`$NDATE +${fcst} $START_TIME`
    yyyy=`echo ${datestr_temp} | cut -c1-4`
    mm=`echo ${datestr_temp} | cut -c5-6`
    dd=`echo ${datestr_temp} | cut -c7-8`
    hh=`echo ${datestr_temp} | cut -c9-10`
    datestr=${yyyy}-${mm}-${dd}_${hh}
#    rm -f ${src}:${datestr}
    if [ -e ${source_path_list[${i}]}/${src}:${datestr} ]; then
      ln -sf ${source_path_list[${i}]}/${src}:${datestr} 
    fi
    (( fcst=fcst+${FCST_INTERVAL} ))
  done
  (( i=i+1 ))
done

# Create linkes to all the constants_name constant files
i=0
for const in ${constant_list[*]}; do
#  rm -f ${const}*
  if [ -e ${constant_path_list[${i}]}/${const} ]; then
    ln -sf ${constant_path_list[${i}]}/${const}
    echo "sst file is available"
  elif [ -e ${constant_path_list[${i}]}/hrrrdas_${const} ]; then
    ln -sf ${constant_path_list[${i}]}/hrrrdas_${const} ${const}
    echo "stripped hrrrdas_ from hrrrdas_QNWFA_QNIFA_Monthly_GFS"
  else
    echo "WARNING: Constant file ${constant_path_list[${i}]}/${const} does not exis
t!"
  fi
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
if [ ${output_format} -eq 1 ]; then
  metgrid_suffix=".int"
elif [ ${output_format} -eq 2 ]; then
  metgrid_suffix=".nc"
else
  metgrid_suffix=""
fi

# Run metgrid
cp ${EXEChrrrdas}/hrrr_wps_metgrid .
#runline="aprun -n 24 -N 24 ./hrrr_wps_metgrid"
runline="mpiexec -n 24 -ppn 24 ./hrrr_wps_metgrid"
$runline
export err=$?; err_chk

echo "metgrid completed successfully at `${DATE}`"

##########################################################
# Run the REAL code to generate the BC and Input files
##########################################################
#for HH in 00 12

echo ${START_TIME}
start_minute="00"
start_YYYYMMDDHHMM=${syyyy}${smm}${sdd}${shh}${start_minute}
pert_bank_dir=${FIXhrrrdas}/pert_bank

for HH in 00
do
   $USHhrrrdas/hrrrdas_real_wpsdfi_bd.sh $HH
# 
# generate perturbed boundary
   echo "perturbing boundary conditions for members "
   no_member=36
   echo $hrrrdas_num_ens
   ensmem=1
   while [[ $ensmem -le $no_member ]];do

     print "\$ensmem is $ensmem"
     ensmemid=`printf %4.4i $ensmem`

     # get background for each member
     cpreq ${DATA}/tar/wrfbdy_d01.${start_YYYYMMDDHHMM} ./wrfbdy_d01.${start_YYYYMMDDHHMM}.mem${ensmemid}

     # next member
     (( ensmem += 1 ))

   done

wait

   ln -sf ${HRRRDASBC}/wrfinput_d01_${syyyy}-${smm}-${sdd}_${shh}_00_00 wrfinput_d01
   cp ${EXEChrrrdas}/hrrr_cal_bcpert .
   ensmem=1
   while [[ $ensmem -le $no_member ]];do

     print "\$ensmem is $ensmem"
     ensmemid=`printf %4.4i $ensmem`

     runline="mpiexec -n 1 -ppn 1 ./hrrr_cal_bcpert wrfbdy_d01.${start_YYYYMMDDHHMM}.mem${ensmemid} wrfinput_d01 ${pert_bank_dir}"
     $runline
     export err=$?; err_chk

     # next member
     (( ensmem += 1 ))

   done

# copy perturbed members to HRRRDASBC directory
   ensmem=1
   while [[ $ensmem -le $no_member ]];do

     print "\$ensmem is $ensmem"
     ensmemid=`printf %4.4i $ensmem`

     cpreq ./wrfbdy_d01.${start_YYYYMMDDHHMM}.mem${ensmemid} ${HRRRDASBC}/wrfbdy_d01.${start_YYYYMMDDHHMM}.mem${ensmemid}

     # next member
     (( ensmem += 1 ))

   done

# create a "ready" file indicating perturbed wrfbdy files are available
   touch ${HRRRDASBC}/wrfbdy.ready.${start_YYYYMMDDHHMM}

done

cd $DATA/tar
mpiexec tar -cvf hrrrdas.t${cyc}z.makebc.tar *
cp hrrrdas.t${cyc}z.makebc.tar $COMOUT/.

msg="JOB $job FOR RAP_MAKEBC HAS COMPLETED NORMALLY"
postmsg "$jlogfile" "$msg"

exit 0
