#! /bin/bash
# This is a bash script; not ksh or sh.  Do not change the above line.

# Runs the prep_chem_sources program to produce FV3 inputs for
# chemistry fields.  All inputs are environment variables; any
# arguments are ignored.
#
# Output filenames are generated from this pair of variables:
#   $COMOUTchem/$CHEM_OUTPUT_FORMAT
# The following two strings are replaced:
#   %INPUT% = tracer name or other input name
#   %TILE% = tile number
#
# Date & Time:
#   $SYEAR, $SMONTH, $SDAY, $SHOUR
#     -or-
#   $PDY = $SYEAR$SMONTH$SDAY, $cyc = $SHOUR
#
# Executable: $PREP_CHEM_SOURCES_EXE
#
# Resolution (ie. C384): $CASE
#  NOTE: Only C384 will work presently.
#
# Input data:
#   $PARMchem/prep_chem_sources.inp.IN
#   $FIXchem = static input directory
#   $BBEM_MODIS_DATA_DIR_TODAY = directory with today's modis fire data
#   $BBEM_MODIS_DATA_DIR_YESTERDAY = directory with yesterday's modis fire data
#   $BBEM_WFABBA_DATA_DIR_TODAY = directory with today's wf_abba data
#   $BBEM_WFABBA_DATA_DIR_YESTERDAY = directory with yesterday's wf_abba data
#   $GBBEPX_DATA_DIR = directory with today's GSCE GBBEPx data
# (Each data source must have both today and yesterday directories specified, but
# only one day per source must exist to run this script.)
#
# Other:
#   $DATA = where to run
#   $SENDCOM = YES if data should be copied to COM; default is YES
#   $SENDDBN = ignored; would enable NCO's dbnet calls
#   $SENDECF = ignored; would enable ecflow_client calls

# DEV NOTES
# As of this writing, test data for MODIS $BBEM_MODIS_DATA is:
#  Gyre/Surge/Venus: /gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/MODISfiredata/datafiles/FIRMS/c6/Global/MODIS_C6_Global_MCD14DL_NRT_
# For "other input data" $BBEM_WFABBA_DATA:
#  Jet: /public/data/sat/nesdis/wf_abba/
#  Gyre/Surge/Venus: /gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/public/data/sat/nesdis/wf_abba/f
# The FIXchem:
#  Gyre/Surge/Venus: /gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/FIXchem/

set -xue

if [[ ! -d "$COMOUTchem" ]] ; then
    mkdir -p "$COMOUTchem"
fi

if [[ ! -d "$DATA" ]] ; then
    mkdir -p "$DATA"
    cd "$DATA"
fi

SYEAR="${SYEAR:-${PDY:0:4}}"
SMONTH="${SMONTH:-${PDY:4:2}}"
SDAY="${SDAY:-${PDY:6:2}}"
SHOUR="${SHOUR:-$cyc}"
PDY="${PDY:-$SYEAR$SMONTH$SDAY}"

PREP_CHEM_SOURCES_EXE="${PREP_CHEM_SOURCES_EXE:-$EXECchem/prep_chem_sources_RADM_FV3_SIMPLE.exe}"

SENDCOM="${SENDCOM:-YES}"
# - this variable is not used - SENDDBN="${SENDDBN:-NO}"
# - this variable is not used - SENDECF="${SENDECF:-NO}"

echo "in emission_setup:"
emiss_date="$SYEAR-$SMONTH-$SDAY-$SHOUR"
echo "emiss_date: $emiss_date"
echo "yr: $SYEAR mm: $SMONTH dd: $SDAY hh: $SHOUR"

FIX_GRID_SPEC=$FIXchem/grid-spec/${CASE}/${CASE}_grid_spec

mkdir MODIS
mkdir WFABBA

jul_today=$( date -d "$SYEAR-$SMONTH-$SDAY 12:00:00 +0000" +%Y%j )
jul_yesterday=$( date -d "$SYEAR-$SMONTH-$SDAY 12:00:00 +0000 + -1 day" +%Y%j )

count_modis=0
count_wfabba=0
count_gbbepx=0
expect_gbbepx=0
use_gbbepx=NO

gbbepx_list="GBBEPx.bc GBBEPx.oc GBBEPx.so2 GBBEPx.pm25 meanFRP"

cd MODIS

set +x # This region is too verbose for "set -x"
for path in \
    "$BBEM_MODIS_DIR_YESTERDAY/MODIS_C6_Global_MCD14DL_NRT_$jul_today"* \
    "$BBEM_MODIS_DIR_TODAY/MODIS_C6_Global_MCD14DL_NRT_$jul_yesterday"*
do
    if [[ -s "$path" ]] ; then
        ln -s "$path" .
        count_modis=$(( count_modis+1 ))
	echo "WILL LINK: $path"
    else
	echo "EMPTY: $path"
    fi
done
echo "Found $count_modis MODIS fire data files."
set -x

cd ../WFABBA

set +x # This region is too verbose for "set -x"
for path in \
    "$BBEM_WFABBA_DIR_YESTERDAY/f$jul_yesterday"* \
    "$BBEM_WFABBA_DIR_TODAY/f$jul_today"*
do
    if [[ -s "$path" ]] ; then
        ln -s "$path" .
        count_wfabba=$(( count_wfabba+1 ))
	echo "WILL LINK: $path"
    else
	echo "EMPTY: $path"
    fi
done
echo "Found $count_wfabba wf_abba data files."
set -x

cd ..

set +x  # This region is too verbose for "set -x"
for gbbepx_file in $gbbepx_list ; do
    for itile in 1 2 3 4 5 6 ; do
	tiledir=tile$itile
	expect_gbbepx=$(( expect_gbbepx + 1 ))
	infile="$GBBEPX_DATA_DIR/${PDY}.${gbbepx_file}.FV3.${CASE}Grid.$tiledir.bin"
	if [[ -s "$infile" ]] ; then
	    count_gbbepx=$(( count_gbbepx + 1 ))
	fi
    done
done
set -x

if (( count_modis==0 )) ; then
    echo "WARNING: NO MODIS FIRE DATA FOUND!" 1>&2
fi

if (( count_gbbepx == 0 )) ; then
    echo "WARNING: NO GBBEPX FILES FOUND!" 1>&2
    use_gbbepx=NO
elif (( count_gbbepx!=expect_gbbepx )) ; then
    echo "WARNING: EXPECTED $expect_gbbepx GBBEPX FILES BUT FOUND $count_gbbepx!  WILL NOT USE GBBEPX!" 1>&2
    use_gbbepx=NO
else
    use_gbbepx=YES
fi

if (( count_wfabba==0 )) ; then
    echo "WARNING: NO WF_ABBA DATA FOUND!" 1>&2
fi

if (( count_wfabba==0 && count_modis==0 && count_gbbepx!=expect_gbbepx )) ; then
    echo "WARNING: NO REAL-TIME DATA FOUND!  RESORTING TO STATIC DATA!" 1>&2
fi

cp -fp "$PARMchem/prep_chem_sources.inp.IN" .
cat prep_chem_sources.inp.IN | sed \
    "s:%HH%:$SHOUR:g                                             ;
     s:%DD%:$SDAY:g                                              ;
     s:%MM%:$SMONTH:g                                            ;
     s:%YYYY%:$SYEAR:g                                           ;
     s:%FIXchem%:$FIXchem:g                                      ;
     s:%FIX_GRID_SPEC%:$FIX_GRID_SPEC:g                          ;
     s:%BBEM_MODIS_DATA%:./MODIS/MODIS_C6_Global_MCD14DL_NRT_:g  ;
     s:%BBEM_WFABBA_DATA%:./WFABBA/f:g                           ;
    " > prep_chem_sources.inp

if ( cat prep_chem_sources.inp | grep % ) ; then
    echo "POSSIBLE ERROR: still have % signs in prep_chem_sources.inp" 1>&2
    echo "Some variables may not have been replaced." 1>&2
    echo "I will continue, but you should keep your fingers crossed." 1>&2
fi

#
$PREP_CHEM_SOURCES_EXE
#

if [[ "$use_gbbepx" == YES ]] ; then
    inout_list="BBURN3-bb,ebu_pm_10 SO4-bb,ebu_sulf plume,plumestuff GBBEPx.bc,ebu_bc GBBEPx.oc,ebu_oc GBBEPx.so2,ebu_so2 GBBEPx.pm25,ebu_pm_25 meanFRP,plumefrp"
else
    inout_list="plume,plumestuff OC-bb,ebu_oc BC-bb,ebu_bc BBURN2-bb,ebu_pm_25 BBURN3-bb,ebu_pm_10 SO2-bb,ebu_so2 SO4-bb,ebu_sulf"
    #inout_list="plume,plumestuff OC-bb,ebu_oc BC-bb,ebu_bc BBURN2-bb,ebu_pm_25 BBURN3-bb,ebu_pm_10 SO2-bb,ebu_so2 SO4-bb,ebu_sulf ALD-bb,ebu_ald ASH-bb,ebu_ash.dat CO-bb,ebu_co CSL-bb,ebu_csl DMS-bb,ebu_dms ETH-bb,ebu_eth HC3-bb,ebu_hc3 HC5-bb,ebu_hc5 HC8-bb,ebu_hc8 HCHO-bb,ebu_hcho ISO-bb,ebu_iso KET-bb,ebu_ket NH3-bb,ebu_nh3 NO2-bb,ebu_no2 NO-bb,ebu_no OLI-bb,ebu_oli OLT-bb,ebu_olt ORA2-bb,ebu_ora2 TOL-bb,ebu_tol XYL-bb,ebu_xyl"
fi

if [[ "${SENDCOM:-YES}" == YES ]] ; then
    for itile in 1 2 3 4 5 6 ; do
        tiledir=tile$itile
        pushd $tiledir

        set +x # A line-by-line log is too verbose here:
        for inout in $inout_list ; do
            if [[ $inout =~ (.*),(.*) ]] ; then
                local_name="${BASH_REMATCH[1]}"
                comdir_name="${BASH_REMATCH[2]}"

		is_gbbepx_data=NO
		for gdat in $gbbepx_list ; do
		    if [[ "$gdat" == "$local_name" ]] ; then
			is_gbbepx_data=YES
			break
		    fi
		done

		if [[ "$is_gbbepx_data" == YES ]] ; then
		    if [[ "$use_gbbepx" != YES ]] ; then
			continue
		    fi
		    infile="$GBBEPX_DATA_DIR/${PDY}.${local_name}.FV3.${CASE}Grid.$tiledir.bin"
		else
                    infile="${CASE}-T-${emiss_date}0000-${local_name}.bin"
		fi
                step1="$COMOUTchem/$CHEM_OUTPUT_FORMAT"
                step2=${step1//%INPUT%/$comdir_name}
                outfile=${step2//%TILE%/$itile}
            else
                echo "Internal error: could not split \"$inout\" into two elements at a comma." 1>&2
                exit 9
            fi

	    set -ue
            outdir=$( dirname "$outfile" )
            outbase=$( basename "$outfile" )
            randhex=$( printf '%02x%02x%02x%02x' $(( RANDOM%256 )) $(( RANDOM%256 )) $(( RANDOM%256 )) $(( RANDOM%256 )) )
            tempbase=".tmp.$outbase.$randhex.part"
            tempfile="$outdir/$tempbase"

            if [[ ! -d "$outdir" ]] ; then
                echo "make directory $outdir" 1>&2
                mkdir -p "$outdir"
            fi
            rm -f "$outfile"
            echo "copy $infile => $tempfile" 1>&2
            cp -fp "$infile" "$tempfile"
            echo "rename $tempbase => $outbase in $outdir" 1>&2
            mv -T -f "$tempfile" "$outfile"
        done
        set -x
        popd
    done
fi

echo 'Success!'
echo 'Please enjoy your new tracers.'
exit 0
