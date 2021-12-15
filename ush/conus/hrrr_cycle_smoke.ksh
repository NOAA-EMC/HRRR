#! /bin/ksh

set -xue

echo "hrrr_cycle_smoke.ksh started at $( date )"

to_file="$1"
now="${2:-${START_TIME:-$PDY$cyc}}"

maxback="${maxback:-9}" # how many hours ago to search for wrfout in nwges
minback="${minback:-0}" # minimum number of cycles to go back for smoke in nwges

dom="${dom:-conus}"
gesdir="${gespath}/hrrr/hrrrges_sfc/$dom"
SPAWN="mpiexec -n 1 -ppn 1"
CYCLE_NETCDF="$EXEChrrr/hrrr_cycle_netcdf"

set +x

if [[ ! -d "$gesdir" ]] ; then
    echo "ERROR: Odd.  The hrrr ges dir doesn't exist: $gesdir" 1>&2
    echo "ERROR: Maybe check the \$NWGES, \$dom, and \$envir variables?" 1>&2
    echo "ERROR: This just isn't your day, is it?" 1>&2
    exit 1
fi

saw=NO

for back in $( seq "$minback" "$maxback" ) ; do
    set -x
    before=$( $NDATE -$back $now )
    from_file="$gesdir/hrrr_${before}f$( printf %03d $back )"

    if [[ ! -s "$from_file" ]] ; then
        echo "File $back hours back is unavailable: $from_file"
        continue
    fi

    if ( $SPAWN $CYCLE_NETCDF smoke "$from_file" "$to_file" ) ; then
        postmsg "Hurray! Cycle $now WRF input has an initial smoke state from \"$from_file\""
        echo "hrrr_cycle_smoke.ksh finished at $( date )"
        exit 0
    else
        echo "Non-zero exit status from cycle_netcdf.  Will try the next file." 1>&2
        SAW=YES
    fi
    set +x
done

if [[ "$saw" == NO ]] ; then
    echo "WARNING: Looked back $maxback hours and saw no ges files." 1>&2
fi

postmsg "ERROR: Unable to cycle smoke for hrrr $dom at $now"

exit 1


