#! /bin/ksh

set -xue

wrfinput_d01="$1"
STAT_TIME="${START_TIME:-$PDY$cyc}"
EXEC_FIRES="$EXEChrrr/hrrr_fires_ncfmake"
EXEC_CYCLE="$EXEChrrr/hrrr_cycle_netcdf"

# Wrapper to start prep-chem-sources.  Use aprun if this is a MAMU node:
RUNSERIAL='mpiexec -n 1 -ppn 1'

# Make sure we are using GMT time zone for time computations:
export TZ="GMT"

# Set up paths to shell commands
ECHO="${ECHO:-/bin/echo}"
DATE="${DATE:-/bin/date}"

# Run the command:
${ECHO} "hrrr_smoke_wrfinput_pre.ksh started at `${DATE}`"

if [ "$cyc" = 00 ]; then
  sources="$COMINm1/hrrr.t${cycm1}z.smoke.sources.bin"
else
  sources="$COMOUT/hrrr.t${cycm1}z.smoke.sources.bin"
fi

if [[ ! -s "$sources" ]] || ( ! $RUNSERIAL ${EXEC_FIRES} ./wrfinput_d01 "$sources" ) ; then
    # Things broke, so zero-out emissions.
    all_vars='ebu_in_no ebu_in_co ebu_in_pm25 ebu_in_pm10 ebu_in_oc ebu_in_bc ebu_in_smoke FLAM_FRAC MEAN_FRP STD_FRP MEAN_FSIZE STD_FSIZE EBB_SMOKE'
    $RUNSERIAL $EXEC_CYCLE -z -u $all_vars wrfinput_d01 wrfinput_d01
fi

${ECHO} "hrrr_smoke_wrfinput.ksh finished at `${DATE}`"
