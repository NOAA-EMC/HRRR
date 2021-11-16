#! /bin/sh

set -xue

wrf_exe=${1:-hrrr_wrfarw_fcst} # name of wrf executable in local directory
override_file=${2:-/dev/null} # file that overrides some variables

# $PERFTOOLS_GRID_ORDER: The perftools module does not load reliably
# due to errors in the modulefile, so we have an explicit path to
# grid_order here.  The calling script can override this variable if
# preferred.
PERFTOOLS_GRID_ORDER=${PERFTOOLS_GRID_ORDER:-/opt/cray/perftools/6.2.3/bin/grid_order}
WRF_NAMELIST="${WRF_NAMELIST:-namelist.input}"

#test -x "$PERFTOOLS_GRID_ORDER"
#test -s "$PERFTOOLS_GRID_ORDER"

test -x "$wrf_exe"
test -s "$wrf_exe"

omp_num_threads=2 # compute only; io is always 1
mkl_num_threads=1 # compute only; io is always 1
hyperthreads=1
io_hyperthreads=2
turbo_mode=NO # YES or NO
reorder_ranks=grid_order # grid_order or NO
mpich_tuning=cb_nodes # cb_config_list, cb_nodes, or NO
#unset OMP_NUM_THREADS # will be set in aprun -e
#unset MKL_NUM_THREADS # will be set in aprun -e
export OMP_STACKSIZE=500M
#export KMP_AFFINITY=disabled

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

# Override values if requested
if [[ -s "$override_file" ]] ; then
    source "$override_file"
fi

# Calculate further resource information:
nproc_x=$(( nnode_x * nrank_x ))
nproc_y=$(( nnode_y * nrank_y ))
ppn_compute=$(( 24 * hyperthreads / omp_num_threads / mkl_num_threads ))
npes_compute=$(( nproc_x * nproc_y ))
npes_io=$(( nio_groups * nio_tasks_per_group ))
nodes_io=$(( nio_groups * nio_tasks_per_group / nio_ppn ))
nodes_required=$(( nodes_io + nnode_x*nnode_y ))

if [[ "${NODES:-0}" -lt 1 ]] ; then
    echo "WARNING: \$NODES variable is unset.  Will use \$LSB_MAX_NUM_PROCESSORS and assume WCOSS Cray."
    provided_nodes=$(( LSB_MAX_NUM_PROCESSORS / 24 ))
else
    provided_nodes="$NODES"
fi

set +x
echo "Total ranks:      $npes_compute compute and $npes_io io"
echo "Ranks per node:   $ppn_compute compute and $nio_ppn io"
echo "OpenMP Threads:   $omp_num_threads compute and 1 io"
echo "MKL Threads:      $mkl_num_threads compute and 1 io"
echo "Hyperthreads:     $hyperthreads"
echo "IO Hyperthreads:  $io_hyperthreads"
echo "Stack per thread: $OMP_STACKSIZE"
echo "Nodes required:   $nodes_required"
echo "Nodes provided:   $provided_nodes"
set -x

#if (( nodes_required > provided_nodes )) ; then
#    err_exit "Have only ${provided_nodes} of ${nodes_required} required nodes.  Check task geometry, job card, and \$NODES variable."
#    exit 1 # should not get here
#fi
#if (( provided_nodes > nodes_required )) ; then
#    postmsg "WARNING: Job has more nodes than required; will run anyway: ${provided_nodes} > ${nodes_required}"
#fi

# Set task geometry
#if [[ "$reorder_ranks" == grid_order ]] ; then
#    export MPICH_RANK_REORDER_METHOD=3
#    $PERFTOOLS_GRID_ORDER \
#	-C -c "$nrank_x,$nrank_y" \
#        -g "$nproc_x,$nproc_y" > MPICH_RANK_ORDER
#    irank=$npes_compute
#    for inode in $( seq 1 $nodes_io ) ; do
#	first_rank=$irank
#	irank=$(( irank + $nio_ppn ))
#	last_rank=$(( irank - 1 ))
#	echo $( seq $first_rank $last_rank ) \
#	    | sed 's: :,:g' >> MPICH_RANK_ORDER
#    done
#fi

#if [[ "$mpich_tuning" == "cb_nodes" ]] ; then
#    export MPICH_MPIIO_HINTS="wrfinput*:cb_nodes=$cb_nodes,wrfrst*:cb_nodes=$cb_nodes,wrfout*:cb_nodes=$cb_nodes"
#fi

# Disable log messages that slow down the model; use defaults:
#unset MPICH_MPIIO_AGGREGATOR_PLACEMENT_DISPLAY
#unset MPICH_ENV_DISPLAY
#unset MPICH_VERSION_DISPLAY
#unset MPICH_ABORT_ON_ERROR
#unset MPICH_MPIIO_STATS

global_options="-b" # /usr/bin/env is on the compute nodes already

# Specify task geometry and affinity for compute:
wrf_compute="/usr/bin/env OMP_NUM_THREADS=$omp_num_threads"
wrf_compute="$wrf_compute MKL_NUM_THREADS=$mkl_num_threads $wrf_exe"
geo_compute="-N $ppn_compute -n $npes_compute -d $omp_num_threads"

# Specify task geometry and affinity for io
wrf_io="/usr/bin/env OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 $wrf_exe"
geo_io="-N $nio_ppn -n $npes_io -d 1"

# Keep threads close to one another and restrict MPI rank to specific
# list of CPUs:
geo_compute="$geo_compute -cc depth"
geo_io="$geo_io -cc depth"

# Configure hyperthreading separately on compute and io:
geo_compute="$geo_compute -j $hyperthreads"
geo_io="$geo_io -j $io_hyperthreads"

# Enable maximum sustained clock speed or turbo mode if we can.  Only
# do this on the WRF ranks, since those are compute-heavy.  There is a
# special number for a fake clock speed that indicates turbo mode; it
# is always greater than the actual clock speeds, so we find that
# number:
#if [[ "$turbo_mode" == YES ]] ; then
#    scaling_frequencies=/sys/devices/system/cpu/cpu0/cpufreq/scaling_available_frequencies
#    aprun -n 1 cat "$scaling_frequencies" > scaling_frequencies
#    grep -E '^[0-9 	]' < scaling_frequencies > scaling_frequencies_numbers
#    sed 's, ,\n,g' < scaling_frequencies_numbers > scaling_frequencies_eoln
#    sort -nu scaling_frequencies_eoln > scaling_frequencies_sorted
#    tail -1 scaling_frequencies_sorted
#    p_state=$( tail -1 scaling_frequencies_sorted )
#    if [ "$p_state" -gt 0 ] ; then
#	geo_compute="$geo_compute --p-state $p_state"
#    fi
#fi

cat ${WRF_NAMELIST} | sed \
  -e 's,\(nproc_x.*=\).*,\1 '$nproc_x',g' \
  -e 's,\(nproc_y.*=\).*,\1 '$nproc_y',g' \
  -e 's,\(numtiles.*=\).*,\1 '$omp_num_threads',g' \
  -e 's,\(nio_tasks_per_group.*=\).*,\1 '$nio_tasks_per_group',g' \
  -e 's,\(nio_groups.*=\).*,\1 '$nio_groups',g' \
  -e 's,namelist_quilt,namelist_quilt\n poll_servers=.true.,g' \
    > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

########################################################################

# Run wrf
startmsg
# quilting with nio_groups = 4 and 72 tasks per group - 108 nodes
#runline="aprun $global_options $geo_compute $wrf_compute : $geo_io $wrf_io"
#runline="mpiexec -n 1152 -ppn 64 --cpu-bind core -depth 2 ./hrrr_wrfarw_fcst"
runline="mpiexec -n 2304 -ppn 64 --cpu-bind core -depth 2 ./hrrr_wrfarw_fcst"
$runline
export err=$?; err_chk
