#! /bin/sh

set -xue

wrf_exe=${1:-hrrr_wrfarw_fcst} # name of wrf executable in local directory
override_file=${2:-/dev/null} # file that overrides some variables

WRF_NAMELIST="${WRF_NAMELIST:-namelist.input}"

test -x "$wrf_exe"
test -s "$wrf_exe"

omp_num_threads=2 # compute only; io is always 1
mkl_num_threads=1 # compute only; io is always 1
hyperthreads=1
io_hyperthreads=2
turbo_mode=NO # YES or NO
reorder_ranks=grid_order # grid_order or NO
mpich_tuning=cb_nodes # cb_config_list, cb_nodes, or NO
export OMP_STACKSIZE=500M

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
if [ $cyc -eq 00 -o $cyc -eq 06 -o $cyc -eq 12 -o $cyc -eq 18 ]; then
  runline="mpiexec -n 2304 -ppn 64 --cpu-bind core -depth 2 ./hrrr_wrfarw_fcst"
else
  runline="mpiexec -n 1152 -ppn 64 --cpu-bind core -depth 2 ./hrrr_wrfarw_fcst"
fi
$runline
export err=$?; err_chk
