set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge

#
module purge

# These get unset by the module purge:
module use /opt/cray/ari/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/lib/modulefiles/
module use /gpfs/hps/nco/ops/nwprod/modulefiles/

# System and compiler prereqs:
module load modules/3.2.6.7
module load switch/1.0-1.0502.57058.1.58.ari
module load craype-network-aries
module load ncep/1.0
module load xt-lsfhpc/9.1.3
module load craype/2.3.0
module load PrgEnv-intel/5.2.56
module load craype-haswell
module switch intel intel/15.0.3.187
module load hpss/4.1.0.3
module load dvs
module load eswrap

# Load iobuf module to add buffering to unbuffered I/O in
# applications:
module load iobuf/2.0.6

#PNetCDF:
module load PNetCDF-intel-haswell/1.5.0
#export PNETCDF=/gpfs/hps/usrx/local/prod/PNetCDF/1.5.0/intel/haswell/

#NetCDF:
module load HDF5-serial-intel-haswell/1.8.9
module load NetCDF-intel-haswell/4.2
#export NETCDF=/gpfs/hps/usrx/local/prod/NetCDF/4.2/intel/haswell/

#Huge pages
module load craype-hugepages256M

cd ${BASE}/hrrr_cal_bcpert.fd
make clean
make

##############################
