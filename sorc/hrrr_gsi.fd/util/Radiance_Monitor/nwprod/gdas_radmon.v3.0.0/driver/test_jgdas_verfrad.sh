#!/bin/ksh

#BSUB -o gdas_verfrad.o%J
#BSUB -e gdas_verfrad.o%J
#BSUB -J gdas_verfrad
#BSUB -q dev_shared
#BSUB -n 1
#BSUB -R affinity[core]
#BSUB -M 100
#BSUB -W 00:20
#BSUB -a poe
#BSUB -P GFS-T2O

set -x

export PDATE=2018091718

#############################################################
# Specify whether the run is production or development
#############################################################
export PDY=`echo $PDATE | cut -c1-8`
export cyc=`echo $PDATE | cut -c9-10`
export job=gdas_verfrad.${cyc}
export pid=${pid:-$$}
export jobid=${job}.${pid}
export envir=para
export DATAROOT=/gpfs/td2/emc/da/noscrub/${LOGNAME}/test_data
export COMROOT=/ptmpp1/$LOGNAME/com


#############################################################
# Specify versions
#############################################################
export gdas_ver=v15.0.0
export global_shared_ver=v15.0.0
export gdas_radmon_ver=v3.0.0
export radmon_shared_ver=v3.0.0


#############################################################
# Load modules
#############################################################
. /usrx/local/Modules/3.2.9/init/ksh
module use /nwprod2/modulefiles
#module load grib_util
module load prod_util
#module load util_shared

module list


#############################################################
# WCOSS environment settings
#############################################################
export POE=YES


#############################################################
# Set user specific variables
#############################################################
export RADMON_SUFFIX=testrad
export NWTEST=/gpfs/td2/emc/da/noscrub/Edward.Safford/ProdGSI/util/Radiance_Monitor/nwprod
export HOMEgdas=${NWTEST}/gdas_radmon.${gdas_radmon_ver}
export JOBGLOBAL=${HOMEgdas}/jobs
export HOMEradmon=${NWTEST}/radmon_shared.${radmon_shared_ver}
export COM_IN=${DATAROOT}
export TANKverf=${COMROOT}/${RADMON_SUFFIX}


#############################################################
# Execute job
#############################################################
$JOBGLOBAL/JGDAS_VERFRAD

exit

