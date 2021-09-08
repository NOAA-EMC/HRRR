#!/bin/ksh

#Theia Job options
#PBS -o compout
#PBS -e comperr
#PBS -N covcalc
#PBS -q batch
#PBS -l walltime=01:00:00
#PBS -A da-cpu
#PBS -l nodes=1:ppn=16

#WCOSS Job options
#BSUB -e comperr
#BSUB -o compout
#BSUB -J covcalc
#BSUB -q dev
#The next line is only necessary on Cray
#It can be deleted on Phase 1/2
#BSUB -M 50
#BSUB -openmp
#BSUB -n 16
#BSUB -W 01:00
#BSUB -R span[ptile=16]
#BSUB -P GFS-T2O
if [ ! -z "$PBS_NP" ] ; then
   export OMP_NUM_THREADS=$PBS_NP
elif [ ! -z "$LSB_DJOB_NUMPROC" ] ; then
   export OMP_NUM_THREADS=$LSB_DJOB_NUMPROC
else
   export OMP_NUM_THREADS=16
fi
corrdir=/scratch4/NCEPDEV/da/save/${USER}/GSI/trunk/util/Correlated_Obs
cd ${corrdir}
./run.sh 

