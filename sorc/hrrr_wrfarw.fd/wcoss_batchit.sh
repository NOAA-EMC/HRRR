#! /bin/bash

#BSUB -J hrrr-test
#BSUB -o hrrr-test-%J.log
#BSUB -q devonprod
#BSUB -P HRRR-T2O
#BSUB -M 2000
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 1:59
#BSUB -L /bin/sh

export NODES=1

main() {
   aprun -n 1 -d 24 -N 1 -j 1 ./wcoss_doit.sh configure
   aprun -cc depth -d 8 -n 1 -d 24 -N 1 -j 1 ./wcoss_doit.sh compile
#   aprun -n 1 -d 24 -N 1 -j 1 ./wcoss_doit.sh perftools
#   aprun -n 1 -d 24 -N 1 -j 1 ./wcoss_doit.sh altperf
}

main # > build.log 2>&1
