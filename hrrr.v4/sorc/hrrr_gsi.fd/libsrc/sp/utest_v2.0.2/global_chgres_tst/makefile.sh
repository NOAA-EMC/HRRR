#!/bin/sh
set -x
export LIBDIR=/nwprod/lib
#export FCMP=xlf_r
#export FCMP95=xlf95_r
export FCMP=${1:-ifort}
export FCMP95=${2:-${FCMP}}
if [ $LIBDIR = /nwprod/lib ] ; then
  export INCS="-I $LIBDIR/incmod/sfcio_4"

# export LIBS="-L$LIBDIR -lsigio_4 -lsfcio_4 -llandsfcutil_d -lip_d -lsp_d -lgfsio_4 -lbacio_4 -lw3emc_d -lw3nco_d -lnemsio"

#export LIBS="-L/global/noscrub/Shrinivas.Moorthi/para/lib -lsigio_4 -L$LIBDIR -lsfcio_4 -llandsfcutil_d -lip_d -lgfsio_4 -lbacio_4 -lw3emc_d -lw3nco_d -lnemsio -L/gpfs/thm/u/Eugene.Mirvis/WCOSS/lib -lsp_d"

  export LIBS="-L/global/noscrub/Shrinivas.Moorthi/para/lib -lsigio_4 -L$LIBDIR  -lsfcio_4 -llandsfcutil_d -lip_d -lgfsio_4 -lbacio_4 -lw3emc_d -lw3nco_d -lnemsio -L/global/noscrub/Shrinivas.Moorthi/para/lib -lsp_d"

#export LIBS="-L/global/noscrub/Shrinivas.Moorthi/para/lib -lsigio_4 -L$LIBDIR  -lsfcio_4 -llandsfcutil_d -lip_d -lgfsio_4 -lbacio_4 -lw3emc_d -lw3nco_d -lnemsio -L/global/noscrub/Shrinivas.Moorthi/lib -lsp_d"


else
  export INCS="-I $LIBDIR/incmod/sfcio_4"
  export LIBS="-L$LIBDIR -lbacio_4 -lsigio_4 -lsfcio_4 -llandsfcutil_d -lip_d -lsp_d -lgfsio_4 -lw3lib-2.0_d -lnemsio"
fi
if [ $FCMP = xlf_r ] ; then
 export FFLAGSM=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1"
 export FFLAGS2M=" -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1"
 export LIBFLAGSM="-lessl "
 export OMPFLAGM=
 export RECURS=
else
#export FFLAGSM=" -O0 -i4 -r8 -xHOST -convert big_endian -traceback -override-limits -g -auto"
#export FFLAGS2M=" -O0 -i4 -r8 -xHOST -convert big_endian -traceback -override-limits -g -FR -auto"

 export FFLAGSM=" -g -i4 -O3 -r8 -xHOST -convert big_endian -fp-model strict"
 export FFLAGS2M=" -g -i4 -O3 -r8 -xHOST -convert big_endian -fp-model strict -FR"

#export RECURS="-recursive"
 export RECURS=

 export LIBFLAGSM=" "
 export LDFLAGSM=-mkl
 export OMPFLAGM="-openmp -auto"
fi
make -f Makefile
