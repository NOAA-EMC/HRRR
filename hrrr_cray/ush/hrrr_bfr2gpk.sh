#!/bin/sh -l
#########################################################################
#									#
# Script: hrrr_bfr2gpk							#
#									#
#  This script reads HRRR BUFR output and unpacks it into GEMPAK	#
#  surface and sounding data files.					#
#									#
# Log:									#
# G. Manikin/EMC        5/30/14  adapt for HRRR                         #
#########################################################################  
set -x


# Set GEMPAK paths.

. ${NWROOTp1}/gempak/.gempak

#  Go to a working directory.

cd $DATA
cp $PARMhrrr/hrrr_snhrrr.prm snhrrr.prm
cp $PARMhrrr/hrrr_sfhrrr.prm_aux sfhrrr.prm_aux
cp $PARMhrrr/hrrr_sfhrrr.prm sfhrrr.prm

#  Set input file name.

INFILE=$COMOUT/hrrr.t${cyc}z.class1.bufr.tm00
export INFILE

#  Set output directory:

OUTDIR=${COMAWP:-${COMROOT}/nawips/${envir}/hrrr.${PDY}}

outfilbase=hrrr_${PDY}${cyc}

namsnd << EOF > /dev/null
SNBUFR   = $INFILE
SNOUTF   = ${outfilbase}.snd
SFOUTF   = ${outfilbase}.sfc+
SNPRMF   = snhrrr.prm
SFPRMF   = sfhrrr.prm
TIMSTN   = 85/2000
r

exit
EOF

/bin/rm *.nts

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc
aux=${outfilbase}.sfc_aux
cp $snd $OUTDIR/.$snd
cp $sfc $OUTDIR/.$sfc
cp $aux $OUTDIR/.$aux
mv $OUTDIR/.$snd $OUTDIR/$snd
mv $OUTDIR/.$sfc $OUTDIR/$sfc
mv $OUTDIR/.$aux $OUTDIR/$aux

if [ "$SENDDBN" = 'YES' ] ; then
  $DBNROOT/bin/dbn_alert MODEL SFC_HRRR${ALERT_EXT} $job $OUTDIR/$sfc
  $DBNROOT/bin/dbn_alert MODEL SFC_HRRR${ALERT_EXT} $job $OUTDIR/$aux
  $DBNROOT/bin/dbn_alert MODEL SND_HRRR${ALERT_EXT} $job $OUTDIR/$snd
fi
