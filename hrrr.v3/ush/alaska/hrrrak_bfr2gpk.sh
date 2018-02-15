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


#  Go to a working directory.

cd $DATA
cp $FIXgem/hrrr_snhrrr.prm snhrrr.prm
cp $FIXgem/hrrr_sfhrrr.prm_aux sfhrrr.prm_aux
cp $FIXgem/hrrr_sfhrrr.prm sfhrrr.prm

#  Set input file name.

INFILE=$COMOUT/hrrr.t${cyc}z.class1.bufr.ak.tm00
export INFILE

#  Set output directory:

outfilbase=hrrrak_${PDY}${cyc}

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
cp $snd $COMNAWP/.$snd
cp $sfc $COMNAWP/.$sfc
cp $aux $COMNAWP/.$aux
mv $COMNAWP/.$snd $COMNAWP/$snd
mv $COMNAWP/.$sfc $COMNAWP/$sfc
mv $COMNAWP/.$aux $COMNAWP/$aux

if [ "$SENDDBN" = 'YES' ] ; then
  $DBNROOT/bin/dbn_alert MODEL SFC_HRRR${ALERT_EXT} $job $COMNAWP/$sfc
  $DBNROOT/bin/dbn_alert MODEL SFC_HRRR${ALERT_EXT} $job $COMNAWP/$aux
  $DBNROOT/bin/dbn_alert MODEL SND_HRRR${ALERT_EXT} $job $COMNAWP/$snd
fi
