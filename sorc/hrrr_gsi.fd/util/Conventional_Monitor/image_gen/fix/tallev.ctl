DTYPE  station 
options big_endian sequential
STNMAP tupair.map
UNDEF  -999.0
TITLE  Station Data Sample
TDEF   1 linear 00z27may2006 12hr 
*ZDEF pressure 915.,840.,690.,590.,490.,390.,290.,240.,190.,90. 
VARS  20
shgt     1  0   the station elsvation(meters)
press    1  0   surface pressure
ohgt     1  0   the observation height(meters)
dtime    1  0   relative time to analysis hours
iqc      1  0   input prepbufr qc or event mark
setqc    1  0   setup qc or event mark
iuse     1  0   read_prepbufr data usage flag
muse     1  0   setup data usage flag
rwgt     1  0   nonlear qc relative weight (weight/0.25)
err      1  0   the original data (bufr table) 1/error
rerr     1  0   the readbufr subroutine data 1/error
ferr     1  0   the final data 1/error
obs      1  0   oberved values
obg      1  0   obs-ges used in analysis
obg_ori  1  0   obs-ges w/o adjustment 
dpof     1  0   data pof (used for aircraft bias correction)
dvv      1  0   data vertical velocity (used for aircraft bias correction)
pred_a   1  0   predbias term 1
pred_b   1  0   predbias term 2
pred_c   1  0   predbias term 3
ENDVARS
