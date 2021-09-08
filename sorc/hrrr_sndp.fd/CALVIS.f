c**********************************************************************c
      SUBROUTINE CALVIS(QV,QC,QR,QI,QS,TT,PP,HOVI)
c
c   This routine computes horizontal visibility at the
c   surface or lowest model layer, from qc, qr, qi, and qs.  
c   qv--water vapor mixing ratio (kg/kg)
c   qc--cloud water mixing ratio (kg/kg)
c   qr--rain water mixing ratio  (kg/kg)
c   qi--cloud ice mixing ratio   (kg/kg)
c   qs--snow mixing ratio        (kg/kg)
c   tt--temperature              (k)
c   pp--pressure                 (mb)
c
c   The routine uses the following
c   expressions for extinction coefficient, beta (in km**-1),
c   with C being the mass concentration (in g/m**3):
c
c      cloud water:  beta = 144.7 * C ** (0.8800)
c      rain water:   beta =  2.24 * C ** (0.7500)
c      cloud ice:    beta = 327.8 * C ** (1.0000)
c      snow:         beta = 10.36 * C ** (0.7776)
c
c   These expressions were obtained from the following sources:
c
c      for cloud water: from Kunkel (1984)
c      for rainwater: from M-P dist'n, with No=8e6 m**-4 and
c         rho_w=1000 kg/m**3
c      for cloud ice: assume randomly oriented plates which follow
c         mass-diameter relationship from Rutledge and Hobbs (1983)
c      for snow: from Stallabrass (1985), assuming beta = -ln(.02)/vis
c
c   The extinction coefficient for each water species present is
c   calculated, and then all applicable betas are summed to yield
c   a single beta. Then the following relationship is used to
c   determine visibility (in km), where epsilon is the threshhold
c   of contrast, usually taken to be .02:
c
c      vis = -ln(epsilon)/beta      [found in Kunkel (1984)]
c
C------------------------------------------------------------------
C
C      REAL QV,QC,QI,QR,QS,TT,PP,HOVI
C------------------------------------------------------------------
C------------------------------------------------------------------
      H1=1.0
      D608=0.608
      RD=287.04
      CELKEL=273.15
      TICE=CELKEL-10.
      COEFLC=144.7
      COEFLP=2.24
      COEFFC=327.8
      COEFFP=10.36
      EXPONLC=0.8800
      EXPONLP=0.7500
      EXPONFC=1.0000
      EXPONFP=0.7776
      CONST1=-LOG(.02)
      RHOICE=917.
      RHOWAT=1000.
C
          QRAIN=QR
          QSNOW=QS
          QCLW=QC
          QCLICE=QI
c          print *, 'QRAIN in CALVIS', QRAIN
c          print *, 'QSNOW ', QSNOW
c          print *, 'QCLW ', QCLW
c          print *, 'QCLICE ', QCLICE

c       ENDIF
c       TV=VIRTUAL(TT,QV)
        TV=TT*(H1+D608*QV)
        RHOAIR=PP/(RD*TV)
          VOVERMD=(1.+QV)/RHOAIR+(QCLW+QRAIN)/RHOWAT+
     1            (QCLICE+QSNOW)/RHOICE
c          print *, 'vovermd ', VOVERMD
c          print *, 'rhoair ', RHOAIR
          CONCLC=QCLW/VOVERMD*1000.
          CONCLP=QRAIN/VOVERMD*1000.
          CONCFC=QCLICE/VOVERMD*1000.
          CONCFP=QSNOW/VOVERMD*1000.
c       ENDIF
        BETAV=COEFFC*CONCFC**EXPONFC+COEFFP*CONCFP**EXPONFP
     1       +COEFLC*CONCLC**EXPONLC+COEFLP*CONCLP**EXPONLP
     2       +1.E-10
c        print *, 'snow beta ', COEFFP*CONCFP**EXPONFP
c        print *, 'rain beta ', COEFLP*CONCLP**EXPONLP
c        print *, 'cloud water beta', COEFLC*CONCLC**EXPONLC
c        print *, 'cloud ice beta', COEFFC*CONCFC**EXPONFC 
c        print *, 'total beta ', BETAV

C     CONVERT TO METERS.  MAX VISIBILTY is 80 KM.
c  changed GSM 3-00 --> feedback from the field has suggested that
c     there is no reason to distinguigh any values of visibility
c     greater than 20 km, so the max value has been changed accordingly 
      HOVI=1000*(MIN(20.,CONST1/BETAV))
c      print *, 'hovi ', HOVI
C
      RETURN
      END
