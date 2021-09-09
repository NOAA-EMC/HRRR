Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C DoPhase is a subroutine written and provided by Jim Ramer at NOAA/FSL
C
C    Ramer, J, 1993: An empirical technique for diagnosing precipitation
C           type from model output.  Preprints, 5th Conf. on Aviation
C           Weather Systems, Vienna, VA, Amer. Meteor. Soc., 227-230.
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
       SUBROUTINE CALWXT_RAMER(ttq,qq,ppq,pint,nq,lm,ppt,ptyp)
c      SUBROUTINE dophase(pq,   !  input pressure sounding mb
c     +    tq,   !  input temperature sounding K
c     +    pq,   |  input pressure
c     +    qq,   !  input spec humidityfraction
c     +    nq,   !  input number of levels in sounding
c     +    twq,  !  output wet-bulb sounding K
c     +    icefrac,      !  output ice fraction
c     +    ptyp) !  output(2) phase 2=Rain, 3=Frzg, 4=Solid,
C                                               6=IP     JC  9/16/99
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516)
      PARAMETER (G=9.80665,CP=1004.686,RCP=0.2857141,LECP=1572.5)
      PARAMETER (twice=266.55,rhprcp=0.80,deltag=1.02,prcpmin=0.3,
     *             emelt=0.045,rlim=0.04,slim=0.85)
      PARAMETER (twmelt=273.15,tz=273.15,efac=1.0,PTHRES=0.02)
C
      INTEGER*4 i, k1, lll, k2, toodry, iflag, nq
C
      INTEGER ptyp
C
      REAL rcp, flg, flag, xxx, pq(lm), tq(lm), twq(lm), rhq(lm), mye,
     *              qq(lm), icefrac, tqtmp(lm), pqtmp(lm), rhqtmp(lm),
     *              pint(lm+1), ttq(lm), ppq(lm), ppint(lm+1),
     *              pinttmp(lm+1)
C
      COMMON /flagflg/ flag, flg
      DATA iflag / -9/
C
C  Initialize.
      icefrac = flag
      ptyp = 0 
      IF (PPT.LE.PTHRES) RETURN
C
C GSM compute RH, convert pressure to mb, and reverse order

      DO 88 i = 1, nq
        LEV=NQ-I+1
        QC=PQ0/PPQ(I) * EXP(A2*(TTQ(I)-A3)/(TTQ(I)-A4))
        RHQTMP(LEV)=QQ(I)/QC
        PQTMP(LEV)=PPQ(I)/100.
        TQTMP(LEV)=TTQ(I)
        PINTTMP(LEV)=PINT(I)
   88 CONTINUE

      do 92 i=1,nq
         TQ(I)=TQTMP(I)
         PQ(I)=PQTMP(I)
         RHQ(I)=RHQTMP(I)
         PPINT(I)=PINTTMP(I)
   92 continue


C     See if there was too little precip reported.
C
CCC   RATE RESTRICTION REMOVED BY JOHN CORTINAS 3/16/99
C
C     Construct wet-bulb sounding, locate generating level.
      twmax = -999.0
      rhmax = 0.0
      k1 = 0    !  top of precip generating layer
      k2 = 0    !  layer of maximum rh
C
      IF (rhq(1).lt.rhprcp) THEN
          toodry = 1
      ELSE
          toodry = 0
      END IF
C
C     toodry=((Rhq(1).lt.rhprcp).and.1)
      lmhk=nq
      pbot = ppint(lmhk+1) 
      DO 10 i = 1, nq
          xxx = tdofesat(esat(tq(i))*rhq(i))
          twq(i) = xmytw(tq(i),xxx,pq(i))
          twmax = amax1(twq(i),twmax)
          IF (pq(i).ge.400.0) THEN
              IF (rhq(i).gt.rhmax) THEN
                  rhmax = rhq(i)
                  k2 = i
              END IF
C
              IF (i.ne.1) THEN
                  IF (rhq(i).ge.rhprcp.or.toodry.eq.0) THEN
                      IF (toodry.ne.0) THEN
                          dpdrh = alog(pq(i)/pq(i-1)) / (rhq(i)-
     +                        rhq(i-1))
                          pbot = exp(alog(pq(i))+(rhprcp-rhq(i))*dpdrh)
C
                          ptw = pq(i)
                          toodry = 0
                      ELSE IF (rhq(i).ge.rhprcp) THEN
                          ptw = pq(i)
                      ELSE
                          toodry = 1
                          dpdrh = alog(pq(i)/pq(i-1)) / (rhq(i)-
     +                        rhq(i-1))
                          ptw = exp(alog(pq(i))+(rhprcp-rhq(i))*dpdrh)
C
                      END IF
C
                      IF (pbot/ptw.ge.deltag) THEN
                          k1 = i
                          ptop = ptw
                      END IF
                  END IF
              END IF
          END IF
C
   10 CONTINUE

C     Gross checks for liquid and solid precip which dont require generating level.
C
      IF (twq(1).ge.273.15+2.0) THEN
          ptyp = 8   ! liquid
          icefrac = 0.0
          RETURN
      END IF
C
      IF (twmax.le.twice) THEN
          icefrac = 1.0
          ptyp = 1   !  solid
          RETURN
      END IF
C
C     Check to see if we had no success with locating a generating level.
C
      IF (k1.eq.0) THEN
          RETURN
      END IF
C
      IF (ptop.eq.pq(k1)) THEN
          twtop = twq(k1)
          rhtop = rhq(k1)
          k2 = k1
          k1 = k1 - 1
      ELSE
          k2 = k1
          k1 = k1 - 1
          wgt1 = alog(ptop/pq(k2)) / alog(pq(k1)/pq(k2))
Clin      wgt1=(ptop-Pq(k2))/(Pq(k1)-Pq(k2))
          wgt2 = 1.0 - wgt1
          twtop = twq(k1) * wgt1 + twq(k2) * wgt2
          rhtop = rhq(k1) * wgt1 + rhq(k2) * wgt2
      END IF
C
C     Calculate temp and wet-bulb ranges below precip generating level.
      DO 20 i = 1, k1
          twmax = amax1(twq(i),twmax)
   20 CONTINUE
C
C     Gross check for solid precip, initialize ice fraction.
      IF (twtop.le.twice) THEN
          icefrac = 1.0
          IF (twmax.le.twmelt) THEN     ! gross check for solid precip.
              ptyp = 1       !   solid precip
              RETURN
          END IF
          lll = 0
      ELSE
          icefrac = 0.0
          lll = 1
      END IF
C
C     Loop downward through sounding from highest precip generating level.
   30 CONTINUE
C
      IF (icefrac.ge.1.0) THEN  !  starting as all ice
          IF (twq(k1).lt.twmelt) GO TO 40       ! cannot commence melting
          IF (twq(k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(k1)) / (twtop-twq(k1))
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dtavg = (twmelt-twq(k1)) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin   dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
      ELSE IF (icefrac.le.0.0) THEN     !  starting as all liquid
          lll = 1
C         If (Twq(k1).le.Twice) icefrac=1.0 ! autoconvert
C         Goto 1020
          IF (twq(k1).gt.twice) GO TO 40        ! cannot commence freezing
          IF (twq(k1).eq.twtop) THEN
              wgt1 = 0.5
          ELSE
              wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
          END IF
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dtavg = twmelt - (twq(k1)+twice) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
      ELSE IF ((twq(k1).le.twmelt).and.(twq(k1).lt.twmelt)) THEN       ! mix
          rhavg = (rhq(k1)+rhtop) / 2
          dtavg = twmelt - (twq(k1)+twtop) / 2
          dpk = alog(pq(k1)/ptop)       !lin   dpk=Pq(k1)-Ptop
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
           
      ELSE      ! mix where Tw curve crosses twmelt in layer
          IF (twq(k1).eq.twtop) GO TO 40        ! both equal twmelt, nothing h
          wgt1 = (twmelt-twq(k1)) / (twtop-twq(k1))
          wgt2 = 1.0 - wgt1
          rhavg = rhtop + wgt2 * (rhq(k1)-rhtop) / 2
          dtavg = (twmelt-twtop) / 2
          dpk = wgt2 * alog(pq(k1)/ptop)        !lin   dpk=wgt2*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
          icefrac = amin1(1.0,amax1(icefrac,0.0))
          IF (icefrac.le.0.0) THEN
C             If (Twq(k1).le.Twice) icefrac=1.0 ! autoconvert
C             Goto 1020
              IF (twq(k1).gt.twice) GO TO 40    ! cannot commence freezin
              wgt1 = (twice-twq(k1)) / (twtop-twq(k1))
              dtavg = twmelt - (twq(k1)+twice) / 2
          ELSE
              dtavg = (twmelt-twq(k1)) / 2
          END IF
          rhavg = rhq(k1) + wgt1 * (rhtop-rhq(k1)) / 2
          dpk = wgt1 * alog(pq(k1)/ptop)        !lin  dpk=wgt1*(Pq(k1)-Ptop)
C         mye=emelt*(1.0-(1.0-Rhavg)*efac)
          mye = emelt * rhavg ** efac
          icefrac = icefrac + dpk * dtavg / mye
      END IF
C
      icefrac = amin1(1.0,amax1(icefrac,0.0))
C
C     Get next level down if there is one, loop back.
   40 IF (k1.gt.1) THEN
          twtop = twq(k1)
          ptop = pq(k1)
          rhtop = rhq(k1)
          k1 = k1 - 1
          GO TO 30
      END IF
C
C
C     Determine precip type based on snow fraction and surface wet-bulb.
C
C
      IF (icefrac.ge.slim) THEN
          IF (lll.ne.0) THEN
              ptyp = 2       ! Ice Pellets   JC 9/16/99
          ELSE
              ptyp = 1       !  Snow
          END IF
      ELSE IF (icefrac.le.rlim) THEN
          IF (twq(1).lt.tz) THEN
              ptyp = 4       !  Freezing Precip
          ELSE
              ptyp = 8       !  Rain
          END IF
      ELSE
          IF (twq(1).lt.tz) THEN
cGSM not sure what to do when 'mix' is predicted;  I chose sleet as
cGSM      a shaky best option.  The use of dominant type should
CGSM      help with these situation.  Future users may want to
CGSM      make the type 0 in this situation and leave the decision
CGSM      up to the other algorithms.
      
              ptyp = 2       !  Ice Pellets 
c              ptyp = 5       !  Mix
          ELSE
c              ptyp = 5       !  Mix
              ptyp = 2       !  Ice Pellets
          END IF
      END IF
      RETURN
C
      END
C
      REAL*4 FUNCTION esat(t)
C
C*  Calculates saturation vapor pressure in millibars as a function of
C*  either Kelvin of Celcius temperature.
C
      IMPLICIT NONE
C
      REAL*4 t, k
C
      REAL*4 flag, flg
      COMMON /flagflg/ flag, flg
C
C  Account for both Celsius and Kelvin.
      k = t
      IF (k.lt.100.) k = k + 273.15
C     
C     Flag ridiculous values.
      IF (k.lt.0.0.or.k.gt.373.15) THEN
          esat = flag
          RETURN
      END IF
C     
C     Avoid floating underflow.
      IF (k.lt.173.15) THEN
          esat = 3.777647E-05
          RETURN
      END IF
C     
C     Calculation for normal range of values.
      esat = exp(26.660820-0.0091379024*k-6106.3960/k)
C     
      RETURN
      END
C
      REAL*4 FUNCTION tdofesat(es)
C
C*  As a function of saturation vapor pressure in millibars, returns
C*  dewpoint in degrees K.
C
      IMPLICIT NONE
C
      REAL*4 es, lim1, lim2, b
C
      DATA lim1, lim2 /3.777647E-05, 980.5386/
C
      REAL*4 flag, flg
      COMMON /flagflg/ flag, flg
C
C  Flag ridiculous values.
      IF (es.lt.0.0.or.es.gt.lim2) THEN
          tdofesat = flag
          RETURN
      END IF
C     
C     Avoid floating underflow.
      IF (es.lt.lim1) THEN
          tdofesat = 173.15
          RETURN
      END IF
C     
C     Calculations for normal range of values.
      b = 26.66082 - alog(es)
      tdofesat = (b-sqrt(b*b-223.1986)) / 0.0182758048
C     
      RETURN
      END
C
c      REAL*4 FUNCTION mytw(t,td,p)
      FUNCTION xmytw(t,td,p)
C
      IMPLICIT NONE
C
      INTEGER*4 cflag, l
      REAL*4 f, c0, c1, c2, k, kd, kw, ew, t, td, p, ed, fp, s,
     *          de, xmytw
      DATA f, c0, c1, c2 /0.0006355, 26.66082, 0.0091379024, 6106.3960/
C
C
      xmytw = (t+td) / 2
      IF (td.ge.t) RETURN
C
      IF (t.lt.100.0) THEN
          k = t + 273.15
          kd = td + 273.15
          IF (kd.ge.k) RETURN
          cflag = 1
      ELSE
          k = t
          kd = td
          cflag = 0
      END IF
C
      ed = c0 - c1 * kd - c2 / kd
      IF (ed.lt.-14.0.or.ed.gt.7.0) RETURN
      ed = exp(ed)
      ew = c0 - c1 * k - c2 / k
      IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
      ew = exp(ew)
      fp = p * f
      s = (ew-ed) / (k-kd)
      kw = (k*fp+kd*s) / (fp+s)
C
      DO 10 l = 1, 5
          ew = c0 - c1 * kw - c2 / kw
          IF (ew.lt.-14.0.or.ew.gt.7.0) RETURN
          ew = exp(ew)
          de = fp * (k-kw) + ed - ew
          IF (abs(de/ew).lt.1E-5) GO TO 20
          s = ew * (c1-c2/(kw*kw)) - fp
          kw = kw - de / s
   10 CONTINUE
   20 CONTINUE
C
      IF (cflag.ne.0) THEN
          xmytw = kw - 273.15
      ELSE
          xmytw = kw
      END IF
C
      RETURN
      END
