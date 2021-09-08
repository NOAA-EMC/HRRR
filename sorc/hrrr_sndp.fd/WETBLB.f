
      SUBROUTINE WETBLB(TEMP1,TD1,PRESS1,LVL,TEMPW)
C
C  COMPUTE WET BULB TEMPS FOR PRESSURES ABOVE 500MB
C
C  SAM CONTORNO ??/??/91
C  MIKE BALDWIN 10/13/94  : CONVERTED FOR USE IN CALWXT 1D VERSION
C 
C  INPUT:
C    LVL    - NUMBER OF LEVELS
C    TEMP1  - TEMP (K)
C    TD1    - DEW POINT (K)
C    PRESS1 - PRESSURE (Pa)
C  OUTPUT:
C    TEMPW  - WET BULB (K)
C  INTERNAL:
C    TEMP   - TEMP (C)
C    TD     - DEW POINT (C)
C    PRESS  - PRESSURE (MB)
C
      DIMENSION TEMP1(99),PRESS1(99),TD1(99)
      DIMENSION TEMP(99),TEMPW(99),PRESS(99),TD(99)
C
      do i=1,lvl
       temp(i) = temp1(i) - 273.15
       td(i)   = td1(i)   - 273.15
       press(i) = press1(i) * 0.01
      enddo
      XL = 2.5E+06
      CP = 1004.
      CPV = 1952.
      DO I=1,LVL
      tempw(i)=temp(i)
      if (press(i).gt.500.) then
C
C     ...DT determined from the moistness (sic) of the layer...
C
      DT = (TEMP(I)-TD(I))*.1
      C = 6.1078
      C1 = 3.8/PRESS(I)
      C2 = 7.5*ALOG(10.)
      EW = VAP(TD(I))
      W = SMIX(EW,PRESS(I))
      D = 17.2694
      tnew = temp(i)
      tdnew = td(i)
      j = 0
C
C     ...Following equation  is derived from diffferential form of
C        Teetjen's equation....  
C   
 10   C3 = ((TDnew+tnew)*.5)/((TDnew+tnew)*.5+237.)
      DW = C1*C2*EXP(C2*C3)*237.*DT/(((TDnew+Tnew)*.5+237.)**2.)
 15   WLAST=W
      TLAST = TNEW
      TDLAST = TDNEW
C
C     ...Adding increments of water to be evaporated...
C
      W = W+DW
      B = W*PRESS(I)/.622
      X = ALOG(B/C)/D
C
C     ...Teetjen's equation solved for temp.  This is the new "dew
C        point" after dw is evaporated...
C
      TDNEW =  237.3*X/(1-X)
C
C     ... Calcs. new temperature after evaporative cooling occurs...
C  
      TNEW = TNEW- DW*XL/(CP+ wlast*CPV)
      IF (ABS(TNEW-TDNEW) .LT. .05) THEN
         TEMPW(I) = (TNEW+TDNEW)*.5
         GOTO 30
      ENDIF
      IF (TDNEW .GT. TNEW) THEN
         j=j+1
         DW= DW*.5
         W=WLAST
         TNEW = TLAST
         TDNEW = TDLAST
         GOTO 15
      ELSE
         if (j.eq.0) then
            GOTO 10
         else
            dw=dw*.5
            goto 15
         endif
      ENDIF
 30   CONTINUE
      endif
      tempw(i) = tempw(i) + 273.15
      ENDDO
      RETURN
      END
