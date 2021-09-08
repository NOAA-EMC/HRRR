
      FUNCTION SMIX(EW,PRESS)
C    
C     ...Calculates mixing ratio...
c        from vapor pressure and pressure  (same units)
C
      SMIX = .622*EW/PRESS
      RETURN
      END
