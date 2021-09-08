
      FUNCTION VAP(TD)
C
C     ...Calculates vapor pressure...
c        from dew point (C)
C
      VAP = 6.1078*EXP(17.2694*TD/(TD+237.3))
      RETURN
      END
