      SUBROUTINE GET_BITS(IBM,SGDS,LEN,MG,G,ISCALE,GROUND,
     &                    GMIN,GMAX,NBIT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GET_BITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
C   PRGMMR: IREDELL          ORG: W/NP23     DATE: 92-10-31
C
C ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
C   AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
C   THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
C   THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
C   GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-04-14  BALDWIN - MODIFY FOLLOWING KEITH BRILL'S CODE
C                       TO USE SIG DIGITS TO COMPUTE DEC SCALE
C
C USAGE:   CALL GET_BITS(IBM,ISGDS,LEN,MG,G,ISCALE,GROUND,GMIN,GMAX,NBIT)
C   INPUT ARGUMENT LIST:
C     IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
C     SGDS     - MAXIMUM SIGNIFICANT DIGITS TO KEEP
C                (E.G. SGDS=3.0 KEEPS 3 SIGNIFICANT DIGITS)
C                OR BINARY PRECISION IF <0
C                (E.G. SGDS=-2.0 KEEPS FIELD TO NEAREST 1/4
C                           -3.0 "                    " 1/8
C                         2**SGDS PRECISION)
C     LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
C     MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
C     G        - REAL (LEN) FIELD
C
C   OUTPUT ARGUMENT LIST:
C     ISCALE   - INTEGER DECIMAL SCALING
C     GROUND   - REAL (LEN) FIELD ROUNDED TO DECIMAL SCALING
C     GMIN     - REAL MINIMUM VALID ROUNDED FIELD VALUE
C     GMAX     - REAL MAXIMUM VALID ROUNDED FIELD VALUE
C     NBIT     - INTEGER NUMBER OF BITS TO PACK
C
C SUBPROGRAMS CALLED:
C   ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE EXTREMES WHERE BITMAP IS ON
C
      IF(IBM.EQ.0) THEN
        GMAX=G(1)
        GMIN=G(1)
        DO I=2,LEN
          GMAX=MAX(GMAX,G(I))
          GMIN=MIN(GMIN,G(I))
        ENDDO
      ELSE
        I1=0
        DO I=1,LEN
          IF(MG(I).NE.0.AND.I1.EQ.0) I1=I
        ENDDO
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          GMAX=G(I1)
          GMIN=G(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GMAX=MAX(GMAX,G(I))
              GMIN=MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
C
C
C
      CALL FNDBIT  ( GMIN, GMAX, SGDS, NBIT, ISCALE, IRETT)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
	SUBROUTINE FNDBIT  ( rmin, rmax, rdb, nmbts, iscale, iret )
C************************************************************************
C* FNDBIT								*
C*									*
C* This subroutine computes the number of packing bits given the	*
C* maximum number of significant digits to preserve or the binary	*
C* precision to store the data.  The binary precision is given as a	*
C* negative integer, ISCALE will always be zero in this case.		*
C*									*
C* The binary precision translates as follows:				*
C*     -1  =>  store data to nearest 1/2				*
C*     -2  =>  store data to nearest 1/4				*
C*     -3  =>  store data to nearest 1/8				*
C*									*
C* Note that a fractional number of significant digits is allowed.	*
C*									*
C* FNDBIT ( AMIN, AMAX, RDB, NBITS, ISCALE, IRET )			*
C*									*
C* Input parameters:							*
C*	AMIN 		REAL		Minimum value			*
C*	AMAX		REAL		Maximum value			*
C*	RDB		REAL		Maximum # of significant digits	*
C*					  OR binary precision if < 0	*
C*									*
C* Output parameters:							*
C*	NBITS		INTEGER		Number of bits for packing	*
C*	ISCALE		INTEGER		Power of 10 scaling to use	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/EMC		12/95	Added binary precision			*
C* M. Baldwin           10/96   Added fix for negative nmbts
C************************************************************************
C*
	DATA		rln2/0.69314718/
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF
C*
	IF ( rdb .eq. 0.0 ) THEN
	    nmbts = 8
	    RETURN
	ELSE IF ( rdb .gt. 0.0 ) THEN
	    ipo = INT (ALOG10 ( range ))
	    IF ( range .lt. 1.00 ) ipo = ipo - 1
	    po = float(ipo) - rdb + 1.
	    iscale = - INT ( po )
	    rr = range * 10. ** ( -po )
	    nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
	ELSE
	    ibin = NINT ( -rdb )
	    rng2 = range * 2. ** ibin
	    nmbts = INT ( ALOG ( rng2 ) / rln2 ) + 1
	END IF
C*
        IF(NMBTS.LE.0) THEN
          NMBTS=0
          IF(ABS(RMIN).GE.1.) THEN
            ISCALE=-INT(ALOG10(ABS(RMIN)))
          ELSE IF (ABS(RMIN).LT.1.0.AND.ABS(RMIN).GT.0.0) THEN
            ISCALE=-INT(ALOG10(ABS(RMIN)))+1
          ELSE
            ISCALE=0
          ENDIF
        ENDIF
	RETURN
	END
