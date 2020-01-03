      SUBROUTINE NENUBD(NEMO,NUMB,LUN)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    NENUBD
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE CHECKS A MNEMONIC AND FXY VALUE PAIR THAT
C   WERE READ FROM A USER-SUPPLIED BUFR DICTIONARY TABLE IN CHARACTER
C   FORMAT, IN ORDER TO MAKE SURE THAT NEITHER VALUE HAS ALREADY BEEN
C   DEFINED WITHIN INTERNAL BUFR TABLE B OR D (IN COMMON BLOCK
C   /TABABD/) FOR THE GIVEN LUN.  IF EITHER VALUE HAS ALREADY BEEN
C   DEFINED FOR THIS LUN, THEN AN APPROPRIATE CALL IS MADE TO
C   BUFR ARCHIVE LIBRARY SUBROUTINE BORT.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR (ENTRY POINT IN NENUCK)
C 1995-06-28  J. WOOLLEN -- INCREASED THE SIZE OF INTERNAL BUFR TABLE
C                           ARRAYS IN ORDER TO HANDLE BIGGER FILES
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT" (IN PARENT ROUTINE NENUCK)
C 1999-11-18  J. WOOLLEN -- THE NUMBER OF BUFR FILES WHICH CAN BE
C                           OPENED AT ONE TIME INCREASED FROM 10 TO 32
C                           (NECESSARY IN ORDER TO PROCESS MULTIPLE
C                           BUFR FILES UNDER THE MPI) (IN PARENT
C                           ROUTINE NENUCK)
C 2002-05-14  J. WOOLLEN -- CHANGED FROM AN ENTRY POINT TO INCREASE
C                           PORTABILITY TO OTHER PLATFORMS (NENUCK WAS
C                           THEN REMOVED BECAUSE IT WAS JUST A DUMMY
C                           ROUTINE WITH ENTRIES)
C 2003-11-04  J. ATOR    -- ADDED DOCUMENTATION
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED HISTORY
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C
C USAGE:    CALL NENUBD (NEMO, NUMB, LUN)
C   INPUT ARGUMENT LIST:
C     NEMO     - CHARACTER*8: MNEMONIC
C     NUMB     - CHARACTER*6: FXY VALUE ASSOCIATED WITH NEMO
C     LUN      - INTEGER: I/O STREAM INDEX INTO INTERNAL MEMORY ARRAYS
C
C    THIS ROUTINE CALLS:        BORT
C    THIS ROUTINE IS CALLED BY: STBFDX   STNTBI
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /TABABD/ NTBA(0:NFILES),NTBB(0:NFILES),NTBD(0:NFILES),
     .                MTAB(MAXTBA,NFILES),IDNA(MAXTBA,NFILES,2),
     .                IDNB(MAXTBB,NFILES),IDND(MAXTBD,NFILES),
     .                TABA(MAXTBA,NFILES),TABB(MAXTBB,NFILES),
     .                TABD(MAXTBD,NFILES)

      CHARACTER*600 TABD
      CHARACTER*128 BORT_STR
      CHARACTER*128 TABB
      CHARACTER*128 TABA
      CHARACTER*8   NEMO
      CHARACTER*6   NUMB

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C  CHECK TABLE B AND D
C  -------------------

      DO N=1,NTBB(LUN)
      IF(NUMB.EQ.TABB(N,LUN)(1: 6)) GOTO 900
      IF(NEMO.EQ.TABB(N,LUN)(7:14)) GOTO 901
      ENDDO

      DO N=1,NTBD(LUN)
      IF(NUMB.EQ.TABD(N,LUN)(1: 6)) GOTO 902
      IF(NEMO.EQ.TABD(N,LUN)(7:14)) GOTO 903
      ENDDO

C  EXITS
C  -----

      RETURN
900   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
901   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE B MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
902   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D FXY VALUE (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NUMB
      CALL BORT(BORT_STR)
903   WRITE(BORT_STR,'("BUFRLIB: NENUBD - TABLE D MNEMONIC (",A,") '//
     . 'HAS ALREADY BEEN DEFINED (DUPLICATE)")') NEMO
      CALL BORT(BORT_STR)
      END
