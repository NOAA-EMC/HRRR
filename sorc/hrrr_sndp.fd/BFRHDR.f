	SUBROUTINE BFRHDR ( luntbl, cseqn, prfflg, clist, np, iret )
C************************************************************************
C* BFRHDR								*
C*									*
C* This subroutine reads a Jack Woollen BUFR encoding table file to	*
C* get the string of parameters to be written.  This subroutine is	*
C* given the sequence nmemonic and returns the list associated with it. *
C* This list is a character string and is used as the last input to 	*
C* UFBINT.								*
C*									*
C*									*
C* BFRHDR ( LUNTBL, CSEQN, PRFFLG, CLIST, NP, IRET )			*
C*									*
C* Input parameters:							*
C*	LUNTBL		INTEGER		Unit number of BUFR Table file	*
C*	CSEQN		CHAR*		Sequence mnemonic		*
C*	PRFFLG		LOGICAL		Flag for profile parms		*
C*					  = .true. for multi-level parms*
C*									*
C* Output parameters:							*
C*	CLIST		CHAR*		String of parm names		*
C*	NP		INTEGER		Number of parm names in string  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = Improper table file	*
C*					 -2 = Sequence not found	*
C*					 -3 = CLIST too short		*
C**									*
C* Log:									*
C* K. Brill/NMC		05/94						*
C* K. BRill/EMC		 2/99	Sequence continuation line parsing	*
C************************************************************************
C*
	CHARACTER*(*)		cseqn, clist
	LOGICAL			prfflg
C*
	LOGICAL			found, done
	CHARACTER*256		sbuf
C
C*	Set starting column number of parameter list in the table.
C
C-----------------------------------------------------------------------
	iret = 0
	istart = 14
	lenmax = LEN ( clist )
C
C*	Count the number of lines to end of file (used to reposition
C*	pointer to original line at the end).
C
	found = .true.
	lcnt = 1
	DO WHILE ( found )
	    READ ( luntbl, 1000, IOSTAT=ios ) sbuf
1000	    FORMAT (A)
	    IF ( ios .ne. 0 ) THEN
		found = .false.
	    ELSE
		lcnt = lcnt + 1
	    END IF
	END DO
C
C*	Read from the file for positioning.
C
	REWIND luntbl
	found = .false.
	DO WHILE ( .not. found )
	    READ (luntbl, 1000, IOSTAT=ios ) sbuf
	    IF ( ios .ne. 0 ) THEN
		iret = -1
		RETURN
	    END IF
	    iq1 = INDEX ( sbuf, '| SEQUENCE' )
	    iq2 = INDEX ( sbuf, '| MNEMONIC' )
	    iq = iq1 * iq2
	    IF ( iq .ne. 0 ) found = .true.
	END DO
C
C*	Get length of sequence mnemonic string.
C
	lc = LEN ( cseqn )
	DO WHILE ( cseqn ( lc:lc ) .eq. ' ' )
	    lc = lc-1
	END DO
C
C*	Start searching foreward for the sequence mnemonic.
C
	clist = ' '
	found = .false.
	done = .false.
	ib = 1
	lenc = 0
	DO WHILE ( .not. done )
	    READ ( luntbl, 1000, IOSTAT=ios ) sbuf
	    IF ( ios .ne. 0 ) THEN
		iret = -2
		RETURN
	    END IF
	    iq = INDEX ( sbuf ( 1:14 ), cseqn ( 1:lc ) )
	    IF ( iq .ne. 0 ) THEN
		found = .true.
C
C*		Find the last character of last parameter.
C
		i = 79
		DO WHILE ( sbuf ( i:i ) .eq. ' ' )
		    i = i - 1
		END DO
		IF ( lenc .gt. 0 ) THEN
		    lenc = lenc + 1
		    IF ( lenc .gt. lenmax ) THEN
			iret = -3
			RETURN
		    END IF
		    clist (lenc:lenc) = ' '
		    ib = ib + 1
		END IF
		lenc = lenc + i - istart + 1
		IF ( lenc .gt. lenmax ) THEN
		    iret = -3
		    RETURN
		END IF
		clist (ib: ) = sbuf ( istart:i )
		ib = lenc + 1
	    ELSE IF ( found ) THEN
		done = .true.
	    END IF
	END DO
C
C*	Count the number of entries in CLIST.
C
	np = 0
	nspcs = 0
	DO j = 1, lenc
	    IF ( clist ( j:j ) .eq. ' ' ) nspcs = nspcs + 1
	END DO
	IF ( nspcs .gt. 0 ) np = nspcs + 1
C
C*	Handle profile sequence.
C
	IF ( prfflg .and. found ) THEN
c	    sbuf = cseqn ( 1:lc ) // '^ ' // clist ( 1:lenc )
            sbuf = clist(1:lenc)
	    clist = sbuf
	END IF
C
C*	Reposition file to original record.
C
	found = .true.
	DO WHILE ( found )
	    READ ( luntbl, 1000, IOSTAT=ios ) sbuf
	    IF ( ios .ne. 0 ) found = .false.
	END DO
	DO k = 1, lcnt
	    BACKSPACE luntbl
	END DO
C*
	RETURN
	END
