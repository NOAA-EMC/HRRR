!############################# Change Log ##################################
! 2.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################


MODULE mod_dateutils

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: julday ! Function

CONTAINS

  INTEGER FUNCTION julday (imonth,iday,iyear)
    
    INTEGER, INTENT(IN) :: imonth
    INTEGER, INTENT(IN) :: iday
    INTEGER, INTENT(IN) :: iyear

    ! compute the julian day from a normal date

    julday= iday  &
         + MIN(1,MAX(0,imonth-1))*31  &
         + MIN(1,MAX(0,imonth-2))*(28+(1-MIN(1,MOD(iyear,4))))  &
         + MIN(1,MAX(0,imonth-3))*31  &
         + MIN(1,MAX(0,imonth-4))*30  &
         + MIN(1,MAX(0,imonth-5))*31  &
         + MIN(1,MAX(0,imonth-6))*30  &
         + MIN(1,MAX(0,imonth-7))*31  &
         + MIN(1,MAX(0,imonth-8))*31  &
         + MIN(1,MAX(0,imonth-9))*30  &
         + MIN(1,MAX(0,imonth-10))*31  &
         + MIN(1,MAX(0,imonth-11))*30  &
         + MIN(1,MAX(0,imonth-12))*31

    RETURN
  END FUNCTION julday

END MODULE mod_dateutils




























!***************************************************************************

INTEGER FUNCTION julday (imonth,iday,iyear)
  IMPLICIT NONE
  INTEGER :: imonth,iday,iyear

  ! compute the julian day from a normal date

  julday= iday  &
       + MIN(1,MAX(0,imonth-1))*31  &
       + MIN(1,MAX(0,imonth-2))*(28+(1-MIN(1,MOD(iyear,4))))  &
       + MIN(1,MAX(0,imonth-3))*31  &
       + MIN(1,MAX(0,imonth-4))*30  &
       + MIN(1,MAX(0,imonth-5))*31  &
       + MIN(1,MAX(0,imonth-6))*30  &
       + MIN(1,MAX(0,imonth-7))*31  &
       + MIN(1,MAX(0,imonth-8))*31  &
       + MIN(1,MAX(0,imonth-9))*30  &
       + MIN(1,MAX(0,imonth-10))*31  &
       + MIN(1,MAX(0,imonth-11))*30  &
       + MIN(1,MAX(0,imonth-12))*31

  RETURN
END FUNCTION julday

!***************************************************************************

SUBROUTINE date_abs_secs (indate1,seconds)
  IMPLICIT NONE
  CHARACTER(len=14) :: indate1
  REAL(kind=8) :: seconds

  ! compute number of seconds past 1 January 1900 12:00 am

  REAL(kind=8) :: s1,s2,s3,s4
  INTEGER :: year1,month1,date1,hour1,iy,ndays
  INTEGER, EXTERNAL :: julday

  CALL date_unmake_big(year1,month1,date1,hour1,indate1)

  iy = year1 - 1900
  ndays = iy * 365 + (iy-1)/4 + julday(month1,date1,iy)
  s1= DBLE(ndays) *86400.
  s2= DBLE(hour1/10000)*3600.
  s3= DBLE(MOD(hour1,10000)/100)*60.
  s4= DBLE(MOD(hour1,100))
  seconds= s1+s2+s3+s4

  RETURN
END SUBROUTINE date_abs_secs

!***************************************************************************

SUBROUTINE date_abs_secs2 (year1,month1,date1,hour1,seconds)
  IMPLICIT NONE
  REAL(kind=8) :: seconds

  ! compute number of seconds past 1 January 1900 12:00 am

  REAL(kind=8) :: s1,s2,s3,s4
  INTEGER :: year1,month1,date1,hour1,iy,ndays
  INTEGER, EXTERNAL :: julday

  iy = year1 - 1900
  ndays = iy * 365 + (iy-1)/4 + julday(month1,date1,iy)
  s1= DBLE(ndays) *86400.
  s2= DBLE(hour1/10000)*3600.
  s3= DBLE(MOD(hour1,10000)/100)*60.
  s4= DBLE(MOD(hour1,100))
  seconds= s1+s2+s3+s4

  RETURN
END SUBROUTINE date_abs_secs2

!***************************************************************************

SUBROUTINE date_subtract (indate1,indate2,tinc,tunits)
  IMPLICIT NONE
  REAL :: tinc
  CHARACTER(len=1) :: tunits
  CHARACTER(len=14) :: indate1, indate2

  ! add (or subracts) a time increment to a date and output new date
  ! -> uses hhmmss for hours, 4 digit year

  INTEGER :: mondays(12)
  DATA mondays/31,28,31,30,31,30,31,31,30,31,30,31/
  REAL(kind=8) :: secs1,secs2
  REAL :: ttinc

  CALL date_abs_secs(indate1,secs1)
  CALL date_abs_secs(indate2,secs2)
  !print*,'sub:',indate1,indate2,secs1,secs2

  ! convert time to requested unit

  ttinc=secs2-secs1
  IF(tunits.EQ.'s') tinc=ttinc
  IF(tunits.EQ.'m') tinc=ttinc/60.
  IF(tunits.EQ.'h') tinc=ttinc/3600.
  IF(tunits.EQ.'d') tinc=ttinc/86400.
  !print*,'sub:',secs1,secs2,tinc,tinc

  RETURN
END SUBROUTINE date_subtract

!***************************************************************************

SUBROUTINE date_secs_ymdt (seconds,iyear1,imonth1,idate1,ihour1)
  IMPLICIT NONE
  REAL(kind=8) :: seconds,s1
  INTEGER :: iyear1,imonth1,idate1,ihour1

  ! compute real time given number of seconds past 1 January 1900 12:00 am  

  INTEGER :: ny,nyr,ileap,nm,ihr,imn,isc

  INTEGER :: mondays(12)
  DATA mondays/31,28,31,30,31,30,31,31,30,31,30,31/

  ! Get what year it is
  s1=seconds
  DO ny=0,10000
     ileap=0
     IF(MOD(1900+ny,4) == 0) ileap=1
     s1=s1-(365.+ileap)*86400.
     IF(s1 < 0.) THEN
        nyr=ny
        s1=s1+(365.+ileap)*86400.
        EXIT
     ENDIF
  ENDDO
  iyear1=1900+nyr

  ! s1 is now number of secs into the year
  !   Get month
  DO nm=1,12
     ileap=0
     IF(MOD(1900+ny,4) == 0 .AND. nm == 2) ileap=1
     s1=s1-(mondays(nm)+ileap)*86400.
     IF(s1 < 0.) THEN
        s1=s1+(mondays(nm)+ileap)*86400.
        EXIT
     ENDIF
  ENDDO
  imonth1=nm

  ! s1 is now number of secs into the month
  !   Get date and time

  idate1=INT(s1/86400.)
  s1=s1-idate1*86400.
  idate1=idate1+1 ! Since date starts at 1

  ihr=INT(s1/3600.)
  s1=s1-ihr*3600.
  imn=INT(s1/60.)
  s1=s1-imn*60.
  isc=s1
  ihour1=ihr*10000+imn*100+isc

  RETURN
END SUBROUTINE date_secs_ymdt
!***************************************************************************

SUBROUTINE date_add_to_big (cindate,tinc,tunits,coutdate)
  IMPLICIT NONE

  CHARACTER(len=14) cindate,coutdate
  REAL :: tinc
  CHARACTER(len=1) :: tunits

  ! adds/subtracts a time increment to a date and output new date
  ! -> uses hhmmss for hours, 4 digit year

  REAL(kind=8) :: ttinc,secs
  INTEGER :: inyear,inmonth,indate,inhour  &
       ,outyear,outmonth,outdate,outhour

  ! convert input time to seconds

  ttinc=tinc
  IF(tunits.EQ.'m') ttinc=tinc*60.
  IF(tunits.EQ.'h') ttinc=tinc*3600.
  IF(tunits.EQ.'d') ttinc=tinc*86400.
  !print*,'inc:',tinc,tunits,ttinc
  !print*,'big:',cindate
  CALL date_unmake_big(inyear,inmonth,indate,inhour,cindate)
  !print*,'big:',inyear,inmonth,indate,inhour
  CALL date_abs_secs2(inyear,inmonth,indate,inhour,secs)

  !print*,'big:',secs,ttinc
  secs=secs+ttinc

  CALL date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)
  !print*,'big:',outyear,outmonth,outdate,outhour
  CALL date_make_big(outyear,outmonth,outdate,outhour,coutdate)

  !print*,'out stuff:',coutdate


  RETURN
END SUBROUTINE date_add_to_big
!***************************************************************************

SUBROUTINE date_add_to (inyear,inmonth,indate,inhour  &
     ,tinc,tunits,outyear,outmonth,outdate,outhour)
  IMPLICIT NONE

  INTEGER inyear,inmonth,indate,inhour  &
       ,outyear,outmonth,outdate,outhour
  REAL tinc
  CHARACTER(len=1) :: tunits

  ! adds/subtracts a time increment to a date and output new date
  ! -> uses hhmmss for hours, 4 digit year


  REAL(kind=8) :: ttinc,secs

  ! convert input time to seconds

  ttinc=tinc
  IF(tunits.EQ.'m') ttinc=tinc*60.
  IF(tunits.EQ.'h') ttinc=tinc*3600.
  IF(tunits.EQ.'d') ttinc=tinc*86400.
  !print*,'inc:',tinc,tunits,ttinc


  CALL date_abs_secs2(inyear,inmonth,indate,inhour,secs)

  secs=secs+ttinc

  CALL date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)

  !print*,'out stuff:',outyear,outmonth,outdate,outhour

  RETURN
END SUBROUTINE date_add_to

!***************************************************************************

SUBROUTINE date_make_big (inyear,inmonth,indate,inhour,outdate)
  IMPLICIT NONE
  INTEGER :: inyear,inmonth,indate,inhour
  CHARACTER(len=14) ::  outdate

  WRITE(outdate(1:4),10) inyear
  WRITE(outdate(5:6),11) inmonth
  WRITE(outdate(7:8),11) indate
  WRITE(outdate(9:14),12) inhour
10 FORMAT (i4.4)
11 FORMAT (i2.2)
12 FORMAT (i6.6)

  RETURN
END SUBROUTINE date_make_big

!***************************************************************************

SUBROUTINE date_unmake_big (inyear,inmonth,indate,inhour,outdate)
  IMPLICIT NONE
  INTEGER :: inyear,inmonth,indate,inhour
  CHARACTER(len=14) :: outdate

  READ(outdate(1:4),10) inyear
  READ(outdate(5:6),11) inmonth
  READ(outdate(7:8),11) indate
  READ(outdate(9:14),12) inhour
10 FORMAT (i4)
11 FORMAT (i2)
12 FORMAT (i6)

  RETURN
END SUBROUTINE date_unmake_big

!***************************************************************************

SUBROUTINE RAMS_dintsort(ni,chnums,cstr)
  IMPLICIT NONE
  INTEGER :: ni
  CHARACTER(len=14) :: chnums(*)
  CHARACTER(len=*) :: cstr(*)

  ! sort an array of character strings by an associated character field

  CHARACTER(len=200) :: cscr
  CHARACTER(len=14) :: mini,nscr

  INTEGER :: n,nm,nmm

  DO n=1,ni
     mini='99999999999999'
     DO nm=n,ni
        IF(chnums(nm).LT.mini) THEN
           nmm=nm
           mini=chnums(nm)
        ENDIF
     ENDDO
     nscr=chnums(n)
     chnums(n)=chnums(nmm)
     chnums(nmm)=nscr
     cscr=cstr(n)
     cstr(n)=cstr(nmm)
     cstr(nmm)=cscr
  ENDDO

  RETURN
END SUBROUTINE RAMS_dintsort

!***************************************************************************

SUBROUTINE RAMS_sort_dint3 (n1,ia1,n2,ia2,n3,ia3,nt,IALL)
  IMPLICIT NONE
  INTEGER :: n1,n2,n3,nt
  CHARACTER(len=14) :: ia1(*),ia2(*),ia3(*),IALL(*)

  !     sort 3 arrays of char's, put back in 1 array
  !     copy all to output array

  CHARACTER(len=14) :: mini,nscr
  INTEGER :: n,nm,nmm

  nt=0
  DO n=1,n1
     nt=nt+1
     IALL(nt)=ia1(n)
  ENDDO
  DO n=1,n2
     nt=nt+1
     IALL(nt)=ia2(n)
  ENDDO
  DO n=1,n3
     nt=nt+1
     IALL(nt)=ia3(n)
  ENDDO

  DO n=1,nt
     mini='99999999999999'
     DO nm=n,nt
        IF(IALL(nm).LT.mini) THEN
           nmm=nm
           mini=IALL(nm)
        ENDIF
     ENDDO
     nscr=IALL(n)
     IALL(n)=IALL(nmm)
     IALL(nmm)=nscr
  ENDDO

  RETURN
END SUBROUTINE RAMS_sort_dint3

!***************************************************************************

SUBROUTINE RAMS_unique_dint (n1,ia1)
  IMPLICIT NONE
  INTEGER :: n1
  CHARACTER(len=14) :: ia1(*)

  INTEGER :: n,nt,nn

  ! reduce an array to get rid of duplicate entries


  nt=n1
10 CONTINUE
  DO n=2,nt
     IF(ia1(n).EQ.ia1(n-1)) THEN
        DO nn=n,nt
           ia1(nn-1)=ia1(nn)
        ENDDO
        nt=nt-1
        GOTO 10
     ENDIF
  ENDDO
  n1=nt

  RETURN
END SUBROUTINE RAMS_unique_dint


!************************************************************************

INTEGER FUNCTION julday1970 (imonth,iday,iyear)
  IMPLICIT NONE
  INTEGER :: imonth,iday,iyear

  INTEGER :: i,imm,idd,jd

  ! compute the julian day (from 1970) from a normal date w/4 digit yr

  julday1970=0
  DO i=1970,iyear

     imm=12
     idd=31
     IF(i==iyear)THEN
        imm=imonth
        idd=iday
     ENDIF

     jd= idd  &
          + MIN(1,MAX(0,imm-1))*31  &
          + MIN(1,MAX(0,imm-2))*(28+(1-MIN(1,MOD(i,4))))  &
          + MIN(1,MAX(0,imm-3))*31  &
          + MIN(1,MAX(0,imm-4))*30  &
          + MIN(1,MAX(0,imm-5))*31  &
          + MIN(1,MAX(0,imm-6))*30  &
          + MIN(1,MAX(0,imm-7))*31  &
          + MIN(1,MAX(0,imm-8))*31  &
          + MIN(1,MAX(0,imm-9))*30  &
          + MIN(1,MAX(0,imm-10))*31  &
          + MIN(1,MAX(0,imm-11))*30  &
          + MIN(1,MAX(0,imm-12))*31

     julday1970=julday1970+jd

  ENDDO

  RETURN
END FUNCTION julday1970
