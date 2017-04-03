  MODULE rdgrib
  use grddef
        USE GRIB_MOD
        USE pdstemplates

!=======================================================================
!  routines to  read/write grib data 
!=======================================================================
     
   REAL,      ALLOCATABLE  :: GRID(:)
   LOGICAL*1, ALLOCATABLE  :: MASK(:)

contains
     SUBROUTINE SETVAR(LUB,LUI,NUMV,J,JPDS,JGDS,KF, K,KPDS,KGDS,MASK,GRID,VARB,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field, GRID, and initializes a 2-D variable,
!     VARB.
!     CALLS: w3lib GETGB routine
!     INPUT 
!        LUB : Input GRIB GRID file unit number
!        LUI : Input GRIB GRID Index file unit number
!        NUMV: Total number of grid points (NX * NY)
!           J: Input record number to skip to 
!        JPDS: Array containing grid variable definition parms.
!              At minimum The following two ids should be defined
!               JPDS(5) = grib variable id  (see grib office note 388, Table 2)
!               JPDS(6) = grib vertical level type (see grib office note 388,
!               Table 3)
!        JGDS: Array containing grid definitions, default set = -1
!     OUTPUT: 
!       GRID, MASK : 1D nx*ny array of grib field and land sea mask
!       VARB       : 2-D grib field variable array (NX,NY)
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================

   REAL,      INTENT(INOUT)  :: GRID(:),VARB(:,:)
   LOGICAL*1, INTENT(INOUT)  :: MASK(:)
!-----------------------------------------------------------------------------------------
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

!     Get GRIB Variable
      
      CALL GETGB(LUB,LUI,NUMV,J,JPDS,JGDS,KF,K,KPDS,KGDS,MASK,GRID,IRET)
      IMAX=KGDS(2)
      IF(IRET.EQ.0) THEN
        DO KK = 1, NUMV
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF
          VARB(M,N) = GRID(KK)
        ENDDO
       IF(JPDS(6).ne.109 .or. JPDS(6).eq.109.and.J.le.100) &
        WRITE(6,100) JPDS(5),JPDS(6),JPDS(7),J,MINVAL(VARB),MAXVAL(VARB)
 100   FORMAT('VARB UNPACKED ', 4I7,2F14.4)
      ELSE
       WRITE(6,*)'====================================================='
       WRITE(6,*)' GETGB ERROR: ',IRET
       WRITE(6,*)'COULD NOT UNPACK VARB FOR J= ',J,'GRID', JPDS(3)
       WRITE(6,*) 'VARB',JPDS(5),'LVL TYP',JPDS(6),'VERT LVL',JPDS(7)
       WRITE(6,*)'UNIT', LUB,LUI,'NUMV',NUMV,'KF',KF
       WRITE(6,*)'====================================================='
       print *,'JPDS',jpds(1:25)
       ISTAT = IRET
! 01-29-13 JTM : past hour 60 nam output only to level 35
       if (JPDS(5).eq.191 .or. JPDS(6).eq.109 .or. JPDS(6).eq.245) then 
         print *, 'GRIB VARB READ ERROR: program continuing'
!       else
!         STOP 'ABORT: GRIB VARB READ ERROR'
       endif
      ENDIF

      RETURN
      END SUBROUTINE setvar

     SUBROUTINE SETVAR_g2(LUB,LUI,NUMV,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF, &
                                       K,KPDS,KGDS,MASK,GRID,VARB,GFLD,SREF,IRET,ISTAT)
!============================================================================
!     This Routine reads in a grib field and initializes a 2-D variable
!     Requested from w3lib GETGRB routine
!     10-2012   Jeff McQueen
!     NOTE: ONLY WORKS for REAL Type Variables
!============================================================================

   REAL,      INTENT(INOUT)  :: GRID(:),VARB(:,:)
   LOGICAL*1, INTENT(INOUT)  :: MASK(:)
!-----------------------------------------------------------------------------------------
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
        INTEGER :: KK, NUMV
        INTEGER :: M, N, ISTAT, IMAX, KF
        INTEGER, INTENT(IN) :: SREF

! C grib2
      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET
      TYPE(GRIBFIELD) :: GFLD
! C grib2

!     Get GRIB Variable

!!        J=0
        UNPACK=.TRUE.
        K=0
!        print*,'IRET',IRET
        call getgb2(LUB,LUI,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
          UNPACK,K,GFLD,IRET)

        if (IRET .ne. 0) then
         write(0,*) 'IRET from getgb2: ', IRET
!        STOP
        endif

        if (IRET .eq. 0) then
        IMAX=gfld%igdtmpl(8)
        endif

      IF(IRET.EQ.0) THEN

        DO KK = 1, NUMV
          IF(MOD(KK,IMAX).EQ.0) THEN
            M=IMAX
            N=INT(KK/IMAX)
          ELSE
            M=MOD(KK,IMAX)
            N=INT(KK/IMAX) + 1
          ENDIF

        if (SREF .eq. 1 .and. gfld%fld(KK) .gt. 100.) then
!          write(0,*) 'ignore as bad SREF value at: ', KK, gfld%fld(KK)
          VARB(M,N) = 0.
        else
          VARB(M,N) = gfld%fld(KK)
        endif

        ENDDO

       IF(JPDT(10).ne.105 .or. JPDT(10).eq.109.and.J.le.50) &
        WRITE(0,100) JPDT(1),JPDT(2),JPDT(10),J,MINVAL(VARB),MAXVAL(VARB)
 100   FORMAT('VARB UNPACKED ', 4I7,2G12.4)
      ELSE
       WRITE(0,*)'====================================================='
       WRITE(0,*)'COULD NOT UNPACK VARB(setvar)',K,JPDT(1),JPDT(2),IRET
       write(0,*) 'JPDT(1:12): ' , JPDT(1:12)
       WRITE(0,*)'UNIT', LUB,LUI,NUMV
       WRITE(0,*)'====================================================='
       ISTAT = IRET
! 01-29-13 JTM : past hour 60 nam output onli to level 35
       if (JPDT(10).ne.105) then
        write(0,*) 'JPDT(1:12): ' , JPDT(1:12)
!               STOP 'ABORT: GRIB VARB READ ERROR'
       endif
      ENDIF

      RETURN
      END SUBROUTINE setvar_g2


      SUBROUTINE RDHDRS(LUB,LUI,IGDN,GDIN,NUMV)
      use grddef
!=============================================================
!     This Routine Reads a GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!     Calls : BAOPEN,GETGI and GETGB1S
!     INPUT: 
!       LUB,LUI --> Input grib file and grib index file unit numbers 
!     OUTPUT: 
!       IGDN    --> Grib Grid domain number  (KPDS(3), see on 388, Table B)
!       GDIN TYPE variable with following grid information defined in module
!       grddef
!       GDIN%NX --> number of X grid points: also KGDS(2)
!       GDIN%NY --> number of Y grid points: also KGDS(3)
!       NUMV    --> Total number of horizontal grid points
!
!     10-2012  Jeff McQueen
!=============================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      PARAMETER(MBUF=2000000)
      CHARACTER CBUF(MBUF)
      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)
      TYPE (GINFO)  ::  GDIN

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
      CALL BAOPENR(LUB,FNAME,IRETGB)
      print *,'BAOPENR',LUB,'IRET ',IRETGB
      if (iretgb.ne.0) then
        print *,' COULD NOT OPEN GRIB FILE UNIT ',LUB
        stop 99
      endif
   
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPENR(LUI,FNAME,IRETGI)
      print *,'BAOPENR',LUI,'IRET ',IRETGI
      if (iretgi.ne.0) print *,'COULD NOT OPEN INDEX FILE UNIT',LUI
      CALL GETGI(LUI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)

      write(6,*)' IRET FROM GETGI ',IRGI,LUB,LUI,NLEN,NNUM
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE '
        ISTAT = IRGI
!TEST        STOP 'ABORT RDHDRS: GRIB INDEX FILE READ ERROR '
      ENDIF


      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
        CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)

        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,280) IGDN,JPDS(4),JPDS(5)
          ISTAT = IRGS
          STOP 'ABORT RDHDRS: GRIB HDR READ ERROR '
        ENDIF
        IGDN = KPDS(3)
        GDIN%IMAX = KGDS(2)
        GDIN%JMAX = KGDS(3)
        NUMV = GDIN%IMAX*GDIN%JMAX
      ENDDO

  280 FORMAT(' IGDN, IMAX,JMAX, ',4I5)
      RETURN
      END SUBROUTINE rdhdrs
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE RDHDRS_g2(LUB,LUI,IGDN,GDIN,NUMV)
      use grddef
!=============================================================
!     This Routine Reads GRIB index file and returns its contents
!     (GETGI)
!     Also reads GRIB index and grib file headers to
!     find a GRIB message and unpack pds/gds parameters (GETGB1S)
!
!     10-2012  Jeff McQueen
!     10-2014  M. Pyle - GRIB2 version
!=============================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)

      CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF

!C  DECLARE INTERFACES (REQUIRED FOR CBUF POINTER)

      INTERFACE
         SUBROUTINE GETIDX(LUGB,LUGI,CBUF,NLEN,NNUM,IRGI)
            CHARACTER(LEN=1),POINTER,DIMENSION(:) :: CBUF
            INTEGER,INTENT(IN) :: LUGB,LUGI
            INTEGER,INTENT(OUT) :: NLEN,NNUM,IRGI
         END SUBROUTINE GETIDX
      END INTERFACE


      CHARACTER*80 FNAME
      INTEGER JENS(200),KENS(200)
      INTEGER :: IRGI, IRGS, JR, KSKIP, IRETGB, IRETGI, NLEN
      INTEGER :: NNUM, ISTAT, IGDN, NUMV

! C grib2
      INTEGER :: LUB,LUI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET,LPOS
      TYPE(GRIBFIELD) :: GFLD
! C grib2

      TYPE (GINFO)  ::  GDIN

!jtm  Input Filename prefix on WCOSS
      FNAME='fort.  '

      IRGI = 1
      IRGS = 1
!TEST 1/27/13      KMAX = 0
      JR=0
      KSKIP = 0

      WRITE(FNAME(6:7),FMT='(I2)')LUB
        write(0,*) 'call baopen: ', lub
      CALL BAOPEN(LUB,FNAME,IRETGB)
        if (IRETGB .ne. 0) then
        write(0,*) 'IRETGB on baopen, LUB: ', IRETGB, LUB
        endif
      WRITE(FNAME(6:7),FMT='(I2)')LUI
      CALL BAOPEN(LUI,FNAME,IRETGI)
        if (IRETGI .ne. 0) then
        write(0,*) 'IRETGI on baopen, LUI: ', IRETGi, LUI
        endif

        write(0,*) 'to getidx call'

        CALL GETIDX(LUB,LUI,CBUF,NLEN,NNUM,IRGI)
        write(0,*) 'NLEN, NNUM from GETIDX: ', NLEN, NNUM
        if(nlen.eq.0.or.nnum.eq.0)return
      IF(IRGI .NE. 0) THEN
        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
        ISTAT = IRGI
!       STOP 'ABORT RDHDRS: GRIB INDEX FILE READ ERROR '
      ENDIF

!!! why retrieve for all records???
        NNUM=1

      DO K = 1, NNUM
        JR = K - 1
        JPDS = -1
        JGDS = -1
!        CALL
!        GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)


        JDISC=-1
        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999

        write(0,*) 'to GETGB2S call: '
        call GETGB2S(CBUF,NLEN,NNUM,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                        JGDT,K,GFLD,LPOS,IRGS)
        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,*) 'IRGS: ', IRGS
        ENDIF

!        if (K .eq. 1) then
!        do N=1,30
!        write(0,*) 'N, gfld%igdtmpl(N): ', N, gfld%igdtmpl(N)
!        enddo
!        endif

        write(0,*) 'gfld%igdtnum: ', gfld%igdtnum
        write(0,*) 'gfld%igdtmpl: ', gfld%igdtmpl
        write(0,*) 'gfld%fld: ', gfld%fld
!        print *,'GDIN',GDIN
!        print *,'GDIN%imax',GDIN%IMAX
!        print *,'GDIN%jmax',GDIN%JMAX

          selectcase( gfld%igdtnum )     !  Template number
           case (0:3)   ! Lat/Lon
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (10)   ! Mercator
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (20)   ! Polar Stereographic
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (30)   ! Lambert Conformal
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (40:43)   ! Gaussian
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (90)   ! Space View/Orthographic
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case (110)   ! Equatorial Azimuthal
              GDIN%IMAX=gfld%igdtmpl(8)
              GDIN%JMAX=gfld%igdtmpl(9)
           case default
              GDIN%IMAX=0
              GDIN%JMAX=0
         end select

!JTM    write(6,*)' IRET FROM GETGB1S ',IRGS,JR
        IF(IRGS .NE. 0) THEN
          WRITE(6,*)' PROBLEMS ON 1ST READ OF GRIB FILE SO ABORT'
          WRITE(6,280) IGDN,JPDS(4),JPDS(5)
          ISTAT = IRGS
          STOP 'ABORT RDHDRS: GRIB HDR READ ERROR '
        ENDIF
!        IGDN = KPDS(3)
!        GDIN%IMAX = KGDS(2)
!        GDIN%JMAX = KGDS(3)
        NUMV = GDIN%IMAX*GDIN%JMAX
      ENDDO

!        write(0,*) 'to diag writes'

!        write(0,*) 'gfld%igdtnum: ', gfld%igdtnum
!        write(0,*) 'GDIN%IMAX, GDIN%JMAX: ', GDIN%IMAX, GDIN%JMAX

  280 FORMAT(' IGDN, IMAX,JMAX, ',4I5)
      RETURN
      END SUBROUTINE rdhdrs_g2

  END MODULE rdgrib
