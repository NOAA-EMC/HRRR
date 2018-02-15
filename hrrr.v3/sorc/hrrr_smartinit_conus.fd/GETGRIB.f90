      SUBROUTINE GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND, &
        T2,Q2,D2,U10,V10,COAST,GUST,VIS,TCLD,BASEZ, &
        CEIL,PMSL,VALIDPT,DATE,IFHR,GDIN,GFLD,GFLD8)
        
       use grddef
       use rdgrib
       USE GRIB_MOD
       USE pdstemplates 

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-09-30
!
! ABSTRACT:
!   .
!
! PROGRAM HISTORY LOG:
!   11-09-30  G MANIKIN  - ADAPT CODE TO HI-RES RAPID REFRESH 
!
! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
!     NONE
      TYPE (GINFO) :: GDIN
      PARAMETER(ILIM=2145,JLIM=1377,MAXLEV=20)
!      PARAMETER(ITOT=ILIM*JLIM)
!      REAL, ALLOCATABLE :: GRID(:)
      DIMENSION DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5),YEAR,MON,DAY,IHR,DATE,IFHR
      INTEGER NUMVAL, IMAX, JMAX, KMAX, NUMLEV, ITOT, KRET, &
              ISSREF, JDISC, JPDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
!      LOGICAL*1 MASK(ITOT),VALPT(ILIM,JLIM)
      LOGICAL*1 VALIDPT(ILIM,JLIM)
!
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      REAL ZSFC(ILIM,JLIM),T(ILIM,JLIM,MAXLEV),PSFC(ILIM,JLIM), &
       Q(ILIM,JLIM,MAXLEV),PMID(ILIM,JLIM,MAXLEV),VEG(ILIM,JLIM), &
       UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV), &
       T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM),COAST(ILIM,JLIM), &
       U10(ILIM,JLIM),V10(ILIM,JLIM),HGHT(ILIM,JLIM,MAXLEV), &
       VIS(ILIM,JLIM),GUST(ILIM,JLIM),BASEZ(ILIM,JLIM), &
       CEIL(ILIM,JLIM),TCLD(ILIM,JLIM),PMSL(ILIM,JLIM)
      TYPE(GRIBFIELD)::GFLD,GFLD_S,GFLD8,GFLD8_S
!
!      allocate(grid(itot))
!      NUMLEV=MAXLEV

!  ASSIGN UNIT NUMBERS 
!
!  FOR 12-hr TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!   DATA FOR THE PREVIOUS 11 HOURS
       LUGB=11
       LUGI=12

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

! GSM  READ HRRR FILE 
!  READ INDEX FILE TO GET GRID SPECS
!
      IRGI = 1
      IRGS = 1
      KMAX = 0
!      CALL BAOPEN(LUGB,'fort.11',IRETGB)
!      CALL BAOPEN(LUGI,'fort.12',IRETGI)
      write(0,*) 'call RDHDRS'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      GDIN%KMAX=MAXLEV
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,igdnum,numval'
      print *,imax,jmax,kmax,numlev,igdnum,numval
!      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
!      write(6,*)' IRET FROM GETGI ',IRGI
!      IF(IRGI .NE. 0) THEN
!        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
!        ISTAT = IRGI
!        RETURN
!      ENDIF
!      REWIND LUGI

      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'GRID ALLOCATED',ITOT,' kret ',kret

      JIDS=-9999
      JPDTN=-1
      JPDT=-9999
      JGDTN=-1
      JGDT=-9999
!     JPDTN needs to be 0 to match specific records
      JPDTN=0
!     JDISC matches discipline table
      JDISC=0
      ISSREF=0


!   get the vertical profile of pressure
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 3
       JPDT(2) = 0
       JPDT(10) = 105
       JPDT(12) = LL

       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)
       J=K

      ENDDO

!   get the vertical profile of height
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 3
       JPDT(2) = 5
       JPDT(10) = 105
       JPDT(12) = LL

       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)
       J=K

      ENDDO

!   get the vertical profile of temperature
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 0
       JPDT(2) = 0
       JPDT(10) = 105
       JPDT(12) = LL
       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &   
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,T(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)

       IF (LL.EQ.1) THEN
        DO M=1,ILIM
         DO N=1,JLIM
          IF (T(M,N,LL).lt.5) THEN
           VALIDPT(M,N)=.FALSE.
          ELSE
           VALIDPT(M,N)=.TRUE.
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       J=K

      ENDDO

!   get the vertical profile of q
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 1
       JPDT(2) = 0
       JPDT(10) = 105
       JPDT(12) = LL

       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &   
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)
       J=K

      ENDDO

!   get the vertical profile of u
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 2
       JPDT(2) = 2
       JPDT(10) = 105
       JPDT(12) = LL

       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &   
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)
       J=K

      ENDDO

!   get the vertical profile of v
      J=0
      DO LL=1,MAXLEV
       JPDT(1) = 2
       JPDT(2) = 3
       JPDT(10) = 105
       JPDT(12) = LL

       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &   
                    JGDT,KF,K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),GFLD, &
                    ISSREF,IRET,ISTAT)
       J=K

      ENDDO

!   get sfc height 
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 1
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,GFLD8, &
                     ISSREF,IRET,ISTAT)
       print*,'minval zsfc',minval(zsfc)
       print*,'maxval zsfc',maxval(zsfc)
       WHERE (ZSFC < 0.0) ZSFC=0.0

!   get surface pressure
      JPDT(1) = 3
      JPDT(2) = 0
      JPDT(10) = 1
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PSFC,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval psfc',minval(psfc)
       print*,'maxval psfc',maxval(psfc)

!   get sea level pressure
      JPDT(1) = 3
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PMSL,GFLD8, &
                     ISSREF,IRET,ISTAT)
       print*,'minval pmsl',minval(pmsl)
       print*,'maxval pmsl',maxval(pmsl)

!   2-m temp
      JPDT(1) = 0
      JPDT(2) = 0
      JPDT(10) = 103
      JPDT(12) = 2
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,T2,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval t2',minval(t2)
       print*,'maxval t2',maxval(t2)

!   2-m spec hum
      JPDT(1) = 1
      JPDT(2) = 0
      JPDT(10) = 103
      JPDT(12) = 2
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q2,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval q2',minval(q2)
       print*,'maxval q2',maxval(q2)


!   2-m dew point 
      JPDT(1) = 0
      JPDT(2) = 6
      JPDT(10) = 103
      JPDT(12) = 2
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,D2,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval d2',minval(d2)
       print*,'maxval d2',maxval(d2)

!   10-m U 
      JPDT(1) = 2
      JPDT(2) = 2
      JPDT(10) = 103
      JPDT(12) = 10
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,U10,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval u10',minval(u10)
       print*,'maxval u10',maxval(u10)

!   10-m V
      JPDT(1) = 2
      JPDT(2) = 3
      JPDT(10) = 103
      JPDT(12) = 10
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,V10,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval v10',minval(v10)
       print*,'maxval v10',maxval(v10)

!   vegetation fraction (actually vegetation type...)
      JDISC = 2
      JPDT(1) = 0
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VEG,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval veg',minval(veg)
       print*,'maxval veg',maxval(veg)

!   land mask (used for changing land to water and vice versa)
      JPDT(1) = 0
      JPDT(2) = 0
      JPDT(10) = -9999
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,COAST,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval coast',minval(coast)
       print*,'maxval coast',maxval(coast)

!   visibility 
      JDISC = 0
      JPDT(1) = 19
      JPDT(2) = 0
      JPDT(10) = 1
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VIS,GFLD8, &
                     ISSREF,IRET,ISTAT)
       print*,'minval vis',minval(vis)
       print*,'maxval vis',maxval(vis)

!   sfc wind gust
      JPDT(1) = 2
      JPDT(2) = 22
      JPDT(10) = -9999
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &          
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,GUST,GFLD8, &
                     ISSREF,IRET,ISTAT)
       print*,'minval gust',minval(gust)
       print*,'maxval gust',maxval(gust)

!   total cloud fraction
      JPDT(1) = 6
      JPDT(2) = 1
      JPDT(10) = -9999
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &                 
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,TCLD,GFLD8, &
                     ISSREF,IRET,ISTAT)
       print*,'minval tcld',minval(tcld)
       print*,'maxval tcld',maxval(tcld)

!   cloud base height
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 2
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &                 
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,BASEZ,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval basez',minval(basez)
       print*,'maxval basez',maxval(basez)

! ceiling
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 215
      JPDT(12) = -9999
      J = 0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &                 
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,CEIL,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval ceil',minval(ceil)
       print*,'maxval ceil',maxval(ceil)

      END
