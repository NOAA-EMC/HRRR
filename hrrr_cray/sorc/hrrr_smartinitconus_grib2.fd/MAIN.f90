!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SMARTINITHRRRCONUS    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 2011-09-20
!
! ABSTRACT:   THIS CODE TAKES NATIVE HRRR FILES AND GENERATES
!          2.5 KM OUTPUT OVER CONUS CONTAINING NDFD ELEMENTS
!
! PROGRAM HISTORY LOG:
!   2011-09-10  G MANIKIN  - CODE REVISED FOR HI-RES RAPID REFRESH 
!
      PROGRAM SMARTINIT
       use grddef
       use rdgrib
       USE GRIB_MOD
       USE pdstemplates  

      PARAMETER(IM=2145,JM=1377,MAXLEV=15)
      PARAMETER(ITOT=IM*JM)
      PARAMETER (CAPA=0.28589641)
      REAL, PARAMETER :: SPVAL=9.9E10
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER DATE,LUB,NUMVAL
      INTEGER P, PP, R, RR, S, SS, W, WW, X, XX
!      LOGICAL*1 MASK(ITOT), VALIDPT(IM,JM)
      LOGICAL*1 VALIDPT(IM,JM)
      LOGICAL RITEHD,NEED12,BITMAP(ITOT)
      CHARACTER *50 WXSTRING(IM,JM)
      CHARACTER *80 FNAMEOUT
!
      PARAMETER(MBUF=2000000,JF=1000000)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86, &
        PQ0=379.90516,P1000=100000.0)

      DIMENSION ID(25)
      INTEGER FHR,CYC,HOUR
      REAL, ALLOCATABLE :: HGHT(:,:,:), T(:,:,:), &
        Q(:,:,:),UWND(:,:,:),VWND(:,:,:), &
        PSFC(:,:),PMID(:,:,:),ZSFC(:,:), &
        T2(:,:),Q2(:,:),U10(:,:),V10(:,:),D2(:,:), &
        VIS(:,:),TOTCLD(:,:),TOPO(:,:),WGUST(:,:), &
        BASEZ(:,:),CEIL(:,:),SLP(:,:)
      REAL, ALLOCATABLE :: TOPO_NDFD(:,:),ROUGH(:,:),VEG(:,:), &
        DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:),DOWNP(:,:), &
        DOWNV(:,:),DOWNQ(:,:),LAND(:,:),GRIDWX(:,:),GFIELD(:)
      TYPE (GINFO) :: GDIN
      TYPE (GRIBFIELD) :: GFLD,GFLD8

      allocate (hght(im,jm,maxlev),t(im,jm,maxlev),q(im,jm,maxlev), &
         uwnd(im,jm,maxlev),vwnd(im,jm,maxlev),psfc(im,jm), &
         zsfc(im,jm),ceil(im,jm),pmid(im,jm,maxlev), &
         u10(im,jm),v10(im,jm),d2(im,jm),totcld(im,jm), &
         t2(im,jm),q2(im,jm),basez(im,jm),vis(im,jm), &
         topo(im,jm),wgust(im,jm),slp(im,jm))
      allocate (topo_ndfd(im,jm),rough(im,jm),veg(im,jm),downt(im,jm), &
         downdew(im,jm),downu(im,jm),downv(im,jm),downq(im,jm), &
         downp(im,jm),land(im,jm))

      READ (5,*) FHR
      READ (5,*) CYC
      print *, 'into main ', FHR
      print *, 'cyc ', CYC
      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12
!      SPVAL=9.9E10

!  READ THE GRIB FILES FROM THE HRRR.  WE NEED TO READ A
!   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
!   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
!   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
!   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
!   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

        CALL GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND, &
         T2,Q2,D2,U10,V10,LAND,WGUST,VIS,TOTCLD,BASEZ,CEIL, &
         SLP,VALIDPT,DATE,FHR,GDIN,GFLD,GFLD8)

!  CALL THE DOWNSCALING CODE 
       CALL NDFDgrid(PSFC,ZSFC,T,HGHT,Q,UWND,VWND,PMID, &
           T2,Q2,D2,U10,V10,LAND,DOWNT,DOWNDEW,DOWNU,DOWNV, &
           DOWNQ,DOWNP,TOPO,VALIDPT)
         write(0,*) 'return from NDFD: completed successfully' 

       FNAMEOUT='fort.  '

       LUB = 71
!      Open grib2 file for writing
       WRITE(FNAMEOUT(6:7),FMT='(I2)')LUB
         write(0,*) 'call baopen: ', lub
       CALL BAOPEN(LUB,FNAMEOUT,IRET)    
         write(0,*) 'return from baopen: completed successfully'

!-----------------------------------------
!      Write 2-m temperature to grib2
!-----------------------------------------
       write(0,*) 'minval T2 before bitmap',minval(downt)
       write(0,*) 'maxval T2 before bitmap',maxval(downt) 
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNT(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO

       DEC=-2.0
!       GFLD%ibmap=255
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNT)
         write(0,*) 'return from FILL_FLD 2-m T: completed successfully'
       print *,'template number',GFLD%ipdtmpl
       print *,'idrtemplate number',GFLD%idrtmpl
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0 ! Grib2 Code Table 4.2 (Parameter Category)
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(9)=FHR
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=2
       print*,'template number after',GFLD%ipdtmpl

       GFLD%idrtnum=40 !JPEG2000
       GFLD%idrtmpl(2)=DEC
       GFLD%idrtmpl(5)=0
       GFLD%idrtmpl(6)=0
       GFLD%idrtmpl(7)=-1
       GFLD%idrtmpl(1)=0

       CALL set_scale(gfld,DEC)
         write(0,*) 'return from setscale 2-m T: completed successfully'
       CALL PUTGB2(71,GFLD,IRET)
         write(0,*) 'return from putgb2 2-m T: completed successfully'

!-----------------------------------------
!      Write 2-m dew point temperature to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNDEW(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO

       DEC=-2.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNDEW)
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=2
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write 2-m Q to grib2
!-----------------------------------------
       write(0,*) 'minval Q2 before bitmap',minval(downq)
       write(0,*) 'maxval Q2 before bitmap',maxval(downq) 
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNQ(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
       write(0,*) 'minval Q2 after bitmap',minval(downq)
       write(0,*) 'maxval Q2 after bitmap',maxval(downq) 

       DEC=3.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNQ)
       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=2
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write 10-m u wind to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNU(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO

       DEC=-2.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNU)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=2
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=10
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)
 
!-----------------------------------------
!      Write 10-m v wind to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNV(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO

       DEC=-2.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNV)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=3
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=10
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write surface pressure to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNP(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO

       DEC=3.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNP)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


       DO J=1,JM
       DO I=1,IM
         IF(.NOT. VALIDPT(I,J)) THEN
           SLP(I,J)=SPVAL
           ZSFC(I,J)=SPVAL
           WGUST(I,J)=SPVAL
           VIS(I,J)=SPVAL
           TOTCLD(I,J)=SPVAL
         ENDIF  
       ENDDO
       ENDDO


!-----------------------------------------
!      Write sea level pressure to grib2
!-----------------------------------------

       DEC=3.0
       GFLD%bmap=GFLD8%bmap

       CALL FILL_FLD(GFLD,ITOT,IM,JM,SLP)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=198
       GFLD%ipdtmpl(10)=101
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write surface Geopotential height to grib2
!-----------------------------------------
       DEC=3.0
       GFLD%bmap=GFLD8%bmap

       CALL FILL_FLD(GFLD,ITOT,IM,JM,ZSFC)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write surface wind gust to grib2
!-----------------------------------------
       DEC=3.0
       GFLD%bmap=GFLD8%bmap

       CALL FILL_FLD(GFLD,ITOT,IM,JM,WGUST)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=22
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=10
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write surface visibility to grib2
!-----------------------------------------
       DEC=2.7
       GFLD%bmap=GFLD8%bmap

       CALL FILL_FLD(GFLD,ITOT,IM,JM,VIS)
       GFLD%ipdtmpl(1)=19
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC
       
       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write total cloud cover (entire atmosphere) to grib2
!-----------------------------------------
       DEC=-3.0
       GFLD%bmap=GFLD8%bmap

       CALL FILL_FLD(GFLD,ITOT,IM,JM,TOTCLD)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=200
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write cloud base height to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (BASEZ(M,N).EQ. 0.) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
!        DO J=1,JM
!        DO I=1,IM
!          IF (BASEZ(I,J).LE. 0.) THEN
!            BASEZ(I,J)=SPVAL
!          ENDIF
!        ENDDO
!        ENDDO 
       DEC=3.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,BASEZ)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=2
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC
       
       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!-----------------------------------------
!      Write ceiling height to grib2
!-----------------------------------------
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (CEIL(M,N).EQ. 0.) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
!        DO J=1,JM
!        DO I=1,IM
!          IF (CEIL(I,J).LE. 0.) THEN
!            CEIL(I,J)=SPVAL
!          ENDIF
!        ENDDO
!        ENDDO
       DEC=-3.0 ! originally was 3.0
       GFLD%ibmap=0
       GFLD%bmap=BITMAP

       CALL FILL_FLD(GFLD,ITOT,IM,JM,CEIL)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=215
       GFLD%ipdtmpl(12)=-9999
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

       CALL BACLOSE(71,iret)
 
       print *, 'completed main'
      deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,d2,t2,q2,u10,v10, &
         wgust,vis,totcld,basez,ceil,pmid)
      deallocate (topo_ndfd,rough,veg,downt,downdew,downu,downv,downq, &
         downp,land,gridwx,gfield)

      STOP
      END PROGRAM smartinit
! -------------------------
        SUBROUTINE FILL_FLD(GFLD,NUMV,IM,JM,ARRAY2D)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        INTEGER :: NUMV, IM, JM, KK
        REAL :: ARRAY2D(IM,JM)
!          write(0,*) 'FILLFLD: IM, JM, NUMV', IM, JM, NUMV

        DO KK = 1, NUMV
!          write(0,*) 'MOD(KK,IM)', MOD(KK,IM)
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
!          write(0,*) 'M, N, ARRAY2D(M,N)', M, N, ARRAY2D(M,N)
!          write(0,*) 'FILL_FLD: before array2D is called'
          GFLD%FLD(KK)=ARRAY2D(M,N)
!          write(0,*) 'FILL_FLD: after array2D is called' 
        ENDDO
        END SUBROUTINE FILL_FLD

! -------------------------
        SUBROUTINE SET_SCALE(GFLD,DEC)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        LOGICAL*1, allocatable:: locbmap(:)
        real :: DEC



        allocate(locbmap(size(GFLD%fld)))

        if (GFLD%ibmap .eq. 0 .or. GFLD%ibmap .eq. 254) then
        locbmap=GFLD%bmap
        write(0,*) 'used GFLD bmap'
        else
        write(0,*) 'hardwire locbmap to true'
        locbmap=.true.
        endif

! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack



        call g2getbits(GFLD%ibmap,DEC,size(GFLD%fld),locbmap,GFLD%fld, &
                      GFLD%idrtmpl(1),GFLD%idrtmpl(2),GFLD%idrtmpl(3),GFLD%idrtmpl(4))

        write(0,*) 'gfld%idrtmpl(2:3) defined, inumbits: ', gfld%idrtmpl(2:4)

        END SUBROUTINE SET_SCALE

! --------------------------------

       subroutine g2getbits(ibm,scl,len,bmap,g,gmin,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of
!   bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Program log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF
      write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range
!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
 !        I1=1
 !        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
 !          I1=I1+1
 !        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
        write(0,*) 'leave g2getbits with GMIN: ', GMIN
!        GFLD%idrtmpl(1)=GMIN
!     write(0,*)'in g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits
