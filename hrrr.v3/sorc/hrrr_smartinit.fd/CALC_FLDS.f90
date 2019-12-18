      subroutine calc_flds(im,jm,maxlev,model)

      use constants
      use aset3d
      use aset2d
      use asetdown

      character*4 model
      REAL, ALLOCATABLE :: PBLMARK(:,:),RH(:,:,:)
      allocate (rh(im,jm,maxlev),pblmark(im,jm))

      print*,'begin calc_flds=',im,jm,malxev,model

      DO J=1,JM
      DO I=1,IM
        IF (VALIDPT(I,J)) THEN
          SPEED=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J))
          WGUST(I,J)=MAX(GUST(I,J),SPEED)
        ELSE
          WGUST(I,J)=SPVAL
          PSFC(I,J)=SPVAL
          REFC(I,J)=SPVAL
          WETFRZ(I,J)=SPVAL
          VIS(I,J)=SPVAL
          SFCR(I,J)=SPVAL
          SST(I,J)=SPVAL
        ENDIF
      ENDDO
      ENDDO

      print*,'here 1'

      if(trim(model) == 'RAP')then
! GSM  for boundary layer computations, find the number of
! levels within the lowest 180 mb

      DO J=1,JM
      DO I=1,IM
        PBLMARK(I,J)=1
        TOP=PSFC(I,J)-18000.
        lloop: DO L=35,1,-1
          IF(PMID(I,J,L).GT.TOP)THEN
            PBLMARK(I,J)=L
            exit lloop
          ENDIF
        ENDDO lloop
      ENDDO
      ENDDO

      print*,'here 2'
! compute RH
      DO J=1,JM
      DO I=1,IM
        DO L=1,MAXLEV
          IF (VALIDPT(I,J)) THEN
            QC=PQ0/PMID(I,J,L) &
               *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
            RH(I,J,L)=Q(I,J,L)/QC
          ELSE
            RH(I,J,L)=0.
          ENDIF
        ENDDO
        DO L=41,MAXLEV
          RH(I,J,L) = 0.
        ENDDO
      ENDDO
      ENDDO
      endif

      print*,'here 3'
      DO J=1,JM
        DO I=1,IM
          if (trim(model) == 'RAP') then
          SKY(I,J)=SPVAL
          IF (VALIDPT(I,J)) THEN
            SKYTMP=AMAX1(LCLD(I,J),MCLD(I,J))
            SKY(I,J)=AMAX1(SKYTMP,HCLD(I,J))
! put in for Puerto Rico, but apply to all grids
            IF (SKY(I,J) .LT. 1.) THEN
              SKY(I,J)=0.
            ENDIF
          ENDIF
          else
            sky(i,j)=tcld(i,j)
          endif
        ENDDO
      ENDDO

      print*,'here 4'
      if (trim(model) == 'RAP') then
!==========================================================================
!  TransWind - the average winds in the layer between the surface
!              and the mixing height.
!--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
        IF (VALIDPT(I,J)) THEN
          MGD=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J))
          UTOT=0.
          VTOT=0.
          COUNT=0.
          LMBL=PBLMARK(I,J)
          DO L=1,LMBL
            UTOT=UTOT+UWND(I,J,L)
            VTOT=VTOT+VWND(I,J,L)
            COUNT=COUNT+1
          ENDDO
          UTRANS=UTOT/COUNT
          VTRANS=VTOT/COUNT
          MGTRANS(I,J)=SQRT(UTRANS*UTRANS+VTRANS*VTRANS)
          IF (MGTRANS(I,J).EQ.0.) THEN
            DIRTRANS(I,J)=0.
          ELSE
            DIRTRANS(I,J)=ATAN2(-UTRANS,-VTRANS) / 0.0174
          ENDIF
          IF(DIRTRANS(I,J).LT.0.) DIRTRANS(I,J)=DIRTRANS(I,J)+360.0
          IF(DIRTRANS(I,J).GT.360.) DIRTRANS(I,J)=DIRTRANS(I,J)-360.0
        ELSE
          DIRTRANS(I,J)=SPVAL
          MGTRANS(I,J)=SPVAL
        ENDIF
      ENDDO
      ENDDO

!  compute BL RH

      DO J=1,JM
      DO I=1,IM
        IF (VALIDPT(I,J)) THEN
          BLH=PBLMARK(I,J)
          SUM=0.
          LEVS=0.
          DO L=1, BLH
            SUM=SUM+RH(I,J,L)
            LEVS=LEVS+1
          ENDDO
          BLR(I,J)=(SUM/LEVS)*100.
        ELSE
          BLR(I,J)=SPVAL
        ENDIF
      ENDDO
      ENDDO
      CALL BOUND(BLR,0.,100.,im,jm)

!========================================================================
!  MixHgt - the height to which a parcel above a 'fire' would rise
!    (in height) above ground level (in feet).
!
!  Calculated by assuming a parcel above a fire is VERY hot - but the fire
!  is very small - so that entrainment quickly makes it only a few degrees
!  warmer than the environment.  Ideally would want to consider moisture
!  and entrainment - but this is a very simple first guess.

      jloop2: DO J=1,JM
      iloop2: DO I=1,IM
        MIXHGT(I,J)=SPVAL
        IF (VALIDPT(I,J)) THEN
          firetheta=((P1000/PSFC(I,J))**CAPA)*(T2(I,J)+2.0)
          lloop2: DO L=2,MAXLEV
            theta=((P1000/PMID(I,J,L))**CAPA)*(T(I,J,L))
            IF (theta.gt.firetheta) THEN
              MIXHGT(I,J)=HGHT(I,J,L)-ZSFC(I,J)
              cycle iloop2
            ENDIF
          ENDDO lloop2
          MIXHGT(I,J)=HGHT(I,J,40)+300.
        ENDIF
      END DO iloop2
      END DO jloop2

!--------------------------------------------------------------------------
! LAL - Based mainly on lifted index.  Adds more when RH at top of BL is
!       high, but RH at bottom of BL is low.
!--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
        LAL(I,J)=SPVAL
        IF(VALIDPT(I,J)) THEN
          IF (BLI(I,J).LT.-5.) THEN
            LLAL=4.
          ELSE IF (BLI(I,J).LT.-3) THEN
            LLAL=3.
          ELSE IF (BLI(I,J).LT.0) THEN
            LLAL=2.
          ELSE
            LLAL=1.
          ENDIF

!   Add more when RH at top of BL is greater than
!      than 70% and RH at bottom of BL is less than 30

          RH1TOT=0.
          RH1SUM=0.
          RH2TOT=0.
          RH2SUM=0.
          DO L=1,MAXLEV
            IF(PSFC(I,J)-PMID(I,J,L).LT.3000.) THEN
              RH1TOT=RH1TOT+RH(I,J,L)
              RH1SUM=RH1SUM+1.
            ENDIF
            IF(PSFC(I,J)-PMID(I,J,L).LT.18000. .AND. &
              PSFC(I,J)-PMID(I,J,L).GT.12000.) THEN
              RH2TOT=RH2TOT+RH(I,J,L)
              RH2SUM=RH2SUM+1.
            ENDIF
          ENDDO
          RH1=RH1TOT/RH1SUM
          RH2=RH2TOT/RH2SUM
          IF (RH2.GT.0.8 .AND. RH1.LT.0.2) THEN
            LAL(I,J)=LLAL+1.
          ELSE
            LAL(I,J)=LLAL
          ENDIF
          IF (LAL(I,J).LT.-18.) THEN
            LAL(I,J)=1.
          ENDIF
        ENDIF
      ENDDO
      ENDDO

      endif

      deallocate (rh,pblmark)
      return
      end
