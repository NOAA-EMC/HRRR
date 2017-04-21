C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    SMARTINITHRRRCONUS    CREATES NDFD FILES 
C   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 2011-09-20
C
C ABSTRACT:   THIS CODE TAKES NATIVE HRRR FILES AND GENERATES
C          2.5 KM OUTPUT OVER CONUS CONTAINING NDFD ELEMENTS
C
C PROGRAM HISTORY LOG:
C   2011-09-10  G MANIKIN  - CODE REVISED FOR HI-RES RAPID REFRESH 
C
      PARAMETER(IM=2145,JM=1377,MAXLEV=15)
      PARAMETER(ITOT=IM*JM)
      PARAMETER (CAPA=0.28589641)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER DATE
      INTEGER P, PP, R, RR, S, SS, W, WW, X, XX
      LOGICAL RITEHD
      LOGICAL*1 VALIDPT(IM,JM)
      CHARACTER *50 WXSTRING(IM,JM)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,
     &  PQ0=379.90516,P1000=100000.0)

      DIMENSION ID(25)
      INTEGER FHR,CYC,HOUR
      REAL, ALLOCATABLE :: HGHT(:,:,:), T(:,:,:),
     X   Q(:,:,:),UWND(:,:,:),VWND(:,:,:),
     X   PSFC(:,:),PMID(:,:,:),ZSFC(:,:),
     X   T2(:,:),Q2(:,:),U10(:,:),V10(:,:),D2(:,:),
     X   VIS(:,:),TOTCLD(:,:),TOPO(:,:),WGUST(:,:),
     X   BASEZ(:,:),CEIL(:,:),SLP(:,:)
      REAL, ALLOCATABLE :: TOPO_NDFD(:,:),ROUGH(:,:),VEG(:,:), 
     X   DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:),DOWNP(:,:),
     X   DOWNV(:,:),DOWNQ(:,:),LAND(:,:)

      allocate (hght(im,jm,maxlev),t(im,jm,maxlev),q(im,jm,maxlev),
     X    uwnd(im,jm,maxlev),vwnd(im,jm,maxlev),psfc(im,jm),
     X    zsfc(im,jm),ceil(im,jm),pmid(im,jm,maxlev),
     x    u10(im,jm),v10(im,jm),d2(im,jm),totcld(im,jm),
     X    t2(im,jm),q2(im,jm),basez(im,jm),vis(im,jm),
     X    topo(im,jm),wgust(im,jm),slp(im,jm))
      allocate (topo_ndfd(im,jm),rough(im,jm),veg(im,jm),downt(im,jm),
     X    downdew(im,jm),downu(im,jm),downv(im,jm),downq(im,jm),
     X    downp(im,jm),land(im,jm))

      READ (5,*) FHR
      READ (5,*) CYC
      print *, 'into main ', FHR
      print *, 'cyc ', CYC
      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12
      SPVAL=9.9E10

c  READ THE GRIB FILES FROM THE HRRR.  WE NEED TO READ A
c   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
c   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
c   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
c   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
c   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

        CALL GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X    T2,Q2,D2,U10,V10,LAND,WGUST,VIS,TOTCLD,BASEZ,CEIL,
     X    SLP,VALIDPT,DATE,FHR)

c  CALL THE DOWNSCALING CODE 
       CALL NDFDgrid(PSFC,ZSFC,T,HGHT,Q,UWND,VWND,PMID,
     X      T2,Q2,D2,U10,V10,LAND,DOWNT,DOWNDEW,DOWNU,DOWNV,
     X      DOWNQ,DOWNP,TOPO,VALIDPT)

       RITEHD = .TRUE.
       ID(1:25) = 0
       ID(8)=11
       ID(9)=105
       ID(11)=2
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNT,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=17
       ID(9)=105
       ID(11)=2
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNDEW,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=51
       ID(9)=1
       ID(9)=105
       ID(11)=2
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,DOWNQ,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=33
       ID(9)=105
       ID(11)=10
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNU,DATE,FHR,DEC)
 
       ID(1:25) = 0
       ID(8)=34
       ID(9)=105
       ID(11)=10
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNV,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=1
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,DOWNP,DATE,FHR,DEC)

       DO J=1,JM
       DO I=1,IM
         IF(.NOT. VALIDPT(I,J)) THEN
           SLP(I,J)=SPVAL
           ZSFC(I,J)=SPVAL
           WGUST(I,J)=SPVAL
           VIS(I,J)=SPVAL
           TOTCLD(I,J)=SPVAL
           BASEZ(I,J)=SPVAL
           CEIL(I,J)=SPVAL
         ENDIF  
       ENDDO
       ENDDO

       ID(1:25) = 0
       ID(8)=2
       ID(9)=102
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,SLP,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=7
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,ZSFC,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=180
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,WGUST,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=20
       ID(9)=1
       DEC=2.7
       CALL GRIBIT(ID,RITEHD,VIS,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=71
       ID(9)=200
       DEC=-3.0
       CALL GRIBIT(ID,RITEHD,TOTCLD,DATE,FHR,DEC)


       ID(1:25) = 0
       ID(8)=7
       ID(9)=2
       DEC=-3.0
       CALL GRIBIT(ID,RITEHD,BASEZ,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=7
       ID(9)=215
       DEC=-3.0
       CALL GRIBIT(ID,RITEHD,CEIL,DATE,FHR,DEC)
 
       print *, 'completed main'
      deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,d2,t2,q2,u10,v10,
     X    wgust,vis,totcld,basez,ceil,pmid)
      deallocate (topo_ndfd,rough,veg,downt,downdew,downu,downv,downq,
     X    downp,land)

      STOP
      END

