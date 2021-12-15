C      SUBROUTINE PROF(NHB,LRSTRT,ITAG,LCLAS1)
C      SUBROUTINE PROF(ITAG,LCLAS1)
      SUBROUTINE PROF_EM_NET(filename,datestr,ITAG,INCR)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  PROF        PROFILE SOUNDINGS
C   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-04-22
C
C ABSTRACT:  THIS ROUTINE GENERATES THE RAW PROFILE SOUNDING
C            OUTPUT FILES FROM THE FORECAST RESTRT FILE AND
C            AUXILIARY FILES
C
C PROGRAM HISTORY LOG:
C   99-04-22  T BLACK - ORIGINATOR
C   02-07-01  G MANIKIN - FIXED PROBLEM WITH DHCNVC AND DHRAIN
C                          COMPUTATIONS - SEE COMMENTS BELOW
C   03-04-01  M PYLE - BEGAN CONVERTING FOR WRF
C
C USAGE:  CALL PROF FROM PROGRAM POST0
C
C   INPUT ARGUMENT LIST:
C     NHB    - THE UNIT NUMBER FOR READING THE NHB FILE
C     LRSTRT - THE UNIT NUMBER FOR READING THE RESTRT FILE
C     ITAG   - THE FORECAST HOUR WE ARE DEALING WITH
C     LCLAS1 - THE UNIT NUMBER FOR WRITING THE PROFILE DATA
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C
C-----------------------------------------------------------------------
c      use vrbls3d
c      use vrbls2d
c      use soil
c      use masks
C
      include 'wrf_io_flags.h'

!      INCLUDE "parmeta"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (NSTAT=2000,LCL1ML=13,LCL1SL=50)
!       NWORDM=(LCL1ML+1)*LM+2*LCL1SL
!     &, LRECPR=4*(8+9+LCL1ML*LM+LCL1SL))

C-----------------------------------------------------------------------
C
C    PARMS FOR HOURLY PROFILER OUTPUT
C      NSTAT - MAX NUMBER OF STATIONS
C      NWORDM - DIMENSION OF OUTPUT ARRAY, MUST BE LARGE ENOUGH
C          TO HOLD ALL VARIABLES
C          (MAX NO MULTI-LAYER VARIABLES*LM + NO OF SINGLE LAYER VARS)
C      LCL1ML - NUMBER OF MULTI-LAYER VARIABLES OUTPUT FOR CLASS 1
C      LCL1SL - NUMBER OF SINGLE LAYER VARIABLES OUTPUT FOR CLASS 1
C
C------------------------------------------------------------------------
                             P A R A M E T E R
     & (ITB=76,JTB=134)
                             P A R A M E T E R
     & (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516,DTR=1.74532925E-2
     &, G=9.81,GI=1./G,RD=287.04,CP=1004.6,CAPA=RD/CP,RHCRIT=0.9999)

      PARAMETER (GAMMA=6.5/1000.,ZSL=0.0,D608=0.608)
      PARAMETER (TAUCR=RD*GI*290.66,CONST=0.005*G/RD)

C------------------------------------------------------------------------
                             R E A L
     & STNLAT(NSTAT),STNLON(NSTAT)
!                             R E A L
!     & DETA(LM),RDETA(LM),AETA(LM),UL(2*LM)
C
       REAL, ALLOCATABLE::
     & RES(:),FIS(:),THS(:),HBOT(:)
     &,CFRACL(:),CFRACM(:),CFRACH(:),SNO(:)
     &,SOILTB(:),SFCEXC(:),SMSTAV(:),SMSTOT(:)
     &,Z0(:),CZEN(:),CZMEAN(:),U00(:),SR(:)
     &,ACPREC(:),CUPREC(:),ACSNOW(:),ACSNOM(:)
     &,SSROFF(:),BGROFF(:),SFCSHX(:),SFCLHX(:)
     &,SUBSHX(:),SNOPCX(:),ASWIN(:),ASWOUT(:)
     &,ASWTOA(:),ALWIN(:),ALWOUT(:),ALWTOA(:)
     &,TSHLTR(:),QSHLTR(:),TH2_hold(:)
     &,TH10(:),Q10(:),U10(:),V10(:)
     &,TLMIN(:),TLMAX(:)
     &,SMC(:,:),CMC(:),STC(:,:),SH2O(:,:)
     &,VEGFRC(:),POTFLX(:),PSLP(:),PDSL1(:)
     &,EGRID2(:),SM(:),SICE(:),TSK(:)
     &,HBM2(:),FACTR(:)
     &,PTBL(:,:),TTBL(:,:)
     &,STATPR(:),STACPR(:),STAEVP(:)
     &,STAPOT(:),STASHX(:),STASUB(:),STAPCX(:)
     &,STASWI(:),STASWO(:),STALWI(:),STALWO(:)
     &,STALWT(:),STASWT(:),STASNM(:),STASRF(:)
     &,STABRF(:),STASNO(:),SWEM(:)
     &,ACPREC0(:),CUPREC0(:),SFCLHX0(:),POTFLX0(:)
     &,SFCSHX0(:),SUBSHX0(:),SNOPCX0(:),ASWIN0(:)
     &,ASWOUT0(:),ALWIN0(:),ALWOUT0(:),ALWTOA0(:)
     &,ASWTOA0(:),ACSNOW0(:),ACSNOM0(:),SSROFF0(:)
     &,BGROFF0(:),CANOPY(:)

C
!                             R E A L
!     & T(NSTAT,LM),Q(NSTAT,LM),U(NSTAT,LM),V(NSTAT,LM),Q2(NSTAT,LM)
!     &,OMGALF(NSTAT,LM),CWM(NSTAT,LM),TRAIN(NSTAT,LM),TCUCN(NSTAT,LM)
!     &,RSWTT(NSTAT,LM),RLWTT(NSTAT,LM),CCR(NSTAT,LM),RTOP(NSTAT,LM)
!     &,HTM(NSTAT,LM),OMGA(NSTAT,LM)

      REAL, ALLOCATABLE:: T(:,:),Q(:,:),U(:,:),V(:,:),Q2(:,:)
     &,                   OMGALF(:,:),CWM(:,:),TRAIN(:,:),TCUCN(:,:)
     &,                   RSWTT(:,:),RLWTT(:,:),CCR(:,:),RTOP(:,:)
     &,                   HTM(:,:),OMGA(:,:),p_hold(:,:),t_hold(:,:)
     &,                   PINT(:,:),UL(:),CLDFRA_BL(:,:)
C  

      REAL, ALLOCATABLE:: DHCNVC(:,:),DHRAIN(:,:),STADHC(:),STADHR(:),
     &                      TCUCN0(:,:),TRAIN0(:,:)

      REAL,ALLOCATABLE:: DUM(:,:,:),DUMMY(:,:),DUMMY2(:,:),DUMMY3(:,:),
     &  DUM3D(:,:,:),DUM3D2(:,:,:),DUM3D3(:,:,:),GDLAT(:,:),GDLON(:,:)

      REAL, ALLOCATABLE:: PRODAT(:),FPACK(:)

      INTEGER, ALLOCATABLE:: IDUM(:,:),LMH(:,:),IW(:,:)


!     &,p_hold(NSTAT,LM),t_hold(NSTAT,LM)
!       REAL:: PINT(NSTAT,LM+1),LATSTART,LONSTART

       REAL, ALLOCATABLE :: PMID(:,:),W(:,:), WH(:,:),
     &                      pint_part(:),PDS(:),SFCP(:)

	real, allocatable:: CROT(:),SROT(:)
C------------------------------------------------------------------------
                             I N T E G E R
     & IDSTN(NSTAT),IHINDX(NSTAT),JHINDX(NSTAT)
     &,             IVINDX(NSTAT),JVINDX(NSTAT),IDAT(3)
	INTEGER:: GDS(200)
C
C------------------------------------------------------------------------
                             L O G I C A L
     & RUN,RESTRT,FRST
C------------------------------------------------------------------------
                             C H A R A C T E R
     & RSTFIL*90,RESTHR*4,LABEL*32,CISTAT*8,CIDSTN(NSTAT)*8
     &,FNAME*90,ENVAR*90,BLANK*4

	CHARACTER(LEN=8), ALLOCATABLE :: CIDSTN_SAVE(:)

C	new stuff
      character(len=31) :: VarName,varin
	character(len=90) :: fileName
	character(len=90) :: fileNamehold
      integer :: Status, DataHandle
      character(len=19):: startdate,datestr,datestrold,datestrold1

	real:: rinc(5)
	integer:: IDATE(8),JDATE(8)
        character :: SysDepInfo*80
        real :: truelat1, truelat2

C------------------------------------------------------------------------
      DATA BLANK/'    '/
C------------------------------------------------------------------------
C***
C***  READ IN THE INFORMATION FILE ABOUT THE SOUNDINGS
C***

c	write(6,*) 'filename= ', filename
c	write(6,*) 'startedate= ', startdate

!	datestr=startdate

      print *, 'into netcdf PROF_EM'
      print *,  'DateStr: ', DateStr
      REWIND 19
C
      READ(19)NUMSTA,IDSTN,STNLAT,STNLON
     1,       IHINDX,JHINDX,IVINDX,JVINDX,CIDSTN
        print *, 'NUMSTA IDSTN ', NUMSTA,IDSTN,STNLAT,IHINDX,JHINDX
	
	write(6,*) 'STNLAT(1), STNLON(1): ', STNLAT(1), STNLON(1)
	write(6,*) 'IHINDX(1),JHINDX(1): ', IHINDX(1),JHINDX(1)
	write(6,*) 'IVINDX(1),JVINDX(1): ', IVINDX(1),JVINDX(1)
      WRITE(6,20)NUMSTA
   20 FORMAT('INIT:  NUMBER OF PROFILE STATIONS ',I5)

!mp
	allocate(CIDSTN_SAVE(NUMSTA))
	DO N=1,NUMSTA
	CIDSTN_SAVE(N)=CIDSTN(N)
	ENDDO

!mp

	if (ITAG .eq. 0) then
      WRITE(6,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
     1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
     2,               CIDSTN(N),N=1,NUMSTA)
!	else
!      WRITE(6,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
!     1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
!     2,               CIDSTN_SAVE(N),N=1,NUMSTA,20)
!
	endif
   30 FORMAT(2X,I6,2F8.2,4I8,4X,A8)

c	if (ITAG .eq. 0) then
	  FRST=.TRUE.
c	else 
c	  FRST=.FALSE.
c	endif


!--------------------------------------------------------------------
!--------------------------------------------------------------------
!--------------------------------------------------------------------

       if ( frst ) then
         frst = .false.
         CALL ext_ncd_ioinit(SysDepInfo,Status)
          print*,'CALLed ioinit', Status
	write(6,*) 'filename early in PROF= ', filename
         CALL ext_ncd_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
          print*,'CALLed open for read', Status
	  print*,'associated DataHandle: ', DataHandle
       else
           Status = 0
       endif
       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif

!	write(6,*) 'js, je, jev: ', js,je,jev

C Getting start time
      CALL ext_ncd_get_dom_ti_char(DataHandle
     1 ,'START_DATE',startdate, status )
        print*,'startdate= ',startdate

      jdate=0
      idate=0
      read(startdate,15)iyear,imn,iday,ihrst
         IDATE(2)=imn
         IDATE(3)=iday
         IDATE(1)=iyear
         IDATE(5)=ihrst

 15   format(i4,1x,i2,1x,i2,1x,i2)
      print*,'start yr mo day hr =',iyear,imn,iday,ihrst

      ifhr=ITAG
      print*,' in INITPOST ifhr fileName=',ifhr,fileName

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'WEST-EAST_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'west-east dimension: ', itmp
        IM=itmp-1

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'SOUTH-NORTH_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'south-north dimension: ', itmp
        JM=itmp-1

        call ext_ncd_get_dom_ti_integer(DataHandle,
     &   'BOTTOM-TOP_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'bottom-top dimension: ', itmp
        LM=itmp-1

        write(6,*) 'allocate with IM, JM, LM: ', IM, JM, LM

!  The end j row is going to be jend_2u for all variables except for V.
	JSTA_2L=1
	JEND_2U=JM
      JS=JSTA_2L
      JE=JEND_2U
      IF (JEND_2U.EQ.JM) THEN
       JEV=JEND_2U+1
      ELSE
       JEV=JEND_2U
      ENDIF
	write(6,*) 'js, je, jev: ', js,je,jev

!!!!!
       ALLOCATE(RES(NUMSTA),FIS(NUMSTA),THS(NUMSTA),HBOT(NUMSTA))
       ALLOCATE(CFRACL(NUMSTA),CFRACM(NUMSTA),CFRACH(NUMSTA))
       ALLOCATE(SNO(NUMSTA),SOILTB(NUMSTA),SFCEXC(NUMSTA))
       ALLOCATE(SMSTAV(NUMSTA),SMSTOT(NUMSTA))
       ALLOCATE(Z0(NUMSTA),CZEN(NUMSTA),CZMEAN(NUMSTA))
       ALLOCATE(U00(NUMSTA),SR(NUMSTA),ACPREC(NUMSTA))
       ALLOCATE(CUPREC(NUMSTA),ACSNOW(NUMSTA),ACSNOM(NUMSTA))
       ALLOCATE(SSROFF(NUMSTA),BGROFF(NUMSTA),SFCSHX(NUMSTA))
       ALLOCATE(SFCLHX(NUMSTA),SUBSHX(NUMSTA),SNOPCX(NUMSTA))
       ALLOCATE(ASWIN(NUMSTA),ASWOUT(NUMSTA),ASWTOA(NUMSTA))
       ALLOCATE(ALWIN(NUMSTA),ALWOUT(NUMSTA),ALWTOA(NUMSTA))
       ALLOCATE(TSHLTR(NUMSTA),QSHLTR(NUMSTA),TH2_hold(NUMSTA))
       ALLOCATE(TH10(NUMSTA),Q10(NUMSTA),U10(NUMSTA),V10(NUMSTA))
       ALLOCATE(TLMIN(NUMSTA),TLMAX(NUMSTA),SMC(NUMSTA,NSOIL))
       ALLOCATE(CMC(NUMSTA),STC(NUMSTA,NSOIL),SH2O(NUMSTA,NSOIL))
       ALLOCATE(VEGFRC(NUMSTA),POTFLX(NUMSTA),PSLP(NUMSTA))
       ALLOCATE(PDSL1(NUMSTA),EGRID2(NUMSTA),SM(NUMSTA),SICE(NUMSTA))
       ALLOCATE(HBM2(NUMSTA),FACTR(NUMSTA),PTBL(ITB,JTB),TTBL(JTB,ITB))
       ALLOCATE(STATPR(NUMSTA),STACPR(NUMSTA),STAEVP(NUMSTA))
       ALLOCATE(STAPOT(NUMSTA),STASHX(NUMSTA),STASUB(NUMSTA))
       ALLOCATE(STAPCX(NUMSTA),STASWI(NUMSTA),STASWO(NUMSTA))
       ALLOCATE(STALWI(NUMSTA),STALWO(NUMSTA),STALWT(NUMSTA))
       ALLOCATE(STASWT(NUMSTA),STASNM(NUMSTA),STASRF(NUMSTA))
       ALLOCATE(STABRF(NUMSTA),STASNO(NUMSTA),ACPREC0(NUMSTA))
       ALLOCATE(CUPREC0(NUMSTA),SFCLHX0(NUMSTA),POTFLX0(NUMSTA))
       ALLOCATE(SFCSHX0(NUMSTA),SUBSHX0(NUMSTA),SNOPCX0(NUMSTA))
       ALLOCATE(ASWIN0(NUMSTA),ASWOUT0(NUMSTA),ALWIN0(NUMSTA))
       ALLOCATE(ALWOUT0(NUMSTA),ALWTOA0(NUMSTA),ASWTOA0(NUMSTA))
       ALLOCATE(ACSNOW0(NUMSTA),ACSNOM0(NUMSTA),SSROFF0(NUMSTA))
       ALLOCATE(BGROFF0(NUMSTA),TSK(NUMSTA),SWEM(NUMSTA))
       ALLOCATE(CANOPY(NUMSTA))


	ALLOCATE(T(NUMSTA,LM))
	ALLOCATE(Q(NUMSTA,LM))
	ALLOCATE(U(NUMSTA,LM))
	ALLOCATE(V(NUMSTA,LM))
	ALLOCATE(Q2(NUMSTA,LM))
	ALLOCATE(OMGALF(NUMSTA,LM))
	ALLOCATE(CWM(NUMSTA,LM))
	ALLOCATE(TRAIN(NUMSTA,LM))
	ALLOCATE(TCUCN(NUMSTA,LM))
	ALLOCATE(RSWTT(NUMSTA,LM))
	ALLOCATE(RLWTT(NUMSTA,LM))
	ALLOCATE(CCR(NUMSTA,LM))
	ALLOCATE(RTOP(NUMSTA,LM))
	ALLOCATE(HTM(NUMSTA,LM))
	ALLOCATE(OMGA(NUMSTA,LM))
	ALLOCATE(p_hold(NUMSTA,LM))
	ALLOCATE(t_hold(NUMSTA,LM))
	ALLOCATE(PINT(NUMSTA,LM+1))
        ALLOCATE(W(NUMSTA,LM+1))
        ALLOCATE(WH(NUMSTA,LM))
        ALLOCATE(IW(NUMSTA,LM))
        ALLOCATE(CLDFRA_BL(NUMSTA,LM))

        ALLOCATE(STADHC(LM))
        ALLOCATE(STADHR(LM))
        ALLOCATE(DHRAIN(LM,NUMSTA))
        ALLOCATE(DHCNVC(LM,NUMSTA))
        ALLOCATE(TCUCN0(LM,NUMSTA))
        ALLOCATE(TRAIN0(LM,NUMSTA))

! former parameter statements
        NWORDM=(LCL1ML+1)*LM+2*LCL1SL
        LRECPR=4*(8+9+LCL1ML*LM+LCL1SL)
! former parameter statements

        if (allocated(FPACK)) deallocate(FPACK); allocate(FPACK(NWORDM))
        if (allocated(PRODAT)) deallocate(PRODAT);
     &                  allocate(PRODAT(NWORDM))
        if (ALLOCATED(DUM)) deallocate(DUM);
     &                          allocate(DUM(IM,JM,4))
        if (ALLOCATED(DUMMY)) deallocate(DUMMY);
     &                          allocate(DUMMY(IM,JM))
        if (ALLOCATED(DUMMY2)) deallocate(DUMMY2);
     &                          allocate(DUMMY2(IM,JM))
        if (ALLOCATED(DUMMY3)) deallocate(DUMMY3);
     &                          allocate(DUMMY3(IM,JM))
        if (ALLOCATED(DUM3D)) deallocate(DUM3D);
     &                          allocate(DUM3D(IM+1,JM+1,LM+1))
        if (ALLOCATED(DUM3D2)) deallocate(DUM3D2);
     &                          allocate(DUM3D2(IM+1,JM+1,LM+1))
        if (ALLOCATED(DUM3D3)) deallocate(DUM3D3);
     &                          allocate(DUM3D3(IM+1,JM+1,LM+1))
        if (ALLOCATED(GDLAT)) deallocate(GDLAT);
     &                          allocate(GDLAT(IM,JM))
        if (ALLOCATED(GDLON)) deallocate(GDLON);
     &                          allocate(GDLON(IM,JM))
        if (ALLOCATED(IDUM)) deallocate(IDUM);
     &                          allocate(IDUM(IM,JM))
        if (ALLOCATED(LMH)) deallocate(LMH);
     &                          allocate(LMH(IM,JM))


!!!!!

        call ext_ncd_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_ncd_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        write(6,*) 'cenlat= ', cenlat
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        write(6,*) 'cenlon= ', cenlon
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=tmp*1000.0
        write(6,*) 'truelat1= ', truelat1
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=tmp*1000.0
        write(6,*) 'truelat2= ', truelat2
        call ext_ncd_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype
!need to get DT
        call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
	write(6,*) 'status from DT get: ', istatus
	write(6,*) 'returned value for DT: ', tmp
        DT=tmp
        print*,'DT= ',DT

! get 3-D variables
      print*,'im,jm,lm= ',im,jm,lm
c
	write(6,*) 'DateStr: ', DateStr
      VarName='U'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM+1,JS,JE,LM)

	write(6,*) 'U: ', DUM3D(20,20,20)

      VarName='V'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JEV,LM)

	write(6,*) 'V: ', DUM3D2(20,20,20)

        print *, 'u v loop'
        DO L = 1, LM
	DO N=1,NUMSTA
	  U(N,L)=DUM3D(IHINDX(N),JHINDX(N),L)
	  V(N,L)=DUM3D2(IHINDX(N),JHINDX(N),L)
	ENDDO
	ENDDO

        print *, 'past u v loop'
	write(6,*) 'U,V defined: '

      VarName='W'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

	write(6,*) 'W: ', DUM3D(20,20,20)

      DO l = 1, lm+1
      DO N=1,NUMSTA
	I=IHINDX(N)	
	J=JHINDX(N)
            w ( N,L ) = dum3d ( i, j, l )
      END DO
      END DO

      DO L = 1,LM
      DO N=1,NUMSTA
	I=IHINDX(N)	
	J=JHINDX(N)
            WH(N,L) = (W(N,L)+W(N,L+1))*0.5
      END DO
      END DO

      VarName='SWDOWN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='GLW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY2,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='GSW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY3,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ASWIN(N)=DUMMY(I,J)
        ALWIN(N)=DUMMY2(I,J)
        ASWOUT(N)=DUMMY3(I,J)-DUMMY(I,J)
      ENDDO

      VarName='PH'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
	write(6,*) 'PH: ', DUM3D(20,20,20)

      VarName='PHB'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
	write(6,*) 'PHB: ', DUM3D(20,20,20)

      VarName='T'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)
	write(6,*) 'T: ', DUM3D(20,20,20)

      do l = 1, lm
       do N = 1, NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
             t_hold ( N , L ) = dum3d ( i, j, l ) + 300.
        end do
       end do

      VarName='MU'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MUB'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY2,
     &  IM,1,JM,1,IM,JS,JE,1)

  633	format(15(f6.0,1x))

!	write(6,*) 'past MU, MUB'

        if (allocated(pint_part)) deallocate(pint_part)
        allocate(pint_part(NUMSTA))

        if (allocated(PDS)) deallocate(PDS)
        allocate(PDS(NUMSTA))
        allocate(SFCP(NUMSTA))


	DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
  	 pint_part(N)=DUMMY(I,J)+DUMMY2(I,J)
	ENDDO


!      VarName='MU0'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

      VarName='P'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

	DO L=1,LM
	DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
	 p_hold(N,L)=DUM3D2(I,J,L)
	ENDDO
	ENDDO

      VarName='QVAPOR'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

	DO L=1,LM
	DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
	 Q(N,L)=DUM3D(I,J,L)/(1.0+DUM3D(I,J,L))
	ENDDO
	ENDDO

      VarName='QCLOUD'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

        DO L = 1, LM
        DO N=1,NUMSTA
          Q2(N,L)=0.
          CWM(N,L)=DUM3D(IHINDX(N),JHINDX(N),L)
        ENDDO
        ENDDO

      VarName='CLDFRA_BL'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,     
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

        DO L = 1, LM
        DO N=1,NUMSTA
          CLDFRA_BL(N,L)=DUM3D(IHINDX(N),JHINDX(N),L)
        ENDDO
        ENDDO

      VarName='TSLB'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      DO L = 1, NSOIL
        DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
! flip soil layer again because wrf soil variable vertical indexing
! is the same with eta and vertical indexing was flipped for both
! atmospheric and soil layers within getVariable
            STC(N,L) = DUM3D(I,J,NSOIL-L+1)
        END DO
      END DO

      VarName='Q2'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
!!! conversion needed for QSHLTR?  (spec hum, mix ratio, what??)
!!! conversion needed for QSHLTR?  (spec hum, mix ratio, what??)
!!! conversion needed for QSHLTR?  (spec hum, mix ratio, what??)
        QSHLTR(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO


      VarName='T2'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='TH2'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        TH2_hold(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      VarName='U10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        U10(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      VarName='V10'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        V10(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        TH10(N)=-9999.
        Q10(N)=-9999.
       END DO

      VarName='SMOIS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      VarName='SH2O'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      DO L = 1, NSOIL
        DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
! flip soil layer again because wrf soil variable vertical indexing
! is the same with eta and vertical indexing was flipped for both
! atmospheric and soil layers within getVariable
        SMC(N,L) = DUM3D (I,J,NSOIL-L+1)
        CMC(N)   = DUM3D (I,J,1)  ! ??????
        SH2O(N,L)= DUM3D2(I,J,NSOIL-L+1)
        END DO
      END DO


c
c reading SMSTAV
      VarName='SMOIS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SMSTAV(N)=DUMMY(I,J)
      ENDDO

      VarName='SFROFF'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='UDROFF'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='IVGTYP'
      call getIVariable(fileName,DateStr,DataHandle,VarName,IDUM
     &  ,IM,1,JM,1,IM,JS,JE,1)
      VarName='ISLTYP'
      call getIVariable(fileName,DateStr,DataHandle,VarName,IDUM
     &  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGFRA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        VEGFRC(N)=DUMMY(I,J)
!	if (mod(N,25) .eq. 0) then
!	write(6,*) 'N, VEGFRC(N): ', N, VEGFRC(N)
!	endif
      ENDDO

      VarName='GRDFLX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACSNOW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM(:,:,1),
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACSNOM'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM(:,:,2),
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ACSNOW(N)=DUM(I,J,1)
        ACSNOM(N)=DUM(I,J,2)
      ENDDO

      VarName='SNOW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SWEM(N)=DUMMY(I,J)
      ENDDO

      VarName='SNOWH'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        CANOPY(N)=DUMMY(I,J)
      ENDDO

      VarName='SST'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='THZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='QZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='UZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='VZ0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='Z0'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        Z0(N)=DUMMY(I,J)
        print *, 'Z0 ', Z0(N)
      ENDDO

      VarName='QSFC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='AKHS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='AKMS'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='PB'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

	if (allocated(PMID)) deallocate(PMID)
	allocate(PMID(NUMSTA,LM))

	write(6,*) 'to PMID DEFINITIONS '
	DO L=1,LM
	DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
	 PMID(N,L)=p_hold(N,L)+DUM3D(I,J,L)
         T(N,L)=t_hold(N,L)*(PMID(N,L)*1.e-5)**CAPA
 	 OMGA(N,L)=-WH(N,L)*PMID(N,L)*G/
     &              (RD*T(N,L)*(1+.608*Q(N,L)))

!!!!! CONSTRAIN Q TO A PARTICULAR RH VALUE, FOLLOWING CALRH OF WRFPOST
           QC= (PQ0/PMID(N,L)) *EXP(A2*(T(N,L)-A3)/(T(N,L)-A4))
           RH=Q(N,L)/QC

           IF (RH .gt. RHCRIT) THEN
           IF (RH .gt. 1.02) THEN
           write(6,*) 'reducing RH from: ', RH, ' at N,L: ', N,L
	   ENDIF
           Q(N,L)=0.999*RHCRIT*QC
           ENDIF
!!!!! END RH CONSTRAIN

	ENDDO
	ENDDO

      VarName='MAPFAC_M'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_U'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_V'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)


      VarName='F'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='E'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SINALPHA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='COSALPHA'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='HGT'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        RES(N)=1.0
        FIS(N)=DUMMY(IHINDX(N),JHINDX(N))*G
      ENDDO

!HERENOW

      VarName='TSK'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        TSK(N)=DUMMY(I,J)
      ENDDO

      VarName='P_TOP'
      call getVariable(fileName,DateStr,DataHandle,VarName,PT,
     &  1,1,1,1,1,1,1,1)

	write(6,*) 'returned P_TOP into PT as : ', PT
      VarName='PSFC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCP(N)=DUMMY(I,J)
      ENDDO

        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         PINT (N,LM+1)=pint_part(N)+PT
         PINT (N,1)=PT

!        PD(I,J)=PINT (N,LM+1) - PINT(N,1)

        TSHLTR(N)=TH2_hold(N)*(PINT(N,LM+1)/100000.)**CAPA
	THS(N)=DUMMY(I,J)*(100000./PINT(N,LM+1))**CAPA

!!! constrain surface RH

           QC=(PQ0/PINT(N,LM+1))*EXP(A2*(TSHLTR(N)-A3)/(TSHLTR(N)-A4))
           RH=QSHLTR(N)/QC
           IF (RH .gt. RHCRIT) THEN
           write(6,*) 'reducing surface RH from: ', RH, ' at N: ', N
           QSHLTR(N)=0.999*RHCRIT*QC
           ENDIF


        ENDDO


!      VarName='FNM'
!	write(6,*) 'FNM'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!	write(6,*) 'FNP'
!      VarName='FNP'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!	write(6,*) 'RDNW'
!      VarName='RDNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)
!	write(6,*) 'return getVariable'

!	write(6,*) 'RDN'
!      VarName='RDN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!	write(6,*) 'DNW'
!      VarName='DNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!	write(6,*) 'DN'
!      VarName='DN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='ZNU'
!	write(6,*) 'ZNU'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='ZNW'
!	write(6,*) 'ZNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM+1,1,1,1,LM+1,1,1,1)

!      VarName='CFN'
!	write(6,*) 'CFN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

!      VarName='CFN1'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

!      VarName='EPSTS'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

CC
CC RAINC is "ACCUMULATED TOTAL CUMULUS PRECIPITATION"
CC RAINNC is "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"

        write(6,*) 'getting RAINC'
      VarName='RAINC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM(:,:,1),
     &  IM,1,JM,1,IM,JS,JE,1)
      write(6,*) 'getting RAINNC'
      VarName='RAINNC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM(:,:,2),
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        CUPREC(N)=DUM(IHINDX(N),JHINDX(N),1)*.001
        ACPREC(N)=( DUM(IHINDX(N),JHINDX(N),1)+
     &                  DUM(IHINDX(N),JHINDX(N),2) )*.001
      ENDDO

      VarName='GSW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='GLW'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='TMN'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUM(:,:,1),
     &  IM,1,JM,1,IM,JS,JE,1)

! XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SM(N)=DUMMY(I,J)-1.0
        SOILTB(N)=DUM(I,J,1) ! NOT 100% sure on this definition
      ENDDO

      VarName='HFX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCSHX(N)=DUMMY(I,J)
      ENDDO
 
      VarName='QFX'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCLHX(N)=DUMMY(I,J)/(2.5E-06)
      ENDDO

      VarName='LH'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &   IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='FLHC'
      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

       DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCEXC(N)=DUMMY(I,J)
      ENDDO

!        call ext_ncd_ioclose(DataHandle)



!!!!!!!!!!!!!!!!! END INSERT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

C------------------------------------------------------------------------
C***
C***  READ QUANTITIES NEEDED FROM THE NHB FILE
C***

      DO N=1,NUMSTA
!       HBM2(N)=DUM(IHINDX(N),JHINDX(N),1)
       HBM2(N)=1.0
      ENDDO
C

!!	ICE available in wrfinput file...zero out for now

      DO N=1,NUMSTA
	SICE(N)=0.0
      ENDDO
C
      DO L=1,LM
       DO N=1,NUMSTA
         HTM(N,L)=1.0
       ENDDO
      ENDDO


	write(6,*) 'set LMH to : ', LM
	write(6,*) 'IM,jm: ', Im,jm
	do J=1,JM
	 do I=1,IM
	   LMH(I,J)=LM
	 enddo
	enddo

C
C       Define a GDS, then use GDSWIZ to find N.N. point

        GDS=-1
        if(maptype .eq. 1)THEN  ! Lambert conformal
          GDS(1)=3
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(GDLAT(1,1)*1000)
          GDS(5)=int(GDLON(1,1)*1000)
          GDS(6)=8
          GDS(7)=CENLON
          GDS(8)=DXVAL
          GDS(9)=DYVAL
          GDS(10)=0
          GDS(11)=64
          GDS(12)=TRUELAT2
          GDS(13)=TRUELAT1
        ELSE IF(MAPTYPE .EQ. 2)THEN  !Polar stereographic
          GDS(1)=5
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(GDLAT(1,1)*1000)
          GDS(5)=int(GDLON(1,1)*1000)
          GDS(6)=8
          GDS(7)=CENLON
          GDS(8)=DXVAL
          GDS(9)=DYVAL
          GDS(10)=0
          GDS(11)=64
        ELSE IF(MAPTYPE .EQ. 3)THEN  !Mercator
          GDS(1)=1
          GDS(2)=im
          GDS(3)=jm
          GDS(4)=int(GDLAT(1,1)*1000)
          GDS(5)=int(GDLON(1,1)*1000)
          GDS(6)=8
          GDS(7)=int(GDLAT(IM,JM)*1000)
          GDS(8)=int(GDLON(IM,JM)*1000)
          GDS(9)=TRUELAT1
          GDS(10)=0
          GDS(11)=64
          GDS(12)=DXVAL
          GDS(13)=DYVAL
        END IF

	write(6,*) 'GDS= ', (GDS(NN),NN=1,13)


C
C	GET ROTATION ANGLES FOR WIND
C

	write(6,*) 'numsta= ', numsta

	ALLOCATE(CROT(NUMSTA),SROT(NUMSTA))

	CROT=0.	
	SROT=0.

	DO N=1,NUMSTA
	I=IHINDX(N)
	J=JHINDX(N)
	RLATX=GDLAT(I,J)
	RLONX=GDLON(I,J)
	
        CALL GDSWIZ(GDS,-1,1,-9999.,xout,yout,
     &                  RLONX,RLATX,NRET,1,CROT(N),SROT(N))
        print *, 'gdswiz ', N, RLATX, RLONX, CROT(N), SROT(N)

	ENDDO

      NTSPH=INT(3600./DT+0.50)

      DO N=1,NUMSTA
       PSFC = SFCP(N)
       ZSFC = FIS(N)*GI
       PSLP(N) = PSFC

!       write(0,*) 'N, ZSFC, PSFC: ', N, ZSFC, PSFC
C
C    COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
Cwrong       TVRT = T(N,1)*(1.0+D608*Q(N,1))
       TVRT = T(N,LM)*(1.0+D608*Q(N,LM))
       TAU  = TVRT*RD*GI
C
C    COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
C    ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
       TVRSFC = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSFC = TVRSFC*RD*GI
       TVRSL  = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSL  = TVRSL*RD*GI
C
C     IF NEED BE APPLY SHEULL CORRECTION.
        IF ((TAUSL.GT.TAUCR).AND.(TAUSFC.LE.TAUCR)) THEN
           TAUSL=TAUCR
        ELSEIF ((TAUSL.GT.TAUCR).AND.(TAUSFC.GT.TAUCR)) THEN
           TAUSL = TAUCR-CONST*(TAUSFC-TAUCR)**2
        ENDIF
C
C    COMPUTE MEAN TAU.
       TAUAVG = 0.5*(TAUSL+TAUSFC)
C
C    COMPUTE SEA LEVEL PRESSURE.
       IF (FIS(N).GT.1.0) PSLP(N) = PSFC*EXP(ZSFC/TAUAVG)
c      print *,n,idstn(n),sfcp(n),pslp(n),zsfc,tauavg, gamma
      ENDDO

C
C------------------------------------------------------------------------
C

	DO L=1,LM
        DO N=1,NUMSTA
	  TRAIN(N,L)=-9999.
	  TCUCN(N,L)=-9999.
        ENDDO
        ENDDO

      DO N=1,NUMSTA
c	Z0(N)=-9999.
        HBOT(N)=-9999.
        CFRACL(N)=0.
        CFRACM(N)=0.
        CFRACH(N)=0.
        CZEN(N)=0.

        DO L=1,17
         IF (CLDFRA_BL(N,L).GT.CFRACH(N)) THEN
           CFRACH(N)=CLDFRA_BL(N,L)
         ENDIF 
        ENDDO

        DO L=18,35
         IF (CLDFRA_BL(N,L).GT.CFRACM(N)) THEN
           CFRACM(N)=CLDFRA_BL(N,L)
         ENDIF
        ENDDO

        DO L=36,50
         IF (CLDFRA_BL(N,L).GT.CFRACL(N)) THEN
           CFRACL(N)=CLDFRA_BL(N,L)
         ENDIF
        ENDDO
	CZEN(N)=-9999.
      ENDDO


      DO N=1,NUMSTA
        SNO(N)=-9999. ! many "sno" type variables...which do we need here?
	SMSTOT(N)=-9999.
c	SFCEXC(N)=-9999.
	CZMEAN(N)=-9999.
	U00(N)=-9999.
	SR(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
        SSROFF(N)=-9999.
        BGROFF(N)=-9999.
c       SFCSHX(N)=-9999.
c       SFCLHX(N)=-9999.
       SUBSHX(N)=-9999.
       SNOPCX(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
c        ASWIN(N)=-9999.
c        ASWOUT(N)=-9999.
        ASWTOA(N)=-9999.
c        ALWIN(N)=-9999.
        ALWOUT(N)=-9999.
        ALWTOA(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
	POTFLX(N)=-9999.
	TLMIN(N)=-9999.
	TLMAX(N)=-9999.
      ENDDO
C------------------------------------------------------------------------
C***
C***  READ RADIATIVE TEMPERATURE TENDENCIES
C***
      DO L=1,LM
        DO N=1,NUMSTA
         RSWTT(N,L)=-9999.
         RLWTT(N,L)=-9999.
        ENDDO
      ENDDO
C
c     CLOSE(LRSTRT)
C------------------------------------------------------------------------
C***
C***  THE FORECAST HOUR
C***  
c     IFHR=NTSD/NTSPH

	IFHR=ITAG
	write(6,*) 'IFHR: ', IFHR
C------------------------------------------------------------------------
      IF(ITAG.GT.0)THEN
	write(6,*) 'working on preceding file'
C***
C***  GENERATE THE NAME OF THE PRECEDING RESTRT FILE
C***
        ITAG0=ITAG-INCR


        RINC(1)=0.
        RINC(2)=float(ITAG0)
        RINC(3)=0.
        RINC(4)=0.
        RINC(5)=0.

	write(6,*) 'RINC(2): ', rinc(2)
        call w3movdat(rinc,idate,jdate)
        write(DateStrold1,301) JDATE(1),JDATE(2),JDATE(3),JDATE(5)
        write(DateStrold,302) JDATE(1),JDATE(2),JDATE(3),JDATE(5)
 301  format(i4,'-',i2.2,'-',i2.2,'_',i2.2,'_00_00')
 302  format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':00:00')

	write(6,*) 'filename later in PROF: ', filename, '_END'
        len=index(filename,' ')-1
	write(6,*) 'LEN= ', LEN
        filename=filename(1:len-19)//DateStrold1

!	write(6,*) 'old filename is ', trim(filename)
	write(6,*) 'date for old file is: ', datestrold


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INSERT READ FROM ABOVE ONCE WORK OUT KINKS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         CALL ext_ncd_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
          print*,'CALLed open for read', Status
	  print*,'associated DataHandle: ', DataHandle

       if ( Status /= 0 ) then
         print*,'error opening ',fileName, ' Status = ', Status ; stop
       endif

C Getting start time
      CALL ext_ncd_get_dom_ti_char(DataHandle
     1 ,'START_DATE',startdate, status )
        print*,'startdate= ',startdate

!      ifhr=nint(rinc(2))
!      print*,' in INITPOST ifhr fileName=',ifhr,fileName


        call ext_ncd_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_ncd_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        write(6,*) 'cenlat= ', cenlat
        call ext_ncd_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        write(6,*) 'cenlon= ', cenlon
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=1000.*tmp
        write(6,*) 'truelat1= ', truelat1
        call ext_ncd_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=1000.*tmp
        write(6,*) 'truelat2= ', truelat2
        call ext_ncd_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype
!need to get DT
        call ext_ncd_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
        DT=tmp
        print*,'DT= ',DT

! get 3-D variables
      print*,'im,jm,lm= ',im,jm,lm
c
      VarName='U'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM+1,JS,JE,LM)

	write(6,*) 'U: ', DUM3D(20,20,20)

      VarName='V'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JEV,LM)

	write(6,*) 'V: ', DUM3D2(20,20,20)

      VarName='W'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

      VarName='PH'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)
      VarName='PHB'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM+1)

      VarName='T'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='MU'
      call getVariable(fileName,DateStrold,DataHandle,
     &  VarName,DUM(:,:,1),
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MUB'
      call getVariable(fileName,DateStrold,DataHandle,
     &  VarName,DUM(:,:,2),
     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='MU0'
!      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

      VarName='P'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='QVAPOR'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='QCLOUD'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='TSLB'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      VarName='Q2'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='T2'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='TH2'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='U10'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='V10'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SMOIS'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      VarName='SH2O'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D2,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,NSOIL)

      VarName='SMSTAV'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SFROFF'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='UDROFF'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='IVGTYP'
      call getIVariable(fileName,DateStrold,DataHandle,VarName,IDUM
     &  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='ISLTYP'
      call getIVariable(fileName,DateStrold,DataHandle,VarName,IDUM
     &  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGFRA'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRDFLX'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACSNOW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

        DO N=1,NUMSTA
          ACSNOW0(N)=DUMMY(IHINDX(N),JHINDX(N))
	ENDDO

      VarName='ACSNOM'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

        DO N=1,NUMSTA
          ACSNOM0(N)=DUMMY(IHINDX(N),JHINDX(N))
	ENDDO

      VarName='SNOW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWH'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='SST'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='THZ0'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='QZ0'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='UZ0'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='VZ0'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='QSFC'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='AKHS'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='AKMS'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='PB'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUM3D3,
     &  IM+1,1,JM+1,LM+1,IM,JS,JE,LM)

      VarName='MAPFAC_M'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_U'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_V'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='F'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='E'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='SINALPHA'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='COSALPHA'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='HGT'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='TSK'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUMMY,
!     &  IM,1,JM,1,IM,JS,JE,1)

!      VarName='P_TOP'
!      call getVariable(fileName,DateStr,DataHandle,VarName,PT,
!     &  1,1,1,1,1,1,1,1)

!      VarName='FNM'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='FNP'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='RDNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='RDN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='DNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='DN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='ZNU'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM,1,1,1,LM,1,1,1)

!      VarName='ZNW'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM1D,
!     &  LM+1,1,1,1,LM+1,1,1,1)

!      VarName='CFN'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

!      VarName='CFN1'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

!      VarName='EPSTS'
!      call getVariable(fileName,DateStr,DataHandle,VarName,DUM0D,
!     &  1,1,1,1,1,1,1,1)

CC
CC RAINC is "ACCUMULATED TOTAL CUMULUS PRECIPITATION"
CC RAINNC is "ACCUMULATED TOTAL GRID SCALE PRECIPITATION"

      VarName='RAINC'
      call getVariable(fileName,DateStrold,DataHandle,
     &  VarName,DUM(:,:,1),
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='RAINNC'
      call getVariable(fileName,DateStrold,DataHandle,
     &  VarName,DUM(:,:,2),
     &  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        CUPREC0(N)=DUM(IHINDX(N),JHINDX(N),1)*.001
        ACPREC0(N)=( DUM(IHINDX(N),JHINDX(N),1)+
     &                  DUM(IHINDX(N),JHINDX(N),2) )*.001
      ENDDO

      VarName='GSW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
      VarName='GLW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='LU_INDEX'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='TMN'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)
! XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

      VarName='HFX'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

        DO N=1,NUMSTA
          SFCSHX0(N)=DUMMY(IHINDX(N),JHINDX(N))
        ENDDO

      VarName='QFX'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

        DO N=1,NUMSTA
         SFCLHX0(N)=DUMMY(IHINDX(N),JHINDX(N))/(2.5E-06)
        ENDDO

      VarName='LH'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &   IM,1,JM,1,IM,JS,JE,1)

      VarName='SWDOWN'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &   IM,1,JM,1,IM,JS,JE,1)

      VarName='GLW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY2,
     &   IM,1,JM,1,IM,JS,JE,1)

      VarName='GSW'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY3,
     &   IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ASWIN0(N)=DUMMY(I,J)
        ALWIN0(N)=DUMMY2(I,J)
        ASWOUT0(N)=DUMMY3(I,J)-DUMMY(I,J)
        print *, 'ASWIN ', ASWIN(N), ASWIN0(N)
        print *, 'ALWIN ', ALWIN(N), ALWIN0(N)
        print *, 'ASWOUT ', ASWOUT(N), ASWOUT0(N)
      ENDDO

      VarName='SNOWC'
      call getVariable(fileName,DateStrold,DataHandle,VarName,DUMMY,
     &  IM,1,JM,1,IM,JS,JE,1)

	write(6,*) 'done reading old file'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END INSERT READ 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


C
c          DO N=1,NUMSTA
c           TRAIN0(N,L)=DUM(IHINDX(N),JHINDX(N),1)
c           TCUCN0(N,L)=DUM(IHINDX(N),JHINDX(N),2)
c	TRAIN0(N,L)=-9999.
c	TCUCN0(N,L)=-9999.
c          ENDDO
C
c       ENDDO      
C
	TRAIN0=-9999.
	TCUCN0=-9999.

!!!
!!!	THESE RUNOFF ASSIGNMENTS COULD BE *WRONG* !!!!!!
!!!

        DO N=1,NUMSTA
          SSROFF0(N)=DUM(IHINDX(N),JHINDX(N),3)
          BGROFF0(N)=DUM(IHINDX(N),JHINDX(N),4)
        ENDDO
C
        DO N=1,NUMSTA
c         SFCSHX0(N)=-9999.
c         SFCLHX0(N)=-9999.
         SUBSHX0(N)=-9999.
         SNOPCX0(N)=-9999.
        ENDDO
C
        DO N=1,NUMSTA
c        ASWIN0(N)=-9999.
c        ASWOUT0(N)=-9999.
        ASWTOA0(N)=-9999.
c        ALWIN0(N)=-9999.
        ALWOUT0(N)=-9999.
        ALWTOA0(N)=-9999.
        POTFLX0(N)=-9999.
        ENDDO
C
C
      ENDIF
C
c     CLOSE(LRSTRT)

!	write(6,*) 'down to here (a)'
C
C
Cmp 	IDEALLY, WON'T NEED MANY MODS BELOW THIS POINT
C
C

C------------------------------------------------------------------------
C***
C***  ALL THE DATA IS NOW IN.
C***  CALCULATE CLOUD FRACTION AND CLOUD WATER/ICE ID NUMBER.
C***
C------------------------------------------------------------------------
      UTIM=1.
      US=1.
      CCLIMIT=1.E-3
      CLIMIT =1.E-20
C-----------------------------------------------------------------------
!$OMP parallel do 
      DO N=1,NUMSTA
        IW(N,1)=-9999
        CCR(N,1)=-9999.
!        PDSL1(N)=PD(IHINDX(N),JHINDX(N))*RES(N)
        PDSL1(N)=pint_part(N)*RES(N)
      ENDDO

!	write(6,*) 'here b'
C
C------------------QW, QI AND QINT--------------------------------------
C

!!!
!!! skip section for now
!!!
!	goto 221

      DO 220 L=2,LM
C
!$OMP parallel do private(cwmkl,fiq,hh,iwkl,lml,pp,qc,qi,qint,qkl,qw,
!$OMP*                    rqkl,tkl,tmt0,tmt15,u00kl)
      DO 210 N=1,NUMSTA
	IW(N,L)=-9999
        CCR(N,L)=-9999.

!      LML=LM-LMH(IHINDX(N),JHINDX(N))
!	write(6,*) 'LML, IHINDX,JHINDX,LMH: ', IHINDX(N), 
!     &            JHINDX(N),LMH(IHINDX(N),JHINDX(N))
!      HH=HTM(N,L)*HBM2(N)
!      TKL=T(N,L)
!      QKL=Q(N,L)
!      CWMKL=CWM(N,L)
!      TMT0=(TKL-273.16)*HH
!      TMT15=AMIN1(TMT0,-15.)*HH
!      AI=0.008855
!      BI=1.
C
!      IF(TMT0.LT.-20.)THEN
!        AI=0.007225
!        BI=0.9674
!      ENDIF
C
Cmp      PP=PDSL1(N)*AETA(L)+PT
!      PP=PMID(N,L)
!      QW=HH*PQ0/PP*EXP(HH*A2*(TKL-A3)/(TKL-A4))
!      QI=QW*(BI+AI*AMIN1(TMT0,0.))
!      QINT=QW*(1.-0.00032*TMT15*(TMT15+15.))
!      IF(TMT0.LE.-40.)QINT=QI
C
C-------------------ICE-WATER ID NUMBER IW------------------------------
C

!	no defs for U00 or UL
!
!	so no U00KL,FIQ,IW,CWM
!
!	write(6,*) 'here c'
!	write(6,*) 'L+LML: ', L+LML
!	write(6,*) 'U00(N): ', U00(N)
!	write(6,*) 'UL(L+LML): ', UL(L+LML)

!      U00KL=U00(N)+UL(L+LML)*(0.95-U00(N))*UTIM
!      IF(TMT0.LT.-15.)THEN
!        FIQ=QKL-U00KL*QI
!        IF(FIQ.GT.0..OR.CWMKL.GT.CLIMIT)THEN
!          IW(N,L)=1
!        ELSE
!          IW(N,L)=0
!        ENDIF
!      ENDIF
C
!      IF(TMT0.GE.0.)THEN
!        IW(N,L)=0
!      ENDIF
C
!      IF(TMT0.LT.0..AND.TMT0.GE.-15.)THEN
!        IW(N,L)=0
!        IF(IW(N,L-1).EQ.1.AND.CWMKL.GT.CLIMIT)IW(N,L)=1
!      ENDIF
C
!      IWKL=IW(N,L)
C----------------THE SATUATION SPECIFIC HUMIDITY------------------------
!      FIW=FLOAT(IWKL)
!      QC=(1.-FIW)*QINT+FIW*QI
C----------------THE RELATIVE HUMIDITY----------------------------------
!      IF(QC.LE.0.)THEN
!         RQKL=0.
!       ELSE
!         RQKL=QKL/QC
!      ENDIF
C----------------CLOUD COVER RATIO CCR----------------------------------
!      IF(RQKL.GE.0.9999)THEN
!        CCR(N,L)=AMIN1(US,RQKL)
!      ELSE
!        ARG=-1000.*CWMKL/(US-RQKL)
!        ARG=AMAX1(ARG,-25.)
!        CCR(N,L)= RQKL*(1.-EXP(ARG))
!      ENDIF
C----------------------------------------------------------------------
!	write(6,*) 'here d'
  210                 CONTINUE
  220                 CONTINUE
  221                 continue
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  BEGIN THE PROFILE POSTING CODE.
C***
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C***
C***  USE ZERO IN ACCUMULATION ARRAYS AT APPROPRIATE TIMES
C***
       IF(ITAG .eq. 0) THEN

!	write(6,*) 'here (2)'
C
C
C what would appropriate if test be here?
C
C
        DO N=1,NUMSTA
C
C*** ZERO ACCUMLATION ARRAYS.
C
          STATPR(N)=0.
          STACPR(N)=0.
          STAEVP(N)=0.
          STAPOT(N)=0.
          STASHX(N)=0.
          STASUB(N)=0.
          STAPCX(N)=0.
          STASWI(N)=0.
          STASWO(N)=0.
          STALWI(N)=0.
          STALWO(N)=0.
          STALWT(N)=0.
          STASWT(N)=0.
          STASNM(N)=0.
          STASNO(N)=0.
          STASRF(N)=0.
          STABRF(N)=0.
          DO L=1,LM
            DHCNVC(L,N)=0.
            DHRAIN(L,N)=0.
          ENDDO
        ENDDO
C
        GO TO 300
       ENDIF
C---------------------------------------------------------------------
C***
C***  WE MUST CHECK TO SEE IF WE ARE 1 HOUR AFTER ANY OF THE 
C***  ACCUMULATION BUCKETS HAVE BEEN EMPTIED.  IF WE ARE AT SUCH A 
C***  TIME THEN WE NEED TO SET TO ZERO THE VARIABLES USED TO HOLD
C***  THE PRECEDING HOUR'S VALUES.
C***
C---------------------------------------------------------------------
C
C
C 	At this point, accumulation buckets are a foreign concept in
C	the WRF model.  
C
C
c     TIME=(NTSD-1)*DT
c     RESET0=TIME-(NTSD/NPREC)*NPREC*DT
c     RESET1=(NPHS-1)*DT+3600.

	TIME=IFCST
!	write(6,*) 'here (3)'

	RESET0=25.  ! designed to prevent resets.  Reconsider later


C
c      IF(MOD(NTSD,NPREC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
c        DO N=1,NUMSTA
c          STATPR(N)=0.
c          STACPR(N)=0.
c          STASNM(N)=0.
c          STASNO(N)=0.
c          STASRF(N)=0.
c          STABRF(N)=0.
c        ENDDO
c      ELSE
!	write(6,*) 'set STATPR'
        DO N=1,NUMSTA
          STATPR(N)=ACPREC0(N)*1.E3
	if (ACPREC0(N) .gt. 0) then
!	write(6,*) 'N,ACPREC0(N),STATPR(N): ', N,
!     &			ACPREC0(N),STATPR(N)
	endif
          STACPR(N)=CUPREC0(N)*1.E3
          STASNM(N)=ACSNOM0(N)*1.E3
          STASNO(N)=ACSNOW0(N)*1.E3
          STASRF(N)=SSROFF0(N)*1.E3
          STABRF(N)=BGROFF0(N)*1.E3
        ENDDO

!	write(6,*) 'past set'
c      ENDIF          
C
c     RESET0=TIME-(NTSD/NRDSW)*NRDSW*DT
c     IF(MOD(NTSD,NRDSW).GE.NPHS.AND.RESET0.LE.RESET1)THEN
c       DO N=1,NUMSTA
c         STASWI(N)=0.
c         STASWO(N)=0.
c         STASWT(N)=0.
c       ENDDO
c     ELSE
        DO N=1,NUMSTA
          STASWI(N)=ASWIN0(N)
          STASWO(N)=ASWOUT0(N)
          STASWT(N)=ASWTOA0(N)
        ENDDO
c     ENDIF
C
c     RESET0=TIME-(NTSD/NRDLW)*NRDLW*DT
c     IF(MOD(NTSD,NRDLW).GE.NPHS.AND.RESET0.LE.RESET1)THEN
c       DO N=1,NUMSTA
c         STALWI(N)=0.
c         STALWO(N)=0.
c         STALWT(N)=0.
c       ENDDO
c     ELSE
        DO N=1,NUMSTA
          STALWI(N)=ALWIN0(N)
          STALWO(N)=ALWOUT0(N)
          STALWT(N)=-ALWTOA0(N)
        ENDDO
c     ENDIF
C
c     RESET0=TIME-(NTSD/NSRFC)*NSRFC*DT
c     IF(MOD(NTSD,NSRFC).GE.NPHS.AND.RESET0.LE.RESET1)THEN
c       DO N=1,NUMSTA
c         STAEVP(N)=0.
c         STAPOT(N)=0.
c         STASHX(N)=0.
c         STASUB(N)=0.
c         STAPCX(N)=0.
c       ENDDO
c     ELSE
        DO N=1,NUMSTA
          STAEVP(N)=SFCLHX0(N)
          STAPOT(N)=POTFLX0(N)
          STASHX(N)=SFCSHX0(N)
          STASUB(N)=SUBSHX0(N)
          STAPCX(N)=SNOPCX0(N)
        ENDDO
c     ENDIF
C
c     RESET0=TIME-(NTSD/NHEAT)*NHEAT*DT
c     IF(MOD(NTSD,NHEAT).GE.NCNVC.AND.RESET0.LE.RESET1)THEN
c       DO N=1,NUMSTA
c         DO L=1,LM
c           DHCNVC(L,N)=0.
c           DHRAIN(L,N)=0.
c         ENDDO
c       ENDDO
c     ELSE
       DO N=1,NUMSTA
         DO L=1,LM
            DHCNVC(L,N)=TCUCN0(L,N)
            DHRAIN(L,N)=TRAIN0(L,N)
         ENDDO
       ENDDO
c     ENDIF
 
C------------------------------------------------------------------
  300 CONTINUE
C------------------------------------------------------------------
C
C***  FOR ROTATION OF WINDS FROM E-GRID TO GEODETIC ORIENTATION
C***  WE NEED THE TWO QUANTITIES BELOW.
C
c      SINPH0=SIN(TPH0D*DTR)
c      COSPH0=COS(TPH0D*DTR)
C
C***  INITIAL CALCULATIONS/PREPARATIONS.  WE LOAD SEVERAL
C***  ARRAYS WITH PROFILE VARIABLES.
C
!$OMP parallel do
      DO N=1,NUMSTA
        IF(CZMEAN(N).GT.0.)THEN
          FACTR(N)=CZEN(N)/CZMEAN(N)
        ELSE
          FACTR(N)=0.
        ENDIF
      ENDDO
C
C***  ADJUST SHORTAVE TENDENCIES TO ACCOUNT FOR CHANGE OF SOLAR POSITION
C***  BETWEEN CALLS TO RADIATION
C
!$OMP parallel do
c      DO L=1,LM
c        DO N=1,NUMSTA
c          RSWTT(N,L)=RSWTT(N,L)*FACTR(N)
c        ENDDO
c      ENDDO
C
C***  COMPUTE RTOP
C
!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
!          APEL=PT+AETA(L)*PDSL1(N)
          APEL=PMID(N,L)
          RTOP(N,L)=RD*T(N,L)*(1.+0.608*Q(N,L))/APEL
        ENDDO
      ENDDO
C
C***  PDS IS SURFACE PRESSURE.
C
!$OMP parallel do 
	DO N=1,NUMSTA
!	I=IHINDX(N)	
!	J=JHINDX(N)
!        PDS(N)=PD(I,J)+PT
        PDS(N)=pint_part(N)+PT


	ENDDO
C
C***  EGRID2 IS THE SURFACE TEMPERATURE.
C
!$OMP parallel do 
      DO N=1,NUMSTA
        EGRID2(N)= THS(N)*(PDS(N)*1.E-5)**CAPA
        IF(ACPREC(N).LT.0.)ACPREC(N)=0.
        IF(CUPREC(N).LT.0.)CUPREC(N)=0.
      ENDDO
C
C***  SET CYCLE, DATE, AND FORECAST TIME.
C
c      IHR  =NTSD/NTSPH+0.5

!MAYBE      read(datestr,15)iyear,imn,iday,ihrst
         IDATE(2)=imn
         IDATE(3)=iday
         IDATE(1)=iyear
         IDATE(5)=ihrst

	IDAT(3)=IDATE(1)
	IDAT(1)=IDATE(2)
	IDAT(2)=IDATE(3)
	
      IYR  =IDAT(3)
      IMNTH=IDAT(1)
      IDAY =IDAT(2)
c      IFCST=(NTSD-1)*DT

!      IFCST=NTSPH*ITAG
      IFCST=3600*ITAG
	IHR=ITAG
	write(6,*) 'IFCST: ', IFCST
C
      WRITE(6,*)' POST PROFILE FOR ',
     1                       IYR,IMNTH,IDAY,IHR
	write(6,*) 'IHRST= ', IHRST
C
C***  SET RTSPH,RTSCU,RTSRA TO 1. OVER THE NUMBER OF TIMES THE
C***  VARIOUS PHYSICS ROUTINES HAVE BEEN
C***  CALLED SINCE LAST OUTPUT OF PROFILER DATA.  NECESSARY FOR
C***  CORRECT AVERAGING OF VARIABLES.
C
	APHTIM=0.
	ACUTIM=0.
	ARATIM=0.

	write(6,*) 'APHTIM, ACUTIM, ARATIM were: ', 
     &                APHTIM, ACUTIM, ARATIM
      IF(APHTIM.GT.0.)THEN
        RTSPH=1./APHTIM
      ELSE
        RTSPH=1.
      ENDIF
C
      IF(ACUTIM.GT.0.)THEN
        RTSCU=1./ACUTIM
      ELSE
        RTSCU=1.
      ENDIF
C
      IF(ARATIM.GT.0.)THEN
        RTSRA=1./ARATIM
      ELSE
        RTSRA=1.
      ENDIF
C
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
C***
C***  OUTPUT PROFILE DATA.  THE FOLLOWING LOOP IS OVER ALL PROFILE SITES.
C***
C--------------------------------------------------------------------------
	LCLAS1=79


	write(6,*) 'open output file with RECL: ', LRECPR
      OPEN(UNIT=LCLAS1,ACCESS='DIRECT',RECL=LRECPR,IOSTAT=IER)
C--------------------------------------------------------------------------
	write(6,*) 'RECORD LENGTH = ', LRECPR

      DO 1000 N=1,NUMSTA
C
C***  ZERO OUTPUT ARRAY.
C
      DO K=1,NWORDM
        PRODAT(K)=0.
        FPACK(K) =0.
      ENDDO
C
C***  CONSTRUCT HEADER FOR CURRENT PROFILE SITE.  THE HEADER CONTAINS
C***  THE FOLLOWING INFORMATION:  PACKED CYCLE-DATE, FORECAST TIME,
C***  INTEGER STATION ID, STATION LATITUDE, STATION LONGITUDE, STATION
C***  ELEVATION, NUMBER OF VERTICAL LEVELS IN PROFILE, NUMBER OF MULTI-
C***  LEVEL PARAMETERS, NUMBER OF SINGLE LEVEL PARAMETERS, TOTAL LENGTH
C***  (IN WORDS) OF MULTI- AND SINGLE LEVEL DATA, PROFILE CLASS FLAG,
C***  AND A DUMMY WORD FOR FUTURE USE.
C
      IH=IHINDX(N)
      JH=JHINDX(N)
      LMHK     = LMH(IH,JH)
      NWORD2   = 2*LMHK
      NWORD3   = 3*LMHK
      NWORD4   = 4*LMHK
      NWORD5   = 5*LMHK
      NWORD6   = 6*LMHK
      NWORD7   = 7*LMHK
      NWORD8   = 8*LMHK
      NWORD9   = 9*LMHK
      NWORD10  = 10*LMHK
      NWORD11  = 11*LMHK
      NWORD12  = 12*LMHK
      NWORD13  = 13*LMHK
      ISTAT    = IDSTN(N)
      CISTAT   = CIDSTN_SAVE(N)
!	write(6,*) 'CISTAT: ', CISTAT
C
      FPACK(1) = STNLAT(N)/DTR
!mp      FPACK(2) = -STNLON(N)/DTR
      FPACK(2) = STNLON(N)/DTR
      IF(FPACK(2).LT.-180.)FPACK(2)=FPACK(2)+360.
      FPACK(3) = FIS(N)*GI
      FPACK(4) = FLOAT(LMHK)
      FPACK(5) = LCL1ML
      FPACK(6) = LCL1SL
      FPACK(7) = 9+FPACK(5)*FPACK(4)+FPACK(6)
      FPACK(8) = 999.
      FPACK(9) = 999.

C
C***  WIND ROTATION SINES AND COSINES
C
      TLM0D=0.0
      COSPH0=0.0
      SINPH0=0.0
      DLM    = STNLON(N)+TLM0D*DTR
      XX     = COSPH0*COS(STNLAT(N))*COS(DLM)
     1        +SINPH0*SIN(STNLAT(N))
      YY     = -COS(STNLAT(N))*SIN(DLM)
      if(abs(XX) > 1.0e-10) then
        TLON   = ATAN(YY/XX)
      else
        TLON = 1.570796
      endif
      ALPHA  = ASIN(SINPH0*SIN(TLON)/COS(STNLAT(N)))
       SINALP = SIN(ALPHA)
       COSALP = COS(ALPHA)

c      SINALP = SROT(N)
c      COSALP = CROT(N)
C
C------------------------------------------------------------------
C***  EXTRACT PRESSURE AND TEMPERATURE PROFILES.
C***  EXTRACT/ROTATE U AND V WIND COMPONENT PROFILES.
C***  EXTRACT SPECIFIC HUMIDITY AND TEMPERATURE TENDENCY.
C***  EXTRACT CLOUD WATER, HEATING DUE TO CONVECTION, LARGE
C***  SCALE RAIN, SHORT WAVE RADIATION, LONG WAVE RADIATION,
C***  AND CLOUD FRACTION.
C------------------------------------------------------------------
C
      DO LV=1,LMHK
        LVL=LMHK-LV+1
!        PRODAT(LVL)      = PDSL1(N)*AETA(LV)+PT
        PRODAT(LVL)      = PMID(N,LV)
	if (mod(LV,15) .eq. 0 .and. mod(N,50) .eq. 0) then
!	write(6,*) 'PRODAT definition, PMID: ', N,L,PMID(N,LV)
	endif
	
!	if (LVL .eq. 1 .and. mod(N,25) .eq. 0) then
!	write(6,*) 'N, PSFC: ', N,PRODAT(1)
!	endif

        PRODAT(LMHK+LVL) = T(N,LV)

C***  ROTATE WINDS
C
        UT     = U(N,LV)
        VT     = V(N,LV)
        PRODAT(NWORD2+LVL) = UT*COSALP+VT*SINALP
        PRODAT(NWORD3+LVL) = VT*COSALP-UT*SINALP

	if (N .eq. 1) THEN
c	WRITE(6,*) 'orig U,V: ', UT,VT	
c	write(6,*) 'COSALP,SINALP: ', COSALP,SINALP
c	WRITE(6,*) 'rotat U,V: ', PRODAT(NWORD2+LVL),PRODAT(NWORD3+LVL)
c	write(6,*) '-----------------'
	endif

C
        PRODAT(NWORD4+LVL) = Q(N,LV)
C
        IF(RTOP(N,LV).GT.1.E-12) THEN
           PRODAT(NWORD5+LVL) = OMGA(N,LV)
Cmp     1   PRODAT(NWORD5+LVL) = OMGALF(N,LV)*CP/(RTOP(N,LV)*DT)
	ENDIF

        IF(IW(N,LV).GT.0.5)THEN
          PRODAT(NWORD6+LVL) = -CWM(N,LV)
        ELSE
          PRODAT(NWORD6+LVL) = CWM(N,LV)
        ENDIF

C
        PRODAT(NWORD7+LVL) = TCUCN(N,LV)
        PRODAT(NWORD8+LVL) = TRAIN(N,LV)
        PRODAT(NWORD9+LVL) = RSWTT(N,LV)
        PRODAT(NWORD10+LVL)= RLWTT(N,LV)
        PRODAT(NWORD11+LVL)= CLDFRA_BL(N,LV)*100.

C
        IF(LV.EQ.1)THEN
          PRODAT(NWORD12+LVL)=Q2(N,LV)
        ELSE
          PRODAT(NWORD12+LVL)=(Q2(N,LV)+Q2(N,LV-1))*0.5
        ENDIF
      ENDDO

C
C***  MODIFY ACCUMLATIONS SO AS TO REPRESENT ACCUMULATED
C***  CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
CGSM  MODIFIED CODE TO ACCOUNT FOR DHCNVC AND DHRAIN BEING
C       COMPUTED FROM TOP DOWN WHILE PRODAT IS FILLED FROM
C       BOTTOM UP 
C

      DO LL=1,LMHK
        LVL=LMHK-LL+1
        STADHC(LL) = PRODAT(NWORD7+LL) - DHCNVC(LVL,N)
        STADHR(LL) = PRODAT(NWORD8+LL) - DHRAIN(LVL,N)
C
        DHCNVC(LVL,N) = PRODAT(NWORD7+LL)
        DHRAIN(LVL,N) = PRODAT(NWORD8+LL)
C
Ctmp        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          DHCNVC(LVL,N) = 0.
          DHRAIN(LVL,N) = 0.
Ctmp        ENDIF
      ENDDO
C
C***  EXTRACT SINGLE LEVEL DATA.   EGRID2 IS SURFACE TEMPERATURE.
C
      PRODAT(NWORD13+1)  = PSLP  (N)
      PRODAT(NWORD13+2)  = SFCP   (N)
c      PRODAT(NWORD13+3)  = EGRID2(N)
      PRODAT(NWORD13+3)  = TSK   (N) 
      PRODAT(NWORD13+4)  = TLMIN (N)
      PRODAT(NWORD13+5)  = TLMAX (N)
      PRODAT(NWORD13+6)  = SMSTAV(N)*100.
      PRODAT(NWORD13+7)  = ACPREC(N)*1000.
      PRODAT(NWORD13+8)  = CUPREC(N)*1000.
      PRODAT(NWORD13+27) = Z0    (N)
C
      STAPRX=PRODAT(NWORD13+7)-STATPR(N)
      STACRX=PRODAT(NWORD13+8)-STACPR(N)

!	if (STAPRX .gt. 0) then
!	write(6,*) '1hr precip: ',  N,STAPRX
!	endif
C
C***  ROTATE WINDS
C
      UT     = U10(N)
      VT     = V10(N)
      PRODAT(NWORD13+28) = UT*COSALP+VT*SINALP
      PRODAT(NWORD13+29) = VT*COSALP-UT*SINALP
      PRODAT(NWORD13+30) = TH10  (N)
      PRODAT(NWORD13+31) = Q10   (N)
      PRODAT(NWORD13+32) = TSHLTR(N)
      PRODAT(NWORD13+33) = QSHLTR(N)
      PRODAT(NWORD13+34) = SFCEXC(N)*.001
      PRODAT(NWORD13+35) = VEGFRC(N)
      PRODAT(NWORD13+36) = CANOPY(N)*1000.
      PRODAT(NWORD13+37) = SMC   (N,1)
      PRODAT(NWORD13+38) = SMC   (N,2)
      PRODAT(NWORD13+39) = SMC   (N,3)
      PRODAT(NWORD13+40) = SMC   (N,4)
      PRODAT(NWORD13+41) = STC   (N,1)
      PRODAT(NWORD13+42) = STC   (N,2)
      PRODAT(NWORD13+43) = STC   (N,3)
      PRODAT(NWORD13+44) = STC   (N,4)
      PRODAT(NWORD13+45) = SM    (N) + SICE(N)
      PRODAT(NWORD13+46) = CFRACL(N)*100.
      PRODAT(NWORD13+47) = CFRACM(N)*100.
      PRODAT(NWORD13+48) = CFRACH(N)*100.
      PRODAT(NWORD13+49) = SR    (N)*100.
      PRODAT(NWORD13+50) = NINT(HBOT(N))
C
      PRODAT(NWORD13+9)   = SFCLHX(N)
      PRODAT(NWORD13+10)  = POTFLX(N)
      PRODAT(NWORD13+11)  = SFCSHX(N)
      PRODAT(NWORD13+12)  = SUBSHX(N)
      PRODAT(NWORD13+13)  = SNOPCX(N)
      PRODAT(NWORD13+14)  = ASWIN (N)
      PRODAT(NWORD13+15)  = ASWOUT(N)
      PRODAT(NWORD13+16)  = ALWIN (N)
      PRODAT(NWORD13+17)  = ALWOUT(N)
      PRODAT(NWORD13+18)  =-ALWTOA(N)
      PRODAT(NWORD13+19)  = ASWTOA(N)
      PRODAT(NWORD13+20)  = ACSNOW(N)*1000.
      PRODAT(NWORD13+21)  = SMSTOT(N)*1000.
      PRODAT(NWORD13+22)  = SWEM  (N)*1000.
      PRODAT(NWORD13+23)  = ACSNOM(N)*1000.
      PRODAT(NWORD13+24)  = SSROFF(N)*1000.
      PRODAT(NWORD13+25)  = BGROFF(N)*1000.
      PRODAT(NWORD13+26)  = SOILTB(N)
C
C***  ACCUMULATED CHANGE SINCE LAST PROFILE OUTPUT TIME.
C
      PSFCEVP  = PRODAT(NWORD13+9 ) - STAEVP(N)
      PPOTEVP  = PRODAT(NWORD13+10) - STAPOT(N)
      PSFCSHX  = PRODAT(NWORD13+11) - STASHX(N)
      PSFCSUB  = PRODAT(NWORD13+12) - STASUB(N)
      PSNOPCX  = PRODAT(NWORD13+13) - STAPCX(N)
      PRSWIN   = PRODAT(NWORD13+14) - STASWI(N)
      PRSWOUT  = PRODAT(NWORD13+15) - STASWO(N)
      PRLWIN   = PRODAT(NWORD13+16) - STALWI(N)
      PRLWOUT  = PRODAT(NWORD13+17) - STALWO(N)
      PRLWTOA  = PRODAT(NWORD13+18) - STALWT(N)
      PRSWTOA  = PRODAT(NWORD13+19) - STASWT(N)
      PACSNOW  = PRODAT(NWORD13+20) - STASNO(N)
      PACSNOM  = PRODAT(NWORD13+23) - STASNM(N)
      PSSROFF  = PRODAT(NWORD13+24) - STASRF(N)
      PBGROFF  = PRODAT(NWORD13+25) - STABRF(N)
C***
C***  TRANSFER STATION PROFILE DATA TO "PACKED" OUTPUT ARRAY.
C***
      NN   = 0
      NLEN = FPACK(7)
C	write(6,*) 'NWORD13+41,NWORD13+32 ', NWORD13+41,NWORD13+32
C	write(6,*) 'SOIL TEMP ', PRODAT(NWORD13+41)
C        write(6,*) 'SHELT TEMP ', PRODAT(NWORD13+32) 
C
      DO NL = 10,NLEN
        NN = NL-9
        FPACK(NL) = PRODAT(NN)
      ENDDO
C
C***  REPLACE ACCUMULATED QUANTITIES WITH ACCUMULATION
C***  SINCE LAST PROFILE OUTPUT TIME.
C
      DO LL = 1,LMHK
!        FPACK(9+NWORD7+LL) = STADHC(LL)*RTSCU
!        FPACK(9+NWORD8+LL) = STADHR(LL)*RTSRA
        FPACK(9+NWORD7+LL) = -9999.
        FPACK(9+NWORD8+LL) = -9999.
      ENDDO
C
      FPACK(9+NWORD13+7)  = STAPRX
!	write(6,*) 'precip written to FPACK element: ', 9+NWORD13+7
      FPACK(9+NWORD13+8)  = STACRX
      FPACK(9+NWORD13+9)  = PSFCEVP * RTSPH
c      FPACK(9+NWORD13+10) = PPOTEVP * RTSPH
      FPACK(9+NWORD13+11) = PSFCSHX * RTSPH
c      FPACK(9+NWORD13+12) = PSFCSUB * RTSPH
c      FPACK(9+NWORD13+13) = PSNOPCX * RTSPH
c      FPACK(9+NWORD13+14) = PRSWIN  * RTSPH
c      FPACK(9+NWORD13+15) = PRSWOUT * RTSPH
c      FPACK(9+NWORD13+16) = PRLWIN  * RTSPH
c      FPACK(9+NWORD13+17) = PRLWOUT * RTSPH
c      FPACK(9+NWORD13+18) = PRLWTOA * RTSPH
c      FPACK(9+NWORD13+19) = PRSWTOA * RTSPH
c      FPACK(9+NWORD13+9)  = -9999. 
      FPACK(9+NWORD13+10) = -9999. 
c      FPACK(9+NWORD13+11) = -9999. 
      FPACK(9+NWORD13+12) = -9999. 
      FPACK(9+NWORD13+13) = -9999. 
      FPACK(9+NWORD13+14) = -9999. 
      FPACK(9+NWORD13+15) = -9999. 
      FPACK(9+NWORD13+16) = -9999.
      FPACK(9+NWORD13+17) = -9999. 
      FPACK(9+NWORD13+18) = -9999. 
      FPACK(9+NWORD13+19) = -9999. 

      FPACK(9+NWORD13+20) = PACSNOW
      FPACK(9+NWORD13+23) = PACSNOM
      FPACK(9+NWORD13+24) = PSSROFF
      FPACK(9+NWORD13+25) = PBGROFF
C
!      IF(RESTRT)THEN
      IF(ITAG .eq. 0)THEN
        DO LL = 1,LMHK
          FPACK(9+NWORD7+LL) = 0.
          FPACK(9+NWORD8+LL) = 0.
        ENDDO
C
        FPACK(9+NWORD13+7)  = 0.
        FPACK(9+NWORD13+8)  = 0.
        FPACK(9+NWORD13+9)  = 0.
        FPACK(9+NWORD13+10) = 0.
        FPACK(9+NWORD13+11) = 0.
        FPACK(9+NWORD13+12) = 0.
        FPACK(9+NWORD13+13) = 0.
        FPACK(9+NWORD13+14) = 0.
        FPACK(9+NWORD13+15) = 0.
        FPACK(9+NWORD13+16) = 0.
        FPACK(9+NWORD13+17) = 0.
        FPACK(9+NWORD13+18) = 0.
        FPACK(9+NWORD13+19) = 0.
        FPACK(9+NWORD13+20) = 0.
        FPACK(9+NWORD13+23) = 0.
        FPACK(9+NWORD13+24) = 0.
        FPACK(9+NWORD13+25) = 0.
      ENDIF
C---------------------------------------------------------------------
C***
C***  WRITE PROFILE DATA
C***
      
!	write(6,*) 'IFHR, NUMSTA, N, NREC: ', IFHR, NUMSTA, N, 
!     &                      IFHR*NUMSTA+N

!       NREC=(IFHR/INCR)*NUMSTA+N
       NREC=N

!	write(6,*) 'NREC, NLEN, FPACK: ', NREC, NLEN,
!     &                       (FPACK(NNN),NNN=1,NLEN,NLEN/5)


!	if (mod(NREC,20) .eq. 0) then
!	write(6,*) 'NREC, IHRST, IDAT, IFCST, ISTAT, CISTAT: ', 
!     &	NREC, IHRST, IDAT, IFCST, ISTAT, CISTAT
!	endif

      WRITE(LCLAS1,REC=NREC)IHRST,IDAT,IFCST,ISTAT,CISTAT
     1,                    (FPACK(NL),NL=1,NLEN)

C---------------------------------------------------------------------
 1000 CONTINUE
      CLOSE(LCLAS1)
	DEALLOCATE(T,Q,U,V,Q2,OMGALF,CWM,TRAIN,TCUCN)
	DEALLOCATE(RSWTT,RLWTT,CCR,RTOP,HTM,OMGA,p_hold)
	DEALLOCATE(t_hold,PINT,CLDFRA_BL)

       DEALLOCATE(DHCNVC,DHRAIN,STADHC,STADHR,TCUCN0,TRAIN0)
        DEALLOCATE(DUM,DUMMY,DUMMY2,DUM3D,DUM3D2,DUM3D3,GDLAT)
        DEALLOCATE(GDLON,PRODAT,FPACK,IDUM,LMH)

        DEALLOCATE(
     & RES,FIS,THS,HBOT
     &,CFRACL,CFRACM,CFRACH,SNO
     &,SOILTB,SFCEXC,SMSTAV,SMSTOT
     &,Z0,CZEN,CZMEAN,U00,SR
     &,ACPREC,CUPREC,ACSNOW,ACSNOM
     &,SSROFF,BGROFF,SFCSHX,SFCLHX
     &,SUBSHX,SNOPCX,ASWIN,ASWOUT
     &,ASWTOA,ALWIN,ALWOUT,ALWTOA
     &,TSHLTR,QSHLTR,TH2_hold
     &,TH10,Q10,U10,V10
     &,TLMIN,TLMAX
     &,SMC,CMC,STC,SH2O
     &,VEGFRC,POTFLX,PSLP,PDSL1
     &,EGRID2,SM,SICE
     &,HBM2,FACTR
     &,PTBL,TTBL
     &,STATPR,STACPR,STAEVP
     &,STAPOT,STASHX,STASUB,STAPCX
     &,STASWI,STASWO,STALWI,STALWO
     &,STALWT,STASWT,STASNM,STASRF
     &,STABRF,STASNO
     &,ACPREC0,CUPREC0,SFCLHX0,POTFLX0
     &,SFCSHX0,SUBSHX0,SNOPCX0,ASWIN0
     &,ASWOUT0,ALWIN0,ALWOUT0,ALWTOA0
     &,ASWTOA0,ACSNOW0,ACSNOM0,SSROFF0
     &,BGROFF0,TSK,SWEM,CANOPY)


C
C***  END OF PROFILE SITE LOOP
C
C***  END PROFILE POSTING CODE.
C---------------------------------------------------------------------
      RETURN
      END
