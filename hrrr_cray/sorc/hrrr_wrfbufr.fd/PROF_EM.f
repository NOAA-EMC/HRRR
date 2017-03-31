C      SUBROUTINE PROF(NHB,LRSTRT,ITAG,LCLAS1)
C      SUBROUTINE PROF(ITAG,LCLAS1)
      SUBROUTINE PROF_EM(filename,prefilename,startdate,
     +                   ITAG,INCR) 
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
C   06-07-14  B ZHOU - ADATP FOR SREF WRF-ARW  
C   07-08-07  J. Du & B. Zhou  - (1) Many extra fields were added;
C               (2) a new prefilename is defined for previous
C                   forecast file in order to calculate precip rate
C                   during INCHOUR interval
C   08-07-01  B ZHOU - CHANGED TO MPIIO 
C   08-10-10  J. Du and B. ZHOU - The order of vertical levels are reversed 
C                   for the new ARW model 
C   13-01-21  G MANIKIN - ADAPT CODE TO RUN ON WCOSS
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
       use kinds, only             : i_llong

      include 'wrf_io_flags.h'
      include 'mpif.h'

      INCLUDE "parmsoil"

C-----------------------------------------------------------------------
                             P A R A M E T E R
     &     (NSTAT=1500,LCL1ML=15,LCL1SL=52)
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
     &, G=9.81,GI=1./G,RD=287.04,CP=1004.6,CAPA=RD/CP,RHCRIT=0.9999,
     & TFRZ=273.15)

      PARAMETER (GAMMA=6.5/1000.,ZSL=0.0,D608=0.608)
      PARAMETER (TAUCR=RD*GI*290.66,CONST=0.005*G/RD)

C------------------------------------------------------------------------
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
     &,EGRID2(:),SM(:),SICE(:)
     &,HBM2(:),FACTR(:)
     &,PTBL(:,:),TTBL(:,:)
     &,STATPR(:),STACPR(:),STAEVP(:)
     &,STAPOT(:),STASHX(:),STASUB(:),STAPCX(:)
     &,STASWI(:),STASWO(:),STALWI(:),STALWO(:)
     &,STALWT(:),STASWT(:),STASNM(:),STASRF(:)
     &,STABRF(:),STASNO(:)
     &,ACPREC0(:),CUPREC0(:),SFCLHX0(:),POTFLX0(:)
     &,SFCSHX0(:),SUBSHX0(:),SNOPCX0(:),ASWIN0(:)
     &,ASWOUT0(:),ALWIN0(:),ALWOUT0(:),ALWTOA0(:)
     &,ASWTOA0(:),ACSNOW0(:),ACSNOM0(:),SSROFF0(:)
     &,BGROFF0(:)

C
      REAL, ALLOCATABLE:: T(:,:),Q(:,:),U(:,:),V(:,:),Q2(:,:)
     &,                   OMGALF(:,:),CWM(:,:),TRAIN(:,:),TCUCN(:,:)
     &,F_RAIN(:,:),F_ICE(:,:),CLDFRA(:,:)
     &,F_RIMEF(:,:)
     &,                   RSWTT(:,:),RLWTT(:,:),CCR(:,:),RTOP(:,:)
     &,                   HTM(:,:),OMGA(:,:),p_hold(:,:),t_hold(:,:)
     &,                   PINT(:,:),UL(:),PVAPOR(:),ZINT(:,:)


      REAL, ALLOCATABLE:: DHCNVC(:,:),DHRAIN(:,:),STADHC(:),STADHR(:),
     &                      TCUCN0(:,:),TRAIN0(:,:),CPRATE(:)


      REAL,ALLOCATABLE:: DUM(:,:,:),DUMMY(:,:),DUMMY2(:,:),
     &  DUM3D(:,:,:),DUM3D2(:,:,:),DUM3D3(:,:,:),GDLAT(:,:),GDLON(:,:),
     &  DUM3D_U(:,:,:),DUM3D_V(:,:,:),DUM3D_SOIL(:,:,:),DUM3DB(:,:,:)
      save dum3d2
      REAL, ALLOCATABLE:: PRODAT(:),FPACK(:)

      INTEGER, ALLOCATABLE:: IDUM(:,:),LMH(:,:),IW(:,:)

      REAL, ALLOCATABLE:: qqw(:,:),qqr(:,:),qqs(:,:),qqi(:,:),
     & qqg(:,:)

      INTEGER, allocatable:: IDUMMY(:,:)         !Binbin: new adding
      REAL,    allocatable:: DUM1D(:) 

	
	REAL:: STNLAT(NSTAT),STNLON(NSTAT)


       REAL, ALLOCATABLE :: PMID(:,:), W(:,:), WH(:,:),
     &                      pint_part(:),PDS(:)

	
C------------------------------------------------------------------------
                             I N T E G E R
     & IDSTN(NSTAT),IHINDX(NSTAT),JHINDX(NSTAT)
     &,             IVINDX(NSTAT),JVINDX(NSTAT),IDAT(3)
	INTEGER:: GDS(200)
C------------------------------------------------------------------------
                             L O G I C A L
     & RUN,RESTRT,FRST, PRINT_DIAG
C------------------------------------------------------------------------
                             C H A R A C T E R
     & RSTFIL*90,RESTHR*4,LABEL*32,CISTAT*8,CIDSTN(NSTAT)*8
     &,FNAME*90,ENVAR*90,BLANK*4

	CHARACTER(LEN=8), ALLOCATABLE :: CIDSTN_SAVE(:)

C	new stuff
      character(len=31) :: VarName,varin
	character(len=256) :: fileName
	character(len=256) :: prefileName       !new added by Jun Du
      integer :: Status, DataHandle, hor_size, hor_size_u, hor_size_v
      character(len=19):: startdate,datestr,datestrold
      character titlestring*80

	real:: rinc(5)
	integer:: IDATE(8),JDATE(8), IDATENEW(8)

      character*132, allocatable :: datestr_all(:)
      character*132, allocatable :: varname_all(:)
      integer, allocatable       :: domainend_all(:,:)
      integer, allocatable       :: start_block(:)
      integer, allocatable       :: end_block(:)
      integer, allocatable       :: start_byte(:)
      integer, allocatable       :: end_byte(:)
      integer(kind=i_llong), allocatable           :: file_offset(:)
      integer this_offset, this_length
      REAL SPVAL, SLDPTH2(NSOIL)

      character*2 cfhr    !add by B Zhou
C------------------------------------------------------------------------
      DATA BLANK/'    '/
      DATA SPVAL/-9999./
C------------------------------------------------------------------------
C***
C***  READ IN THE INFORMATION FILE ABOUT THE SOUNDINGS
C***

        print *, 'into PROF_EM' 
        write(6,*) 'filename= ', trim(filename)
        write(6,*) 'startedate= ', startdate
 101    format(f20.2)

	datestr=startdate

      REWIND 18
C
      print  *, 'about to read profdat'
      READ(18)NUMSTA,IDSTN,STNLAT,STNLON
     1,       IHINDX,JHINDX,IVINDX,JVINDX,CIDSTN
	
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
	else
      WRITE(6,30)(IDSTN(N),STNLAT(N)/DTR,STNLON(N)/DTR
     1,               IHINDX(N),JHINDX(N),IVINDX(N),JVINDX(N)
     2,               CIDSTN_SAVE(N),N=1,NUMSTA,10)

	endif
   30 FORMAT(2X,I6,2F8.2,4I8,4X,A8)

      write(*,*) 'ITAG=', ITAG

      FRST=.TRUE.

!--------------------------------------------------------------------
!--------------------------------------------------------------------
!--------------------------------------------------------------------

       if ( frst ) then
         frst = .false.
         CALL ext_int_ioinit(Status)
          print*,'CALLed ioinit', Status
	write(6,*) 'filename early in PROF= ', trim(filename)
         CALL ext_int_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
          print*,'CALLed open for read', Status
       else
           Status = 0
       endif

       if ( Status /= 0 ) then
         print*,'error opening ',trim(fileName), ' Status = ', Status ; stop
       endif

        call ext_int_get_dom_ti_char(DataHandle,'TITLE',
     &        titlestring,status)

C Getting start time
      CALL ext_int_get_dom_ti_char(DataHandle
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
      print*,' ifhr fileName=',ifhr,trim(fileName)

        call ext_int_get_dom_ti_integer(DataHandle,
     &   'WEST-EAST_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'west-east dimension: ', itmp
        IM=itmp-1

        call ext_int_get_dom_ti_integer(DataHandle,
     &   'SOUTH-NORTH_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'south-north dimension: ', itmp
        JM=itmp-1
        call ext_int_get_dom_ti_integer(DataHandle,
     &   'BOTTOM-TOP_GRID_DIMENSION',itmp
     + ,1,ioutcount,istatus)

        write(6,*) 'bottom-top dimension: ', itmp
        LM=itmp-1

	write(6,*) 'to big allocate block'

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
       ALLOCATE(STABRF(NUMSTA),STASNO(NUMSTA),ACPREC0(NUMSTA))   !Binbin: from this, need 2 files
       ALLOCATE(CUPREC0(NUMSTA),SFCLHX0(NUMSTA),POTFLX0(NUMSTA))     !to compute
       ALLOCATE(SFCSHX0(NUMSTA),SUBSHX0(NUMSTA),SNOPCX0(NUMSTA))
       ALLOCATE(ASWIN0(NUMSTA),ASWOUT0(NUMSTA),ALWIN0(NUMSTA))
       ALLOCATE(ALWOUT0(NUMSTA),ALWTOA0(NUMSTA),ASWTOA0(NUMSTA))
       ALLOCATE(ACSNOW0(NUMSTA),ACSNOM0(NUMSTA),SSROFF0(NUMSTA))
       ALLOCATE(BGROFF0(NUMSTA))
       ALLOCATE(F_RAIN(NUMSTA,LM),F_ICE(NUMSTA,LM),CLDFRA(NUMSTA,LM))
       ALLOCATE(F_RIMEF(NUMSTA,LM))


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

	ALLOCATE(STADHC(LM))
	ALLOCATE(STADHR(LM))
        ALLOCATE(CPRATE(LM))
	ALLOCATE(DHRAIN(NUMSTA,LM))
	ALLOCATE(DHCNVC(NUMSTA,LM))
	ALLOCATE(TCUCN0(NUMSTA,LM))
	ALLOCATE(TRAIN0(NUMSTA,LM))


! former parameter statements
        NWORDM=(LCL1ML+1)*LM+2*LCL1SL
        LRECPR=4*(8+9+LCL1ML*LM+LCL1SL)
! former parameter statements

	if (allocated(FPACK)) deallocate(FPACK); allocate(FPACK(NWORDM))
	if (allocated(PRODAT)) deallocate(PRODAT); 
     &                  allocate(PRODAT(NWORDM))

        write(6,*) 'allocate with IM, JM, LM: ', IM, JM, LM
        JS=1
        JE=JM
        JEV=JM+1

        if (ALLOCATED(DUM)) deallocate(DUM);
     &                          allocate(DUM(IM,JM,4))
        if (ALLOCATED(DUMMY)) deallocate(DUMMY);
     &                          allocate(DUMMY(IM,JM))
        if (ALLOCATED(IDUMMY)) deallocate(IDUMMY);
     &                          allocate(IDUMMY(IM,JM))
        if (ALLOCATED(DUMMY2)) deallocate(DUMMY2);
     &                          allocate(DUMMY2(IM,JM))
       if (ALLOCATED(DUM1D)) deallocate(DUM1D);
     &                          allocate(DUM1D(LM+1))
        if (ALLOCATED(DUM3D)) deallocate(DUM3D);
     &                          allocate(DUM3D(IM,JM,LM))
       if (ALLOCATED(DUM3DB)) deallocate(DUM3DB);
     &                          allocate(DUM3DB(IM,JM,LM))
        if (ALLOCATED(DUM3D_SOIL)) deallocate(DUM3D_SOIL);
     &                          allocate(DUM3D_SOIL(IM,JM,NSOIL))
        if (ALLOCATED(DUM3D_U)) deallocate(DUM3D_U);
     &                          allocate(DUM3D_U(IM+1,JM,LM))
        if (ALLOCATED(DUM3D_V)) deallocate(DUM3D_V);
     &                          allocate(DUM3D_V(IM,JM+1,LM))
        if (ALLOCATED(DUM3D2)) deallocate(DUM3D2);
     &                          allocate(DUM3D2(IM,JM,LM+1))
        if (ALLOCATED(DUM3D3)) deallocate(DUM3D3);
     &                          allocate(DUM3D3(IM,JM,LM+1))
        if (ALLOCATED(GDLAT)) deallocate(GDLAT);
     &                          allocate(GDLAT(IM,JM))
        if (ALLOCATED(GDLON)) deallocate(GDLON);
     &                          allocate(GDLON(IM,JM))
        if (ALLOCATED(IDUM)) deallocate(IDUM);
     &                          allocate(IDUM(IM,JM))
        if (ALLOCATED(LMH)) deallocate(LMH);
     &                          allocate(LMH(IM,JM))

        allocate(qqw(NUMSTA,LM+1))
        allocate(qqr(NUMSTA,LM+1))
        allocate(qqs(NUMSTA,LM+1))
        allocate(qqi(NUMSTA,LM+1))
        allocate(qqg(NUMSTA,LM+1))
        imp_physics=3

        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval

        call ext_int_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
        DT=tmp
        print*,'DT= ',DT

        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        TPH0D=cenlat/1000.
        write(6,*) 'cenlat= ', cenlat
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        TLM0D=cenlon/1000.
        write(6,*) 'cenlon= ', cenlon
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=nint(1000.*tmp)
        write(6,*) 'truelat1= ', truelat1
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=nint(1000.*tmp)
        write(6,*) 'truelat2= ', truelat2
        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype


c closing wrf io api

!      call ext_int_ioclose ( DataHandle, Status )

      hor_size=IM*JM
      hor_size_u=(IM+1)*(JM)
      hor_size_v=(IM)*(JM+1)


! get 3-D variables
      print*,'im,jm,lm= ',im,jm,lm
c
      VarName='LU_INDEX'
        write(6,*) 'call getIVariable for : ', VarName
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
     +  IM,1,JM,1,IM,JS,JE,1)

       varname='ZNU'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,LM+1,1,1,1,LM)

       varname='ZNW'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,LM+1,1,1,1,LM+1)

       varname='ZS'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,SLDPTH2,       &
     +  NSOIL,1,1,1,NSOIL,1,1,1)

       varname='DZS'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,SLDPTH2,       &
     +  NSOIL,1,1,1,NSOIL,1,1,1)

      VarName='U'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D_U,      &
     + IM+1,1,JM,LM,IM+1,JS,JE,LM)

          DO L = 1,LM
           DO N=1,NUMSTA
            I=IVINDX(N)
            J=JVINDX(N)
            U(N,L) = DUM3D_U(I,J,L)
        if (N .eq. 5) write(6,*) 'L, U(N,L): ', L, U(N,L)
           END DO
          END DO

      VarName='V'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D_V,      &
     + IM,1,JM+1,LM,IM,JS,JE,LM)
          DO L = 1,LM
           DO N=1,NUMSTA
            I=IVINDX(N)
            J=JVINDX(N)
            V(N,L) = DUM3D_V(I,J,L)
        if (N .eq. 5) write(6,*) 'L, V(N,L): ', L, V(N,L)
           END DO
          END DO

        write(6,*) 'V: ', DUM3D_V(20,20,20)
      VarName='W'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D2,       &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)
          DO L = 1,LM+1
           DO N=1,NUMSTA
            I=IVINDX(N)
            J=JVINDX(N)
            W(N,L) = DUM3D2(I,J,L)
           END DO

          END DO

        write(6,*) 'W: ', DUM3D(20,20,20)

      DO L = 1,LM
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
            WH(N,L) = (W(N,L)+W(N,L+1))*0.5
      END DO
      END DO

      VarName='PH'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D2,      &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)

      VarName='PHB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D3,       &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)

!bad       goto 979


        allocate(ZINT(NUMSTA,LM+1))

      DO L=1,LM+1
      DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         ZINT(N,L)=(DUM3D2(I,J,L)+DUM3D3(I,J,L))/G
        if (N .eq. 5) print *, ' L, ZINT(L), pieces: ', L, ZINT(N,L),
     +              DUM3D2(I,J,L), DUM3D3(I,J,L), G
       ENDDO
      ENDDO


      VarName='T'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

!          DO L = 1,LM+1
!           DO N=1,NUMSTA
!            I=IVINDX(N)
!            J=JVINDX(N)
!            t_hold(N,L) = DUM3D(I,J,L)
!           END DO
!          END DO

      do l = 1, lm
       do N = 1, NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
             t_hold ( N , L ) = dum3d ( i, j, l ) + 300.
        end do
       end do

      VarName='T_INIT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM+1,1,1,1,8)

! reading sfc pressure
      VarName='MU'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
      VarName='MUB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

! t_hold has potential temperature here

  633   format(15(f6.0,1x))

!bad        goto 979
        if (allocated(pint_part)) deallocate(pint_part)
        allocate(pint_part(NUMSTA))

        if (allocated(PDS)) deallocate(PDS)
        allocate(PDS(NUMSTA))

        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         pint_part(N)=DUMMY(I,J)+DUMMY2(I,J)
        if (CIDSTN_SAVE(N) .eq. 'KOAK    ') then
        write(6,*) 'KOAK, MU,MUB,pint_part: ',DUMMY(I,J),DUMMY2(I,J),
     &         pint_part(N)
        endif
        if (CIDSTN_SAVE(N) .eq. 'BLU     ') then
        write(6,*) 'BLU, MU,MUB,pint_part: ',DUMMY(I,J),DUMMY2(I,J),
     &          pint_part(N)
        endif
        ENDDO


      VarName='MU0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='P'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3DB,     &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='PB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

        write(6,*) 'to PMID alloc statements'
        if (allocated(PMID)) deallocate(PMID)
        allocate(PMID(NUMSTA,LM))

        DO L=1,LM
        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         PMID(N,L)=DUM3D(I,J,L)+DUM3DB(I,J,L)
         if (N .eq. 5) print *, ' L, PMID(L): ', L, PMID(N,L)
        ENDDO
        ENDDO

      VarName='FNM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='FNP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='RDNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='RDN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='DNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='DN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='T_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='CFN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CFN1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='STEP_NUMBER'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='P_HYD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='Q2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
      DO N=1,NUMSTA
        QSHLTR(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      VarName='T2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TH2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
      DO N=1,NUMSTA
        TH2_hold(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      VarName='PSFC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='U10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        U10(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO
!
      VarName='V10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        V10(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO

      VarName='WSPD10MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_UP_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_DN_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='REFD_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UP_HELI_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UP_HELI_MAX16'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_MEAN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRPL_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG1_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG2_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG3_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_LTG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_LTG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_W'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_W'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_WQ'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_WQ'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_REFD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_REFD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QR_MAX_CI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QG_MAX_CI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1) 

      VarName='UH16'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RDX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='RDY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='DTS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='DTSEPS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='RESM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='ZETATOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF3'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='ITIMESTEP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
     +  1,1,1,1,1,1,1,1)

      VarName='XTIME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='QVAPOR'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

        DO L=1,LM
        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         Q(N,L)=DUM3D(I,J,L)/(1.0+DUM3D(I,J,L))
        ENDDO
        ENDDO

      VarName='QCLOUD'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

        DO L = 1, LM
        DO N=1,NUMSTA
          Q2(N,L)=0.
          CWM(N,L)=DUM3D(IHINDX(N),JHINDX(N),L)
        ENDDO
        ENDDO

      VarName='QRAIN'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QICE'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QSNOW'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QGRAUP'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

       VarName='QNICE'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QNRAIN'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='qke_adv'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FCX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GCX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='DTBC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSTDV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSLPX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSLPY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SHDMAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SHDMIN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOALB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SLOPECAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SOILHGT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LANDUSEF'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,21,IM,JS,JE,21)

      VarName='SOILCTOP'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,16,IM,JS,JE,16)

      VarName='SOILCBOT'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,16,IM,JS,JE,16)

      VarName='SOILCAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,       &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGCAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,       &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TSLB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      DO L = 1, NSOIL
        DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
            STC(N,L) = DUM3D(I,J,NSOIL-L+1)
        if (N .eq. 5) write(6,*) 'L, STC(N,L): ', L, STC(N,L)
        END DO
      END DO
!
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        TH10(N)=-9999.
        Q10(N)=-9999.
       END DO

      VarName='SMOIS'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      DO L = 1, NSOIL
        DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SMC(N,L) = DUM3D(I,J,NSOIL-L+1)
        if (N .eq. 5) write(6,*) 'L, SMC(N,L): ', L, SMC(N,L)
        ENDDO
      ENDDO

      VarName='SH2O'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      DO L = 1, NSOIL
        DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SH2O(N,L)= DUM3D(I,J,NSOIL-L+1)
        END DO
      END DO

        write(6,*) 'past soil'

      VarName='SMCREL'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      VarName='SEAICE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SICE(N)=DUMMY(I,J)
      ENDDO

      VarName='SFROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SSROFF(N)=DUMMY(I,J)
      ENDDO

      VarName='UDROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        BGROFF(N)=DUMMY(I,J)
      ENDDO

      VarName='IVGTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='ISLTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGFRA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        VEGFRC(N)=DUMMY(I,J)
      ENDDO

      VarName='GRDFLX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACSNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ACSNOW(N)=DUMMY(I,J)
      ENDDO

      VarName='ACSNOM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ACSNOM(N)=DUMMY(I,J)
      ENDDO

      VarName='SNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SNO(N)=DUMMY(I,J)
      ENDDO

      VarName='SNOWH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        CMC(N)=DUMMY(I,J)
      ENDDO

      VarName='SST'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FNDSNOWH'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

       VarName='FNDSOILW'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SSTSK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAD_TTEN_DFI'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_1'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_2'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_3'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_4'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='TTEN_TIMES'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='LAI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='THZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='Z0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
!
      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        Z0(N)=DUMMY(I,J)
      ENDDO

      VarName='QKE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CUTOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CUBOT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_M'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
  
      VarName='MAPFAC_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_MX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_MY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
   
      VarName='MAPFAC_UX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_UY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_VX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MF_VX_INV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
  
      VarName='MAPFAC_VY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='F'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='E'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SINALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='COSALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HGT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        RES(N)=1.0
        FIS(N)=DUMMY(IHINDX(N),JHINDX(N))*G
      ENDDO

      VarName='TSK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='U_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='V_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1) 

      VarName='QV_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='Z_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='U_FRAME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='V_FRAME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1) 

      VarName='P_TOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,             &
     +          PT,1,1,1,1,1,1,1,1)

        print*, 'PT is: ', PT


      VarName='T00'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='P00'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='TLP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='TISO'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='MAX_MSTFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='MAX_MSTFY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,      &
     +  1,1,1,1,1,1,1,1)


      VarName='RAINC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        CUPREC(N)=DUMMY(IHINDX(N),JHINDX(N))*.001
        ACPREC(N)=( DUMMY(IHINDX(N),JHINDX(N))+
     &              DUMMY2(IHINDX(N),JHINDX(N)) )*.001
      ENDDO

        write(0,*) 'max of ACPREC: ', maxval(ACPREC)


      VarName='PRATEC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRAUPELNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HAILNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRAUPELNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLDFRA'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

        DO L = 1, LM
        DO N=1,NUMSTA
          CLDFRA(N,L)=DUM3D(IHINDX(N),JHINDX(N),L)
        ENDDO
        ENDDO

      VarName='SWDOWN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

        write(6,*) 'max SWDOWN: ', maxval(DUMMY)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ASWIN(N)=DUMMY(I,J)
      ENDDO

      VarName='GSW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GLW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

        write(6,*) 'max GLW: ', maxval(DUMMY)


      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ALWIN(N)=DUMMY(I,J)
        if (IDSTN(N) .eq. 832) then
        write(6,*) 'for station 832, found ASWIN, ASWOUT: ',
     &                             ASWIN(N), ASWOUT(N)
        endif
      ENDDO

      VarName='OLR'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      do j = 1, jm
        do i = 1, im
            GDLAT ( i, j ) = DUMMY ( i, j )
        end do
       end do


      VarName='XLONG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

       do j = 1, jm
        do i = 1, im
            GDLON ( i, j ) = DUMMY ( i, j )
        end do
       end do

      VarName='XLAT_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ALBEDO'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLONG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ALBBCK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TMN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SOILTB(N)=DUMMY(I,J)
      ENDDO


! XLAND 1 land 2 sea
      VarName='XLAND'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SM(N)=DUMMY(I,J)-1.0
      ENDDO

      VarName='ZNT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UST'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='PBLH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCSHX(N)=-1.0*DUMMY(I,J)
      ENDDO

      VarName='QFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCLHX(N)=-1.0*DUMMY(I,J)
      ENDDO

      VarName='ACHFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACLHF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FLHC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QVG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QCG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SOILT1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
 
      VarName='SNOWC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        SMSTAV(N)=SPVAL
      ENDDO

      DO N=1,NUMSTA
        SMSTOT(N)=SPVAL
      ENDDO

      VarName='MAVAIL'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SR'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

        DO L=1,LM
        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         T(N,L)=t_hold(N,L)*(PMID(N,L)*1.e-5)**CAPA
        if (N .eq. NUMSTA/2) then
        write(6,*) 'N,L,PMID,t_hold,T:', N,L,PMID(N,L),
     +              t_hold(N,L),T(N,L)
        endif

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

!HERENOW

!!!! need to do the qvapor fix - integrated contribution of
!!!! moisture to the surface pressure

        allocate(PVAPOR(NUMSTA))
!        allocate(ZINT(NUMSTA,LM+1))

       DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         PVAPOR(N)=0.
       do L=1,LM
       dz=ZINT(N,L+1)-ZINT(N,L)
       rho=PMID(N,L)/(RD*T(N,L))

        if (L .le. LM-1) then
        QMEAN=0.5*(Q(N,L)+Q(N,L+1))
        else
        QMEAN=Q(N,L)
        endif

c       if (mod(L,5) .eq. 0 .and. mod(N,20) .eq. 0) then
c        write(6,*) 'N, L, dz, rho, qmean, increm: ',
c     &        N, L, dz, rho, qmean, G*rho*dz*QMEAN
c       endif

       pvapor(N)=pvapor(N)+G*rho*dz*QMEAN
       enddo

        ENDDO

!!!! end vapor fix

!!!
!!! ?????????????????????
!!!
        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         PINT (N,LM+1)=PT
         PINT (N,1)=pint_part(N)+PT+PVAPOR(N)

        PDS(N)=pint_part(N)+PT+PVAPOR(N)
        TSHLTR(N)=TH2_hold(N)*(PINT(N,1)/100000.)**CAPA

        HBOT(N)=-9999.
        CFRACL(N)=-9999.

!!! constrain surface RH

           QC=(PQ0/PINT(N,1))*EXP(A2*(TSHLTR(N)-A3)/(TSHLTR(N)-A4))
           RH=QSHLTR(N)/QC
           IF (RH .gt. RHCRIT) THEN
           write(6,*) 'reducing surface RH from: ', RH, ' at N: ', N
           QSHLTR(N)=0.999*RHCRIT*QC
           ENDIF

        ENDDO


ccc------------------------------------------------------------------------------


C** Compute PSLP using NMC reduction
C
      DO N=1,NUMSTA
       PSFC = PDS(N)
       ZSFC = FIS(N)*GI
       PSLP(N) = PSFC

!       write(0,*) 'N, ZSFC, PSFC: ', N, ZSFC, PSFC
C
C    COMPUTE LAYER TAU (VIRTUAL TEMP*RD/G).
Cwrong       TVRT = T(N,1)*(1.0+D608*Q(N,1))
       TVRT = T(N,LM)*(1.0+D608*Q(N,LM))
       if (mod(N,50) .eq. 0) then
       write(0,*) 'N, T(N,LM), Q(N,LM), TVRT: ',
     &          N, T(N,LM), Q(N,LM), TVRT
       endif

!        write(0,*) 'N, TVRT: ', N, TVRT
       TAU  = TVRT*RD*GI
C
C    COMPUTE TAU AT THE GROUND (Z=ZSFC) AND SEA LEVEL (Z=0)
C    ASSUMING A CONSTANT LAPSE RATE OF GAMMA=6.5DEG/KM.
       TVRSFC = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSFC = TVRSFC*RD*GI
       TVRSL  = TVRT + (ZSFC- ZSL)*GAMMA
       TAUSL  = TVRSL*RD*GI
C
C    IF NEED BE APPLY SHEULL CORRECTION.
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
c      print *,n,idstn(n),pslp(n),tvrt
        if (mod(N,25) .eq. 0) then
C        write(0,*) 'N, PDS(N), PSLP(N): ', N, PDS(N),TAUAVG, PSLP(N)
        endif

      ENDDO

      write(*,*) 'TSHLTR=',TSHLTR(1)
      write(*,*) 'U10=',U10(1)
      write(*,*) 'V10=',V10(1)
      write(*,*) 'ACSNOW=',ACSNOW(1)
      write(*,*) 'ACSNOM=',ACSNOM(1)
      write(*,*) 'CMC=',CMC(1)
      write(*,*) 'SMSTAV=',SMSTAV(1)
      write(*,*) 'VEGFRC=',VEGFRC(1)
      write(*,*) 'CUPREC=',CUPREC(1)
      write(*,*) 'ACPREC=',ACPREC(1)
      write(*,*) 'GDLAT=', GDLAT(IHINDX(1),JHINDX(1))
      write(*,*) 'GDLON=', GDLON(IHINDX(1),JHINDX(1))
      write(*,*) 'SM=',SM(1)


!!!!!!!!!!!!!!!!! END INSERT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

!!! make PINT(1)=sfc
!!!
!!! ?????????????????????
!!!
        DO N=1,NUMSTA
         I=IHINDX(N)
         J=JHINDX(N)
         PINT (N,LM+1)=PT
         PINT (N,1)=pint_part(N)+PT+PVAPOR(N)

        PDS(N)=pint_part(N)+PT+PVAPOR(N)
        TSHLTR(N)=TH2_hold(N)*(PINT(N,1)/100000.)**CAPA

        HBOT(N)=-9999.
        CFRACL(N)=-9999.

!!! constrain surface RH

           QC=(PQ0/PINT(N,1))*EXP(A2*(TSHLTR(N)-A3)/(TSHLTR(N)-A4))
           RH=QSHLTR(N)/QC
           IF (RH .gt. RHCRIT) THEN
           write(6,*) 'reducing surface RH from: ', RH, ' at N: ', N
           QSHLTR(N)=0.999*RHCRIT*QC
           ENDIF

        ENDDO


C***
C***  READ QUANTITIES NEEDED FROM THE NHB FILE
C***

      DO N=1,NUMSTA
!       HBM2(N)=DUM(IHINDX(N),JHINDX(N),1)
       HBM2(N)=1.0
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
C	GET ROTATION ANGLES FOR WIND
C

	write(6,*) 'numsta= ', numsta

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
cBinbin	Z0(N)=-9999.
        HBOT(N)=-9999.
        CFRACL(N)=-9999.
        CFRACM(N)=-9999.
	CZEN(N)=-9999.
      ENDDO


      DO N=1,NUMSTA
	CFRACH(N)=-9999.
	CZMEAN(N)=-9999.
	U00(N)=-9999.
	SR(N)=0.0
        Z0(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
       SUBSHX(N)=-9999.
       SNOPCX(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
        ASWTOA(N)=-9999.
        ALWOUT(N)=-9999.
        ALWTOA(N)=-9999.
      ENDDO

      DO N=1,NUMSTA
	POTFLX(N)=-9999.
!	TLMIN(N)=-9999.
!	TLMAX(N)=-9999.
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
C------------------------------------------------------------------------
C***  THE FORECAST HOUR
C***  

	write(6,*) 'past block of code'
	IFHR=ITAG
	write(6,*) 'IFHR: ', IFHR
C------------------------------------------------------------------------

      IF(ITAG.GT.0)THEN

	write(6,*) 'working on preceding file'

C***  GENERATE THE NAME OF THE PRECEDING RESTRT FILE
C***
        ITAG0=ITAG-INCR
        RINC(1)=0.
        RINC(2)=float(ITAG0)
        RINC(3)=0.
        RINC(4)=0.
        RINC(5)=0.
        write(6,*) 'RINC(2): ', rinc(2)

cZhou        call w3movdat(rinc,idate,jdate)
cZhou        write(DateStrold,301) JDATE(1),JDATE(2),JDATE(3),JDATE(5)
cZhou 301  format(i4,'-',i2.2,'-',i2.2,'_',i2.2,':00:00')
cZhou        write(6,*) 'filename later in PROF: ', filename, '_END'
cZhou        len=index(filename,' ')-1
cZhou        write(6,*) 'LEN= ', LEN
cZhou        write(6,*) 'carried over part: ', filename(1:len-19)
cZhou        filename=filename(1:len-19)//DateStrold

         if(ITAG0.lt.10) then
          write(cfhr,'(1a,i1)') '0', ITAG0
         else
          write(cfhr,'(i2)')  ITAG0
         end if

         len=lnblnk(filename)          !call C code which has been compiled to object file

         filename=prefilename(1:len)
cDu.Jun   filename=filename(1:len-2)//cfhr
         write(*,*) 'Previous file=', trim(filename)

         write(6,*) 'date for old file is: ', datestrold


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INSERT READ FROM ABOVE ONCE WORK OUT KINKS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         CALL ext_int_ioinit(Status)
        write(6,*) 'filename early in PROF= ', trim(filename)

         CALL ext_int_open_for_read( trim(fileName), 0, 0, " ",
     &  DataHandle, Status)
         
       if ( Status /= 0 ) then
         print*,'error opening ',trim(fileName), ' Status = ', Status ; stop
       endif
        
        call ext_int_get_dom_ti_char(DataHandle,'TITLE',
     &        titlestring,status)
 
C Getting start time
      CALL ext_int_get_dom_ti_char(DataHandle
     1 ,'START_DATE',startdate, status )
        
c20080701        call ext_int_get_dom_ti_integer(DataHandle,'MP_PHYSICS'
c20080701     + ,itmp,1,ioutcount,istatus)

        imp_physics=3
c20080701        print*,'MP_PHYSICS= ',imp_physics

        call ext_int_get_dom_ti_real(DataHandle,'DX',tmp
     + ,1,ioutcount,istatus)
        dxval=nint(tmp)
        write(6,*) 'dxval= ', dxval
        call ext_int_get_dom_ti_real(DataHandle,'DY',tmp
     + ,1,ioutcount,istatus)
        dyval=nint(tmp)
        write(6,*) 'dyval= ', dyval

        call ext_int_get_dom_ti_real(DataHandle,'DT',tmp
     +    ,1,ioutcount,istatus)
        DT=tmp
        print*,'DT= ',DT

        call ext_int_get_dom_ti_real(DataHandle,'CEN_LAT',tmp
     + ,1,ioutcount,istatus)
        cenlat=nint(1000.*tmp)
        TPH0D=cenlat/1000.
        write(6,*) 'cenlat= ', cenlat
        call ext_int_get_dom_ti_real(DataHandle,'CEN_LON',tmp
     + ,1,ioutcount,istatus)
        cenlon=nint(1000.*tmp)
        TLM0D=cenlon/1000.
        write(6,*) 'cenlon= ', cenlon
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT1',tmp
     + ,1,ioutcount,istatus)
        truelat1=nint(1000.*tmp)
        write(6,*) 'truelat1= ', truelat1
        call ext_int_get_dom_ti_real(DataHandle,'TRUELAT2',tmp
     + ,1,ioutcount,istatus)
        truelat2=nint(1000.*tmp)
        write(6,*) 'truelat2= ', truelat2
        call ext_int_get_dom_ti_integer(DataHandle,'MAP_PROJ',itmp
     + ,1,ioutcount,istatus)
        maptype=itmp
        write(6,*) 'maptype is ', maptype
      VarName='LU_INDEX'
        write(6,*) 'call getIVariable for : ', VarName
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
     +  IM,1,JM,1,IM,JS,JE,1)

       varname='ZNU'
        write(6,*) 'call getVariableB for : ', VarName
        write(6,*) 'size(DUM1D): ', size(DUM1D)
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,LM+1,1,1,1,LM)

       varname='ZNW'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,LM+1,1,1,1,LM+1)

       varname='ZS'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,SLDPTH2,       &
     +  NSOIL,1,1,1,NSOIL,1,1,1)

       varname='DZS'
        write(6,*) 'call getVariableB for : ', VarName
      call getVariableB(fileName,DateStr,DataHandle,VarName,SLDPTH2,       &
     +  NSOIL,1,1,1,NSOIL,1,1,1)

      VarName='U'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D_U,      &
     + IM+1,1,JM,LM,IM+1,JS,JE,LM)

      VarName='V'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D_V,      &
     + IM,1,JM+1,LM,IM,JS,JE,LM)

      VarName='W'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D2,       &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)

      VarName='PH'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D2,      &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)

      VarName='PHB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D3,       &
     +  IM,1,JM,LM+1,IM,JS,JE,LM+1)

      VarName='T'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='T_INIT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM+1,1,1,1,8)
 
      VarName='MU'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MUB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)
 
       VarName='MU0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='P'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3DB,     &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='PB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='FNM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='FNP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='RDNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='RDN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='DNW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

           VarName='DN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='T_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,LM,1,1,1,LM)

      VarName='CFN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CFN1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='STEP_NUMBER'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='P_HYD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='Q2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='T2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TH2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='PSFC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

           VarName='U10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        U10(N)=DUMMY(IHINDX(N),JHINDX(N))
      ENDDO
!
      VarName='V10'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='WSPD10MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_UP_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_DN_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='REFD_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UP_HELI_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UP_HELI_MAX16'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='W_MEAN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

            VarName='GRPL_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG1_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG2_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LTG3_MAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_LTG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_LTG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_W'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_W'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_WQ'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_WQ'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCI_REFD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='NCA_REFD'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QR_MAX_CI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QG_MAX_CI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UH16'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RDX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='RDY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='DTS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='DTSEPS'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='RESM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='ZETATOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF2'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='CF3'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='ITIMESTEP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY,     &
     +  1,1,1,1,1,1,1,1)

      VarName='XTIME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM1D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='QVAPOR'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QCLOUD'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QRAIN'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QICE'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QSNOW'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QGRAUP'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

       VarName='QNICE'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='QNRAIN'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='FCX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GCX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='DTBC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,         &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LANDMASK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSTDV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSLPX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TOPOSLPY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SHDMAX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SHDMIN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOALB'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SLOPECAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SOILHGT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LANDUSEF'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,21,IM,JS,JE,21)

      VarName='SOILCTOP'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,16,IM,JS,JE,16)

      VarName='SOILCBOT'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,16,IM,JS,JE,16)

      VarName='SOILCAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,       &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGCAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,       &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TSLB'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,    &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      VarName='SMOIS'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      VarName='SH2O'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      VarName='SMCREL'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,       &
     +  IM,1,JM,LM,IM,JS,JE,NSOIL)

      VarName='SEAICE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SFROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SSROFF0(N)=DUMMY(I,J)
      ENDDO
 
      VarName='UDROFF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        BGROFF0(N)=DUMMY(I,J)
      ENDDO

      VarName='IVGTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='ISLTYP'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='VEGFRA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRDFLX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACSNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ACSNOW0(N)=DUMMY(I,J)
      ENDDO

      VarName='ACSNOM'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ACSNOM0(N)=DUMMY(I,J)
      ENDDO

      VarName='SNOW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CANWAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SST'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FNDSNOWH'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FNDSOILW'
      call getIVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
 
      VarName='SSTSK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAD_TTEN_DFI'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_1'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_2'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_3'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='RAD_TTEN_DFI_4'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='TTEN_TIMES'
       call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,  &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='LAI'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TKE_MYJ'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,   &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='THZ0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='Z0'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CUTOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CUBOT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_M'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_MX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_MY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_UX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_UY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_VX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MF_VX_INV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAPFAC_VY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='F'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='E'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SINALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='COSALPHA'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HGT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TSK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='U_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='V_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QV_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='Z_BASE'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='U_FRAME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='V_FRAME'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='P_TOP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,             &
     +          PT,1,1,1,1,1,1,1,1)

      VarName='T00'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='P00'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)
   
      VarName='TLP'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='TISO'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,       &
     +  1,1,1,1,1,1,1,1)

      VarName='MAX_MSTFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,      &
     +  1,1,1,1,1,1,1,1)

      VarName='MAX_MSTFY'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUM0D,      &
     +  1,1,1,1,1,1,1,1)


      VarName='RAINC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        CUPREC0(N)=DUMMY(IHINDX(N),JHINDX(N))*.001
        ACPREC0(N)=( DUMMY(IHINDX(N),JHINDX(N))+
     &              DUMMY2(IHINDX(N),JHINDX(N)) )*.001
      ENDDO


      VarName='PRATEC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,IDUMMY      &
     +  ,IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='RAINNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY2,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRAUPELNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HAILNC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GRAUPELNCV'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLDFRA'
      call getVariableBikj(fileName,DateStr,DataHandle,VarName,DUM3D,      &
     +  IM,1,JM,LM,IM,JS,JE,LM)

      VarName='SWDOWN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='GSW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ASWIN0(N)=DUMMY(I,J)
        if (IDSTN(N) .eq. 832) then
        write(6,*) 'for station 832, found ASWIN, ASWOUT: ',
     &                             ASWIN(N), ASWOUT(N)
        endif
      ENDDO

      VarName='GLW'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        ALWIN0(N)=DUMMY(I,J)
        if (IDSTN(N) .eq. 832) then
        write(6,*) 'for station 832, found ASWIN, ASWOUT: ',
     &                             ASWIN(N), ASWOUT(N)
        endif
      ENDDO

      VarName='OLR'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG_U'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAT_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLONG_V'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ALBEDO'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLAT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='CLONG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ALBBCK'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='TMN'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='XLAND'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ZNT'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='UST'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='PBLH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='HFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCSHX0(N)=-1.0*DUMMY(I,J)
      ENDDO

      VarName='QFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='LH'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      DO N=1,NUMSTA
        I=IHINDX(N)
        J=JHINDX(N)
        SFCLHX0(N)=-1.0*DUMMY(I,J)
      ENDDO

       VarName='ACHFX'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='ACLHF'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='FLHC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QVG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='QCG'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SOILT1'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SNOWC'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='MAVAIL'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)

      VarName='SR'
      call getVariableB(fileName,DateStr,DataHandle,VarName,DUMMY,      &
     +  IM,1,JM,1,IM,JS,JE,1)
 
      write(6,*) 'done reading old file'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! END INSERT READ
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        do L=1,LM
         DO N=1,NUMSTA
          TRAIN0(N,L)=-9999.
	  TCUCN0(N,L)=-9999.
         ENDDO
        enddo

        DO N=1,NUMSTA
          SFCSHX0(N)=-9999.
          SFCLHX0(N)=-9999.
          SUBSHX0(N)=-9999.
          SNOPCX0(N)=-9999.
        ENDDO

        DO N=1,NUMSTA
          ASWTOA0(N)=-9999.
          ALWOUT0(N)=-9999.
          ALWTOA0(N)=-9999.
          POTFLX0(N)=-9999.
        ENDDO

      ENDIF

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
        PDSL1(N)=pint_part(N)*RES(N)
      ENDDO

C
C------------------QW, QI AND QINT--------------------------------------
C

!!!
!!! skip section for now
!!!

      DO 220 L=2,LM
      DO 210 N=1,NUMSTA


        IW(N,L)=-9999
        CCR(N,L)=-9999.

C
C-------------------ICE-WATER ID NUMBER IW------------------------------
C
!       write(6,*) 'here d'
  210                 CONTINUE
  220                 CONTINUE
  221                 continue


C----------------------------------------------------------------------
C***
C***  BEGIN THE PROFILE POSTING CODE.
C----------------------------------------------------------------------
C***
C***  USE ZERO IN ACCUMULATION ARRAYS AT APPROPRIATE TIMES
C***
       IF(ITAG .eq. 0) THEN

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
            DHCNVC(N,L)=0.
            DHRAIN(N,L)=0.
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
c     TIME=(NTSD-1)*DT
c     RESET0=TIME-(NTSD/NPREC)*NPREC*DT
c     RESET1=(NPHS-1)*DT+3600.

	TIME=IFCST
!	write(6,*) 'here (3)'

	RESET0=1.  ! designed to prevent resets.  Reconsider later

        DO N=1,NUMSTA
          STATPR(N)=ACPREC0(N)*1.E3
          STACPR(N)=CUPREC0(N)*1.E3
          STASNM(N)=ACSNOM0(N)*1.E3
          STASNO(N)=ACSNOW0(N)*1.E3
          STASRF(N)=SSROFF0(N)*1.E3
          STABRF(N)=BGROFF0(N)*1.E3
        ENDDO

        DO N=1,NUMSTA
          STASWI(N)=ASWIN0(N)
          STASWO(N)=ASWOUT0(N)
          STASWT(N)=ASWTOA0(N)
        ENDDO

        DO N=1,NUMSTA
          STALWI(N)=ALWIN0(N)
          STALWO(N)=ALWOUT0(N)
          STALWT(N)=-ALWTOA0(N)
        ENDDO
        DO N=1,NUMSTA
          STAEVP(N)=SFCLHX0(N)
          STAPOT(N)=POTFLX0(N)
          STASHX(N)=SFCSHX0(N)
          STASUB(N)=SUBSHX0(N)
          STAPCX(N)=SNOPCX0(N)
            DO L=1,LM
             DHCNVC(N,L)=TCUCN0(N,L)
             DHRAIN(N,L)=TRAIN0(N,L)
            ENDDO
         ENDDO 
C------------------------------------------------------------------
  300 CONTINUE
C------------------------------------------------------------------
C
C***  FOR ROTATION OF WINDS FROM E-GRID TO GEODETIC ORIENTATION
C***  WE NEED THE TWO QUANTITIES BELOW.

      SINPH0=SIN(TPH0D*DTR)
      COSPH0=COS(TPH0D*DTR)

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
      DO L=1,LM
        DO N=1,NUMSTA
	if (RSWTT(N,L) .ne. -9999) then
          RSWTT(N,L)=RSWTT(N,L)*FACTR(N)
	endif
        ENDDO
      ENDDO
C
C***  COMPUTE RTOP
C
!$OMP parallel do
      DO L=1,LM
        DO N=1,NUMSTA
          APEL=PMID(N,L)
          RTOP(N,L)=RD*T(N,L)*(1.+0.608*Q(N,L))/APEL
        ENDDO
      ENDDO
C
C***  PDS IS SURFACE PRESSURE.
C
!$OMP parallel do
      DO N=1,NUMSTA
	I=IHINDX(N)	
	J=JHINDX(N)
        PDS(N)=pint_part(N)+PT
      ENDDO

C
C***  EGRID2 IS THE SURFACE TEMPERATURE.
C
!$OMP parallel do
      DO N=1,NUMSTA
        IF(ACPREC(N).LT.0.)ACPREC(N)=0.
        IF(CUPREC(N).LT.0.)CUPREC(N)=0.
      ENDDO
C
C***  SET CYCLE, DATE, AND FORECAST TIME.
C
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

      IFCST=3600*ITAG
	IHR=ITAG
	write(6,*) 'IFCST: ', IFCST
      WRITE(6,*)' POST PROFILE FOR ',
     1                       IYR,IMNTH,IDAY,IHR
	write(6,*) 'IHRST= ', IHRST
C
C***  SET RTSPH,RTSCU,RTSRA TO 1. OVER THE NUMBER OF TIMES THE
C***  VARIOUS PHYSICS ROUTINES HAVE BEEN
C***  CALLED SINCE LAST OUTPUT OF PROFILER DATA.  NECESSARY FOR
C***  CORRECT AVERAGING OF VARIABLES.
C

	write(6,*) 'APHTIM, ACUTIM, ARATIM were: ', 
     &                               APHTIM, ACUTIM, ARATIM
	APHTIM=0.
	ACUTIM=0.
	ARATIM=0.

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

        write(6,*) 'RTSPH, RTSCU, RTSRA: ', RTSPH, RTSCU, RTSRA
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
c20080708      NWORD14  = 14*LMHK
c20080708      NWORD13  = 15*LMHK
      ISTAT    = IDSTN(N)
      CISTAT   = CIDSTN_SAVE(N)
!	write(6,*) 'CISTAT: ', CISTAT
C
      FPACK(1) = STNLAT(N)/DTR
      FPACK(2) = STNLON(N)/DTR
      IF(FPACK(2).LT.-180.)FPACK(2)=FPACK(2)+360.
      FPACK(3) = FIS(N)*GI
      FPACK(4) = FLOAT(LMHK)
      FPACK(5) = LCL1ML -2  !20080708: B Zhou don't store 13 and 14th sounding 
      FPACK(6) = LCL1SL
      FPACK(7) = 9+FPACK(5)*FPACK(4)+FPACK(6)
      FPACK(8) = 999.
      FPACK(9) = 999.

      DLM    = -STNLON(N)+TLM0D*DTR
      XX     = COSPH0*COS(STNLAT(N))*COS(DLM)
     1        +SINPH0*SIN(STNLAT(N))
      YY     = -COS(STNLAT(N))*SIN(DLM)
      TLON   = ATAN(YY/XX)
      ALPHA  = ASIN(SINPH0*SIN(TLON)/COS(STNLAT(N)))
      SINALP = SIN(ALPHA)
      COSALP = COS(ALPHA)


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
       LVL=LMHK-LV+1      !old ARW version Jun Du: 20081010
c        LVL=LV
        PRODAT(LVL)      = PMID(N,LV)
c	if (mod(LV,15) .eq. 0 .and. mod(N,50) .eq. 0) then
c	write(6,*) 'PRODAT definition, PMID: ', N,L,PMID(N,LV)
c	endif

c        if (LVL .eq. 1 .and. mod(N,25) .eq. 0) then
c        write(6,*) 'N, PSFC: ', N,PRODAT(1)
c        endif

        PRODAT(LMHK+LVL) = T(N,LV)

C***  ROTATE WINDS
C
        UT     = U(N,LV)
        VT     = V(N,LV)
c        if (IHR .eq. 0) then
c         print *, 'sin cos check ', ISTAT, COSALP, SINALP,
c     *            stnlat(n)/dtr, stnlon(n)/dtr
c        endif
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

        QLIQ = (1.-F_ice(N,LV))*CWM(N,LV)
c20080708        PRODAT(NWORD13+LVL) = F_rain(N,LV)*QLIQ
        PRODAT(NWORD6+LVL) = QLIQ-PRODAT(NWORD7+LVL)
c20080708        PRODAT(NWORD14+LVL) = F_ice(N,LV)*CWM(N,LV)
        PRODAT(NWORD7+LVL) = TCUCN(N,LV)
        PRODAT(NWORD8+LVL) = TRAIN(N,LV)
        PRODAT(NWORD9+LVL) = RSWTT(N,LV)
        PRODAT(NWORD10+LVL)= RLWTT(N,LV)
        PRODAT(NWORD11+LVL)= CLDFRA(N,LV)*100.

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
c        LVL=LMHK-LL+1       !old ARW version  Jun Du: 20081010
        LVL=LL
        STADHC(LL) = PRODAT(NWORD7+LL) - DHCNVC(N,LVL)
        STADHR(LL) = PRODAT(NWORD8+LL) - DHRAIN(N,LVL)
C
        DHCNVC(N,LVL) = PRODAT(NWORD7+LL)
        DHRAIN(N,LVL) = PRODAT(NWORD8+LL)
C
Ctmp        IF(MOD(NTSD,NHEAT).LT.NCNVC)THEN
          DHCNVC(N,LVL) = 0.
          DHRAIN(N,LVL) = 0.
Ctmp        ENDIF
      ENDDO
C
C***  EXTRACT SINGLE LEVEL DATA.   EGRID2 IS SURFACE TEMPERATURE.
C
      PRODAT(NWORD13+1)  = PSLP  (N)        !sea level pressure, -999
      PRODAT(NWORD13+2)  = PDS   (N)        !sfc pressure
      PRODAT(NWORD13+3)  = EGRID2(N)        !Skin temperaure
      PRODAT(NWORD13+4)  = TLMIN (N)        !1hr min T, -999
      PRODAT(NWORD13+5)  = TLMAX (N)        !1hr max T,-999
      PRODAT(NWORD13+6)  = SMSTAV(N)*100.   !Soil moisture availability
        if (mod(N,50) .eq. 0) then
        write(0,*) 'NWORD13+6, PRODAT(NWORD13+6) for SMSTAV(%): ',
     &                     NWORD13+6, PRODAT(NWORD13+6)
        endif
      PRODAT(NWORD13+7)  = ACPREC(N)*1000.  !ACCUMULATED TOTAL GRID SCALE PRECIPITATION
      PRODAT(NWORD13+8)  = CUPREC(N)*1000.  !ACCUMULATED TOTAL CUMULUS PRECIPITATION
      PRODAT(NWORD13+27) = Z0    (N)        !Roughness length
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
      PRODAT(NWORD13+28) = UT*COSALP+VT*SINALP   !10 U
      PRODAT(NWORD13+29) = VT*COSALP-UT*SINALP   !10 V
C
      PRODAT(NWORD13+30) = TH10  (N)         ! -999
      PRODAT(NWORD13+31) = Q10   (N)         ! -999
      PRODAT(NWORD13+32) = TSHLTR(N)         !T2m
      PRODAT(NWORD13+33) = QSHLTR(N)         !Q2m
      PRODAT(NWORD13+34) = SFCEXC(N)         !sfc exchange coefficient
      PRODAT(NWORD13+35) = VEGFRC(N)         !veg fraction
      PRODAT(NWORD13+36) = CMC   (N)*1000.   !canopy water
      PRODAT(NWORD13+37) = SMC   (N,1)       !layer 1 volumnetric soil moisture
      PRODAT(NWORD13+38) = SMC   (N,2)       !layer 2 volumnetric soil moisture
      PRODAT(NWORD13+39) = SMC   (N,3)       !layer 3 volumnetric soil moisture
      PRODAT(NWORD13+40) = SMC   (N,4)       !layer 4 volumnetric soil moisture
      PRODAT(NWORD13+41) = STC   (N,1)       !layer 1 volumnetric soil temperature       
      PRODAT(NWORD13+42) = STC   (N,2)       !layer 2 volumnetric soil temperature
      PRODAT(NWORD13+43) = STC   (N,3)       !layer 3 volumnetric soil temperature
      PRODAT(NWORD13+44) = STC   (N,4)       !layer 4 volumnetric soil temperature
      PRODAT(NWORD13+45) = SM    (N) + SICE(N) !Land/sea mask
      PRODAT(NWORD13+46) = CFRACL(N)*100.    !low cloud fraction, -999 
      PRODAT(NWORD13+47) = CFRACM(N)*100.    !mid cloud fraction, -999
      PRODAT(NWORD13+48) = CFRACH(N)*100.    !high cloud fraction, -999
      PRODAT(NWORD13+49) = SR    (N)*100.    !snow ratio, set = 0.0
      PRODAT(NWORD13+50) = NINT(HBOT(N))     !snow type, -999 
C
      PRODAT(NWORD13+9)   = SFCLHX(N)        !average sfc latent heat flux,-999
      PRODAT(NWORD13+10)  = POTFLX(N)        !average sfc sensible heat flux,-999
      PRODAT(NWORD13+11)  = SFCSHX(N)        !sfc sensible heat flux,-999
      PRODAT(NWORD13+12)  = SUBSHX(N)        !sub-sfc heat,999
      PRODAT(NWORD13+13)  = SNOPCX(N)        !snow phase change,-999
      PRODAT(NWORD13+14)  = ASWIN (N)        !average short wave down flux, -999
      PRODAT(NWORD13+15)  = ASWOUT(N)        !average short wave up flux, -999
      PRODAT(NWORD13+16)  = ALWIN (N)        !average long wave down flux, -999
      PRODAT(NWORD13+17)  = ALWOUT(N)        !average long wave down flux, -999
      PRODAT(NWORD13+18)  =-ALWTOA(N)        !net long wave flux at top, -999 
      PRODAT(NWORD13+19)  = ASWTOA(N)        !net short wave flux at top, -999
      PRODAT(NWORD13+20)  = ACSNOW(N)*1000.  !1hr accumu snow fall
      PRODAT(NWORD13+21)  = SMSTOT(N)*1000.  !total soil moisture, -999
      PRODAT(NWORD13+22)  = SNO   (N)*1000.  !snow water equivalent,
      PRODAT(NWORD13+23)  = ACSNOM(N)*1000.  !1hr snow water melt
      PRODAT(NWORD13+24)  = SSROFF(N)*1000.  !1hr sfc accumu runoff
      PRODAT(NWORD13+25)  = BGROFF(N)*1000.  !1hr accumu baseflow-ground water runoff
      PRODAT(NWORD13+26)  = SOILTB(N)        !bottom soil temperature, -999
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
        if (N .eq. 256) then
        write(0,*) 'N, PRODAT(NWORD13+25), STABRF(N), PBGROFF: ', N,
     &                 PRODAT(NWORD13+25), STABRF(N), PBGROFF
        endif
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
      FPACK(9+NWORD13+10) = PPOTEVP * RTSPH
      FPACK(9+NWORD13+11) = PSFCSHX * RTSPH
      FPACK(9+NWORD13+12) = PSFCSUB * RTSPH
      FPACK(9+NWORD13+13) = PSNOPCX * RTSPH
      FPACK(9+NWORD13+14) = PRSWIN  * RTSPH
      FPACK(9+NWORD13+15) = PRSWOUT * RTSPH
      FPACK(9+NWORD13+16) = PRLWIN  * RTSPH
      FPACK(9+NWORD13+17) = PRLWOUT * RTSPH
      FPACK(9+NWORD13+18) = PRLWTOA * RTSPH
      FPACK(9+NWORD13+19) = PRSWTOA * RTSPH
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
     
c       write(6,*) 'IFHR, NUMSTA, N, NREC: ', IFHR, NUMSTA, N, 
c     &                     IFHR*NUMSTA+N

!normal       NREC=IFHR*NUMSTA+N
cZHOU       NREC=(IFHR/INCR)*NUMSTA+N
        NREC=N
!	write(6,*) 'NREC, NLEN, FPACK: ', NREC, NLEN,
!     &                       (FPACK(NNN),NNN=1,NLEN,NLEN/5)


 	if (NREC.eq.1 .or. NREC.eq.100) then
 	write(6,*) 'NREC, IHRST, IDAT, IFCST, ISTAT, CISTAT: ', 
     &	NREC, IHRST, IDAT, IFCST, ISTAT, CISTAT
        write(*,'(a5,10f8.2)') 'F1-10:',(FPACK(k),k=1,10)
        do kk=1,LM
        write(*,'(a5,f10.2,3f8.3)') 'FPACK',FPACK(9+kk),
     &    FPACK(9+LM+kk),FPACK(9+2*LM+kk),
     &    FPACK(9+3*LM+kk)
        end do
        end if

      WRITE(LCLAS1,REC=NREC)IHRST,IDAT,IFCST,ISTAT,CISTAT
     1,                    (FPACK(NL),NL=1,NLEN)

C---------------------------------------------------------------------
 1000 CONTINUE
      CLOSE(LCLAS1)

        print *, 'about to deallocate'
	DEALLOCATE(T,Q,U,V,Q2,OMGALF,CWM,TRAIN,TCUCN)
	DEALLOCATE(RSWTT,RLWTT,CCR,RTOP,HTM,OMGA,p_hold)
	DEALLOCATE(t_hold,PINT,W,WH,IW)

	DEALLOCATE(DHCNVC,DHRAIN,STADHC,STADHR,TCUCN0,TRAIN0)
	DEALLOCATE(DUM,DUMMY,DUMMY2,DUM3D,DUM3D2)
        DEALLOCATE(DUM3D3,GDLAT)
	DEALLOCATE(GDLON,PRODAT,FPACK,IDUM,LMH,DUM1D)

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
     &,BGROFF0,PVAPOR,ZINT)
C
C***  END OF PROFILE SITE LOOP
C
C***  END PROFILE POSTING CODE.
C---------------------------------------------------------------------
      RETURN
      END
