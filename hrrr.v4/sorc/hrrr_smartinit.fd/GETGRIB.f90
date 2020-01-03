      subroutine getgrib(date,fhr,gdin,gfld,maxlev,domain,model)

        use grddef
        use aset3d
        use aset2d
        use rdgrib
        use constants
        use GRIB_MOD
        use pdstemplates

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-09-30
!
! ABSTRACT:
!   .
!
! PROGRAM HISTORY LOG:
!   11-09-30  G MANIKIN  - ADAPT CODE TO RAPID REFRESH 
!   18-10-23  A GIBBS    - Code revised to consolidate RAP and HRRR
!                          and the different grids
!                        - Code revised to read in and write out netcdf
!                          for 3DRTMA
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
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER YEAR,MON,DAY,IHR,DATE,IFHR
      INTEGER NUMVAL, IMAX, JMAX, KMAX, NUMLEV, ITOT, KRET
      INTEGER ISSREF,JDISC,JPDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
 
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      character*2 domain
      character*4 model
      TYPE(GRIBFIELD)::GFLD
 
      REAL, ALLOCATABLE   :: htagl(:,:),mdlsfc(:,:)
! ASSIGN UNIT NUMBERS 
 
      LUGB=11
      LUGI=12

! Open and read DATE file

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

! Read native-level model data
! Read index file to get grid specs
!
      KMAX = 0
      write(0,*) 'call RDHDRS_g2'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      GDIN%KMAX=MAXLEV
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,igdnum,numval'
      print *,imax,jmax,kmax,numlev,igdnum,numval

      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'GRID ALLOCATED',ITOT,' kret ',kret
  
      JIDS=-9999
      JPDTN=-1
      JPDT=-9999
      JGDTN=-1
      JGDT=-9999
! JPDTN needs to be 0 to match specific records
      JPDTN = 0
! JDISC matches discipline table
      JDISC  = 0
      ISSREF=0 ! not SREF data

! Get vertical profile of pressure
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 003
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
         
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K
        print*,'minval PMID',minval(PMID(:,:,LL))
        print*,'maxval PMID',maxval(PMID(:,:,LL))

      ENDDO
      print*,'got pressure'
        
! Get vertical profile of height
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 003
        JPDT(2) = 005
        JPDT(10) = 105
        JPDT(12) = LL
         
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  
        print*,'minval HGHT',minval(HGHT(:,:,LL))
        print*,'maxval HGHT',maxval(HGHT(:,:,LL))

      ENDDO
      print*,'got height'
  
! Get vertical profile of temperature
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 000
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
         
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,T(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

        print*,'minval T',minval(T(:,:,LL))
        print*,'maxval T',maxval(T(:,:,LL))

      ENDDO
      print*,'got temperature'

! Find bitmap points
      DO MM=1,IMAX
      DO NN=1,JMAX
        IF(T(MM,NN,1).EQ.0) THEN
          VALIDPT(MM,NN) = .FALSE.
        ELSE
          VALIDPT(MM,NN) = .TRUE.
        END IF
      ENDDO
      ENDDO
      print*,'got bitmap'

! Get vertical profile of q
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 001
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

        print*,'minval Q',minval(q(:,:,LL))
        print*,'maxval Q',maxval(q(:,:,LL))

      ENDDO
      print*,'got q'


! Get vertical profile of u
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 002
        JPDT(2) = 002
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

        print*,'minval UWND',minval(UWND(:,:,LL))
        print*,'maxval UWND',maxval(UWND(:,:,LL))

      ENDDO
      print*,'got u'

! Get vertical profile of v
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 002
        JPDT(2) = 003
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

        print*,'minval VWND',minval(VWND(:,:,LL))
        print*,'maxval VWND',maxval(VWND(:,:,LL))

      ENDDO
      print*,'got v'

! Get surface height
      JPDT=-9999
      JPDT(2) = 005
      JPDT(10) = 1
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,GFLD, &
                     ISSREF,IRET,ISTAT)

      print*,'minval zsfc',minval(zsfc)
      print*,'maxval zsfc',maxval(zsfc)

      allocate (mdlsfc(imax,jmax))
      mdlsfc=zsfc

      WHERE (ZSFC < 0.0) ZSFC=0.0

! Calculate height above ground

      allocate (htagl(imax,jmax))
      do ll=1,maxlev
        rmin=+huge(rmin)
        rmax=-huge(rmax)
        where (.not. validpt) htagl=1.e19
        where (validpt) htagl=hght(:,:,ll)-mdlsfc
        do i=1,imax
        do j=1,jmax
          if(htagl(i,j) < 1.e19)then
            if(htagl(i,j) < rmin) rmin=htagl(i,j)
            if(htagl(i,j) > rmax) rmax=htagl(i,j)
          endif
        enddo
        enddo
        write(6,*) 'min/max of HTAGL: ',LL, rmin, rmax
      enddo

! Get surface pressure
      JPDT(1) = 003
      JPDT(2) = 000
      JPDT(10) = 1
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PSFC,GFLD, &
                     ISSREF,IRET,ISTAT)

      print*,'minval psfc',minval(psfc)
      print*,'maxval psfc',maxval(psfc)
 
      if(trim(model) == 'RAP')then
! Get lowest wet bulb zero
      JPDT(1) = -9999
      JPDT(2) = 005
      JPDT(10) = 245
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,WETFRZ,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval wet0',minval(wetfrz)
      print*,'maxval wet0',maxval(wetfrz)
      endif

! Get visibility
      JPDT(1) = 19
      JPDT(2) = 000
      JPDT(10) = 1
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VIS,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval vis',minval(vis)
      print*,'maxval vis',maxval(vis)

! Get 2-m Q
      JPDT(1) = 1
      JPDT(2) = 000
      JPDT(10) = 103
      JPDT(12) = 2
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q2,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval q2',minval(q2)
      print*,'maxval q2',maxval(q2)

! Get 2-m dew point
      JPDT(1) = 0
      JPDT(2) = 006
      JPDT(10) = 103
      JPDT(12) = 2
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,D2,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval d2',minval(d2)
      print*,'maxval d2',maxval(d2)

! Get 10-m u wind
      JPDT(1) = 2
      JPDT(2) = 002
      JPDT(10) = 103
      JPDT(12) = 10
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,U10,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval u10',minval(u10)
      print*,'maxval u10',maxval(u10)

! Get 10-m v wind
      JPDT(1) = 2
      JPDT(2) = 003
      JPDT(10) = 103
      JPDT(12) = 10
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,V10,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval v10',minval(v10)
      print*,'maxval v10',maxval(v10)

! Get SFCR (Surface Roughness)

      JDISC = 2
      JPDT(1) = 0
      JPDT(2) = 001
      JPDT(10) = 001
      JPDT(12) = 000
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,JGDT,KF,K,&
                     KPDS,KGDS,MASK,GRID,SFCR,GFLD,ISSREF,IRET,ISTAT)

      print*,'minval sfcr',minval(sfcr)
      print*,'maxval sfcr',maxval(sfcr)

      if(trim(model) == 'RAP')then
! Get best lifted index
      JDISC = 0
      JPDT(1) = 7
      JPDT(2) = 193
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,BLI,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval bli',minval(bli)
      print*,'maxval bli',maxval(bli)
      endif

! Get surface wind gust
      JDISC = 0
      JPDT(1) = 002
      JPDT(2) = 022
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,GUST,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval gust',minval(gust)
      print*,'maxval gust',maxval(gust)

      if(trim(model) == 'RAP')then
! Get composite reflectivity
      JPDT(1) = 16
      JPDT(2) = 196
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,REFC,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval refc',minval(refc)
      print*,'maxval refc',maxval(refc)
      endif

! Get total cloud fraction
      JPDT(1) = 6
      JPDT(2) = 1
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,TCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval tcld',minval(tcld)
      print*,'maxval tcld',maxval(tcld)

      if(trim(model) == 'RAP')then
! Get low cloud fraction
      JPDT(1) = 6
      JPDT(2) = 3
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,LCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval lcld',minval(lcld)
      print*,'maxval lcld',maxval(lcld)

! Get mid cloud fraction
      JPDT(1) = 6
      JPDT(2) = 4
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,MCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval mcld',minval(mcld)
      print*,'maxval mcld',maxval(mcld)

! Get high cloud fraction
      JPDT(1) = 6
      JPDT(2) = 5
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,HCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval hcld',minval(hcld)
      print*,'maxval hcld',maxval(hcld)

      if (domain == 'CS' .or. domain == 'AK') then
! Get snow depth
        JPDT(1) = 1
        JPDT(2) = 11
        JPDT(10) = -9999
        JPDT(12) = -9999
        J=0

        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,SNOD,GFLD, &
                     ISSREF,IRET,ISTAT)
        print*,'minval snod',minval(snod)
        print*,'maxval snod',maxval(snod)
      endif
      endif

! Get land mask (actually vegetation type; used for changing land to
! water and vice versa)
      JDISC  = 2
      JPDT(1) = 0
! would be 0 if we wanted land/sea classification
      if(domain == 'AK') then
        JPDT(2) = 0
      elseif(domain == 'CS' .and. trim(model) == 'HRRR') then
        JPDT(2) = 0
      else
        JPDT(2) = 198
      endif
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,COAST,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval coast',minval(coast)
      print*,'maxval coast',maxval(coast)

! Get ceiling
      JDISC  = 0
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 215
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,CEIL,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval ceil',minval(ceil)
      print*,'maxval ceil',maxval(ceil)
      print*,'gfld ibmap: ',gfld%ibmap

! Get sea level pressure
      JPDT(1) = 3
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,SLP,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval mslp',minval(slp)
      print*,'maxval mslp',maxval(slp)
      print*,'gfld ibmap: ',gfld%ibmap


! Get 2-m temperature 
! JPDT(1) is table 4.1
! JPDT(2) is parameter number
! JPDT(10) is level type in table 4.5
! JPDT(12) is level (e.g., 2 for 2-m or 30 for hybrid level 30)

      JPDT(1) = 0
      JPDT(2) = 000
      JPDT(10) = 103
      JPDT(12) = 2

      JDISC=0
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,T2,GFLD, &
                     ISSREF,IRET,ISTAT)

      print*,'minval t2',minval(t2)
      print*,'maxval t2',maxval(t2)
      print *,'2-m template number',GFLD%ipdtmpl
      print *,'2-m idrtemplate number',GFLD%idrtmpl

! Get skin temperature/SST
       JDISC=0
       JPDT=-9999
       JPDT(1) = 0
       JPDT(2) = 0
       JPDT(10) = 1
       J=0
       CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                      JGDT,KF,K,KPDS,KGDS,MASK,GRID,SST,GFLD, &
                      ISSREF,IRET,ISTAT)

       print*,'minval sst',minval(sst)
       print*,'maxval sst',maxval(sst)

! Get cloud base height
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 2
      JPDT(12) = -9999
      J=0

      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,BASEZ,GFLD, &
                     ISSREF,IRET,ISTAT)
      print*,'minval basez',minval(basez)
      print*,'maxval basez',maxval(basez)
      print*,'gfld ibmap: ',gfld%ibmap
      print*,'GFLD%fld',GFLD%fld(1)
      print*,'GFLD%fld',GFLD%fld(1000)

      print *, 'done with unpacking'

      deallocate (grid,mask,mdlsfc,htagl) 

      RETURN
      END
