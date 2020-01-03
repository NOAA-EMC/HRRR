      PROGRAM SMARTINIT

      use constants          ! Define constants used by NDFDgrid.f90
      use grddef             ! Initialize grid and run params
      use aset3d             ! Define 3-d grids
      use aset2d             ! Define 2-d grids
      use asetdown           ! Define downscaled output grids
      use rdgrib             ! Define grib read routines rdhdrs, setvar
      use GRIB_MOD
      use pdstemplates
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SMARTINIT   CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 2011-09-20
!
! ABSTRACT:   THIS CODE TAKES NATIVE FILES AND GENERATES
!          2.5 KM OR 3 KM OUTPUT CONTAINING NDFD ELEMENTS
!
! PROGRAM HISTORY LOG:
!   2011-09-10  G MANIKIN  - CODE REVISED FOR RAPID REFRESH 
!   2018-10-23  A GIBBS    - Code revised to consolidate RAP and HRRR
!                            and the different grids
!                          - Code revised to read in and write out netcdf
!                            for 3DRTMA
!
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200),ID(25)
      INTEGER DATE,LUB,NUMVAL,FHR,CYC
      CHARACTER *80 FNAMEOUT
      character*4 model
      character*2 domain
      character*6 datafmt

!  USED in MAIN only
!     REAL, ALLOCATABLE :: PBLMARK(:,:),RH(:,:,:)

      TYPE (GINFO) :: GDIN
      TYPE (GRIBFIELD) :: GFLD
      
!  Read input file to determine model, domain, fhr, and cycle

      READ (5,*) model
      READ (5,*) domain
      READ (5,*) datafmt
      READ (5,*) FHR
      READ (5,*) CYC
      print*, 'Running Smartinit for ', model, domain
      print *, 'Into Main ', 'fhr=',FHR, 'cyc=',cyc

!  Read index file to get the grid specs; set maxlev based on model

      LUGB=11
      LUGI=12
      write(0,*) 'call RDHDRS MAIN to get grid specs'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      im=gdin%imax
      jm=gdin%jmax
      itot=numval
      if (trim(model) == 'RAP') then
        maxlev=50
      elseif (trim(model) == 'HRRR') then
        maxlev=20
      else
        print*,'Model is not specified, set model in smartinit script'
      endif

! 3d fields

      allocate (hght(im,jm,maxlev),t(im,jm,maxlev),q(im,jm,maxlev))
      allocate (uwnd(im,jm,maxlev),vwnd(im,jm,maxlev),pmid(im,jm,maxlev))

! 2d fields

      allocate (psfc(im,jm),zsfc(im,jm),u10(im,jm),v10(im,jm))
      allocate (t2(im,jm),q2(im,jm),d2(im,jm),coast(im,jm),gust(im,jm))
      allocate (basez(im,jm),bli(im,jm))
      allocate (tcld(im,jm),wetfrz(im,jm),lcld(im,jm),mcld(im,jm))
      allocate (hcld(im,jm),snod(im,jm),refc(im,jm),topo_ndfd(im,jm))
      allocate (vis(im,jm),ceil(im,jm),slp(im,jm),sfcr(im,jm),sst(im,jm))
      allocate (gam(im,jm))
      allocate (validpt(im,jm),bitmap(itot))

! Used in Main only

!     allocate (rh(im,jm,maxlev),pblmark(im,jm))

! Used for downscaling output

      allocate (downt(im,jm),downdew(im,jm),downu(im,jm),downv(im,jm))
      allocate (downq(im,jm),downp(im,jm),wgust(im,jm))
      allocate (dirtrans(im,jm),mgtrans(im,jm),lal(im,jm),sky(im,jm))
      allocate (mixhgt(im,jm),blr(im,jm))

! Read in the model data
      
      if (trim(datafmt) == 'GRIB2') then
        call getgrib(date,fhr,gdin,gfld,maxlev,domain,model)
      elseif (trim(datafmt) == 'NETCDF') then
!       call get_netcdf(date,fhr)
        print*,'Need to write get_netcdf subroutine'
      else
        print*,'Data format is not specified, set datafmt in script'
      endif

! Call the downscaling code

      call NDFDgrid(downt,downdew,downu,downv,downq,downp,gdin,im,jm,maxlev,domain,model)

! Calculate the extra fields

      call calc_flds(im,jm,maxlev,model)

! Write out the downscaled data

      if (trim(datafmt) == 'GRIB2') then
        call write_grib(date,fhr,gdin,gfld,itot,maxlev,domain,model)
      elseif (trim(datafmt) == 'NETCDF') then
!       call write_netcdf(date,fhr)
        print*,'Need to write write_netcdf subroutine'
      endif
    
      print *, 'completed main'

!     deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,rh,pblmark,blr)
      deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,blr)
      deallocate (sky,tcld,wetfrz,dirtrans,vis,gust,d2)
      deallocate (lcld,mcld,hcld,wgust,u10,v10,refc,t2,q2,bli)
      deallocate (topo_ndfd,coast,downt,downdew,downu,downv,downq) 
      deallocate (downp,mixhgt,mgtrans,lal,basez,snod,slp,ceil)
      deallocate (pmid,sfcr,validpt,bitmap)
      deallocate (sst,gam)

      STOP
      END PROGRAM smartinit
! -------------------------
      SUBROUTINE FILL_FLD(GFLD,NUMV,IM,JM,ARRAY2D)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        INTEGER :: NUMV, IM, JM, KK
        REAL :: ARRAY2D(IM,JM)
        
        DO KK = 1, NUMV
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
          GFLD%FLD(KK)=ARRAY2D(M,N) 
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

      deallocate (locbmap)

      END SUBROUTINE SET_SCALE

! --------------------------------

     subroutine g2getbits(ibm,scl,len,bmap,g,gmin,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of
!   bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
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
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits

