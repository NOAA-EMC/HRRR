!----------------------------------------------------------------------------
module nemsio_module
!$$$ module document block
!
! module:   nemsio_module      API for NEMS input/output 
!
! Abstract: This module handles NEMS input/output
!
! Program history log
!    2006-11-10    Jun Wang  for gfsio
!    2008-02-29    Jun Wang
!    2008-11-04    Jun Wang - Changes to NFRAME; users can output fewer
!                             metadata records and update data fields in
!                             existing files.
!    2009-03-31    Jun Wang - use new bacio to handle >2gb file, changed
!                             record name to 16 characters
!
! Public Variables
! Public Defined Types
!   nemsio_gfile
!     private
!        gtype:   character(nemsio_charkind8)  NEMSIO file identifier
!        gdatatype:character(nemsio_charkind8) data format
!        modelname:character(nemsio_charkind8) modelname
!        version: integer(nemsio_intkind)   verion number
!        nmeta:   integer(nemsio_intkind)   number of metadata rec
!        lmeta:   integer(nemsio_intkind)   length of metadata rec 2 for model paramodels
!        nrec:    integer(nemsio_intkind)   number of data rec
!        idate(1:7):integer(nemsio_intkind) initial date (yyyy/mm/dd/hh/mm/ssn/ssd)
!        nfday:   integer(nemsio_intkind)   forecast day
!        nfhour:  integer(nemsio_intkind)   forecast hour
!        nfminute:integer(nemsio_intkind)   forecast minutes
!        nfsecondn:integer(nemsio_intkind)  numerator of forecast second fraction
!        nfsecondd:integer(nemsio_intkind)  denominator of forecast second fraction
!        dimy:    integer(nemsio_intkind)   dimension in latitude
!        dimx:    integer(nemsio_intkind)   dimension in Longitude
!        dimz:    integer(nemsio_intkind)   number of levels
!        nframe:  integer(nemsio_intkind)   dimension of halo
!        nsoil:    integer(nemsio_intkind)  number of soil layers
!        ntrac:    integer(nemsio_intkind)  number of tracers
!        jcap:    integer(nemsio_intkind)   spectral truncation
!        ncldt:   integer(nemsio_intkind)   number of cloud types
!        idsl:    integer(nemsio_intkind)   semi-lagrangian id
!        idvc:    integer(nemsio_intkind)   vertical coordinate id
!        idvm:    integer(nemsio_intkind)   mass variable id
!        idrt:    integer(nemsio_intkind)   grid identifier
!                 (idrt=4 for gaussian grid,
!                  idrt=0 for equally-spaced grid including poles,
!                  idrt=256 for equally-spaced grid excluding poles)
!        rlon_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to 0)
!        rlon_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 360.)
!        rlat_min:real(nemsio_realkind)     minimal longtitude of regional domain (global:set to -90)
!        rlat_max:real(nemsio_realkind)     maximal longtitude of regional domain (global:set to 90)
!        extrameta:logical(nemsio_logickind)extra meta data flag 
!        nmetavari:integer(nemsio_intkind)  number of extra meta data integer variables
!        nmetavarr:integer(nemsio_intkind)  number of extra meta data real variables
!        nmetavarl:integer(nemsio_intkind)  number of extra meta data logical variables
!        nmetavarc:integer(nemsio_intkind)  number of extra meta data character variables
!        nmetaaryi:integer(nemsio_intkind)  number of extra meta data integer arrays
!        nmetaaryr:integer(nemsio_intkind)  number of extra meta data real arrays
!        nmetaaryl:integer(nemsio_intkind)  number of extra meta data logical arrays
!        nmetaaryc:integer(nemsio_intkind)  number of extra meta data character arrays
!
!        recname: character(nemsio_charkind),allocatable    recname(:)
!        reclevtyp: character(nemsio_charkind),allocatable    reclevtyp(:)
!        reclev:  integer(nemsio_intkind),allocatable       reclev(:)
!        vcoord:  real(nemsio_realkind),allocatable         vcoord(:,:,:)
!        lat:  real(nemsio_realkind),allocatable         lat(:) lat for mess point
!        lon:  real(nemsio_realkind),allocatable         lon(:) lon for mess point
!        gvlat1d: real(nemsio_realkind),allocatable         gvlat1d(:) lat for wind point
!        gvlon1d: real(nemsio_realkind),allocatable         gvlon1d(:) lon for wind point
!        Cpi:     real(nemsio_realkind),allocatable         cpi(:)
!        Ri:      real(nemsio_realkind),allocatable         ri(:)
!
!        variname:character(nemsio_charkind)  names of extra meta data integer variables
!        varrname:character(nemsio_charkind)  names of extra meta data real variables
!        varlname:character(nemsio_charkind)  names of extra meta data logical variables
!        varcname:character(nemsio_charkind)  names of extra meta data character variables
!        varival: integer(nemsio_intkind)     values of extra meta data integer variables
!        varrval: real(nemsio_realkind)       values of extra meta data integer variables
!        varlval: logical(nemsio_logickind)   values of extra meta data integer variables
!        varcval: character(nemsio_charkind)  values of extra meta data integer variables
!        aryiname:character(nemsio_charkind)  names of extra meta data integer arrays
!        aryrname:character(nemsio_charkind)  names of extra meta data real arrays
!        arylname:character(nemsio_charkind)  names of extra meta data logical arrays
!        arycname:character(nemsio_charkind)  names of extra meta data character arrays
!        aryilen: integer(nemsio_intkind)     lengths of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays
!        aryilen: integer(nemsio_intkind)     number of extra meta data integer arrays

!!--- file handler
!        gfname:  character(255)  file name
!        gaction: character(nemsio_charkind)  read/write
!        flunit:  integer(nemsio_intkind)  unit number  
!
! Public method
!   nemsio_init
!   nemsio_finalize
!   nemsio_open
!   nemsio_writerec
!   nemsio_readirec
!   nemsio_writerecv
!   nemsio_readirecv
!   nemsio_writerecw34
!   nemsio_readirecw34
!   nemsio_writerecvw34
!   nemsio_readirecvw34
!   nemsio_close
!   nemsio_getfilehead
!   nemsio_getrechead
! Possible return code
!          0   Successful call
!         -1   Open or close I/O error
!         -2   array size
!         -3   Meta data I/O error (possible EOF)
!         -4   GETGB/PUTGB error
!         -5   Search record or set GRIB message info error
!         -6   allocate/deallocate error
!         -7   set grib table
!         -8   file meta data initialization (default:1152*576)
!         -9   NOT nemsio type file
!         -10  get/close file unit
!         -11  read/write bin data
!         -12  read/write NMM B grid lat lon
!         -13  read/write NMM sfc var
!         -15  read/write gsi 
!         -17  get var from file header
!
!$$$ end module document block
!
  implicit none
  private
!------------------------------------------------------------------------------
! private variables and type needed by nemsio_gfile
  integer,parameter:: nemsio_lmeta1=48,nemsio_lmeta3=32
  integer,parameter:: nemsio_intkind=4,nemsio_intkind8=8
  integer,parameter:: nemsio_realkind=4,nemsio_dblekind=8
  integer,parameter:: nemsio_charkind=16,nemsio_charkind8=8,nemsio_charkind4=4
  integer,parameter:: nemsio_logickind=4
  integer(nemsio_intkind),parameter     :: nemsio_intfill=-9999_nemsio_intkind
  integer(nemsio_intkind8),parameter    :: nemsio_intfill8=-9999_nemsio_intkind8
  logical(nemsio_logickind),parameter:: nemsio_logicfill=.false.
  real(nemsio_intkind),parameter     :: nemsio_kpds_intfill=-1_nemsio_intkind
  real(nemsio_realkind),parameter    :: nemsio_realfill=-9999._nemsio_realkind
  real(nemsio_dblekind),parameter    :: nemsio_dblefill=-9999._nemsio_dblekind
!for grib
  real(nemsio_realkind),parameter    :: nemsio_undef_grb=9.E20_nemsio_realkind
!
!------------------------------------------------------------------------------
!---  public types
  type,public :: nemsio_gfile
    private
    character(nemsio_charkind8) :: gtype=' '
    integer(nemsio_intkind):: version=nemsio_intfill
    character(nemsio_charkind8):: gdatatype=' '
    character(nemsio_charkind8):: modelname=' '
    integer(nemsio_intkind):: nmeta=nemsio_intfill
    integer(nemsio_intkind):: lmeta=nemsio_intfill
    integer(nemsio_intkind):: nrec=nemsio_intfill
!
    integer(nemsio_intkind):: idate(7)=nemsio_intfill
    integer(nemsio_intkind):: nfday=nemsio_intfill
    integer(nemsio_intkind):: nfhour=nemsio_intfill
    integer(nemsio_intkind):: nfminute=nemsio_intfill
    integer(nemsio_intkind):: nfsecondn=nemsio_intfill
    integer(nemsio_intkind):: nfsecondd=nemsio_intfill
!    integer(nemsio_intkind):: ifdate(7)=nemsio_intfill
!
    integer(nemsio_intkind):: dimx=nemsio_intfill
    integer(nemsio_intkind):: dimy=nemsio_intfill
    integer(nemsio_intkind):: dimz=nemsio_intfill
    integer(nemsio_intkind):: nframe=nemsio_intfill
    integer(nemsio_intkind):: nsoil=nemsio_intfill
    integer(nemsio_intkind):: ntrac=nemsio_intfill
!
    integer(nemsio_intkind) :: jcap=nemsio_intfill
    integer(nemsio_intkind) :: ncldt=nemsio_intfill
    integer(nemsio_intkind) :: idvc=nemsio_intfill
    integer(nemsio_intkind) :: idsl=nemsio_intfill
    integer(nemsio_intkind) :: idvm=nemsio_intfill
    integer(nemsio_intkind) :: idrt=nemsio_intfill
    real(nemsio_realkind) :: rlon_min=nemsio_realfill
    real(nemsio_realkind) :: rlon_max=nemsio_realfill
    real(nemsio_realkind) :: rlat_min=nemsio_realfill
    real(nemsio_realkind) :: rlat_max=nemsio_realfill
    logical(nemsio_logickind) :: extrameta=nemsio_logicfill
!
    integer(nemsio_intkind):: nmetavari=nemsio_intfill
    integer(nemsio_intkind):: nmetavarr=nemsio_intfill
    integer(nemsio_intkind):: nmetavarl=nemsio_intfill
    integer(nemsio_intkind):: nmetavarc=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryi=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryr=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryl=nemsio_intfill
    integer(nemsio_intkind):: nmetaaryc=nemsio_intfill
!
    character(nemsio_charkind),allocatable :: recname(:)
    character(nemsio_charkind),allocatable :: reclevtyp(:)
    integer(nemsio_intkind),allocatable    :: reclev(:)
!
    real(nemsio_realkind),allocatable      :: vcoord(:,:,:)
    real(nemsio_realkind),allocatable      :: lat(:)
    real(nemsio_realkind),allocatable      :: lon(:)
    real(nemsio_realkind),allocatable      :: dx(:)
    real(nemsio_realkind),allocatable      :: dy(:)
!
    real(nemsio_realkind),allocatable      :: Cpi(:)
    real(nemsio_realkind),allocatable      :: Ri(:)
!
    character(nemsio_charkind),allocatable :: variname(:)
    integer(nemsio_intkind),allocatable    :: varival(:)
    character(nemsio_charkind),allocatable :: varrname(:)
    real(nemsio_realkind),allocatable      :: varrval(:)
    character(nemsio_charkind),allocatable :: varlname(:)
    logical(nemsio_logickind),allocatable  :: varlval(:)
    character(nemsio_charkind),allocatable :: varcname(:)
    character(nemsio_charkind),allocatable :: varcval(:)
!
    character(nemsio_charkind),allocatable :: aryiname(:)
    integer(nemsio_intkind),allocatable    :: aryilen(:)
    integer(nemsio_intkind),allocatable    :: aryival(:,:)
    character(nemsio_charkind),allocatable :: aryrname(:)
    integer(nemsio_intkind),allocatable    :: aryrlen(:)
    real(nemsio_realkind),allocatable      :: aryrval(:,:)
    character(nemsio_charkind),allocatable :: arylname(:)
    integer(nemsio_intkind),allocatable    :: aryllen(:)
    logical(nemsio_logickind),allocatable  :: arylval(:,:)
    character(nemsio_charkind),allocatable :: arycname(:)
    integer(nemsio_intkind),allocatable    :: aryclen(:)
    character(nemsio_charkind),allocatable :: arycval(:,:)
!  
    character(255) :: gfname
    character(nemsio_charkind8) :: gaction
    integer(nemsio_intkind8)    :: tlmeta=nemsio_intfill
    integer(nemsio_intkind)    :: fieldsize=nemsio_intfill
    integer(nemsio_intkind)    :: flunit=nemsio_intfill
    integer(nemsio_intkind)    :: headvarinum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarrnum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarcnum=nemsio_intfill
    integer(nemsio_intkind)    :: headvarlnum=nemsio_intfill
    integer(nemsio_intkind)    :: headaryinum=nemsio_intfill
    integer(nemsio_intkind)    :: headaryrnum=nemsio_intfill
    integer(nemsio_intkind)    :: headarycnum=nemsio_intfill
    character(nemsio_charkind),allocatable :: headvarcname(:)
    character(nemsio_charkind),allocatable :: headvariname(:)
    character(nemsio_charkind),allocatable :: headvarrname(:)
    character(nemsio_charkind),allocatable :: headvarlname(:)
    character(nemsio_charkind),allocatable :: headaryiname(:)
    character(nemsio_charkind),allocatable :: headaryrname(:)
    character(nemsio_charkind),allocatable :: headarycname(:)
    integer(nemsio_intkind),allocatable    :: headvarival(:)
    real(nemsio_realkind),allocatable      :: headvarrval(:)
    character(nemsio_charkind),allocatable :: headvarcval(:)
    logical(nemsio_logickind),allocatable  :: headvarlval(:)
    integer(nemsio_intkind),allocatable    :: headaryival(:,:)
    real(nemsio_realkind),allocatable      :: headaryrval(:,:)
    character(nemsio_charkind),allocatable :: headarycval(:,:)
    character,allocatable       :: cbuf(:)
    integer(nemsio_intkind):: mbuf=0,nlen,nnum,mnum
    integer(nemsio_intkind8)    :: tlmetalat=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetalon=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetadx=nemsio_intfill
    integer(nemsio_intkind8)    :: tlmetady=nemsio_intfill
  end type nemsio_gfile
!
!------------------------------------------------------------------------------
!--- private types
  type :: nemsio_meta1
    sequence
     character(nemsio_charkind8) :: gtype
     character(nemsio_charkind8) :: modelname
     character(nemsio_charkind8) :: gdatatype
     integer(nemsio_intkind) :: version,nmeta,lmeta
     integer(nemsio_intkind) :: reserve(3)
  end type nemsio_meta1
!
  type :: nemsio_meta2
    sequence
    integer(nemsio_intkind) :: nrec 
    integer(nemsio_intkind) :: idate(1:7),nfday,nfhour,nfminute,nfsecondn, &
                               nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,&
                               jcap,ncldt,idvc,idsl,idvm,idrt
    real(nemsio_realkind)   :: rlon_min,rlon_max,rlat_min,rlat_max 
    logical(nemsio_logickind) :: extrameta
  end type nemsio_meta2
!
  type :: nemsio_meta3
    integer(nemsio_intkind) :: nmetavari,nmetavarr,nmetavarl,nmetavarc, &
                               nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
  end type nemsio_meta3
!
  type  :: nemsio_grbmeta
    integer(nemsio_intkind)   :: jf=nemsio_intfill
    integer(nemsio_intkind)   :: j=nemsio_kpds_intfill
    logical*1,allocatable     :: lbms(:)
    integer(nemsio_intkind)   :: jpds(200)=nemsio_kpds_intfill
    integer(nemsio_intkind)   :: jgds(200)=nemsio_kpds_intfill
  end type nemsio_grbmeta
!
  type :: nemsio_grbtbl_item
    character(nemsio_charkind)     :: shortname=' '
    character(nemsio_charkind*2)   :: leveltype=' '
    integer(nemsio_intkind)    :: precision,g1lev,g1param,g1level 
  end type nemsio_grbtbl_item
!
  type :: nemsio_grbtbl
    integer                        :: iptv
    type(nemsio_grbtbl_item)       :: item(255)
  end type nemsio_grbtbl 
!
  type(nemsio_grbtbl),save  :: gribtable(10)
!
!----- interface
  interface nemsio_getheadvar
    module procedure nemsio_getfheadvari
    module procedure nemsio_getfheadvarr
    module procedure nemsio_getfheadvarl
    module procedure nemsio_getfheadvarc
    module procedure nemsio_getfheadaryi
    module procedure nemsio_getfheadaryr
    module procedure nemsio_getfheadaryl
    module procedure nemsio_getfheadaryc
  end interface nemsio_getheadvar
!
  interface nemsio_readrec
    module procedure nemsio_readrec4
    module procedure nemsio_readrec8
  end interface nemsio_readrec
!
  interface nemsio_readrecv
    module procedure nemsio_readrecv4
    module procedure nemsio_readrecv8
  end interface nemsio_readrecv
!
  interface nemsio_writerec
    module procedure nemsio_writerec4
    module procedure nemsio_writerec8
  end interface nemsio_writerec
!
  interface nemsio_writerecv
    module procedure nemsio_writerecv4
    module procedure nemsio_writerecv8
  end interface nemsio_writerecv
!
  interface splat
    module procedure nemsio_splat4
    module procedure nemsio_splat8
  end interface splat
!
  interface nemsio_readrecbin4
    module procedure nemsio_readrecbin4d4
    module procedure nemsio_readrecbin4d8
  end interface nemsio_readrecbin4
!
  interface nemsio_readrecbin8
    module procedure nemsio_readrecbin8d4
    module procedure nemsio_readrecbin8d8
  end interface nemsio_readrecbin8
!
  interface nemsio_readrecvbin4
    module procedure nemsio_readrecvbin4d4
    module procedure nemsio_readrecvbin4d8
  end interface nemsio_readrecvbin4
!
  interface nemsio_readrecvbin8
    module procedure nemsio_readrecvbin8d4
    module procedure nemsio_readrecvbin8d8
  end interface nemsio_readrecvbin8
!
  interface nemsio_writerecbin4
    module procedure nemsio_writerecbin4d4
    module procedure nemsio_writerecbin4d8
  end interface nemsio_writerecbin4
!
  interface nemsio_writerecbin8
    module procedure nemsio_writerecbin8d4
    module procedure nemsio_writerecbin8d8
  end interface nemsio_writerecbin8
!
  interface nemsio_writerecvbin4
    module procedure nemsio_writerecvbin4d4
    module procedure nemsio_writerecvbin4d8
  end interface nemsio_writerecvbin4
!
  interface nemsio_writerecvbin8
    module procedure nemsio_writerecvbin8d4
    module procedure nemsio_writerecvbin8d8
  end interface nemsio_writerecvbin8
!
!--- file unit for putgb/getgb ----
  integer(nemsio_intkind),save   :: fileunit(600:699)=0
!------------------------------------------------------------------------------
!public mehtods
  public nemsio_undef_grb
  public nemsio_intkind,nemsio_intkind8,nemsio_realkind,nemsio_dblekind
  public nemsio_charkind,nemsio_charkind8,nemsio_logickind
  public nemsio_init,nemsio_finalize,nemsio_open,nemsio_close
  public nemsio_readrec,nemsio_writerec,nemsio_readrecv,nemsio_writerecv
  public nemsio_readrecw34,nemsio_writerecw34,nemsio_readrecvw34,nemsio_writerecvw34
  public nemsio_getfilehead,nemsio_getheadvar,nemsio_getrechead,nemsio_setfilehead
!
contains
!-------------------------------------------------------------------------------
  subroutine nemsio_init(iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! initialization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    integer(nemsio_intkind),optional,intent(out):: iret
    integer :: ios
!------------------------------------------------------------
! abstract: set grib table 
!------------------------------------------------------------
    call nemsio_setgrbtbl(ios)
    if ( present(iret)) iret=ios
    if ( ios.ne.0) then
       if (present(iret)) return
       call nemsio_stop
    endif
!
  end subroutine nemsio_init
!------------------------------------------------------------------------------
  subroutine nemsio_finalize()
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: finalization
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none

  end subroutine nemsio_finalize
!------------------------------------------------------------------------------
  subroutine nemsio_open(gfile,gfname,gaction,iret,gdatatype,version, &
      nmeta,lmeta,modelname,nrec,idate,nfday,nfhour,                  &
      nfminute,nfsecondn,nfsecondd,                                   &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,           &
      nmetavari,nmetavarr,nmetavarl,nmetavarc,                              &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,                              &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval  )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: open nemsio file, and read/write the meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    character*(*),intent(in)            :: gfname
    character*(*),intent(in)            :: gaction
!-------------------------------------------------------------------------------
! optional variables
!-------------------------------------------------------------------------------
    integer(nemsio_intkind),optional,intent(out) :: iret
    character*(*),optional,intent(in)            :: gdatatype,modelname
    integer(nemsio_intkind),optional,intent(in)  :: version,nmeta,lmeta,nrec
    integer(nemsio_intkind),optional,intent(in)  :: idate(7),nfday,nfhour,    &
            nfminute, nfsecondn,nfsecondd
    integer(nemsio_intkind),optional,intent(in)  :: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(nemsio_intkind),optional,intent(in)  :: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(nemsio_realkind),optional,intent(in)    :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(nemsio_intkind),optional,intent(in)  :: nmetavari,nmetavarr, &   
            nmetavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)  :: reclev(:)
    real(nemsio_realkind),optional,intent(in)    :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(in)    :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(in)    :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),aryiname(:),aryrname(:),arylname(:),arycname(:)
    integer(nemsio_intkind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:)
    integer(nemsio_intkind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(in)    :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
    integer(nemsio_intkind)      :: ios
!------------------------------------------------------------
! assign a unit number 
!------------------------------------------------------------
    if (present(iret)) iret=-1
    call nemsio_getlu(gfile,gfname,gaction,ios)
    if ( ios.ne.0 ) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! open and read meta data for READ
!------------------------------------------------------------
!    print *,'in rcreate, gfname=',gfname,'gaction=',lowercase(gaction)
    if ( equal_str_nocase(trim(gaction),'read') .or. equal_str_nocase(trim(gaction),'rdwr')) then
      if ( equal_str_nocase(trim(gaction),'read') )then
       call baopenr(gfile%flunit,gfname,ios)
       if ( ios.ne.0) then
        if ( present(iret))  then
          return
        else
          call nemsio_stop
        endif
       endif
      else
       call baopen(gfile%flunit,gfname,ios)
       if ( ios.ne.0) then
        if ( present(iret))  then
          return
        else
          call nemsio_stop
        endif
       endif
     endif
!     print *,'open read file=',gfname
!
! read  meta data for gfile
!
       call nemsio_rcreate(gfile,ios)
       if ( ios.ne.0) then
        if ( present(iret))  then
          iret=ios
          return
        else
          call nemsio_stop
        endif
       endif
!
!set grib index buf 
!
      if(gfile%gdatatype=='grib') then
       gfile%mbuf=256*1024
       gfile%nnum=0
       gfile%nlen=0
       gfile%mnum=-1
       if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
       allocate(gfile%cbuf(gfile%mbuf))
      endif
!------------------------------------------------------------
! open and write meta data for WRITE
!------------------------------------------------------------
    elseif ( equal_str_nocase(trim(gaction),'write') ) then
      call baopenwt(gfile%flunit,gfname,ios)
      if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
      endif
      call nemsio_wcreate(gfile,ios,gdatatype=gdatatype, &
        version=version, nmeta=nmeta,lmeta=lmeta,modelname=modelname,  &
        nrec=nrec,idate=idate,nfday=nfday,nfhour=nfhour,nfminute=nfminute,&
        nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
        dimx=dimx,dimy=dimy,dimz=dimz,nframe=nframe,nsoil=nsoil,   &
        ntrac=ntrac,jcap=jcap,ncldt=ncldt,idvc=idvc,idsl=idsl,    &
        idvm=idvm,idrt=idrt,                          &
        rlon_min=rlon_min,rlon_max=rlon_max,rlat_min=rlat_min, &
        rlat_max=rlat_max,extrameta=extrameta, &
        nmetavari=nmetavari,nmetavarr=nmetavarr,  &
        nmetavarl=nmetavarl,nmetaaryi=nmetaaryi,nmetaaryr=nmetaaryr,&
        nmetaaryl=nmetaaryl,recname=recname,reclevtyp=reclevtyp,    &
        reclev=reclev,vcoord=vcoord,lat=lat,lon=lon,dx=dx,dy=dy,    &
        cpi=cpi,ri=ri,variname=variname,varival=varival,varrname=varrname,&
        varrval=varrval,varlname=varlname,varlval=varlval, &
        varcname=varcname,varcval=varcval, &
        aryiname=aryiname,aryilen=aryilen,aryival=aryival, &
        aryrname=aryrname,aryrlen=aryrlen,aryrval=aryrval, &
        arylname=arylname,aryllen=aryllen,arylval=arylval, &
        arycname=arycname,aryclen=aryclen,arycval=arycval  )
      if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
     endif
!------------------------------------------------------------
! if gaction is wrong
!------------------------------------------------------------
    else
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! set default header
!------------------------------------------------------------
    if(.not.allocated(gfile%headvariname).or. &
       .not.allocated(gfile%headvarrname).or. &
       .not.allocated(gfile%headvarcname).or. &
       .not.allocated(gfile%headvarlname).or. &
       .not.allocated(gfile%headaryiname).or. &
       .not.allocated(gfile%headaryrname) ) then

      call nemsio_setfhead(gfile,ios)
      if ( present(iret)) iret=ios
      if ( ios.ne.0) then
        if (present(iret)) return
        call nemsio_stop
      endif
    endif

    iret=0
  end subroutine nemsio_open
!------------------------------------------------------------------------------
  subroutine nemsio_close(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! abstract: close gfile including closing the file, returning unit number, 
!           setting file meta data empty
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind)      :: ios
!------------------------------------------------------------
! close the file
!------------------------------------------------------------
    if ( present(iret) ) iret=-1
    call baclose(gfile%flunit,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! free the file unit
!------------------------------------------------------------
    call nemsio_clslu(gfile,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! empty gfile meta data
!------------------------------------------------------------
    call nemsio_axmeta(gfile,ios)
    if ( ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  end subroutine nemsio_close
!------------------------------------------------------------------------------
  subroutine nemsio_rcreate(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
!local variables
    integer(nemsio_intkind)      :: ios,nmeta
    integer(nemsio_intkind8)     :: iskip,iread,nread
    type(nemsio_meta1)           :: meta1
    type(nemsio_meta2)           :: meta2
    type(nemsio_meta3)           :: meta3
    integer(nemsio_intkind) :: i
    character(nemsio_charkind8),allocatable :: char8var(:)
!------------------------------------------------------------
! read first meta data record
!------------------------------------------------------------
    iret=-3
    iskip=0
    iread=nemsio_lmeta1
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta1)
    print *,'in rcreate, iread=',iread,'nread=',nread
    if(nread.lt.iread) return
    gfile%tlmeta=nread
    print *,'tlmeta=',gfile%tlmeta
    gfile%gtype=meta1%gtype
    gfile%version=meta1%version
    gfile%nmeta=meta1%nmeta
    gfile%lmeta=meta1%lmeta
    gfile%gdatatype=meta1%gdatatype
    gfile%modelname=meta1%modelname
    if ( trim(gfile%gdatatype).ne."bin4" .and. trim(gfile%gdatatype).ne."bin8" &
         .and. trim(gfile%gdatatype).ne."grib" ) then
      gfile%gdatatype="grib"
    endif
    if ( gfile%gtype(1:6) .ne. 'NEMSIO' ) then
      iret=-9
      return
    endif
    if ( gfile%nmeta .ne. 12 ) then
      print*,'WARNING: Not standard meta data, may not be ingested into GSI!!!'
!      iret=-9
!      return
    endif
!------------------------------------------------------------
! read second meta data record
!------------------------------------------------------------
    iskip=iskip+nread
    iread=gfile%lmeta
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta2)
    if(nread.lt.iread) return
    gfile%tlmeta=gfile%tlmeta+nread
!    print *,'tlmeta2 =',gfile%tlmeta,'iskip=',iskip,'iread=',iread,'nread=',nread
    gfile%nrec=meta2%nrec
    gfile%idate(1:7)=meta2%idate(1:7)
    gfile%nfday=meta2%nfday
    gfile%nfhour=meta2%nfhour
    gfile%nfminute=meta2%nfminute
    gfile%nfsecondn=meta2%nfsecondn
    gfile%nfsecondd=meta2%nfsecondd
    gfile%dimx=meta2%dimx
    gfile%dimy=meta2%dimy
    gfile%dimz=meta2%dimz
    gfile%nframe=meta2%nframe
    gfile%nsoil=meta2%nsoil
    gfile%ntrac=meta2%ntrac
    gfile%jcap=meta2%jcap
    gfile%ncldt=meta2%ncldt
    gfile%idvc=meta2%idvc
    gfile%idsl=meta2%idsl
    gfile%idvm=meta2%idvm
    gfile%idrt=meta2%idrt
    gfile%rlon_min=meta2%rlon_min
    gfile%rlon_max=meta2%rlon_max
    gfile%rlat_min=meta2%rlat_min
    gfile%rlat_max=meta2%rlat_max
    gfile%extrameta=meta2%extrameta
    gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)

    nmeta=gfile%nmeta-2
!------------------------------------------------------------
! set up gfile required meata arrays
!------------------------------------------------------------
    call nemsio_almeta(gfile,ios)
    if ( ios .ne. 0 ) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! read gfile meta data array (meta rec 3:13)
!------------------------------------------------------------
!meta3:recname
    if ( gfile%nmeta.lt.3 ) then
      print *,'WARNING: no names,level type and  &
     &   levs for the fields in the meta data in this nemsio file'
    endif
    if(gfile%nmeta-2>0) then
      iskip=iskip+nread
      iread=len(gfile%recname)*size(gfile%recname)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%recname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*size(gfile%recname)
         allocate(char8var(size(gfile%recname)))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%recname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetarecname =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-3>0 ) then
!meta4:reclevtyp
      iskip=iskip+nread
      iread=len(gfile%reclevtyp)*size(gfile%reclevtyp)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%reclevtyp)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetareclwvtyp =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-4 >0 ) then
!meta5:reclev
      iskip=iskip+nread
      iread=kind(gfile%reclev)*size(gfile%reclev)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%reclev)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetareclev =',gfile%tlmeta,'nread=',nread
    endif

    if (gfile%nmeta-5 >0 ) then
!meta6:vcoord
      iskip=iskip+nread
      iread=kind(gfile%vcoord)*size(gfile%vcoord)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%vcoord)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavcoord =',gfile%tlmeta,'nread=',nread
    endif

    if ( gfile%nmeta-6>0 ) then
!meta7:lat
      iskip=iskip+nread
      iread=kind(gfile%lat)*size(gfile%lat)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%lat)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetareclat =',gfile%tlmeta,'nread=',nread,   &
         maxval(gfile%lat),minval(gfile%lat)
    endif

    if ( gfile%nmeta-7>0 ) then
!meta8:lon
      iskip=iskip+nread
      iread=kind(gfile%lon)*size(gfile%lon)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%lon)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetareclon =',gfile%tlmeta,'nread=',nread,  &
         maxval(gfile%lat),minval(gfile%lat)
    endif

    if ( gfile%nmeta-8>0 ) then
!meta9:dx
      iskip=iskip+nread
      iread=kind(gfile%dx)*size(gfile%dx)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%dx)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetarecdx =',gfile%tlmeta,'nread=',nread,  &
        maxval(gfile%dx),minval(gfile%dx)
    endif

    if ( gfile%nmeta-9>0 ) then
!meta10:dy
      iskip=iskip+nread
      iread=kind(gfile%dy)*size(gfile%dy)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%dy)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetarecdy =',gfile%tlmeta,'nread=',nread, &
         maxval(gfile%dy),maxval(gfile%dy)
    endif

     if ( gfile%nmeta-10>0 ) then
!meta11:cpi
      iskip=iskip+nread
      iread=kind(gfile%cpi)*size(gfile%Cpi)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%Cpi)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetacpi =',gfile%tlmeta,'nread=',nread
    endif

    if ( gfile%nmeta-11>0 ) then
!Ri
      iskip=iskip+nread
      iread=kind(gfile%ri)*size(gfile%Ri)
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%Ri)
      if(nread.lt.iread) return
      nmeta=nmeta-1
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetri =',gfile%tlmeta,'nread=',nread
    endif
!
    if ( gfile%nmeta-12>0 ) then
      print *,'nmeta=',nmeta,' WARNING:there are more meta to be read!'
    endif
     
    if(gfile%extrameta) then
!------------------------------------------------------------
! read out extra meta data
!------------------------------------------------------------
    iskip=iskip+nread
    iread=nemsio_lmeta3
    call bafrreadl(gfile%flunit,iskip,iread,nread,meta3)
    if(nread.lt.iread) return
    gfile%tlmeta=gfile%tlmeta+nread
    gfile%nmetavari=meta3%nmetavari
    gfile%nmetavarr=meta3%nmetavarr
    gfile%nmetavarl=meta3%nmetavarl
    gfile%nmetavarc=meta3%nmetavarc
    gfile%nmetaaryi=meta3%nmetaaryi
    gfile%nmetaaryr=meta3%nmetaaryr
    gfile%nmetaaryl=meta3%nmetaaryl
    gfile%nmetaaryc=meta3%nmetaaryc
      print *,'tlmeta3 =',gfile%tlmeta,'nread=',nread, &
     'nmetavari=',gfile%nmetavari,'nvarr=',gfile%nmetavarr, &
     'varl=',gfile%nmetavarl,'varc=',gfile%nmetavarc, &
     gfile%nmetaaryi,gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc
   
    call nemsio_alextrameta(gfile,ios)
    if ( ios .ne. 0 ) then
      iret=ios
      return
    endif
    print *,'after nemsio_alextrameta'

!meta var integer
    if (gfile%nmetavari.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%variname)*gfile%nmetavari
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%variname)
      print *,'after get varint name,iskip=',iskip,'iread=',iread,'nread=',nread
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavari
         allocate(char8var(gfile%nmetavari))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%variname=char8var
         deallocate(char8var)
      print *,'after get varint name8,iskip=',iskip,'iread=',iread,'nread=',nread
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetavari =',gfile%tlmeta,'nread=',nread,'iread=',iread,gfile%nmetavari
      iskip=iskip+nread
      iread=nemsio_intkind*gfile%nmetavari
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varival)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread 
!      print *,'tlmetavarival =',gfile%tlmeta,'nread=',nread
    endif
!meta var real
    if (gfile%nmetavarr.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%varrname)*gfile%nmetavarr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varrname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarr
         allocate(char8var(gfile%nmetavarr))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varrname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetavarr =',gfile%tlmeta,'nread=',nread,gfile%nmetavarr
      iskip=iskip+nread
      iread=kind(gfile%varrval)*gfile%nmetavarr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varrval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarrval =',gfile%tlmeta,'nread=',nread
    endif
!meta var logical
    if (gfile%nmetavarl.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%varlname)*gfile%nmetavarl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varlname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarl
         allocate(char8var(gfile%nmetavarl))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varlname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetavarl =',gfile%tlmeta,'nread=',nread,gfile%nmetavarl
      iskip=iskip+nread
      iread=nemsio_logickind*gfile%nmetavarl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varlval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarlval =',gfile%tlmeta,'nread=',nread
    endif
!meta var string
    if (gfile%nmetavarc.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%varcname)*gfile%nmetavarc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varcname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetavarc
         allocate(char8var(gfile%nmetavarc))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%varcname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetavarc =',gfile%tlmeta,'nread=',nread,gfile%nmetavarc
      iskip=iskip+nread
      iread=len(gfile%varcval)*gfile%nmetavarc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%varcval)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
!      print *,'tlmetavarcval =',gfile%tlmeta,'nread=',nread
    endif
!meta arr integer
    if (gfile%nmetaaryi.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%aryiname)*gfile%nmetaaryi
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryiname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryi
         allocate(char8var(gfile%nmetaaryi))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%aryiname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryinam =',gfile%tlmeta,'nread=',nread
      iskip=iskip+nread
      iread=kind(gfile%nmetaaryi)*gfile%nmetaaryi
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryilen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryilen =',gfile%tlmeta,'nread=',nread
      allocate(gfile%aryival(maxval(gfile%aryilen),gfile%nmetaaryi))
      do i=1,gfile%nmetaaryi
        iskip=iskip+nread
        iread=kind(gfile%aryival)*gfile%aryilen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryival(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread

      print *,'tlmetaaryival =',gfile%tlmeta,'nread=',nread
      enddo
    endif
!meta arr real
    if (gfile%nmetaaryr.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%aryrname)*gfile%nmetaaryr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryr
         allocate(char8var(gfile%nmetaaryr))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%aryrname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryrnam =',gfile%tlmeta,'nread=',nread
      iskip=iskip+nread
      iread=kind(gfile%aryrlen)*gfile%nmetaaryr
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrlen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryrlen =',gfile%tlmeta,'nread=',nread
      allocate(gfile%aryrval(maxval(gfile%aryrlen),gfile%nmetaaryr) )
      do i=1,gfile%nmetaaryr
        iskip=iskip+nread
        iread=kind(gfile%aryrval)*gfile%aryrlen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryrval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
      print *,'tlmetaaryrval =',gfile%tlmeta,'nread=',nread
      enddo
    endif
!meta arr logical
    if (gfile%nmetaaryl.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%arylname)*gfile%nmetaaryl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arylname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryl
         allocate(char8var(gfile%nmetaaryl))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%arylname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      iskip=iskip+nread
      iread=kind(gfile%aryllen)*gfile%nmetaaryl
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryllen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      allocate(gfile%arylval(maxval(gfile%aryllen),gfile%nmetaaryl) )
      do i=1,gfile%nmetaaryl
        iskip=iskip+nread
        iread=kind(gfile%arylval)*gfile%aryllen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arylval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
      enddo
    endif
!meta arr char
    if (gfile%nmetaaryc.gt.0) then
      iskip=iskip+nread
      iread=len(gfile%arycname)*gfile%nmetaaryc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arycname)
      if(nread.lt.iread)  then
         iread=nemsio_charkind8*gfile%nmetaaryc
         allocate(char8var(gfile%nmetaaryc))
         call bafrreadl(gfile%flunit,iskip,iread,nread,char8var)
         gfile%arycname=char8var
         deallocate(char8var)
         if (nread.lt.iread) return
      endif
      gfile%tlmeta=gfile%tlmeta+nread
      iskip=iskip+nread
      iread=kind(gfile%aryclen)*gfile%nmetaaryc
      call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%aryclen)
      if(nread.lt.iread) return
      gfile%tlmeta=gfile%tlmeta+nread
      allocate(gfile%arycval(maxval(gfile%aryclen),gfile%nmetaaryc) )
      do i=1,gfile%nmetaaryc
        iskip=iskip+nread
        iread=len(gfile%arycval)*gfile%aryclen(i)
        call bafrreadl(gfile%flunit,iskip,iread,nread,gfile%arycval(:,i))
        if(nread.lt.iread) return
        gfile%tlmeta=gfile%tlmeta+nread
      enddo
    endif
!
!end if extrameta
   endif
!
   print *,'end of rcreate!'
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
   iret=0
  end subroutine nemsio_rcreate
!------------------------------------------------------------------------------
  subroutine nemsio_wcreate(gfile,iret,gdatatype,version,  &
      nmeta,lmeta,modelname,nrec,idate,nfday,              &
      nfhour,nfminute,nfsecondn,nfsecondd,                 &
      dimx,dimy,dimz,nframe,nsoil,ntrac,jcap,ncldt,idvc,idsl,idvm,idrt,     &
      rlon_min,rlon_max,rlat_min,rlat_max,extrameta,                        &
      nmetavari,nmetavarr,nmetavarl,nmetavarc,                              &
      nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc,                              &
      recname,reclevtyp,reclev,vcoord,lat,lon,dx,dy,cpi,ri,                 &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval,  &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,                    &
      arylname,aryllen,arylval,arycname,aryclen,arycval  )
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio meta data
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)            :: gfile
    integer(nemsio_intkind),intent(out)         :: iret
!optional variables
    character*(*),optional,intent(in)           :: gdatatype,modelname
    integer(nemsio_intkind),optional,intent(in)  :: version,nmeta,lmeta,nrec
    integer(nemsio_intkind),optional,intent(in)  :: idate(7),nfday,nfhour,  &
            nfminute,nfsecondn,nfsecondd
    integer(nemsio_intkind),optional,intent(in)  :: dimx,dimy,dimz,nframe,    &
            nsoil,ntrac
    integer(nemsio_intkind),optional,intent(in)  :: jcap,ncldt,idvc,idsl,     &
            idvm,idrt
    real(nemsio_realkind),optional,intent(in)    :: rlat_min,rlat_max,   &
             rlon_min,rlon_max
    logical(nemsio_logickind),optional,intent(in):: extrameta
    integer(nemsio_intkind),optional,intent(in)  :: nmetavari,nmetavarr, &
            nmetavarl,nmetavarc,nmetaaryi,nmetaaryr,nmetaaryl,nmetaaryc
!
    character*(*),optional,intent(in)            :: recname(:),reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)  :: reclev(:)
    real(nemsio_realkind),optional,intent(in)    :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(in)    :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(in)    :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(in)    :: Cpi(:),Ri(:)
!
    character*(*),optional,intent(in)            :: variname(:),varrname(:),&
          varlname(:),varcname(:),aryiname(:),aryrname(:),arylname(:),arycname(:)
    integer(nemsio_intkind),optional,intent(in)  :: aryilen(:),aryrlen(:),  &
          aryllen(:),aryclen(:)
    integer(nemsio_intkind),optional,intent(in)  :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(in)    :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(in):: varlval(:),arylval(:,:)
    character(*),optional,intent(in)             :: varcval(:),arycval(:,:)
!
!---  local variables
!
    real(nemsio_realkind) :: radi
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite
    type(nemsio_meta1)      :: meta1
    type(nemsio_meta2)      :: meta2
    type(nemsio_meta3)      :: meta3
    integer(nemsio_intkind) :: i,n,ios,nummeta
    logical :: linit
!------------------------------------------------------------
! set gfile meta data to operational model (default) if it's empty
!------------------------------------------------------------
    iret=-3
    gfile%gtype="NEMSIO"
    if(present(gdatatype)) then
      if ( trim(gdatatype).ne.'grib'.and.trim(gdatatype).ne.'bin4'.and. &
           trim(gdatatype).ne.'bin8' ) return
      gfile%gdatatype=gdatatype
    else
      gfile%gdatatype='grib'
    endif
    if(present(modelname)) then 
      gfile%modelname=modelname
    else
      gfile%modelname="GFS"
    endif
!
!    print *,'NEMSIO file,datatype,model is ',gfile%gtype, &
!        gfile%gdatatype,gfile%modelname,idate(1:7)
    if(present(version)) gfile%version=version
    if(present(dimx)) gfile%dimx=dimx
    if(present(dimy)) gfile%dimy=dimy
    if(present(dimz)) gfile%dimz=dimz
    if(present(nrec)) gfile%nrec=nrec
    if(present(nmeta)) gfile%nmeta=nmeta
    if(gfile%nmeta==nemsio_intfill) gfile%nmeta=12
    if(present(lmeta)) gfile%lmeta=lmeta
    if(gfile%lmeta==nemsio_intfill)   &
      gfile%lmeta=25*nemsio_intkind+4*nemsio_realkind+nemsio_logickind
!    print *,'in wcreate, dim=',gfile%dimx,gfile%dimy,gfile%dimz,'nmeta=',gfile%nmeta,gfile%lmeta
    if(present(nsoil)) gfile%nsoil=nsoil
    if(gfile%nsoil.eq.nemsio_intfill) gfile%nsoil=4
    if(present(nframe)) gfile%nframe=nframe
    if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=0
    if(gfile%modelname=='GFS')gfile%nframe=0
    if(present(idate)) gfile%idate=idate
    if ( gfile%idate(1) .lt. 50) then
        gfile%idate(1)=2000+gfile%idate(1)
    else if (gfile%idate(1) .lt. 100) then
        gfile%idate(1)=1999+gfile%idate(1)
    endif
    if ( gfile%idate(1).eq.nemsio_intfill) then
      print *,'idate=',gfile%idate,' WRONG: please provide idate(1:7)(yyyy/mm/dd/hh/min/secn/secd)!!!'
      call nemsio_stop()
    endif
!
    linit= gfile%dimx .eq. nemsio_intfill .or. gfile%dimy .eq. nemsio_intfill &
      .or. gfile%dimz .eq. nemsio_intfill .or. gfile%nrec .eq. nemsio_intfill &
      .or. gfile%nmeta .eq. 12
!    
    if ( gfile%gtype(1:6).eq."NEMSIO" .and. linit ) then
      call nemsio_gfinit(gfile,ios,recname=recname,reclevtyp=reclevtyp,reclev=reclev)
      if (ios .ne.0 ) then
        iret=ios
        return
      endif
    endif
    print*,'after gfinit,gtype=',gfile%gtype,'nmeta=',gfile%nmeta,'linit=',linit
!
!------------------------------------------------------------
! set up basic gfile meta data variables from outsides to 
! define meta data array
!------------------------------------------------------------
    if(present(nfday)) gfile%nfday=nfday
    if(present(nfhour)) gfile%nfhour=nfhour
    if(present(nfminute)) gfile%nfminute=nfminute
    if(present(nfsecondn)) gfile%nfsecondn=nfsecondn
    if(present(nfsecondd)) gfile%nfsecondd=nfsecondd
    if(present(ntrac)) gfile%ntrac=ntrac
    if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=0
    if(present(ncldt)) gfile%ncldt=ncldt
    if(present(jcap)) gfile%jcap=jcap
    if(present(idvc)) gfile%idvc=idvc
    if(present(idsl)) gfile%idsl=idsl
    if(present(idvm)) gfile%idvm=idvm
    if(present(idrt)) gfile%idrt=idrt
    if(present(rlon_min)) gfile%rlon_min=rlon_min
    if(present(rlon_max)) gfile%rlon_max=rlon_max
    if(present(rlat_min)) gfile%rlat_min=rlat_min
    if(present(rlat_max)) gfile%rlat_max=rlat_max
    if(present(extrameta)) gfile%extrameta=extrameta
    if(gfile%fieldsize.eq.nemsio_intfill) &
       gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
!
    if( gfile%extrameta )then
      if(present(nmetavari).and.nmetavari.gt.0.and.present(variname) &
         .and.size(variname).eq.nmetavari .and. &
         present(varival).and.size(varival).eq.nmetavari) then
           gfile%nmetavari=nmetavari
           if(allocated(gfile%variname)) deallocate(gfile%variname)
           if(allocated(gfile%varival)) deallocate(gfile%varival)
           allocate(gfile%variname(nmetavari),gfile%varival(nmetavari))
           gfile%variname=variname
           gfile%varival=varival
      endif
      if(present(nmetavarr).and.nmetavarr.gt.0.and.present(varrname) &
         .and.size(varrname).eq.nmetavarr .and. &
         present(varrval).and.size(varrval).eq.nmetavarr) then
           gfile%nmetavarr=nmetavarr
           if(allocated(gfile%varrname)) deallocate(gfile%varrname)
           if(allocated(gfile%varrval)) deallocate(gfile%varrval)
           allocate(gfile%varrname(nmetavarr),gfile%varrval(nmetavarr))
           gfile%varrname=varrname
           gfile%varrval=varrval
      endif
      if(present(nmetavarl).and.nmetavarl.gt.0.and.present(varlname) &
         .and.size(varlname).eq.nmetavarl .and. &
         present(varlval).and.size(varlval).eq.nmetavarl) then
           gfile%nmetavarl=nmetavarl
           if(allocated(gfile%varlname)) deallocate(gfile%varlname)
           if(allocated(gfile%varlval)) deallocate(gfile%varlval)
           allocate(gfile%varlname(nmetavarl),gfile%varlval(nmetavarl))
           gfile%varlname=varlname
           gfile%varlval=varlval
      endif
      if(present(nmetavarc).and.nmetavarc.gt.0.and.present(varcname) &
         .and.size(varcname).eq.nmetavarc .and. &
         present(varcval).and.size(varcval).eq.nmetavarc) then
           gfile%nmetavarc=nmetavarc
           if(allocated(gfile%varcname)) deallocate(gfile%varcname)
           if(allocated(gfile%varcval)) deallocate(gfile%varcval)
           allocate(gfile%varcname(nmetavarc),gfile%varcval(nmetavarc))
           gfile%varcname=varcname
           gfile%varcval=varcval
      endif
      if(present(nmetaaryi).and.nmetaaryi.gt.0.and.present(aryiname) &
         .and.size(aryiname).eq.nmetaaryi .and. &
         present(aryilen).and.size(aryilen).eq.nmetaaryi) then
           gfile%nmetaaryi=nmetaaryi
           if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
           if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
           allocate(gfile%aryiname(nmetaaryi),gfile%aryilen(nmetaaryi))
           gfile%aryiname=aryiname
           gfile%aryilen=aryilen
           if(present(aryival).and.size(aryival).eq.nmetaaryi*maxval(gfile%aryilen) ) then
             if(allocated(gfile%aryival)) deallocate(gfile%aryival)
             allocate(gfile%aryival(maxval(gfile%aryilen),nmetaaryi))
             gfile%aryival=aryival
           endif
      endif
      if(present(nmetaaryr).and.nmetaaryr.gt.0.and.present(aryrname) &
         .and.size(aryrname).eq.nmetaaryr .and. &
         present(aryrlen).and.size(aryrlen).eq.nmetaaryr) then
           gfile%nmetaaryr=nmetaaryr
           if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
           if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
           allocate(gfile%aryrname(nmetaaryr),gfile%aryrlen(nmetaaryr))
           gfile%aryrname=aryrname
           gfile%aryrlen=aryrlen
!           print *,'in wcreate,gfile%aryrname=',gfile%aryrname
!           print *,'in wcreate,gfile%aryrlen=',gfile%aryrlen
           if(present(aryrval).and.size(aryrval).eq.nmetaaryr*maxval(gfile%aryrlen)) then
             if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
             allocate(gfile%aryrval(maxval(gfile%aryrlen),nmetaaryr))
             gfile%aryrval=aryrval
           endif
      endif
      if(present(nmetaaryl).and.nmetaaryl.gt.0.and.present(arylname) &
          .and.size(arylname).eq.nmetaaryl .and. &
           present(aryllen).and.size(aryllen).eq.nmetaaryl) then
           gfile%nmetaaryl=nmetaaryl
           if(allocated(gfile%arylname)) deallocate(gfile%arylname)
           if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
           allocate(gfile%arylname(nmetaaryl),gfile%aryllen(nmetaaryl))
           gfile%arylname=arylname
           gfile%aryllen=aryllen
           if(present(arylval).and.size(arylval).eq.nmetaaryl*maxval(gfile%aryllen)) then
             if(allocated(gfile%arylval)) deallocate(gfile%arylval)
             allocate(gfile%arylval(maxval(gfile%aryllen),nmetaaryl))
             gfile%arylval=arylval
           endif
      endif
      if(present(nmetaaryc).and.nmetaaryc.gt.0.and.present(arycname) &
          .and.size(arycname).eq.nmetaaryc .and. &
          present(aryclen).and.size(aryclen).eq.nmetaaryc) then
           gfile%nmetaaryc=nmetaaryc
           if(allocated(gfile%arycname)) deallocate(gfile%arycname)
           if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
           allocate(gfile%arycname(nmetaaryc),gfile%aryclen(nmetaaryc))
           gfile%arycname=arycname
           gfile%aryclen=aryclen
           if(present(arycval).and.size(arycval).eq.nmetaaryc*maxval(gfile%aryclen)) then
             if(allocated(gfile%arycval)) deallocate(gfile%arycval)
             allocate(gfile%arycval(maxval(gfile%aryclen),nmetaaryc))
             gfile%arycval=arycval
           endif
      endif
      if (gfile%nmetavari+gfile%nmetavarr+gfile%nmetavarl+gfile%nmetavarc+ &
           gfile%nmetaaryi+gfile%nmetaaryr+gfile%nmetaaryl+gfile%nmetaaryc &
           .lt.8*nemsio_intfill )then
           print *,'WRONG: gfile%extrameta is not compatiable with input extra meta!'
           return
      endif
    endif 
!------------------------------------------------------------
! check gfile meta data array size
!------------------------------------------------------------
    call nemsio_chkgfary(gfile,ios)
    if (ios.ne. 0) then
      iret=ios
      return
    endif
!------------------------------------------------------------
! continue to set gfile meta data variables tnd arrays
!------------------------------------------------------------
!set gfile data type to bin/grb, default set to grb
!recname
    if(present(recname) ) then
!       print*,'gfile%nrec=',gfile%nrec,'size(recname)=',size(recname)
       if (gfile%nrec.eq.size(recname)) then
         gfile%recname=recname
       else
         print *,'WRONG: the size of recname is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!reclevtyp
    if(present(reclevtyp)) then
       if (gfile%nrec.eq.size(reclevtyp)) then
         gfile%reclevtyp=reclevtyp
       else
         print *,'WRONG: the size of reclevtyp is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!reclev
    if(present(reclev) ) then
       if (gfile%nrec.eq.size(reclev)) then
         gfile%reclev=reclev
       else
         print *,'WRONG: the size of reclev is not equal to the total number of the fields in the file!'
         return
       endif
    endif
!
!vcoord vcoord(levs+1
    if(present(vcoord) ) then
       if ((gfile%dimz+1)*3*2.eq.size(vcoord)) then
         gfile%vcoord=vcoord
       else
         print *,'WRONG: the size of vcoord is not (lm+1,3,2) !'
         return
       endif
    endif
!lat
    if(present(lat) ) then
       write(0,*)'gfile%fieldsize=',gfile%fieldsize,'size(lat)=',size(lat)
       if (gfile%fieldsize.eq.size(lat)) then
         if(.not.(all(lat==0.))) gfile%lat=lat
       else
         print *,'WRONG: the input size(lat) ',size(lat),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
    gfile%rlat_max=maxval(gfile%lat)
    gfile%rlat_min=minval(gfile%lat)
!lon
    if(present(lon) ) then
       if (gfile%fieldsize.eq.size(lon)) then
         if(.not.(all(lon==0.)) ) gfile%lon=lon
       else
         print *,'WRONG: the input size(lon) ',size(lon),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
    gfile%rlon_max=maxval(gfile%lon)
    gfile%rlon_min=minval(gfile%lon)
!dx
    if(present(dx) ) then
       write(0,*)'gfile%fieldsize=',gfile%fieldsize,'size(dx)=',size(dx)
       if (gfile%fieldsize.eq.size(dx)) then
         if(.not.(all(dx==0.)) ) gfile%dx=dx
       else
         print *,'WRONG: the input size(dx) ',size(dx),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
!dy
    if(present(dy) ) then
       if (gfile%fieldsize.eq.size(dy)) then
         if(.not.(all(dy==0.)) ) gfile%dy=dy
       else
         print *,'WRONG: the input size(dy) ',size(dy),' is not equal to: ',gfile%fieldsize
         return
       endif
    endif
!Cpi
    if( present(Cpi) ) then
       if (gfile%ntrac+1.eq.size(gfile%Cpi)) then
         if(.not.(all(cpi==0.))) gfile%Cpi = Cpi
       else
         print *,'WRONG: the input size(cpi) ',size(cpi),' is not equal to: ',gfile%ntrac+1
         return
       endif
!       print *,'in wcreate,cpi=',maxval(cpi),minval(cpi),maxval(gfile%Cpi),minval(cpi),&
!        'size(cpi)=',size(cpi),size(gfile%Cpi),'ntrac=',gfile%ntrac

    endif
!Ri
    if( present(Ri) ) then
       if (gfile%ntrac+1.eq.size(gfile%Ri)) then
         if(.not.(all(ri==0.))) gfile%Ri = Ri
       else
         print *,'WRONG: the input size(ri) ',size(ri),' is not equal to: ',gfile%ntrac+1
         return
       endif
    endif
!       print *,'in wcreate,ri=',maxval(ri),minval(ri),maxval(gfile%ri),minval(ri),&
!        'size(ri)=',size(ri),size(gfile%ri),'ntrac=',gfile%ntrac
!
!------------------------------------------------------------
! write out first meta data record
!------------------------------------------------------------
    meta1%gtype=gfile%gtype
    meta1%gdatatype=gfile%gdatatype
    meta1%modelname=gfile%modelname
    meta1%version=gfile%version
    meta1%nmeta=gfile%nmeta
    meta1%lmeta=gfile%lmeta
    meta1%reserve=0
    iskip=0
    iwrite=nemsio_lmeta1
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta1)
    if(nwrite.lt.iwrite) return
    gfile%tlmeta=nwrite
!    print *,'tlmet1 =',gfile%tlmeta,'nwrite=',nwrite,meta1%gdatatype
!------------------------------------------------------------
! write out second meta data record
!------------------------------------------------------------
    meta2%nrec=gfile%nrec
    meta2%idate(1:7)=gfile%idate(1:7)
    meta2%nfday=gfile%nfday
    meta2%nfhour=gfile%nfhour
    meta2%nfminute=gfile%nfminute
    meta2%nfsecondn=gfile%nfsecondn
    meta2%nfsecondd=gfile%nfsecondd
    meta2%dimx=gfile%dimx
    meta2%dimy=gfile%dimy
    meta2%dimz=gfile%dimz
    meta2%nframe=gfile%nframe
    meta2%nsoil=gfile%nsoil
    meta2%ntrac=gfile%ntrac
    meta2%jcap=gfile%jcap
    meta2%ncldt=gfile%ncldt
    meta2%idvc=gfile%idvc
    meta2%idsl=gfile%idsl
    meta2%idvm=gfile%idvm
    meta2%idrt=gfile%idrt
    meta2%rlon_min=gfile%rlon_min
    meta2%rlon_max=gfile%rlon_max
    meta2%rlat_min=gfile%rlat_min
    meta2%rlat_max=gfile%rlat_max
    meta2%extrameta=gfile%extrameta
    iskip=iskip+nwrite
    iwrite=gfile%lmeta
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta2)
    if(nwrite.lt.iwrite) return
    gfile%tlmeta=gfile%tlmeta+nwrite
!------------------------------------------------------------
! write out 3rd-13th meta data record (arrays)
!------------------------------------------------------------
!recname
    if ( gfile%nmeta-2>0 ) then
      iskip=iskip+nwrite
      iwrite=len(gfile%recname)*size(gfile%recname)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%recname)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecname =',gfile%tlmeta,'nwrite=',nwrite
    endif

!reclevtyp
    if ( gfile%nmeta-3>0 ) then
      iskip=iskip+nwrite
      iwrite=len(gfile%reclevtyp)*size(gfile%reclevtyp)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%reclevtyp)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclevty=',gfile%tlmeta,'nwrite=',nwrite
    endif

!reclev
    if ( gfile%nmeta-4>0 ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%reclev)*size(gfile%reclev)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%reclev)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclev=',gfile%tlmeta,'nwrite=',nwrite
    endif
!vcoord
    nummeta=gfile%nmeta-5
    if ( nummeta.gt.0 ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%vcoord)*size(gfile%vcoord)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%vcoord)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetavcoord=',gfile%tlmeta,'nwrite=',nwrite,'nummeta=', &
!        nummeta,'gfile%nmeta=',gfile%nmeta
      nummeta=nummeta-1
    endif
!lat
    if ( nummeta.gt.0 ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%lat)*size(gfile%lat)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lat)
      if(nwrite.lt.iwrite) return
      gfile%tlmetalat=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclat=',gfile%tlmeta,'nwrite=',nwrite,  &
!       maxval(gfile%lat),minval(gfile%lat)
      nummeta=nummeta-1
    endif
!lon
    if ( nummeta.gt.0 ) then
      iskip=iskip+nwrite
      iwrite=kind(gfile%lon)*size(gfile%lon)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lon)
      if(nwrite.lt.iwrite) return
      gfile%tlmetalon=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreclon=',gfile%tlmeta,'nwrite=',nwrite,'nummeta=',nummeta, &
!         maxval(gfile%lon),minval(gfile%lon)
      nummeta=nummeta-1
    endif
!dx
    if ( nummeta.gt.0 ) then
      if(all(gfile%dx==0.)) gfile%dx=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%dx)*size(gfile%dx)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dx)
!      print *,'tlmetrecdx=',gfile%tlmeta,'iwrite=',iwrite,'nwrite=',nwrite, &
!         maxval(gfile%dx),minval(gfile%dx)
      if(nwrite.lt.iwrite) return
      gfile%tlmetadx=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecdx=',gfile%tlmeta,'nwrite=',nwrite,  &
!        maxval(gfile%dx),minval(gfile%dx),maxval(gfile%dy),maxval(gfile%dy)
      nummeta=nummeta-1
    endif
!dy
    if ( nummeta.gt.0 ) then
      if(all(gfile%dy==0.)) gfile%dy=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%dy)*size(gfile%dy)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dy)
      if(nwrite.lt.iwrite) return
      gfile%tlmetady=gfile%tlmeta
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecdy=',gfile%tlmeta,'nwrite=',nwrite
      nummeta=nummeta-1
    endif
!Cpi
    if ( nummeta.gt.0 ) then
      if(all(gfile%cpi==0.)) gfile%cpi=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%Cpi)*size(gfile%Cpi)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%Cpi)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetreccpi=',gfile%tlmeta,'nwrite=',nwrite,  &
!        'cpi=',maxval(gfile%cpi),minval(gfile%cpi)
      nummeta=nummeta-1
    endif
!Ri
    if ( nummeta.gt.0 ) then
      if(all(gfile%ri==0.)) gfile%ri=nemsio_realfill
      iskip=iskip+nwrite
      iwrite=kind(gfile%Ri)*size(gfile%Ri)
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%Ri)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetrecri=',gfile%tlmeta,'nwrite=',nwrite,   &
!        'ri=',maxval(gfile%ri),minval(gfile%ri)
      nummeta=nummeta-1
    endif
!------------------------------------------------------------
! write out extra meta data record 
!------------------------------------------------------------
    if(gfile%extrameta) then
      meta3%nmetavari=gfile%nmetavari
      meta3%nmetavarr=gfile%nmetavarr
      meta3%nmetavarl=gfile%nmetavarl
      meta3%nmetavarc=gfile%nmetavarc
      meta3%nmetaaryi=gfile%nmetaaryi
      meta3%nmetaaryr=gfile%nmetaaryr
      meta3%nmetaaryl=gfile%nmetaaryl
      meta3%nmetaaryc=gfile%nmetaaryc
      iskip=iskip+nwrite
      iwrite=nemsio_lmeta3
      call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,meta3)
      if(nwrite.lt.iwrite) return
      gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'tlmetameta3=',gfile%tlmeta
!
!-- write meta var integer
      if (gfile%nmetavari.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%variname)*gfile%nmetavari
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%variname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varival)*gfile%nmetavari
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varival)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
!      print *,'rlmetavari=',gfile%tlmeta
      endif
      if (gfile%nmetavarr.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varrname)*gfile%nmetavarr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varrname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varrval)*gfile%nmetavarr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varrval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
      if (gfile%nmetavarl.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varlname)*gfile%nmetavarl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varlname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%varlval)*gfile%nmetavarl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varlval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
      if (gfile%nmetavarc.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%varcname)*gfile%nmetavarc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varcname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=len(gfile%varcval)*gfile%nmetavarc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%varcval)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
      endif
!meta arr integer
      if (gfile%nmetaaryi.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%aryiname)*gfile%nmetaaryi
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryiname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryilen)*gfile%nmetaaryi
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryilen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        do i=1,gfile%nmetaaryi
          iskip=iskip+nwrite
          iwrite=kind(gfile%aryival)*gfile%aryilen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%aryival(1:gfile%aryilen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetaryint=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
!          print *,'after tlmetaryi ',gfile%nmetaaryr,gfile%nmetaaryl,gfile%nmetaaryc
      endif
!meta arr real
      if (gfile%nmetaaryr.gt.0) then
!          print *,'before tlmetaryr'
        iskip=iskip+nwrite
        iwrite=len(gfile%aryrname)*gfile%nmetaaryr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryrname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'before tlmetaryr 1'
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryrlen)*gfile%nmetaaryr
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryrlen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'before tlmetaryr 2'
        do i=1,gfile%nmetaaryr
          iskip=iskip+nwrite
          iwrite=kind(gfile%aryrval)*gfile%aryrlen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%aryrval(1:gfile%aryrlen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetaryreal=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
!meta arr logical
      if (gfile%nmetaaryl.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%arylname)*gfile%nmetaaryl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%arylname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryllen)*gfile%nmetaaryl
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryllen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        do i=1,gfile%nmetaaryl
          iskip=iskip+nwrite
          iwrite=kind(gfile%arylval)*gfile%aryllen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%arylval(1:gfile%aryllen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetarylogic=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
!meta arr logical
      if (gfile%nmetaaryc.gt.0) then
        iskip=iskip+nwrite
        iwrite=len(gfile%arycname)*gfile%nmetaaryc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%arycname)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        iskip=iskip+nwrite
        iwrite=kind(gfile%aryclen)*gfile%nmetaaryc
        call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%aryclen)
        if(nwrite.lt.iwrite) return
        gfile%tlmeta=gfile%tlmeta+nwrite
        do i=1,gfile%nmetaaryc
          iskip=iskip+nwrite
          iwrite=len(gfile%arycval)*gfile%aryclen(i)
          call bafrwritel(gfile%flunit,iskip,iwrite,nwrite, &
                         gfile%arycval(1:gfile%aryclen(i),i))
          if(nwrite.lt.iwrite) return
          gfile%tlmeta=gfile%tlmeta+nwrite
!          print *,'tlmetarycogic=',i,gfile%tlmeta,'nwrite=',nwrite
        enddo
      endif
    endif
!    print *,'end of wcreate,nsoil=',gfile%nsoil,'nrec=',gfile%nrec,'tlmeta=',gfile%tlmeta

    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_wcreate
!------------------------------------------------------------------------------
  subroutine nemsio_setfilehead(gfile,iret,lat,lon,dx,dy)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: reset some nemsio meta data information from outside
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
!
    implicit none
    type(nemsio_gfile),intent(inout)             :: gfile
    integer(nemsio_intkind),optional,intent(out) :: iret
    real(nemsio_realkind),optional,intent(in)    :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(in)    :: dx(:),dy(:)
!
!--- local vars
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite
!
!---
    if (present(iret)) iret=-3
!
!--- check the size first, then set the value
!--- lat
    if(present(lat) ) then
       if (size(lat).ne.gfile%fieldsize) then
         if ( present(iret))  return
         call nemsio_stop
       else
         gfile%lat=lat
         if(equal_str_nocase(trim(gfile%gaction),'write') .and. &
             gfile%tlmetalat/=nemsio_intfill8) then
            iskip=gfile%tlmetalat
            iwrite=kind(gfile%lat)*size(gfile%lat)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lat)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- lon
    if(present(lon) ) then
       if (size(lon).ne.gfile%fieldsize) then
         if ( present(iret)) return
         call nemsio_stop
       else
         gfile%lon=lon
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetalon/=nemsio_intfill8) then
            iskip=gfile%tlmetalon
            iwrite=kind(gfile%lon)*size(gfile%lon)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%lon)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- dx
    if(present(dx) ) then
       print *,'getfilehead, size(dx)=',size(dx),gfile%fieldsize,  &
          maxval(gfile%dx),minval(gfile%dx)
       if (size(dx).ne.gfile%fieldsize) then
         if ( present(iret))  return
         call nemsio_stop
       else
         gfile%dx=dx
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetadx/=nemsio_intfill8) then
            iskip=gfile%tlmetadx
            iwrite=kind(gfile%dx)*size(gfile%dx)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dx)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!--- dy
    if(present(dy) ) then
!       print *,'getfilehead, size(dy)=',size(dy),gfile%fieldsize,  &
!          maxval(gfile%dy),minval(gfile%dy)
       if (size(dy).ne.gfile%fieldsize) then
         if ( present(iret)) return
         call nemsio_stop
       else
         gfile%dy=dy
         if(equal_str_nocase(trim(gfile%gaction),'write').and.   &
             gfile%tlmetady/=nemsio_intfill8) then
            iskip=gfile%tlmetady
            iwrite=kind(gfile%dy)*size(gfile%dy)
            call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,gfile%dy)
            if(nwrite.lt.iwrite) return
         endif
       endif
    endif
!
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_setfilehead
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
  subroutine nemsio_getfilehead(gfile,iret,gtype,gdatatype,gfname,gaction, &
      modelname,version,nmeta,lmeta,nrec,idate,nfday,nfhour,nfminute, &
      nfsecondn,nfsecondd,dimx,dimy,dimz,nframe,nsoil,ntrac,ncldt,jcap,&
      idvc,idsl,idvm,idrt, rlon_min,rlon_max,rlat_min,rlat_max,tlmeta, &
      extrameta,nmetavari,nmetavarr,nmetavarl,nmetavarc,nmetaaryi,nmetaaryr, &
      nmetaaryl,nmetaaryc,    &
      recname,reclevtyp,reclev,vcoord,lon,lat,dx,dy,cpi,ri, &
      variname,varival,varrname,varrval,varlname,varlval,varcname,varcval, &
      aryiname,aryilen,aryival,aryrname,aryrlen,aryrval,    &
      arylname,aryllen,arylval,arycname,aryclen,arycval    )

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get nemsio meta data information from outside
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(nemsio_intkind),optional,intent(out) :: iret
    character*(*),optional,intent(out)           :: gtype,gdatatype,gfname, &
                                                    gaction,modelname
    integer(nemsio_intkind),optional,intent(out) :: version,nmeta,lmeta
    integer(nemsio_intkind),optional,intent(out) :: nrec,idate(7),nfday,nfhour, &
                                                    nfminute,nfsecondn,nfsecondd
    integer(nemsio_intkind),optional,intent(out) :: dimx,dimy,dimz,nframe, &
                                                    nsoil,ntrac
    integer(nemsio_intkind),optional,intent(out) :: ncldt,jcap,idvc,idsl,idvm,idrt
    real(nemsio_realkind),optional,intent(out)   :: rlon_min,rlon_max,rlat_min, &
                                                    rlat_max
    integer(nemsio_intkind),optional,intent(out)  :: tlmeta
    logical(nemsio_logickind),optional,intent(out):: extrameta
    integer(nemsio_intkind),optional,intent(out)  :: nmetavari,nmetavarr, &
                                                    nmetavarl,nmetavarc,nmetaaryi, &
                                                    nmetaaryr,nmetaaryl,nmetaaryc
    character(*),optional,intent(out)            :: recname(:)
    character(*),optional,intent(out)            :: reclevtyp(:)
    integer(nemsio_intkind),optional,intent(out) :: reclev(:)
    real(nemsio_realkind),optional,intent(out)   :: vcoord(:,:,:)
    real(nemsio_realkind),optional,intent(out)   :: lat(:),lon(:)
    real(nemsio_realkind),optional,intent(out)   :: dx(:),dy(:)
    real(nemsio_realkind),optional,intent(out)   :: Cpi(:),Ri(:)
    character(*),optional,intent(out)            :: variname(:),varrname(:)
    character(*),optional,intent(out)            :: varlname(:),varcname(:)
    character(*),optional,intent(out)            :: aryiname(:),aryrname(:)
    character(*),optional,intent(out)            :: arylname(:),arycname(:)
    integer(nemsio_intkind),optional,intent(out) :: aryilen(:),aryrlen(:)
    integer(nemsio_intkind),optional,intent(out) :: aryllen(:),aryclen(:)
    integer(nemsio_intkind),optional,intent(out) :: varival(:),aryival(:,:)
    real(nemsio_realkind),optional,intent(out)   :: varrval(:),aryrval(:,:)
    logical(nemsio_logickind),optional,intent(out):: varlval(:),arylval(:,:)
    character(*),optional,intent(out)             :: varcval(:),arycval(:,:)
!
    integer i,j
!------------------------------------------------------------
    if (present(iret)) iret=-3
    if(present(gtype)) gtype=gfile%gtype
    if(present(gdatatype)) gdatatype=gfile%gdatatype
    if(present(gfname)) gfname=trim(gfile%gfname)
    if(present(gaction)) gaction=gfile%gaction
    if(present(modelname)) modelname=gfile%modelname
    if(present(version)) version=gfile%version
    if(present(nmeta)) nmeta=gfile%nmeta
    if(present(lmeta)) lmeta=gfile%lmeta
    if(present(nrec)) nrec=gfile%nrec
    if(present(nfday)) nfday=gfile%nfday
    if(present(nfhour)) nfhour=gfile%nfhour
    if(present(nfminute)) nfminute=gfile%nfminute
    if(present(nfsecondn)) nfsecondn=gfile%nfsecondn
    if(present(nfsecondd)) nfsecondd=gfile%nfsecondd
    if(present(idate)) idate=gfile%idate
    if(present(dimx)) dimx=gfile%dimx
    if(present(dimy)) dimy=gfile%dimy
    if(present(dimz)) dimz=gfile%dimz
    if(present(nframe)) nframe=gfile%nframe
    if(present(nsoil)) nsoil=gfile%nsoil
    if(present(ntrac)) ntrac=gfile%ntrac
    if(present(jcap)) jcap=gfile%jcap
    if(present(ncldt)) ncldt=gfile%ncldt
    if(present(idvc)) idvc=gfile%idvc
    if(present(idsl)) idsl=gfile%idsl
    if(present(idvm)) idvm=gfile%idvm
    if(present(idrt)) idrt=gfile%idrt
    if(present(rlon_min)) rlon_min=gfile%rlon_min
    if(present(rlon_max)) rlon_max=gfile%rlon_max
    if(present(rlat_min)) rlat_min=gfile%rlat_min
    if(present(rlat_max)) rlat_max=gfile%rlat_max
    if(present(rlat_max)) rlat_max=gfile%rlat_max
    if(present(tlmeta)) tlmeta=gfile%tlmeta
    if(present(extrameta)) extrameta=gfile%extrameta
!
!    print *,'in getfilehead, 1extrameta=',gfile%extrameta,        &
!     'nrec=',gfile%nrec,'size(recname)=',size(recname),           &
!     size(reclevtyp),size(reclev)
!--- rec
    if(present(recname) ) then
       if (gfile%nrec.ne.size(recname)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         recname=gfile%recname
       endif
    endif
    if(present(reclevtyp)) then
       if (gfile%nrec.ne.size(reclevtyp)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclevtyp=gfile%reclevtyp
       endif
    endif
    if(present(reclev) ) then
       if (gfile%nrec.ne.size(reclev)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         reclev=gfile%reclev
       endif
    endif
!--- vcoord
    if(present(vcoord)) then
       if (size(vcoord) .ne. (gfile%dimz+1)*2*3 ) then
         if ( present(iret))  return
         call nemsio_stop
       else
         vcoord=gfile%vcoord
       endif
    endif
!--- lat
    if(present(lat) ) then
       if (size(lat).ne.gfile%fieldsize) then
         print *,'WRONG: size(lat)=',size(lat),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         lat=gfile%lat
       endif
    endif
!--- lon
    if(present(lon) ) then
       if (size(lon).ne.gfile%fieldsize) then
         print *,'WRONG: size(lon)=',size(lon),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         lon=gfile%lon
       endif
    endif
!--- dx
    if(present(dx) ) then
       print *,'getfilehead, size(dx)=',size(dx),gfile%fieldsize,  &
          maxval(gfile%dx),minval(gfile%dx)
       if (size(dx).ne.gfile%fieldsize) then
         print *,'WRONG: size(dX)=',size(dx),' is not equal to ',gfile%fieldsize
         if ( present(iret))  return
         call nemsio_stop
       else
         dx=gfile%dx
       endif
    endif
!--- dy
    if(present(dy) ) then
!       print *,'getfilehead, size(dy)=',size(dy),gfile%fieldsize,  &
!          maxval(gfile%dy),minval(gfile%dy)
       if (size(dy).ne.gfile%fieldsize) then
         print *,'WRONG: size(dy)=',size(dy),' is not equal to ',gfile%fieldsize
         if ( present(iret)) return
         call nemsio_stop
       else
         dy=gfile%dy
       endif
    endif
!--- Cpi
    if(present(Cpi) ) then
       if (gfile%ntrac+1.ne.size(Cpi)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Cpi=gfile%Cpi
       endif
    endif
!--- Ri
    if(present(Ri) ) then 
       if (gfile%ntrac+1.ne.size(Ri)) then
         if ( present(iret)) return
         call nemsio_stop
       else
         Ri=gfile%Ri
       endif
    endif
!------------------------------------------------------------------------------
!*** for extra meta field
!------------------------------------------------------------------------------
!extrameta
    if(present(extrameta) ) extrameta=gfile%extrameta
    if(gfile%extrameta) then
      if (present(nmetavari) ) nmetavari=gfile%nmetavari
      if (present(nmetavarr) ) nmetavarr=gfile%nmetavarr
      if (present(nmetavarl) ) nmetavarl=gfile%nmetavarl
      if (present(nmetavarc) ) nmetavarc=gfile%nmetavarc
      if (present(nmetaaryi) ) nmetaaryi=gfile%nmetaaryi
      if (present(nmetaaryr) ) nmetaaryr=gfile%nmetaaryr
      if (present(nmetaaryl) ) nmetaaryl=gfile%nmetaaryl
      if (present(nmetaaryc) ) nmetaaryc=gfile%nmetaaryc
      if ( gfile%nmetavari.gt.0 ) then
         if (present(variname).and.size(variname).eq.gfile%nmetavari) &
             variname=gfile%variname
         if (present(varival).and.size(varival).eq.gfile%nmetavari) &
             varival=gfile%varival
      endif
      if ( gfile%nmetavarr.gt.0 ) then
         if (present(varrname).and.size(varrname).eq.gfile%nmetavarr) &
             varrname=gfile%varrname
         if (present(varrval).and.size(varrval).eq.gfile%nmetavarr) &
             varrval=gfile%varrval
      endif
      if ( gfile%nmetavarl.gt.0 ) then
         if (present(varlname).and.size(varlname).eq.gfile%nmetavarl) &
             varlname=gfile%varlname
         if (present(varlval).and.size(varlval).eq.gfile%nmetavarl) &
             varlval=gfile%varlval
      endif
      if ( gfile%nmetavarc.gt.0 ) then
         if (present(varcname).and.size(varcname).eq.gfile%nmetavarc) &
             varcname=gfile%varcname
         if (present(varcval).and.size(varcval).eq.gfile%nmetavarc) &
             varcval=gfile%varcval
      endif
      if ( gfile%nmetaaryi.gt.0 ) then
         if (present(aryiname).and.size(aryiname).eq.gfile%nmetaaryi) &
             aryiname=gfile%aryiname
         if (present(aryilen).and.size(aryilen).eq.gfile%nmetaaryi) &
             aryilen=gfile%aryilen
         if (present(aryival).and.size(aryival).eq.gfile%nmetaaryi*maxval(gfile%aryilen) ) &
             aryival=gfile%aryival
      endif
      if ( gfile%nmetaaryr.gt.0 ) then
         if (present(aryrname).and.size(aryrname).eq.gfile%nmetaaryr) &
             aryrname=gfile%aryrname
         if (present(aryrlen).and.size(aryrlen).eq.gfile%nmetaaryr) &
             aryrlen=gfile%aryrlen
         if (present(aryrval).and.size(aryrval).eq.gfile%nmetaaryr*maxval(gfile%aryrlen) ) &
             aryrval=gfile%aryrval
      endif
      if ( gfile%nmetaaryl.gt.0 ) then
         if (present(arylname).and.size(arylname).eq.gfile%nmetaaryl) &
             arylname=gfile%arylname
         if (present(aryllen).and.size(aryllen).eq.gfile%nmetaaryl) &
             aryllen=gfile%aryllen
         if (present(arylval).and.size(arylval).eq.gfile%nmetaaryl*maxval(gfile%aryllen) ) &
             arylval=gfile%arylval
      endif
      if ( gfile%nmetaaryc.gt.0 ) then
         if (present(arycname).and.size(arycname).eq.gfile%nmetaaryc) &
             arycname=gfile%arycname
         if (present(aryclen).and.size(aryclen).eq.gfile%nmetaaryc) &
             aryclen=gfile%aryclen
         if (present(arycval).and.size(arycval).eq.gfile%nmetaaryc*maxval(gfile%aryclen) ) &
             arycval=gfile%arycval
      endif
    endif

!    print *,'after getfilehead'
    if ( present(iret)) iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_getfilehead
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvari(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    integer(nemsio_intkind),intent(out)           :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarinum
      if(equal_str_nocase(trim(varname),trim(gfile%headvariname(i))) ) then
           varval=gfile%headvarival(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavari.gt.0) then
      do i=1,gfile%nmetavari
        if(equal_str_nocase(trim(varname),trim(gfile%variname(i))) ) then
           varval=gfile%varival(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvari
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarr(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                 :: varname
    real(nemsio_realkind),intent(out)             :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarrname(i))) ) then
           varval=gfile%headvarrval(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavarr.gt.0) then
      do i=1,gfile%nmetavarr
        if(equal_str_nocase(trim(varname),trim(gfile%varrname(i))) ) then
           varval=gfile%varrval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif

    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarr
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarl(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetavarl.gt.0) then
      do i=1,gfile%nmetavarl
        if(equal_str_nocase(trim(varname),trim(gfile%varlname(i))) ) then
           varval=gfile%varlval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarl
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadvarc(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    character(*),intent(out)                      :: varval
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headvarcnum
      if(equal_str_nocase(trim(varname),trim(gfile%headvarcname(i))) ) then
           varval=gfile%headvarcval(i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetavarc.gt.0) then
      do i=1,gfile%nmetavarc
        if(equal_str_nocase(trim(varname),trim(gfile%varcname(i))) ) then
           varval=gfile%varcval(i)
           if(present(iret) ) iret=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadvarc
!------------------------------------------------------------------------------
  subroutine nemsio_getfheadaryi(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    integer(nemsio_intkind),intent(out)           :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ierr
!---
    if(present(iret) ) iret=-17
    do i=1,gfile%headaryinum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryiname(i))) ) then
           varval(:)=gfile%headaryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=0
           return
      endif
    enddo
!---
    if(gfile%nmetaaryi.gt.0) then
      do i=1,gfile%nmetaaryi
        if(equal_str_nocase(trim(varname),trim(gfile%aryiname(i))) ) then
           varval(:)=gfile%aryival(1:gfile%aryilen(i),i)
           if(present(iret) ) iret=0
           ierr=0
           return
        endif
      enddo
    endif
!---    
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryi
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryr(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    real(nemsio_realkind),intent(out)             :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%headaryrnum>0) then
     do i=1,gfile%headaryrnum
      if(equal_str_nocase(trim(varname),trim(gfile%headaryrname(i))) ) then
           varval(:)=gfile%headaryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=0
           return
      endif
     enddo
    endif
!---
    if(gfile%nmetaaryr.gt.0) then
      do i=1,gfile%nmetaaryr
        if(equal_str_nocase(trim(varname),trim(gfile%aryrname(i)))) then
           varval(:)=gfile%aryrval(1:gfile%aryrlen(i),i)
           if(present(iret) ) iret=0
           ierr=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryr
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryl(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),  intent(in)                     :: varname
    logical(nemsio_logickind),intent(out)         :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%nmetaaryl.gt.0) then
      do i=1,gfile%nmetaaryl
        if(equal_str_nocase(trim(varname),trim(gfile%arylname(i)))) then
           varval(:)=gfile%arylval(1:gfile%aryllen(i),i)
           if(present(iret) ) iret=0
           ierr=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryl
!------------------------------------------------------------------------------
   subroutine nemsio_getfheadaryc(gfile,varname,varval,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: get meta data var value from file header
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(len=*),  intent(in)                     :: varname
    character(*),intent(out)                      :: varval(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i,j,ierr
!---
    if(present(iret) ) iret=-17
    if(gfile%headarycnum>0) then
     do i=1,gfile%headarycnum
      if(equal_str_nocase(trim(varname),trim(gfile%headarycname(i))) ) then
           varval(:)=gfile%headarycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=0
           return
      endif
     enddo
    endif
!---
    if(gfile%nmetaaryc.gt.0) then
      do i=1,gfile%nmetaaryc
        if(equal_str_nocase(trim(varname),trim(gfile%arycname(i)))) then
           varval(:)=gfile%arycval(1:gfile%aryclen(i),i)
           if(present(iret) ) iret=0
           ierr=0
           return
        endif
      enddo
    endif
!---
    if(.not.present(iret) ) call nemsio_stop
    return
  end subroutine nemsio_getfheadaryc
!------------------------------------------------------------------------------
  subroutine nemsio_readrec4(gfile,jrec,data,gdatatype,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    character(*), optional, intent(in)           :: gdatatype
    integer(nemsio_intkind),optional,intent(in)  :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    integer :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
!---     
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 
!---
   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then 
      call nemsio_readrecbin4d4(gfile,jrec,datatmp,iret)
      if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
      call nemsio_readrecbin8d4(gfile,jrec,datatmp,iret)
      if ( iret .ne.0 ) return
   else
     call nemsio_readrecgrb4(gfile,jrec,datatmp,iret)
     if ( iret .ne.0 ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-1)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!---
   iret=0
   return
  end subroutine nemsio_readrec4
!------------------------------------------------------------------------------
  subroutine nemsio_readrec8(gfile,jrec,data,gdatatype,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    character(*), optional, intent(in)           :: gdatatype
    integer(nemsio_intkind),optional,intent(in)  :: nframe
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize))
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecbin4d8(gfile,jrec,datatmp,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecbin8d8(gfile,jrec,datatmp,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_readrecgrb8(gfile,jrec,datatmp,iret)
     if ( iret .ne.0 ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-1)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)

!
   iret=0
   return
  end subroutine nemsio_readrec8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv4(gfile,name,levtyp,lev,data,gdatatype,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_realkind),allocatable             :: datatmp(:)
    integer :: i,j
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecvbin4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecvbin8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_readrecvgrb4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-1)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!---
   iret=0
   return
  end subroutine nemsio_readrecv4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecv8(gfile,name,levtyp,lev,data,gdatatype,nframe,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),intent(in),optional              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    character(*), optional, intent(in)            :: gdatatype
    integer(nemsio_intkind),optional,intent(in)   :: nframe
    real(nemsio_dblekind),allocatable             :: datatmp(:)
    integer :: i,j
!------------------------------------------------------------
! read 8 byte rec
!------------------------------------------------------------
   iret=-11
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   allocate(datatmp(gfile%fieldsize) )
   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_readrecvbin4(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_readrecvbin8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_readrecvgrb8(gfile,name,levtyp,lev,datatmp,iret)
     if ( iret .ne.0 ) return
   endif
!---
   if ( present(nframe) ) then
     if(nframe.le.gfile%nframe ) then
      do j=1,gfile%dimy+2*gfile%nframe-2*nframe
       do i=1,gfile%dimx+2*gfile%nframe -2*nframe
        data(i+(j-1)*(gfile%dimx+2*gfile%nframe-2*nframe))=datatmp(i+nframe        &
          +(j-1+nframe)*(gfile%dimx+2*gfile%nframe))
       enddo
      enddo
     else
       print *,"WARNING: nframe is larger than the nframe in the file!"
       call nemsio_stop
     endif
   else
     data=datatmp
   endif
   deallocate(datatmp)
!
   iret=0
   return
  end subroutine nemsio_readrecv8
!------------------------------------------------------------------------------

!*****************   read bin data set :  ********************************

!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin4d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(inout)           :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread

    iret=-11
    iskip=gfile%tlmeta+int(jrec-1,8)*int(kind(data)*gfile%fieldsize+8,8)
    iread=int(nemsio_realkind,8)*int(size(data),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=0

    return
  end subroutine nemsio_readrecbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin4d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_realkind),allocatable        :: data4(:)
    integer(nemsio_intkind8) :: iskip,iread,nread

    iret=-11
    allocate(data4(size(data)) )
    iskip=gfile%tlmeta+int(jrec-1,8)*int(kind(data4)*gfile%fieldsize+8,8)
    iread=int(nemsio_realkind,8)*int(size(data4),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data4)
    if(nread.lt.iread) return
    data=data4
    iret=0

    return
  end subroutine nemsio_readrecbin4d8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin4d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    iret=-12
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0)  return
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iread=int(kind(data),8)*int(size(data),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=0

    return
  end subroutine nemsio_readrecvbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin4d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_realkind),allocatable        :: data4(:)
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    iret=-11
    allocate(data4(size(data)) )
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iread=int(kind(data4),8)*int(size(data4),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data4)
    if(nread.lt.iread) return
    data=data4
    iret=0

    return
  end subroutine nemsio_readrecvbin4d8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin8d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable             :: data8(:)
    integer(nemsio_intkind8) :: iskip,iread,nread

    iret=-11
    allocate(data8(size(data)) )
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data8)
    if(nread.lt.iread) return
    data=data8
    iret=0

    return
  end subroutine nemsio_readrecbin8d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecbin8d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread

    iret=-11
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=0

    return
  end subroutine nemsio_readrecbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin8d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    iret=-11
    allocate(data8(size(data)) )
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
!    print *,'name=',name,'levtyp=',levtyp,'lev=',lev,'jrec=',jrec
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data8)
    if(nread.lt.iread) return
    data=data8
    iret=0

    return
  end subroutine nemsio_readrecvbin8d4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvbin8d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iread,nread
    integer :: jrec, ierr

    iret=-11
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iread=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrreadl(gfile%flunit,iskip,iread,nread,data)
    if(nread.lt.iread) return
    iret=0

    return
  end subroutine nemsio_readrecvbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_searchrecv(gfile,jrec,name,levtyp,lev,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: search rec number giving rec name, levtyp and lev
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(out)           :: jrec
    character(*),intent(in)                      :: name
    character(*),intent(in),optional             :: levtyp
    integer(nemsio_intkind),optional,intent(in)  :: lev
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer i, nsize,nlen,nlen1

    iret=-11
    nlen=min(len(name),len(gfile%recname))
    nlen1=min(len(levtyp),len(gfile%reclevtyp))
!
    jrec=0
!    print *,'in search rec, recname length=',gfile%recname,'nrec=',gfile%nrec
    if(size(gfile%recname)/=gfile%nrec) return
    if(.not.present(levtyp)) then
!      print *,'in search rec, name=',lowercase(name)(1:nlen)
      do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) ) then
           jrec=i
           exit
        endif
      enddo
    else if (size(gfile%reclevtyp).eq.gfile%nrec) then
      if(.not.present(lev)) then
       do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) .and. &
           equal_str_nocase(trim(levtyp),trim(gfile%reclevtyp(i))) ) then
           jrec=i
           exit
        endif
       enddo
      else if(size(gfile%reclev).eq.gfile%nrec) then
       do i=1,gfile%nrec
        if ( equal_str_nocase(trim(name),trim(gfile%recname(i))) .and. &
           equal_str_nocase(trim(levtyp),trim(gfile%reclevtyp(i)) ) .and. &
           lev==gfile%reclev(i) ) then
           jrec=i
           exit
        endif
       enddo
      endif
    endif
    if ( jrec .ne.0 ) iret=0
!
    return
  end subroutine nemsio_searchrecv
!------------------------------------------------------------------------------
!
!*****************   read grb data set :  *************************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_readrecw34(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    allocate(lbms(grbmeta%jf))
    N=0
!------------------------------------------------------------
! get data from getgb
!------------------------------------------------------------
    print *,'in nemsio, before getgbm,mbuf=',gfile%mbuf,&
     'nlen=',gfile%nlen,'nnum=',gfile%nnum,'mnum=',gfile%mnum, &
     'jf=',grbmeta%jf,'jpds=',grbmeta%jpds(1:20),'jgds=', &
     grbmeta%jgds(1:20) 
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecw34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecgrb4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    allocate(data8(size(data)) )
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    N=0
    print *,'getrecgrb4,in nemsio, before getgbm,mbuf=',gfile%mbuf,&
     'nlen=',gfile%nlen,'nnum=',gfile%nnum,'mnum=',gfile%mnum, &
     'jf=',grbmeta%jf,'jpds=',grbmeta%jpds(1:20),'jgds=', &
     grbmeta%jgds(1:20) 
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecgrb8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 64 bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
         print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if (present(iret)) iret=0
  end subroutine nemsio_readrecgrb8
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvw34(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into 32 bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i,w34
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    w34=1
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev ,w34=w34)
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w34
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvw34
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb4(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                      :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_realkind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    allocate(data8(size(data)) )
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    data8=data
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data8,ios)
    data=data8
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_readrecvgrb8(gfile,vname,vlevtyp,vlev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character*(*),intent(in)                     :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)            :: vlev
    real(nemsio_dblekind),intent(out)             :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: luidx
    integer(nemsio_intkind)      :: kf,k,kpds(200),kgds(200)
    logical*1,allocatable       :: lbms(:)
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: ios,i
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    luidx=0
    if ( present(iret)) iret=-4
    call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
      vlevtyp=vlevtyp, vlev=vlev )
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!------------------------------------------------------------
! get data from getgb _w3d
!------------------------------------------------------------
    allocate(lbms(grbmeta%jf))
    N=0
    call getgbm(gfile%flunit,luidx,grbmeta%jf,N,grbmeta%jpds,grbmeta%jgds,&
      gfile%mbuf,gfile%cbuf,gfile%nlen,gfile%nnum,gfile%mnum, &
      kf,k,kpds,kgds,lbms,data,ios)
    deallocate(lbms,grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'getgb_ios=',ios
         return
       else
         call nemsio_stop
       endif
    endif
    if ( present(iret)) iret=0
  end subroutine nemsio_readrecvgrb8
!------------------------------------------------------------------------------

!*****************   write data set :  ********************************

!------------------------------------------------------------------------------
  subroutine nemsio_writerec4(gfile,jrec,data,gdatatype,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    character(*), optional, intent(in)           :: gdatatype
!
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d4(gfile,jrec,data,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d4(gfile,jrec,data,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_writerecgrb4(gfile,jrec,data,iret,itr=itr,zhour=zhour)
     if ( iret .ne.0 ) return
   endif
   iret=0
!
   return
  end subroutine nemsio_writerec4
!------------------------------------------------------------------------------
  subroutine nemsio_writerec8(gfile,jrec,data,gdatatype,iret,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 64 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    character(*), optional, intent(in)           :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecbin4d8(gfile,jrec,data,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecbin8d8(gfile,jrec,data,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_writerecgrb8(gfile,jrec,data,iret,itr=itr,zhour=zhour)
     if ( iret .ne.0 ) return
   endif
   iret=0
!
   return
  end subroutine nemsio_writerec8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv4(gfile,name,levtyp,lev,data,gtype,gdatatype,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    character(*), optional, intent(in)           :: gtype
    character(*), optional, intent(in)           :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if (present(gtype) .and. gfile%gtype.ne.trim(gtype) ) then
     print *,'ERROR: the NEMSIO model type is ',gfile%gtype, 'input is',gtype
     call nemsio_stop
   endif
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
!     print *,'call nemsio_writerecvbin4d4'
     call nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.0 ) return
   else
!     print *,'call nemsio_writerecvgrb4'
     call nemsio_writerecvgrb4(gfile,name,levtyp,lev,data,iret,itr=itr,        &
          zhour=zhour)
     if ( iret .ne.0 ) return
   endif
   iret=0
!
   return
  end subroutine nemsio_writerecv4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecv8(gfile,name,levtyp,lev,data,gtype,gdatatype,iret, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: write nemsio a 2D 32 bits array data into bin file using record number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind),optional,intent(in)   :: itr 
    real(nemsio_realkind),optional,intent(in)     :: zhour
    character(*), optional, intent(in)           :: gtype
    character(*), optional, intent(in)           :: gdatatype
!------------------------------------------------------------
! read 4 byte rec
!------------------------------------------------------------
   if (present(gtype) .and. gfile%gtype.ne.trim(gtype) ) then
     print *,'ERROR: the NEMSIO model type is ',gfile%gtype, 'input is',gtype
     call nemsio_stop
   endif
   if ( present(gdatatype) ) then
      if (trim(gdatatype).ne.trim(gfile%gdatatype) ) then
      print *,'WRONG: data type not consistant in fileheader and read request' 
      call nemsio_stop
     endif
   endif 

   if ( gfile%gdatatype .eq. 'bin4') then
     call nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.0 ) return
   else if ( gfile%gdatatype .eq. 'bin8') then
     call nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,iret)
     if ( iret .ne.0 ) return
   else
     call nemsio_writerecvgrb8(gfile,name,levtyp,lev,data,iret,itr=itr,    &
          zhour=zhour)
     if ( iret .ne.0 ) return
   endif
   iret=0
!
   return
  end subroutine nemsio_writerecv8
!------------------------------------------------------------------------------

!*****************   write out bin data set :  ********************************

!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin4d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

!
    real(8) timef,stime
!
    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
     write(0,'(a15,I13.0)')'writerec iskip=',iskip 
     print *,'in nemsio_writerecbin4d4,iwrite=',iwrite,'nwrite=',nwrite
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin4d4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin4d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_realkind),allocatable             :: data4(:)
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data4(size(data)) )
    data4=data
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data4),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin4d8
!------------------------------------------------------------------------------
 subroutine nemsio_writerecvbin4d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin4d4
!------------------------------------------------------------------------------
 subroutine nemsio_writerecvbin4d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_realkind),allocatable        :: data4(:)
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    allocate(data4(size(data)) )
    data4=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_realkind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_realkind,8)*int(size(data4),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data4)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin4d8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin8d4(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin8d4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecbin8d8(gfile,jrec,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    integer(nemsio_intkind),intent(in)            :: jrec
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecbin8d8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvbin8d4(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_realkind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    real(nemsio_dblekind),allocatable        :: data8(:)
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    allocate(data8(size(data)) )
    data8=data
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data8),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data8)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin8d4

!------------------------------------------------------------------------------
  subroutine nemsio_writerecvbin8d8(gfile,name,levtyp,lev,data,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data (bin) by record number into a 2D 32 bits array
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                 :: gfile
    character(*),intent(in)                       :: name
    character(*),optional,intent(in)              :: levtyp
    integer(nemsio_intkind),optional,intent(in)   :: lev
    real(nemsio_dblekind),intent(in)              :: data(:)
    integer(nemsio_intkind),optional,intent(out)  :: iret
    integer :: jrec, ierr
    integer(nemsio_intkind8) :: iskip,iwrite,nwrite

    iret=-11
    if(size(data)/=gfile%fieldsize) then
      print *,'WRONG: input data size ',size(data),' is not match the data domain ', &
        gfile%fieldsize,'please check dimension and nframe'
      return
    endif
    call nemsio_searchrecv(gfile,jrec,name,levtyp,lev,ierr)
    if ( ierr .ne. 0) return
    iskip=gfile%tlmeta+int(jrec-1,8)*int(nemsio_dblekind*gfile%fieldsize+8,8)
    iwrite=int(nemsio_dblekind,8)*int(size(data),8)
    call bafrwritel(gfile%flunit,iskip,iwrite,nwrite,data)
    if(nwrite.lt.iwrite) return
    iret=0

    return
  end subroutine nemsio_writerecvbin8d8
!------------------------------------------------------------------------------
!
!*****************   write out grb data set :  ********************************
!
!------------------------------------------------------------------------------
  subroutine nemsio_writerecw34(gfile,jrec,data,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,w34,ibms
!---
    real(nemsio_realkind)      :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
    w34=1
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb)) ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           idrt=idrt,itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,w34=w34, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
       if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecw34
!------------------------------------------------------------------------------
  subroutine nemsio_writerecgrb4(gfile,jrec,data,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i,nc1
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_intkind)         :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta ibms
!------------------------------------------------------------
    ibms=0
!
    allocate(data8(size(data)) )
    data8=data
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
!------------------------------------------------------------
! set up grib meta data
!------------------------------------------------------------
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
!------------------------------------------------------------
! set up lbms 
!------------------------------------------------------------
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!    if(jrec==6.and.ibms==1.and.gfile%nfhour==3)write(85,'(10f12.4)')data8
!     write(0,*)'in writerecgrb4,max=',mymax,'nc=',nc,'nc1=',nc1,'imb=',ibms, &
!      'size(data)=',size(data),'size(lbms)=',size(grbmeta%lbms), &
!      grbmeta%lbms(1:15),data8(1:15)
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
!    allocate(data8(size(data)) )
!    data8=data
    write(0,*)'in writerecgrb4,before putgb=',grbmeta%lbms(1:15)
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecgrb8(gfile,jrec,data8,iret,idrt,itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by record number into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    integer(nemsio_intkind),intent(in)          :: jrec
    real(nemsio_dblekind),intent(in)            :: data8(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
!---
    real(nemsio_realkind)      :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec,idrt=idrt, &
           itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,jrec=jrec, &
           itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecgrb8
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvw34(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_4 library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                    :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,w34,ibms
    real(nemsio_realkind)        :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb))  ibms=1
!
    w34=1
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34, idrt=idrt,  &
        itr=itr,zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, w34=w34,itr=itr,     &
        zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
        if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w34
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvw34
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvgrb4(gfile,vname,vlevtyp,vlev,data,iret,idrt, &
             itr,zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 32bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_realkind),intent(in)            :: data(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    real(nemsio_dblekind),allocatable        :: data8(:)
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_realkind)        :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data)
    do i=1,gfile%fieldsize
     if(abs(data(i))<nemsio_undef_grb) then
       if(data(i) .gt.mymax) mymax=data(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    allocate(data8(size(data)) )
    daTa8=data
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvgrb4
!------------------------------------------------------------------------------
  subroutine nemsio_writerecvgrb8(gfile,vname,vlevtyp,vlev,data8,iret,idrt,itr, &
       zhour)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: read nemsio data by field name into a 2D 64bits array, 
!           using w3_d library to compile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)               :: gfile
    character*(*),intent(in)                   :: vname,vlevtyp
    integer(nemsio_intkind),intent(in)          :: vlev
    real(nemsio_dblekind),intent(in)            :: data8(:)
    integer(nemsio_intkind),optional,intent(out):: iret
    integer(nemsio_intkind),optional,intent(in) :: idrt
    integer(nemsio_intkind),optional,intent(in) :: itr
    real(nemsio_realkind),optional,intent(in)   :: zhour
    type(nemsio_grbmeta)         :: grbmeta
    integer(nemsio_intkind)      :: N=nemsio_kpds_intfill
    integer(nemsio_intkind)      :: nc,i
    integer(nemsio_intkind)      :: ios,ibms
    real(nemsio_realkind)        :: mymax
!------------------------------------------------------------
! set up grib meta 
!------------------------------------------------------------
    if(present(iret)) iret=-4
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    ibms=0
    if(any(abs(data8)>=nemsio_undef_grb))  ibms=1
!
    if(present(idrt)) then
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev, idrt=idrt,itr=itr,   &
        zhour=zhour,ibms=ibms)
    else
      call nemsio_setrqst(gfile,grbmeta,ios,vname=vname, &
        vlevtyp=vlevtyp, vlev=vlev,itr=itr,zhour=zhour,ibms=ibms)
    endif
    if (ios.ne.0) then
       if ( present(iret))  then
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
!
    grbmeta%lbms=.true.
    where(abs(data8)>=nemsio_undef_grb) grbmeta%lbms=.false.
    mymax=minval(data8)
    do i=1,gfile%fieldsize
     if(abs(data8(i))<nemsio_undef_grb) then
        if(data8(i) .gt.mymax) mymax=data8(i)
     endif
    enddo
!
!------------------------------------------------------------
! check precision -- for pressure now
!------------------------------------------------------------
    if ( grbmeta%jpds(5).eq.1 .and. grbmeta%jpds(6).eq.109 ) then
     grbmeta%jpds(22)=min(int(5-log10(mymax)),2)
    endif
!------------------------------------------------------------
! get data from putgb _w3d
!------------------------------------------------------------
    call putgb(gfile%flunit,grbmeta%jf,grbmeta%jpds,grbmeta%jgds, &
      grbmeta%lbms,data8,ios)
    deallocate(grbmeta%lbms)
    if(ios.ne.0) then
       if ( present(iret))  then
          print *,'putgb_ios=',ios
         iret=ios
         return
       else
         call nemsio_stop
       endif
    endif
    if(present(iret)) iret=0
  end subroutine nemsio_writerecvgrb8
!----------------------------------------------------------------------------
  subroutine nemsio_setrqst(gfile,grbmeta,iret,jrec,vname,vlevtyp,vlev,w34,idrt, &
                            itr,zhour,ibms)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: if given record number, find record name, lev typ, and levs or
!           record name,lev type and lev can be got from argument list.
!           with record name,lev typ and level, set up grib meta, jpds and
!           jgds
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    type(nemsio_grbmeta),intent(out)             :: grbmeta
    integer(nemsio_intkind),optional,intent(in)  :: jrec
    character(*),optional,intent(in)             :: vname,vlevtyp
    integer(nemsio_intkind),optional,intent(in)  :: vlev
    integer(nemsio_intkind),intent(out)          :: iret
    integer(nemsio_intkind),optional,intent(in)  :: w34
    integer(nemsio_intkind),optional,intent(in)  :: idrt
    integer(nemsio_intkind),optional,intent(in)  :: itr
    real(nemsio_realkind),optional,intent(in)    :: zhour
    integer(nemsio_intkind),optional,intent(in)  :: ibms
    character(255) :: name,levtyp
    integer :: icen,igrid,iptv,itl,jbms,jftu,jp1,jp2,jtr,jna,jnm,ios
    integer :: i,lev,ktbl,krec,idrt_in
!------------------------------------------------------------
! with record number, find record name, level type and level
!------------------------------------------------------------
    iret=-5
    if ( present(jrec)) then
      if ( jrec.gt.0 .and. jrec.le.gfile%nrec) then
        name=gfile%recname(jrec)
        levtyp=gfile%reclevtyp(jrec)
        lev=gfile%reclev(jrec)
      else
        return
      endif
    elseif ( present(vname) .and. present(vlevtyp) .and. present(vlev)) then
      name=trim(vname)
      levtyp=trim(vlevtyp)
      lev=vlev
    else 
       return
    endif
!    write(0,*)'in searchrst,jrec=',jrec,'name=',trim(name),'levtyp=',levtyp,&
!      'lev=',lev
!------------------------------------------------------------
! find index in grib table according to recname and reclevtyp
!------------------------------------------------------------
    call nemsio_grbtbl_search(trim(name),trim(levtyp),ktbl,krec,ios)
    if(ios.ne.0) return
!*** lev: for special layer
!    if ( gribtable(ktbl)%item(krec)%leveltype .eq.'sfc' ) then
    if ( gribtable(ktbl)%item(krec)%leveltype .ne.'layer' .or. &
         gribtable(ktbl)%item(krec)%leveltype .ne.'mid layer' ) then
        lev=0
    endif
!------------------------------------------------------------
! for read, just need to set up jpds(05-07)
!------------------------------------------------------------
!--- read:set jpds5,6,7
!    if ( lowercase(gfile%gaction)(1:4).eq."read") then
    if ( equal_str_nocase(trim(gfile%gaction),"read") ) then
      grbmeta%jpds(05)=gribtable(ktbl)%item(krec)%g1param
      grbmeta%jpds(06)=gribtable(ktbl)%item(krec)%g1level
      grbmeta%jpds(07)=lev
      if ( grbmeta%jpds(06).eq.110 ) then
        grbmeta%jpds(07)=256*(lev-1)+lev
      endif
      if (gribtable(ktbl)%item(krec)%g1lev.ne.0) then
        grbmeta%jpds(07)=gribtable(ktbl)%item(krec)%g1lev
      endif
    else
!------------------------------------------------------------
! for write, need to set up jgds(1:25), jpds(01-20)
!------------------------------------------------------------
      if (present(idrt)) then
        idrt_in = idrt
      else
!*** gfile idrt
        idrt_in=gfile%idrt
      endif
!*** for itr
      jftu=1
      jtr=10
      jp1=gfile%nfhour
      jp2=0
      if(present(itr) ) then
        jtr=itr
        if(itr==3.or.itr==2.or.itr==4) then   !avg
           if(present(zhour)) then
             jp1=nint(zhour)
             jp2=gfile%nfhour
           else
             print *,'ERROR in nemsio gribfile,itr=',itr,'need to set zhour'
           endif
        endif
      endif 
      jbms=0
      if(present(ibms)) jbms=ibms
!
      icen=7
!
      if ( present(w34) ) then
        call nemsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios,w34)
      else
        call nemsio_makglgds(gfile,idrt_in,igrid,grbmeta%jgds,ios)
        write(0,*)'after nemsio_makglgds,idrt=',idrt_in,'ios=',ios,'igrid=',igrid, &
          'jbms=',jbms
      endif
      if(ios.ne.0) return
      iptv=gribtable(ktbl)%iptv
!      itl=1
      jna=0
      jnm=0
        write(0,*)'before nemsio_makglpds,lev=',lev
      call nemsio_makglpds(gfile,iptv,icen,igrid,jbms,&
           jftu,jp1,jp2,jtr,jna,jnm,jrec,ktbl,krec,lev,grbmeta%jpds,ios)
        write(0,*)'after nemsio_makglpds,jpds=',grbmeta%jpds(1:25),'ios=',ios,  &
           'lev=',lev
      if(ios.ne.0) return
    endif
!------------------------------------------------------------
! set up grib meta lbms
!------------------------------------------------------------
    grbmeta%jf=gfile%fieldsize
    allocate(grbmeta%lbms(grbmeta%jf))
    iret=0 
  end subroutine nemsio_setrqst    
!------------------------------------------------------------------------------
  subroutine nemsio_getrechead(gfile,jrec,name,levtyp,lev,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: given record number, return users record name, lev typ, and levs
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)                :: gfile
    integer(nemsio_intkind),intent(in)           :: jrec
    character(*),intent(inout)                   :: name
    character(*),optional,intent(inout)          :: levtyp
    integer(nemsio_intkind),optional,intent(out) :: lev
    integer(nemsio_intkind),optional,intent(out) :: iret
    integer :: ios
! - - - - - - - - - - - - - -  - - - - - - - -  - - - - - - - - - - - - - - - -
    if( present(iret)) iret=-5
    if ( jrec.gt.0 .or. jrec.le.gfile%nrec) then
      if(gfile%nmeta>2) then
        name=gfile%recname(jrec)
      else
        print *,'WRONG: recname is not specified in meta data!'
        return
      endif
      if(present(levtyp).and.gfile%nmeta>3) then
        levtyp=gfile%reclevtyp(jrec)
      endif
      if(present(lev).and.gfile%nmeta>4) then
        lev=gfile%reclev(jrec)
      endif
      if(present(iret)) iret=0
!      print *,'in getrechead, nrec=',gfile%nrec,'name=',name,'levtyp=',levtyp,'lev=',lev
      return
    else
      if ( present(iret))  then
       print *,'WRONG: jrec is either less than 1 or greater than gfile%nrec'
       return
      else
        call nemsio_stop
      endif
    endif
  end subroutine nemsio_getrechead
!------------------------------------------------------------------------------
  subroutine nemsio_makglgds(gfile,idrt,igrid,kgds,iret,w34)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up gds for grib meta
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in) :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer,intent(in):: idrt
    integer,optional,intent(in):: w34
    integer,intent(out):: igrid,kgds(200)
    real(nemsio_dblekind) :: slat8(gfile%dimy),wlat8(gfile%dimy)
    real(nemsio_intkind) :: slat4(gfile%dimy),wlat4(gfile%dimy)
    integer :: n
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    igrid=255
    if(idrt.eq.0.and.gfile%dimx.eq.144.and.gfile%dimy.eq.73) igrid=2
    if(idrt.eq.0.and.gfile%dimx.eq.360.and.gfile%dimy.eq.181) igrid=3
    if(idrt.eq.0.and.gfile%dimx.eq.720.and.gfile%dimy.eq.361) igrid=4
    if(idrt.eq.4.and.gfile%dimx.eq.192.and.gfile%dimy.eq.94) igrid=98
    if(idrt.eq.4.and.gfile%dimx.eq.384.and.gfile%dimy.eq.192) igrid=126
    if(idrt.eq.4.and.gfile%dimx.eq.512.and.gfile%dimy.eq.256) igrid=170
    if(idrt.eq.4.and.gfile%dimx.eq.768.and.gfile%dimy.eq.384) igrid=127
    write(0,*)'in nemsio_makdglgds,idrt=',idrt,'dimx=',gfile%dimx,'dimy=',gfile%dimy
    kgds(1)=modulo(idrt,256)
    kgds(2)=gfile%dimx
    kgds(3)=gfile%dimy
    select case(idrt)
    case(0)
      kgds(4)=90000
    case(4)
!------------------------------------------------------------
! call different split for w3_4 lib and w3_d lib
!------------------------------------------------------------
      if (present (w34)) then
        call splat(idrt,gfile%dimy,slat4,wlat4)
        kgds(4)=nint(180000./acos(-1.)*asin(slat4(1)))
      else
        call splat(idrt,gfile%dimy,slat8,wlat8)
        kgds(4)=nint(180000./acos(-1.)*asin(slat8(1)))
      endif
    case(256)
      kgds(4)=90000-nint(0.5*180000./gfile%dimy)
    end select
    kgds(5)=0
    kgds(6)=128
    kgds(7)=-kgds(4)
    kgds(8)=-nint(360000./gfile%dimx)
    kgds(9)=-kgds(8)
    select case(idrt)
    case(0)
      kgds(10)=nint(180000./(gfile%dimy-1))
    case(4)
      kgds(10)=gfile%dimy/2
    case(256)
      kgds(10)=nint(180000./gfile%dimy)
    end select
    kgds(11)=0
    kgds(12)=0
    kgds(13:18)=-1
    kgds(19)=0
    kgds(20)=255
    kgds(21:)=-1
    write(0,*)'in nemsio_makdglgds,kgds=',kgds(1:20)
    iret=0
  end subroutine nemsio_makglgds
!------------------------------------------------------------------------------
  subroutine nemsio_makglpds(gfile,iptv,icen,igrid,ibms,&
                    iftu,ip1,ip2,itr,ina,inm,jrec,ktbl,krec,lev,kpds,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up gps for grib meta
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(in)  :: gfile
    integer,intent(in):: iptv,icen,igrid,ibms
    integer,intent(in):: iftu,ip1,ip2,itr,ina,inm,jrec,ktbl,krec,lev
   integer,intent(out):: kpds(200)
   integer(nemsio_intkind),intent(out)  :: iret
   integer :: i,igen,icen2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
!
!get igen icen2 
    call nemsio_getheadvar(gfile,'igen',igen,iret)
    if (iret.ne.0 ) then
      if(trim(gfile%modelname)=='GFS') igen=82
    else
      print *,'ERROR: please specify model generating flag'
      return
    endif
    call nemsio_getheadvar(gfile,'icen2',icen2,iret)
    if (iret.ne.0 ) then
      if(trim(gfile%modelname).eq.'GFS') then
        icen2=0
      else
        print *,'ERROR: please specify subcenter id,modelname=',gfile%modelname
        return
      endif
    endif
!
    kpds(01)=icen
    kpds(02)=igen
    kpds(03)=igrid
    kpds(04)=128+64*ibms
    kpds(05)=gribtable(ktbl)%item(krec)%g1param
    kpds(06)=gribtable(ktbl)%item(krec)%g1level
    kpds(07)=lev
    if(gribtable(ktbl)%item(krec)%g1lev/=0)then
      kpds(07)=gribtable(ktbl)%item(krec)%g1lev
    endif
!*** deal with dpres 
    if ( kpds(06).eq.110 ) then
    kpds(07)=256*(lev-1)+lev
    endif
!***
    kpds(08)=mod(gfile%idate(1)-1,100)+1
    kpds(09)=gfile%idate(2)
    kpds(10)=gfile%idate(3)
    kpds(11)=gfile%idate(4)
    kpds(12)=0
    kpds(13)=iftu
    kpds(14)=ip1
    kpds(15)=ip2
    kpds(16)=itr
    kpds(17)=ina
    kpds(18)=1
    kpds(19)=iptv
    kpds(20)=inm
    kpds(21)=(gfile%idate(1)-1)/100+1
    kpds(22)=gribtable(ktbl)%item(krec)%precision
    kpds(23)=icen2
    kpds(24)=0
    kpds(25)=0
    kpds(26:)=-1
    write(0,*)'in nemsio_makdglgds,kpds=',kpds(1:20),'lev=',lev
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_makglpds
!------------------------------------------------------------------------------
  subroutine nemsio_grbtbl_search(vname,vlevtyp,ktbl,krec,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: given record name, levtyp and index number in grib table
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    character(*),intent(in)   :: vname,vlevtyp
    integer(nemsio_intkind),intent(out)   :: ktbl,krec
    integer(nemsio_intkind),intent(out)   :: iret
    integer  :: i,j,nlen,nlen1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-5
    nlen=len(trim(vname))
    nlen1=len(trim(vlevtyp))
    ktbl=0
    krec=0
!    write(0,*)'vname=',vname,'vlevtyp=',vlevtyp,'nlen=',nlen,'nlen1=',nlen1
    do j=1,size(gribtable)
    do i=1,size(gribtable(j)%item)
      if(equal_str_nocase(trim(vname),trim(gribtable(j)%item(i)%shortname)) .and. &
        equal_str_nocase(trim(vlevtyp),trim(gribtable(j)%item(i)%leveltype)) )then
        ktbl=j
        krec=i
        iret=0
        exit
      endif
    enddo 
    enddo 
!    write(0,*)'in grbtbl_search,krec=',krec,'ktbl=',ktbl
  end subroutine nemsio_grbtbl_search
!------------------------------------------------------------------------------
  subroutine nemsio_chkgfary(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: check if arrays in gfile is allocated and with right size
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)         :: gfile
    integer(nemsio_intkind),intent(out)   :: iret
    integer   :: ios
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-2
    if ( gfile%dimx .eq. nemsio_intfill .or. gfile%dimy .eq. nemsio_intfill &
        .or. gfile%dimz .eq. nemsio_intfill .or. gfile%nrec .eq. nemsio_intfill ) then
        print *,'WRONG: dimx,dimy,dimz and nrec  must be defined!'
        return
    endif
    if(gfile%nmeta>5) then
      if (.not. allocated(gfile%vcoord) .or. size(gfile%vcoord).ne. &
       (gfile%dimz+1)*3*2 ) then
       call nemsio_almeta1(gfile,ios)
       if (ios .ne. 0) return
      endif
    endif
    if(gfile%nmeta>6) then
    if (.not.allocated(gfile%lat) .or. size(gfile%lat).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%lon) .or. size(gfile%lon).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dx) .or. size(gfile%dx).ne.gfile%fieldsize .or.&
        .not.allocated(gfile%dy) .or. size(gfile%dy).ne.gfile%fieldsize) then
        call nemsio_almeta2(gfile,ios)
        if (ios .ne. 0) return
    endif
    endif
    if(gfile%nmeta>10) then
      if(gfile%ntrac==nemsio_intfill) then
        print *,'WRONG: ntrac is not defined!'
        return
      endif
      if (.not.allocated(gfile%Cpi) .or. size(gfile%Cpi).ne.gfile%ntrac+1 .or. &
        .not.allocated(gfile%Ri) .or. size(gfile%Ri).ne.gfile%ntrac+1 ) then
        call nemsio_almeta3(gfile,ios)
        if (ios .ne. 0) return
      endif
    endif

    if(gfile%nmeta>2) then
    if (allocated(gfile%recname) .and. size(gfile%recname).eq.gfile%nrec)&
    then
        if (allocated(gfile%reclevtyp) .and. size(gfile%reclevtyp) &
        .eq.gfile%nrec) then
           if (allocated(gfile%reclev) .and. size(gfile%reclev).eq. &
             gfile%nrec) then
               iret=0
               return
           endif
         endif
   endif
   call  nemsio_almeta4(gfile,ios)
   if (ios .ne. 0) return
   endif
   iret=0
  end subroutine nemsio_chkgfary
!------------------------------------------------------------------------------
  subroutine nemsio_almeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate all the arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile 
    integer(nemsio_intkind),intent(out)  :: iret
    integer ::dimvcoord1,dimvcoord2,dimnmmlev
    integer ::dimrecname,dimreclevtyp,dimreclev
    integer ::dimfield
    integer ::dimcpr
    integer ::iret1,iret2,iret3,iret4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=0
    dimvcoord1=gfile%dimz+1
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    dimfield=gfile%fieldsize
    dimcpr=gfile%ntrac+1
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    if(gfile%nmeta>2)then
      allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), &
             stat=iret1)
      if(iret1.eq.0) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
      endif
      iret=iret+abs(iret1)
    endif
    if(gfile%nmeta>5)then
      allocate(gfile%vcoord(dimvcoord1,3,2) ,stat=iret2) 
      if(iret3.eq.0) then
      gfile%vcoord=nemsio_realfill
      endif
      iret=iret+abs(iret2)
    endif
    if(gfile%nmeta>6)then
      allocate(gfile%lat(dimfield), gfile%lon(dimfield), &
             gfile%dx(dimfield), gfile%dy(dimfield) ,stat=iret3)
      if(iret3.eq.0) then
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
      endif
      iret=iret+abs(iret3)
    endif
    if(gfile%nmeta>10)then
      allocate(gfile%Cpi(dimcpr), gfile%Ri(dimcpr), stat=iret4)
      if(iret4.eq.0) then
      gfile%Cpi=nemsio_realfill
      gfile%Ri=nemsio_realfill
      endif
      iret=iret+abs(iret4)
    endif

!    print *,'iret1=',iret1,'iret2=',iret2,'dimx=',gfile%dimx,'dimy=',gfile%dimy,'nframe=',gfile%nframe
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta
!------------------------------------------------------------------------------
  subroutine nemsio_alextrameta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate all the arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer ::iret1,iret2,iret3,iret4
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    if(gfile%extrameta) then
!      print *,'nmetavari=',gfile%nmetavari,'nmetavarr=',gfile%nmetavarr, &
!              'nmetavarl=',gfile%nmetavarl,'nmetavarc=',gfile%nmetavarc, &
!              'nmetaaryi=',gfile%nmetaaryi,'nmetaaryr=',gfile%nmetaaryi, &
!              'nmetaaryl=',gfile%nmetaaryl,'nmetaaryc=',gfile%nmetaaryc
      if(gfile%nmetavari.gt.0) then
         if(allocated(gfile%variname)) deallocate(gfile%variname)
         if(allocated(gfile%varival)) deallocate(gfile%varival)
         allocate(gfile%variname(gfile%nmetavari), &
                  gfile%varival(gfile%nmetavari), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarr.gt.0) then
         if(allocated(gfile%varrname)) deallocate(gfile%varrname)
         if(allocated(gfile%varrval)) deallocate(gfile%varrval)
         allocate(gfile%varrname(gfile%nmetavarr), &
                  gfile%varrval(gfile%nmetavarr), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarl.gt.0) then
         if(allocated(gfile%varlname)) deallocate(gfile%varlname)
         if(allocated(gfile%varlval)) deallocate(gfile%varlval)
         allocate(gfile%varlname(gfile%nmetavarl), &
                  gfile%varlval(gfile%nmetavarl), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetavarc.gt.0) then
         if(allocated(gfile%varcname)) deallocate(gfile%varcname)
         if(allocated(gfile%varcval)) deallocate(gfile%varcval)
         allocate(gfile%varcname(gfile%nmetavarc), &
                  gfile%varcval(gfile%nmetavarc), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryi.gt.0) then
         if(allocated(gfile%aryiname)) deallocate(gfile%aryiname)
         if(allocated(gfile%aryilen)) deallocate(gfile%aryilen)
         if(allocated(gfile%aryival)) deallocate(gfile%aryival)
         allocate(gfile%aryiname(gfile%nmetaaryi), &
                  gfile%aryilen(gfile%nmetaaryi), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryr.gt.0) then
         if(allocated(gfile%aryrname)) deallocate(gfile%aryrname)
         if(allocated(gfile%aryrlen)) deallocate(gfile%aryrlen)
         if(allocated(gfile%aryrval)) deallocate(gfile%aryrval)
         allocate(gfile%aryrname(gfile%nmetaaryr), &
                  gfile%aryrlen(gfile%nmetaaryr), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryl.gt.0) then
         if(allocated(gfile%arylname)) deallocate(gfile%arylname)
         if(allocated(gfile%aryllen)) deallocate(gfile%aryllen)
         if(allocated(gfile%arylval)) deallocate(gfile%arylval)
         allocate(gfile%arylname(gfile%nmetaaryl), &
                  gfile%aryllen(gfile%nmetaaryl), stat=iret1 )
         if(iret1.ne.0) return
      endif
      if(gfile%nmetaaryc.gt.0) then
         if(allocated(gfile%arycname)) deallocate(gfile%arycname)
         if(allocated(gfile%aryclen)) deallocate(gfile%aryclen)
         if(allocated(gfile%arycval)) deallocate(gfile%arycval)
         allocate(gfile%arycname(gfile%nmetaaryc), &
                  gfile%aryclen(gfile%nmetaaryc), stat=iret1 )
         if(iret1.ne.0) return
      endif
    endif

    iret=0
!    print *,'end of alextrameta'
  end subroutine nemsio_alextrameta
!------------------------------------------------------------------------------
  subroutine nemsio_almeta1(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate vcoord in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimvcoord1,dimnmmlev,dimnmmnsoil
    integer :: dimgsilev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimvcoord1=gfile%dimz+1
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord)
    allocate(gfile%vcoord(dimvcoord1,3,2), stat=iret)
    if(iret.eq.0) then
      gfile%vcoord=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta1
!------------------------------------------------------------------------------
  subroutine nemsio_almeta2(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lat1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimlat
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dimlat=gfile%fieldsize
    if(allocated(gfile%lat)) deallocate(gfile%lat)
    if(allocated(gfile%lon)) deallocate(gfile%lon)
    if(allocated(gfile%dx)) deallocate(gfile%dx)
    if(allocated(gfile%dy)) deallocate(gfile%dy)
    allocate(gfile%lat(dimlat),gfile%lon(dimlat), &
             gfile%dx(dimlat),gfile%dy(dimlat), stat=iret)
    if(iret.eq.0) then
      gfile%lat=nemsio_realfill
      gfile%lon=nemsio_realfill
      gfile%dx=nemsio_realfill
      gfile%dy=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta2
!------------------------------------------------------------------------------
  subroutine nemsio_almeta3(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate lon1d in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dim1d
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    dim1d=gfile%ntrac+1
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri)
    allocate(gfile%Cpi(dim1d),gfile%Ri(dim1d),stat=iret)
    if(iret.eq.0) then
       gfile%Cpi=nemsio_realfill
       gfile%Ri=nemsio_realfill
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta3
!------------------------------------------------------------------------------
  subroutine nemsio_almeta4(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: allocate recnam, reclvevtyp, and reclev in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)  :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer :: dimrecname,dimreclevtyp,dimreclev
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(gfile%nrec<0) then
      print *,'WRONG: Please set nrec, it is ',gfile%nrec,' now!'
      iret=-6
      return
    endif
    dimrecname=gfile%nrec
    dimreclevtyp=gfile%nrec
    dimreclev=gfile%nrec
    if(allocated(gfile%recname)) deallocate(gfile%recname)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev)
    allocate(gfile%recname(dimrecname),  gfile%reclevtyp(dimreclevtyp), &
             gfile%reclev(dimreclev), stat=iret)
!    print *,'allocate recname,iert=',iret,'dim=',gfile%nrec
    if(iret.eq.0) then
      gfile%reclev=nemsio_intfill
      gfile%recname=' '
      gfile%reclevtyp=' '
    endif
    if(iret.ne.0) iret=-6
  end subroutine nemsio_almeta4
!------------------------------------------------------------------------------
  subroutine nemsio_axmeta(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: empty gfile variables and decallocate arrays in gfile
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)      :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer(nemsio_intkind)              :: ierr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-6
    gfile%gtype=' '
    gfile%gdatatype=' '
    gfile%modelname=' '
    gfile%version=nemsio_intfill
    gfile%nmeta=nemsio_intfill
    gfile%lmeta=nemsio_intfill
    gfile%nrec=nemsio_intfill
    gfile%idate(1:7)=nemsio_intfill
    gfile%nfday=nemsio_intfill
    gfile%nfhour=nemsio_intfill
    gfile%nfminute=nemsio_intfill
    gfile%nfsecondn=nemsio_intfill
    gfile%nfsecondd=nemsio_intfill
    gfile%dimx=nemsio_intfill
    gfile%dimy=nemsio_intfill
    gfile%dimz=nemsio_intfill
    gfile%nframe=nemsio_intfill
    gfile%nsoil=nemsio_intfill
    gfile%ntrac=nemsio_intfill
    gfile%jcap=nemsio_intfill
    gfile%ncldt=nemsio_intfill
    gfile%idvc=nemsio_intfill
    gfile%idsl=nemsio_intfill
    gfile%idvm=nemsio_intfill
    gfile%idrt=nemsio_intfill
    gfile%rlon_min=nemsio_realfill
    gfile%rlon_max=nemsio_realfill
    gfile%rlat_min=nemsio_realfill
    gfile%rlat_max=nemsio_realfill
    gfile%extrameta=nemsio_logicfill
    gfile%nmetavari=nemsio_intfill
    gfile%nmetavarr=nemsio_intfill
    gfile%nmetavarl=nemsio_intfill
    gfile%nmetavarc=nemsio_intfill
    gfile%nmetaaryi=nemsio_intfill
    gfile%nmetaaryr=nemsio_intfill
    gfile%nmetaaryl=nemsio_intfill
    gfile%nmetaaryc=nemsio_intfill
    gfile%tlmeta=nemsio_intfill
    gfile%tlmetalat=nemsio_intfill
    gfile%tlmetalon=nemsio_intfill
    gfile%tlmetadx=nemsio_intfill
    gfile%tlmetady=nemsio_intfill

    if(gfile%nmeta>2) then
    if(allocated(gfile%recname)) deallocate(gfile%recname,stat=ierr)
    if(allocated(gfile%reclevtyp)) deallocate(gfile%reclevtyp,stat=ierr)
    if(allocated(gfile%reclev)) deallocate(gfile%reclev,stat=ierr)
    endif

    if(gfile%nmeta>5) then
    if(allocated(gfile%vcoord)) deallocate(gfile%vcoord,stat=ierr)
    endif

    if(gfile%nmeta>6) then
    if(allocated(gfile%lat)) deallocate(gfile%lat,stat=ierr)
    if(allocated(gfile%lon)) deallocate(gfile%lon,stat=ierr)
    if(allocated(gfile%dx)) deallocate(gfile%dx,stat=ierr)
    if(allocated(gfile%dy)) deallocate(gfile%dy,stat=ierr)
    endif

    if(gfile%nmeta>10) then
    if(allocated(gfile%Cpi)) deallocate(gfile%Cpi,stat=ierr)
    if(allocated(gfile%Ri)) deallocate(gfile%Ri,stat=ierr)
    endif
!
    gfile%mbuf=0
    gfile%nnum=0
    gfile%nlen=0
    gfile%mnum=0
    if(allocated(gfile%cbuf)) deallocate(gfile%cbuf)
    if(allocated(gfile%headvariname)) deallocate(gfile%headvariname,stat=ierr)
    if(allocated(gfile%headvarrname)) deallocate(gfile%headvarrname,stat=ierr)
    if(allocated(gfile%headvarlname)) deallocate(gfile%headvarlname,stat=ierr)
    if(allocated(gfile%headvarcname)) deallocate(gfile%headvarcname,stat=ierr)
    if(allocated(gfile%headvarival)) deallocate(gfile%headvarival,stat=ierr)
    if(allocated(gfile%headvarrval)) deallocate(gfile%headvarrval,stat=ierr)
    if(allocated(gfile%headvarlval)) deallocate(gfile%headvarlval,stat=ierr)
    if(allocated(gfile%headvarcval)) deallocate(gfile%headvarcval,stat=ierr)
    if(allocated(gfile%headaryiname)) deallocate(gfile%headaryiname,stat=ierr)
    if(allocated(gfile%headaryrname)) deallocate(gfile%headaryrname,stat=ierr)
    if(allocated(gfile%headarycname)) deallocate(gfile%headarycname,stat=ierr)
    if(allocated(gfile%headaryival)) deallocate(gfile%headaryival,stat=ierr)
    if(allocated(gfile%headaryrval)) deallocate(gfile%headaryrval,stat=ierr)
    if(allocated(gfile%headarycval)) deallocate(gfile%headarycval,stat=ierr)
!
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine nemsio_axmeta
!------------------------------------------------------------------------------
  subroutine nemsio_setfhead(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: required file header (default)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    integer(nemsio_intkind) i,j,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-17
    gfile%headvarinum=29
    gfile%headvarrnum=4
    gfile%headvarlnum=1
    gfile%headvarcnum=3
!
    if(gfile%nmeta>4) then
      gfile%headaryinum=2
    else
      gfile%headaryinum=1
    endif
!
    if(gfile%nmeta>11) then
      gfile%headaryrnum=7
    elseif(gfile%nmeta>10) then
      gfile%headaryrnum=6
    elseif(gfile%nmeta>9) then
      gfile%headaryrnum=5
    elseif(gfile%nmeta>8) then
      gfile%headaryrnum=4
    elseif(gfile%nmeta>7) then
      gfile%headaryrnum=3
    elseif(gfile%nmeta>6) then
      gfile%headaryrnum=2
    elseif(gfile%nmeta>5) then
      gfile%headaryrnum=1
    endif
!
    if(gfile%nmeta>3) then
      gfile%headarycnum=2
    elseif(gfile%nmeta>2) then
      gfile%headarycnum=1
    else
      gfile%headarycnum=0
    endif
!
!    print*,'in setfhead,before headvariname,headvarival'
    allocate(gfile%headvariname(gfile%headvarinum),gfile%headvarival(gfile%headvarinum) )
    gfile%headvariname(1)='version'
    gfile%headvarival(1)=gfile%version
    gfile%headvariname(2)='nmeta'
    gfile%headvarival(2)=gfile%nmeta
    gfile%headvariname(3)='lmeta'
    gfile%headvarival(3)=gfile%lmeta
    gfile%headvariname(4)='nrec'
    gfile%headvarival(4)=gfile%nrec
    gfile%headvariname(5)='nfday'
    gfile%headvarival(5)=gfile%nfday
    gfile%headvariname(6)='nfhour'
    gfile%headvarival(6)=gfile%nfhour
    gfile%headvariname(7)='nfminute'
    gfile%headvarival(7)=gfile%nfminute
    gfile%headvariname(8)='nfsecondn'
    gfile%headvarival(8)=gfile%nfsecondn
    gfile%headvariname(9)='nfsecondd'
    gfile%headvarival(9)=gfile%nfsecondd
    gfile%headvariname(10)='dimx'
    gfile%headvarival(10)=gfile%dimx
    gfile%headvariname(11)='dimy'
    gfile%headvarival(11)=gfile%dimy
    gfile%headvariname(12)='dimz'
    gfile%headvarival(12)=gfile%dimz
    gfile%headvariname(13)='nframe'
    gfile%headvarival(13)=gfile%nframe
    gfile%headvariname(14)='nsoil'
    gfile%headvarival(14)=gfile%nsoil
    gfile%headvariname(15)='ntrac'
    gfile%headvarival(15)=gfile%ntrac
    gfile%headvariname(16)='jcap'
    gfile%headvarival(16)=gfile%jcap
    gfile%headvariname(17)='ncldt'
    gfile%headvarival(17)=gfile%ncldt
    gfile%headvariname(18)='idvc'
    gfile%headvarival(18)=gfile%idvc
    gfile%headvariname(19)='idsl'
    gfile%headvarival(19)=gfile%idsl
    gfile%headvariname(20)='idvm'
    gfile%headvarival(20)=gfile%idvm
    gfile%headvariname(21)='idrt'
    gfile%headvarival(21)=gfile%idrt
    gfile%headvariname(22)='nmetavari'
    gfile%headvarival(22)=gfile%nmetavari
    gfile%headvariname(23)='nmetavarr'
    gfile%headvarival(23)=gfile%nmetavarr
    gfile%headvariname(24)='nmetavarl'
    gfile%headvarival(24)=gfile%nmetavarl
    gfile%headvariname(25)='nmetavarc'
    gfile%headvarival(25)=gfile%nmetavarc
    gfile%headvariname(26)='nmetaaryi'
    gfile%headvarival(26)=gfile%nmetaaryi
    gfile%headvariname(27)='nmetaaryr'
    gfile%headvarival(27)=gfile%nmetaaryr
    gfile%headvariname(28)='nmetaaryl'
    gfile%headvarival(28)=gfile%nmetaaryl
    gfile%headvariname(29)='nmetaaryc'
    gfile%headvarival(29)=gfile%nmetaaryc
!
    allocate(gfile%headvarrname(gfile%headvarrnum),gfile%headvarrval(gfile%headvarrnum) )
    gfile%headvarrname(1)='rlon_min'
    gfile%headvarrval(1)=gfile%rlon_min
    gfile%headvarrname(2)='rlon_max'
    gfile%headvarrval(2)=gfile%rlon_max
    gfile%headvarrname(3)='rlat_min'
    gfile%headvarrval(3)=gfile%rlat_min
    gfile%headvarrname(4)='rlat_min'
    gfile%headvarrval(4)=gfile%rlat_min
!
    allocate(gfile%headvarcname(gfile%headvarcnum),gfile%headvarcval(gfile%headvarcnum) )
    gfile%headvarcname(1)='gtype'
    gfile%headvarcval(1)=gfile%gtype
    gfile%headvarcname(2)='modelname'
    gfile%headvarcval(2)=gfile%modelname
    gfile%headvarcname(3)='gdatatype'
    gfile%headvarcval(3)=gfile%gdatatype
!head logic var
    allocate(gfile%headvarlname(gfile%headvarlnum),gfile%headvarlval(gfile%headvarlnum) )
    gfile%headvarlname(1)='extrameta'
    gfile%headvarlval(1)=gfile%extrameta
!
!--- gfile%head int ary
!    print *,'before setfhead, headaryi,nrec=',gfile%nrec,gfile%headaryinum
    allocate(gfile%headaryiname(gfile%headaryinum) )
    allocate(gfile%headaryival(max(size(gfile%reclev),7),gfile%headaryinum))
    gfile%headaryiname(1)='idate'
    gfile%headaryival(1:7,1)=gfile%idate(1:7)
    if(gfile%headaryinum>1) then
       gfile%headaryiname(2)='reclev'
       gfile%headaryival(:,2)=gfile%reclev(:)
    endif
!
!--- gfile%head real ary
    if(gfile%headaryrnum>0) then
      if(.not.allocated(gfile%headaryrname)) allocate(gfile%headaryrname(gfile%headaryrnum) )
      if(.not.allocated(gfile%headaryrval)) &
        allocate(gfile%headaryrval(max(gfile%fieldsize,(gfile%dimz+1)*6),gfile%headaryrnum))
      gfile%headaryrname(1)='vcoord'
      do j=1,2
      do i=1,3
       do k=1,gfile%dimz+1
        gfile%headaryrval(k+((j-1)*3+i-1)*(gfile%dimz+1),1)=gfile%vcoord(k,i,j)
       enddo
      enddo
      enddo
      if(gfile%headaryrnum>1) then
        gfile%headaryrname(2)='lat'
        gfile%headaryrval(:,2)=gfile%lat
      endif
      if(gfile%headaryrnum>2) then
        gfile%headaryrname(3)='lon'
        gfile%headaryrval(:,3)=gfile%lon
      endif
      if(gfile%headaryrnum>3) then
        gfile%headaryrname(4)='dx'
        gfile%headaryrval(:,4)=gfile%dx
      endif
      if(gfile%headaryrnum>4) then
        gfile%headaryrname(5)='dy'
        gfile%headaryrval(:,5)=gfile%dy
      endif
      if(gfile%headaryrnum>5) then
        gfile%headaryrname(6)='cpi'
        gfile%headaryrval(1:size(gfile%cpi),6)=gfile%cpi
      endif
      if(gfile%headaryrnum>6) then
        gfile%headaryrname(7)='ri'
        gfile%headaryrval(1:size(gfile%ri),7)=gfile%ri
      endif
    endif
!
!--- gfile%head char var
!    print *,'before setfhead, headaryc,nrec=',gfile%nrec,gfile%headarycnum
    if(gfile%headarycnum >0) then
      allocate(gfile%headarycname(gfile%headarycnum) )
      allocate(gfile%headarycval(size(gfile%recname),gfile%headarycnum))
      gfile%headarycname(1)='recname'
      gfile%headarycval(:,1)=gfile%recname
      if(gfile%headarycnum >1) then
        gfile%headarycname(2)='reclevtyp'
        gfile%headarycval(:,2)=gfile%reclevtyp
      endif
    endif
!
    iret=0
  end subroutine nemsio_setfhead
!------------------------------------------------------------------------------
  subroutine nemsio_setgrbtbl(iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set up grib table
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    integer(nemsio_intkind),intent(out)  :: iret
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    iret=-7
    gribtable(1)%iptv=2
    gribtable(1)%item(1)=nemsio_grbtbl_item('hgt','sfc',1,0,7,1)
    gribtable(1)%item(2)=nemsio_grbtbl_item('pres','sfc',0,0,1,1)
    gribtable(1)%item(3)=nemsio_grbtbl_item('pres','mid layer',0,0,1,109)
    gribtable(1)%item(4)=nemsio_grbtbl_item('dpres','mid layer',2,0,1,110)
    gribtable(1)%item(5)=nemsio_grbtbl_item('tmp','mid layer',2,0,11,109)
    gribtable(1)%item(6)=nemsio_grbtbl_item('ugrd','mid layer',2,0,33,109)
    gribtable(1)%item(7)=nemsio_grbtbl_item('vgrd','mid layer',2,0,34,109)
    gribtable(1)%item(8)=nemsio_grbtbl_item('spfh','mid layer',7,0,51,109)
    gribtable(1)%item(9)=nemsio_grbtbl_item('o3mr','mid layer',9,0,154,109)
    gribtable(1)%item(10)=nemsio_grbtbl_item('clwmr','mid layer',7,0,153,109)
!
    gribtable(1)%item(11)=nemsio_grbtbl_item('vvel','mid layer',9,0,39,109)
    gribtable(1)%item(12)=nemsio_grbtbl_item('tmp','sfc',3,0,11,1)
    gribtable(1)%item(13)=nemsio_grbtbl_item('soilw','0-10 cm down',4,10,144,112)
    gribtable(1)%item(14)=nemsio_grbtbl_item('soilw','10-40 cm down',4,2600,144,112)
    gribtable(1)%item(15)=nemsio_grbtbl_item('soilw','40-100 cm down',4,10340,144,112)
    gribtable(1)%item(16)=nemsio_grbtbl_item('soilw','100-200 cm down',4,25800,144,112)
    gribtable(1)%item(17)=nemsio_grbtbl_item('tmp','0-10 cm down',3,10,11,112)
    gribtable(1)%item(18)=nemsio_grbtbl_item('tmp','10-40 cm down',3,2600,11,112)
    gribtable(1)%item(19)=nemsio_grbtbl_item('tmp','40-100 cm down',3,10340,11,112)
    gribtable(1)%item(20)=nemsio_grbtbl_item('tmp','100-200 cm down',3,25800,11,112)
!
    gribtable(1)%item(21)=nemsio_grbtbl_item('weasd','sfc',5,0,65,1)
    gribtable(1)%item(22)=nemsio_grbtbl_item('tg3','sfc',2,0,11,111)
    gribtable(1)%item(23)=nemsio_grbtbl_item('sfcr','sfc',4,0,83,1)
    gribtable(1)%item(24)=nemsio_grbtbl_item('tcdc','high cld lay',0,0,71,234)
    gribtable(1)%item(25)=nemsio_grbtbl_item('pres','high cld top',-1,0,1,233)
    gribtable(1)%item(26)=nemsio_grbtbl_item('pres','high cld bot',-1,0,1,232)
    gribtable(1)%item(27)=nemsio_grbtbl_item('tmp','high cld top',3,0,11,233)
    gribtable(1)%item(28)=nemsio_grbtbl_item('tcdc','mid cld lay',0,0,71,224)
    gribtable(1)%item(29)=nemsio_grbtbl_item('pres','mid cld top',-1,0,1,223)
    gribtable(1)%item(30)=nemsio_grbtbl_item('pres','mid cld bot',-1,0,1,222)
!
    gribtable(1)%item(31)=nemsio_grbtbl_item('tmp','mid cld top',3,0,11,223)
    gribtable(1)%item(32)=nemsio_grbtbl_item('tcdc','low cld lay',0,0,71,214)
    gribtable(1)%item(33)=nemsio_grbtbl_item('pres','low cld top',-1,0,1,213)
    gribtable(1)%item(34)=nemsio_grbtbl_item('pres','low cld bot',-1,0,1,212)
    gribtable(1)%item(35)=nemsio_grbtbl_item('tmp','low cld top',3,0,11,213)
    gribtable(1)%item(36)=nemsio_grbtbl_item('tcdc','atmos col',0,0,71,200)   !orog???
    gribtable(1)%item(37)=nemsio_grbtbl_item('tcdc','convect-cld laye',3,0,71,244)   !orog???
    gribtable(1)%item(38)=nemsio_grbtbl_item('pres','convect-cld bot',-1,0,1,242)
    gribtable(1)%item(39)=nemsio_grbtbl_item('pres','convect-cld top',-1,0,1,243)
    gribtable(1)%item(40)=nemsio_grbtbl_item('tcdc','bndary-layer cld',3,0,71,211)   !orog???
!
    gribtable(1)%item(41)=nemsio_grbtbl_item('alvsf','sfc',3,0,176,1)
    gribtable(1)%item(42)=nemsio_grbtbl_item('alvwf','sfc',3,0,177,1)
    gribtable(1)%item(43)=nemsio_grbtbl_item('alnsf','sfc',3,0,178,1)
    gribtable(1)%item(44)=nemsio_grbtbl_item('alnwf','sfc',3,0,179,1)
    gribtable(1)%item(45)=nemsio_grbtbl_item('land','sfc',0,0,81,1)
    gribtable(1)%item(46)=nemsio_grbtbl_item('veg','sfc',2,0,87,1)
    gribtable(1)%item(47)=nemsio_grbtbl_item('cnwat','sfc',5,0,223,1)
    gribtable(1)%item(48)=nemsio_grbtbl_item('f10m','10 m above gnd',5,10,180,105)
    gribtable(1)%item(49)=nemsio_grbtbl_item('ugrd','10 m above gnd',2,10,33,105)
    gribtable(1)%item(50)=nemsio_grbtbl_item('vgrd','10 m above gnd',2,10,34,105)
!
    gribtable(1)%item(51)=nemsio_grbtbl_item('tmp','2 m above gnd',3,2,11,105)
    gribtable(1)%item(52)=nemsio_grbtbl_item('spfh','2 m above gnd',6,2,51,105)
    gribtable(1)%item(53)=nemsio_grbtbl_item('vtype','sfc',1,0,225,1)
    gribtable(1)%item(54)=nemsio_grbtbl_item('facsf','sfc',3,0,207,1)
    gribtable(1)%item(55)=nemsio_grbtbl_item('facsf','sfc',3,0,208,1)
    gribtable(1)%item(56)=nemsio_grbtbl_item('fricv','sfc',3,0,253,1)
    gribtable(1)%item(57)=nemsio_grbtbl_item('ffmm','sfc',3,0,253,1)   !???
    gribtable(1)%item(58)=nemsio_grbtbl_item('ffhh','sfc',3,0,253,1)   !???
    gribtable(1)%item(59)=nemsio_grbtbl_item('icetk','sfc',2,0,92,1) 
    gribtable(1)%item(60)=nemsio_grbtbl_item('icec','sfc',3,0,91,1) 
!
    gribtable(1)%item(61)=nemsio_grbtbl_item('tisfc','sfc',2,0,171,1) 
    gribtable(1)%item(62)=nemsio_grbtbl_item('tprcp','sfc',2,0,171,1)  !tprc ???
    gribtable(1)%item(63)=nemsio_grbtbl_item('crain','sfc',0,0,140,1)  !srflag ???
    gribtable(1)%item(64)=nemsio_grbtbl_item('snod','sfc',6,0,66,1)  
    gribtable(1)%item(65)=nemsio_grbtbl_item('slc','soil layer',3,130,160,112)
    gribtable(1)%item(66)=nemsio_grbtbl_item('shdmin','sfc',3,0,189,1)
    gribtable(1)%item(67)=nemsio_grbtbl_item('shdmax','sfc',3,0,190,1)
    gribtable(1)%item(68)=nemsio_grbtbl_item('sotyp','sfc',1,0,224,1)
    gribtable(1)%item(69)=nemsio_grbtbl_item('salbd','sfc',1,0,194,1)
!jw    gribtable(1)%item(49)=nemsio_grbtbl_item('orog','sfc',1,0,194,1)   !orog???
!flx
    gribtable(1)%item(70)=nemsio_grbtbl_item('uflx','sfc',3,0,124,1)   
!
    gribtable(1)%item(71)=nemsio_grbtbl_item('vflx','sfc',3,0,125,1)   
    gribtable(1)%item(72)=nemsio_grbtbl_item('shtfl','sfc',0,0,122,1)   
    gribtable(1)%item(73)=nemsio_grbtbl_item('lhtfl','sfc',0,0,121,1)   
    gribtable(1)%item(74)=nemsio_grbtbl_item('dlwrf','sfc',0,0,205,1)   
    gribtable(1)%item(75)=nemsio_grbtbl_item('ulwrf','sfc',0,0,212,1)   
    gribtable(1)%item(76)=nemsio_grbtbl_item('ulwrf','nom. top',0,0,212,8)   
    gribtable(1)%item(77)=nemsio_grbtbl_item('uswrf','nom. top',0,0,211,8)  
    gribtable(1)%item(78)=nemsio_grbtbl_item('uswrf','sfc',0,0,211,1)  
    gribtable(1)%item(79)=nemsio_grbtbl_item('dswrf','sfc',0,0,204,1)   
    gribtable(1)%item(80)=nemsio_grbtbl_item('prate','sfc',6,0,59,1)   

    gribtable(1)%item(81)=nemsio_grbtbl_item('soilm','0-200 cm down',4,200,86,112)   
    gribtable(1)%item(82)=nemsio_grbtbl_item('vgtyp','sfc',1,0,225,1)   
    gribtable(1)%item(83)=nemsio_grbtbl_item('cprat','sfc',6,0,214,1)   
    gribtable(1)%item(84)=nemsio_grbtbl_item('gflux','sfc',0,0,155,1)  
    gribtable(1)%item(85)=nemsio_grbtbl_item('tmax','2 m above gnd',1,2,15,105)   
    gribtable(1)%item(86)=nemsio_grbtbl_item('tmin','2 m above gnd',1,2,16,105)  
    gribtable(1)%item(87)=nemsio_grbtbl_item('watr','sfc',5,0,90,1)   
    gribtable(1)%item(88)=nemsio_grbtbl_item('pevpr','sfc',0,0,145,1)   
    gribtable(1)%item(89)=nemsio_grbtbl_item('cwork','atmos col',0,0,146,200)   
    gribtable(1)%item(90)=nemsio_grbtbl_item('u-gwd','sfc',3,0,147,1)   
!
    gribtable(1)%item(91)=nemsio_grbtbl_item('v-gwd','sfc',3,0,148,1)  
    gribtable(1)%item(92)=nemsio_grbtbl_item('hpbl','sfc',0,0,221,1)  
    gribtable(1)%item(93)=nemsio_grbtbl_item('pwat','atmos col',1,0,54,200)   
    gribtable(1)%item(94)=nemsio_grbtbl_item('albdo','sfc',1,0,84,1)   
    gribtable(1)%item(95)=nemsio_grbtbl_item('cnwat','sfc',5,0,223,1)   
    gribtable(1)%item(96)=nemsio_grbtbl_item('sfexc','sfc',4,0,208,1)   
    gribtable(1)%item(97)=nemsio_grbtbl_item('pevpr','sfc',0,0,145,1)  
    gribtable(1)%item(98)=nemsio_grbtbl_item('dlwrf','sfc',0,0,205,1)   
    gribtable(1)%item(99)=nemsio_grbtbl_item('ulwrf','sfc',0,0,212,1)   
    gribtable(1)%item(100)=nemsio_grbtbl_item('uswrf','sfc',0,0,211,1)  
!
    gribtable(1)%item(101)=nemsio_grbtbl_item('dswrf','sfc',0,0,204,1)   
    gribtable(1)%item(102)=nemsio_grbtbl_item('ssrun','sfc',5,0,235,1)   
    gribtable(1)%item(103)=nemsio_grbtbl_item('tmp','hybrid lev 1',3,1,11,109)   
    gribtable(1)%item(104)=nemsio_grbtbl_item('spfh','hybrid lev 1',6,1,51,109)   
    gribtable(1)%item(105)=nemsio_grbtbl_item('ugrd','hybrid lev 1',2,1,33,109)  
    gribtable(1)%item(106)=nemsio_grbtbl_item('vgrd','hybrid lev 1',2,1,34,109)  
    gribtable(1)%item(107)=nemsio_grbtbl_item('hgt','hybrid lev 1',2,1,7,109)   
    gribtable(1)%item(108)=nemsio_grbtbl_item('evbs','sfc',0,0,199,1)   
    gribtable(1)%item(109)=nemsio_grbtbl_item('evcw','sfc',0,0,200,1)   
    gribtable(1)%item(110)=nemsio_grbtbl_item('trans','sfc',0,0,210,1)   
    gribtable(1)%item(111)=nemsio_grbtbl_item('snowc','sfc',3,0,238,1)   
!    
!    gribtable(1)%item(50)=nemsio_grbtbl_item('nlat','sfc',2,0,176,1)
!    gribtable(1)%item(51)=nemsio_grbtbl_item('elon','sfc',2,0,177,1)
!    gribtable(1)%item(52)=nemsio_grbtbl_item('nlonb','sfc',2,0,177,1)   !vlat ???
!    gribtable(1)%item(53)=nemsio_grbtbl_item('elonb','sfc',2,0,177,1)   !vlon ???
!    gribtable(1)%item(54)=nemsio_grbtbl_item('wtend','sfc',6,0,236,1)   !wtend precision
!    gribtable(1)%item(55)=nemsio_grbtbl_item('omgalf','sfc',6,0,154,1)   !wtend precision
!    gribtable(1)%item(56)=nemsio_grbtbl_item('omgalf','sfc',6,0,154,1)   !wtend precision
!
!*** table 129
    gribtable(2)%iptv=129
    gribtable(2)%item(1)=nemsio_grbtbl_item('duvb','sfc',2,0,200,1)   
    gribtable(2)%item(2)=nemsio_grbtbl_item('cduvb','sfc',2,0,201,1)   
!
!*** table 130
    gribtable(3)%iptv=130
    gribtable(3)%item(1)=nemsio_grbtbl_item('sltyp','sfc',0,0,222,1)
    gribtable(3)%item(2)=nemsio_grbtbl_item('sbsno','sfc',0,0,198,1)   
    gribtable(3)%item(3)=nemsio_grbtbl_item('soill','0-10 cm down',4,10,160,112)
    gribtable(3)%item(4)=nemsio_grbtbl_item('soill','10-40 cm down',4,2600,160,112)
    gribtable(3)%item(5)=nemsio_grbtbl_item('soill','40-100 cm down',4,10340,160,112)
    gribtable(3)%item(6)=nemsio_grbtbl_item('soill','100-200 cm down',4,25800,160,112)
    gribtable(3)%item(7)=nemsio_grbtbl_item('acond','sfc',4,0,179,1)   
!
    iret=0
  end subroutine nemsio_setgrbtbl
!------------------------------------------------------------------------------
  subroutine nemsio_gfinit(gfile,iret,recname,reclevtyp,reclev)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set gfile variables to operational model output
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
    implicit none
    type(nemsio_gfile),intent(inout)     :: gfile
    integer(nemsio_intkind),intent(out)  :: iret
    character(*),optional,intent(in)  :: recname(:)
    character(*),optional,intent(in)  :: reclevtyp(:)
    integer(nemsio_intkind),optional,intent(in)     :: reclev(:)
    integer  :: i,j,rec,rec3dopt
    real(nemsio_dblekind),allocatable :: slat(:),wlat(:)
    real(nemsio_dblekind),allocatable :: dx(:)
    real(nemsio_dblekind)             :: radi
    logical(nemsio_logickind)         :: linit
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! set operational format
!
    iret=-8
    gfile%version=200809
    gfile%nfday=0
    gfile%nfhour=0
    gfile%nfminute=0
    gfile%nfsecondn=0
    gfile%nfsecondd=100.
    gfile%extrameta=.false.
    gfile%nmetavari=0
    gfile%nmetavarr=0
    gfile%nmetavarl=0
    gfile%nmetavarc=0
    gfile%nmetaaryi=0
    gfile%nmetaaryr=0
    gfile%nmetaaryl=0
    gfile%nmetaaryc=0
    write(0,*)'in gfinit, modelname=',gfile%modelname

    if ( gfile%modelname .eq. 'GFS') then
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=576
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=1152
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=64
      if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=0
      if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=3
      if(gfile%nrec.eq.nemsio_intfill)gfile%nrec=2+9*gfile%dimz+35+3*gfile%nsoil
      gfile%ncldt=1
      gfile%idsl=0
      gfile%idvm=0
      gfile%idrt=4
      linit=gfile%dimy==576.and.gfile%dimx==1152.and.gfile%dimz==64
      gfile%extrameta=.True.
      gfile%nmetavari=5
      if(linit) then
        gfile%jcap=382
        gfile%idvc=2
        gfile%nmetavari=15
        gfile%nmetavarr=1
        gfile%nmetaaryi=1
      endif
    else if (gfile%modelname .eq. 'NMMB' ) then
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=257
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=181
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=35
      if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=1
      if(gfile%ntrac.eq.nemsio_intfill) gfile%ntrac=4
      if(gfile%nrec.eq.nemsio_intfill)     & 
        gfile%nrec=86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4
      linit=gfile%dimx==257.and.gfile%dimy==181.and.gfile%dimz==35
      if(linit) then
        gfile%extrameta=.True.
        gfile%nmetavari=9
        gfile%nmetavarr=12
        gfile%nmetavarl=2
        gfile%nmetaaryr=7
        gfile%rlon_min=-178.5937347
        gfile%rlon_max=178.5937347
        gfile%rlat_min=-89.49999237
        gfile%rlat_max=89.49999237
      endif
!    print *,'in gfinit, nrec=',gfile%nrec
    else if (gfile%modelname.eq. "GSI" ) then
      if(gfile%dimx.eq.nemsio_intfill) gfile%dimx=1152
      if(gfile%dimy.eq.nemsio_intfill) gfile%dimy=576
      if(gfile%dimz.eq.nemsio_intfill) gfile%dimz=64
      if(gfile%nrec.eq.nemsio_intfill)     & 
        gfile%nrec=10+3*gfile%dimz+gfile%ntrac*gfile%dimz
      linit=gfile%dimx==1152.and.gfile%dimy==576.and.gfile%dimz==64
      if(linit) then
        gfile%jcap=382
        gfile%idvc=2
        gfile%ncldt=1
        gfile%idsl=0
        gfile%idvm=0
        gfile%idrt=4
        gfile%extrameta=.True.
        gfile%nmetaaryc=1
      endif
    endif
    if(gfile%dimx.eq.nemsio_intfill.or.gfile%dimy.eq.nemsio_intfill.or. &
       gfile%dimz.eq.nemsio_intfill.or.gfile%idate(1).eq.nemsio_intfill) then
       print *,'WRONG: please provide dimensions!'
       call nemsio_stop
    endif
    if(gfile%nframe.eq.nemsio_intfill) gfile%nframe=0
    gfile%fieldsize=(gfile%dimx+2*gfile%nframe)*(gfile%dimy+2*gfile%nframe)
    if(gfile%nrec.eq.nemsio_intfill) gfile%nrec=12+(3+gfile%ntrac)*gfile%dimz
!
!    print *,'gfinit, after set up dimension,',gfile%nrec,gfile%ntrac,gfile%fieldsize,&
!       gfile%dimz
    call nemsio_almeta(gfile,iret)
    if ( iret.ne.0 ) return
    call nemsio_alextrameta(gfile,iret)
    if ( iret.ne.0 ) return
!    print *,'gfinit, after set up allocate array size dx',size(gfile%dx),size(gfile%cpi), &
!      size(gfile%variname), size(gfile%varrname),size(gfile%varlname),size(gfile%aryrname),&
!      gfile%nmetavari,gfile%nmetavarr,gfile%nmetavarl,gfile%nmetaaryr,gfile%nmetaaryi
!
!
    if ( gfile%modelname .eq. 'GFS' ) then
      gfile%variname=(/'itrun  ','iorder ','irealf ','igen   ','icen2  '/ )
      gfile%varival=(/1,2,1,82,0/)
      if(linit) then
      gfile%variname=(/'itrun  ','iorder ','irealf ','igen   ','latf   ','lonf   ','latr   ','lonr   ', &
                      'icen2  ','idpp   ','idvt   ','idrun  ','idusr  ','ixgr   ','nvcoord'/)
      gfile%varival=(/1,2,1,82,576,1152,576,1152,0,21,0,0,0,0,2/)
      gfile%varrname=(/'pdryini'/)
      gfile%varrval=(/98.29073/)
      gfile%aryiname(1)='iens'
      gfile%aryilen(1)=2
      allocate(gfile%aryival(maxval(gfile%aryilen),gfile%nmetaaryi))
      gfile%aryival(:,1)=(/0,0/)
!      print *,'before gfile vcoord',size(gfile%vcoord,1),size(gfile%vcoord,2),size(gfile%vcoord,3)

      if(gfile%dimz==64) then
      gfile%vcoord(1:gfile%dimz+1,1,1)=(/2*0.0000000,0.57499999,5.7410002,21.516001,55.712002, &
      116.89900,214.01500,356.22299,552.71997,812.48901,1143.9880,1554.7889, &
      2051.1499,2637.5530,3316.2170,4086.6140,4945.0288,5884.2061,6893.1172, &
      7956.9082,9057.0508,10171.712,11276.348,12344.490,13348.671,14261.435, &
      15056.342,15708.893,16197.315,16503.145,16611.604,16511.736,16197.967, &
      15683.489,14993.074,14154.316,13197.065,12152.937,11054.853,9936.6143, &
      8832.5371,7777.1499,6804.8740,5937.0498,5167.1460,4485.4932,3883.0520, &
      3351.4600,2883.0381,2470.7881,2108.3660,1790.0510,1510.7111,1265.7520, &
      1051.0800,863.05798,698.45697,554.42401,428.43399,318.26599,221.95799, &
      137.78999,64.247002,0.0000000 /)
      gfile%vcoord(1:gfile%dimz+1,2,1)=(/1.0000000,0.99467117,0.98862660,0.98174226,0.97386760, &
      0.96482760,0.95443410,0.94249105,0.92879730,0.91315103,0.89535499, &
      0.87522358,0.85259068,0.82731885,0.79930973,0.76851469,0.73494524, &
      0.69868290,0.65988702,0.61879963,0.57574666,0.53113484,0.48544332, &
      0.43921080,0.39301825,0.34746850,0.30316412,0.26068544,0.22057019, &
      0.18329623,0.14926878,0.11881219,0.92166908E-01,0.69474578E-01,0.50646842E-01, &
      0.35441618E-01, 0.23555880E-01,0.14637120E-01,0.82940198E-02,0.41067102E-02, &
      0.16359100E-02,0.43106001E-03,0.36969999E-04,0.0000000*22 /) 
      gfile%vcoord(1:gfile%dimz+1,3,1)=0.
      gfile%vcoord(1:gfile%dimz+1,1,2)=0.
      gfile%vcoord(1:gfile%dimz+1,2,2)=0.
      gfile%vcoord(1:gfile%dimz+1,3,2)=0.
      endif

     if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
     if(size(gfile%recname).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     rec=1
     gfile%recname(rec)='hgt'
     gfile%recname(rec+1)='pres'
     gfile%recname(rec+2:rec+gfile%dimz+1)='pres'
     gfile%recname(rec+gfile%dimz+2:rec+2*gfile%dimz+1)='dpres'
     gfile%recname(rec+2*gfile%dimz+2:rec+3*gfile%dimz+1)='tmp'
     gfile%recname(rec+3*gfile%dimz+2:rec+4*gfile%dimz+1)='ugrd'
     gfile%recname(rec+4*gfile%dimz+2:rec+5*gfile%dimz+1)='vgrd'
     gfile%recname(rec+5*gfile%dimz+2:rec+6*gfile%dimz+1)='spfh'
     gfile%recname(rec+6*gfile%dimz+2:rec+7*gfile%dimz+1)='o3mr'
     gfile%recname(rec+7*gfile%dimz+2:rec+8*gfile%dimz+1)='clwmr'
     gfile%recname(rec+8*gfile%dimz+2:rec+9*gfile%dimz+1)='vvel'
     rec=rec+9*gfile%dimz+1
     gfile%recname(rec+1:rec+35)=(/'slmsk ','orog  ','tsea  ','sheleg','tg3   ','zorl  ', &
       'cv    ','cvb   ','cvt   ',  &
       'alvsf ','alvwf ','alnsf ','alnwf ','vfrac ','canopy','f10m  ','t2m   ',    &
       'q2m   ','vtype ','stype ','facsf ','facwf ','uustar','ffmm  ','ffhh  ',     &
       'hice  ','fice  ','tisfc ','tprcp ','srflag','snwdph','shdmin','shdmax',  &
       'slope ','snoalb' /)
     gfile%recname(rec+36:rec+35+gfile%nsoil)='stc'
     gfile%recname(rec+36+gfile%nsoil:rec+35+2*gfile%nsoil)='smc'
     gfile%recname(rec+36+2*gfile%nsoil:rec+35+3*gfile%nsoil)='slc'
     endif

     if(size(gfile%reclevtyp).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     rec=1
     gfile%reclevtyp='sfc'
     gfile%reclevtyp(rec+2:rec+gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+gfile%dimz+2:rec+2*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+2*gfile%dimz+2:rec+3*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+3*gfile%dimz+2:rec+4*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+4*gfile%dimz+2:rec+5*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+5*gfile%dimz+2:rec+6*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+6*gfile%dimz+2:rec+7*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+7*gfile%dimz+2:rec+8*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+8*gfile%dimz+2:rec+9*gfile%dimz+1)='mid layer'
     rec=rec+9*gfile%dimz+36
     gfile%reclevtyp(rec+1:rec+3*gfile%nsoil)='soil layer'
     endif
!
     if(size(gfile%reclev).eq.2+9*gfile%dimz+35+3*gfile%nsoil) then
     gfile%reclev=1
     rec=2
     do j=3,11
     do i=1,gfile%dimz
       gfile%reclev(rec+(j-3)*gfile%dimz+i)=i
     enddo
     enddo
     rec=rec+9*gfile%dimz+35
     do j=1,3
     do i=1,gfile%nsoil
       gfile%reclev(rec+(j-1)*gfile%nsoil+i)=i
     enddo
     enddo
     endif
!
     endif
    endif 
!
!lat:
     allocate(slat(gfile%dimy),wlat(gfile%dimy))
     call splat(gfile%idrt,gfile%dimy,slat,wlat)
     radi=180.0/(4.*atan(1.))
     do  i=1,gfile%dimy
       gfile%lat((i-1)*gfile%dimx+1:i*gfile%dimx) = asin(slat(i)) * radi
     enddo
     deallocate(slat,wlat)
!lon:
     do i=1,gfile%dimx
       gfile%lon(i) = 360./gfile%dimx*(i-1)
     enddo
     do j=2,gfile%dimy
       gfile%lon((j-1)*gfile%dimx+1:j*gfile%dimx) = gfile%lon(1:gfile%dimx)
     enddo
!     write(0,*)'in gfinit, lat=',maxval(gfile%lat(1:gfile%fieldsize)),  &
!       minval(gfile%lat(1:gfile%fieldsize)),'lon=',&
!       maxval(gfile%lon(1:gfile%fieldsize)), &
!       minval(gfile%lon(1:gfile%fieldsize))
   else if ( gfile%modelname .eq. "NMMB" .and. linit) then
      gfile%variname=(/'mp_phys ','sfsfcphy','nphs    ','nclod   ', &
        'nheat   ','nprec   ','nrdlw   ','nrdsw   ','nsrfc   ' /)
      gfile%varival=(/5,99,2,60,60,60,60,60,60/)
      gfile%varrname=(/'pdtop ','dt    ','pt    ','tlm0d ','tph0d ','tstart', &
        'aphtim','ardlw ','ardsw ','asrfc ','avcnvc','avrain' /)
      gfile%varrval=(/26887.10156,180.,1000.,0.,0.,0.,-1000000.0, &
        -1000000.0,-1000000.0,-1000000.0,0.,0./)
      gfile%varlname=(/'run   ','global'/)
      gfile%varlval=(/.true.,.false. /)
      gfile%aryrname=(/'dsg1  ','dsg2  ','sgml1 ','sgml2 ','sg1   ','sg2   ','sldpth'/)
      gfile%aryrlen=(/gfile%dimz,gfile%dimz,gfile%dimz,gfile%dimz, &
        gfile%dimz+1,gfile%dimz+1,gfile%nsoil /)
      allocate(gfile%aryrval(maxval(gfile%aryrlen),gfile%nmetaaryr))
      if(size(gfile%aryrval,1).eq.36) then
      gfile%aryrval(1:35,1)=(/0.8208955079E-01,0.8582090586E-01,0.8582088351E-01,  &
             0.8582088351E-01,0.8582091331E-01,0.8582085371E-01,  &
             0.9328359365E-01,0.9701490402E-01,0.9701496363E-01,  &
             0.9701490402E-01,0.1044776440,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
             0.0000000000E+00,0.0000000000E+00  /)
       gfile%aryrval(1:35,2)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.4098360986E-01, &
             0.4371585697E-01,0.4781420529E-01,0.4918029904E-01,0.5054645240E-01, &
             0.5327869952E-01,0.5464482307E-01,0.5464476347E-01,0.5464485288E-01, &
             0.5464485288E-01,0.5464470387E-01,0.5191260576E-01,0.5054640770E-01, &
             0.4918038845E-01,0.4508191347E-01,0.4371589422E-01,0.3961753845E-01, &
             0.3551906347E-01,0.3005468845E-01,0.2732235193E-01,0.2459019423E-01, &
             0.1912564039E-01,0.1639348269E-01,0.8196711540E-02 /)
       gfile%aryrval(1:35,3)=(/0.4104477540E-01,0.1250000000,0.2108208984,0.2966417670,0.3824626803, &
              0.4682835639,0.5578358173,0.6529850364,0.7500000000,0.8470149040, &
              0.9477611780,1.000000000,1.000000000,1.000000000,1.000000000, &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
              1.000000000,1.000000000,1.000000000,1.000000000,1.000000000 /)
       gfile%aryrval(1:35,4)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
              0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,  &
              0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.2049180493E-01,  &
              0.6284153461E-01,0.1086065695,0.1571038216,0.2069672048,0.2588797808, &
              0.3128415346,0.3674863279,0.4221311212,0.4767760038,0.5314207673,     &
              0.5846993923,0.6359289289,0.6857923269,0.7329235077,0.7773224115,     &
              0.8189890981,0.8565573692,0.8893442750,0.9180327654,0.9439890385,     &
              0.9658470154,0.9836065769,0.9959016442/)
        gfile%aryrval(1:36,5)=(/0.0000000000E+00,0.8208955079E-01,0.1679104567,0.2537313402,  &
             0.3395522237,0.4253731370,0.5111939907,0.6044775844,0.7014924884,  &
             0.7985074520,0.8955223560,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000,1.000000000,1.000000000,1.000000000,  &
             1.000000000,1.000000000 /)
        gfile%aryrval(1:36,6)=(/0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.0000000000E+00,0.0000000000E+00,0.0000000000E+00,0.0000000000E+00, &
             0.4098360986E-01,0.8469946682E-01,0.1325136721,0.1816939712,  &
             0.2322404236,0.2855191231,0.3401639462,0.3948087096,0.4494535625, &
             0.5040984154,0.5587431192,0.6106557250,0.6612021327,0.7103825212, &
             0.7554644346,0.7991803288,0.8387978673,0.8743169308,0.9043716192, &
             0.9316939712,0.9562841654,0.9754098058,0.9918032885,1.000000000 /)
        gfile%aryrval(1,7)=0.1000000015
        gfile%aryrval(2,7)=0.3000000119
        gfile%aryrval(3,7)=0.6000000238
        gfile%aryrval(4,7)=1.000000000
        endif
!
        gfile%dy=111282.1953
        allocate(dx(gfile%dimy+2*gfile%nframe))
        dx=0.
        if(size(dx).eq.183) then
        dx(1:183)=(/2731.143066,0.0000000000E+00,2731.143066,5461.452148,8190.078125,10916.22852, &
             13639.05469,16357.72461,19071.41211,21779.29102,24480.51758,27174.30469,29859.81250, &
             32536.22656,35202.73047,37858.51172,40502.74219,43134.64844,45753.42188,48358.25781, &
             50948.35938,53522.93750,56081.21094,58622.40625,61145.74609,63650.46094,66135.78906, &
             68600.96094,71045.23438,73467.88281,75868.14844,78245.29688,80598.62500,82927.38281, &
             85230.89062,87508.42969,89759.32031,91982.86719,94178.39062,96345.23438,98482.71875, &
             100590.2188,102667.0625,104712.6406,106726.3281,108707.5000,110655.5625,112569.9062, &
             114449.9844,116295.1719,118104.9531,119878.7500,121616.0312,123316.2656,124978.9453, &
             126603.5469,128189.5938,129736.5781,131244.0625,132711.5625,134138.6250,135524.8438, &
             136869.7500,138173.0000,139434.1406,140652.8125,141828.6406,142961.2812,144050.3438, &
             145095.5469,146096.5625,147053.0625,147964.7656,148831.4062,149652.6875,150428.4062, &
             151158.2969,151842.1562,152479.7500,153070.9062,153615.4219,154113.1562,154563.9375, &
             154967.6406,155324.1406,155633.3281,155895.1094,156109.3906,156276.1250,156395.2656, &
             156466.7656,156490.5938,156466.7656,156395.2656,156276.1250,156109.3906,155895.1094, &
             155633.3281,155324.1406,154967.6406,154563.9375,154113.1562,153615.4219,153070.9062, &
             152479.7500,151842.1562,151158.2969,150428.4062,149652.6875,148831.4062,147964.7656, &
             147053.0625,146096.5625,145095.5469,144050.3438,142961.2812,141828.6406,140652.8125, &
             139434.1406,138173.0000,136869.7500,135524.8438,134138.6250,132711.5625,131244.0625, &
             129736.5781,128189.5938,126603.5469,124978.9453,123316.2656,121616.0312,119878.7500, &
             118104.9531,116295.1719,114449.9844,112569.9062,110655.5625,108707.5000,106726.3281, &
             104712.6406,102667.0625,100590.2188,98482.71875,96345.23438,94178.39062,91982.86719, &
             89759.32031,87508.42969,85230.89062,82927.38281,80598.62500,78245.29688,75868.14844, &
             73467.88281,71045.23438,68600.96094,66135.78906,63650.46094,61145.74609,58622.40625, &
             56081.21094,53522.93750,50948.35938,48358.25781,45753.42188,43134.64844,40502.74219, &
             37858.51172,35202.73047,32536.22656,29859.81250,27174.30469,24480.51758,21779.29102, &
             19071.41211,16357.72461,13639.05469,10916.22852,8190.078125,5461.452148,2731.143066, &
             0.0000000000E+00,2731.143066 /)
!       print *,'size(dx)=',size(dx),'jm+2=',gfile%dimy+2,'size(gfile%dx)=',size(gfile%dx), &
!          maxval(gfile%dx),minval(gfile%dx),maxval(gfile%dy),maxval(gfile%dy),'nframe=', &
!          gfile%nframe,'dimy=',gfile%dimy
       if(allocated(gfile%dx).and.size(gfile%dx)==183*(gfile%dimx+2*gfile%nframe)) then
        do i=1,gfile%dimy+2*gfile%nframe
         gfile%dx((i-1)*(gfile%dimx+2*gfile%nframe)+1:i*(gfile%dimx+2*gfile%nframe))=dx(i)
        enddo
       endif
      endif
      deallocate(dx)

     if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
     if(size(gfile%recname)==86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4) then
     rec=1
     gfile%recname(1)='hgt'
     gfile%recname(2)='glat'
     gfile%recname(3)='glon'
     gfile%recname(4)='dpres'
     gfile%recname(5)='vlat'
     gfile%recname(6)='vlon'
     gfile%recname(7)='acfrcv'
     gfile%recname(8)='acfrst'
     gfile%recname(9)='acprec'
     gfile%recname(10)='acsnom'
     gfile%recname(11)='acsnow'
     gfile%recname(12)='akhs_out'
     gfile%recname(13)='akms_out'
     gfile%recname(14)='albase'
     gfile%recname(15)='albedo'
     gfile%recname(16)='alwin'
     gfile%recname(17)='alwout'
     gfile%recname(18)='alwtoa'
     gfile%recname(19)='aswin'
     gfile%recname(20)='aswout'
     gfile%recname(21)='aswtoa'
     gfile%recname(22)='bgroff'
     gfile%recname(23)='cfrach'
     gfile%recname(24)='cfracl'
     gfile%recname(25)='cfracm'
     gfile%recname(26)='cldefi'
     gfile%recname(27)='cmc'
     gfile%recname(28)='cnvbot'
     gfile%recname(29)='cnvtop'
     gfile%recname(30)='cprate'
     gfile%recname(31)='cuppt'
     gfile%recname(32)='cuprec'
     gfile%recname(33)='czen'
     gfile%recname(34)='czmean'
     gfile%recname(35)='epsr'
     gfile%recname(36)='grnflx'
     gfile%recname(37)='hbotd'
     gfile%recname(38)='hbots'
     gfile%recname(39)='htopd'
     gfile%recname(40)='htops'
     gfile%recname(41)='mxsnal'
     gfile%recname(42)='pblh'
     gfile%recname(43)='potevp'
     gfile%recname(44)='prec'
     gfile%recname(45)='pshltr'
     gfile%recname(46)='q10'
     gfile%recname(47)='qsh'
     gfile%recname(48)='qshltr'
     gfile%recname(49)='qwbs'
     gfile%recname(50)='qz0'
     gfile%recname(51)='radot'
     gfile%recname(52)='rlwin'
     gfile%recname(53)='rlwtoa'
     gfile%recname(54)='rswin'
     gfile%recname(55)='rswinc'
     gfile%recname(56)='rswout'
     gfile%recname(57)='sfcevp'
     gfile%recname(58)='sfcexc'
     gfile%recname(59)='sfclhx'
     gfile%recname(60)='sfcshx'
     gfile%recname(61)='si'
     gfile%recname(62)='sice'
     gfile%recname(63)='sigt4'
     gfile%recname(64)='sm'
     gfile%recname(65)='smstav'
     gfile%recname(66)='smstot'
     gfile%recname(67)='sno'
     gfile%recname(68)='snopcx'
     gfile%recname(69)='soiltb'
     gfile%recname(70)='sr'
     gfile%recname(71)='ssroff'
     gfile%recname(72)='tsea'
     gfile%recname(73)='subshx'
     gfile%recname(74)='tg'
     gfile%recname(75)='th10'
     gfile%recname(76)='ths'
     gfile%recname(77)='thz0'
     gfile%recname(78)='tshltr'
     gfile%recname(79)='twbs'
     gfile%recname(80)='u10'
     gfile%recname(81)='uustar'
     gfile%recname(82)='uz0'
     gfile%recname(83)='v10'
     gfile%recname(84)='vfrac'
     gfile%recname(85)='vz0'
     gfile%recname(86)='zorl'

     rec=86
     gfile%recname(rec+1:rec+gfile%dimz)='vvel'
     gfile%recname(rec+gfile%dimz+1:rec+2*gfile%dimz)='dwdt'
     gfile%recname(rec+2*gfile%dimz+1:rec+3*gfile%dimz+1)='pres'
     gfile%recname(rec+3*gfile%dimz+2:rec+4*gfile%dimz+1)='omgalf'
     gfile%recname(rec+4*gfile%dimz+2:rec+5*gfile%dimz+1)='rrw'
     gfile%recname(rec+5*gfile%dimz+2:rec+6*gfile%dimz+1)='cldfra'
     gfile%recname(rec+6*gfile%dimz+2:rec+7*gfile%dimz+1)='clwmr'
     gfile%recname(rec+7*gfile%dimz+2:rec+8*gfile%dimz+1)='exch_h'
     gfile%recname(rec+8*gfile%dimz+2:rec+9*gfile%dimz+1)='spfh'
     gfile%recname(rec+9*gfile%dimz+2:rec+10*gfile%dimz+1)='q2'
     gfile%recname(rec+10*gfile%dimz+2:rec+11*gfile%dimz+1)='rlwtt'
     gfile%recname(rec+11*gfile%dimz+2:rec+12*gfile%dimz+1)='rswtt'
     gfile%recname(rec+12*gfile%dimz+2:rec+13*gfile%dimz+1)='tmp'
     gfile%recname(rec+13*gfile%dimz+2:rec+14*gfile%dimz+1)='tcucn'
     gfile%recname(rec+14*gfile%dimz+2:rec+15*gfile%dimz+1)='train'
     gfile%recname(rec+15*gfile%dimz+2:rec+16*gfile%dimz+1)='ugrd'
     gfile%recname(rec+16*gfile%dimz+2:rec+17*gfile%dimz+1)='vgrd'
     gfile%recname(rec+17*gfile%dimz+2:rec+18*gfile%dimz+1)='xlen_mix'
     gfile%recname(rec+18*gfile%dimz+2:rec+19*gfile%dimz+1)='f_ice'
     gfile%recname(rec+19*gfile%dimz+2:rec+20*gfile%dimz+1)='f_rimef'
     gfile%recname(rec+20*gfile%dimz+2:rec+21*gfile%dimz+1)='f_rain'
     gfile%recname(rec+21*gfile%dimz+2:rec+21*gfile%dimz+gfile%nsoil+1)='sh2o'
     gfile%recname(rec+21*gfile%dimz+gfile%nsoil+2:rec+21*gfile%dimz+2*gfile%nsoil+1)='smc'
     gfile%recname(rec+21*gfile%dimz+2*gfile%nsoil+2:rec+21*gfile%dimz+3*gfile%nsoil+1)='stc'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+2)='sltyp'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+3)='vgtyp'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+4)='cfrcv'
     gfile%recname(rec+21*gfile%dimz+3*gfile%nsoil+5)='cfrst'
     endif

!define rec layer type
     if(size(gfile%reclevtyp)==86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4) then
     gfile%reclevtyp='sfc'
     gfile%reclevtyp(4)='hybrid sig lev'
     gfile%reclevtyp(46)='10 m above gnd'
     gfile%reclevtyp(75)='10 m above gnd'
     gfile%reclevtyp(80)='10 m above gnd'
     gfile%reclevtyp(83)='10 m above gnd'
     rec=86
     gfile%reclevtyp(rec+1:rec+gfile%dimz)='mid layer'
     gfile%reclevtyp(rec+gfile%dimz+1:rec+2*gfile%dimz)='mid layer'
     gfile%reclevtyp(rec+2*gfile%dimz+1:rec+3*gfile%dimz+1)='layer'
     gfile%reclevtyp(rec+3*gfile%dimz+2:rec+4*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+4*gfile%dimz+2:rec+5*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+5*gfile%dimz+2:rec+6*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+6*gfile%dimz+2:rec+7*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+7*gfile%dimz+2:rec+8*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+8*gfile%dimz+2:rec+9*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+9*gfile%dimz+2:rec+10*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+10*gfile%dimz+2:rec+11*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+11*gfile%dimz+2:rec+12*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+12*gfile%dimz+2:rec+13*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+13*gfile%dimz+2:rec+14*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+14*gfile%dimz+2:rec+15*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+15*gfile%dimz+2:rec+16*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+16*gfile%dimz+2:rec+17*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+17*gfile%dimz+2:rec+18*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+18*gfile%dimz+2:rec+19*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+19*gfile%dimz+2:rec+20*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+20*gfile%dimz+2:rec+21*gfile%dimz+1)='mid layer'
     gfile%reclevtyp(rec+21*gfile%dimz+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+5)='100-200 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+gfile%nsoil+5)='100-200 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+2)='0-10 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+3)='10-40 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+4)='40-100 cm down'
     gfile%reclevtyp(rec+21*gfile%dimz+2*gfile%nsoil+5)='100-200 cm down'
     endif
!
!reclev
     if(size(gfile%reclev)==86+20*gfile%dimz+(gfile%dimz+1)+3*gfile%nsoil+4) then
     gfile%reclev=1
     rec=86
     do j=1,3
      do i=1,gfile%dimz
       gfile%reclev(rec+(j-1)*gfile%dimz+i)=i
      enddo
     enddo
     gfile%reclev(rec+3*gfile%dimz+1)=gfile%dimz+1
     do j=4,21
      do i=1,gfile%dimz
       gfile%reclev(rec+(j-1)*gfile%dimz+1+i)=i
      enddo
     enddo
     rec=rec+21*gfile%dimz+1
     do j=22,24
      do i=1,gfile%nsoil
       gfile%reclev(rec+(j-22)*gfile%nsoil+i)=i
      enddo
     enddo
     endif
!
    endif
   else if ( gfile%modelname.eq. "GSI" .and.linit) then
!
    gfile%arycname(1)='recunit'
    gfile%aryclen(1)=gfile%nrec
    allocate(gfile%arycval(maxval(gfile%aryclen),gfile%nmetaaryc))
    gfile%arycval(1,1)='pgm'
    gfile%arycval(2,1)='nondim'
    gfile%arycval(3:gfile%dimz+2,1)='K'
    gfile%arycval(gfile%dimz+3:3*gfile%dimz+2,1)='m/s   '
    gfile%arycval(3*gfile%dimz+3:6*gfile%dimz+2,1)='kg/kg '
    gfile%arycval(6*gfile%dimz+3,1)='%'
    gfile%arycval(6*gfile%dimz+4,1)='K'
    gfile%arycval(6*gfile%dimz+5,1)='kg/m2  '
    gfile%arycval(6*gfile%dimz+6,1)='integer'
    gfile%arycval(6*gfile%dimz+7,1)='%      '
    gfile%arycval(6*gfile%dimz+8,1)='integer'
    gfile%arycval(6*gfile%dimz+9,1)='integer'
    gfile%arycval(6*gfile%dimz+10,1)='m     '
    gfile%arycval(6*gfile%dimz+11,1)='K     '
    gfile%arycval(6*gfile%dimz+12,1)='%     '

    if(.not.present(recname).or..not.present(reclevtyp).or..not.present(reclev) )then
!
     if(size(gfile%recname)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz .and.   &
        size(gfile%reclevtyp)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz .and. &
        size(gfile%reclev)==10+3*gfile%dimz+gfile%ntrac*gfile%dimz )then
      gfile%reclevtyp='sfc'
      gfile%reclev=1
      gfile%recname(1)='hgt'
      gfile%recname(2)='pres'
      rec=2
      gfile%recname(rec+1:rec+gfile%dimz)='tmp'
      gfile%reclevtyp(rec+1:rec+gfile%dimz)='mid layer'
      gfile%recname(rec+gfile%dimz+1:rec+2*gfile%dimz)='ugrd'
      gfile%reclevtyp(rec+gfile%dimz+1:rec+2*gfile%dimz)='mid layer'
      gfile%recname(rec+2*gfile%dimz+1:rec+3*gfile%dimz)='vgrd'
      gfile%reclevtyp(rec+2*gfile%dimz+1:rec+3*gfile%dimz)='mid layer'
      do  i=1,3
       do j=1,gfile%dimz
         gfile%reclev(rec+(i-1)*gfile%dimz+j)=j
       enddo
      enddo
      do i=1,gfile%ntrac
       if ( i.eq.1) gfile%recname(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='spfh'
       if ( i.eq.1) gfile%reclevtyp(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='mid layer'
       if ( i.eq.2) gfile%recname(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='o3mr'
       if ( i.eq.2) gfile%reclevtyp(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='mid layer'
       if ( i.eq.3) gfile%recname(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='clwmr'
       if ( i.eq.3) gfile%reclevtyp(rec+(2+i)*gfile%dimz+1:rec+(3+i)*gfile%dimz)='mid layer'
       do j=1,gfile%dimz
         gfile%reclev(rec+(2+i)*gfile%dimz+j)=j
       enddo
      enddo
      rec=rec+3*gfile%dimz+gfile%ntrac*gfile%dimz
      gfile%recname(rec+1)='f10m'
      gfile%recname(rec+2)='tsea'
      gfile%recname(rec+3)='sheleg'
      gfile%recname(rec+4)='vtype'
      gfile%recname(rec+5)='vfrac'
      gfile%recname(rec+6)='stype'
      gfile%recname(rec+7)='slmsk'
      gfile%recname(rec+8)='zorl'
      gfile%recname(rec+9)='stc'
      gfile%recname(rec+10)='smc'
      gfile%reclevtyp(rec+9:rec+10)='soil layer'
     endif
!
    endif
   endif
!jw   print *,' end of gfinit'
!
   iret=0
  end subroutine nemsio_gfinit
!------------------------------------------------------------------------------
  subroutine nemsio_stop()
    implicit none
     stop
  end subroutine nemsio_stop
!------------------------------------------------------------------------------
!  temporary subroutines for basio file unit
    subroutine nemsio_getlu(gfile,gfname,gaction,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: set unit number to the first number available between 600-699
!           according to unit number array fileunit
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
     type(nemsio_gfile),intent (inout) :: gfile
     character*(*),intent(in)       :: gfname,gaction
     integer,intent(out) :: iret
     integer :: i
     iret=-10
     gfile%gfname=gfname
     gfile%gaction=gaction
     do i=600,699
       if ( fileunit(i) .eq. 0 ) then 
         gfile%flunit=i
         fileunit(i)=i
         iret=0
         exit
       endif
     enddo
    end subroutine nemsio_getlu
!------------------------------------------------------------------------------
!  temporary subroutines for free unit number 
    subroutine nemsio_clslu(gfile,iret)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
! abstract: free unit number array index corresponding to unit number
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - -
      implicit none
     type(nemsio_gfile),intent (inout) :: gfile
     integer, intent(out) :: iret
     iret=-10
     if ( fileunit(gfile%flunit) .ne. 0 ) then
       fileunit(gfile%flunit)=0
       gfile%flunit=0
       iret=0
     endif
    end subroutine nemsio_clslu
!------------------------------------------------------------------------------
      SUBROUTINE nemsio_splat4(IDRT,JMAX,ASLAT,WLAT)
!$$$
      implicit none
      integer(nemsio_intkind),intent(in) :: idrt,jmax
      real(4),intent(out) :: ASLAT(JMAX),WLAT(JMAX)
      INTEGER(nemsio_intkind),PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: ASLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      integer,PARAMETER:: JZ=50
      REAL(nemsio_dblekind) BZ(JZ)
      DATA BZ        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      REAL(8):: DLT,D1=1.
      REAL(8) AWORK((JMAX+1)/2,((JMAX+1)/2)),BWORK(((JMAX+1)/2))
      INTEGER(4):: JHE,JHO,J0=0
      INTEGER(4) IPVT((JMAX+1)/2)
      real,PARAMETER :: PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25
      real r,pkml
      integer jh,js,n,j
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  GAUSSIAN LATITUDES
!      print *,'nemsio_module,in SPLAT4',IDRT,JMAX
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1./SQRT((JMAX+0.5)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.
          DO J=1,JH
            PKM1(J)=1.
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*ASLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          WLAT(J)=(2.*(1.-ASLATD(J)**2))/(JMAX*PKM1(J))**2
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2./JMAX**2
          DO N=2,JMAX,2
            WLAT(JHE)=WLAT(JHE)*N**2/(N-1)**2
          ENDDO
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        ASLAT(1)=1.
        DO J=2,JH
          ASLAT(J)=COS((J-1)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*J*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
        CALL DGEF(AWORK,JHE,JHO,IPVT)
        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J+1)=BWORK(J)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=1.
        DO J=1,JH
          ASLAT(J)=COS((J-0.5)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*(J-0.5)*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
        CALL DGEF(AWORK,JHE,JHO,IPVT)
        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J)=BWORK(J)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
      ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine nemsio_splat4
!----------------------------------------------------------------------
      SUBROUTINE nemsio_splat8(IDRT,JMAX,ASLAT,WLAT)
!$$$
      implicit none
      integer(nemsio_intkind),intent(in) :: idrt,jmax
      real(nemsio_dblekind),intent(out) :: ASLAT(JMAX),WLAT(JMAX)
      INTEGER(nemsio_intkind),PARAMETER:: KD=SELECTED_REAL_KIND(15,45)
      REAL(KIND=KD):: PK(JMAX/2),PKM1(JMAX/2),PKM2(JMAX/2)
      REAL(KIND=KD):: ASLATD(JMAX/2),SP,SPMAX,EPS=10.*EPSILON(SP)
      integer,PARAMETER:: JZ=50
      REAL(nemsio_dblekind) BZ(JZ)
      DATA BZ        / 2.4048255577,  5.5200781103, &
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
      21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, &
      33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, &
      46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, &
      58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, &
      71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, &
      84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, &
      96.6052679510, 99.7468198587, 102.888374254, 106.029930916, &
      109.171489649, 112.313050280, 115.454612653, 118.596176630, &
      121.737742088, 124.879308913, 128.020877005, 131.162446275, &
      134.304016638, 137.445588020, 140.587160352, 143.728733573, &
      146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      REAL(8):: DLT,D1=1.
      REAL(8) AWORK((JMAX+1)/2,((JMAX+1)/2)),BWORK(((JMAX+1)/2))
      INTEGER(4):: JHE,JHO,J0=0
      INTEGER(4) IPVT((JMAX+1)/2)
      real(nemsio_dblekind),PARAMETER :: PI=3.14159265358979,C=(1.-(2./PI)**2)*0.25
      real(nemsio_dblekind) r,splatd,pkml
      integer jh,js,n,j
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  GAUSSIAN LATITUDES
      IF(IDRT.EQ.4) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        R=1./SQRT((JMAX+0.5)**2+C)
        DO J=1,MIN(JH,JZ)
          ASLATD(J)=COS(BZ(J)*R)
        ENDDO
        DO J=JZ+1,JH
          ASLATD(J)=COS((BZ(JZ)+(J-JZ)*PI)*R)
        ENDDO
        SPMAX=1.
        DO WHILE(SPMAX.GT.EPS)
          SPMAX=0.
          DO J=1,JH
            PKM1(J)=1.
            PK(J)=ASLATD(J)
          ENDDO
          DO N=2,JMAX
            DO J=1,JH
              PKM2(J)=PKM1(J)
              PKM1(J)=PK(J)
              PK(J)=((2*N-1)*ASLATD(J)*PKM1(J)-(N-1)*PKM2(J))/N
            ENDDO
          ENDDO
          DO J=1,JH
            SP=PK(J)*(1.-ASLATD(J)**2)/(JMAX*(PKM1(J)-ASLATD(J)*PK(J)))
            ASLATD(J)=ASLATD(J)-SP
            SPMAX=MAX(SPMAX,ABS(SP))
          ENDDO
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(J)=ASLATD(J)
          WLAT(J)=(2.*(1.-ASLATD(J)**2))/(JMAX*PKM1(J))**2
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2./JMAX**2
          DO N=2,JMAX,2
            WLAT(JHE)=WLAT(JHE)*N**2/(N-1)**2
          ENDDO
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES INCLUDING POLES
      ELSEIF(IDRT.EQ.0) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE-1
        DLT=PI/(JMAX-1)
        ASLAT(1)=1.
        DO J=2,JH
          ASLAT(J)=COS((J-1)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*J*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
        CALL DGEF(AWORK,JHE,JHO,IPVT)
        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J+1)=BWORK(J)
        ENDDO
!CDIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  EQUALLY-SPACED LATITUDES EXCLUDING POLES
      ELSEIF(IDRT.EQ.256) THEN
        JH=JMAX/2
        JHE=(JMAX+1)/2
        JHO=JHE
        DLT=PI/JMAX
        ASLAT(1)=1.
        DO J=1,JH
          ASLAT(J)=COS((J-0.5)*DLT)
        ENDDO
        DO JS=1,JHO
          DO J=1,JHO
            AWORK(JS,J)=COS(2*(JS-1)*(J-0.5)*DLT)
          ENDDO
        ENDDO
        DO JS=1,JHO
          BWORK(JS)=-D1/(4*(JS-1)**2-1)
        ENDDO
        CALL DGEF(AWORK,JHE,JHO,IPVT)
        CALL DGES(AWORK,JHE,JHO,IPVT,BWORK,J0)
        WLAT(1)=0.
        DO J=1,JHO
          WLAT(J)=BWORK(J)
        ENDDO
!DIR$ IVDEP
        DO J=1,JH
          ASLAT(JMAX+1-J)=-ASLAT(J)
          WLAT(JMAX+1-J)=WLAT(J)
        ENDDO
        IF(JHE.GT.JH) THEN
          ASLAT(JHE)=0.
          WLAT(JHE)=2.*WLAT(JHE)
        ENDIF
      ENDIF
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     end subroutine nemsio_splat8
!------------------------------------------------------------------------------
!
     elemental function equal_str_nocase(str1,str2)
!
!-----------------------------------------------------------------------
!
! convert a word to lower case
!
      logical              :: equal_str_nocase
      Character (len=*) , intent(in) :: str1
      Character (len=*) , intent(in) :: str2
      integer :: i,ic1,ic2,nlen
      nlen = len(str2)
!
      if(len(str1)/=nlen)  then
        equal_str_nocase=.false.
        return
      endif
      equal_str_nocase=.false.
      do i=1,nlen
        ic1 = ichar(str1(i:i))
        if (ic1 >= 65 .and. ic1 < 91) ic1 = ic1+32
        ic2 = ichar(str2(i:i))
        if (ic2 >= 65 .and. ic2 < 91) ic2 = ic2+32
        if(ic1/=ic2) then
           equal_str_nocase=.false.
           return
        endif
      end do
      equal_str_nocase=.true.
!
!k
!-----------------------------------------------------------------------
!
      end function equal_str_nocase
!
!-----------------------------------------------------------------------
! 
     elemental function lowercase(word)
!
!-----------------------------------------------------------------------
!
! convert a word to lower case
!
      Character (len=32)              :: lowercase
      Character (len=*) , intent(in) :: word
      integer :: i,ic,nlen
      nlen = len(word)
      if(nlen >32) then
        nlen=32
      endif
      lowercase(1:nlen)=word(1:nlen)
      do i=1,nlen
        ic = ichar(word(i:i))
        if (ic >= 65 .and. ic < 91) lowercase(i:i) = char(ic+32)
      end do
      if(nlen<32) lowercase(nlen+1:)=' '
!
!-----------------------------------------------------------------------
!
      end function lowercase
!
!----------------------------------------------------------------------
  end module nemsio_module
