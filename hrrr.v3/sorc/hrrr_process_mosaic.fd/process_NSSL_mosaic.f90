program process_NSSL_mosaic
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI mass grid
!
!     tversion=8  : NSSL 8 tiles netcdf
!     tversion=81 : NCEP 8 tiles binary
!     tversion=4  : NSSL 4 tiles binary
!     tversion=1  : NSSL 1 tile grib2
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________
!
!      use constants, only: zero,one_tenth,one,deg2rad,rad2deg
!      use gridmod, only: regional,nlon,nlat,nsig,         &
!                         tll2xy,txy2ll,                   &
!                         regional_time,nhr_assimilation,  &
!                         regional_fhr,    &
!                         ylat,xlon
  use mpi
  use kinds, only: r_kind,i_kind

  implicit none
!
  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  REAL, allocatable :: ref3d(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: ref0(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: maxref(:,:)   ! composite reflectivity
  integer , allocatable :: imaxref(:,:)   ! composite reflectivity
  REAL(r_kind), allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column
  CHARACTER*180   geofile
!
!  For reflectiivty mosaic
!
  CHARACTER*256   mosaicfile
  CHARACTER*256   filenameall(200)

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: msclon(:)        ! longitude of mosaic data
  REAL, allocatable :: msclat(:)        ! latitude of mosaic data
  REAL, allocatable :: msclev(:)        ! level of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity
  REAL, allocatable :: mscValue3d(:,:,:)    ! reflectivity

  REAL   :: lonMin,latMin,lonMax,latMax
  REAL*8 :: dlon,dlat
  integer :: height
  integer,parameter :: maxMosaiclvl=33
  integer :: levelheight(maxMosaiclvl)
  data levelheight /500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500,&
                  2750,3000,3500, 4000,4500,5000,5500,6000,6500,7000,&
                  7500,8000,8500,9000,10000,11000,12000,13000,14000, &
                  15000,16000,17000,18000,19000/
                   
!
!  4 Tile binary format
!
  integer           :: ntot, ntot2d, mt
  integer*4         :: nx,ny,nz
  integer*4         :: yr, mo, da, hr, mn, sc
  real*8            :: rdx,rdy
  real*4            :: rdx4,rdy4
  real              :: rlatmax,rlonmin
  integer*4         :: var_scale

  integer*2, dimension(:),   allocatable  :: var
!
!
!  namelist files
!
  INTEGER(i_kind)  ::  tversion
  character*10 :: analysis_time
  CHARACTER*180   dataPath
  namelist/setup/ tversion,analysis_time,dataPath
  integer(i_kind)  ::  idate
!
!
!  ** misc
      
  real        ::rix  ! x-grid coordinate (grid units)
  real        ::riy  ! y-grid coordinate (grid units)
  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain
  logical     :: fileexist

  integer i,j,k,itype,iymdh,ier,jret,ifn,kk
  integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

  integer :: NCID

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp,ipp1,jpp1
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4,refl_ltng

  INTEGER(i_kind)  ::  maxlvl, nlevel,worklevel
  INTEGER(i_kind)  ::  numlvl,numref
  integer :: status
  REAL ::  rthresh_ref,rthresh_miss

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  if(mype==0) write(*,*) mype, 'deal with mosaic'

  open(15, file='mosaic.namelist')
    read(15,setup)
  close(15)

  read(analysis_time,'(I10)') idate
  if(mype==0) write(6,*) 'cycle time is :', idate

  if( tversion == 8 .or. tversion == 14) then
     maxlvl = 31
     rthresh_ref=-500.0
     rthresh_miss=-5000.0
  elseif( tversion == 81 ) then
     maxlvl = 31
     rthresh_ref=-90.0
     rthresh_miss=-900.0
  elseif( tversion == 4 ) then
     maxlvl = 33
     rthresh_ref=-500.0
     rthresh_miss=-5000.0
  elseif( tversion == 1 ) then
     maxlvl = 33
     rthresh_ref=-90.0
     rthresh_miss=-900.0
     if(npe < maxlvl) then
        write(*,*) 'npe must larger or equal to maxlvl'
        stop 1234
     endif
  else
     write(*,*) 'unknow tversion !'
     stop 1234
  endif
!
! set geogrid fle name
!
  write(geofile,'(a,a)') './', 'geo_em.d01.nc'

  if(mype==0) write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  if(mype==0) write(*,*) 'NLON,NLAT',NLON,NLAT
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)
!
  mypeLocal=mype+1
  if ( tversion == 1 ) then
     open(10,file='filelist_mrms',form='formatted',err=300)
     do n=1,200
        read(10,'(a)',err=200,end=400) filenameall(n)
     enddo
300  write(6,*) 'read_grib2 open filelist_mrms failed ',mype
     stop(555)
200  write(6,*) 'read_grib2 read msmr file failed ',n,mype
     stop(555)
400  nlevel=n-1
     close(10)
     if(nlevel .gt. maxlvl) then
        write(*,*) 'vertical level is too large:',nlevel,maxlvl
        stop 666
     endif
     if(mypeLocal <= npe) then
        mosaicfile=trim(filenameall(mypeLocal))
        write(*,*) 'process level:',mypeLocal,trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe < nlevel ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  elseif(tversion==81) then
     if(mypeLocal <= 8) then
        write(mosaicfile,'(a,a,I1)') trim(dataPath), 'mosaic_t',mypeLocal
        write(*,*) 'process tile:',trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe <8 ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  else
     if(mypeLocal <= tversion) then
        write(mosaicfile,'(a,a,I1)') trim(dataPath), 'mosaic_t',mypeLocal
        write(*,*) 'process tile:',trim(mosaicfile)
     else
        mosaicfile=''
     endif
     if(npe < tversion ) then
        write(6,*) 'Error, need more cores to run',npe
        stop 234
     endif
  endif
!
!   deal with certain tile
!
  fileexist=.false.
   
  if( tversion == 8 .or. tversion == 14) then
     call ifexist_file(mosaicfile,STATUS)
     fileexist=STATUS .EQ. NF_NOERR
  elseif( tversion == 81) then
     inquire(file=trim(mosaicfile),exist=fileexist)
     if(mypeLocal > 8) fileexist=.false.
  elseif(tversion == 4) then
     open(99,file=trim(mosaicfile),form='unformatted',access='direct',&
             recl=6*4,status='old',err=225)
        rewind(99)
        read(99,rec=1,err=225) yr, mo, da, hr, mn, sc
        fileexist=.true.
225     continue
     close(99)
  elseif(tversion == 1) then
     inquire(file=trim(mosaicfile),exist=fileexist)
     if(mypeLocal > nlevel) fileexist=.false.
  endif

  if(fileexist) then
      IF( tversion == 14 ) then
         call GET_DIM_ATT_Mosaic(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
         var_scale=10
      ELSEIF( tversion == 8 ) then
         call GET_DIM_ATT_Mosaic8(mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,dlon,dlat)
         var_scale=10
      ELSEIF( tversion == 81 ) then
         call read_ncep_binary_head(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   lonMin,latMin,lonMax,latMax,rdx4,rdy4)
         nx=mscNlon
         ny=mscNlat
         nz=mscNlev
         dlon=rdx4
         dlat=rdy4
         var_scale=1
      ELSEIF( tversion == 4 ) then
         call read_head_Mosaic4(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy,var_scale)
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)
      ELSEIF( tversion == 1 ) then
         call read_grib2_head(mosaicfile,nx,ny,nz,rlonmin,rlatmax,&
                   rdx,rdy)
         var_scale=1
         mscNlon=nx
         mscNlat=ny
         mscNlev=nz
         dlon=rdx
         dlat=rdy
         lonMin=rlonmin
         lonMax=lonMin+dlon*(mscNlon-1)
         latMax=rlatmax
         latMin=latMax-dlat*(mscNlat-1)
      ELSE
         write(*,*) ' unknown tile version !!!'
         stop 123
      ENDIF
      if(mype==0) then
            write(*,*) 'mscNlon,mscNlat,mscNlev'
            write(*,*) mscNlon,mscNlat,mscNlev
            write(*,*) 'dlon,dlat,lonMin,lonMax,latMax,latMin'
            write(*,*) dlon,dlat,lonMin,lonMax,latMax,latMin
      endif

      if( maxlvl == mscNlev .or. maxlvl >= nlevel ) then
         allocate(ref3d(nlon,nlat,maxlvl))
      else
         write(*,*) 'Wrong vertical layers:', maxlvl, mscNlev,nlevel
         stop 1234
      endif
      ref3d=-999.0

      allocate(msclon(mscNlon))
      allocate(msclat(mscNlat))
      allocate(msclev(mscNlev))
      allocate(mscValue(mscNlon,mscNlat))

      DO i=1,mscNlon
         msclon(i)=lonMin+(i-1)*dlon
      ENDDO
      DO i=1,mscNlat
         msclat(i)=latMin+(i-1)*dlat
      ENDDO
!
!  ingest mosaic file and interpolation
! 
      if( tversion == 8 .or. tversion == 14) then
         call OPEN_Mosaic(mosaicfile, NCID)

         if(tversion == 14 ) then
            call Check_DIM_ATT_Mosaic(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
         elseif(tversion == 8 ) then
            call Check_DIM_ATT_Mosaic8(NCID,mscNlon,mscNlat,mscNlev,  &
               lonMin,latMin,lonMax,latMax,dlon,dlat)
         endif
         write(*,*) mscNlon,mscNlat,mscNlev
         write(*,*) 'Area of tile=',lonMin,latMin,lonMax,latMax,dlon,dlat

      elseif(tversion == 81) then
          allocate(mscValue3d(mscNlon,mscNlat,mscNlev))
          call read_ncep_binary_value(mype,mosaicfile,mscNlon,mscNlat,mscNlev, &
                   mscValue3d)
      elseif(tversion == 4) then
         ntot = nx*ny*nz
         allocate(var(ntot))
         call read_data_Mosaic4(mosaicfile,ntot,var)
      elseif(tversion == 1) then
         ntot = nx*ny*nz
         call read_grib2_sngle(mosaicfile,ntot,height,mscValue)
!         write(*,*) 'height,max,min',height,maxval(mscValue),minval(mscValue)
         if(levelheight(mypeLocal) .eq. height) then
            worklevel=mypeLocal
         else
            worklevel=0
            do k=1,maxMosaiclvl
               if (levelheight(k) .eq. height) worklevel=k
            enddo
            if(worklevel==0) then
               write(6,*) 'Error, cannot find working level', &
                         mypeLocal,levelheight(mypeLocal), height
               stop 12345
            else
               write(6,*) 'Find new level for core ',mypeLocal,' new level is',worklevel   
            endif
         endif
      else
         write(*,*) 'unknown type'
         stop 1234
      endif
  endif
  call mpi_barrier(MPI_COMM_WORLD,ierror)
!  stop 999
  if(fileexist) then
!
      if(tversion == 1) mscNlev=1
      DO k=1, mscNlev
!          if(tversion > 1) write(*,*) mype, 'deal with level:', k,mscNlon,mscNlat
          if( tversion == 8 .or. tversion == 14) then
             call  GET_Mosaic_sngl_Mosaic(NCID,mscNlon,mscNlat,k,mscValue)
          elseif(tversion == 81) then
             do j=1,ny
             do i=1,nx
                mscValue(i,j) = mscValue3d(i,j,k)
             enddo
             enddo
          elseif(tversion == 4) then
             ntot2d=nx*ny*(k-1)
             do j=1,ny
             do i=1,nx
                mscValue(i,j) = var(ntot2d+(j-1)*nx+i)
             enddo
             enddo
          elseif(tversion == 1) then
             write(*,*) 'level max min height',mypeLocal,maxval(mscValue),minval(mscValue),height
          endif
          DO j=1,nlat
          DO i=1,nlon
             rlat=ylat(i,j)
             rlon=xlon(i,j)

             if(tversion == 14 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(rlat-latMin)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             elseif(tversion == 8 .or. tversion == 81) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(latMax-rlat)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp 
             elseif(tversion == 4 ) then
               rip=(rlon-lonMin)/dlon+1
               rjp=(rlat-latMin)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             elseif(tversion == 1 ) then
               if(rlon<0.0) rlon=360.0+rlon
               rip=(rlon-lonMin)/dlon+1
               rjp=(latMax-rlat)/dlat+1
               ip=int(rip)
               jp=int(rjp)
               dip=rip-ip
               djp=rjp-jp
             else
               write(*,*) ' Unknown Mosaic format !!'
               stop 123
             endif
             if( ip >= 1 .and. ip <= mscNlon ) then
             if( jp >= 1 .and. jp <= mscNlat ) then
! inside mosaic domain
               ipp1=min(ip+1,mscNlon)
               jpp1=min(jp+1,mscNlat)
               w1=(1.0-dip)*(1.0-djp)
               w2=dip*(1.0-djp)
               w3=dip*djp
               w4=(1.0-dip)*djp
               ref1=mscValue(ip,jp)
               ref2=mscValue(ipp1,jp)
               ref3=mscValue(ipp1,jpp1)
               ref4=mscValue(ip,jpp1)
               kk=k
               if(tversion == 1) kk=worklevel
               if(ref1 > rthresh_ref .and. ref2 > rthresh_ref .and.  &
                  ref3 > rthresh_ref .and. ref4 > rthresh_ref ) then
                  ref3d(i,j,kk)=(ref1*w1+ref2*w2+ref3*w3+ref4*w4)/float(var_scale)
               elseif(ref1 > rthresh_miss .and. ref2 > rthresh_miss .and.  &
                  ref3 > rthresh_miss .and. ref4 > rthresh_miss ) then
                  ref3d(i,j,kk)=-99.0   ! clear
               else
                  ref3d(i,j,kk)=-999.0  ! no observation
               endif
             endif
             endif
          ENDDO
          ENDDO
      ENDDO  ! mscNlev

      if( tversion == 8 .or. tversion == 14) then
         call CLOSE_Mosaic(NCID)
      endif
      if(tversion == 4)   deallocate(var)
      if(tversion == 81)   deallocate(mscValue3d)

      deallocate(msclon)
      deallocate(msclat)
      deallocate(msclev)
      deallocate(mscValue)
   else
      allocate(ref3d(nlon,nlat,maxlvl))
      ref3d=-999.0
      write(*,*) trim(mosaicfile), '   does not exist!!!'
   ENDIF

   call mpi_barrier(MPI_COMM_WORLD,ierror)
!
!  collect data from all processes to root (0)
!
   if(mype==0) then
     allocate( ref0(nlon,nlat,maxlvl) )
     allocate( maxref(nlon,nlat) )
     allocate( imaxref(nlon,nlat) )
   endif
   call MPI_REDUCE(ref3d, ref0, nlon*nlat*maxlvl, MPI_REAL, MPI_MAX, 0, &
                     MPI_COMM_WORLD, ierror)
   deallocate(ref3d)
!
  if(mype==0) then
    OPEN(10,file='./'//'RefInGSI3D.dat',form='unformatted')
     write(10) maxlvl,nlon,nlat
     write(10) ref0
    close(10)
        DO k=1,maxlvl
           write(*,*) k,maxval(ref0(:,:,k)),minval(ref0(:,:,k))
        ENDDO
  endif

  if(mype==0 .and. 1==1) then
!
    allocate(ref3d_column(maxlvl+2,nlon*nlat))
    ref3d_column=-999.0
    numref=0
!    DO j=1,nlat
!    DO i=1,nlon
    DO j=2,nlat-1
    DO i=2,nlon-1
      numlvl=0
      DO k=1,maxlvl
        if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
      ENDDO
      if(numlvl > 0 ) then
        numref=numref+1
        ref3d_column(1,numref)=float(i)
        ref3d_column(2,numref)=float(j)
        DO k=1,maxlvl
           ref3d_column(2+k,numref)=ref0(i,j,k)
        ENDDO
      endif
    ENDDO
    ENDDO

    write(*,*) 'Dump out results', numref, 'out of', nlon*nlat
    OPEN(10,file='./'//'RefInGSI.dat',form='unformatted')
     write(10) maxlvl,nlon,nlat,numref,1,2
     write(10) ((ref3d_column(k,i),k=1,maxlvl+2),i=1,numref)
    close(10)
  
    write(*,*) 'Start write_bufr_nsslref'
    call write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
  endif

  call MPI_FINALIZE(ierror)
!
end program process_NSSL_mosaic
