subroutine update_SST_netcdf_mass (sstRR, glat, glon, nlon, nlat, xland, vegtyp, ilake,iice)
!$$$  documentation block
!                .      .    .                                       .
!   update_SST_netcdf_mass: read SST from wrf mass netcdf old background file
!           and update SST in wrf mass background file
!   prgmmr: Ming Hu                 date: 2008-02-25
!   updates: Tanya Smirnova         date: 2010-10-11
!
! program history log:
!
!
!   input argument list:
!       sstRR: sst
!       nlon:  x dimension
!       nlat:  Y dimension
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind
  implicit none

  INCLUDE 'netcdf.inc'

  integer, parameter :: WRF_INTEGER = 106
!
  integer :: nlon, nlat, ilake, iice
  real  :: sstRR(nlon,nlat)
  real  :: glat(nlon,nlat)
  real  :: glon(nlon,nlat)
  real  :: xland(nlon,nlat)
  real  :: vegtyp(nlon,nlat)
  real  :: laked(nlon,nlat)

! Declare local parameters

  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  integer(i_kind)            :: dh1
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering
  character (len=31) :: name,name1,name2,name3,name4,name5
  
  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo
  
  integer(i_kind) :: l, n
  
  integer(i_kind) :: ierr, ier, Status, Status_next_time

! rmse stuff
  
  character (len=31) :: rmse_var
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field2(:,:)
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::surftemp(:,:)
  real(r_single),allocatable::temp2m(:,:)
  real(r_single),allocatable::sst(:,:)
  real(r_single),allocatable::lu_index(:,:)
  real(r_single),allocatable::landmask_soilmoisture1(:,:)
  real(r_single),allocatable::lakemask(:,:)

  integer(i_kind) wrf_real

  real(r_single)    :: time, time1, time2
  real(r_single)    :: a, b
  integer(i_kind), dimension(4)  :: start_index1,  end_index1

! Lakes from RUC model
! -- Great Salt Lake lake surface temps
        REAL salt_lake_lst (13)
        data salt_lake_lst &
        /1.,3.,6.,13.,17.,20.,26.,25.,20.,14.,9.,3.,1./
! -- Salton Sea - California
        REAL salton_lst (13)
        data salton_lst &
        / 12.8, 12.8, 17.2, &
         21.1, 24.4, 26.7,  &
         30.0, 31.7, 29.4,  &
         25.0, 20.6, 15.0,  &
         12.8/
! -- Lake Champlain - Vermont
        REAL champ_lst (13)
        data champ_lst&
       /  1.3,  0.6,  1.0,  &
          3.0,  7.5, 15.5,  &
         20.5, 21.8, 18.2,  &
         13.0,  8.2,  4.5,  &
          1.3/

        real xc1,yc1, xc2,yc2
        integer isup,jsup, iwin,jwin, isalton,jsalton

      integer julm(13)
      data julm/0,31,59,90,120,151,181,212,243,273,304,334,365/

        INTEGER  mon1,mon2,day1,day2,juld
        real rday,wght1,wght2
!20aug18 - lake model
        integer :: NCID,in_SF_LAKE_PHYSICS


!
  wrf_real=104

!   transfer code from diffwrf for converting netcdf wrf nmm restart file
!      to temporary binary format

  call ext_ncd_ioinit(sysdepinfo,status)
  
  flnm1='wrf_inout'        ! for full cycle

  in_SF_LAKE_PHYSICS=0
!
  STATUS=NF_OPEN(trim(flnm1),0,NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS = NF_GET_ATT_INT (NCID, NF_GLOBAL, 'SF_LAKE_PHYSICS',in_SF_LAKE_PHYSICS)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  STATUS=NF_CLOSE(NCID)
  IF (STATUS .NE. NF_NOERR) CALL HANDLE_ERR_M(STATUS)
  write(*,*) 'in_SF_LAKE_PHYSICS=',in_SF_LAKE_PHYSICS
  if ( in_SF_LAKE_PHYSICS == 1 ) write(*,*) 'CLM Lake model is turned on, no SST update for lakes'

  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'CONVERT_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74 
  endif

!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' Skin temp data from background file at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info

  rmse_var='T'

  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE

  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE

  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(surftemp(nlon_regional,nlat_regional))
  allocate(temp2m(nlon_regional,nlat_regional))
  allocate(sst(nlon_regional,nlat_regional))
  allocate(lu_index(nlon_regional,nlat_regional))
  allocate(landmask_soilmoisture1(nlon_regional,nlat_regional))
  allocate(lakemask(nlon_regional,nlat_regional))

  allocate(field2(nlon_regional,nlat_regional))
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  
!
  write(6,*) '================================================='
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE
  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  surftemp=field2(:,:)
  write(6,*)' max,min bck skin temp (K)=',maxval(surftemp),minval(surftemp)
       write(6,*)'background skin temp(170,170)', surftemp(170,170)
       write(6,*)'new  sstRR(170,170)', sstRR(170,170)
       write(6,*)'Winnipeg skin temp(516,412)', surftemp(516,412)
       write(6,*)'Winnipeg sstRR(516,412)', sstRR(516,412)
!
  write(6,*) '================================================='
  rmse_var='T2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE
  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  temp2m=field2(:,:)
  write(6,*)' max,min bck 2m temp (K)=',maxval(temp2m),minval(temp2m)
       write(6,*)'background t2 temp(292,258)', temp2m(292,258)
       write(6,*)'new  sstRR(170,170)', sstRR(292,258)
!
  write(6,*) '================================================='
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE
  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  sst=field2(:,:)
  write(6,*)' max,min bck sst (K)=',maxval(sst),minval(sst)
       write(6,*)'background sst(170,170)', sst(170,170)
       write(6,*)'new  sstRR(170,170)', sstRR(170,170)
       write(6,*)'Winnipeg sst(516,412)', sst(516,412)
!
  write(6,*) '================================================='
  rmse_var='LAKE_DEPTH'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )                !DEDE
  write(6,*)' dh1  = ',dh1         !DEDE
  write(6,*)'rmse_var = ',trim(rmse_var)
  write(6,*)'ndim1 = ',ndim1
  write(6,*)'ordering = ',trim(ordering)
  write(6,*)'staggering = ',trim(staggering)
  write(6,*)'start_index = ',start_index
  write(6,*)'end_index = ',end_index
  write(6,*)'WrfType = ',WrfType
  write(6,*)'ierr  = ',ierr   !DEDE
  end_index(3) =1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  laked=field2(:,:)
  write(6,*)' max,min bck laked (K)=',maxval(laked),minval(laked)
  write(6,*)'laked(170,170)', laked(170,170)
  write(6,*)'Winnipeg laked(516,412)', laked(516,412)
!
  write(6,*) '================================================='
  rmse_var='LU_INDEX'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min LU_INDEX=',maxval(field2),minval(field2)
  lu_index=field2
!
  write(6,*) '================================================='
  rmse_var='LAKEMASK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  write(6,*)' max,min LAKEMASK=',maxval(field2),minval(field2)
  write(6,*)'Winnipeg lakemask(516,412)', field2(516,412)
  lakemask=field2
!
  write(6,*) '================================================='
  rmse_var='SMOIS'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1 
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index    
  deallocate(field3)
  nsig_regional=end_index(3)
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  do k=1,nsig_regional
    write(6,*)' max,min SMOIS=',k, maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  landmask_soilmoisture1=field3(:,:,1)  ! use soil mositure to find water 

  call ext_ncd_ioclose(dh1, Status)

! Compute weight for the current date
       juld = julm(imonth) + iday
       if(juld.le.15) juld=juld+365

       mon2 = imonth
       if(iday.gt.15) mon2 = mon2 + 1
       if(mon2.eq.1) mon2=13
       mon1=mon2-1
! **** Assume data valid at 15th of month
       day2=julm(mon2)+15
       day1=julm(mon1)+15
       rday=juld
       wght1=(day2-rday)/float(day2-day1)
       wght2=(rday-day1)/float(day2-day1)
       write(6,*)'Date weights =',wght1,wght2

!
!  update skin temperature over water
!
if(1==1) then  ! turn off , use GFS SST
! find i,j for a point in northern Lake Superior
  DO J=1,nlat
  DO I=1,nlon
   if((glat(i,j)>48.4 .and. glat(i,j)<49.6) .and. (glon(i,j)<-87.9 .and. glon(i,j)>-88.1)) then
     isup=i
     jsup=j
     print *,' Lake Superior --> i,j,glat(i,j),glon(i,j)',i,j,glat(i,j),glon(i,j), &
     'vegtyp(i,j)=',vegtyp(i,j),'lu_index(i,j)',lu_index(i,j),xland(i,j)
     goto 99
   endif
  ENDDO
  ENDDO

99  continue

! find i,j for a point in northern Lake Winnipeg
  DO J=1,nlat
  DO I=1,nlon
   if((glat(i,j)>53.3 .and. glat(i,j)<53.7) .and. (glon(i,j)<-98.3 .and.  glon(i,j)>-98.7)) then
     iwin=i
     jwin=j
     print *,' Lake Winnipeg --> i,j,glat(i,j),glon(i,j)',i,j,glat(i,j),glon(i,j), &
     'vegtyp(i,j)=',vegtyp(i,j),'lu_index(i,j)',lu_index(i,j),xland(i,j)
     goto 999
   endif
  ENDDO
  ENDDO

999  continue

       
  DO J=1,nlat
  DO I=1,nlon
!    if( abs(landmask_soilmoisture1(i,j) -1.0) < 0.00001 ) then    ! water
    if( xland(i,j) < 0.00001 ) then    ! water, could be sea ice
!        if(abs(lu_index(i,j)- 24) > 0.01) then  !USGS ice = 24
        if(abs(lu_index(i,j) - iice) > 0.01) then    ! MODIS ice = 15
! only unfrozen water points (sea or lakes)
!
!     if(vegtyp(i,j)==ilake .and. (sstRR(i,j)-temp2m(i,j)).lt.-5) then
! correct lake temperature when it is too cold, excluding the Great Lakes
!        IF (.not.(GLAT(i,j).LT.50..AND.GLAT(i,j).GT.40. .AND.   &
!        glon(i,j).LT.-74..AND.glon(i,j).GT.-94.)) then
!          print *,'corrected lake sstRR at i,j =, sstRR,temp2m,vegtyp',i,j,sstRR(i,j),temp2m(i,j)
!              sstRR(i,j)=max(273.15,sstRR(i,j) + 0.75*(temp2m(i,j)-sstRR(i,j)))
!        ENDIF  ! no Great Lakes
!     endif  ! lakes correction
    
       if(in_SF_LAKE_PHYSICS == 0 .and. (vegtyp(i,j)==ilake .or. laked(i,j) .ne. 10.)) then ! the fill value for lake depth is 10.
! --- CLM lake model is off, use climatology for several lakes
! --- Great Salt Lake, Utah Lake -- Utah
            if (glat(i,j).gt.39.5 .and. glat(i,j).lt.42. .and.  &
               glon(i,j).gt.-114..and. glon(i,j).lt.-111.) then
           write(6,*)'Global data Salt Lake temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*salt_lake_lst(mon1)  &
                       +wght2*salt_lake_lst(mon2)
            write(6,*)'Climatology Salt Lake temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
            end if

! --- Salton Sea -- California
            if (glat(i,j).gt.33. .and. glat(i,j).lt.33.7 .and.  &
                glon(i,j).gt.-116.3 .and. glon(i,j).lt.-115.3) then
            write(6,*)'Global data Salton Sea temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*salton_lst(mon1)  &  
                       +wght2*salton_lst(mon2)
            write(6,*)'Climatology Salton Sea temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
              isalton=i
              jsalton=j
            end if

! --- Lake Champlain -- Vermont
            if (glat(i,j).gt.44. .and. glat(i,j).lt.45.2 .and.  &
               glon(i,j).gt.-74. .and. glon(i,j).lt.-73.) then
            write(6,*)'Global data Lake Champlain temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*champ_lst(mon1)  &
                       +wght2*champ_lst(mon2)
            write(6,*)'Climatology Lake Champlain temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
            end if
! --- For Lake Nipigon, use point for n. Lake Superior
!   -- Lake Nipigon is deep!
            if (glat(i,j).gt.49. .and. glat(i,j).lt.51. .and. &
               glon(i,j).gt.-90. .and. glon(i,j).lt.-87.) then
               write(6,*)'Global data Lake Nipigon temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(isup,jsup)
                write(6,*)'Lake Nipigon temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

     if(1 == 2) then
! --- For Lake of the Woods and other
!      Minnesota lakes, use point for n. Lake Winnipeg
!    -- These lakes are NOT DEEP!
            if (glat(i,j).gt.46. .and. glat(i,j).lt.50. .and.  &
               glon(i,j).gt.-96. .and. glon(i,j).lt.-93.) then
                write(6,*)'Global data Minnesota lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
                write(6,*)'Minnesota lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

! --- For Canadian lakes, including Winnipeg, Manitoba, Winnipegosis,
!      use point for n. Lake Winnipeg
!    -- These lakes are NOT DEEP!
            if (glat(i,j).gt.50. .and. glat(i,j).lt.68. .and.  &
               glon(i,j).gt.-148. .and. glon(i,j).lt.-48.) then
                write(6,*)'Global data Canadian lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
                write(6,*)'Canadian lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if
! --- For lakes in Washington, Oregon, Nevada
!      use point for n. Lake Winnipeg (?????)
!    -- These lakes are NOT DEEP!
            if (glat(i,j) > 33.8 .and. glat(i,j) < 50. .and.  &
               glon(i,j) < -114. ) then
                write(6,*)'Global data US west lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
!                sstRR(i,j) = sstRR(isalton,jsalton)
                write(6,*)'US west lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

     endif ! 1 == 2
      endif   ! lakes

!-- update skin temp
!-- 22Aug18
! in_SF_LAKE_PHYSICS == 0 --> lakemask == 0, SST is updated at all water points
! in_SF_LAKE_PHYSICS == 1 --> lakemask == 1 where CLM lake model is applied, SST is
! not updated at these points.
       if( lakemask(i,j) == 0.) then
              surftemp(i,j)=sstRR(i,j)
       endif
           if(sstRR(i,j) > 400. .or. sstRR(i,j) < 100. ) then
             print *,'Bad SST at point i,j',i,j,sstRR(i,j)
           endif

        else
! frozen water - MODIS type = 15
      if(in_SF_LAKE_PHYSICS == 0) then
! CLM lake model is turned off
        if(lu_index(i,j) == iice .and. laked(i,j).ne.10. .and. sstRR(i,j) > 273.) then
! -- frozen lakes with CLM lake model turned off.
!           print *,'Ice lake cannnot have SST > 274K'
!           print *,'i,j,sstRR,lu_index,vegtyp =' ,i,j,sstRR(i,j),lu_index(i,j),vegtyp(i,j)
! set skin temp of frozen lakes to 2-m temperature
              sstRR(i,j)= min(273.15,temp2m(i,j))
!update skin temp
              surftemp(i,j)=sstRR(i,j)
        endif
      endif ! in_SF_LAKE_PHYSICS == 0

        endif  ! MODIS ice = 15
    endif  ! water and ice

        sst(i,j)=sstRR(i,j)
  ENDDO
  ENDDO
  write(*,*) 'Skin temperature updated with current SST'
       write(6,*)'updated skin temp(170,170)', surftemp(170,170)
endif
!
!
!           update mass core netcdf file with new SST
!
  write(6,*) ' ============================= '
  write(6,*) ' update SST in background file '
  write(6,*) ' ============================= '
  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'UPDATE_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 75
  endif
     
!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*) ' Update SST in background at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
  if( (nlon_regional .ne. end_index1(1)) .or.    &
      (nlat_regional .ne. end_index1(2)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat=',nlon_regional,nlat_regional
      stop 123
  endif
   
  write(6,*) '================================================='
  field2=surftemp
  write(6,*)' max,min skin temp =',maxval(field2),minval(field2)
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  write(6,*) '================================================='
  field2=sst
  write(6,*)' max,min sst =',maxval(field2),minval(field2)
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  deallocate(field2)

  call ext_ncd_ioclose(dh1, Status)
  
end subroutine update_SST_netcdf_mass

SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
  IF ( level .LE. debug_level ) THEN
    ! old behavior
!      CALL wrf_message( str )
  ENDIF
  write(*,*) 'wrf_debug called !'
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE HANDLE_ERR_M(STATUS)
     INCLUDE 'netcdf.inc'
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
       PRINT *, NF_STRERROR(STATUS)
       STOP 'Stopped'
     ENDIF
END SUBROUTINE HANDLE_ERR_M

