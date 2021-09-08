!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module gocart_backgr
!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 5 

integer, parameter ::         &
 DMS		  =1  &
,EROD		  =2  &
,H2O2		  =3  & 
,OH		  =4  &
,NO3		  =5  

!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                          & 
  'DMS  '&
, 'EROD '&
, 'H2O2 '&
, 'OH   '&
, 'NO3  '&
/)
integer,dimension(nspecies), parameter :: nlevels_netcdf= &
(/12 &  !12 months
 ,3  &  ! 3 layers
 ,55 &  ! 55 vertical levels
 ,55 &
 ,55 &
/)
real, dimension(55) :: lev


character(LEN=25),dimension(nspecies),parameter :: netcdf_spc_name= &
! '1234567890123456789012345'
(/                          & 
  'DMSO               '&
, 'EROD               '&
, 'h2o2               '&
, 'oh                 '&
, 'no3                '&
/)

!REAL,PARAMETER,DIMENSION(nspecies) :: convert_to_kg_per_day=(/&
!    1.e6/365.  ,   & ! OC  => Gg/year per grid box = 1.e6/365. kg/day
!    1.e6/365.  ,   & ! BC  => Gg/year per grid box = 1.e6/365. kg/day
!    1./365.    ,   & ! SO2 => kg/year per grid box = 1./365.   kg/day
!    1.e6/365.  ,   & ! BC  => Gg/year per grid box = 1.e6/365. kg/day
!    1./365.        & ! SO2 => kg/year per grid box = 1./365.   kg/day
!/)
!---------------------------------------------------------------------------

!---------------------------------------------------------------------------

  type gocart_bg_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type gocart_bg_vars

  type (gocart_bg_vars), allocatable :: gocart_bg_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_gocart_bg(gocart_bg,n1,n2,n3)

    implicit none

    type (gocart_bg_vars),dimension(nspecies)  :: gocart_bg
    integer,intent(in) :: n1,n2,n3
    integer ispc, max_nlevels_netcdf
   
!    max_nlevels_netcdf = max(nlevels_netcdf)
   
    do ispc=1,nspecies
     allocate (gocart_bg(ispc)%src(n1,n2,n3))
     gocart_bg(ispc)%src = 0.
    enddo

    return
  end subroutine alloc_gocart_bg

  !---------------------------------------------------------------

  subroutine nullify_gocart_bg(gocart_bg)

    implicit none

    type (gocart_bg_vars),dimension(nspecies)  :: gocart_bg
    integer ispc

    do ispc=1,nspecies
       if (associated(gocart_bg(ispc)%src))    nullify (gocart_bg(ispc)%src)
    enddo

    return
  end subroutine nullify_gocart_bg

end module gocart_backgr

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_gocart_bg(n1,n2)
    use gocart_backgr
    implicit none
    integer i
    integer, intent(in) :: n1,n2

    if(.not. allocated(gocart_bg_g)) allocate(gocart_bg_g(nspecies))
    !do i=1,nspecies
    ! if(associated(gocart_bg_g(i)%src)) deallocate(gocart_bg_g(i)%src)
    !enddo

    call nullify_gocart_bg(gocart_bg_g(:))      
    call alloc_gocart_bg  (gocart_bg_g(:),n1,n2,maxval(nlevels_netcdf)) 
  end subroutine mem_gocart_bg

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_gocart_bg(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, gocart_bg_data_dir
use gocart_backgr
use mem_grid, only :grid_g
!KY use netcdf
implicit none
!KY include 'netcdf.inc'
integer, parameter ::  nlon = 288, nlat=181, nmonths=12
integer, parameter ::  ndlon = 288, ndlat=181 !ndlon = 1440, ndlat=720
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,stat,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid
character*240 prefix,suffix,filename(nspecies)
character*10 dummy
character*2 cmon(12)
real longgocart_bg(nlon),latgocart_bg(nlat)
real longgocart_dust(ndlon),latgocart_dust(ndlat)
!real, parameter:: ilatn=1.,ilonn=1.25, idlonn = 0.25,idlatn = 0.25
real, parameter:: ilatn=1.,ilonn=1.25, idlonn = 1.25,idlatn = 1.
 data (cmon(i),i=1,12) /'01','02','03','04','05','06','07','08','09','10',  &
                       '11','12'/

!real lat,lon,src_dummy(nlon,nlat,maxval(nlevels_netcdf))
real lat,lon
real         src_dust (ndlon,ndlat,3,1)
real         src_dust_dummy (ndlon,ndlat,55)
! real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(maxval(nlevels_netcdf),nspecies),plat,plon,area
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(55,nspecies),plat,plon,area

real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6370000.

real, allocatable :: src_dummy(:,:,:)
real,allocatable ,save :: RAWsrc(:,:,:,:)
real,allocatable ,save :: RAWdustsrc(:,:,:,:)

integer :: a_i, a_len, a_inds(1), a_err

!--- for gocart_bg there is not monthly variation, 
imonx=1  

!--- lat e lon gocart_bg (corner mais ao sul e mais a oeste)				      
!do k=1,nlon;  longgocart_bg(k)=-180. + (k-1)*ilonn; enddo
!do i=1,nlat;  latgocart_bg(i)= -89.75 + (i-1)*ilatn; enddo
!do k=1,ndlon;  longgocart_dust(k)=-179.875 + (k-1)*idlonn; enddo
!do i=1,ndlat;  latgocart_dust(i)= -89.875 + (i-1)*idlatn; enddo
do k=1,nlon;  longgocart_bg(k)=-180. +.5*ilonn + (k-1)*ilonn; enddo
do i=1,nlat;  latgocart_bg(i)= -90. + .5*ilatn + (i-1)*ilatn; enddo
do k=1,ndlon;  longgocart_dust(k)=-180. +.5*idlonn + (k-1)*idlonn; enddo
do i=1,ndlat;  latgocart_dust(i)= -90. + .5*idlatn + (i-1)*idlatn; enddo

if( .not. allocated (RAWsrc)) then
!  allocate(  RAWsrc(nlon,nlat,maxval(nlevels_netcdf),nspecies) )
   allocate(  RAWsrc(nlon,nlat,55,nspecies) )
   allocate(  RAWdustsrc(ndlon,ndlat,55,nspecies) )
   RAWsrc= 0.0
ENDIF

!KY if(ng == 1) then  ! need to read just one time

  !- name of dataset
  !KY suffix='.nc'


  !KY do ispc=1,nspecies


    !KY nc=len_trim(spc_name(ispc))


    !print*,'nc=',nc,spc_name(ispc)
    !KY if(trim( spc_name(ispc) ) == 'DMS') prefix= 'dms_data/dms_1x1.25'
    !KY if(trim( spc_name(ispc) ) == 'EROD') prefix= 'erod/GAO_source_3cl'
    !KY if(trim( spc_name(ispc) ) == 'H2O2' .or.&
       !KY trim( spc_name(ispc) ) == 'OH'   .or.&
       !KY trim( spc_name(ispc) ) == 'NO3' ) prefix= 'gocart_bg/gmi_2006'//cmon(IMON)
!KY 


    !KY filename(ispc)=trim(gocart_bg_data_dir)//'/'//trim(prefix)//suffix
    
    !KY print *,'opening   ', trim(filename(ispc)),' - specie= ',trim(spc_name(ispc))
    
    
! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
!   call check( nf_open(TRIM(filename(ispc)), NF_NOWRITE, ncid) )
    !KY stat= nf_open(TRIM(filename(ispc)), NF_NOWRITE, ncid)
    
!     print*,'1',ncid,trim(netcdf_spc_name(ISPC))
!pause
! Get the varid of the data variable, based on its name.
!   call check( nf_inq_varid(ncid, trim(netcdf_spc_name(ispc)), var_id) )
    !KY stat=nf_inq_varid(ncid, trim(netcdf_spc_name(ispc)), var_id)

    
    !KY if(trim( spc_name(ispc) ) == 'EROD') then
!     call check( nf_get_var(ncid, var_id, src_dust  ) )
      !KY stat=nf_get_var(ncid, var_id, src_dust  )
      !KY src_dust_dummy(1:ndlon,1:ndlat,1:3) = src_dust(:,:,1:3,1)
      !KY do i=1,ndlon;do j=1,ndlat
	    !KY RAWdustsrc(i,j,1:nlevels_netcdf(ispc),ispc)=src_dust_dummy(i,j,1:nlevels_netcdf(ispc))
      !KY enddo;enddo      
    !KY else 
      !KY allocate(src_dummy(nlon,nlat,nlevels_netcdf(ispc)))
!     call check( nf_get_var(ncid, var_id, src_dummy  ) )      
      !KY stat=nf_get_var(ncid, var_id, src_dummy  )
      !KY do i=1,nlon;do j=1,nlat
	    !KY RAWsrc(i,j,1:nlevels_netcdf(ispc),ispc)=src_dummy(i,j,1:nlevels_netcdf(ispc))
      !KY enddo;enddo
      !KY deallocate(src_dummy)
    !KY endif

    !-special section for reading 'lev' 
    !KY if(trim( spc_name(ispc) ) == 'H2O2') then
!      call check( nf_inq_varid(ncid, 'lev', var_id) )
       !KY stat=nf_inq_varid(ncid, 'lev', var_id)
       !  call check( nf_get_var(ncid, var_id,lev ) )

      ! read the values for lev one layer at a time
      !do a_i = 1,55      ! nlevels_netcdf(ispc)
       !KY do a_i = 1,maxval(nlevels_netcdf)
          !KY a_inds(1) = a_i
!         call check(nf_get_var(ncid, var_id, lev(a_i), a_inds ))
          !KY stat=nf_get_var(ncid, var_id, lev(a_i), a_inds )
          !print*, 'NetCDF Error message: ', nf_strerror( a_err )
          !print*, 'ind = ', a_i, 'lev(ind) = ', lev(a_i)
       !KY end do
    !KY endif

! Close the file, freeing all resources.
!     call check( nf_close(ncid) )
      !KY call ncclos(ncid,stat)
  !KY enddo ! nspecies


! convert to mass/area (m^2) , only EROD
  !KY do j=1,nlat
    !KY area=cos(latgocart_dust(j)*pi180)* (r_earth**2) * idlatn * idlonn * pi180**2
 
    !KY do i=1,nlon   
      !KY do ispc=erod,erod
           !KY RAWdustsrc(i,j,1:nlevels_netcdf(ispc),ispc)= RAWdustsrc(i,j,1:nlevels_netcdf(ispc),ispc)/area
      !KY enddo
    !KY enddo
  !KY enddo 


!KY endif ! ng==1

!--- performs the interpolation to model grid box
do i=1,n1
  do j=1,n2

    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longgocart_bg,latgocart_bg &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

!   print *,'1', i,j,n1,n2,ic,jc,nlat,nlon,maxval(nlevels_netcdf),ilatn,ilonn,imon,nmonths,nspecies

    call interpol2_gocart_bg(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,maxval(nlevels_netcdf)&
                   ,ilatn,ilonn ,imon,nmonths,nspecies,RAWsrc,tx(:,:))
!   call interpol2_gocart_bg(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,55                   &
!                  ,ilatn,ilonn ,imon,nmonths,nspecies,RAWsrc,tx(:,:))
! 
! TX is the value interpolated to the model grid box.
!-obs: convert to kg / day
    do ispc = 1, nspecies 
     if(ispc .ne. erod) then
     gocart_bg_g(ispc)%src(i,j,1:nlevels_netcdf(ispc))=TX(1:nlevels_netcdf(ispc),ispc) 
     endif
    enddo

  enddo
enddo 

! print *,'interpolate dust to grid'
!--- performs the interpolation to model grid box
do i=1,n1
  do j=1,n2

    call get_index1(i,j,ndlon,ndlat,n1,n2,rlat,rlon,longgocart_dust,latgocart_dust &
                 ,idlatn, idlonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

! print *,'grid point',ic,jc
    call interpol2_gocart_bg(i,j,n1,n2,rlon,rlat,ic,jc,ndlat,ndlon,maxval(nlevels_netcdf) &
                   ,idlatn,idlonn ,imon,nmonths,nspecies,RAWdustsrc,tx(:,:))
! TX is the value interpolated to the model grid box.
!-obs: convert to kg / day
    do ispc = erod, erod 
     gocart_bg_g(ispc)%src(i,j,1:nlevels_netcdf(ispc))=TX(1:nlevels_netcdf(ispc),ispc) 
    enddo

  enddo
enddo 


!print*,' MAX=',maxval(gocart_bg_g(EROD)%src(:,:,1)),maxval(gocart_bg_g(EROD)%src(:,:,1)),maxval(gocart_bg_g(SO2)%src(:,:,1))

if(ng==ngrids) then
   deallocate (RAWsrc)
   deallocate (RAWdustsrc)
endif


end subroutine read_gocart_bg
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_gocart_bg_indentity(spc_name,ident)
!use chem1_list
use gocart_backgr, only :  gocart_bg_nspecies=>nspecies&
                           ,gocart_bg_spc_name=>spc_name!&
!, gocart_bg_OC     => OC	  &
!, gocart_bg_BC    => BC	  &    
!, gocart_bg_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,gocart_bg_nspecies
  ident=-1
  if(spc_name == gocart_bg_spc_name(isp)) then
      print*,'==>gocart_bg found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of gocart_bg'
!stop 444
end subroutine get_gocart_bg_indentity
!---------------------------------------------------------------
subroutine interpol2_gocart_bg(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,nlev,ilatn,ilonn&
	                ,imon,nmonths,nspecies,RAWsrc,tx)
use grid_dims_out, only: grid_type
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,imon,nmonths,nspecies,ispc,nlev
real, dimension(n1,n2) :: rlat,rlon
real, dimension(nlon,nlat,nlev,nspecies) :: RAWsrc
real ilatn,ilonn,tx(nlev,nspecies),delta
!-local var
real dlonr,dlatr,usdum
integer qi1,qi2,qj1,qj2,ncount,ii,jj

if(grid_type == 'fim') then

 ncount = 0
 dlonr = 0.
 dlatr = 0.
 do ii=1,n1-1
     ncount = ncount+1
     dlonr= dlonr + rlon(ii+1,1)-rlon(ii,1)
     dlatr= dlatr + rlat(ii+1,1)-rlat(ii,1)
 enddo
 dlonr = 0.5*dlonr/(float(ncount) + 1.E-10)
 dlatr = 0.5*dlatr/(float(ncount) + 1.E-10)
elseif(grid_type == 'mercator') then
 dlonr=0.5*(rlon(2,j)-rlon(1,j))
 dlatr=0.5*(rlat(i,n2)-rlat(i,1))/float(n2-1)
else    	     
 delta = .01*(int(100.*rlon(n1,j))-int(100.*rlon(1,j)))
 if (delta .gt. rlon(2,j)-rlon(1,j)) then            
   dlonr=0.5*(rlon(n1,j)-rlon(1,j))/float(n1-1)
 else
   dlonr=180./float(n1-1)
 endif
 dlatr=0.5*(rlat(i,n2)-rlat(i,1))/float(n2-1)
endif

qi1=int(dlonr/ilonn+0.5)
qi2=int(dlonr/ilonn+0.5)
qj1=int(dlatr/ilatn+0.5)
qj2=int(dlatr/ilatn+0.5)
 
ncount = 0
TX(:,:)  = 0.

do jj = min(max(1,jc-qj1),nlat),min(nlat,jc+qj2)
   do ii = min(max(1,ic-qi1),nlon),min(nlon,ic+qi2)   
   
     	    ncount = ncount + 1
     	    TX(1:nlev,:) = TX(1:nlev,:) + RAWsrc(ii,jj,1:nlev,:)  
   enddo
enddo
TX(1:nlev,:) = TX(1:nlev,:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol2_gocart_bg

