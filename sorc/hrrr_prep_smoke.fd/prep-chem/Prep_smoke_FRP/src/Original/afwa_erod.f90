!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module afwa_erodab
!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 2 

integer, parameter ::         &
 SAND             =1  &
,CLAY     	  =2

!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                          & 
  'SAND '&
, 'CLAY '&
/)
integer,dimension(nspecies), parameter :: nlevels_netcdf= &
(/1 &  !only surface layer
 ,1 &  !only surface layer
/)

character(LEN=25),dimension(nspecies),parameter :: netcdf_spc_name= &
! '1234567890123456789012345'
(/                          & 
  'SAND               '&
, 'CLAY               '&
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

  type afwa_erod_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type afwa_erod_vars

  type (afwa_erod_vars), allocatable :: afwa_erod_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_afwa_erod(afwa_erod,n1,n2,n3)

    implicit none

    type (afwa_erod_vars),dimension(nspecies)  :: afwa_erod
    integer,intent(in) :: n1,n2,n3
    integer ispc, max_nlevels_netcdf
   
!    max_nlevels_netcdf = max(nlevels_netcdf)
   
    do ispc=1,nspecies
     allocate (afwa_erod(ispc)%src(n1,n2,n3))
     afwa_erod(ispc)%src = 0.
    enddo

    return
  end subroutine alloc_afwa_erod

  !---------------------------------------------------------------

  subroutine nullify_afwa_erod(afwa_erod)

    implicit none

    type (afwa_erod_vars),dimension(nspecies)  :: afwa_erod
    integer ispc

    do ispc=1,nspecies
       if (associated(afwa_erod(ispc)%src))    nullify (afwa_erod(ispc)%src)
    enddo

    return
  end subroutine nullify_afwa_erod

end module afwa_erodab

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_afwa_erod(n1,n2)
    use afwa_erodab
    implicit none
    integer i
    integer, intent(in) :: n1,n2

    if(.not. allocated(afwa_erod_g)) allocate(afwa_erod_g(nspecies))
    !do i=1,nspecies
    ! if(associated(afwa_erod_g(i)%src)) deallocate(afwa_erod_g(i)%src)
    !enddo

    call nullify_afwa_erod(afwa_erod_g(:))      
    call alloc_afwa_erod  (afwa_erod_g(:),n1,n2,maxval(nlevels_netcdf)) 
  end subroutine mem_afwa_erod

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_afwa_erod(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland, &
           deltax,deltay,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, afwa_erod_data_dir
use afwa_erodab
use mem_grid, only :grid_g
!use netcdf
implicit none
!include 'netcdf.inc'
! Put 1km binary data into 10km resolution grid of HTAP - then process area
! averaging like HTAP emissions 10/31/17
integer, parameter ::  nlon = 3600, nlat=1800
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,jj,i,k,iread,nc,ilaea,jlaea,stat,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid
character*240 prefix,suffix,filename(nspecies)
character*10 dummy
real longafwa_erod(nlon),latafwa_erod(nlat)
real, parameter:: ilatn=.1,ilonn=.1

!real lat,lon,src_dummy(nlon,nlat,maxval(nlevels_netcdf))
real lat,lon,accum_emission
! real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(maxval(nlevels_netcdf),nspecies),plat,plon,area
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),plat,plon,area

real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6370000.

real, allocatable :: src_dummy(:,:)
real,allocatable ,save :: RAWsrc(:,:,:,:)

integer :: a_i, a_len, a_inds(1), a_err
integer :: LoadFlatDirtData
! Stu variables for HTAP-FV3 option 10/13/17
        integer, allocatable, dimension(:,:) :: htap_fimN
        real, allocatable, dimension(:,:)  :: grid_area
real dlonr,dlatr,d2r,rearth
   rearth=6371.67e3  ! earth radius in meters, need in same units as variable grid_area
   d2r = 4.0*ATAN(1.0)/180.0
   dlonr=d2r*(ilonn)*rearth
   dlatr=d2r*(ilatn)*rearth
 print *,'In read_afwa_erod,n1,n2=',n1,n2
 call flush(6)

!--- for afwa_erod there is not monthly variation, 
imonx=1  

!--- lat e lon afwa_erod (corner mais ao sul e mais a oeste)				      
do k=1,nlon;  longafwa_erod(k)=-180.+.5*ilonn + (k-1)*ilonn; enddo
do i=1,nlat;  latafwa_erod(i)= -90. +.5*ilatn + (i-1)*ilatn; enddo

if( .not. allocated (src_dummy)) then
   allocate(  src_dummy(nlon,nlat) )
   src_dummy= 0.0
ENDIF
if( .not. allocated (RAWsrc)) then
!  allocate(  RAWsrc(nlon,nlat,maxval(nlevels_netcdf),nspecies) )
   allocate(  RAWsrc(nlon,nlat,1,nspecies) )
   RAWsrc= 0.0
ENDIF

if(ng == 1) then  ! need to read just one time

  !- name of dataset
  suffix='.1gd4r'

  do ispc=1,nspecies

    nc=len_trim(spc_name(ispc))
! now read in sand and clay files
!
    if(trim( spc_name(ispc) ) == 'SAND') prefix= 'sand60_1KM'
    if(trim( spc_name(ispc) ) == 'CLAY') prefix= 'clay60_1KM'
    filename(ispc)=trim(afwa_erod_data_dir)//'/'//trim(prefix)//suffix

   print *, 'Getting AFWA ',trim( spc_name(ispc) ), filename(ispc)
   stat=LoadFlatDirtData ( filename(ispc) ,nlon,nlat,src_dummy)
   ! Insert your code here...
   print *,'After read,stat,maxval= ',stat,maxval(src_dummy)

      do i=1,nlon;do j=1,nlat
	    RAWsrc(i,j,1,ispc)=src_dummy(i,j)
      enddo;enddo

  enddo ! nspecies
      deallocate(src_dummy)


! convert to mass/area (m^2) , only EROD
  do j=1,nlat
!   area=cos(latafwa_erod(j)*pi180)* (r_earth**2) * ilatn * ilonn * pi180**2
    area=1.
 
    do i=1,nlon   
      do ispc=1,nspecies
           RAWsrc(i,j,1,ispc)= RAWsrc(i,j,1,ispc)/area
      enddo
    enddo
  enddo 

   if(grid_type == 'fim')then
           if( .not. allocated (htap_fimN)) then
                  allocate(  htap_fimN(nlon,nlat) )
           endif
           open(11,FILE='htapLL_to_fimN.bin',FORM='UNFORMATTED')
           read(11)htap_fimN
           close(11)
           write(6,*)'htapLL_to_fimN.bin opened OK'
           CALL FLUSH(6)
           if( .not. allocated (grid_area)) then
                  allocate(  grid_area(n1,n2) )
                  grid_area = 0.0
           endif
           call get_area_fim(grid_area,n1,n2)
           accum_emission = 0.
           do i=1,n1
           accum_emission = accum_emission + grid_area(i,1)
           enddo
           write(6,*)'After get_area_fim,area sum=',accum_emission
           call flush(6)
    endif

endif ! ng==1

!--- performs the interpolation to model grid box
  if(grid_type == 'fv3')then                        
do i=1,n1
  do j=1,n2
        call grid_htap2fv3(i,j,rlat(i,j),rlon(i,j),n1,n2,nlon,nlat,1,1,nspecies, &
        ilonn,ilatn,latafwa_erod,longafwa_erod,RAWsrc,rland,tx)
! On return I think TX has units of area/area (or fractional area)
    do ispc = 1, nspecies 
     afwa_erod_g(ispc)%src(i,j,1)=TX(ispc) 
    enddo
  enddo
enddo 
  elseif(grid_type == 'fim')then                        
! Note that HTAP grid is used for afwa_erod, so orig fim logic applies 11/4/17
  do jj=1,nlat
  do ii=1,nlon
  i=htap_fimN(ii,jj)
  j=1
!       call grid_htap2fim(i,j,n1,n2,nlon,nlat,htap_fimN,1,1,nspecies, &
!       ilonn,ilatn,latafwa_erod,RAWsrc,grid_area,tx(1:nspecies))

            TX(:) = cos(latafwa_erod(jj)*d2r)*dlonr*dlatr*RAWsrc(ii,jj,1,:)
    do ispc = 1, nspecies 
     afwa_erod_g(ispc)%src(i,j,1)=afwa_erod_g(ispc)%src(i,j,1)+TX(ispc) 
    enddo
  enddo
  enddo
    do ispc = 1, nspecies 
     afwa_erod_g(ispc)%src(:,:,1)=afwa_erod_g(ispc)%src(:,:,1)/grid_area(:,:)
    enddo
  else
    print *,'Only set up for fv3 or fim domains, stopping'
    STOP
!XX    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longafwa_erod,latafwa_erod &
!XX                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)
!XX
!XX!   print *,'1', i,j,n1,n2,ic,jc,nlat,nlon,maxval(nlevels_netcdf),ilatn,ilonn,imon,nmonths,nspecies
!XX
!XX    call interpol2_afwa_erod(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,maxval(nlevels_netcdf)&
!XX                   ,ilatn,ilonn ,imon,nmonths,nspecies,RAWsrc,tx(:,:))
!XX! TX is the value interpolated to the model grid box.
   endif
print*,' MAX,sand,clay=',maxval(afwa_erod_g(SAND)%src(:,:,1)),maxval(afwa_erod_g(CLAY)%src(:,:,1))


if(ng==ngrids) then
   deallocate (RAWsrc)
endif

end subroutine read_afwa_erod
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_afwa_erod_indentity(spc_name,ident)
!use chem1_list
use afwa_erodab, only :  afwa_erod_nspecies=>nspecies&
                           ,afwa_erod_spc_name=>spc_name!&
!, afwa_erod_OC     => OC	  &
!, afwa_erod_BC    => BC	  &    
!, afwa_erod_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,afwa_erod_nspecies
  ident=-1
  if(spc_name == afwa_erod_spc_name(isp)) then
      print*,'==>afwa_erod found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of afwa_erod'
!stop 444
end subroutine get_afwa_erod_indentity
!---------------------------------------------------------------
subroutine interpol2_afwa_erod(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,nlev,ilatn,ilonn&
	                ,imon,nmonths,nspecies,RAWsrc,tx)
use grid_dims_out, only: grid_type
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,imon,nmonths,nspecies,ispc,nlev
real, dimension(n1,n2) :: rlat,rlon
real, dimension(nlon,nlat,nlev,nspecies) :: RAWsrc
real ilatn,ilonn,tx(nspecies),delta
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
TX(:)  = 0.

do jj = min(max(1,jc-qj1),nlat),min(nlat,jc+qj2)
   do ii = min(max(1,ic-qi1),nlon),min(nlon,ic+qi2)   
   
     	    ncount = ncount + 1
     	    TX(:) = TX(:) + RAWsrc(ii,jj,1,:)  
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol2_afwa_erod
! binary_read_afwafiles.f90 in /scratch3/BMC/fim/lzhang/sandclay 10/29/17
!!!--------------------------------------------------------------------------!!!
!~                                                                            ~!
!~ Name:                                                                      ~!
!~    LoadFlatDirtData                                                        ~!
!~                                                                            ~!
!~ Description:                                                               ~!
!~    This function loads sand, clay, and silt data from flat binary files.   ~!
!~                                                                            ~!
!!!--------------------------------------------------------------------------!!!

FUNCTION LoadFlatDirtData ( fname, n1,n2, array ) result ( ostat )

implicit none

   character ( * ), intent ( in ) :: fname  !~ Name of the file to load
   integer                        :: n1,n2
   integer                        :: ostat  !~ Function status variable.
                                            !~ Non-zero is bad.

   real,intent (inout)  :: array(n1,n2)

   !~ Utility variables.
   !  ------------------
   integer                        :: i,j,k  !~ Dummy iterators
   integer                        :: retval !~ Internal status variable
   integer                        :: lun    !~ Dummy logical unit number
   integer                        :: nx,ny  !~ Incoming grid dimenstions
   integer                        :: irec   !~ File record number
   logical                        :: exstat !~ File existance flag
   real, allocatable              :: data ( :,: ) !~ Temporary data array.
   character ( 16 )               :: self   !~ Function name
   parameter ( self = 'LoadFlatDirtData' )

   !~ Initialize variables.
   !  ---------------------
   ostat  = 0
   retval = 0
   nx     = 36000
   ny     = 18000

   !~ Make sure the submitted data file exists.
   !  -----------------------------------------
   INQUIRE ( file = TRIM (fname), exist = exstat ) 
   IF ( .not. exstat ) THEN
      WRITE ( *,* ) ''
      WRITE ( *,* ) ' ERROR: Submitted dataset does not exist.'
      WRITE ( *,* ) ' Submitted file name: ' // TRIM ( fname )
      WRITE ( *,* ) ' Exiting ' // self
      WRITE ( *,* ) ''
      ostat = 1
      RETURN; END IF

   !~ Open the submitted file.
   !  -------------------------
   lun = 102
   OPEN ( lun, file = TRIM (fname), action = 'read', form = 'unformatted' &
        ,convert='BIG_ENDIAN', access = 'direct', recl = 36000, iostat = retval ) ! Linux
!                    , access = 'direct', recl = 4*36000, iostat = retval )       ! IBM

   IF ( retval /= 0 ) THEN
      WRITE ( *,* ) ''
      WRITE ( *,* ) ' ERROR: Unable to open submitted file.' 
      WRITE ( *,* ) ' File name:     ' // TRIM ( fname )
      WRITE ( *,* ) ' Return status: ', retval 
      WRITE ( *,* ) ' Exiting ' // self 
      WRITE ( *,* ) ''
      ostat = retval
      RETURN; END IF
      
   !~ Allocate the temporary storage.
   !  -------------------------------
   ALLOCATE ( data (nx,ny), stat = retval )
   IF ( retval /= 0 ) THEN
      WRITE ( *,* ) ''
      WRITE ( *,* ) ' ERROR: Unable to allocate storage space for flat data.' 
      WRITE ( *,* ) ' Return status: ', retval
      WRITE ( *,* ) ' Requested dataset: ', TRIM ( fname )
      WRITE ( *,* ) ' Exiting ' // self 
      WRITE ( *,* ) ''
      ostat = retval 
      RETURN; END IF

   !~ Initialize temporary array.
   !  ---------------------------
   WRITE ( *,* )  ' Initializing...'
   data = 0.

   !~ Loop through the file, load the data.
   !  -------------------------------------
   WRITE ( *,* ) ' Reading file...'
   irec = 1
   DO
      READ ( lun, rec = irec, iostat = retval ) data ( :,irec+3000 )
      IF ( retval /= 0 ) EXIT    
      irec = irec + 1
   END DO

   IF ( irec < ny-3000  ) THEN
      WRITE ( *,* ) ''
      WRITE ( *,* ) ' ERROR: Did not complete reading data file. '
      WRITE ( *,* ) ' File :             ' // TRIM ( fname )
      WRITE ( *,* ) ' Final read record: ', irec
      WRITE ( *,* ) ' Exiting ' // self
      WRITE ( *,* ) ' '
      ostat = 1

      IF ( ALLOCATED (data) ) DEALLOCATE ( data )
      CLOSE ( lun )
      RETURN; END IF 

   !~ Close the data file.
   !  --------------------
   CLOSE ( lun ) 

! Only sample every 10th point in 1km file
   do i=1,nx/10
   do j=1,ny/10
   array(i,j) = data ( 1+(i-1)*10,1+(j-1)*10)
   if (array(i,j) .eq. -.99) then
     array(i,j)=0.
   endif
   enddo
   enddo
 
   IF ( ALLOCATED (data) ) DEALLOCATE ( data ) 

END FUNCTION LoadFlatDirtData

!XXEND PROGRAM read_binary
