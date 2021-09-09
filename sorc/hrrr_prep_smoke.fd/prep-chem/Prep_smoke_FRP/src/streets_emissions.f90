!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model                                    !
!  Streets anthropogenic emission over Aisa                                     !
!  Coded by Li Zhang                                                            !
!  Contact: kate.zhang@noaa.org                                                 !
!###############################################################################!

module streets_emissions
!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 3 

integer, parameter ::         &
 OC		  =1  &
,BC		  =2  &
,SO2		  =3  

!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                          & 
  'OC                      '&
, 'BC                      '&
, 'SO2                     '&
/)

!character(LEN=25),dimension(nspecies),parameter :: netcdf_spc_name= &
! '1234567890123456789012345'
!(/                          & 
!  'OC_NOSHIP               '&
!, 'BC_NOSHIP               '&
!, 'SO2_NOSHIP              '&
!/)

REAL,PARAMETER,DIMENSION(nspecies) :: convert_to_kg_per_day=(/&
    1./365.  ,   & ! OC  => Gg/year per grid box = 1./365. kg/day
    1./365.  ,   & ! BC  => Gg/year per grid box = 1./365. kg/day
    1./365.        & ! SO2 => kg/year per grid box = 1./365.   kg/day
/)

!---------------------------------------------------------------------------

!---------------------------------------------------------------------------

  type streets_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type streets_vars

  type (streets_vars), allocatable :: streets_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_streets(streets,n1,n2,n3)

    implicit none

    type (streets_vars),dimension(nspecies)  :: streets
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (streets(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_streets

  !---------------------------------------------------------------

  subroutine nullify_streets(streets)

    implicit none

    type (streets_vars),dimension(nspecies)  :: streets
    integer ispc

    do ispc=1,nspecies
       if (associated(streets(ispc)%src))    nullify (streets(ispc)%src)
    enddo

    return
  end subroutine nullify_streets

end module streets_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_streets(n1,n2,n3)
    use streets_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(streets_g)) allocate(streets_g(nspecies))
    !do i=1,nspecies
    ! if(associated(streets_g(i)%src)) deallocate(streets_g(i)%src)
    !enddo

    call nullify_streets(streets_g(:))      
    call alloc_streets  (streets_g(:),n1,n2,n3) 
  end subroutine mem_streets

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_streets_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, streets_data_dir
use streets_emissions
use mem_grid, only :grid_g

implicit none
integer, parameter ::  nlon = 720, nlat=360, nmonths=1 !don't have monthly information 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid
character*240 filename1,filename2,filename3
character*10 dummy
real longstreets(nlon),latstreets(nlat)
real, parameter:: ilatn=0.5,ilonn=0.5
real lat,lon,src_dummy(nlon,nlat)
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),plat,plon,area
real, allocatable, dimension(:,:):: grid_area
real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6370000.


real,allocatable ,save :: RAWsrc(:,:,:,:)


!--- lat e lon streets (corner mais ao sul e mais a oeste)				      
do k=1,nlon;  longstreets(k)=-179.75 + (k-1)*ilonn; enddo
do i=1,nlat;   latstreets(i)= -89.75 + (i-1)*ilatn; enddo

if( .not. allocated (RAWsrc)) allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )

!--- for streets there is not monthly variation, 
imonx=1 

if(ng == 1) then  ! need to read just one time
 
filename1=trim(streets_data_dir)//'/'//'OC-streets-0.5x0.5.dat'
print*,'================================================================='
print *,'Streets OC source opening   ', trim(filename1)
print*,'================================================================='
 open(11,file=filename1,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)
   
 read (11,rec=1) ((RAWsrc(i,j,imonx,1),i=1,nlon),j=1,nlat)
 close (11)

filename2=trim(streets_data_dir)//'/'//'BC-streets-0.5x0.5.dat'
print*,'================================================================='
print *,'Streets BC source opening   ', trim(filename2)
print*,'================================================================='
 open(12,file=filename2,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)
   
 read (12,rec=1) ((RAWsrc(i,j,imonx,2),i=1,nlon),j=1,nlat)
 close (12)

filename3=trim(streets_data_dir)//'/'//'SO2-streets-0.5x0.5.dat'
print*,'================================================================='
print *,'Streets SO2 source opening   ', trim(filename3)
print*,'================================================================='
 open(13,file=filename3,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)

 read (13,rec=1) ((RAWsrc(i,j,imonx,3),i=1,nlon),j=1,nlat)
 close (13)



!print*,' MAX=',maxval(RAWsrc(:,:,imonx,OC)),maxval(RAWsrc(:,:,imonx,BC)),maxval(RAWsrc(:,:,imonx,SO2))
!pause


! convert to mass/area (m^2) 
 do i=1,nlat
   area=cos(latstreets(i)*pi180)* (r_earth**2) * ilatn * ilonn * pi180**2
 
   do k=1,nlon   
     do ispc=1,nspecies
          RAWsrc(k,i,imonx,ispc)=RAWsrc(k,i,imonx,ispc)/area
     enddo
   enddo
 enddo 


endif ! ng==1


!--- performs the interpolation to model grid box
do i=1,n1
  do j=1,n2

    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longstreets,latstreets &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

    call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imonx,nmonths,nspecies,RAWsrc,tx(1:nspecies))
! 
! TX is the value interpolated to the model grid box.
!-obs: convert to kg / day
    do ispc = 1, nspecies 
     streets_g(ispc)%src(i,j,1)=TX(ispc) * convert_to_kg_per_day(ispc) ! convert to kg/m2/day
    enddo

  enddo
enddo 


print*,' MAX=',maxval(streets_g(OC)%src(:,:,1)),maxval(streets_g(BC)%src(:,:,1)),maxval(streets_g(SO2)%src(:,:,1))


end subroutine read_streets_antro
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_streets_indentity(spc_name,ident)
!use chem1_list
use streets_emissions, only :  streets_nspecies=>nspecies&
                            ,streets_spc_name=>spc_name&
, streets_OC     => OC	  &
, streets_BC    => BC	  &    
, streets_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,streets_nspecies
  ident=-1
  if(spc_name == streets_spc_name(isp)) then
      print*,'==>streets found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of streets'
!stop 444
end subroutine get_streets_indentity
!---------------------------------------------------------------

