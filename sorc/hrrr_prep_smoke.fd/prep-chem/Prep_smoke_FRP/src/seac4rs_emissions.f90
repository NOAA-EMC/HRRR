!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model                                    !
!  SEAC4RS anthropogenic emission over Aisa                                     !
!  Coded by Li Zhang                                                            !
!  Contact: kate.zhang@noaa.org                                                 !
!###############################################################################!

module seac4rs_emissions
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

  type seac4rs_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type seac4rs_vars

  type (seac4rs_vars), allocatable :: seac4rs_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_seac4rs(seac4rs,n1,n2,n3)

    implicit none

    type (seac4rs_vars),dimension(nspecies)  :: seac4rs
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (seac4rs(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_seac4rs

  !---------------------------------------------------------------

  subroutine nullify_seac4rs(seac4rs)

    implicit none

    type (seac4rs_vars),dimension(nspecies)  :: seac4rs
    integer ispc

    do ispc=1,nspecies
       if (associated(seac4rs(ispc)%src))    nullify (seac4rs(ispc)%src)
    enddo

    return
  end subroutine nullify_seac4rs

end module seac4rs_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_seac4rs(n1,n2,n3)
    use seac4rs_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(seac4rs_g)) allocate(seac4rs_g(nspecies))
    !do i=1,nspecies
    ! if(associated(seac4rs_g(i)%src)) deallocate(seac4rs_g(i)%src)
    !enddo

    call nullify_seac4rs(seac4rs_g(:))      
    call alloc_seac4rs  (seac4rs_g(:),n1,n2,n3) 
  end subroutine mem_seac4rs

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_seac4rs_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, seac4rs_data_dir
use seac4rs_emissions
use mem_grid, only :grid_g

implicit none
integer, parameter ::  nlon = 3600, nlat=1800, nmonths=1 !don't have monthly information 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid
character*240 filename1,filename2,filename3
character*10 dummy
real longseac4rs(nlon),latseac4rs(nlat)
real, parameter:: ilatn=0.1,ilonn=0.1
real lat,lon,src_dummy(nlon,nlat)
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),plat,plon,area
real, allocatable, dimension(:,:):: grid_area
real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6370000.


real,allocatable ,save :: RAWsrc(:,:,:,:)


!--- lat e lon seac4rs (corner mais ao sul e mais a oeste)				      
do k=1,nlon;  longseac4rs(k)=-179.95 + (k-1)*ilonn; enddo
do i=1,nlat;   latseac4rs(i)= -89.95 + (i-1)*ilatn; enddo

if( .not. allocated (RAWsrc)) allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )

!--- for seac4rs there is not monthly variation, 
imonx=1 

if(ng == 1) then  ! need to read just one time
 
filename1=trim(seac4rs_data_dir)//'/'//'OC-seac-0.1x0.1.dat'
print*,'================================================================='
print *,'SEAC4RS OC source opening   ', trim(filename1)
print*,'================================================================='
 open(11,file=filename1,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)
   
 read (11,rec=1) ((RAWsrc(i,j,imonx,1),i=1,nlon),j=1,nlat)
 close (11)

filename2=trim(seac4rs_data_dir)//'/'//'BC-seac-0.1x0.1.dat'
print*,'================================================================='
print *,'SEAC4RS BC source opening   ', trim(filename2)
print*,'================================================================='
 open(12,file=filename2,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)
   
 read (12,rec=1) ((RAWsrc(i,j,imonx,2),i=1,nlon),j=1,nlat)
 close (12)

filename3=trim(seac4rs_data_dir)//'/'//'SO2-seac-0.1x0.1.dat'
print*,'================================================================='
print *,'SEAC4RS SO2 source opening   ', trim(filename3)
print*,'================================================================='
 open(13,file=filename3,form='unformatted',access='direct', &
  status='OLD', recl=nlon*nlat*4)

 read (13,rec=1) ((RAWsrc(i,j,imonx,3),i=1,nlon),j=1,nlat)
 close (13)



!print*,' MAX=',maxval(RAWsrc(:,:,imonx,OC)),maxval(RAWsrc(:,:,imonx,BC)),maxval(RAWsrc(:,:,imonx,SO2))
!pause


! convert to mass/area (m^2) 
 do i=1,nlat
   area=cos(latseac4rs(i)*pi180)* (r_earth**2) * ilatn * ilonn * pi180**2
 
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

    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longseac4rs,latseac4rs &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

    call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imonx,nmonths,nspecies,RAWsrc,tx(1:nspecies))
! 
! TX is the value interpolated to the model grid box.
!-obs: convert to kg / day
    do ispc = 1, nspecies 
     seac4rs_g(ispc)%src(i,j,1)=TX(ispc) * convert_to_kg_per_day(ispc) ! convert to kg/m2/day
    enddo

  enddo
enddo 


print*,' MAX=',maxval(seac4rs_g(OC)%src(:,:,1)),maxval(seac4rs_g(BC)%src(:,:,1)),maxval(seac4rs_g(SO2)%src(:,:,1))


end subroutine read_seac4rs_antro
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_seac4rs_indentity(spc_name,ident)
!use chem1_list
use seac4rs_emissions, only :  seac4rs_nspecies=>nspecies&
                            ,seac4rs_spc_name=>spc_name&
, seac4rs_OC     => OC	  &
, seac4rs_BC    => BC	  &    
, seac4rs_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,seac4rs_nspecies
  ident=-1
  if(spc_name == seac4rs_spc_name(isp)) then
      print*,'==>seac4rs found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of seac4rs'
!stop 444
end subroutine get_seac4rs_indentity
!---------------------------------------------------------------

