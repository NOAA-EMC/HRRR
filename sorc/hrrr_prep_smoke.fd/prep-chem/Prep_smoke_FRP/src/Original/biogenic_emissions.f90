!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module bioge_emissions
!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 11

integer, parameter ::         &
 NO	 =1  &
,CO	 =2  &
,CH3OH	 =3  &
,C3H8	 =4  &
,C2H6	 =5  &
,C2H4	 =6  &
,ACETONE =7  &
,NVOC    =8  &
,TERPENES=9  &
,ISOPRENE=10 &
,C3H6    =11 !propene 

!---------------------------------------------------------------------------
character(LEN=8),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                           & 
  'NO      '&
, 'CO      '&
, 'CH3OH   '&
, 'C3H8    '&
, 'C2H6    '&
, 'C2H4    '&
, 'ACETONE '&
, 'NVOC    '&
, 'TERPENES'&
, 'ISOPRENE'&
, 'C3H6    '/)
!---------------------------------------------------------------------------

  type bioge_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type bioge_vars

  type (bioge_vars), allocatable :: bioge_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_bioge(bioge,n1,n2,n3)

    implicit none

    type (bioge_vars),dimension(nspecies)  :: bioge
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (bioge(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_bioge

  !---------------------------------------------------------------

  subroutine nullify_bioge(bioge)

    implicit none

    type (bioge_vars),dimension(nspecies)  :: bioge
    integer ispc

    do ispc=1,nspecies
       if (associated(bioge(ispc)%src))    nullify (bioge(ispc)%src)
    enddo

    return
  end subroutine nullify_bioge

end module bioge_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_bioge(n1,n2,n3)
    use bioge_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(bioge_g)) allocate(bioge_g(nspecies))
    !do i=1,nspecies
    ! if(associated(bioge_g(i)%src)) deallocate(bioge_g(i)%src)
    !enddo

    call nullify_bioge(bioge_g(:))      
    call alloc_bioge  (bioge_g(:),n1,n2,n3) 
  end subroutine mem_bioge

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_bioge(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland &
                     ,deltax,deltay,xt,yt,xm,ym,plat,plon)

use grid_dims_out, only: grid_type, bioge_data_dir,maxfiles
use bioge_emissions
implicit none
integer, parameter ::  nlon = 360, nlat=180, nmonths=12 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,nf
character*240 prefix,suffix,filename(maxfiles),fname
character*1 dummy
real longbioge(nlon),latbioge(nlat)
real, parameter ::   ilatn=1., ilonn=1.
real lat,lon,src_dummy(nmonths),plat,plon
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies)
real,allocatable ,save :: RAWsrc(:,:,:,:)
integer :: icity,jcity,nfiles

if( .not. allocated (RAWsrc)) allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )

!lat e lon bioge (corner mais ao sul e mais a oeste)				      
do k=1,nlon
 longbioge(k)=-179.5 + (k-1)*ilonn
enddo
do i=1,nlat
 latbioge(i)= -89.5 + (i-1)*ilatn
enddo


if(ng == 1) then  ! need to read just one time
  nfiles = 0

  do ispc=1,nspecies
    !- name of dataset
    nc=len_trim(spc_name(ispc))
    fname=trim(bioge_data_dir)//'/'//spc_name(ispc)(1:nc)
    
    call chem_RAMS_filelist(filename,trim(fname)//'*',maxfiles,nfiles,2)

    RAWsrc(:,:,:,ispc) = 0.
    
    do nf=1,nfiles
    
    	  print *,'opening   ',nf, trim(filename(nf)),' - specie= ',trim(spc_name(ispc))
    	  open(11,file=trim(filename(nf)),status='old')
    	  
	  read(11,*) dummy
          DO WHILE (dummy == '#')
  	    read(11,*) dummy 
            !print*,dummy     
          enddo
          backspace (11)
	  
	  do iread=1,nlon*nlat !maximum number of points for nlon,nlat 
    	
    	      read(11,*,end=11) lon,lat,(src_dummy(im),im=1,nmonths)
    	      !print*,lon,lat,(src_dummy(im),im=1,nmonths)
    	      
    	      ilaea = nint(1.*(lon - (-180. + 0.5) )) + 1 ! 2. = 1/resolution
    	      jlaea = nint(1.*(lat - ( -90. + 0.5) )) + 1 ! 0.25 = 0.5*resolution
    	      ilaea= max(ilaea,1)
    	      if(ilaea < 1 .or. ilaea > nlon) stop 33
    	      if(jlaea < 1 .or. jlaea > nlat) stop 35
    	
    	      RAWsrc(ilaea,jlaea,:,ispc)=RAWsrc(ilaea,jlaea,:,ispc)+src_dummy(:)
    	   enddo !of iread

    	11 close(11)

     enddo ! nfiles
  enddo ! nspecies
endif ! ng==1

!--- performs the interpolation to model grid box
do i=1,n1
  do j=1,n2
    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longbioge,latbioge &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)


    call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imon,nmonths,nspecies,RAWsrc,tx(1:nspecies))
! 
! TX is the value interpolated to the model grid box.
!-obs: the raw data is  the monthly mean and the units are in kg /(m^2 s)
   do ispc=1,nspecies
    bioge_g(ispc)%src(i,j,1)=TX(ispc) * 86400. ! convert to kg/m2/day
   enddo
  enddo
enddo 
!- deallocate memory that we do not need anymore    
if(ng==ngrids) deallocate (RAWsrc)

end subroutine read_bioge
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_bioge_indentity(spc_name,ident)
!use chem1_list
use bioge_emissions, only :  bioge_nspecies=>nspecies&
                            ,bioge_spc_name=>spc_name

implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,bioge_nspecies
  ident=-1
  if(spc_name == bioge_spc_name(isp)) then
      print*,'==>bioge found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of bioge'
!stop 444
end subroutine get_bioge_indentity
!---------------------------------------------------------------
