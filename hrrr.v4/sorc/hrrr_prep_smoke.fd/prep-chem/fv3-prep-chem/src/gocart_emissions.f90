!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module gocart_emissions
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

character(LEN=25),dimension(nspecies),parameter :: netcdf_spc_name= &
! '1234567890123456789012345'
(/                          & 
  'OC_NOSHIP               '&
, 'BC_NOSHIP               '&
, 'SO2_NOSHIP              '&
/)

REAL,PARAMETER,DIMENSION(nspecies) :: convert_to_kg_per_day=(/&
    1.e6/365.  ,   & ! OC  => Gg/year per grid box = 1.e6/365. kg/day
    1.e6/365.  ,   & ! BC  => Gg/year per grid box = 1.e6/365. kg/day
    1./365.        & ! SO2 => kg/year per grid box = 1./365.   kg/day
/)

!---------------------------------------------------------------------------

!---------------------------------------------------------------------------

  type gocart_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type gocart_vars

  type (gocart_vars), allocatable :: gocart_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_gocart(gocart,n1,n2,n3)

    implicit none

    type (gocart_vars),dimension(nspecies)  :: gocart
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (gocart(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_gocart

  !---------------------------------------------------------------

  subroutine nullify_gocart(gocart)

    implicit none

    type (gocart_vars),dimension(nspecies)  :: gocart
    integer ispc

    do ispc=1,nspecies
       if (associated(gocart(ispc)%src))    nullify (gocart(ispc)%src)
    enddo

    return
  end subroutine nullify_gocart

end module gocart_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_gocart(n1,n2,n3)
    use gocart_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(gocart_g)) allocate(gocart_g(nspecies))
    !do i=1,nspecies
    ! if(associated(gocart_g(i)%src)) deallocate(gocart_g(i)%src)
    !enddo

    call nullify_gocart(gocart_g(:))      
    call alloc_gocart  (gocart_g(:),n1,n2,n3) 
  end subroutine mem_gocart

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_gocart_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, gocart_data_dir
use gocart_emissions
use cetesb_update!, only:  ncities  ,cetesb_nspecies, n_data &
                  !,CO_cetesb=>CO,NOX_cetesb=>NOX,
use netcdf
use mem_grid, only :grid_g

implicit none
!include 'netcdf.inc'
integer, parameter ::  nlon = 360, nlat=180, nmonths=1 !don't have monthly information 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid
character*240 prefix,suffix,filename(nspecies)
character*10 dummy
real longgocart(nlon),latgocart(nlat)
real, parameter:: ilatn=1.,ilonn=1.
real lat,lon,src_dummy(nlon,nlat)
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),plat,plon,area
real, allocatable, dimension(:,:):: grid_area
character (len=30), dimension(10) :: name_city
real, parameter ::                    &
        pi180    = 3.1415927 / 180.   &
    ,   r_earth  = 6370000.


real,allocatable ,save :: RAWsrc(:,:,:,:)

!- for CETESB update :
integer :: icity,jcity,icity_ANT,jcity_ANT,ici,flag_cetesb_update,flag_extrapolation_update
real weight, accum_emission
!- for EXTRAPOLATION update :
INTEGER :: NVERT
CHARACTER (20) :: DUMMY_CITY
integer, parameter ::Length=21
INTEGER, PARAMETER :: MaxCity = 3659
INTEGER , PARAMETER :: npoints = 25
CHARACTER(20), ALLOCATABLE,DIMENSION (:) :: ACity
REAL,ALLOCATABLE,DIMENSION (:) :: Emission,EmissionNOX,city_size,alat,alon
REAL,ALLOCATABLE,DIMENSION(:,:) :: p_lon,p_lat
CHARACTER(Length) :: Iprefix
REAL, DIMENSION (:), ALLOCATABLE :: NewEmiss,NewRatio,OldRatio,ratio,NewSpecies,OldSpecies
INTEGER :: NumCity, AllocateStatus, Location
ALLOCATE(NewRatio(MaxCity),ratio(MaxCity),OldRatio(MaxCity),NewEmiss(MaxCity),NewSpecies(MaxCity),OldSpecies(MaxCity))



!- do update using CETESB/2005 data (=1: yes, =0, no)
flag_cetesb_update = 0; accum_emission=0.
!- do update using EXTRAPOLATION data (=1: yes, =0, no)
flag_extrapolation_update = 0; accum_emission=0

!--- for gocart there is not monthly variation, 
imonx=1  

!--- lat e lon gocart (corner mais ao sul e mais a oeste)				      
do k=1,nlon;  longgocart(k)=-179.5 + (k-1)*ilonn; enddo
do i=1,nlat;   latgocart(i)= -89.5 + (i-1)*ilatn; enddo

if( .not. allocated (RAWsrc)) allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )

if(ng == 1) then  ! need to read just one time

  !- name of dataset
  prefix=''
  suffix='_anthro_noship_2006.nc'


  do ispc=1,nspecies
    nc=len_trim(spc_name(ispc))
    !print*,'nc=',nc,spc_name(ispc)
    
    filename(ispc)=trim(gocart_data_dir)//'/'//trim(prefix)//spc_name(ispc)(1:nc)//suffix
    
    print *,'opening   ', trim(filename(ispc)),' - specie= ',trim(spc_name(ispc))

    
! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
      call check( nf90_open(TRIM(filename(ispc)), NF90_NOWRITE, ncid) )
    
      print*,ncid,trim(netcdf_spc_name(ISPC))

! Get the varid of the data variable, based on its name.
      call check( nf90_inq_varid(ncid, trim(netcdf_spc_name(ispc)), var_id) )


      print*,ncid,trim(netcdf_spc_name(ISPC)),var_id
      call FLUSH(6)
      
! Read the data.  
      call check( nf90_get_var(ncid, var_id, src_dummy) )  
       !print*,'spc=',spc_name(ispc),maxval(src_dummy)
      
      
      RAWsrc(:,:,imonx,ispc)=src_dummy(:,:)
      print*,' ispc=',maxval(src_dummy(:,:))
      !pause
      
! Close the file, freeing all resources.
      call check( nf90_close(ncid) )
  enddo ! nspecies

!print*,' MAX=',maxval(RAWsrc(:,:,imonx,OC)),maxval(RAWsrc(:,:,imonx,BC)),maxval(RAWsrc(:,:,imonx,SO2))
!pause


! convert to mass/area (m^2) 
 do i=1,nlat
   area=cos(latgocart(i)*pi180)* (r_earth**2) * ilatn * ilonn * pi180**2
 
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

    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longgocart,latgocart &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

    call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imonx,nmonths,nspecies,RAWsrc,tx(1:nspecies))
! 
! TX is the value interpolated to the model grid box.
!-obs: convert to kg / day
    do ispc = 1, nspecies 
     gocart_g(ispc)%src(i,j,1)=TX(ispc) * convert_to_kg_per_day(ispc) ! convert to kg/m2/day
    enddo

  enddo
enddo 


print*,' MAX=',maxval(gocart_g(OC)%src(:,:,1)),maxval(gocart_g(BC)%src(:,:,1)),maxval(gocart_g(SO2)%src(:,:,1))


!-----------------------------------Anthropogenic Emissions------------------------------------------!
!--------------------------Correcao para as principais cidades brasileiras---------------------------!
!-----------------------------Extrapolacao conforme arquivo INDEX.xls--------------------------------!
!-----------------------------------Marcelo Alonso (mar,2007)----------------------------------------!
if(flag_extrapolation_update == 1) then
!-------------------- calculate the grib box area, if necessary--------------------------------------!
allocate(grid_area(n1,n2))
if(grid_type == 'rams' .or. grid_type == 'polar') then
         call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
 elseif(grid_type == 'll') then
         call get_area_ll(grid_area,n1,n2) 
 else
     grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
! stop 'grid_type wrong'
endif
icity_ANT=0 ; jcity_ANT=0
!-----------------------------------------------------------------------------------------------------
!Read a file containing the updated emissions in array
!Identifiers used are:
!   EmissionData: Inventory Emission Data
!   Length: length of character strings (constant)
!   ACity: Inventory and extrapolation cities
!   MaxCity: maximum number of cities in the file
!   NumCity: number of cities in the file
!------------------------------------------------------------------------------------------------------
!#######################################READ DATA FILES##############################################

 
 ALLOCATE (city_size(MaxCity),Acity(MaxCity),Emission(MaxCity),EmissionNOX(MaxCity),alat(MaxCity),alon(MaxCity))
 ALLOCATE (p_lon(npoints,MaxCity),p_lat(npoints,MaxCity))

OPEN(UNIT = 201, FILE = 'EMISSION_DATA.dat', &
        STATUS = 'old')
 DO NumCity=1,Maxcity,1
 READ (201,*)Acity(NumCity),Emission(NumCity),EmissionNOX(NumCity),city_size(NumCity)
 READ (201,*)alat(NumCity),alon(NumCity)
 ENDDO
 CLOSE(201)


OPEN(UNIT = 206, FILE = 'POLLY_LAT.txt', &
        STATUS = 'old')
 DO NumCity=1,Maxcity,1
 READ (206,*)NVERT
 READ (206,*)p_lat(1:NVERT,NumCity)
 READ (206,*)DUMMY_CITY
 ENDDO
 CLOSE(206)

OPEN(UNIT = 207, FILE = 'POLLY_LON.txt', &
        STATUS = 'old')
 DO NumCity=1,Maxcity,1
 READ (207,*)NVERT
 READ (207,*)p_lon(1:NVERT,NumCity)
 READ (207,*)DUMMY_CITY
 ENDDO
 CLOSE(207)

!#########################################################################################################

!-------------------------------------------DATA UPDATED-----------------------------------------------
NumCity = 1
DO NumCity=1,MaxCity,1
ispc= SO2
rrlat= alat(NumCity)
rrlon= alon(NumCity)
!--------------------------A informacao esta em x10^7kg/ano--------------------------------------------
!--------------------------Converte para kg/dia--------------------------------------------------------
NewEmiss(NumCity)=(Emission(NumCity)*1E7)/365
!------------------------------------------------------------------------------------------------------
call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                ,rrlon,rrlat,icity,jcity,ACity(NumCity),grid_type)

if(icity /= -999 .and. jcity /= -999) then
print*,'====================================================================================='
print*,'Redefinindo Emissao antropogenica em: ',ACity(NumCity),'com dados da EXTRAPOLACAO'
print*,'valor da base gocart3  =',gocart_g(ispc)%src(icity,jcity,1)
accum_emission = 0.
if(icity_ANT==icity .and. jcity_ANT==jcity) accum_emission=gocart_g(ispc)%src(icity,jcity,1)
OldRatio(NumCity)=gocart_g(ispc)%src(icity,jcity,1)*1 !Para o calculo da razao
gocart_g(ispc)%src(icity,jcity,1) = accum_emission+(NewEmiss(NumCity) /grid_area(icity,jcity))! kg[CO]/m^2dia
print*,'valor da EXTRAPOLACAO =',gocart_g(ispc)%src(icity,jcity,1)
print*,'====================================================================================='
NewRatio(NumCity)=gocart_g(ispc)%src(icity,jcity,1)*1 !Para o calculo da razao
ratio(NumCity)=NewRatio(NumCity)/OldRatio(NumCity)
!--------------------------------------RATIO FOR OTHERS SPECIES----------------------------------------
if(OldRatio(NumCity)>0)then
DO ispc=2,nspecies,1
print*, '===================================================================================='
print*, 'Redefinindo Emissao antropogenica em: ',ACity(NumCity),'com RAZAO DO CO'
print*, 'Specie = ',spc_name(ispc)
print*,'valor da base gocart3  =',gocart_g(ispc)%src(icity,jcity,1)
gocart_g(ispc)%src(icity,jcity,1)=ratio(NumCity)*gocart_g(ispc)%src(icity,jcity,1)
print*,'valor aplicando RAZAO CO  =',gocart_g(ispc)%src(icity,jcity,1)
print*, '===================================================================================='
ENDDO
endif
endif
icity_ANT=icity ; jcity_ANT=jcity
ENDDO

deallocate(grid_area)
deallocate(ratio,NewRatio,OldRatio)
22 continue
endif

!--- section for update specifics emissions using external information (CETESB, etc)
if(flag_cetesb_update == 1) then

   
   !------ calculate the grib box area, if necessary
   allocate(grid_area(n1,n2))
   if(grid_type == 'rams' .or. grid_type == 'polar') then
   	    call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
    elseif(grid_type == 'll') then
   	    call get_area_ll(grid_area,n1,n2) 
    else
      grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
      !stop 'grid_type wrong'
   endif
   icity_ANT=0 ; jcity_ANT=0

   do ici = 1,ncities
      rrlat= emission_geo_info(2,ici)
      rrlon= emission_geo_info(3,ici) 
      print*,'lat - lon=',rrlat,rrlon
      
      
      call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                  ,rrlon,rrlat,icity,jcity,city_name(ici),grid_type)
    
      if(icity == -999 .or. jcity == -999) cycle !- city out of model domain
     
      !- if (grid box area      > grid area emission )
      if(grid_area(icity,jcity) > emission_geo_info(1,ici)) then
          
	  do ispc_cetesb = 1, cetesb_nspecies
	    do ispc = 1, nspecies 
	    
	       !- look for the correspondence between gocart and cetesb spcecies.
	       if(trim(spc_name(ispc)) == trim(cetesb_spc_name(ispc_cetesb))) then
	         
		 !- if the city used before is in the same grid box of the current city =>
		 !   => accumulate the emission 
		 accum_emission = 0.
		 if(icity_ANT==icity .and. jcity_ANT==jcity) accum_emission=gocart_g(ispc)%src(icity,jcity,1)
		 
		 
		 !- unit                         =        kg[specie]/day        / m^2
	         gocart_g(ispc)%src(icity,jcity,1)=accum_emission + &
		                                  emission_city(ispc_cetesb,ici)/grid_area(icity,jcity)
		 
		 print*,'CETESB update applied to city: ',city_name(ici), 'grid=',ng
		 print*,'specie=',trim(spc_name(ispc)), ' updated=',gocart_g(ispc)%src(icity,jcity,1)

	         cycle
	       endif
	  enddo;enddo
	        
      else ! (grid box area      <   grid area emission )
          
	  do ispc_cetesb = 1, cetesb_nspecies;  do ispc = 1, nspecies 
	     
	     !- look for correspondence between cetesb and gocart emissions
	     if(trim(spc_name(ispc)) == trim(cetesb_spc_name(ispc_cetesb))) then
	    	
		!- only 3 x 3 redistribution (in the future, extend for situation with very high resolution)
		do i=icity-1,icity+1 ; do j=jcity-1,jcity+1
            	 
		  if (i==icity .and. j==jcity) cycle
		    
		    !- weight of each neighboor grid box = (emission area - grid box area)/(8*emission area)
		    weight =  ( emission_geo_info(1,ici)-grid_area(i,j))/(8.*  emission_geo_info(1,ici))    

	            !- unit                         =        kg[specie]/day               / m^2
	            gocart_g(ispc)%src(icity,jcity,1)=emission_city(ispc_cetesb,ici)*weight/grid_area(i,j)
		    !- orig 
		    !gocart_g(CO )%src(i,j,1)        = CO_SP*(AREA_SP-grid_area(i,j))/(8.*AREA_SP) /grid_area(i,j)
		                                            
           	enddo;enddo
                
		!- closest grid box of the city (emission position)
                
		!- weight =  (grid_area(icity,jcity)/AREA_SP)
                 weight =   grid_area(icity,jcity)/emission_geo_info(1,ici)
	        
		!- unit  		       =	kg[specie]/day  	     / m^2
	        gocart_g(ispc)%src(icity,jcity,1)=emission_city(ispc_cetesb,ici)*weight/grid_area(icity,jcity)
            	!-orig
            	!gocart_g(NOX)%src(icity,jcity,1) = NOX_SP*(grid_area(icity,jcity)/AREA_SP) /grid_area(icity,jcity)

                
		print*,'CETESB update applied to city and neighbors: ',city_name(ici), 'grid=',ng
		print*,'specie=',trim(spc_name(ispc)), ' updated=',gocart_g(ispc)%src(icity,jcity,1)


 	     endif ! endif (trim(spc_name(ispc)) == trim(cetesb_spc_name(ispc_cetesb)
	  enddo;enddo
     
       endif !endif (grid box area      > grid area emission )
       
    icity_ANT=icity ; jcity_ANT=jcity  
    enddo 
        
    !- deallocate memory that we do not need anymore    
    deallocate(grid_area)
endif

if(ng==ngrids) deallocate (RAWsrc)


if(grid_type == 'rams' .or. grid_type == 'polar') then ! only for 'rams' until rland is also defined for others grids 
   do ispc=1,nspecies 
   !ispc=CO
      call apply_land_restriction(n1,n2,rland,gocart_g(ispc)%src(:,:,1))
   enddo  
endif

end subroutine read_gocart_antro
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_gocart_indentity(spc_name,ident)
!use chem1_list
use gocart_emissions, only :  gocart_nspecies=>nspecies&
                            ,gocart_spc_name=>spc_name&
, gocart_OC     => OC	  &
, gocart_BC    => BC	  &    
, gocart_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,gocart_nspecies
  ident=-1
  if(spc_name == gocart_spc_name(isp)) then
      print*,'==>gocart found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of gocart'
!stop 444
end subroutine get_gocart_indentity
!---------------------------------------------------------------

