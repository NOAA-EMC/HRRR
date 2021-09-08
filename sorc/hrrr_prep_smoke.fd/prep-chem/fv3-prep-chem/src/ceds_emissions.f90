!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
!CEDS            sector:ids = "1: Agriculture; 2: Energy; 3: Industrial; 4:
!CEDS            Transportation; 5: Residential, Commercial, Other; 6: Solvents
!CEDS            production and application; 7: Waste; 8: International
!CEDS            Shipping"
!###############################################################################!

module ceds_emissions
!---------------------------------------------------------------------------
integer, parameter ::  nspecies = 8 

integer, parameter ::         &
 OC		  =1  &
,BC		  =2  &
,SO2		  =3  &
,CO 		  =4  &
,NOx		  =5  &
,NH3		  =6  &
,CH4		  =7  &
,CO2		  =8  

!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                          & 
  'OC                      '&
, 'BC                      '&
, 'SO2                     '&
, 'CO                      '&
, 'NOx                     '&
, 'NH3                     '&
, 'CH4                     '&
, 'CO2                     '&
/)

character(LEN=25),dimension(nspecies),parameter :: netcdf_spc_name= &
! '1234567890123456789012345'
(/                          & 
  'OC_em_anthro            '& !kg/m2/s
, 'BC_em_anthro            '& !kg/m2/s
, 'SO2_em_anthro           '& !kg/m2/s
, 'CO_em_anthro            '& !kg/m2/s
, 'NOx_em_anthro           '& !kg/m2/s as NO2
, 'NH3_em_anthro           '& !kg/m2/s
, 'CH4_em_anthro           '& !kg/m2/s - file start at 1970
, 'CO2_em_anthro           '& !kg/m2/s
/)

REAL,PARAMETER :: convert_to_kg_per_day=86400. ! Convert to kg/dy, need time and area

!---------------------------------------------------------------------------

!---------------------------------------------------------------------------

  type ceds_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type ceds_vars

  type (ceds_vars), allocatable :: ceds_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_ceds(ceds,n1,n2,n3)

    implicit none

    type (ceds_vars),dimension(nspecies)  :: ceds
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (ceds(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_ceds

  !---------------------------------------------------------------

  subroutine nullify_ceds(ceds)

    implicit none

    type (ceds_vars),dimension(nspecies)  :: ceds
    integer ispc

    do ispc=1,nspecies
       if (associated(ceds(ispc)%src))    nullify (ceds(ispc)%src)
    enddo

    return
  end subroutine nullify_ceds

end module ceds_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_ceds(n1,n2,n3)
    use ceds_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(ceds_g)) allocate(ceds_g(nspecies))
    !do i=1,nspecies
    ! if(associated(ceds_g(i)%src)) deallocate(ceds_g(i)%src)
    !enddo

    call nullify_ceds(ceds_g(:))      
    call alloc_ceds  (ceds_g(:),n1,n2,n3) 
  end subroutine mem_ceds

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_ceds_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type, ceds_data_dir
use ceds_emissions
use cetesb_update!, only:  ncities  ,cetesb_nspecies, n_data &
                  !,CO_cetesb=>CO,NOX_cetesb=>NOX,
use netcdf
use mem_grid, only :grid_g

implicit none
include 'netcdf.inc'
integer, parameter ::  nlon = 720, nlat=360, nsect=8, nmonths=1, nyears=15 !yearly/monthly data for CEDS
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,jj,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,imonx
integer :: ic,jc,j1,j2,var_id,ncid,stat,iyr
integer :: istart(4),iend(4)
character*240 prefix,suffix,filename(nspecies)
character*10 dummy
real longceds(nlon),latceds(nlat)
real, parameter:: ilatn=0.5,ilonn=0.5
real lat,lon,src_dummy(nlon,nlat,nsect,1)
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
integer,save :: nXfv3,nlonX,nlatX ! horizontal factor for grid space increase - fv3 C96 is ~.5deg res
real,save :: ilatnX,ilonnX
real, allocatable,save, dimension(:)  :: longcedsX,latcedsX
! Stu variables for HTAP-FV3 option 10/13/17
        integer, allocatable, dimension(:,:) :: htap_fimN
real dlon,dlat,dlonr,dlatr,d2r
   d2r = 4.0*ATAN(1.0)/180.0
   dlon=d2r*(ilonn)*r_earth
   dlat=d2r*(ilatn)*r_earth
 print *,'In read_ceds_antro,n1,n2=',n1,n2
 call flush(6)
ALLOCATE(NewRatio(MaxCity),ratio(MaxCity),OldRatio(MaxCity),NewEmiss(MaxCity),NewSpecies(MaxCity),OldSpecies(MaxCity))

!- do update using CETESB/2005 data (=1: yes, =0, no)
flag_cetesb_update = 0; accum_emission=0.
!- do update using EXTRAPOLATION data (=1: yes, =0, no)
flag_extrapolation_update = 0; accum_emission=0

!--- for CEDS month is read in input (imon), only this month/year applied to global data
imonx=1  

!--- lat e lon ceds (corner mais ao sul e mais a oeste)				      
do k=1,nlon;  longceds(k)=-180.0 +0.5*ilonn +(k-1)*ilonn; enddo
do i=1,nlat;   latceds(i)= -90.0 +0.5*ilatn +(i-1)*ilatn; enddo

   if( .not. allocated (RAWsrc))then
    if(grid_type == 'fv3')then
    nXfv3 = 12  ! horizontal factor for grid space increase - fv3 C96 is ~.5deg res,
    nlonX = nlon*nXfv3; nlatX = nlat*nXfv3
    ilatnX=ilatn/nXfv3;ilonnX=ilonn/nXfv3
    allocate(longcedsX(nlonX),latcedsX(nlatX))
    do k=1,nlonX
     longcedsX(k)=-180. +.5*ilonnX + (k-1)*ilonnX
    enddo
    do i=1,nlatX
     latcedsX(i)= -90.  +.5*ilatnX + (i-1)*ilatnX
    enddo
    allocate(  RAWsrc(nlonX,nlatX,nmonths,nspecies) )
    elseif(grid_type == 'fim')then
! Expand CEDS .5 deg domain to HTAP .1 deg domain so FIM coordinate info matches HTAPs
    nXfv3 = 5  ! horizontal factor for grid space increase - fv3 C96 is ~.5deg res,
    nlonX = nlon*nXfv3; nlatX = nlat*nXfv3
    ilatnX=ilatn/nXfv3;ilonnX=ilonn/nXfv3
    allocate(longcedsX(nlonX),latcedsX(nlatX))
    do k=1,nlonX
     longcedsX(k)=-180. +.5*ilonnX + (k-1)*ilonnX
    enddo
    do i=1,nlatX
     latcedsX(i)= -90.  +.5*ilatnX + (i-1)*ilatnX
    enddo
    allocate(  RAWsrc(nlonX,nlatX,nmonths,nspecies) )
           if( .not. allocated (htap_fimN)) then
                  allocate(  htap_fimN(nlonX,nlatX) )
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
    else
    allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )
    endif
   endif

if(ng == 1) then  ! need to read just one time

  !- name of dataset
  prefix=''
  do ispc=1,nspecies
  suffix='-em-anthro_input4MIPs_emissions_CMIP_CEDS-2017-05-18_gn_200001-201412.nc'
 if(trim(spc_name(ispc)) == 'CH4')then ! CEDS methane starts in 1970, not 2000
  suffix='-em-anthro_input4MIPs_emissions_CMIP_CEDS-2017-05-18_gn_197001-201412.nc'
 endif
    nc=len_trim(spc_name(ispc))
    !print*,'nc=',nc,spc_name(ispc)
    
    filename(ispc)=trim(ceds_data_dir)//'/'//trim(prefix)//spc_name(ispc)(1:nc)//suffix
    
    print *,'opening   ', trim(filename(ispc)),' - specie= ',trim(spc_name(ispc))

    
! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
        stat = nf_open(trim(filename(ispc)), NF_NOWRITE, ncid)
        print*,ncid,stat,trim(netcdf_spc_name(ISPC))

! Get the varid of the data variable, based on its name.
     stat = nf_inq_varid ( ncid, trim(netcdf_spc_name(ispc)), var_id )
      print*,ncid,trim(netcdf_spc_name(ISPC)),var_id
      call FLUSH(6)
! Read the data.  
      istart=1
      iend(1)=nlon
      iend(2)=nlat
      iend(3)=nsect
      iyr=iyear
      if(iyear.lt.2000)iyr=2000
      if(iyear.gt.2014)iyr=2014
      istart(4)=(iyr-2000)*12+imon
 if(trim(spc_name(ispc)) == 'CH4')then ! CEDS anthro methane starts in 1970, not 2000
      istart(4)=(iyr-1970)*12+imon
 endif
      iend(4)=1
      call ncvgt( ncid,var_id,istart,iend,src_dummy,stat)
      RAWsrc(:,:,:,ispc)=0.
      print*,'stat,spc,max=',stat,spc_name(ispc),maxval(src_dummy)

      do ii=1,nsect
           accum_emission = 0.
           do j=1,nlat
           do i=1,nlon
           accum_emission = accum_emission + src_dummy(i,j,ii,1)* &
           dlon*dlat*cos(latceds(j)*d2r)
           enddo
           enddo
      write(*,'(2A,1X,I3,1X,1P2E11.4)')'spc,sectr,max,tot(kg/s)=',trim(spc_name(ispc)), &
      ii,maxval(src_dummy(:,:,ii,1)),accum_emission
      call flush(6)
  if(grid_type == 'fv3' .or. grid_type == 'fim')then
        do j=1,nlatX
        jc=int((j-1)/nXfv3)+1
        do i=1,nlonX
        ic=int((i-1)/nXfv3)+1
        RAWsrc(i,j,imonx,ispc)=RAWsrc(i,j,imonx,ispc)+src_dummy(ic,jc,ii,1)
        enddo
        enddo
   else
      RAWsrc(:,:,imonx,ispc)=RAWsrc(:,:,imonx,ispc)+src_dummy(:,:,ii,1)
   endif
      enddo ! Endof sector (nsect) loop
      print*,' All sectors ispc max=',maxval(RAWsrc(:,:,:,ispc))
      !pause

! Close the file, freeing all resources.
      call ncclos(ncid,stat)
  enddo ! nspecies

!print*,' MAX=',maxval(RAWsrc(:,:,imonx,OC)),maxval(RAWsrc(:,:,imonx,BC)),maxval(RAWsrc(:,:,imonx,SO2))
!pause


! convert to mass/area (m^2) 
!do i=1,nlat
!  area=cos(latceds(i)*pi180)* (r_earth**2) * ilatn * ilonn * pi180**2
 
!  do k=1,nlon   
!    do ispc=1,nspecies
!         RAWsrc(k,i,imonx,ispc)=RAWsrc(k,i,imonx,ispc)/area
!    enddo
!  enddo
!enddo 


endif ! ng==1


!--- performs the interpolation to model grid box
  if(grid_type /= 'fim')then
do i=1,n1
  do j=1,n2
  if(grid_type == 'fv3')then
        call grid_htap2fv3(i,j,rlat(i,j),rlon(i,j),n1,n2,nlonX,nlatX,imonx, &
        nmonths,nspecies,ilonnX,ilatnX,latcedsX,longcedsX,RAWsrc,rland,tx)
! TX is now in kg/m2/sec (primary species) over fv3 grid
  else
    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longceds,latceds &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

    call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,imonx,nmonths,nspecies,RAWsrc,tx(1:nspecies))
  endif
! 
! TX is the value interpolated to the model grid box. -obs: convert to kg / day
    do ispc = 1, nspecies 
     ceds_g(ispc)%src(i,j,1)=TX(ispc) * convert_to_kg_per_day ! convert to kg/m2/day
    enddo

  enddo
enddo 
  elseif(grid_type == 'fim')then
! Note that HTAP grid is used for ceds extended grid, fim logic applies 11/4/17
!       call grid_htap2fv3(i,j,rlat(i,j),rlon(i,j),n1,n2,nlonX,nlatX,imonx, &
!       nmonths,nspecies,ilonnX,ilatnX,latcedsX,longcedsX,RAWsrc,rland,tx)
   dlonr=d2r*(ilonnX)*r_earth
   dlatr=d2r*(ilatnX)*r_earth
  do jj=1,nlatX
  do ii=1,nlonX
  i=htap_fimN(ii,jj)
  j=1
!       call grid_htap2fim(i,j,n1,n2,nlonX,nlatX,htap_fimN,imonx,nmonths, &
!       nspecies,ilonnX,ilatnX,latcedsX,RAWsrc,grid_area,tx(1:nspecies))

            TX(:) = cos(latcedsX(jj)*d2r)*dlonr*dlatr*RAWsrc(ii,jj,1,:)
    do ispc = 1, nspecies
     ceds_g(ispc)%src(i,j,1)=ceds_g(ispc)%src(i,j,1)+TX(ispc)
    enddo
  enddo
  enddo
    do ispc = 1, nspecies
     ceds_g(ispc)%src(:,:,1)=ceds_g(ispc)%src(:,:,1)*convert_to_kg_per_day/grid_area(:,:)
    enddo

  endif
print*,' OC,BC,SO2 MAX=',maxval(ceds_g(OC)%src(:,:,1)),maxval(ceds_g(BC)%src(:,:,1)),maxval(ceds_g(SO2)%src(:,:,1))
 call flush(6)


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
print*,'valor da base gocart3  =',ceds_g(ispc)%src(icity,jcity,1)
accum_emission = 0.
if(icity_ANT==icity .and. jcity_ANT==jcity) accum_emission=ceds_g(ispc)%src(icity,jcity,1)
OldRatio(NumCity)=ceds_g(ispc)%src(icity,jcity,1)*1 !Para o calculo da razao
ceds_g(ispc)%src(icity,jcity,1) = accum_emission+(NewEmiss(NumCity) /grid_area(icity,jcity))! kg[CO]/m^2dia
print*,'valor da EXTRAPOLACAO =',ceds_g(ispc)%src(icity,jcity,1)
print*,'====================================================================================='
NewRatio(NumCity)=ceds_g(ispc)%src(icity,jcity,1)*1 !Para o calculo da razao
ratio(NumCity)=NewRatio(NumCity)/OldRatio(NumCity)
!--------------------------------------RATIO FOR OTHERS SPECIES----------------------------------------
if(OldRatio(NumCity)>0)then
DO ispc=2,nspecies,1
print*, '===================================================================================='
print*, 'Redefinindo Emissao antropogenica em: ',ACity(NumCity),'com RAZAO DO CO'
print*, 'Specie = ',spc_name(ispc)
print*,'valor da base gocart3  =',ceds_g(ispc)%src(icity,jcity,1)
ceds_g(ispc)%src(icity,jcity,1)=ratio(NumCity)*ceds_g(ispc)%src(icity,jcity,1)
print*,'valor aplicando RAZAO CO  =',ceds_g(ispc)%src(icity,jcity,1)
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
		 if(icity_ANT==icity .and. jcity_ANT==jcity) accum_emission=ceds_g(ispc)%src(icity,jcity,1)
		 
		 
		 !- unit                         =        kg[specie]/day        / m^2
	         ceds_g(ispc)%src(icity,jcity,1)=accum_emission + &
		                                  emission_city(ispc_cetesb,ici)/grid_area(icity,jcity)
		 
		 print*,'CETESB update applied to city: ',city_name(ici), 'grid=',ng
		 print*,'specie=',trim(spc_name(ispc)), ' updated=',ceds_g(ispc)%src(icity,jcity,1)

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
	            ceds_g(ispc)%src(icity,jcity,1)=emission_city(ispc_cetesb,ici)*weight/grid_area(i,j)
		    !- orig 
		    !ceds_g(CO )%src(i,j,1)        = CO_SP*(AREA_SP-grid_area(i,j))/(8.*AREA_SP) /grid_area(i,j)
		                                            
           	enddo;enddo
                
		!- closest grid box of the city (emission position)
                
		!- weight =  (grid_area(icity,jcity)/AREA_SP)
                 weight =   grid_area(icity,jcity)/emission_geo_info(1,ici)
	        
		!- unit  		       =	kg[specie]/day  	     / m^2
	        ceds_g(ispc)%src(icity,jcity,1)=emission_city(ispc_cetesb,ici)*weight/grid_area(icity,jcity)
            	!-orig
            	!ceds_g(NOX)%src(icity,jcity,1) = NOX_SP*(grid_area(icity,jcity)/AREA_SP) /grid_area(icity,jcity)

                
		print*,'CETESB update applied to city and neighbors: ',city_name(ici), 'grid=',ng
		print*,'specie=',trim(spc_name(ispc)), ' updated=',ceds_g(ispc)%src(icity,jcity,1)


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
      call apply_land_restriction(n1,n2,rland,ceds_g(ispc)%src(:,:,1))
   enddo  
endif

end subroutine read_ceds_antro
!---------------------------------------------------------------
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_ceds_indentity(spc_name,ident)
!use chem1_list
use ceds_emissions, only :  ceds_nspecies=>nspecies&
                            ,ceds_spc_name=>spc_name&
, ceds_OC     => OC	  &
, ceds_BC    => BC	  &    
, ceds_SO2    => SO2	    


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,ceds_nspecies
  ident=-1
  if(spc_name == ceds_spc_name(isp)) then
      print*,'==>ceds found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of ceds'
!stop 444
end subroutine get_ceds_indentity
!---------------------------------------------------------------

