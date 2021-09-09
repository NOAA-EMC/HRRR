!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module wb_emissions

!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 1

integer, parameter ::         &
 PM25			  =1
!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                           & 
  'PM25                     '&
/)

!---------------------------------------------------------------------------

  type wb_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type wb_vars

  type (wb_vars), allocatable :: wb_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_wb(wb,n1,n2,n3)

    implicit none

    type (wb_vars),dimension(nspecies)  :: wb
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (wb(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_wb

  !---------------------------------------------------------------

  subroutine nullify_wb(wb)

    implicit none

    type (wb_vars),dimension(nspecies)  :: wb
    integer ispc

    do ispc=1,nspecies
       if (associated(wb(ispc)%src))    nullify (wb(ispc)%src)
    enddo

    return
  end subroutine nullify_wb

end module wb_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_wb(n1,n2,n3)
    use wb_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

     if(.not. allocated(wb_g)) allocate(wb_g(nspecies))
!   
    !do i=1,nspecies
    ! if(associated(wb_g(i)%src)) deallocate(wb_g(i)%src)
    !enddo
    call nullify_wb(wb_g(:))      
    call alloc_wb  (wb_g(:),n1,n2,n3) 
  end subroutine mem_wb

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_wb(ihour,rhour,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type,wb_data_dir
use wb_emissions

implicit none
integer, parameter ::  nlon = 4272, nlat=4424, nmonths=1 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids,ihour,rhour
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,ic,jc,j1,j2
!real, intent(out), dimension 
character*240 prefix,suffix,filename(nspecies)
real longwb(nlon),latwb(nlat), plat,plon,fday
real, parameter ::  ilatn=0.00881577 , ilonn=0.00915527   
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),lat,lon,rdummy
real, allocatable, dimension(:,:):: grid_area
real  src_dummy(nmonths)
real,allocatable ,save :: RAWsrc(:,:,:,:)
!- for CETESB update :
character*180 dummy,city
character cyear*4

integer :: icity,jcity,icity_ANT,jcity_ANT,ici,flag_cetesb_update,flag_extrapolation_update &
          , nviz, iviz,jviz,flag_poly_update,flag_extrapoly_update,ipol,jpol
real weight, accum_emission, accum_emission2,mass_total,mass_check,rlandx

REAL :: x_grid_type(n1),y_grid_type(n2),AREA,NOXC,NOXC2

write(cyear,'(i4)') iyear
print*,'ano mes dia',iyear,imon,iday

!-lat e lon wb (corner mais ao sul e mais a oeste)	
do i=1,nlon; longwb(i)=-74.00 + (i-1)*ilonn; enddo 
do j=1,nlat; latwb (j)= -33.70 + (j-1)*ilatn; enddo

if( .not. allocated (RAWsrc)) allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )

if(ng == 1) then  ! need to read just one time

  !- name of dataset
   suffix='.gra'
  
!- para operacao, somente CO
     do ispc=1,nspecies 
     print*,'=================================================================';call flush(6)
     nc=len_trim(spc_name(ispc))
     !print*,'nc=',nc,spc_name(ispc)
     filename(ispc)=trim(wb_data_dir)//cyear//suffix
     print *,'wb source opening   ', trim(filename(ispc)),' - specie= ',trim(spc_name(ispc));call flush(6)

     open(11,file=trim(filename(ispc)),status='unknown',&
          form='unformatted',access='direct',&
          recl=4*nlon*nlat)
     iread=1
     read(11,rec=iread),RAWsrc(:,:,nmonths,nspecies)
     
! Aplica gaussiana com dependencia diaria e converte unidades    
     do i=1,nlon
        do j=1,nlat
           if(Rawsrc(i,j,1,1)>0)then
              call wb_fday(iday,imon,iyear,longwb(i),latwb(j),fday)
              RAWsrc(i,j,:,:)=RAWsrc(i,j,:,:)*fday/(1000*365*24)!kg m-2 day-1
           endif
          end do      
     end do
        
     print*,'min-max=',minval(RAWsrc(:,:,nmonths,nspecies)),maxval(RAWsrc(:,:,nmonths,nspecies))
    
    11 close(11)
   enddo ! nspecies
endif ! ng==1


!--- performs the interpolation to model grid box

do i=1,n1
  do j=1,n2
    call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longwb,latwb &
               ,ilatn, ilonn,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)
    
    !if(ic > nlon .or. ic < 1) then
    !   cycle
    !elseif(jc > nlat .or. jc < 1) then 
    !   cycle
    !else
       call interpol3(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
	           ,nmonths,nmonths,nspecies,RAWsrc,tx(1:nspecies))
    !endif
! 
! TX is the value interpolated to the model grid box.
!-obs: the raw data is  the monthly mean and the units are in kg /(m^2 s)

    do ispc = 1, nspecies 
    wb_g(ispc)%src(i,j,1)=TX(ispc)
    !print*,'TX wb_g',tx(:),wb_g(1)%src(i,j,:)  
    enddo
  enddo
enddo 

print*,'min-maxwb_g',minval(wb_g(1)%src(:,:,:)),maxval(wb_g(1)%src(:,:,:))
!print*,'min-maxTX',minval(tx(:)),maxval(tx(:))

!- deallocate memory that we do not need anymore    
if(ng==ngrids) deallocate (RAWsrc)

if(grid_type == 'rams' .or. grid_type == 'polar') then ! only for 'rams' until rland is also defined for others grids 
   do ispc=1,nspecies 
   !!!ispc=CO
   !   call apply_land_restriction(n1,n2,rland,wb_g(ispc)%src(:,:,1))
   enddo  
endif
end subroutine read_wb
!---------------------------------------------------------------
subroutine get_wb_indentity(spc_name,ident)
!use chem1_list
use wb_emissions, only :  wb_nspecies=>nspecies&
                            ,wb_spc_name=>spc_name

implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,wb_nspecies
  ident=-1
  if(spc_name == wb_spc_name(isp)) then
      print*,'==>wb found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match any one of wb'
!print*,'ident=',ident
!stop 444
end subroutine get_wb_indentity
!---------------------------------------------------------------
subroutine interpol3(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
	                ,imon,nmonths,nspecies,RAWsrc,tx)
use grid_dims_out, only: grid_type
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,imon,nmonths,nspecies,ispc
real, dimension(n1,n2) :: rlat,rlon
real, dimension(nlon,nlat,nmonths,nspecies) :: RAWsrc
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
else if(grid_type == 'mercator') then
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
     	    TX(:) = TX(:) + RAWsrc(ii,jj,imon,:)  
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol3

!---------------------------------------------------------------------
subroutine wb_fday(day,month,year,lon,lat,fday)
implicit none
integer :: day
integer :: day_of_year
integer :: i
integer :: leap_day
integer :: month
integer :: year
integer :: regiao
real    :: fday,fday_soma
real    :: media,desvio
real    :: lon,lat

!!!!!!!!!!!!!!!Acha dia juliano!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if (MOD(year,400) == 0) then
   leap_day = 1
else if(MOD(year,100) == 0) then
   leap_day = 0
else if(MOD(year,4) == 0) then
   leap_day = 1
else
   leap_day = 0
end if

day_of_year = day
DO i=1,month-1
   SELECT CASE (i)
   CASE (1,3,5,7,8,10,12)
      day_of_year=day_of_year + 31
   CASE (4,6,9,11)
      day_of_year=day_of_year + 30
   CASE (2)
      day_of_year=day_of_year + 28 + leap_day
   END SELECT
END DO

!!!!!!!!!!!!Determina a media e desvio da gaussiana dependendo da regiao!!!!!!!$
if(lon >= -74.0 .and. lon < -49.0 .and. lat >= -5.0 .and. lat < 5.3) then
   regiao=1 
   media=288
   desvio=30
else if(lon >= -74.0 .and. lon < -49.0 .and. lat >= -14.5 .and. lat < -5.0) then
   regiao=2
   media=227
   desvio=30
else if(lon >= -49.0 .and. lon < -34.8 .and. lat >= -14.5 .and. lat < 5.3) then
   regiao=3
   media=274
   desvio=45
else if(lon >= -53.4 .and. lon < -45.0 .and. lat >= -24.0 .and. lat < -19.0) then
   regiao=7
   media=227
   desvio=60
else if(lon >= -74.0 .and. lon < -53.4 .and. lat >= -24.0 .and. lat < -14.5) then
   regiao=5
   media=244
   desvio=30
else if(lon >= -74.0 .and. lon < -34.8 .and. lat >= -33.7 .and. lat < -24.0) then 
   regiao=6
   media=213
   desvio=45
else if(lon >= -53.4 .and. lon < -34.8 .and. lat >= -24.0 .and. lat < -14.5) then
   regiao=4
   media=258
   desvio=45
endif

!!!!!!!!!!!!!!!!Calcula gaussiana!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
fday_soma=0
DO i=1,365
   fday_soma=fday_soma + exp(-((i-media)**2)/(2*desvio**2))/(desvio*sqrt(2*3.141592654))
END DO

fday=exp(-((day_of_year-media)**2)/(2*desvio**2))/(desvio*sqrt(2*3.141592654))
fday=fday/fday_soma

!print*,lon,lat,regiao,media,desvio,fday

end subroutine wb_fday

