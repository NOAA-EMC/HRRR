!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module gfedv2_emissions    
use AeM_emission_factors,  only: nspecies=>AeM_nspecies, N2_nitrogenio=>N2, AeM_spc_name, emission_factor

!---------------------------------------------------------------------------
  type gfedv2_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type gfedv2_vars

  type (gfedv2_vars), allocatable :: gfedv2_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_gfedv2(gfedv2,n1,n2,n3,nspecies)

    implicit none

    type (gfedv2_vars),dimension(nspecies)  :: gfedv2
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies
    
    do ispc=1,nspecies
     allocate (gfedv2(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_gfedv2

  !---------------------------------------------------------------

  subroutine nullify_gfedv2(gfedv2,nspecies)

    implicit none

    type (gfedv2_vars),dimension(nspecies)  :: gfedv2
    integer ispc,nspecies

    do ispc=1,nspecies
       if (associated(gfedv2(ispc)%src))    nullify (gfedv2(ispc)%src)
    enddo

    return
  end subroutine nullify_gfedv2

end module gfedv2_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_gfedv2(n1,n2,n3)
    use gfedv2_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(gfedv2_g)) allocate(gfedv2_g(nspecies))
   
    !do i=1,nspecies
    ! if(associated(gfedv2_g(i)%src)) deallocate(gfedv2_g(i)%src)
    !enddo

    call nullify_gfedv2(gfedv2_g(:),nspecies)      
    call alloc_gfedv2  (gfedv2_g(:),n1,n2,n3,nspecies) 
  end subroutine mem_gfedv2

!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine read_gfedv2(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                      ,xt,yt,plat,plon)

use grid_dims_out, only: maxfiles,grid_type,gfedv2_data_dir,use_bbem_plumerise
use gfedv2_emissions
use bbbem_plumerise

implicit none
integer, parameter ::  nlon = 360, nlat=180, nmonths=12 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
integer :: ispc,im,i,k,iread,nc,ilaea,jlaea,kk,k1,k2,ii,i1,i2,j,igbox&
          ,jgbox,j1,j2,ic,jc,iveg_ag, icount
	  
character cjday*3,cyear*4,cmon*2,cday*2
character (len=240) :: fnames(maxfiles)
character*240 filename
logical there
integer jday,jday_avail
integer iveg, veg_type(nlon,nlat)
integer  itimes(maxfiles),nfiles
real longgfedv2(nlon),latgfedv2(nlat)
real, parameter :: ilatn=1. , ilonn=1.  
real rrlat,rrlon,dlon1,dlon2,dlat1,dlat2,TX(nspecies),rdummy
real dx,dy,  dxm ,dym, xt(n1), yt(n2),plat,plon  
real, dimension(12) :: mondays
data mondays/31,28,31,30,31,30,31,31,30,31,30,31/
real, allocatable, save, dimension(:,:) :: carb,drybbc,veg,carb_mean
real, save :: typical_fire_size(0:3)

if(.not. allocated(carb)) allocate(carb(nlon,nlat),drybbc(nlon,nlat)&
                                   ,veg(nlon,nlat),carb_mean(nlon,nlat))


!- allocate memory for plumerise arrays
if(use_bbem_plumerise == 1) call mem_bbbem_plume(n1,n2,n3)

!-lat e lon gfedv2 (corner mais ao sul e mais a oeste)  				
do i=1,nlon
 longgfedv2(i)=-179.5 + (i-1)*ilonn
enddo
do j=1,nlat
 latgfedv2(j)= -89.5 + (j-1)*ilatn
enddo

!- avoid negative values for emission factor
where(emission_factor < 0.) emission_factor=0. 

if(ng == 1) then  

 print*,'==================   GFEDv2  ======================================'
 !- get vegetation dataset 
  open(30, FILE=trim(gfedv2_data_dir)//'/VEG.txt', STATUS='old')
    do j=1,180
      read(30,'(360f12.0)') veg(:,j)	
      !print*,'j=',j,veg(:,J)	   
    enddo
  close(30)

 !- determine the nearest 8-days data available
 call dataset_avail(iyear,imon,iday,cyear,cmon,cday,jday,cjday,jday_avail,filename)
 print*,'file= ',trim(filename),' jday=',jday,' jday-avail=',jday_avail
 !
 !- reading Carbon emission dataset 
 filename=trim(gfedv2_data_dir)//'/'//trim(filename)
 
 inquire(file=filename,exist=there)

 if(.not.there) then	
!- calculate the time average over the existent dataset for the same 8-days period
   print*, 'FILE= ',trim(filename),' does not exist, will get the mean of existent data'

   call bbbem_filelist(fnames,itimes,trim(gfedv2_data_dir)//'/'//'*'//cjday&
                      ,maxfiles,nfiles)
   if(nfiles==0) then
    print*,'No GFEDv2 data available for this time period'
    stop 'stop at subroutine read_gfedv2'
   endif
   call get_mean_gfedv2(nfiles,fnames,nlon,nlat,carb)

 else

  print *,'GFEDv2 source opening   ', trim(filename)
  open(30, FILE=filename, STATUS='old')
    do j=1,180
      read(30,'(360f12.4)') carb(:,j)	
     ! print*,'j=',j,carb(:,J)	   
    enddo
  close(30)

 endif

  !convert from carbon flux to dry-biomass-consumed per day
  drybbc=carb/(0.45*8.)  ! unit = g[DM]/m^2/day
 
 ! compatibilize a legenda de vegetacao com a adoptada no veg_agreg
 !gfedv2: 1=sav, 2=trop for, 3=extra trop forest
 ! agreg: 1 =trop forest, 2 =extra trop for, 3 =sav, 4 = past, 5=deserto, 0 = agua/gelo
 ! veg dominante por grid de 1x1 grau (data from GFEDv2)
 where(nint(veg(:,:)) == 1) veg_type(:,:)=3
 where(nint(veg(:,:)) == 2) veg_type(:,:)=1
 where(nint(veg(:,:)) == 3) veg_type(:,:)=2

 !- for plumerise - typical fire size
 typical_fire_size(0)=0.
 typical_fire_size(1)=20.* 1.e4 ! 20ha for tropical forest
 typical_fire_size(2)=20.* 1.e4 ! 20ha for extra tropical
 typical_fire_size(3)=5. * 1.e4 ! 5ha for savana/past ...
 


 !-revert y-dir
 call yrevert(nlon,nlat,1,veg_type)
 call yrevert(nlon,nlat,1,drybbc)

endif !(ng==1) 


 !--- performs the interpolation to model grid box

do i=1,n1
  do j=1,n2

   call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longgfedv2,latgfedv2 &
                 ,ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)
  !print*,'ic jc=',ic,jc,rlat(i,j),rlon(i,j)
  if(ic == 0 .or. jc == 0. .or. ic > 360 .or. jc > 180) cycle
! RAWsrc contains CO data 
   call interpol_gfedv_8days(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn   &
	           ,nspecies,drybbc,veg_type,tx)
! 
! TX is the value interpolated to the model grid box.
!-obs: the raw data is  the total monthly and the units are in g /m^2
     do ispc=1,nspecies
      gfedv2_g(ispc)%src(i,j,1)=TX(ispc) * 1.e-3 !(/mondays(imon)) ! converte para kg/m2/day
      !if(tx(ispc)>0.)print*,'is= ',TX(ispc),ic,jc
     enddo

     !------- plumerise section - only for areas with burning
     if(use_bbem_plumerise /= 1 .or. veg_type(ic,jc) == 0 .or. gfedv2_g(1)%src(i,j,1) < 1.e-6) cycle
      
      iveg_ag = veg_type(ic,jc) 
      
      do ispc=1,nspecies
          bbbem_plume_g(ispc,iveg_ag)%src(i,j,1) = gfedv2_g(ispc)%src(i,j,1)*flaming(iveg_ag)
      enddo
    
      !-fires properties : accumulated size
      bbbem_plume_fire_prop_g(qarea_agreg,iveg_ag)%fire_prop(i,j) = typical_fire_size(veg_type(ic,jc))
     
      !-fires properties : fire number
      bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j) = 1.     
     !------- end of plumerise section

  enddo
enddo
!------- plumerise section 2
if(use_bbem_plumerise == 1) then
!- produce some statistical quantities for the plumerise model
!
   do i=1,n1
      do j=1,n2
        do iveg_ag=1,nveg_agreg
        ! calculate the mean size of aggregate fire

           if( bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j) .ge. 1.) then 
              
              bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) = & ! mean  size =
              bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) / & ! total size / nfires
              bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j)
          else   
              bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) = 0.
          endif
          !- convert from mass consumed during the flaming phase to fraction of the total mass consumed
         
  
          do ispc=1,nspecies
             if(gfedv2_g(ispc)%src(i,j,1) > 0.) &	  
               !- fraction consumed at the flaming phase:
               bbbem_plume_g(ispc,iveg_ag)%src(i,j,1) = bbbem_plume_g(ispc,iveg_ag)%src(i,j,1) &
        					      / gfedv2_g(ispc)%src(i,j,1)
          enddo
        enddo 
     enddo
   enddo

  !- calculate the mean value for bbbem_plume_g (to save memory)
  do i=1,n1
   do j=1,n2
    do iveg_ag=1,nveg_agreg
         icount = 0
         bbbem_plume_mean_g(iveg_ag)%src(i,j,1)=0.
          
	  do ispc=1,nspecies
             if(gfedv2_g(ispc)%src(i,j,1) > 0.) then
	     	  icount = icount + 1
                  bbbem_plume_mean_g(iveg_ag)%src(i,j,1) = bbbem_plume_mean_g(  iveg_ag)%src(i,j,1)+ &
                                                           bbbem_plume_g(ispc,iveg_ag)%src(i,j,1)
             endif
	  enddo
          bbbem_plume_mean_g(iveg_ag)%src(i,j,1) = bbbem_plume_mean_g(iveg_ag)%src(i,j,1)/(float(icount)+1.e-10)
     enddo
   enddo
  enddo

endif
!------- end of plumerise section 2
      
!print*,'gfedv2_g(CO)%src(i,j,1)=',maxval(gfedv2_g(CO)%src(:,:,1))
!pause '1 '
!return
if(grid_type == 'rams' .or. grid_type == 'polar') then ! only for 'rams' until rland is also defined for others grids 
do ispc=1,nspecies 
  call apply_land_restriction(n1,n2,rland,gfedv2_g(ispc)%src(:,:,1))
enddo  
endif
!- deallocate memory that we do not need anymore    
if(ng==ngrids) deallocate (carb,drybbc,veg,carb_mean)

end subroutine read_gfedv2
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_gfedv2_indentity(spc_name,ident)
!use chem1_list
use gfedv2_emissions, only : gfedv2_nspecies=>nspecies& !don't use AeM_nspecies
                            ,gfedv2_spc_name=>AeM_spc_name
implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,gfedv2_nspecies
  ident=-1
  if(spc_name == gfedv2_spc_name(isp)) then
      print*,'==>gfedv2 found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of gfedv2'
!print*,'ident=',ident
!stop 444
end subroutine get_gfedv2_indentity
!---------------------------------------------------------------
subroutine interpol_gfedv_8days(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
	                ,nspecies,drybbc,veg_type&
			,tx)
use AeM_emission_factors, N2_nitrogenio=>N2
use grid_dims_out, only: grid_type
implicit none
integer n1,n2,ic,jc,nlat,nlon,i,j,nspecies,ispc
real, dimension(n1,n2) :: rlat,rlon
real, dimension(nlon,nlat) :: drybbc
real ilatn,ilonn,tx(nspecies), delta
integer  veg_type(nlon,nlat)

!-local var
real dlonr,dlatr,usdum
integer qi1,qi2,qj1,qj2,ncount,ii,jj
integer iveg , veg_type_model(n1,n2)

!if(ic.ge.0 .and. jc .ge. 0) then 
             
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
     	
     if(veg_type(ii,jj) > 0)  then 
      
       ncount = ncount + 1
     
! use emission factors to convert from  others species ...
       TX(:) = TX(:) + drybbc(ii,jj) * & 
               ! Em factor is in g/kg (1.e-3 => kg/kg]
               emission_factor(veg_type(ii,jj),: )*1.e-3
      endif 
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol_gfedv_8days
!---------------------------------------------------------------

subroutine dataset_avail(iyear,imon,iday,cyear,cmon,cday,&
                         jday,cjday,jday_avail,filename)
 implicit none
 integer :: julday,jday_avail,iyear,imon,iday,jday
 character (len=*)  :: filename,cyear,cmon,cday,cjday

!- get the julian day and the date in characters
 call determine_julian_day(iyear,imon,iday,cyear,cmon,cday,jday,cjday)
 print*,'date required =',iyear,imon,iday,jday

 jday_avail =   (8*int((jday-1)/8.)) + 1        
 print*,'date available=',iyear,jday_avail
 
 !if(jday_avail.le.99) then
 !  write(cjday,'(i2)') jday_avail
 !  cjday='0'//cjday
 !endif
 !if(jday_avail.gt.99) write(cjday,'(i3)') jday_avail
 
 write(cjday,'(i3.3)') jday_avail

 filename='C_'//trim(cyear)//'_JD'//trim(cjday)//'.txt'
 end subroutine dataset_avail
!---------------------------------------------------------------

subroutine  get_mean_gfedv2(nfiles,fnames,nlon,nlat,carb)
implicit none
integer nfiles,nlon,nlat,nfn,j
real carb(nlon,nlat),dummy(nlon,nlat)
character (len=*) :: fnames(nfiles)
dummy=0.
carb=0.
do nfn=1,nfiles
  print *,'GFEDv2 source opening   ', trim(fnames(nfn))
  open(30, FILE=fnames(nfn), STATUS='old')
    do j=1,180
      read(30,'(360f12.4)') carb(:,j)	
     ! print*,'j=',j,dummy(:,J)	   
    enddo
  close(30)
  carb(:,:)=carb(:,:)+dummy(:,:)
enddo
!- get the mean 
  carb(:,:)=carb(:,:)/float(nfiles)

end subroutine  get_mean_gfedv2
