!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
!  3BEM - Brazilian Biomass Burning Emission Model                              !
!###############################################################################!

module bbbem_emissions    
use AeM_emission_factors,  nspecies=>AeM_nspecies, N2_nitrogenio=>N2

  type bbbem_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type bbbem_vars

  type (bbbem_vars), allocatable :: bbbem_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_bbbem(bbbem,n1,n2,n3,nspecies)

    implicit none

    type (bbbem_vars),dimension(nspecies)  :: bbbem
    integer,intent(in) :: n1,n2,n3
    integer ispc,nspecies
    
    do ispc=1,nspecies
     allocate (bbbem(ispc)%src(n1,n2,n3))
      bbbem_g(ispc)%src(:,:,:)=0.
    enddo

    return
  end subroutine alloc_bbbem

  !---------------------------------------------------------------

  subroutine nullify_bbbem(bbbem,nspecies)

    implicit none

    type (bbbem_vars),dimension(nspecies)  :: bbbem
    integer ispc,nspecies

    do ispc=1,nspecies
       if (associated(bbbem(ispc)%src))    nullify (bbbem(ispc)%src)
    enddo

    return
  end subroutine nullify_bbbem

end module bbbem_emissions
!------------------------------------------------------------------------------------------------------

  subroutine mem_bbbem(n1,n2,n3)
    use bbbem_emissions
    implicit none
    integer i,j
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(bbbem_g)) allocate(bbbem_g(nspecies))
   
    !do i=1,nspecies
    ! if(associated(bbbem_g(i)%src)) deallocate(bbbem_g(i)%src)
    !enddo
    call nullify_bbbem(bbbem_g(:),nspecies)      
    call alloc_bbbem  (bbbem_g(:),n1,n2,n3,nspecies) 
    
    
  end subroutine mem_bbbem

!------------------------------------------------------------------------------------------------------
subroutine process_bbbem(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland&
                        ,deltax,deltay,xt,yt,xm,ym,plat,plon)


use grid_dims_out, only: maxfiles,grid_type     &
                      ,bbem_wfabba_data_dir     &
		      ,bbem_modis_data_dir      &
		      ,bbem_inpe_data_dir       &
		      ,bbem_extra_data_dir      &
		      ,veg_type_data_dir        &
		      ,use_vcf                  &
   		      ,vcf_type_data_dir        &
		      ,carbon_density_data_dir  &
		      ,fuel_data_dir

use bbbem_emissions
use fire_properties
implicit none

integer, intent (in) :: iyear,imon,iday,ihour,ng,n1,n2,n3,ngrids
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, intent (in) :: deltax,deltay
real, intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2),plat,plon
real, dimension(ntfocos,nspecies) :: qsc
real, dimension(ntfocos) :: qfires
integer nfocos
data nfocos /0/
integer nfn,jday,iunit,i,j
character (len=240) :: fnames(maxfiles)
integer  itimes(maxfiles)
character*180 dummy
character cjday*3,cyear*4,cmon*2,cday*2
integer nfiles

iunit=10

if(ng == 1) then ! just for the first grid is needed
 
nfocos_sensor_gt_1km=0; nfocos_sensor_le_1km=0

!- allocate memory for fires properties.
 call mem_fire ()

!- get the julian day and the date in characters
 call determine_julian_day(iyear,imon,iday,cyear,cmon,cday,jday,cjday)

 

!--------------------- Section 1 : fires from sensors with resolution > 1 km
!
!---- process WF_ABBA fire product
!
!- look for data available for the specific day
! bbem_wfabba_data_dir=trim(bbem_wfabba_data_dir)!
 print*,'WF_ABBA looking for:',trim(bbem_wfabba_data_dir)//cyear//cjday

 call bbbem_filelist(fnames,itimes,trim(bbem_wfabba_data_dir)//cyear//cjday&
 ,maxfiles,nfiles)

 do nfn=1,nfiles 
     open(iunit,status='old',file=fnames(nfn))
     call read_abba(iunit,ntfocos &
                  , fire_sensor_gt_1km(QLONG)%fire_prop &
                  , fire_sensor_gt_1km(QLATI)%fire_prop &
                  , fire_sensor_gt_1km(QAREA)%fire_prop &
                  , fire_sensor_gt_1km(QSIZE)%fire_prop &
                  , fire_sensor_gt_1km(QFRPW)%fire_prop &
                  , fire_sensor_gt_1km(QTIME)%fire_prop &
                  , nfocos_sensor_gt_1km,itimes(nfn)    )
     close(iunit)
 enddo
 
!- process  WF_ABBA  data set (if exist)
 if(nfiles > 0) &
  call filter_fires_sensor_gt_1km(ntfocos,nfocos_sensor_gt_1km&
                  , fire_sensor_gt_1km(QLONG)%fire_prop &
                  , fire_sensor_gt_1km(QLATI)%fire_prop &
                  , fire_sensor_gt_1km(QAREA)%fire_prop &
                  , fire_sensor_gt_1km(QSIZE)%fire_prop &
                  , fire_sensor_gt_1km(QFRPW)%fire_prop &
                  , fire_sensor_gt_1km(QFLAM)%fire_prop &
                  , fire_sensor_gt_1km(QVEG )%fire_prop &
                  , fire_sensor_gt_1km(QVEG_AGREG)%fire_prop &
                  , fire_sensor_gt_1km(QTIME)%fire_prop      &
                  , veg_type_data_dir)	  

!33 continue
!go to 22
!--------------------- Section 2 : fires from sensors with resol ~ or < 1 km
!
!------ process MODIS(TERRA/AQUA) GLOBAL fire products
!
   print*,trim(bbem_extra_data_dir)//'*'//cyear//cmon//cday//'*'
   call bbbem_filelist(fnames,itimes,trim(bbem_extra_data_dir)//'*'//cyear//cmon//cday//'*'&
                       ,maxfiles,nfiles)

   do nfn=1,nfiles
     open(iunit,status='old',file=fnames(nfn))
     call read_extra(iunit,ntfocos &
                  , fire_sensor_le_1km(QLONG)%fire_prop &
                  , fire_sensor_le_1km(QLATI)%fire_prop &
                  , fire_sensor_le_1km(QAREA)%fire_prop &
                  , fire_sensor_le_1km(QSIZE)%fire_prop &
                  , fire_sensor_le_1km(QFRPW)%fire_prop &
                  , fire_sensor_le_1km(QTIME)%fire_prop &
                  , nfocos_sensor_le_1km &
                  , veg_type_data_dir)
     close(iunit)
   enddo

!------ process MODIS(TERRA/AQUA) SouthAmerica-Africa fire products
   print*,trim(bbem_modis_data_dir)//'*'//cyear//cmon//cday//'*'
   call bbbem_filelist(fnames,itimes,trim(bbem_modis_data_dir)//'*'//cyear//cmon//cday//'*'&
                       ,maxfiles,nfiles)

   do nfn=1,nfiles
     open(iunit,status='old',file=fnames(nfn))
     call read_modis(iunit,ntfocos &
                  , fire_sensor_le_1km(QLONG)%fire_prop &
                  , fire_sensor_le_1km(QLATI)%fire_prop &
                  , fire_sensor_le_1km(QAREA)%fire_prop &
                  , fire_sensor_le_1km(QSIZE)%fire_prop &
                  , fire_sensor_le_1km(QFRPW)%fire_prop &
                  , fire_sensor_le_1km(QTIME)%fire_prop &
                  , nfocos_sensor_le_1km)
     close(iunit)
   enddo

!
!   bbem_extra_data_dir=trim(bbem_extra_data_dir)//'*'//cyear//cjday//'*'
!   print*,trim(bbem_extra_data_dir)
!   call bbbem_filelist(fnames,itimes,trim(bbem_extra_data_dir),maxfiles,nfiles)
!   
!   do nfn=1,nfiles 
!     open(iunit,status='old',file=fnames(nfn))
!     call read_extra(iunit,ntfocos,qlon,qlat,qarea,qsize,nfocos)
!     close(iunit)
!   enddo

!
!------ process  INPE fire products,  over SouthAmerica
!
   print*,trim(bbem_inpe_data_dir)//'*'//cyear//cmon//cday//'*'
   call bbbem_filelist(fnames,itimes,&
        trim(bbem_inpe_data_dir)//'*'//cyear//cmon//cday//'*',maxfiles,nfiles)
   
   do nfn=1,nfiles 
     open(iunit,status='old',file=fnames(nfn))
     call read_inpe(iunit,ntfocos                       &
                  , fire_sensor_le_1km(QLONG)%fire_prop &
                  , fire_sensor_le_1km(QLATI)%fire_prop &
                  , fire_sensor_le_1km(QAREA)%fire_prop &
                  , fire_sensor_le_1km(QSIZE)%fire_prop &
                  , fire_sensor_le_1km(QFRPW)%fire_prop &
                  , fire_sensor_le_1km(QTIME)%fire_prop &
                  , nfocos_sensor_le_1km)
     close(iunit)
   enddo

!-- performs the filtering procedure over fires from sensor with resolution <= 1 km
    call filter_fires_sensor_le_1km(                    &
                    fire_sensor_le_1km(QLONG)%fire_prop &
                  , fire_sensor_le_1km(QLATI)%fire_prop &
                  , fire_sensor_le_1km(QAREA)%fire_prop &
                  , fire_sensor_le_1km(QSIZE)%fire_prop &
                  , fire_sensor_le_1km(QFRPW)%fire_prop &
                  , fire_sensor_le_1km(QTIME)%fire_prop &
                  , nfocos_sensor_le_1km)
!
!
!--------------------- Section 3 :  Final filtering procedure and combines all fires using
!                                   only one array (fire_sensor_gt_1km)
!
    call filter_all_collected_fires(ntfocos             &
                  , nfocos_sensor_gt_1km                &
                  , fire_sensor_gt_1km(QLONG)%fire_prop &
                  , fire_sensor_gt_1km(QLATI)%fire_prop &
                  , fire_sensor_gt_1km(QAREA)%fire_prop &
                  , fire_sensor_gt_1km(QSIZE)%fire_prop &
                  , fire_sensor_gt_1km(QFRPW)%fire_prop &
                  , fire_sensor_gt_1km(QTIME)%fire_prop &
!
                  , nfocos_sensor_le_1km                &
                  , fire_sensor_le_1km(QLONG)%fire_prop &
                  , fire_sensor_le_1km(QLATI)%fire_prop &
                  , fire_sensor_le_1km(QAREA)%fire_prop &
                  , fire_sensor_le_1km(QSIZE)%fire_prop &
                  , fire_sensor_le_1km(QFRPW)%fire_prop &
                  , fire_sensor_le_1km(QTIME)%fire_prop )

!    
!-- process the source emission estimate for each valid fire count and accumulate per
!-- model grid box
!-- (todos os focos combinados (total = nfocos) estao nos arrays qlon,qlat,qarea,qsize
!    do tipo "fire_sensor_gt_1km")

    call get_emission_per_fire_count(nfocos_sensor_gt_1km&
                  , qsc,qfires                           &
                  , fire_sensor_gt_1km(QLONG)%fire_prop  &
                  , fire_sensor_gt_1km(QLATI)%fire_prop  &
                  , fire_sensor_gt_1km(QAREA)%fire_prop  &
                  , fire_sensor_gt_1km(QFRPW)%fire_prop  &
                  , fire_sensor_gt_1km(QFLAM)%fire_prop  &
                  , fire_sensor_gt_1km(QVEG )%fire_prop  &
                  , fire_sensor_gt_1km(QVEG_AGREG)%fire_prop &
		  , veg_type_data_dir,carbon_density_data_dir&
		  , fuel_data_dir)

endif !ng==1

!- performs the "gridding"
   call interp_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon,rlat,rlon,rland  &
                  , nspecies,nfocos_sensor_gt_1km        &
                  , qsc,qfires                           &
                  , fire_sensor_gt_1km(QLONG)%fire_prop  &
                  , fire_sensor_gt_1km(QLATI)%fire_prop  &
                  , fire_sensor_gt_1km(QAREA)%fire_prop  &
                  , fire_sensor_gt_1km(QSIZE)%fire_prop  &
                  , fire_sensor_gt_1km(QFRPW)%fire_prop  &
                  , fire_sensor_gt_1km(QFLAM)%fire_prop  &
                  , fire_sensor_gt_1km(QVEG )%fire_prop  &
                  , fire_sensor_gt_1km(QVEG_AGREG)%fire_prop  )


end subroutine process_bbbem
!------------------------------------------------------------------------------------------------------

subroutine get_bbbem_indentity(spc_name,ident)
!use chem1_list
use bbbem_emissions, only : bbbem_nspecies=>nspecies& !don't use AeM_nspecies
                            ,bbbem_spc_name=>AeM_spc_name
implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,bbbem_nspecies
  ident=-1
  if(spc_name == bbbem_spc_name(isp)) then
      print*,'==>bbbem found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of bbbem'
!print*,'ident=',ident
!stop 444
end subroutine get_bbbem_indentity
!------------------------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------------------------
subroutine read_modis(iunit,n,qlon,qlat,qarea,qsize,QFRPW,qtime,ic)
implicit none
character*120 dum
character (len=255)  :: header
character * 68  label,label0
real   , intent(out), dimension(n):: qlat,qlon,qarea,qsize,QFRPW,qtime
integer,  dimension(n):: itime_modis_foco
integer, intent(in) :: n,iunit
integer, intent(inout) :: ic
integer i,nobs1,ios
real area_por_foco
!-dado de M. Pereira (22.8 ha)
!area_por_foco=0.132 * 1.e+6! m^2  => 0.132 km2 por foco, estatistica WF_ABBAv60 2002
 area_por_foco=0.228 * 1.e+6! m^2       dado de M. Pereira (22.8 ha)

! read the header file and determines the time of fire ocurrence
!read(iunit,'(a)') header
!print*,header

!i = index(header,'total_number=')
!if ( i < 1 ) call die(myname,'corrupted or invalid xfires file')
!label0 = header(i+13:)
!read(label0,*,iostat=ios) nobs1

!ic = 0 >> IC nao pode ser zerado, pois esta se acumulando todos os focos!
!do i=1,nobs1
do i=1,n
 ic=ic+1
 !read(iunit,*,end=110) itime_modis_foco(ic),qlon(ic),qlat(ic)!,QFRPW(ic)
 read(iunit,*,end=110) qlon(ic),qlat(ic)!,QFRPW(ic)
enddo
110  continue
ic = ic - 1   ! numero total de focos

! size/burnt area of fire
 qsize(1:ic) = area_por_foco

 qarea(1:ic) = area_por_foco
 qtime(1:ic) = float(itime_modis_foco(1:ic))
 QFRPW(1:ic) = 0.
print*,'=> number of accumulated MODIS fires=',ic
return
end subroutine read_modis
!------------------------------------------------------------------------------------------------------
subroutine read_extra(iunit,n,qlon,qlat,qarea,qsize,QFRPW,qtime,ic,OFN)
implicit none
CHARACTER (len=*) OFN
character*120 dum
character (len=255)  :: header
character * 68  label,label0
real   , intent(out), dimension(n):: qlat,qlon,qarea,qsize,QFRPW,qtime
integer,  dimension(n):: itime_modis_foco,iveg
integer, intent(in) :: n,iunit
integer, intent(inout) :: ic
integer i,nobs1,ios,i_mean
real area_por_foco, mean_area
!-dado de M. Pereira (22.8 ha)
!area_por_foco=0.132 * 1.e+6! m^2  => 0.132 km2 por foco, estatistica WF_ABBAv60 2002
 area_por_foco=0.228 * 1.e+6! m^2       dado de M. Pereira (22.8 ha)
mean_area=0.
i_mean=0

! read the header file and determines the time of fire ocurrence
!read(iunit,'(a)') header
!print*,header

!i = index(header,'total_number=')
!if ( i < 1 ) call die(myname,'corrupted or invalid xfires file')
!label0 = header(i+13:)
!read(label0,*,iostat=ios) nobs1
!print*,nobs1
 QFRPW(:)=0.
 QTIME(:)=0.

!ic = 0 >> IC nao pode ser zerado, pois esta se acumulando todos os focos!
!do i=1,nobs1
do i=1,n
 ic=ic+1
 !read(iunit,*,end=110) itime_modis_foco(ic),qlon(ic),qlat(ic)!,QFRPW(ic)
 read(iunit,*,end=110) qlon(ic),qlat(ic),qarea(ic)
 qarea(ic)=qarea(ic)*1.e+6!m^2
 !get mean area
  if(qarea(ic)>0) then
    i_mean=i_mean+1
    mean_area=mean_area+qarea(ic)
  endif

enddo
110  continue
ic = ic - 1   ! numero total de focos

! size/burnt area of fire
! qsize(1:ic) = area_por_foco
! qarea(1:ic) = area_por_foco
! qtime(1:ic) = float(itime_modis_foco(1:ic))
if(I_MEAN > 0) mean_area=mean_area/float(I_MEAN)

print*,'=> number of accumulated MODIS fires=',ic,I_MEAN,mean_area,sum(qarea(:))
print*,'max=',maxval(qarea(:))
do i=1,ic; if(qarea(i) < 1.e-6) qarea(i)=mean_area; enddo


!- determines veg type burning to define fire-size for the SAFARI experiment
 call define_veg(ic,qlat,qlon,iveg,OFN)
 do i=1,ic
     qsize(i) = 10000. ! m2 = 10 ha   ! non-forest
     if(iveg(i) < 5) qsize(i)=20000.  ! forest
 enddo
!stop 333
return
end subroutine read_extra
!------------------------------------------------------------------------------------------------------

subroutine read_abba(iunit,n,qlon,qlat,qarea,qsize,qfrpw,qtime,ic,itime_abba)
implicit none
!character*120 dum
real, dimension(n) :: qlat,qlon,qarea,qsize,qfrpw,qtime
!integer, dimension(n) :: itime_abba_foco
integer itime_abba,idate_abba,nlinhas,n,ic
integer i,idum,iunit,fire_flag,iveg
real temp,t4,t11,satzen,pix_size
character*120 dum(5)




nlinhas = 5

! read the header file and determines the time of fire ocurrence
do i=1,3
read(iunit,'(a120)',end=110) dum(i)
!  write(11,'(a120)') dum
enddo
read(iunit,*,end=110) dum(4), idate_abba, dum(5),itime_abba
!print*,dum(4), idate_abba, dum(5),itime_abba
read(iunit,*,end=110) dum(5)


!ic = 0 >> IC nao pode ser zerado, pois esta se acumulando todos os focos!
do i=1,n
 fire_flag = 9999
 ic=ic+1  
 !srf- new version of W_ABBA fire product
 if( dum(2)(9:15) == 'WF_ABBA') then
   if( dum(2)(18:23) == 'vs 6.5') then


     read(iunit,*,end=110) qlon(ic),qlat(ic),satzen,pix_size,t4,t11 &
                      ,qarea(ic),temp,qfrpw(ic),iveg,fire_flag
     
   else ! old version

     read(iunit,*,end=110) qlon(ic),qlat(ic),t4,t11,qarea(ic),temp,iveg,fire_flag

   endif

 else

   print*,'=> No WF_ABBA fire data found in data file'

 endif
 
! print*,'qlon qlat=',qlon(ic),qlat(ic),qarea(ic),qfrpw(ic)
! size of fire 
 qsize(ic)=qarea(ic)

! itime_abba_foco(ic)=itime_abba ! hora da ocorrencia do foco
 qtime(ic) = float(itime_abba)
! Elimina focos com baixa confiabilidade IFF(fire_flag) =4 ou 5 
 if( fire_flag .gt. 3) ic = ic - 1    
enddo
110  continue
ic = ic - 1   ! numero total de focos

print*,'=> number of accumulated WF_ABBA fires=',ic
return
end subroutine read_abba
!------------------------------------------------------------------------------------------------------

subroutine read_inpe(iunit,ntfocos,qlon,qlat,qarea,qsize,qfrpw,qtime,nfocos_sensor_le_1km)
implicit none
real, dimension(ntfocos) :: qlon,qlat,qarea,qsize,qfrpw,qtime
character*10 :: sensor(ntfocos)
integer :: itime,ihour,imin,idummy,ntfocos,nfocos_sensor_le_1km,iunit,ic,i
real area_por_foco
!area_por_foco=0.132 * 1.e+6! m^2  => 0.132 km2 por foco, estatistica WF_ABBAv60 2002
 area_por_foco=0.228 * 1.e+6! m^2       dado de M. Pereira (22.8 ha)

ic=0
do i=nfocos_sensor_le_1km+1,ntfocos
  ic=ic+1  
  read(iunit,*,end=110) qlat(i),qlon(i)!, itime, sensor(i)
! 1000   format(A1,i2,i3,i2,i3,i1)
!
!-area queimada por foco
  qarea(i) = area_por_foco
!-tamanho medio do foco
  qsize(i) = area_por_foco
 

 !call date1(itime/100,idummy,ihour,imin)  !converte para hora
 !time(i)=float(ihour)+float(imin)/60.

 !print*,i,qlat(i),qlon(i),time(i),sensor(i)
 
 
enddo        

110   close(10)
!numero do focos adicionais
 ic=ic-1
 print*,'Numero de focos DSA/INPE=',ic

!- numero de total de focos (res < 1km) acumulados:
 nfocos_sensor_le_1km=nfocos_sensor_le_1km+ic
 print*,'Numero de total de focos (res < 1km) acumulados =',nfocos_sensor_le_1km

 if(nfocos_sensor_le_1km.gt.ntfocos) then
   print*,'Redefina o tamanho das matrizes para dim=',nfocos_sensor_le_1km
 stop 2225
 endif

end subroutine read_inpe
!------------------------------------------------------------------------------------------------------

subroutine filter_fires_sensor_gt_1km(ntfocos,nfocos,qlon,qlat,qarea,qsize,qfrpw,qflam&
                         ,qveg,qveg_agreg,qtime,ofn)
			 
implicit none
CHARACTER (len=*) OFN
integer i,j,ntfocos,nfocos,nfocos_iden,ncount,idummy,ihour,imin
real, dimension(ntfocos) :: qlon,qlat,qarea,qsize,qfrpw,qflam&
                           ,qveg,qveg_agreg,qtime
integer, dimension(ntfocos) :: iveg, ivcf
integer, dimension(ntfocos) :: icount,itime_abba_foco
real area_media,area_total
data  nfocos_iden/0/		      
real filt,time_filt,time_abbai,time_abbaj,time1,time2
data filt/0.1/        ! filtro em graus
data time_filt/2./    ! filtro em horas
idummy=0

!-switch para filtro
!goto 3333

!get back the time occurrence
!itime_abba_foco(1:nfocos)=nint(qtime(1:nfocos))

!-- filtra WF_ABBA e elimina focos identicos
 print*,'=========================================================='   
 print*,' Filtra WF_ABBA e elimina focos identicos '
do i=1,nfocos                                ! loop nos focos 
   !print*,'foco=',i

   icount(i)=1
   nfocos_iden = 0
   if(qlon(i) .lt. 9990.) then
     call define_veg(1,qlat(i),qlon(i),iveg(i),ofn)     
     !print*,' '
     !print*,'=========================================================='   
     !print*,'-----FOCO = ',i,'qveg=',qveg(i),'time=',itime_abba_foco(i)
     !print*,'qlat qlon qarea= ',qlat(i),qlon(i),qarea(i)*1.e+2
   endif

!filtro veg: se veg nao for floresta, focos identicos sao considerados como as frentes 
!            de fogo em savana,pastagem, cerradao, etc
!            => a area queimada e' igual a soma (ou, equivalentemente, mantem o foco)
   if(iveg(i) > 4) goto 333


!filtro temporal para florestas:
   call date1(nint(qtime(i)),idummy,ihour,imin)
   time_abbai=float(ihour)+float(imin)/60.

   do j=1,nfocos                             !loop nos focos GOES
      if(j == i .or.qlon(j) .gt. 9990.)  cycle  ! evita focos ja eliminados

!
!fitro espacial ---------------
      if( abs(qlon(i)-qlon(j)) .le. .5*filt  .and. &  
          abs(qlat(i)-qlat(j)) .le. .5*filt        )     then          


!fitro temporal : se veg=floresta e dtime < 2 horas => mesmo foco ainda em atividade
!                => area queimada = maior area
!
         call date1(nint(qtime(j)),idummy,ihour,imin)
         time_abbaj=float(ihour)+float(imin)/60.
         if(abs(time_abbai-time_abbaj) .gt. time_filt/2.) cycle
       
!
	 nfocos_iden=nfocos_iden+1	 
         !if(qarea(j) > 0) then
	 ! print*,'focos-identicos:',i,j,nfocos_iden
         ! print*,'foc-ident:',qlat(j),qlon(j),qarea(j)*1.e+2,itime_abba_foco(j)
	 !endif
 
         qlon(j)= 9999. 		    !=> focos identicos eliminados 
	 qlat(j)= 9999.
!versao 1:soma
!         if(qarea(j) > 0.) qarea(i) = max(0.,qarea(i))+qarea(j)
!versao 2:maior
         if(qarea(j) > qarea(i)) qarea(i) =  qarea(j)	

!tamanho do fogo para plumerise
! testar no futuro
!        if(qsize(j) > 0.) qsize(i) = max(0.,qsize(i))+qsize(j)
                           qsize(i) = qarea(i)
!

	 icount(i)=icount(i)+1
	 cycle
	   
       endif
!     endif
   enddo

333 continue
   if(qlon(i) .lt. 9990.) then
   ! print*,'area final (ha)=',qarea(i)*1.e+2,'nfocos_iden=',icount(i)-1
   ! print*,'=========================================================='   
   endif
   
enddo 

3333 continue


! calcula o tamanho medio do foco 
! testar no futuro
!do i=1,nfocos
!   if(qlon(i) .lt. 9990.) then
!   qsize(i)=qsize(i)/float(icount(i))   
!   endif
!enddo

!reorganiza os focos 
j=0
do i=1,nfocos
   if(qlon(i) .lt. 9990.) then
   j=j+1
   qlon(j)  =qlon(i)
   qlat(j)  =qlat(i)
   qarea(j) =qarea(i)
   icount(j)=icount(i)
   qsize(j) =qsize(i)
   qtime(j) =qtime(i)
   endif
enddo
!numero final de focos diferentes;
nfocos=j
!-media dos focos:
area_media=0.
ncount=0
do i=1,nfocos
 if( qarea(i) .gt. 0.) then
   area_media=area_media+qarea(i)
   ncount=ncount+1
 endif
enddo
if(ncount>0)then 
 area_media=area_media/ncount
! print*,'area media por foco (ha)=',area_media*1.e+2
! print*,'=========================================================='   
endif

area_total=0.
do i=1,nfocos
! if( qarea(i) .lt. 0.) qarea(i) = 0.128 ! substituindo dado de area faltante por
! if( qsize(i) .lt. 0.) qsize(i) = 0.128 ! 0.128 km2 por foco
	 			        ! estatistica WF_ABBAv60 2002
 if( qarea(i) .le. 0.) qarea(i) = area_media ! substituindo dado de area faltante por
 if( qsize(i) .lt. 0.) qsize(i) = area_media ! pela area media do dia

 area_total=area_total + qarea(i)       ! area total queimada com o WFABBA

 qarea(i) = qarea(i) * 1.e+6            ! converte de km^2 para m^2
 qsize(i) = qsize(i) * 1.e+6


enddo

!print*,'============ Focos WFABBA - final ========================'
!do i=1,nfocos
!   print*,i, qlat(i),qlon(i),icount(i),qsize(i),qarea(i)/1.e+4,'ha'
!enddo
 print*,'Numero final de focos WFABBA       =',nfocos
 print*,'area total queimada com WFABBA (ha)=',area_total*1.e+2,' em km^2:',area_total
 print*,'usando',area_media*1.e+2,' ha para focos com valores nao definidos'
 print*,'=========================================================='   
return
end subroutine filter_fires_sensor_gt_1km
!-------------------------------------------------------------------------------------
!
subroutine filter_fires_sensor_le_1km(qlon,qlat,qarea,qsize,qfrpw,qtime,nfocos_sensor_le_1km)
implicit none
real, dimension(nfocos_sensor_le_1km) :: qlon,qlat,qarea,qsize,qfrpw,qtime
!character*10 :: sensor(nfocos_sensor_le_1km)
integer :: i,j,itime,ihour,imin,idummy,nfocos_sensor_le_1km,nfocos_iden
!integer, dimension(nfocos_sensor_le_1km):: qveg,flag
!character*25, dimension(nfocos_sensor_le_1km) :: a1
real filt
!data filt/0.0043/    ! filtro em graus (430m x 430m => area media de ~ 22.8 ha
 data filt/0.0090/    ! filtro distancia menor que 1 km



!filtra focos subsequentes iguais

nfocos_iden =0
do i=1,nfocos_sensor_le_1km
   if(qlon(i) .gt. 9990.) go to 555          ! evita focos ja eliminados
!  if(qlon(i) .gt. 0) go to 555 ! nao aplique o filtro sobre a Africa


   do j=1,nfocos_sensor_le_1km

   if(j == i) cycle

!    if(qlon(i) .lt. 0) then
     if( (abs(qlon(i)-qlon(j)) .le. filt .and.   &
          abs(qlat(i)-qlat(j)) .le. filt   )	  &
! filtro de hora/sensor	  
!	 .and. a1(i) == a1(j)                       &
	                                            ) then 
         
	 nfocos_iden=nfocos_iden+1
	 
         !print*,'---MODIS : Focos identicos  --------',nfocos_iden
         !print*,qlon(j),qlat(j),qlon(i),qlat(i)
	 
           qlon(i)= 9999.  !=> focos identicos eliminados pelo filtro
	   qlat(i)= 9999.

	   go to 555
	   
       endif
    enddo
555 continue
enddo    

! determines the final number of fires filtered out
j=0
do i=1,nfocos_sensor_le_1km
   if(qlon(i) .lt. 9990.) then
   j=j+1
   qlon (j) = qlon (i)
   qlat (j) = qlat (i)
   qarea(j) = qarea(i) 
   qsize(j) = qsize(i) 
   
   endif
enddo
!numero final de focos diferentes;
nfocos_sensor_le_1km=j

print*,'--------------------------------------------------------------------------'
print*,'subroutine filter_fires_sensor_le_1km - nfocos_sensor_le_1km:'
print*,'initial=',nfocos_sensor_le_1km+nfocos_iden,' final=',nfocos_sensor_le_1km
print*,'filter =',filt*111., 'km radius'
print*,'--------------------------------------------------------------------------'
end subroutine filter_fires_sensor_le_1km
!--------------------------------------------------------------------------------
!
subroutine filter_all_collected_fires(ntfocos                                             &
            ,nfocos,       qlon,       qlat,       qarea,       qsize    ,  qfrpw         &
	    ,qtime                                                                        &
            ,nfocos_le_1km,qlon_le_1km,qlat_le_1km,qarea_le_1km,qsize_le_1km,qfrpw_le_1km &
	    ,qtime_le_1km  )
implicit none
integer i,j,ntfocos,nfocos,nfocos_le_1km,itime_abba,ihour,imin,ic
real, dimension(ntfocos) :: qlon,qlat,qarea,qlon_le_1km,qlat_le_1km,qarea_le_1km &
                           ,qtime_le_1km,qsize,qsize_le_1km,qfrpw,qfrpw_le_1km, qtime 

integer nfocos_filt
real, dimension(ntfocos) :: qlon_le_1km_filt,qlat_le_1km_filt,qarea_le_1km_filt  &
                           ,qsize_le_1km_filt,qfrpw_le_1km_filt, qtime_le_1km_filt 

!character*10 :: sensor_le_1km(ntfocos)

integer :: nfocos_iden		 
data  nfocos_iden/0/		      
real filt,time_filt,time_abba,time1,time2
data filt/0.05/        ! filtro em graus aprox. 5 km
data time_filt/3./     ! filtro em horas

!switch para filtro
!!!!return

!call date1(itime_abba,i,ihour,imin)
!time_abba=float(ihour)+float(imin)/60.
!    janela de exclusao
!time1= max(0., time_abba-time_filt/2.)
!time2= min(24.,time_abba+time_filt/2.)


!-   Compara WF_ABBA x INPE e elimina focos identicos
print*,'=========================================================='   
print*,'------- Compara WF_ABBA x INPE e elimina focos identicos----'

do i=1,nfocos_le_1km               ! loop nos focos le_1km

    if(qlon_le_1km(i) .gt. 9990.) cycle     ! evita focos ja eliminados
                  
    do j=1,nfocos                   !loop nos focos GOES

!       print*,qlon(j),qlat(j),qlon_le_1km(i),qlat_le_1km(i)

!fitro temporal ---------------
    
!     if(time_le_1km(i) .ge. time1 .and. time_le_1km(i) .le. time2) then 
    
!
!fitro espacial ---------------
      if(abs(qlon_le_1km(i)-qlon(j)) .le. filt .and.	  &
         abs(qlat_le_1km(i)-qlat(j)) .le. filt  	  ) then 
         
	 nfocos_iden=nfocos_iden+1
	 
       !print*,'------- Focos identicos  AVHRR/MODIS X WFABBA ----'
       !print*,nfocos,nfocos_le_1km,nfocos_iden
       !print*,qlon(j),qlat(j),qlon_le_1km(i),qlat_le_1km(i)
	 
           qlon_le_1km(i)= 9999.                     !=> focos identicos eliminados do AVHRR
	   qlat_le_1km(i)= 9999.

	   go to 555
	   
       endif
!     endif
   enddo
555 continue
enddo    


nfocos_filt=0
ic=1
do i=1,nfocos_le_1km
!print*,'****',i,qlon_le_1km(i), qlon_le_1km(i)

	if(qlon_le_1km(i) .lt. 9990.)  then  !  foco nao identico => transfere para nfocos_filt

	     qlon_le_1km_filt (ic) =   qlon_le_1km (i)
	     qlat_le_1km_filt (ic) =   qlat_le_1km (i)
	    qarea_le_1km_filt (ic) =  qarea_le_1km (i)
	    qsize_le_1km_filt (ic) =  qsize_le_1km (i)
	                       ic  =  ic + 1
	          nfocos_filt      =  nfocos_filt + 1
	   
	endif
enddo

!- combine all focos in the only one array (for save memory they will be combined at the fire_sensor_gt_1km)

if(nfocos_filt + nfocos .gt. ntfocos) stop 'nfocos_filt+nfocos > ntfocos'
					   
ic=nfocos
do i=1,nfocos_filt

       ic=nfocos+i
            
      qlon(ic) =    qlon_le_1km_filt (i) 
      qlat(ic) =    qlat_le_1km_filt (i) 
     qarea(ic) =   qarea_le_1km_filt (i) 
     qsize(ic) =   qsize_le_1km_filt (i) 

!      qlon_gt_1km(ic) =    qlon_le_1km_filt (i) 
!      qlat_gt_1km(ic) =    qlat_le_1km_filt (i) 
!     qarea_gt_1km(ic) =   qarea_le_1km_filt (i) 
!     qsize_gt_1km(ic) =   qsize_le_1km_filt (i) 
	   
enddo
nfocos   = ic 
print*,'total number of fires filtered out is=', nfocos
print*,'=========================================================='   

end subroutine filter_all_collected_fires
!--------------------------------------------------------------------------------
!
subroutine get_emission_per_fire_count(nfocos,qsc,qfires,qlon,qlat,qarea,qfrpw,qflam &
                           ,qveg,qveg_agreg                                   &
                           ,veg_type_data_dir,carbon_density_data_dir,fuel_data_dir)
use AeM_emission_factors,  nspecies=>AeM_nspecies, N2_nitrogenio=>N2
use bbbem_plumerise, only: nveg_agreg,flaming
implicit none
integer, parameter :: nveg=17, nveg_olson=97
integer :: nfocos,ifoc,i,iesp,nc
CHARACTER (len=*) veg_type_data_dir,carbon_density_data_dir,fuel_data_dir
real,    dimension(nfocos) :: qlon,qlat,qarea,qfrpw,qflam &
                             ,qveg,qveg_agreg        
real,    dimension(nfocos,nspecies) :: qsc
real,    dimension(nfocos) :: qfires
integer, dimension(nfocos) :: iveg
real,    dimension(7,nveg) :: queiparms
real,    dimension(nveg)   :: fc,bas_queiparms  !,efco2,efco,efbc,efoc,efso2
real :: fx

!- OLSON's data base
integer olson; data olson /1/
real, dimension(nveg_olson) :: bas_olson
integer, dimension(nfocos)  :: iveg_olson
! ABG final
real, dimension(nfocos)     :: bas_by_fire
real biomass_burned

!-para Amazon/Alaska's fuel data base
integer fuel; data fuel /1/

!-para fuel load 1999-2000 SAFARI DATA
integer flsafari; data flsafari /0/
CHARACTER (len=256) flsafari_data_dir

!- para SPOT-VEG 
!integer i_spotveg; data i_spotveg /0/
!integer, dimension(nfocos)  :: iveg_spotveg

! plume model
integer, dimension(nfocos) :: iveg_agreg


!-------------------------------------------------------------------------------
data queiparms/  &
! dados de Andreae & Merlet(A&M), Ward et al., Kauffman (unpubl.), Sinha et al, 2003, CAPOS 2004
! Palacios-Orueta et al, Barbosa e Fearnside
!--------------------------------------------------------------------------------------------------------
!fator  !Biomassa   !EF    !EF     !EF    !EF    !EF   !IGBP Land Cover              ! observation
!de com-!acima do   !g/kg  !g/kg   !g/kg  !g/kg  !g/kg !Legend and                   ! reference
!bustao !solo(kg/m2)!CO2   !CO     !BC    !OC   !SO2  !description                  ! 
!-------------------------------------------------------------------------------------------------------
0.50,	 29.24,     1568., 106.0,  .56,  9.1,  1.00, &! 1 Evergreen Needleleaf Fores !A&M
0.50,	 29.24,     1527., 128.8,  .63,  5.2,  0.57, &! 2 Evergreen Broadleaf Forest !CAPOS 2004
0.43,	 12.14,     1639.,  99.0,  .46,  3.2,  0.35, &! 3 Deciduous Needleleaf Forest!  
0.43,	 12.14,     1639.,  99.0,  .46,  3.2,  0.35, &! 4 Deciduous Broadleaf Forest !  
0.43,	 12.14,     1639.,  99.0,  .46,  3.2,  0.35, &! 5 Mixed Forest		   !		     
0.87,	  7.40,     1700.,  68.0,  .46,  3.2,  0.35, &! 6 Closed Shrublands =Caatinga!Kauffman, Sinha et al, 2003
0.72,	  0.86,     1700.,  68.0,  .46,  3.2,  0.35, &! 7 Open Shrublands	     !Sinha et al, 2003
0.45,	  10.0,     1700.,  68.0,  .46,  3.2,  0.35, &! 8 Woody Savannas	     !Sinha et al, 2003
0.52,	  6.20,     1700.,  68.0,  .46,  3.2,  0.35, &! 9 Savannas		     !Sinha et al, 2003 	 
1.00,	  0.71,     1700.,  68.0,  .46,  3.2,  0.35, &!10 Grasslands		     !Sinha et al, 2003   
0.40,	  3.80,     1700.,  68.0,  .46,  3.2,  0.35, &!11 Permanent Wetlands	
0.40,	  3.80,     1700.,  68.0,  .46,  3.2,  0.35, &!12 Croplands	 Palacios-orueta.
0.40,	  3.80,     1700.,  68.0,  .46,  3.2,  0.35, &!13 Urban and Built-Up	
0.40,	  3.80,     1700.,  68.0,  .46,  3.2,  0.35, &!14 Cropland/Natural Veg.Mosaic Palacios-orueta
0.00,	  0.00,     0000.,  00.0,  .00,  0.0,  0.00, &!15 Snow and Ice
0.84,	  1.00,     1723.,  57.5,  .46,  3.2,  0.35, &!16 Barren or Sparsely Vegetated 
0.00,	  0.00,     0000.,  00.0,  .00,  0.0,  0.00  /!17 Water Bodies
!-------------------------------------------------------------------------------------------------------


!- avoid negative values for emission factor
where(emission_factor <0.) emission_factor=0. 

!
do i=1,nveg
fc(i)             = queiparms(1,i)        ! fracao de biomassa queimada
bas_queiparms(i)  = queiparms(2,i)        ! kg/m2
!efco2(i)          = queiparms(3,i)/1000.  ! kg[CO2] /kg[biomassa queimada]
!efco(i)           = queiparms(4,i)/1000.  ! kg[CO]  /kg[biomassa queimada]
!efbc(i)           = queiparms(5,i)/1000.  ! kg[bc]/kg[biomassa queimada]
!efoc(i)           = queiparms(6,i)/1000.  ! kg[oc] /kg[biomassa queimada]
!efso2(i)          = queiparms(7,i)/1000.  ! kg[SO2] /kg[biomassa queimada]
enddo

bas_by_fire(:) = 0.
 
!- determines veg type burning
 call define_veg(nfocos,qlat,qlon,iveg,veg_type_data_dir)

!- performs fire aggregation by biome type
! new version
 call agreg_veg(nfocos,qlat,qlon,iveg,iveg_agreg)
!oper version
! call agreg_veg_oper(nfocos,qlat,qlon,iveg,iveg_agreg)

if(olson == 1 .or. olson == 2) then
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 if( olson == 1 ) then
!- determines carbon density (aboveground) using Olson's database
 	call define_veg(nfocos,qlat,qlon,iveg_olson,carbon_density_data_dir)
 	call define_bas_olson(bas_olson,nveg_olson,nfocos,bas_by_fire,iveg_olson)
 else if (olson == 2) then
 	call define_bas_olson2(qlat,qlon,nfocos,bas_by_fire)
 endif
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!- fixing ABG dry matter over the Amazon with Houghton's or Saatachi data base
! if(houghton_Saatchi == 1)  then
    !for Houghton use this
    !call define_bas_houghton(nfocos,bas_by_fire,iveg,qlat,qlon,houghton_data_dir)
    !for Saatachi use this
    !call define_bas_Saatachi(nfocos,bas_by_fire,iveg,qlat,qlon,houghton_data_dir)
 !endif
 nc=len_trim(fuel_data_dir)
 if(fuel_data_dir(nc-19:nc) == 'glc2000_fuel_load.nc') then
    !- fixing ABG dry matter over Alaska
    call define_bas_fuel_alaska(nfocos,bas_by_fire,iveg,qlat,qlon,fuel_data_dir)
 elseif(fuel_data_dir(nc-23:nc) == 'amazon_biomass_final.gra') then
    !- fixing ABG dry matter over Amazon
    if(fuel == 1) call define_bas_fuel_amazon(nfocos,bas_by_fire,iveg,qlat,qlon,fuel_data_dir)
 else
     print*,'fuel data=',fuel_data_dir(1:nc)
     print*,'no special/regional fuel data available - only global data will be used' 
 endif
 
 
 !- fixing ABG dry matter over the SAFARI 2000 area
 ! flsafari_data_dir = './' ! just for now.
 ! if(flsafari == 1) call define_bas_flsafari(nfocos,bas_by_fire,iveg,qlat,qlon,flsafari_data_dir)

 
 !-usando os valores de literatura(bas_queiparms) para o bas_by_fire sobre a AFRICA:
 ! do i=1,nfocos  
 !    if(qlon(i) .lt. -28.) cycle ! somente sobre a Africa
 !    bas_by_fire(i) = bas_queiparms(iveg(i))
 ! enddo
else
 !
 !- usando  dados  da tabela 'queiparms' para densidade de biomassa acima do solo
 iveg_olson(:)=-9999
 do i=1,nfocos  
     bas_by_fire(i) = bas_queiparms(iveg(i))

 !- usando base de dados de houghton para densidade de biomassa acima do solo
 !- para florestas somente
 !    if(houghton == 1) call define_bas_houghton(nfocos,bas_by_fire,iveg,qlat,qlon)
     if(fuel == 1) call define_bas_fuel_amazon(nfocos,bas_by_fire,iveg,qlat,qlon,fuel_data_dir)
 enddo

endif


!- calculates the emission by fire count
do ifoc = 1,nfocos
      
      if(iveg_agreg(ifoc) == 0) cycle ! eliminates false detection of fires (over ocean/water
                                      ! bodies/iced land)   
     
      
      biomass_burned =  fc(iveg(ifoc))*bas_by_fire(ifoc)*qarea(ifoc) ! kg[dry biomass burned]
      qflam(ifoc)    =  flaming(iveg_agreg(ifoc))
      
      if(bas_by_fire(ifoc) > 0.) then
        qfires(ifoc) = 1.
      else
        qfires(ifoc) = 0.
      endif
      fx=1.
!---  aumentando emiss�es 
!      if(qlon(ifoc) > -20.) then 
!       fx=2.  ! Africa
!      else
       fx=1.3 !S.America
!      endif

      biomass_burned =  fx*biomass_burned
!-----

      
      do iesp=1,nspecies
      
          qsc(ifoc,iesp) =  biomass_burned * emission_factor(iveg_agreg(ifoc),iesp)*1.e-3 !kg[spc]
       !  qsc(ifoc,iesp) =  biomass_burned * emission_factor(iveg      (ifoc),iesp)*1.e-3 !kg[spc]
      
     
!if(AeM_spc_name(iesp)=='CO')then
!       print*,AeM_spc_name(iesp),iveg(ifoc),iveg_agreg(ifoc),iveg_olson(ifoc),bas_by_fire(ifoc)
!       print*,'===',qsc(ifoc,iesp),AeM_spc_name(iesp)
!       print*,'flam=',qflam(ifoc),iveg_agreg(ifoc),emission_factor(iveg_agreg(ifoc),iesp)
!endif

       enddo
enddo
!- copy veg and veg_agreg integer data into real arrays
qveg      (1:nfocos)=float(iveg      (1:nfocos))
qveg_agreg(1:nfocos)=float(iveg_agreg(1:nfocos))
end subroutine get_emission_per_fire_count
!------------------------------------------------------------------------------------------------------

subroutine interp_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon,rlat&
                         ,rlon,rland,nspecies_actual,nfocos,qsc,qfires,qlon,qlat,qarea,qsize &
			 ,qfrpw,qflam,qveg,qveg_agreg)
use grid_dims_out, only: grid_type,use_bbem_plumerise
use bbbem_emissions
use bbbem_plumerise
use mem_grid, only : grid_g
implicit none
integer :: ifoc,ng,n1,n2,n3,igbox,jgbox,iesp,nspecies_actual,nfocos,iveg_ag,i,j,icount
real deltax,deltay,plat,plon
!integer, dimension(nfocos) :: iveg
real,    dimension(nfocos) :: qlon,qlat,qarea,qsize &
			     ,qfrpw,qflam,qveg,qveg_agreg ! por foco 
real, dimension(nfocos,nspecies_actual) :: qsc
real xt(n1),yt(n2),xm(n1),ym(n2)
real, dimension(n1,n2):: rlat,rlon,rland
real, allocatable, dimension(:,:):: grid_area
real,    dimension(nfocos) :: qfires 
! plumeriselocal vars
integer, parameter :: get_STD_of_size = 0! 1=on,0=off: use this for STD calculation of fire size
integer, parameter :: nfgbox=100                     ! nfocos_by_gridbox
real,allocatable,dimension(:,:,:,:) :: qarea_I_agreg ! burned area by gridbox/agreg biome
!real mtotal(3)
real wlon,elon,xlon,splon       !tks

!- allocate memory for plumerise arrays
if(use_bbem_plumerise == 1) call mem_bbbem_plume(n1,n2,n3)

if(use_bbem_plumerise == 1 .and. get_STD_of_size == 1 ) &
        allocate(qarea_I_agreg(n1,n2,nveg_agreg,nfgbox))

wlon=minval(rlon)   !tks
elon=maxval(rlon)   !tks

do ifoc = 1,nfocos
!- define veg_agreg
   iveg_ag = nint(qveg_agreg(ifoc))
   if(iveg_ag < 1 .or. iveg_ag > 4) cycle ! incompatable place for fires

   if(grid_type == 'rams' .or. grid_type == 'polar') then
   	   call get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay&
   			   ,qlon(ifoc),qlat(ifoc),igbox,jgbox)
   elseif(grid_type == 'lambert' .or. grid_type == 'mercator' ) then              !tks
           xlon=qlon(ifoc)                          !tks
           if(wlon.lt.-180)then
             splon=wlon+360.                        !tks
             if(xlon.ge.splon)xlon=xlon-360.        !tks
           endif
           if(elon.gt.180)then
             splon=elon-360.                        !tks
             if(xlon.le.splon)xlon=xlon+360.        !tks
           endif
  	   call get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay&
   			   ,xlon,qlat(ifoc),igbox,jgbox)
   elseif(grid_type == 'll') then
   	   call get_ij_ll(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'fim') then
   	   call get_ij_fim(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

   elseif(grid_type == 'gg') then
   	   call get_ij_gg(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)
   else
    stop 'unknown grid type at 3bem emissions '
   endif
!
   if(igbox == -1 .or. jgbox == -1 ) cycle

   do iesp=1,nspecies_actual
!- total mass emitted (flaming+smoldering) per gridbox
     bbbem_g(iesp)%src(igbox,jgbox,1)               = bbbem_g(iesp)%src(igbox,jgbox,1) &
     
                                                    + qsc(ifoc,iesp)						      
!-  mass emitted (during the flaming phase) per gridbox 
     if(use_bbem_plumerise /= 1) cycle
     bbbem_plume_g(iesp,iveg_ag)%src(igbox,jgbox,1) = bbbem_plume_g(iesp,iveg_ag)%src(igbox,jgbox,1) &

                                                    + qsc(ifoc,iesp)*flaming(iveg_ag)
   enddo
   
   if(use_bbem_plumerise /= 1) cycle
!------------
!-fires properties : accumulated size    
    bbbem_plume_fire_prop_g(qarea_agreg,iveg_ag)%fire_prop(igbox,jgbox)  =              &
    bbbem_plume_fire_prop_g(qarea_agreg,iveg_ag)%fire_prop(igbox,jgbox)  + qsize (ifoc)
     
!-fires properties : fire number
    bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) =              &
    bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) + qfires (ifoc)

  if(get_STD_of_size == 1 .and.   qsize (ifoc) > 0. .and. &
     bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox) > 0.) &

      qarea_I_agreg( igbox,jgbox,iveg_ag,&
                     nint(bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(igbox,jgbox)))&
		     =qsize (ifoc)
!------------
!
enddo

!
if(use_bbem_plumerise == 1) then

!- produce some statistical quantities for the plumerise model
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
! get the STD of fire size, if necessary
      if (get_STD_of_size == 1) then
      !  loop over all fires aggregate within a grid box (i,j)   => STD of size    
        do ifoc=1,nint(bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j))
         
!orig    qarea_std_agreg(i,j,iveg_ag) = qarea_std_agreg(i,j,iveg_ag) + &
!orig                (qarea_I_agreg(i,j,iveg_ag,ifoc) - qarea_agreg(i,j,iveg_ag)) **2
	 
	   bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  =  &
	   bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  +  &
           ( qarea_I_agreg(i,j,iveg_ag,ifoc)                                -  & 
	     bbbem_plume_fire_prop_g(qarea_agreg ,iveg_ag)%fire_prop(i,j) )**2

        enddo
!orig  qarea_std_agreg(i,j,iveg_ag) = sqrt(    qarea_std_agreg(i,j,iveg_ag)  / &
!orig                                        (1.e-4+qfires_agreg(i,j,iveg_ag)) )
            bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)  =  &
       sqrt(bbbem_plume_fire_prop_g(qarea_std_agreg,iveg_ag)%fire_prop(i,j)) /  &
            (1.e-4+bbbem_plume_fire_prop_g(qfires_agreg,iveg_ag)%fire_prop(i,j))
      endif
!
!- convert from mass consumed during the flaming phase to fraction of the total mass consumed
     
      
      do iesp=1,nspecies_actual
	 if(bbbem_g(iesp)%src(i,j,1) > 0.) &	      
	 
!orig      qsco_agreg  (i,j,iveg_ag) = qsco_agreg  (i,j,iveg_ag) /qsco (i,j)
!- fraction consumed at the flamin phase:
           bbbem_plume_g(iesp,iveg_ag)%src(i,j,1) = bbbem_plume_g(iesp,iveg_ag)%src(i,j,1) &
                                                  / bbbem_g(iesp)%src(i,j,1)
      enddo
    enddo 
 enddo
enddo
!-new section
!- calculate the mean value for bbbem_plume_g (to save memory)
do i=1,n1
  do j=1,n2
   do iveg_ag=1,nveg_agreg
         icount = 0
         bbbem_plume_mean_g(iveg_ag)%src(i,j,1)=0.
          
	  do iesp=1,nspecies_actual
             if(bbbem_g(iesp)%src(i,j,1) > 0.) then
	     	  icount = icount + 1
                  bbbem_plume_mean_g(iveg_ag)%src(i,j,1) = bbbem_plume_mean_g(  iveg_ag)%src(i,j,1)+ &
                                                           bbbem_plume_g(iesp,iveg_ag)%src(i,j,1)
             endif
	  enddo
          bbbem_plume_mean_g(iveg_ag)%src(i,j,1) = bbbem_plume_mean_g(iveg_ag)%src(i,j,1)/(float(icount)+1.e-10)
    enddo
  enddo
enddo

endif

!don't change of place these lines below ...
!-------------- convert to surface flux (kg/m^2)
!- calculate the grib box area
allocate(grid_area(n1,n2))
if(grid_type == 'rams' .or. grid_type == 'polar') then
         call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
 elseif(grid_type == 'lambert' .or. grid_type == 'mercator' ) then
        grid_area(:,:)=1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
 elseif(grid_type == 'll') then
         call get_area_ll(grid_area,n1,n2) 
 elseif(grid_type == 'fim') then
         call get_area_fim(grid_area,n1,n2) 
 elseif(grid_type == 'gg') then
         call get_area_gg(grid_area,n1,n2,rlat,rlon) 
 else
       stop 'unknown grid type at 3bem emissions '
endif
! convert from mass [kg/day] to flux [kg/m^2/day]
do iesp=1,nspecies_actual
      bbbem_g(iesp)%src(:,:,1) = bbbem_g(iesp)%src(:,:,1)/grid_area(:,:)
enddo
!  
deallocate(grid_area)

!do i=1,n1
! do j=1,n2
!  
!  if(qsco(i,j) > 0.001)then
!  print*,'--------GRID BOX--------------',i,j
!  print*,'CO area fires=',qsco(i,j),qspm25(i,j)!,qnfires(i,j),qbarea(i,j)
!  print*,'   '
!  mtotal(:)=0.  
!  do iveg=1,3
!  mtotal(1) = mtotal(1) + qsco_agreg(i,j,iveg)
!  mtotal(2) = mtotal(2) + qfires_agreg(i,j,iveg)
!  mtotal(3) = mtotal(3) + qarea_agreg(i,j,iveg)
!  print*,'agregados, veg=',iveg
!  print*,'CO area fires=',qsco_agreg(i,j,iveg),qspm25_agreg(i,j,iveg)
! enddo
!  print*,' CHECK:	 ',mtotal(1),mtotal(1)/qsco(i,j)
!  do iveg=1,3
!   do ifoc=1,int(qfires_agreg(i,j,iveg))
!    print*,iveg,ifoc,qarea_I_agreg(i,j,iveg,ifoc)
!   enddo
!   print*,'MEAN STD:',qarea_agreg(i,j,iveg),qarea_std_agreg(i,j,iveg)
!  enddo
! 
!  !if(abs(mtotal(1) - qsco(i,j)) > 0.001 ) print*,'******************X', mtotal(1) - qsco(i,j)
!  !if(abs(mtotal(2) - qnfires(i,j)) > 0.001 ) print*,'******************Y', mtotal(2) - qnfires(i,j)
!  !if(abs(mtotal(3) - qbarea(i,j)) > 0.001 ) print*,'*****************Z', mtotal(3) - qbarea(i,j)
!endif
!enddo
!enddo

end subroutine interp_to_model
!------------------------------------------------------------------------------------------------------

subroutine get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay,qlon&
                      ,qlat,igbox,jgbox)
use mem_grid, only : stdlat1,stdlat2                              !tks
use grid_dims_out, only: grid_type
implicit none
integer, intent(in) ::n1,n2
real, intent(in) :: xt(n1), yt(n2),xm(n1), ym(n2)
real, intent(in) :: plat,plon,qlon,qlat,deltax,deltay

integer, intent(out) :: igbox,jgbox
!local var
integer :: i,j,i1,j1,i2,j2,ix,jx
real qx,qy, dx,dy,  dxm ,dym,xx
integer igboxx,jgboxx
! Transforma qlat e qlon para polar estereografico.        
!tks      call ge_to_xy(plat,plon,qlon,qlat,qx,qy)   !orig
        if(grid_type .eq. 'lambert')then
          call ll_lc2(qlat,qlon,plat,plon,stdlat1,stdlat2,qx,qy)   !tks
        elseif(grid_type .eq. 'mercator')then
          call llij_merc(qlat,qlon,plat,plon,stdlat1,qx,qy)   !tks
        else
          call ge_to_xy(plat,plon,qlon,qlat,qx,qy)
        endif

!- 10012008
!-use this only for grid with cte spacing

!-new/faster way
        !igbox =   (nint((qx-xt(1))/deltax)) + 1        
        !jgbox =   (nint((qy-yt(1))/deltay)) + 1
	!if(igbox .lt. 1 .or. igbox .gt. n1)  igbox = -1
	!if(jgbox .lt. 1 .or. jgbox .gt. n2)  jgbox = -1
        !return


!- use this for generic grid
!-------------------------------- old way -------------- 
! Define grid box
	do i = 1,n1
	if(qx.le.xt(i)) goto 100
	enddo
	goto 300
  100	continue
  	i1 = i - 1
  	i2 = i
!
	do j = 1,n2
	if(qy.le.yt(j)) goto 200
	enddo
	goto 300
  200	continue
  	j1 = j - 1
  	j2 = j
	
	if(i1 .lt. 1 .or. i2 .gt. n1) go to 300
	if(j1 .lt. 1 .or. j2 .gt. n2) go to 300

!	escolha da localizacao do foco no rams
!

 dx = qx - xt(i1)
 dy = qy - yt(j1)
 dxm = 0.5 * (xt(i2) - xt(i1)) !deltax/2.
 dym = 0.5 * (yt(j2) - yt(j1)) !deltay/2.
!
 if(dx.le.dxm.and.dy.le.dym) then
                           igbox = i1
			   jgbox = j1
 endif			   
  
 if(dx.gt.dxm.and.dy.lt.dym) then
                           igbox = i2
			   jgbox = j1
 endif			   

 if(dx.le.dxm.and.dy.gt.dym) then
                           igbox = i1
			   jgbox = j2
 endif			  

 if(dx.gt.dxm.and.dy.gt.dym)  then
                           igbox = i2
			   jgbox = j2
 endif	
!print*,'box=',igbox,jgbox
!print*,'2',igbox,jgbox

!if(jgbox /= jgboxx) stop 22
!if(igbox /= igboxx) stop 23

return
 
 
300 continue
!- fire out of the model domain
     igbox = -1
     jgbox = -1

 		 
end subroutine get_ij_rams
!------------------------------------------------------------------------------------------------------
!
subroutine  agreg_veg_oper(nfocos,qlat,qlon,qveg,qveg_agreg)
implicit none
integer ifoc,nfocos
real,    dimension(nfocos) :: qlat,qlon
integer, dimension(nfocos) :: qveg,qveg_agreg
integer catb(1:17)
data catb/ &
           1, 1, 1, 1                 & !floresta: 1 a 4
!srf-for NOAA      2, 1, 2, 1                 & !floresta tropical 2 and 4 / extra trop fores 1,3,5
!srf-for NOAA  	 , 2, 3, 3, 3, 3              & !cerrado/woody savanna :6 a 9
	 , 1, 3, 3, 3, 3              & !cerrado/woody savanna :6 a 9

         , 4, 4, 4, 4, 4, 0, 4, 0     / !pastagem/lavouras: 10 ...

do ifoc=1,nfocos
   qveg_agreg(ifoc) =catb( qveg(ifoc) )
enddo

return
end
!------------------------------------------------------------------------------------------------------
!
subroutine  agreg_veg(nfocos,qlat,qlon,qveg,qveg_agreg)
implicit none
integer ifoc,nfocos
real,    dimension(nfocos) :: qlat,qlon
integer, dimension(nfocos) :: qveg,qveg_agreg
integer catb(1:17)
data catb/ &
           2, 1, 2, 1                 & !floresta tropical 2 and 4 / extra trop fores 1,3,5
	 , 2, 3, 3, 3, 3              & !cerrado/woody savanna :6 a 9
         , 4, 4, 4, 4, 4, 0, 4, 0     / !pastagem/lavouras: 10 ...

do ifoc=1,nfocos
   qveg_agreg(ifoc) =catb( qveg(ifoc) )
enddo

return
end
!------------------------------------------------------------------------------------------------------
!
subroutine  define_veg(nfocos,qlat,qlon,qveg,OFN)

use grid_dims_out, only: use_vcf                &
   		      ,vcf_type_data_dir

real,    dimension(nfocos) :: qlat,qlon
integer, dimension(nfocos) :: qveg, qvcf
integer, allocatable :: DATO(:)
CHARACTER*256 TITLE
CHARACTER(*)  OFN
!CHARACTER*80 OFN,TITLE
character(len=1), allocatable:: cdato(:)

!--------Diretorio onde se encontram os arquivos de vegetacao e o prefixo'
!OFN='./data_veg/IGBP'
LB=lastchar(OFN)
TITLE=OFN(1:LB)//'HEADER'
lb=lastchar(title)
call rams_f_open(29,title(1:lb),'formatted','old','read',0)
read(29,2)iblksizo,no,isbego,iwbego

close(29)
deltax=1000. ! 1 km resolucao
deltay=1000. ! 1 km resolucao

deltallo=float(iblksizo)/float(no-1)
np=min(10,max(1,int(deltax/(deltallo*111000.))))
deltaxp=deltax/float(np)
deltayp=deltay/float(np)
mof=4
iodim=no*no*mof
allocate (dato(iodim))
allocate (cdato(no*no))

  call le_veg(nfocos,NO,MOF,NP,DELTALLO,DELTAXP,DELTAYP,IBLKSIZO  &
     ,ISBEGO,IWBEGO,DATO,qveg,OFN,cdato,qlat,qlon,0)  
deallocate (dato,cdato)     
     
  if (use_vcf .eq. 1) then     
    LB=lastchar(vcf_type_data_dir)
    TITLE=vcf_type_data_dir(1:LB)//'HEADER'
    lb=lastchar(title)
    call rams_f_open(29,title(1:lb),'formatted','old','read',0)
    read(29,2)iblksizo,no,isbego,iwbego
    close(29)
    deltax=1000. ! 1 km resolucao
    deltay=1000. ! 1 km resolucao

    deltallo=float(iblksizo)/float(no-1)
    np=min(10,max(1,int(deltax/(deltallo*111000.))))
    deltaxp=deltax/float(np)
    deltayp=deltay/float(np)
    mof=4
    iodim=no*no*mof
    allocate (dato(iodim))
    allocate (cdato(no*no))

    call le_veg(nfocos,NO,MOF,NP,DELTALLO,DELTAXP,DELTAYP,IBLKSIZO  &
        ,ISBEGO,IWBEGO,DATO,qveg,OFN,cdato,qlat,qlon,1)  
    deallocate (dato,cdato) 
  endif

2    format(4i5)
end subroutine define_veg
!------------------------------------------------------------------------------------------------------
subroutine le_veg(nfocos,NO,MOF,NP,DELTALLO,DELTAXP,DELTAYP,IBLKSIZO  &
                 ,ISBEGO,IWBEGO,DATO,qveg,OFN,cdato,qlat,qlon,use_vcf)
		 

integer DATO(NO,NO,MOF)
character*1 cdato(no,no)
integer, dimension(nfocos) :: qveg, qveg_original
real,    dimension(nfocos) :: qlat,qlon

dimension iso(100),iwo(100)
CHARACTER(*)  OFN
character*256 title3
character*3 title1
character*4 title2
integer datp, use_vcf, igbp
logical l1,l2



if (use_vcf .eq. 1) then
  qveg_original = qveg
endif

NONO=NO*NO
NOFR=0
DO IOF=1,MOF
   ISO(IOF)=0
   IWO(IOF)=0
ENDDO

do ifoc=1,nfocos
 glatp=qlat(ifoc)
 glonp=qlon(ifoc)

 !tks  fix for model grid crossing 180, isbego=-90, iwbego=-180
 !tks  want 180>glonp>-180
 if(glonp.lt.-180.)glonp=glonp+360.                     !tks
 if(glonp.ge.180.)glonp=glonp-360.                      !tks 
 !tks

   ISOC=(INT((GLATP-FLOAT(ISBEGO))/FLOAT(IBLKSIZO)+200.)  &
      -200)*IBLKSIZO+ISBEGO
   IWOC=(INT((GLONP-FLOAT(IWBEGO))/FLOAT(IBLKSIZO)+400.)  &
      -400)*IBLKSIZO+IWBEGO

   DO IOFR=1,NOFR
      JOFR=IOFR
      IF(ISO(IOFR).EQ.ISOC.AND.IWO(IOFR).EQ.IWOC)GO TO 10
   ENDDO
   ISOCPT=ABS(ISOC)/10
   ISOCPO=ABS(ISOC)-ISOCPT*10
   IWOCPH=ABS(IWOC)/100
   IWOCPT=(ABS(IWOC)-IWOCPH*100)/10
   IWOCPO=ABS(IWOC)-IWOCPH*100-IWOCPT*10
   IF(ISOC.GE.0)THEN
      WRITE(TITLE1,'(2I1,A1)')ISOCPT,ISOCPO,'N'
   ELSE
      WRITE(TITLE1,'(2I1,A1)')ISOCPT,ISOCPO,'S'
   ENDIF
   IF(IWOC.GE.0)THEN
      WRITE(TITLE2,'(3I1,A1)')IWOCPH,IWOCPT,IWOCPO,'E'
   ELSE
      WRITE(TITLE2,'(3I1,A1)')IWOCPH,IWOCPT,IWOCPO,'W'
   ENDIF
   LB=lastchar(OFN)
   TITLE3=OFN(1:LB)//TITLE1//TITLE2
   LB=lastchar(TITLE3)
   INQUIRE(FILE=TITLE3(1:LB),EXIST=L1,OPENED=L2)
   IF(.NOT.L1)THEN
!  	     PRINT*, ' FILE',TITLE3(1:LB),' DOES NOT EXIST ',
!  		   'WATER IS ASSUMED'

      DATP = 0
      GO TO 20
   ENDIF
   IF(NOFR.GE.MOF)THEN
      DO IOF=1,MOF
   	 ISO(IOF)=0
   	 IWO(IOF)=0
      ENDDO
      NOFR=0
   ENDIF
   NOFR=NOFR+1
   JOFR=NOFR
   !print*, 'getting file ',title3(1:lb),ir,jr,ip,jp
   call rams_c_open(title3(1:lb)//char(0),'r'//char(0))
   call rams_c_read_char(4,no*no,cdato(1,1))
   call rams_c_close()

   do j=1,no
      do i=1,no
   	 dato(i,j,nofr)=ichar(cdato(i,j))
      enddo
   enddo

   ISO(NOFR)=ISOC
   IWO(NOFR)=IWOC
10    CONTINUE
   RIO=(GLONP-FLOAT(IWOC))/DELTALLO+1.5
   RJO=(GLATP-FLOAT(ISOC))/DELTALLO+1.5
!
   IO=INT(RIO)
   JO=INT(RJO)

   DATP=DATO(IO,JO,JOFR)
!  print*,title3(1:lb),GLATP,GLONP,DATP
!  if(ifoc.lt.17)then
!  print*,ifoc,IO,JO,DATP,no,IWOC,ISOC
!   endif
20 	CONTINUE

   qveg(ifoc)=datp
!srf- corrige veg=0
  if(qveg(ifoc)==0 .AND. use_vcf .ne. 1)  qveg(ifoc)= 17
  
  if ( use_vcf .eq. 1 ) then
    if ((qveg(ifoc) .EQ. 55) .OR. (qveg(ifoc) .EQ. 0) .OR. (qveg(ifoc) .EQ. 13) .OR. (qveg(ifoc) .EQ. 21)) then          
       qveg(ifoc) = qveg_original(ifoc)
    else
       call vcf2igbp(igbp,qveg(ifoc))
       qveg(ifoc) = igbp
    endif  
  endif	   
enddo
return
end
!------------------------------------------------------------------------------------------------------
!
subroutine define_bas_olson(bas_olson,nveg_olson,nfocos,bas_by_fire,qveg_olson)
implicit none
integer nveg_olson,nfocos,i
real, dimension(nveg_olson) :: bas_olson
real,    dimension(4,0:96) :: olson_data
real,    dimension(nfocos) :: bas_by_fire
integer, dimension(nfocos) :: qveg_olson
data olson_data/  &
!------------------------------------------------------------------------------------------------------
!Medium carbon density (Kg C/m2) 	
!Revised medium carbon density (Kg C/m2) 	
!Minimum carbon density (Kg C/m2) 	
!Maximum carbon density (Kg C/m2) 	
!Ecosystem codes
!Ecosystem complexes 
!------------------------------------------------------------------------------------------------------
! MED   REV     MIN    MAX     Olson Global Ecosystem Legend (glcc/usgs versao 2)
!                             !X = adaptado, nao existente no dado original 
                              !    (http://cdiac.ornl.gov)
 0.00,  0.00,	0.0,   0.0, & !X 0 INTERRUPTED AREAS (GLOBAL GOO
 0.00,  0.00,	0.0,   0.0, & !X 1 URBAN			
 1.00,  0.80,	0.6,   2.0, & !X 2 LOW SPARSE GRASSLAND 	
16.00, 13.00,  12.0,  20.0, & !X 3 CONIFEROUS FOREST		
10.00,  7.00,	6.0,  14.0, & !X 4 DECIDUOUS CONIFER FOREST	
10.00,  9.00,	8.0,  14.0, & !X 5 DECIDUOUS BROADLEAF FOREST	
10.00,  9.00,	8.0,  14.0, & !X 6 EVERGREEN BROADLEAF FORESTS  
 3.00,  3.00,	2.0,   5.0, & !X 7 TALL GRASSES AND SHRUBS	
 0.00,  0.00,	0.0,   0.0, & !  8 BARE DESERT  		
 0.50,  0.50,	0.0,   1.2, & !X 9 UPLAND TUNDRA		
 2.00,  2.00,	1.0,   3.0, & !X 10 IRRIGATED GRASSLAND  	
 0.40,  0.30,	0.2,   1.0, & !X 11 SEMI DESERT  		
 0.00,  0.00,	0.0,   0.0, & !  12 GLACIER ICE  		
 0.00,  0.00,	0.0,   0.0, & !  13 WOODED WET SWAMP		
 0.00,  0.00,	0.0,   0.0, & !  14 INLAND WATER 		
 0.00,  0.00,	0.0,   0.0, & !  15 SEA WATER			
1.30,  0.90,	0.5,   3.0, & !X 16 SHRUB EVERGREEN		
1.30,  0.90,	0.5,   3.0, & !X 17 SHRUB DECIDUOUS		
10.00,  7.00,	6.0,  14.0, & !X 18 MIXED FOREST AND FIELD	
!15.00, 12.00,	4.0,  25.0, & !X 19 EVERGREEN FOREST AND FIELDS 
15.00, 14.62,	4.0,  25.0, & !X 19 EVERGREEN FOREST AND FIELDS 
 8.00,  6.00,	4.0,  11.0, & ! 20 COOL RAIN FOREST			      
 8.00,  6.00,	4.0,  11.0, & ! 21 CONIFER BOREAL FOREST		      
16.00, 13.00,  12.0,  20.0, & ! 22 COOL CONIFER FOREST  		      
10.00,  7.00,	6.0,  14.0, & ! 23 COOL MIXED FOREST			         
10.00,  7.00,	6.0,  14.0, & ! 24 MIXED FOREST 			         
10.00,  9.00,	8.0,  14.0, & ! 25 COOL BROADLEAF FOREST		      
10.00,  9.00,	8.0,  14.0, & ! 26 DECIDUOUS BROADLEAF FOREST		      
16.00, 13.00,  12.0,  20.0, & ! 27 CONIFER FOREST			      
 5.00,  5.00,	1.0,  15.0, & ! 28 MONTANE TROPICAL FORESTS		      
15.00, 12.00,	4.0,  25.0, & ! 29 SEASONAL TROPICAL FOREST		       
 1.00,  0.70,	0.4,   2.0, & ! 30 COOL CROPS AND TOWNS 		      
 1.00,  0.80,	0.6,   2.0, & ! 31 CROPS AND TOWN			      
 7.00,  6.00,	5.0,   9.0, & ! 32 DRY TROPICAL WOODS			      
!15.00, 12.00,	4.0,  25.0, & ! 33 TROPICAL RAINFOREST  		       
15.00, 14.62,	4.0,  25.0, & ! 33 TROPICAL RAINFOREST  		       
10.00,  7.00,	6.0,  14.0, & ! 34 TROPICAL DEGRADED FOREST		      
 3.00,  3.00,	2.0,   4.0, & ! 35 CORN AND BEANS CROPLAND		      
 3.00,  3.00,	2.0,   4.0, & ! 36 RICE PADDY AND FIELD 		      
 2.00,  2.00,	1.0,   3.0, & ! 37 HOT IRRIGATED CROPLAND		       
 2.00,  2.00,	1.0,   3.0, & ! 38 COOL IRRIGATED CROPLAND		       
 2.00,  2.00,	1.0,   3.0, & ! 39 COLD IRRIGATED CROPLAND		       
 1.00,  0.80,	0.6,   2.0, & ! 40 COOL GRASSES AND SHRUBS		      
 1.30,  0.90,	0.5,   3.0, & ! 41 HOT AND MILD GRASSES AND SHRUBS	      
 1.00,  1.00,	0.5,   4.0, & ! 42 COLD GRASSLAND			      
! 3.00,  3.00,	2.0,   5.0, & ! 43 SAVANNA (WOODS)			      
 3.00,  3.10,	2.0,   5.0, & ! 43 SAVANNA (WOODS)			      
 2.00,  2.00,	1.0,   6.0, & ! 44 MIRE, BOG, FEN			      
 3.00,  2.00,	1.0,   6.0, & ! 45 MARSH WETLAND			      
 4.00,  3.00,	2.0,   8.0, & ! 46 MEDITERRANEAN SCRUB  		      
 4.00,  3.00,	2.0,   8.0, & ! 47 DRY WOODY SCRUB			      
 5.00,  4.00,	2.0,  10.0, & ! 48 DRY EVERGREEN WOODS  		      
 0.40,  0.30,	0.2,   1.0, & ! 49 VOLCANIC ROCK			       
 0.05,  0.05,	0.0,   0.2, & ! 50 SAND DESERT  			      
 0.40,  0.30,	0.2,   1.0, & ! 51 SEMI DESERT SHRUBS			       
 0.60,  0.60,	0.3,   1.0, & ! 52 SEMI DESERT SAGE			      
 0.50,  0.50,	0.0,   1.2, & ! 53 BARREN TUNDRA			      
 0.50,  0.50,	0.0,   1.2, & ! 54 COOL SOUTHERN HEMISPHERE MIXED FORESTS     
 4.00,  3.00,	2.0,   5.0, & ! 55 COOL FIELDS AND WOODS		       
 5.00,  4.00,	4.0,   8.0, & ! 56 FOREST AND FIELD			       
 5.00,  4.00,	4.0,   8.0, & ! 57 COOL FOREST AND FIELD		       
! 4.00,  3.00,	2.0,   5.0, & ! 58 FIELDS AND WOODY SAVANNA		       
 4.00,  1.9,	2.0,   5.0, & ! 58 FIELDS AND WOODY SAVANNA !srf : palacios-orueta
! 4.00,  3.00,	2.0,   6.0, & ! 59 SUCCULENT AND THORN SCRUB		      
 4.00,  3.70,	2.0,   6.0, & ! 59 SUCCULENT AND THORN SCRUB		      
11.00,  8.00,	6.0,  14.0, & ! 60 SMALL LEAF MIXED WOODS		      
11.00,  8.00,	6.0,  14.0, & ! 61 DECIDUOUS AND MIXED BOREAL FOREST	      
 5.00,  5.00,	2.0,   8.0, & ! 62 NARROW CONIFERS			      
 2.00,  2.00,	1.0,   5.0, & ! 63 WOODED TUNDRA			      
 1.50,  1.00,	1.0,   2.0, & ! 64 HEATH SCRUB  			      
 3.00,  3.00,	0.0,  10.0, & ! 65 COASTAL WETLAND - NW 		       
 3.00,  3.00,	0.0,  10.0, & ! 66 COASTAL WETLAND - NE 		       
 3.00,  3.00,	0.0,  10.0, & ! 67 COASTAL WETLAND - SE 		       
 3.00,  3.00,	0.0,  10.0, & ! 68 COASTAL WETLAND - SW 		       
 0.50,  0.50,	0.0,   1.2, & ! 69 POLAR AND ALPINE DESERT		      
 0.50,  0.50,	0.0,   1.2, & ! 70 GLACIER ROCK 			      
 0.40,  0.30,	0.2,   1.0, & ! 71 SALT PLAYAS  			       
 3.00,  2.00,	1.0,   6.0, & ! 72
 0.00,  0.00,	0.0,   0.0, & !   73 WATER AND ISLAND FRINGE		 
 0.00,  0.00,	0.0,   0.0, & !   74 LAND, WATER, AND SHORE		 
 0.00,  0.00,	0.0,   0.0, & !   75 LAND AND WATER, RIVERS		 
 0.00,  0.00,	0.0,   0.0, & !   76 CROP AND WATER MIXTURES		 
10.00,  7.00,   6.0,  14.0, & !X  77 SOUTHERN HEMISPHERE CONIFERS	  
10.00,  7.00,   6.0,  14.0, & !X  78 SOUTHERN HEMISPHERE MIXED FOREST   
10.00,  7.00,   6.0,  14.0, & !X  79 WET SCLEROPHYLIC FOREST		  
 0.00,  0.00,	0.0,   0.0, & !   80 COASTLINE FRINGE		 
 0.00,  0.00,	0.0,   0.0, & !   81 BEACHES AND DUNES  	 
 0.00,  0.00,	0.0,   0.0, & !   82 SPARSE DUNES AND RIDGES		 
 0.00,  0.00,	0.0,   0.0, & !   83 BARE COASTAL DUNES 	 
 0.00,  0.00,	0.0,   0.0, & !   84 RESIDUAL DUNES AND BEACHES  
 0.00,  0.00,	0.0,   0.0, & !   85 COMPOUND COASTLINES		 
 0.00,  0.00,	0.0,   0.0, & !   86 ROCKY CLIFFS AND SLOPES		 
 0.00,  0.00,	0.0,   0.0, & !   87 SANDY GRASSLAND AND SHRUBS  
10.00,  7.00,   6.0,  14.0, & !X  88 BAMBOO				 
10.00,  7.00,   6.0,  14.0, & !X  89 MOIST EUCALYPTUS  		 
!15.00, 12.00,   4.0,  25.0,& !X  90 RAIN GREEN TROPICAL FOREST	 
15.00, 14.62,   4.0,  25.0, & !X  90 RAIN GREEN TROPICAL FOREST	 
!3.00,  3.00,	2.0,   5.0, & !X  91 WOODY SAVANNA			
 3.00,  5.00,	2.0,   5.0, & !X  91 WOODY SAVANNA			
 3.00,  3.00,	2.0,   5.0, & !X  92 BROADLEAF CROPS			
 1.00,  0.80,	0.6,   2.0, & !X  93 GRASS CROPS			
 1.00,  0.80,	0.6,   2.0, & !X  94 CROPS, GRASS, SHRUBS		
 3.00,  3.00,	2.0,   5.0, & !X  95 EVERGREEN TREE CROP
 3.00,  3.00,	2.0,   5.0  / !X  96 DECIDUOUS TREE CROP
!------------------------------------------------------------------------------------------------------

bas_by_fire(:)=0.

do i=1,nveg_olson-1
 bas_olson(i)   = 2.*olson_data(2,i)  ! total biomass ~ 2. * Carbon density 
enddo

!- define densidade de biomassa acima do solo no array bas_by_fire
do i=1,nfocos

 if(qveg_olson(i) > 96 ) then
  print*, 'erro nos dados de vegetacao de OLSON - mudando para oceano'
  qveg_olson(i)=15
 endif
 bas_by_fire(i) = bas_olson(qveg_olson(i))
 !if(bas_by_fire(i) < 0.001)  print*,'OLSON=',i,qveg_olson(i),bas_by_fire(i)
enddo

return
end

!------------------------------------------------------------------------------------------------------
!
subroutine define_bas_olson2(qlat, qlon, nfocos, bas_by_fire)

implicit none
integer nfocos,i
real, allocatable,  dimension(:,:)  :: olsonData
real, intent(OUT),   dimension(nfocos) :: bas_by_fire
real, intent(IN),   dimension(nfocos) :: qlat, qlon

integer :: 	nLat, &
		nLon, &
		ill, &
		jll
	
real :: 	spacing, &
		cdiac_wlon, &
		cdiac_slat, &
		fLon, &
		fLat

nLon = 720
nLat = 360
spacing = 0.5
cdiac_wlon = -179.75
cdiac_slat = -89.75

allocate(olsonData(nLon, nLat))

call readOlsonHdf(nLon, nLat, olsonData)

bas_by_fire(:)=0.

do i=1,nfocos

	fLat=qlat(i)
	fLon=qlon(i)
		
	ill = (nint((fLon-cdiac_wlon)/spacing)) + 1        
  	jll = (nint((fLat-cdiac_slat)/spacing)) + 1
	
	IF(ill .LE. 0 .OR. ill .GT. nLon .OR. 	&
	   jll .LE. 0 .OR. jll .GT. nLat)	THEN
	   	ill = -1
		jll = -1		
		cycle
	END IF
	
	bas_by_fire(i) = 2.*olsonData(ill, jll)
  
enddo

deallocate(olsonData)

return
end subroutine define_bas_olson2


!----------------------------------------------------------------------------------------------------

subroutine bbbem_filelist(fnames,itimes,file_prefix,maxfiles,nfile)
implicit none
character fnames(maxfiles)*(*),file_prefix*(*)
character file*240,command*240
integer itimes(*)
integer :: maxfiles,nfiles
integer iflag,nfile,iprelen,nf,iun,i,it


 iflag=nfile

 nfile = 0
 print *, ' '
 print *, ' Checking directory - ',file_prefix
 
 iprelen=index(file_prefix,' ')
 if(iprelen.eq.0) iprelen=len(file_prefix)
 command=  &
     '/bin/ls -1 '//file_prefix(1:iprelen)//'*'//' >/tmp/fire_filelist'
 call system(command)
 command= 'chmod 777 /tmp/fire_filelist'
 call system(command)
 
!Open the directory list and read through the files
 iun=98
 open(unit=iun,file='/tmp/fire_filelist',status='old',err=15)
 rewind iun
 
 do nf=1,1000000
    read(iun,'(a128)',end=30,err=30) file
    if(nf< maxfiles) then 
   	fnames(nf) = file
    else
       print*,'number of files > maxfiles'
       stop 333
    endif
 enddo
 
 30   continue

  close(iun)
  command= '/bin/rm -f /tmp/fire_filelist'
  call system(command)

  nfile=nf-1

  if (nfile .eq. 0) then
     print *, 'No fire files for prefix:',file_prefix
    ! if(iflag.ge.0) stop 'RAMS_filelist-no_files'
  endif
  
  return 
 15   print *, 'fire_filelist: Error opening /tmp/fire_filelist'
      stop 'fire_filelist-/tmp file error : run again'
      return
      
 100  continue

return
end subroutine bbbem_filelist
!------------------------------------------------------------------------------------------------------
!
subroutine define_bas_houghton(nfocos,bas_by_fire,qveg,qlat,qlon,ofn)
implicit none
integer nveg,nfocos,i,ifoc,jfoc
real,    dimension(nfocos) :: qlat,qlon,bas_by_fire
integer, dimension(nfocos) :: qveg
! houghton data
integer, parameter :: nlon2= 667
integer, parameter :: nlat2=533
real, save, dimension(nlon2,nlat2) :: rcarb_dens
integer, save :: ncall
data ncall /0/
character (len=*) ofn

if(ncall == 0) then
 ncall =1000
 print*,'---------- Houghton Carbon density from 44 observ sites-----'
 print*,'opening : inter44tci_houghton_recorte.vfm data file    -----'
 print*,'------------------------------------------------------------'
 
! le dado de carbono (ton/ha) em VFM
! estrutura do dado
! nlon  667, corner a oeste    -75.0013,   dlon=    0.04505
! nlat  533, corner a sul      -18.0055,   dlat=    0.04505

 !open(unit=22,file='inter44tci_houghton_recorte.vfm',&
 open(unit=22,file=ofn,&
      form='formatted',status='old')
 call vfirec(22,rcarb_dens,nlon2*nlat2,'LIN')!preserve 'LIN' em maiusculo.
 close(unit=22)

! converte de ton/ha de carbono para kg/m2 de biomassa seca  
 rcarb_dens = 2.*0.1*rcarb_dens

endif
 
do i=1,nfocos  
 
   if(qlon(i) .gt. -40. .or. qlat(i) .lt. -18.) cycle

! define  rcarb_dens para o foco em (qlat,qlon)
  
   ifoc = nint(22.198*(qlon(i) -  ( -75.0013   + 0.) )) + 1 !22.19 = 1/0.04505
   jfoc = nint(22.198*(qlat(i) -  ( -18.0055   + 0.) )) + 1

   ifoc=max(1,min(ifoc,nlon2))
   jfoc=max(1,min(jfoc,nlat2))
   !if(rcarb_dens(ifoc,jfoc) > 0.) then
   !  print*,ifoc,jfoc
   !  print*,qlat(i),qlon(i),rcarb_dens(ifoc,jfoc),bas_by_fire(i),qveg(i)
   !endif

! troca valor do Olson's database ou tabela pelo do houghton se qveg=floresta
  if(qveg(i) == 2 .and. rcarb_dens(ifoc,jfoc) > 0.) bas_by_fire(i)=rcarb_dens(ifoc,jfoc)

enddo
end subroutine define_bas_houghton
!------------------------------------------------------------------------------------------------------

subroutine get_ij_ll(n1,n2,rlat,rlon,qlon,qlat,igbox,jgbox)
use grid_dims_out, only:  grid_resolucao_lon,grid_resolucao_lat
implicit none
integer, intent(in) ::n1,n2
real, intent(in) :: rlat(n1,n2), rlon(n1,n2),qlon, qlat

integer, intent(out) :: igbox,jgbox

!local var
integer kk,k1,k2,ii,i1,i2,k
real  :: dlon1,dlon2,dlat1, dlat2,dx,dy,dxm,dym

!-new/faster way
      igbox =   (nint((qlon-rlon(1,1))/grid_resolucao_lon)) + 1        
      jgbox =   (nint((qlat-rlat(1,1))/grid_resolucao_lat)) + 1
!RMF - bounds control
	IF(igbox .LE. 0 .OR. igbox .GT. n1 .OR. &
	   jgbox .LE. 0 .OR. jgbox .GT.	n2)THEN
	   	igbox = -1
		jgbox = -1
	END IF
!RMF 

return
!------------------- old way ------------------------

 
!- lon
    do kk= 1,n1
     if(qlon .le. rlon(kk,1) ) exit
    enddo
!100 continue
      k1 = kk-1
      k2 = kk
      
      if(k1.eq.0) then 
         k1=n1
         dlon1=     qlon - rlon(k1,1) +360.
      else
         dlon1=     qlon - rlon(k1,1)
      endif
      
      if(k2.gt.n1) then
            k2=1
            dlon2= - ( qlon - rlon(k2,1) - 360.)
      else
            dlon2= - ( qlon - rlon(k,1) )
      endif
!- lat   
      do ii= 2,n2-1
       if(qlat .le. rlat(1,ii)  )exit
      enddo
!200 continue
      i1= ii-1
      i2= ii
      dlat1=     qlat - rlat(1,i1)
      dlat2= - ( qlat - rlat(1,i2) )
!-----
      dx = dlon1
      dy = dlat1
      dxm = grid_resolucao_lon/2.
      dym = grid_resolucao_lat/2.
      
      if(dx.le.dxm.and.dy.le.dym) then
  				igbox = k1
  				jgbox = i1
      endif			
       
      if(dx.gt.dxm.and.dy.lt.dym) then
  				igbox = k2
  				jgbox = i1
      endif			

      if(dx.le.dxm.and.dy.gt.dym) then
  				igbox = k1
  				jgbox = i2
      endif		       

      if(dx.gt.dxm.and.dy.gt.dym)  then
  				igbox = k2
  				jgbox = i2
      endif		     
end  subroutine get_ij_ll
!------------------------------------------------------------------------------------------------------

subroutine die(myname,string)
 character(len=*) myname
 character(len=*) string

 print *, ' --------------------------------'
 print *, '	   ',myname
 print *, '   ',string
 print *, ' --------------------------------'
 call exit(1)
end subroutine die
!--------------------------------------------------------------------------------------------------------
subroutine define_bas_flsafari(nfocos,bas_by_fire,qveg,qlat,qlon,ofn)
implicit none
integer nveg,nfocos,i,ifoc,jfoc,n
real,    dimension(nfocos) :: qlat,qlon,bas_by_fire
integer, dimension(nfocos) :: qveg
!! houghton data
!!integer, parameter :: nlon2= 667
!!integer, parameter :: nlat2=533
!!real, save, dimension(nlon2,nlat2) :: rcarb_dens
integer, save :: ncall
data ncall /0/
character (len=*) ofn


INTEGER                                  :: nx, ny,nfiles
REAL                                     :: lat, lon, ilat, ilon, sx, sy

CHARACTER*1,save, DIMENSION(:,:), ALLOCATABLE :: uvector !unsigned vector
REAL*4,save, DIMENSION(:,:,:), ALLOCATABLE    :: ovector !output vector
INTEGER                                ::  x, y
real, parameter :: cv=1.e-3/ &! convert from g to kg
                     (0.45*  &! convert from Carbon to biomass dry-matter
		      0.7)   ! scale factor parameter  

CHARACTER*256, DIMENSION(4)                :: filename
 
 nx   = 3900
 ny   = 4800
 sx   = 0.008333
 sy   = 0.008333
 ilat = -34.991651
 ilon = 10
 
 nfiles = 4
 filename(1) = trim(ofn)//'herb_dead'
 filename(2) = trim(ofn)//'herb_green'
 filename(3) = trim(ofn)//'tree_leaf_litter'
 filename(4) = trim(ofn)//'twig'


if(ncall == 0) then
 ncall =1000
 
 !filename(1) = '/dados4/poluicao/fuel_load/data/herb_dead_2000-07_02.img'
 !filename(2) = '/dados4/poluicao/fuel_load/data/herb_green_2000-07_02.img'
 !filename(3) = '/dados4/poluicao/fuel_load/data/tree_leaf_litter_2000-07_02.img'
 !filename(4) = '/dados4/poluicao/fuel_load/data/twig_1999-09_01.img'

 ALLOCATE(uvector(nx, ny), ovector(nx, ny, nfiles)) 
 
 DO i = 1, nfiles
 	PRINT*,'opening safari ', TRIM(filename(i))
 	OPEN(UNIT=10, FILE=TRIM(filename(i)), ACCESS='DIRECT', RECL=nx*ny, STATUS='old')
 	READ(10, REC=1)uvector(:,:) 
 	CLOSE(10)
	DO x =1, nx
		DO y=1, ny
			ovector(x, y, i) = cv*ICHAR(uvector(x, ny-(y-1))) !axis Y rotation
 		END DO
 	END DO
 END DO

 print*,'max=',maxval(ovector(:,:,:))

endif
 
do i=1,nfocos  
 
   if(qlon(i) .lt. -20. .or. qlat(i) .lt. -40.) cycle

! define  rcarb_dens para o foco em (qlat,qlon)
  
   ifoc = NINT((qlon(i) - ilon)/sx)+1
   jfoc = NINT((qlat(i) - ilat)/sy)+1

   ifoc=max(1,min(ifoc,nx))
   jfoc=max(1,min(jfoc,ny))
   !if(rcarb_dens(ifoc,jfoc) > 0.) then
   !  print*,ifoc,jfoc
   !  print*,qlat(i),qlon(i),rcarb_dens(ifoc,jfoc),bas_by_fire(i),qveg(i)
   !endif

! troca valor do Olson's database ou tabela pelo do SAFARI
  !print*,'old',bas_by_fire(i)
  bas_by_fire(i) = 0.
  do n=1,nfiles
    bas_by_fire(i) = bas_by_fire(i) + ovector(ifoc,jfoc,n)
  enddo
 if(bas_by_fire(i) < 1.e-6) bas_by_fire(i) = 0.1
  

enddo
end subroutine define_bas_flsafari
!------------------------------------------------------------------------------------------------------
subroutine define_bas_fuel_amazon(nfocos,bas_by_fire,qveg,qlat,qlon,ofn)
implicit none
integer nveg,nfocos,i,ifoc,jfoc,n
real,    dimension(nfocos) :: qlat,qlon,bas_by_fire
integer, dimension(nfocos) :: qveg
!! houghton data
!!integer, parameter :: nlon2= 667
!!integer, parameter :: nlat2=533
!!real, save, dimension(nlon2,nlat2) :: rcarb_dens
integer, save :: ncall
data ncall /0/
character (len=*) ofn


INTEGER                                  :: nx, ny,istat, x, y
REAL                                     :: lat, lon, ilat, ilon, sx, sy

INTEGER*1, DIMENSION(:,:),save, ALLOCATABLE :: TiffDataB, Out   
REAL, DIMENSION(:,:),save, ALLOCATABLE      :: dummy, TiffDataA, Temp
real, parameter :: cv=1.e3 * &! convert from Mg to kg
                      1.e-4    ! convert from ha to m^2
real catb(1:11)

!1:     1-25 Mg/ha
!2:     25-50
!3:     50-75
!4:     75-100
!5:     100-150
!6:     150-200
!7:     200-250
!8:     250-300
!9:     300-350
!10:   350-400
!11:     >400
data catb/ 12.5, 37.5, 62.5, 87.5, 125., &
           175., 225., 275., 325., 375., &
	   400. / 

 nx = 5900
 ny = 4200
 sx   = 0.00833333
 sy   = 0.00833333
 ilat = -21.1235
 ilon = -82.7251
 
if(ncall == 0) then
 ncall =1000
 
 print*,'---------- Saatachi Carbon density                     -----'
 print*,'opening Saatachi data:', ofn(1:len_trim(ofn))
 print*,'------------------------------------------------------------'

 if(ofn(len_trim(ofn)-3:len_trim(ofn)) == '.gra') then
  
   allocate(TiffDataB(nx,ny))
   ! - open binary mode - 1byte 
   open(unit = 20, file = ofn(1:len_trim(ofn)), access = 'direct', recl = nx*ny, status = 'old')
   read(20, rec = 1) TiffDataB	
   close(20)

 elseif(ofn(len_trim(ofn)-3:len_trim(ofn)) == '.vfm') then
 
   print*,'use file with sufix - gra'
   stop 333
   
   allocate(tiffdatab(nx,ny), tiffdataa(nx,ny))
   !- open vfm mode
   open(unit=22,file=ofn, form='formatted',status='old')
   call vfirec(22,tiffdataa,nx*ny,'lin')
   !- convert to integer
   tiffdatab = ANINT(TiffDataA)
 else
   print*,'wrong file name data Saatchi :',ofn
   stop 333 
 endif
 
endif
 
 
do i=1,nfocos   
   if(qlon(i) .gt. -20. .or. qlat(i) .lt. -23.) cycle

! define  bas_by_fire para o foco em (qlat,qlon)
   ifoc = NINT((qlon(i) - ilon)/sx)+1
   jfoc = NINT((qlat(i) - ilat)/sy)+1

   ifoc=max(1,min(ifoc,nx))
   jfoc=max(1,min(jfoc,ny))

! troca valor do Olson's database ou tabela pelo do Saatchi
 !print*,'old/new',qlon(i),qlat(i),bas_by_fire(i),catb(TiffDataB(ifoc,jfoc))*CV
  !RMF - UNDEFs CONTROL
  IF(TiffDataB(ifoc,jfoc) .GT. 0) &
  !RMF
  bas_by_fire(i) =  catb(TiffDataB(ifoc,jfoc))*CV
enddo
end subroutine define_bas_fuel_amazon
!---------------------------------------------------------
subroutine get_ij_fim(n1,n2,rlat,rlon,lon_fire,lat_fire,igbox,jgbox)
use grid_dims_out, only:  grid_resolucao_lon,grid_resolucao_lat
implicit none
integer, intent(in) ::n1,n2
real, intent(in) :: rlat(n1,n2), rlon(n1,n2),lon_fire,lat_fire
integer, intent(out) :: igbox,jgbox

!local
real :: mindist,currdist
integer :: i

 mindist=1.e9
 do i=1,n1
   if(abs(lon_fire-rlon(i,1)) .gt. 5. .or.&
      abs(lat_fire-rlat(i,1)) .gt. 5. ) cycle

    currdist=sqrt((lon_fire-rlon(i,1))**2+&
	          (lat_fire-rlat(i,1))**2)

    if(currdist<mindist) then
       mindist=currdist
       igbox=i ; jgbox=1
    endif

 enddo

 if(igbox .lt. 1 .or. igbox .gt. n1)  igbox = -1
 if(jgbox .lt. 1 .or. jgbox .gt. n2)  jgbox = -1
end  subroutine get_ij_fim
!---------------------------------------------------------
subroutine get_ij_gg(n1,n2,rlat,rlon,lon_fire,lat_fire,igbox,jgbox)
use grid_dims_out, only:  grid_resolucao_lon,grid_resolucao_lat
implicit none
integer, intent(in) ::n1,n2
real, intent(in) :: rlat(n1,n2), rlon(n1,n2),lon_fire,lat_fire
integer, intent(out) :: igbox,jgbox

!local
real :: mindist,currdist
integer :: i,j

 mindist=1.e9
 do i=1,n1
  do j=1,n2
   if(abs(lon_fire-rlon(i,j)) .gt. 5. .or.&
      abs(lat_fire-rlat(i,j)) .gt. 5. ) cycle

    currdist=sqrt((lon_fire-rlon(i,j))**2+&
	          (lat_fire-rlat(i,j))**2)

    if(currdist<mindist) then
       mindist=currdist
       igbox=i ; jgbox=j
    endif

 enddo; enddo

 if(igbox .lt. 1 .or. igbox .gt. n1)  igbox = -1
 if(jgbox .lt. 1 .or. jgbox .gt. n2)  jgbox = -1
end  subroutine get_ij_gg
!------------------------------------------------------------------------------------------------------
subroutine vcf2igbp(igbp,ivcf)

implicit none
integer igbp,ivcf,CATB(0:82)


if ((ivcf .gt. 0) .AND. (ivcf .le. 5)) igbp = 7
if ((ivcf .gt. 5) .AND. (ivcf .le. 6)) igbp = 15
if ((ivcf .gt. 6) .AND. (ivcf .le. 10)) igbp = 13
if ((ivcf .gt. 10) .AND. (ivcf .le. 13)) igbp = 10
if ((ivcf .gt. 13) .AND. (ivcf .le. 21)) igbp = 6
if ((ivcf .gt. 21) .AND. (ivcf .le. 24)) igbp = 14
if ((ivcf .gt. 24) .AND. (ivcf .le. 32)) igbp = 8
if ((ivcf .gt. 32) .AND. (ivcf .le. 36)) igbp = 3
if ((ivcf .gt. 36) .AND. (ivcf .le. 45)) igbp = 11
if ((ivcf .gt. 45) .AND. (ivcf .le. 46)) igbp = 4
if ((ivcf .gt. 46) .AND. (ivcf .le. 55)) igbp = 5
if ((ivcf .gt. 55)) igbp = 2




!data CATB/ 0  &
!         , 7, 7, 7, 7, 7,15,13,13,13,13  & !10
!         ,10,10,10, 6, 6, 6, 6, 6, 6, 6  & !20
!	 , 6,14,14,14, 8, 8, 8, 8, 8, 8  & !30
!	 , 8, 8, 3, 3, 3, 3,11,11,11,11  & !40
!	 ,11,11,11,11,11, 4, 5, 5, 5, 5  & !50
!	 , 5, 5, 5, 5, 5, 2, 2, 2, 2, 2  & !60
!	 , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  & !70
!	 , 2, 2, 2, 2, 2, 2, 2, 2, 2, 2  & !80
!	 , 2, 2                          / !82
	 
!igbp = CATB(ivcf)
!
RETURN
end  subroutine vcf2igbp
!------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------

subroutine readOlsonHdf(nLon, nLat, olsonData)
use grid_dims_out, only: olson_data_dir

implicit none

character*11 ::		SDS_NAME
integer      ::		X_LENGTH, Y_LENGTH, RANK 
integer, intent(IN) ::  nLon, nLat
real, intent(OUT), dimension(nLon, nLat) :: olsonData
	
parameter   (RANK = 2)

integer      ::		DFACC_RDONLY, DFNT_FLOAT32
	
parameter   (DFACC_RDONLY = 1, &
             DFNT_FLOAT32 = 5)
		
integer sfstart, sfcreate, sfendacc, sfend, sfrdata, sfselect
integer :: start(2), stride(2), edges(2), dim_sizes(2)
	
integer sd_id, sds_id, sds_index 
integer status	

X_LENGTH = nLon
Y_LENGTH = nLat
	
status = 1
	
dim_sizes(1) = X_LENGTH
dim_sizes(2) = Y_LENGTH
	
start(1) = 0
start(2) = 0	
edges(1) = X_LENGTH
edges(2) = Y_LENGTH	
stride(1) = 1	
stride(2) = 1
	
	
print*, trim(olson_data_dir)//'.hdf'

sd_id = sfstart(trim(olson_data_dir)//'.hdf', DFACC_RDONLY)	
sds_id = sfselect(sd_id, 1)	
status = sfrdata(sds_id, start, stride, edges, olsonData)
	
if (status == 0) then
	print*, '--- OLSON 2 lido corretamente ---'
else
	print*, '--- erro de leitura do OLSON 2 ---'
endif
	
status = sfendacc(sds_id)
status = sfend(sd_id)

return 	
end subroutine readOlsonHdf


!------------------------------------------------------------------------------------------------------
subroutine define_bas_fuel_alaska(nfocos,bas_by_fire,qveg,qlat,qlon,ofn)
implicit none
include 'netcdf.inc'
integer nveg,nfocos,i,ifoc,jfoc,j,var_id,ncid
real,    dimension(nfocos) :: qlat,qlon,bas_by_fire
integer, dimension(nfocos) :: qveg
integer, parameter :: nlon2= 3111
integer, parameter :: nlat2=1807
real, save, dimension(nlon2,nlat2) :: rcarb_dens
real tmp
integer, save :: ncall
data ncall /0/
character (len=*) ofn

if(ncall == 0) then
 ncall =1000
 print*,'---------- Fuel load for Alaska-----'
 print*,'opening:',TRIM(ofn); call flush(6)
!-1.677276785714286E+002
! 7.263839285711552E+001
!dlon=8.9263916E-03

  return

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
    call check( nf_open(TRIM(ofn), NF_NOWRITE, ncid)) 
    
! Get the varid of the data variable, based on its name.
    call check( nf_inq_varid(ncid, "above_ground_biomass", var_id) )
! Read the data.  
    call check( nf_get_var_real(ncid, var_id, rcarb_dens ) )
! Close the file, freeing all resources.
   call check( nf_close(ncid) )
!- invert array 
  do j=1,nlat2/2
   do i=1,nlon2
      rcarb_dens(i,j)         =tmp
      rcarb_dens(i,j)         =rcarb_dens(i,nlat2-j-1)
      rcarb_dens(i,nlat2-j-1) = tmp
   enddo;enddo
endif
!print*,'carb=',maxval(rcarb_dens),minval(rcarb_dens) 

do i=1,nfocos  
 
! define  rcarb_dens for the fire at (qlat,qlon)
  
   ifoc = nint(112.0273504*(qlon(i) -  (-1.677276786E+002   + 0.) )) + 1 !22.19 = 1/0.04505
   jfoc = nint(112.0273504*(qlat(i) -  ( 5.650446429E+001  + 0.) )) + 1
   ifoc=max(1,min(ifoc,nlon2))
   jfoc=max(1,min(jfoc,nlat2))
!   if(rcarb_dens(ifoc,jfoc) > 0.) then
!     print*,ifoc,jfoc
!     print*,qlat(i),qlon(i),rcarb_dens(ifoc,jfoc),bas_by_fire(i),qveg(i)
!   endif


enddo
end subroutine define_bas_fuel_alaska
!------------------------------------------------------------------------------------------------------



