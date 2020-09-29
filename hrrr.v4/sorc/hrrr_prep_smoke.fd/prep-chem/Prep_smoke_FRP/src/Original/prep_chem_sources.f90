!#######################################################################!
!  BRAMS/BAM - CPTEC/INPE - WRF-Chem - FIM-Chem emission models         ! 
!  version 1.8.3: Sep 2017                                              !
!  Coded by Saulo Freitas and Karla Longo                               !
!  brams support: brams_help@cptec.inpe.br - http://brams.cptec.inpe.br !
!#######################################################################!

program prep_chem_sources
use mem_grid, only : ngrids, nnxp,     nnyp,      nxtnest ,  ihtran ,   deltax  ,  deltay  , & 
                     nstratx ,  nstraty ,  polelat ,  polelon ,  &
                     stdlat1 ,  stdlat2 ,  centlat ,  centlon , ninest, njnest , stdlon
use grid_dims_out
implicit none

character (len=180) :: rams_anal_prefix

integer :: ihour,iday,imon,iyear

namelist/rp_input/  grid_type &	
		,rams_anal_prefix &
		,ihour &
		,iday &
		,imon &
		,iyear &
		,use_retro & 
		,retro_data_dir & 
		,use_edgar &
		,edgar_data_dir &
                ,use_ceds &
                ,ceds_data_dir &
		,fim_data_dir &
                ,fv3_data_dir &
		,use_gocart &
		,gocart_data_dir &
                ,use_streets &
                ,streets_data_dir &
                ,use_seac4rs &
                ,seac4rs_data_dir &
		,use_fwbawb &
		,fwbawb_data_dir & 
		,use_bioge &
		,bioge_data_dir &
		,use_gfedv2 &
		,gfedv2_data_dir &
		,use_bbem &
		,use_bbem_plumerise &
		,merge_GFEDv2_bbem &
		,bbem_wfabba_data_dir & 
		,bbem_modis_data_dir  &
		,bbem_inpe_data_dir   &
		,bbem_fre_data_dir    &
		,bbem_extra_data_dir  &
		,veg_type_data_dir   	&
		,use_vcf &
		,vcf_type_data_dir &          
		,olson_data_dir  &
		,carbon_density_data_dir &
		,fuel_data_dir &
		,use_gocart_bg &
		,gocart_bg_data_dir &
              ,use_afwa_erod &
              ,afwa_erod_data_dir &
		,use_volcanoes &
		,volcano_index &
		,use_these_values &
		,begin_eruption &
		,use_degass_volcanoes &
		,degass_volc_data_dir &
		,user_data_dir &
        ,cites_mobile_urban_emissions_file &
        ,streets_inventory_region &
		,pond &
		,grid_resolucao_lon &
		,grid_resolucao_lat &
		,nlat &
		,lon_beg &  
		,lat_beg  &
		,delta_lon &
		,delta_lat &
		,ngrids &
		,nnxp &
		,nnyp &
		,nxtnest &
		,deltax &
		,deltay &
		,nstratx &
		,nstraty &
		,ninest &
		,njnest &
		,polelat &
		,polelon &
		,stdlat1 &
		,stdlat2 &
		,centlat &
		,centlon &
		,lati &
		,latf &
		,loni & 
		,lonf  &
		,proj_to_ll &
		,chem_out_prefix & 
		,chem_out_format &
		,special_output_to_wrf&
		,stdlon
		    
print *, ' '	    
print *, 'Opening prep_chem_sources.inp file'

open(5,file='prep_chem_sources.inp',status='old')
read(5,rp_input)
close(5)
!
!- check  consistency of namelist
call check_consistency

!- determine the way to follow
if(trim(grid_type)=='rams') then
  !grid_type='rams'
  call rams_grid_type(rams_anal_prefix,ihour,iday,imon,iyear)

elseif(trim(grid_type)=='ll' .or. trim(grid_type) =='gg') then

  call other_grid_type(ihour,iday,imon,iyear)

elseif(trim(grid_type)=='polar' .or. trim(grid_type) =='lambert' .or. &
 trim(grid_type) =='mercator' .or. trim(grid_type) =='rotated_ll' .or. &
 trim(grid_type) =='lat-lon') then
  
  call gen_regional_grid_type(ihour,iday,imon,iyear)

elseif(trim(grid_type)=='fim') then  

  call FIM_model_grid_type(ihour,iday,imon,iyear)

elseif(trim(grid_type)=='fv3') then  

  call FV3_model_grid_type(ihour,iday,imon,iyear)

else
  print*,'stop : unknown proj type'

endif
end
!-----------------------------------------------------------------------------

subroutine other_grid_type(ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch
use grid_dims_out 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear
!character (len=*), intent(in)  ::  chem_out_prefix

integer :: i,j,iv,ng,nfn,nfiles,nvert
character (len=240) :: fnames(maxfiles)
real, allocatable, dimension(:,:,:) :: r2d


!- determine the grid configuration
ngrids = 1
proj_to_ll='NO'
nnzp(1:ngrids)=1

! -----------------
! -  start GRID LOOP (ONLY ONE is permited)   -
! -----------------

do ng=1,ngrids
  nfn=1

  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  !-lon/lat configuration
                        nnxp(ng)=int(delta_lon/grid_resolucao_lon + 0.1)
  if(grid_type == 'll')      then
                        nnyp(ng)=int(delta_lat/grid_resolucao_lat + 0.1) + 1
  elseif(grid_type == 'gg')  then
                        nnyp(ng)=nlat
  else

    print*,'no grid defined: grid_type=',grid_type(1:len_trim(grid_type))
    stop
  endif
  !- allocate arrays for lat,lon,land
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  !- lon
  do i=1,nnxp(ng)
     r2d(i,1:nnyp(ng),2)=lon_beg + grid_resolucao_lon*(i-1) 
  enddo
  !- lat
  if(grid_type == 'll')      then
       do j=1,nnyp(ng)
             r2d(1:nnxp(ng),j,1)=lat_beg + grid_resolucao_lat*(j-1) 
       enddo
  elseif(grid_type == 'gg')  then
        call gauss_lat(nlat,r2d(1:1,1:nnyp(ng),1))
        do i=2,nnxp(ng)
	 r2d(i,1:nnyp(ng),1)=r2d(1,1:nnyp(ng),1)
	 !if (i==2) print*,r2d(i,1:nnyp(ng),1)
	enddo
  endif
  ! for now land=1 everywhere
  r2d(:,:,3)=1.      
  !
  !
  !
  !.................
  ! determine the output dimension and arrays:
   nxgrads(ng)    = nnxp(ng)
   nygrads(ng)    = nnyp(ng)	     
   dep_glon(1,ng) = lon_beg
   dep_glon(2,ng) = grid_resolucao_lon
   dep_glat(1,ng) = lat_beg
   dep_glat(2,ng) = grid_resolucao_lat
   nxa(ng) = 1
   nxb(ng) = nnxp(ng)
   nya(ng) = 1
   nyb(ng) = nnyp(ng)
  
  !.................
  !go to 200
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
   nvert = 1        !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nnzp(ng) &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
  !
  200 continue
  !
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for visualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng),5)


  deallocate(r2d)
enddo ! enddo ngrids




end subroutine other_grid_type
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
subroutine FIM_model_grid_type(ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch
use grid_dims_out 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear
!character (len=*), intent(in)  ::  chem_out_prefix

integer :: i,j,iv,ng,nfn,nfiles,nvert
character (len=240) :: fnames(maxfiles)
real, allocatable, dimension(:,:,:) :: r2d
!- for FIM grid informations
integer :: nprg,npoints
integer, parameter :: lat =1 , lon =2
real*8  d, d2r, dist, distance
real*8,  allocatable :: icos_grid(:,:)   ! grid location (ll)
real,	 allocatable :: icos_grid4(:,:) ! grid location (ll), single precision 
integer, allocatable :: icos_nprox(:)	!number of neighbors(5 or 6)
integer, allocatable :: icos_prox(:,:)  !neighbor seq nidex
real*8,  allocatable :: icos_edge(:,:,:,:) ! end points of edges (ll)
real,	 allocatable :: icos_edge4(:,:,:,:) ! end points of edges (ll)prep_chem_sources.inp, single precision
!real*8, allocatable :: xyz(:,:,:)
!real *4, allocatable :: area (:)
logical              :: double_precision,there
character(16)	     :: header
integer :: fire_loc,nprox,prox,proxs,inv_perm,i3,isn,i2,i1	! dummies, not needed

d2r = 4.0*ATAN(1.0)/180.0

print*,'FIM grid directory=',trim(fim_data_dir)
!- determine the grid configuration
ngrids = 1
proj_to_ll='NO'
nnzp(1:ngrids)=1
ng=1
nfn=1

  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  !-lon/lat configuration
! inquire(file='./icos_grid_level.dat',exist=there)
  inquire(file=trim(fim_data_dir)//'/icos_grid_level.dat',exist=there)
  if(.not.there) then
     print*,'file not found: icos_grid_level.dat' 
     stop 'at FIM_model_grid_type routine 1'
  endif

! OPEN(10,file='./icos_grid_level.dat',status='old')
  OPEN(10,file=trim(fim_data_dir)//'/icos_grid_level.dat',status='old')
  READ(10,*) npoints
  CLOSE(10)
  ALLOCATE(icos_grid(2,npoints))   
  ALLOCATE(icos_prox(6,npoints))
  ALLOCATE(icos_nprox(npoints))
  ALLOCATE(icos_edge(6,2,2,npoints))	   
  !ALLOCATE(xyz(npoints,0:6,3))     
  
! inquire(file='./icos_grid_info_level.dat',exist=there)
  inquire(file=trim(fim_data_dir)//'/icos_grid_info_level.dat',exist=there)
  if(.not.there) then
     print*,'file not found: ./icos_grid_info_level.dat' 
     stop 'at FIM_model_grid_type routine 2'
  endif
! OPEN (10,file="./icos_grid_info_level.dat",form='unformatted')
  OPEN (10,file=trim(fim_data_dir)//"/icos_grid_info_level.dat",form='unformatted')
  read(10)header
  read(10)header
  double_precision = .false.
!  IF (double_precision == .true.) THEN
!       READ(10) icos_grid(1,1:npoints),icos_grid(2,1:npoints)
!       do isn = 1,size(icos_prox,1)
!         READ(10) icos_prox(isn,1:npoints)
!       enddo
!       READ(10) icos_nprox(1:npoints)
!       do i3 = 1,size(icos_edge,3)
!         do i2 = 1,size(icos_edge,2)
!           do isn = 1,size(icos_edge,1)
!             READ(10) icos_edge(isn,i2,i3,1:npoints)
!           enddo
!         enddo
!       enddo
!       DEALLOCATE(icos_grid)
!       DEALLOCATE(icos_prox)
!       DEALLOCATE(icos_nprox)
!       DEALLOCATE(icos_edge)
!   ELSE
       ALLOCATE(icos_grid4(2,npoints))        
       icos_grid4 = icos_grid
       DEALLOCATE(icos_grid)
       ALLOCATE(icos_edge4(6,2,2,npoints))    
       icos_edge4 = icos_edge
       DEALLOCATE(icos_edge)
       READ(10) icos_grid4(1,1:npoints),icos_grid4(2,1:npoints)
       icos_grid4(1,1:npoints)=icos_grid4(1,1:npoints)/d2r
       icos_grid4(2,1:npoints)=icos_grid4(2,1:npoints)/d2r
       do isn = 1,size(icos_prox,1)
         READ(10) icos_prox(isn,1:npoints)
       enddo
       READ(10) icos_nprox(1:npoints)
       do i3 = 1,size(icos_edge4,3)
         do i2 = 1,size(icos_edge4,2)
           do isn = 1,size(icos_edge4,1)
             READ(10) icos_edge4(isn,i2,i3,1:npoints)
             icos_edge4(isn,i2,i3,1:npoints)=icos_edge4(isn,i2,i3,1:npoints)/d2r
           enddo
         enddo
       enddo
!      DEALLOCATE(icos_grid4)
!      DEALLOCATE(icos_prox)
!      DEALLOCATE(icos_nprox)
!      DEALLOCATE(icos_edge4)
!  END IF
  CLOSE(10)

  do nprg=1,npoints
   if(icos_grid4(lon,nprg) .gt. 180.0) icos_grid4(lon,nprg)=icos_grid4(lon,nprg)-360.
   do i1=1,6
     do i2=1,2
     if(icos_edge4(i1,i2,lon,nprg) .gt. 180.0) icos_edge4(i1,i2,lon,nprg)= &
              icos_edge4(i1,i2,lon,nprg)-360.
!             180.-icos_edge4(i1,i2,lon,nprg)
     enddo
   enddo   
  enddo
  
  nnxp(ng)=npoints
  nnyp(ng)=1
    
  !- allocate arrays for lat,lon,land
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  !- lon/lat
  do i=1,nnxp(ng)
     r2d(i,1:nnyp(ng),lat)=icos_grid4(lat,i)
     r2d(i,1:nnyp(ng),lon)=icos_grid4(lon,i)
    ! print*,'latlon=',i,r2d(i,1:nnyp(ng),lat),r2d(i,1:nnyp(ng),lon)
  enddo

  ! write(111) icos_grid4(lat,:)
  ! write(112) icos_grid4(lon,:)

  ! for now land=1. everywhere
  r2d(:,:,3)=1.      

!-------------- for grads GS file to plot FIM grid points ------------------
!print*,"'reset'"
!print*,"'set cint 1'"
!print*,"'set lat -90 90'"
!print*,"'set lon -180 180'"
!print*,"'set mproj off'"
!print*,"'d co_bburn'"
!print*,"'set grads off'"
      !print *,'ycit=',lat_fire
      !print *,'xcit=',lon_fire!,area(nprg),test  !lat,lon
      !print*,'dummy=main(xcit,ycit)' 
     
!      do nprg=npoints/2-1000,npoints/2+1000
!      do nprg=fire_loc,fire_loc
      !if(nprg==1) !print *,'icos_grid4(1,nprg),icos_grid4(2,nprg)' !lat,lon
      !test=sqrt(area(nprg)*1.e-6)
      !!print*,"*nprg=",nprg
      !print *,'ycit=',icos_grid4(lat,nprg)
      !print *,'xcit=',icos_grid4(lon,nprg)!,area(nprg),test  !lat,lon
      !print*,'dummy=main(xcit,ycit)' 
!      if(ix==1) then
      !print *,'ycit=',icos_edge4(1,1,lat,nprg)
      !print *,'xcit=',icos_edge4(1,1,lon,nprg)!,icos_edge4(1,2,lat,nprg),icos_edge4(1,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
      !print *,'ycit=',icos_edge4(2,1,lat,nprg)
      !print *,'xcit=',icos_edge4(2,1,lon,nprg)!,icos_edge4(2,2,lat,nprg),icos_edge4(2,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
      !print *,'ycit=',icos_edge4(3,1,lat,nprg)
      !print *,'xcit=',icos_edge4(3,1,lon,nprg)!,icos_edge4(3,2,lat,nprg),icos_edge4(3,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
      !print *,'ycit=',icos_edge4(4,1,lat,nprg)
      !print *,'xcit=',icos_edge4(4,1,lon,nprg)!,icos_edge4(4,2,lat,nprg),icos_edge4(4,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
      !print *,'ycit=',icos_edge4(5,1,lat,nprg)
      !print *,'xcit=',icos_edge4(5,1,lon,nprg)!,icos_edge4(5,2,lat,nprg),icos_edge4(5,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
      !print *,'ycit=',icos_edge4(6,1,lat,nprg)
      !print *,'xcit=',icos_edge4(6,1,lon,nprg)!,icos_edge4(6,2,lat,nprg),icos_edge4(6,2,lon,nprg) ! lat,lon from first edge
      !print*,'dummy=main(xcit,ycit)' 
!      endif
!      enddo

  inquire(file='./htapLL_to_fimN.bin',exist=there)
  if(.not.there) then
     print*,'file not found: htapLL_to_fimN.bin, takes ~10 minutes to make' 
       OPEN(10,FILE='output_HTAPtoFIMtran.out')
       nprg=npoints ! South Pole
       nprg=1 ! North Pole
       WRITE(10,'(A,2I,2F13.5)')'npoints,nprg,Lat,Lon=',npoints,nprg,icos_grid4(1,nprg),icos_grid4(2,nprg)   
       WRITE(10,'(A,I6)')'Nprox=',icos_nprox(nprg)
       CALL FLUSH(10)
       WRITE(10,'(6I6)')(icos_prox(i1,nprg),i1=1,icos_nprox(nprg))
       CALL FLUSH(10)
!     WRITE(10,*)icos_edge4(1,1,lat,nprg),icos_edge4(1,1,lon,nprg),icos_edge4(1,2,lat,nprg),icos_edge4(1,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(1,1,lat,nprg),icos_edge4(1,1,lon,nprg),icos_edge4(1,2,lat,nprg),icos_edge4(1,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(2,1,lat,nprg),icos_edge4(2,1,lon,nprg),icos_edge4(2,2,lat,nprg),icos_edge4(2,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(3,1,lat,nprg),icos_edge4(3,1,lon,nprg),icos_edge4(3,2,lat,nprg),icos_edge4(3,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(4,1,lat,nprg),icos_edge4(4,1,lon,nprg),icos_edge4(4,2,lat,nprg),icos_edge4(4,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(5,1,lat,nprg),icos_edge4(5,1,lon,nprg),icos_edge4(5,2,lat,nprg),icos_edge4(5,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(6,1,lat,nprg),icos_edge4(6,1,lon,nprg),icos_edge4(6,2,lat,nprg),icos_edge4(6,2,lon,nprg) ! lat,lon from first edge
       i3=nprg
       DO i2=1,icos_nprox(i3)
       nprg=icos_prox(i2,i3)
       WRITE(10,'(A,I,2F13.5)')'nprg,Lat,Lon=',nprg,icos_grid4(1,nprg),icos_grid4(2,nprg)   
       WRITE(10,'(A,I6)')'Nprox=',icos_nprox(nprg)
       CALL FLUSH(10)
       WRITE(10,'(6I6)')(icos_prox(i1,nprg),i1=1,icos_nprox(nprg))
       CALL FLUSH(10)
!     WRITE(10,*)icos_edge4(1,1,lat,nprg),icos_edge4(1,1,lon,nprg),icos_edge4(1,2,lat,nprg),icos_edge4(1,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(1,1,lat,nprg),icos_edge4(1,1,lon,nprg),icos_edge4(1,2,lat,nprg),icos_edge4(1,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(2,1,lat,nprg),icos_edge4(2,1,lon,nprg),icos_edge4(2,2,lat,nprg),icos_edge4(2,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(3,1,lat,nprg),icos_edge4(3,1,lon,nprg),icos_edge4(3,2,lat,nprg),icos_edge4(3,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(4,1,lat,nprg),icos_edge4(4,1,lon,nprg),icos_edge4(4,2,lat,nprg),icos_edge4(4,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(5,1,lat,nprg),icos_edge4(5,1,lon,nprg),icos_edge4(5,2,lat,nprg),icos_edge4(5,2,lon,nprg) ! lat,lon from first edge
      WRITE(10,'(4F13.5)')icos_edge4(6,1,lat,nprg),icos_edge4(6,1,lon,nprg),icos_edge4(6,2,lat,nprg),icos_edge4(6,2,lon,nprg) ! lat,lon from first edge
       ENDDO
       CALL htap2fim(npoints,icos_nprox,icos_grid4,icos_edge4)
       CLOSE(10)
  endif  ! endof if(.not.there) then
      !test=sqrt(area(35)*1.e-6)
      !!print *,test  !lat,lon
      !test=sqrt(area(1000)*1.e-6)
      !!print *,test  !lat,lon
      !test=sqrt(area(5000)*1.e-6)
      !!print *,test  !lat,lon
      !test=sqrt(area(10000)*1.e-6)
      !!print *,test  !lat,lon
!print *,'return'

!print *,'function main(xcit,ycit)'
!print *,"'q w2xy 'xcit' 'ycit''"
!print *,'xcit=subwrd(result,3)'
!print *,'ycit=subwrd(result,6)'
!print *,"'set cmark 3'"
!print *,"'set cthick 10'"
!print *,"'set ccolor 13'"
!print *,"'draw mark 3 'xcit'  'ycit' 0.03'"
!print *,'return'
!-----------------------------------------------------------------------------------
  !
  !
  !.................
  ! determine the output dimension and arrays:
  ! these are not accurate
   nxgrads(ng)    = nnxp(ng)
   nygrads(ng)    = nnyp(ng)	     
   dep_glon(1,ng) = r2d(1,1,lon)
   dep_glon(2,ng) = r2d(2,1,lon)-r2d(1,1,lon)
   dep_glat(1,ng) = r2d(1,1,lat)
   dep_glat(2,ng) = r2d(2,1,lat)-r2d(1,1,lat)
   nxa(ng) = 1
   nxb(ng) = nnxp(ng)
   nya(ng) = 1
   nyb(ng) = nnyp(ng)
  
  !.................
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
   nvert = 1        !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nnzp(ng) &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
  !
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for visualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng),5)

  deallocate(r2d)

end subroutine FIM_model_grid_type
!-----------------------------------------------------------------------------
subroutine FV3_model_grid_type(ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch
use grid_dims_out 
implicit none
  include 'netcdf.inc'
integer, intent(in) :: ihour,iday,imon,iyear
!character (len=*), intent(in)  ::  chem_out_prefix

integer :: i,j,iv,ng,nfn,nfiles,nvert
integer :: ncid,stat,nDims, nVars, nAtts, unlimDimID,dval,id_var
integer :: istart(2),iend(2)
!real*8, allocatable, dimension(:,:) :: d2d,lonsv  ! Double precision lat,lons in FV3 files
real, allocatable, dimension(:,:) :: d2d,lonsv  ! Double precision lat,lons in FV3 files
character (len=240) :: fnames(maxfiles),dname
real, allocatable, dimension(:,:,:) :: r2d
!- for FV3 grid informations
integer :: nprg,npoints
integer, parameter :: lat =1 , lon =2 , land =3
real*8  d, d2r, dist, distance
logical              :: double_precision,there
character(16)	     :: header
integer :: fire_loc,nprox,prox,proxs,inv_perm,i3,isn,i2,i1,kk	! dummies, not needed
real*8 xh(4) ! holding variable for longitude

d2r = 4.0*ATAN(1.0)/180.0

print*,'FV3 grid directory=',trim(fv3_data_dir)
!- determine the grid configuration
ngrids = 6
proj_to_ll='NO'
nnzp(1:ngrids)=1

do ng=1,ngrids
!do ng=1,1
  nfn=1

  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  !-lon/lat configuration
! inquire(file='./icos_grid_level.dat',exist=there)
  WRITE(fnames(nfn),'(A5,I1,A)')'.tile',ng,'.nc'
  inquire(file=trim(fv3_data_dir)//trim(fnames(nfn)),exist=there)
  if(.not.there) then
     print*,'file not found:', fnames(nfn)
     stop 'at FV3_model_grid_type routine 1'
  endif

        stat = nf_open(trim(fv3_data_dir)//trim(fnames(nfn)), NF_NOWRITE, ncid)
        print*,'ncid,stat=',ncid,stat
  stat = nf_inq(ncid, nDims, nVars, nAtts, unlimDimID)
  do i = 1, nDims
    stat = nf_inq_dim(ncid, i, dname, dval)
!   if     (dname .eq. 'nx') then
    if     (dname .eq. 'grid_xt') then
    nnxp(ng)=dval
!   elseif (dname .eq. 'ny') then
    elseif (dname .eq. 'grid_yt') then
    nnyp(ng)=dval
    endif
  write(*,'(A,I,A,I,2A)')'dim,i=',i,' dval= ',dval,' name=',dname
  call flush(6)
  enddo
  !- allocate arrays for lat,lon,land
  if( allocated (r2d)) deallocate (r2d)
  allocate(r2d(nnxp(ng),nnyp(ng),3))
! FV3 file lat,lon are staggered grids (corner points), area is unstaggered
  istart(1)=1
  istart(2)=1
  iend(1)=nnxp(ng)+1
  iend(2)=nnyp(ng)+1
  !- longitude (x) and latitiude (y) from FV3 files
  if( allocated (d2d))then
  deallocate (d2d)
  deallocate (lonsv)
  endif
  allocate(d2d(iend(1),iend(2)))
  allocate(lonsv(iend(1),iend(2)))
! stat = nf_inq_varid ( ncid, 'x', id_var )
  stat = nf_inq_varid ( ncid, 'grid_lon', id_var )
  call ncvgt( ncid,id_var,istart,iend,d2d,stat)
  do i=1,nnxp(ng)+1
  do j=1,nnyp(ng)+1
   if(d2d(i,j) .gt. 180.0) d2d(i,j)=d2d(i,j)-360. ! longitude from -180. to 180.
  enddo
  enddo
  lonsv(:,:)=d2d(:,:)
  do i=1,nnxp(ng)
  do j=1,nnyp(ng)
!   r2d(i,j,lon)=d2d(i,j)
! 10/15/17 Fix dateline issue
  xh(1)=d2d(i,j)
  xh(2)=d2d(i+1,j)
  xh(3)=d2d(i,j+1)
  xh(4)=d2d(i+1,j+1)
  if(maxval(xh)- minval(xh).gt.179.)then
  r2d(i,j,lon)=0.
  do kk=1,4
  if(xh(kk).lt.0.)xh(kk)=xh(kk)+360.
  r2d(i,j,lon)=r2d(i,j,lon)+0.25*xh(kk)
  enddo
  if(r2d(i,j,lon).gt.180.)r2d(i,j,lon)=r2d(i,j,lon)-360.
! write(6,'(A,2I7,1P5D15.8)')'dateline fix,i,j,d2ds,r2d=',i,j,d2d(i,j), &
! d2d(i+1,j),d2d(i,j+1),d2d(i+1,j+1),r2d(i,j,lon)
! call flush(6)
  else
  r2d(i,j,lon)=.25*(d2d(i,j)+d2d(i+1,j)+d2d(i,j+1)+d2d(i+1,j+1)) !grid center lon
  endif
  enddo
  enddo

  stat = nf_inq_varid ( ncid, 'grid_lat', id_var )
  call ncvgt( ncid,id_var,istart,iend,d2d,stat)
  do i=1,nnxp(ng)
  do j=1,nnyp(ng)
!   r2d(i,j,lat)=d2d(i,j)
  r2d(i,j,lat)=.25*(d2d(i,j)+d2d(i+1,j)+d2d(i,j+1)+d2d(i+1,j+1)) !grid center lat
  enddo
  enddo
  call fv3_ltln_buff(iend(1),iend(2),d2d,lonsv,0) ! Buffer FV3 lat,lons into memory
! SAM 10/13/17, Hijack 3rd dimension of r2d (normally land) to be area in FV3 file
  iend(1)=nnxp(ng)
  iend(2)=nnyp(ng)
  if( allocated (d2d)) deallocate (d2d)
  allocate(d2d(iend(1),iend(2)))
  stat = nf_inq_varid ( ncid, 'area', id_var )
  call ncvgt( ncid,id_var,istart,iend,d2d,stat)
  do i=1,nnxp(ng)
  do j=1,nnyp(ng)
    r2d(i,j,land)=d2d(i,j)
!   write(*,'(A,2I5,2F11.5,1PE10.3)'),'i,j,lat,lon,area=',i,j,r2d(i,j,lat), &
!  r2d(i,j,lon),r2d(i,j,land)
  enddo
  enddo
  call flush(6)
  deallocate (d2d)
  deallocate (lonsv)
  call ncclos(ncid,stat)
! inquire(file='./htapLL_to_fimN.bin',exist=there)
! if(.not.there) then
!    print*,'file not found: htapLL_to_fimN.bin, takes ~10 minutes to make' 
!      OPEN(10,FILE='output_HTAPtoFIMtran.out')
!      nprg=npoints ! South Pole
!      nprg=1 ! North Pole
! endif  ! endof if(.not.there) then
  !
  !
  !.................
  ! determine the output dimension and arrays:
  ! these are not accurate
   nxgrads(ng)    = nnxp(ng)
   nygrads(ng)    = nnyp(ng)	     
   dep_glon(1,ng) = r2d(1,1,lon)
   dep_glon(2,ng) = r2d(2,1,lon)-r2d(1,1,lon)
   dep_glat(1,ng) = r2d(1,1,lat)
   dep_glat(2,ng) = r2d(2,1,lat)-r2d(1,1,lat)
   nxa(ng) = 1
   nxb(ng) = nnxp(ng)
   nya(ng) = 1
   nyb(ng) = nnyp(ng)
  
  !.................
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
   nvert = 1        !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nnzp(ng) &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
!STOP'EndodalineFV3_model_grid_type'
  !
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for visualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng),5)
enddo ! end of ng=1,ngrids loop

  deallocate(r2d)

end subroutine FV3_model_grid_type
!-----------------------------------------------------------------------------!-----------------------------------------------------------------------------
subroutine rams_grid_type(rams_anal_prefix,ihour,iday,imon,iyear)
use an_header
use mem_grid, only : nnxp, nnyp, nnzp, ngrids, nzg, npatch, stdlat2
use grid_dims_out 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear
character (len=*), intent(in)  :: rams_anal_prefix

integer :: i,iv,ng,nfn,nfiles,ivar_type,nvert
character (len=240) :: fnames(maxfiles),car_var(3),dim_var(3)& 
                      ,name_var(3),units_var(3)

data (car_var(iv), iv=1,3)/'lat', 'lon', 'land'/
data (dim_var(iv), iv=1,3)/'2d', '2d', '3d'/

real, allocatable, dimension(:,:,:) :: r2d


call chem_RAMS_filelist(fnames,trim(rams_anal_prefix)//'*-head.txt',maxfiles,nfiles,1)

call chem_RAMS_read_header(fnames(1)(1:len_trim(fnames(1))))

! --- All model informations have been collected

! -----------------
! -  start GRID LOOP   -
! -----------------

stdlat2 = 90.0

do ng=1,ngrids
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  nfn=1
  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  do iv=1,3
        !call determine_dim_var(car_var(iv),ivar_type)
        !print*,car_var(iv),ivar_type
	!stop 3
        call ep_getvar(iv,car_var(iv),dim_var(iv),name_var(iv),units_var(iv)&
	              ,r2d(:,:,iv), nnxp(ng),nnyp(ng),nnzp(ng),nzg,npatch&
		      ,ng,fnames(nfn)(1:len_trim(fnames(nfn))),ivar_type)
  enddo

  !.................
  ! determine the output dimension and arrays:
  !
  call geo_grid(nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2),  &
               dep_glon(1,ng),dep_glon(2,ng),	  &
               dep_glat(1,ng),dep_glat(2,ng),	  &
               rlatmin,rlatmax,rlonmin,rlonmax,   &
               nxgrads(ng),nygrads(ng), 	  &
               proj_to_ll(1:len_trim(proj_to_ll)))
  !.................            
  call Matriz_interp(ng,nxgrads(ng),nygrads(ng),   &
        	nnxp(ng),nnyp(ng),  	           &
        	dep_glat(1,ng),dep_glat(2,ng),     &
        	dep_glon(1,ng),dep_glon(2,ng),     &
        	iinf,jinf,rmi,  		   &
  		proj_to_ll(1:len_trim(proj_to_ll)),'VMP') ! mean_type='VMP' ! vizinho mais proximo
  	       !proj_to_ll(1:len_trim(proj_to_ll)),mean_type(1:len_trim(mean_type)))

  !_................
  call define_lim(ng,nxgrads(ng),nygrads(ng),	       &
    	         dep_glat(1,ng),dep_glat(2,ng),        &
    	         dep_glon(1,ng),dep_glon(2,ng),        &
    	         lati(ng),latf(ng),loni(ng),lonf(ng),  &
    	         nxa(ng),nxb(ng),nya(ng),nyb(ng),      &
    	         proj_to_ll(1:len_trim(proj_to_ll)),   &
    	         nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2))
  !.................
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
  nvert = 1         !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nvert &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)

  !- collect data that will be send output to produce emission files
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  !- performs the output at native and projected grids (for vizualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng),5)


  deallocate(r2d)
enddo ! enddo ngrids
end subroutine rams_grid_type
!-----------------------------------------------------------------------------

subroutine prep_sources(ng,n1,n2,n3,rlat,rlon,rland,ihour,iday,imon,iyear)

use mem_grid, only : xtn,ytn, xmn,ymn,deltaxn,deltayn,platn,plonn,polelat,polelon,ngrids
use grid_dims_out
use diurnal_cycle_edgar
use diurnal_cycle
use diurnal_cycle_fwb
implicit none
integer, intent(in) :: ng,n1,n2,n3,ihour,iday,imon,iyear
real, dimension(n1,n2) :: rlat,rlon,rland
integer :: local
!
!------------------------------------------------------------
local=ihour-3

!  RETRO anthropogenic emission 
if(use_retro == 1) then   
   call mem_retro(n1,n2,n3)
   call read_retro_antro(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
   	,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
   !if (use_diurnal_cycle == 1) then
   !     call cycle(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
   !             ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
   !endif
   
endif
!------------------------------------------------------------
!  Biogenic  emission 
if(use_bioge == 1) then
   call mem_bioge(n1,n2,n3)
   call read_bioge(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))

elseif(use_bioge == 2) then
   call mem_megan(n1,n2,n3)
   call read_megan(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  EDGAR anthropogenic emission 
if(use_edgar == 1.or.use_edgar == 2.or.use_edgar == 3) then
   call mem_edgar(n1,n2,n3)
   call read_edgar_antro(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))

   !if (use_diurnal_cycle == 1) then
   !     call cycle_edgar(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
   !             ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
   !endif

endif
!------------------------------------------------------------
!  GOCART anthropogenic emission 
if(use_gocart == 1) then
   call mem_gocart(n1,n2,n3)
   call read_gocart_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  CEDS anthropogenic emission 
   if(use_ceds == 1) then
   call mem_ceds(n1,n2,n3)
   call read_ceds_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
   ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
   endif
!------------------------------------------------------------
!  STREETS anthropogenic emission 
if(use_streets == 1) then
   call mem_streets(n1,n2,n3)
   call read_streets_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
                ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  SEAC4RS anthropogenic emission 
if(use_seac4rs == 1) then
   call mem_seac4rs(n1,n2,n3)
   call read_seac4rs_antro(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
                ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  GFEDv2 Biomass Burning emission 
if(use_GFEDv2 == 1)  then 
   call mem_gfedv2(n1,n2,n3)
   call read_gfedv2(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng),deltayn(ng)&
                ,xtn(1,ng),ytn(1,ng),platn(ng),plonn(ng))
endif		
!------------------------------------------------------------
!  FWB and AWB  emission 
!   woodfuels burning (including both fuelwood and charcoal burning),
!   residue and dung used as biofuels, 
!   burning of residues in the fields.  
if(use_fwbawb == 1)  then 
   call mem_fwbawb(n1,n2,n3)
   call read_fwbawb(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng),deltayn(ng)&
                ,xtn(1,ng),ytn(1,ng),platn(ng),plonn(ng))
   
   !if (use_diurnal_cycle == 1) then
   !	call cycle_fwb(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
   !             ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
   !endif

endif		
!------------------------------------------------------------
!  BBBEM Biomass Burning emission 
if(use_bbem .ne. 0)   then
   call mem_bbbem(n1,n2,n3)
   call process_bbbem(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
  	             ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  volcanic emission 
if(use_volcanoes == 1) then
   call mem_volcanoes(n1,n2,n3)
   call process_volcanoes(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
  		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
! degassing volcanic emission 
if(use_degass_volcanoes == 1) then
   call mem_degassing_volcanoes(n1,n2,n3)
   call process_degassing_volc(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
  		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  GOCART Background
if(use_gocart_bg == 1) then
   call mem_gocart_bg(n1,n2)
   call read_gocart_bg(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
    		,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
!  AFWA Erodability factors
if(use_afwa_erod == 1) then
   call mem_afwa_erod(n1,n2)
   call read_afwa_erod(iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
        ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif
!------------------------------------------------------------
! use GFEDv2 information for areas where 3BEM has not information
if(merge_gfedv2_bbem == 1)   then
   call merge_bburn(ng,ngrids,n1,n2,n3,rlat,rlon,rland)
endif

if(use_wb == 1) then   
   call mem_wb(n1,n2,n3)
   call read_wb(ihour,local,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltaxn(ng)&
               ,deltayn(ng),xtn(1,ng),ytn(1,ng),xmn(1,ng),ymn(1,ng),platn(ng),plonn(ng))
endif

end subroutine prep_sources
!-----------------------------------------------------------------------------
subroutine select_data(ng,n1,n2,n3)
!use chem_plus_aer_list

use chem1_list, only : chem_nspecies=>nspecies,spc_chem_alloc=>spc_alloc &
                      ,spc_chem_name=>spc_name

use aer1_list, only :  aer_nspecies=>nspecies,spc_aer_alloc=>spc_alloc, nmodes&
                      ,spc_aer_name=>aer_name
use grid_dims_out
use emiss_vars_emissions!, only : emiss_nspecies,emiss_spc_name&
                        !        ,emiss_g
use edgar_emissions, only :  edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name,edgar_g
use ceds_emissions, only :  ceds_nspecies=>nspecies&
                            ,ceds_spc_name=>spc_name,ceds_g

use gocart_emissions, only :  gocart_nspecies=>nspecies&
                            ,gocart_spc_name=>spc_name,gocart_g

use streets_emissions, only : streets_nspecies=>nspecies&
                            ,streets_spc_name=>spc_name,streets_g

use seac4rs_emissions, only : seac4rs_nspecies=>nspecies&
                            ,seac4rs_spc_name=>spc_name,seac4rs_g

use retro_emissions, only :  retro_nspecies=>nspecies&
                            ,retro_spc_name=>spc_name,retro_g

use bioge_emissions, only :  bioge_nspecies=>nspecies&
                            ,bioge_spc_name=>spc_name,bioge_g
			    
use megan_emissions, only :  megan_nspecies=>nspecies&
                            ,megan_spc_name=>spc_name,megan_g

use gfedv2_emissions, only : gfedv2_nspecies=>nspecies& !don't use AeM_nspecies
                            ,gfedv2_spc_name=>AeM_spc_name,gfedv2_g

use bbbem_emissions, only : bbbem_nspecies=>nspecies& !don't use AeM_nspecies
                           ,bbbem_spc_name=>AeM_spc_name,bbbem_g

use bbbem_plumerise, only : bbbem_plumerise_nspecies=>nspecies    & !don't use AeM_nspecies
                           ,bbbem_plumerise_spc_name=>AeM_spc_name&
			   ,bbbem_plume_g           &
			   ,bbbem_plume_fire_prop_g &
			   ,spc_suf,nveg_agreg,qarea_agreg&
			   ,bbbem_plume_mean_g      &
                           , qsizeIdx               &
                           , dsizeIdx               &
                           , QFRPWIdx               &
                           , dFRPWIdx              
use fwbawb_emissions, only : fwbawb_nspecies=>nspecies& !don't use AeM_nspecies
                            ,fwbawb_spc_name=>AeM_spc_name,fwbawb_g
			   
use volcanoes_emissions, only :volcanoes_nspecies=>nspecies& 
                           ,volcanoes_spc_name=>spc_name,volcanoes_g

use wb_emissions, only :  wb_nspecies=>nspecies&
                         ,wb_spc_name=>spc_name,wb_g

use volc_degassing_emissions, only :volc_degassing_nspecies=>nspecies& 
                           ,volc_degassing_spc_name=>spc_name,degassing_g

implicit none
integer, intent(in):: ng,n1,n2,n3
integer, parameter :: maxnspecies= 300
integer isp,ident,iespc,iv,i,j, use_mean_fraction_plumerise, ident_fwbawb,imode

character(len=8),dimension(maxnspecies) :: spc_name
integer,dimension(1,maxnspecies)        :: spc_alloc
integer nspecies
!- default values
spc_alloc(1,1:maxnspecies) = 0
spc_name(1:maxnspecies) = 'XXXX'

!- number total of chemistry + aerosol species
 nspecies=chem_nspecies + aer_nspecies*nmodes
!- test if  maxnspecies is less the nspecies
 if(nspecies > maxnspecies) stop '1-increase the parameter maxnspecies to:'!,nspecies

!- put chemistry and aerosol in only one array
!- chemistry section
 do isp=1,chem_nspecies
      spc_alloc(1,isp) = spc_chem_alloc(1,isp)
      spc_name(isp)    = spc_chem_name (isp)
      !print*,'spc_chem_name=',isp,spc_name(isp)
 enddo
 iv=chem_nspecies

!- aerosol section 
  do isp=1,aer_nspecies
   do imode= 1,nmodes
      iv=iv+1
      spc_alloc(1,iv) = spc_aer_alloc(1,imode,isp)
      spc_name(iv)    = spc_aer_name (imode,isp)
      !print*,'spc_aer_name=',isp,iv,spc_name(iv),spc_aer_alloc(1,imode,isp)	 
  enddo;enddo
  
!- determine the number of species (chemistry+aerosol) which will have source emissions
!- defined and will be write out for the atmospheric transport model
 emiss_nspecies = 0 
!- chemistry section
 do isp=1,chem_nspecies
  !if(isp.gt.chem_nspecies) exit
  if(spc_chem_alloc(1,isp) == 1) emiss_nspecies = emiss_nspecies+1
 enddo
!- aerosol section 
  do isp=1,aer_nspecies
   do imode= 1,nmodes
     if(spc_aer_alloc(1,imode,isp) == 1) emiss_nspecies=emiss_nspecies+1    
   enddo;enddo
!test if  maxnspecies is less the emiss_nspecies
 if(emiss_nspecies > maxnspecies) stop '2-increase the parameter maxnspecies to:'!,emiss_nspecies
 

!- allocate the memory needed for that 
 call  mem_emiss_vars(n1,n2,n3)
!
!
!--------------------- copy from scratch arrays to output array
!

!------ Part 1 : Anthropogenic sources ----------------------------------
!- in this section, the antropogenic sources will be defined for all species
!- in which exist data available (RETRO or EDGAR or FWBAWB, etc)
 iespc=0

!- loop at chem1-list/aer1-list of species and check only the one will have sources

if( use_retro  == 1 .or.  use_edgar >   0  .or. use_fwbawb  == 1 .or. 	&
    use_gocart == 1 .or.  use_streets ==1  .or. use_seac4rs == 1 &
    .or. use_ceds == 1  ) then
   
   
   do isp=1,nspecies

    if(spc_alloc(1,isp) == 1) then
  	
       ident = -1      
       !===>  use-retro
       if(use_retro == 1 ) then     
  	  print*,'- searching RETRO antro emission for specie= ',trim(spc_name(isp))
   
  	  !- try getting data from RETRO
  	  !- first get the address of specie (isp) at the RETRO 'space' (ident) 
  	  !
	  call get_retro_indentity(spc_name(isp),ident)
  	     
	     if(ident > 0) then
  	       ! ident > 0 => there exist source emission from retro database
  	          iespc=iespc+1
  	          emiss_spc_name(iespc,antro) 	 = retro_spc_name(ident)
  	          emiss_g(iespc)%src_antro(:,:,1)= retro_g(ident)%src(:,:,1)
  	          found_emiss_spc(iespc,antro)   = 1
  	     else
  	      !------------------------------------------------  src type,inv. name
  	      
	      call convert_inventory_to_mech_name(isp,iespc,ident,'antro','retro',spc_name(isp))
  	     endif ! ident > 0
  	
       endif 
  	
       !===> if not have, try EDGAR
       if(ident == -1 .and. use_edgar== 1) then
  	 
	      print*,'- searching EDGAR antro emission for specie= ',trim(spc_name(isp))
  	      
	      call get_edgar_indentity(spc_name(isp),ident)
  	      if(ident > 0) then
  		   iespc=iespc+1
  		   emiss_spc_name(iespc,antro)	  = edgar_spc_name(ident)
  		   emiss_g(iespc)%src_antro(:,:,1)= edgar_g(ident)%src(:,:,1)
  		   found_emiss_spc(iespc,antro)   = 1
  	      else
              call convert_inventory_to_mech_name(isp,iespc,ident,'antro','edgar',spc_name(isp))
              endif
       
       endif
       !===> if not have, try EDGAR 4.1
        if(ident == -1 .and. use_edgar== 2) then

              print*,'- searching EDGAR 4 antro emission for specie= ',trim(spc_name(isp))

              call get_edgar_indentity(spc_name(isp),ident)
              if(ident > 0) then
                   iespc=iespc+1
                   emiss_spc_name(iespc,antro)    = edgar_spc_name(ident)
                   emiss_g(iespc)%src_antro(:,:,1)= edgar_g(ident)%src(:,:,1)
                   found_emiss_spc(iespc,antro)   = 1
              else
              call convert_inventory_to_mech_name(isp,iespc,ident,'antro','edgar',spc_name(isp))
              endif

       endif
       !===> if not have, try EDGAR HTAP
        if(ident == -1 .and. use_edgar== 3) then

              print*,'- searching EDGAR-HTAP antro emission for specie= ',trim(spc_name(isp))

              call get_edgar_indentity(spc_name(isp),ident)

              if(ident > 0) then
                   iespc=iespc+1
                   emiss_spc_name(iespc,antro)    = edgar_spc_name(ident)
                   emiss_g(iespc)%src_antro(:,:,1)= edgar_g(ident)%src(:,:,1)
                   found_emiss_spc(iespc,antro)   = 1
              
	      else
                   call convert_inventory_to_mech_name(isp,iespc,ident,'antro','edgar',spc_name(isp))
 	          
		   if(ident == -1) then
		   !- if i am here, I am an aerosol.
		      call convert_edgar_to_aer(isp,iespc,ident,spc_name(isp),n1,n2)
		   endif
	      endif

       endif

       !===> try  GOCART
       if(ident == -1 .and. use_gocart== 1) then
  	 
	      print*,'- searching GOCART antro emission for specie= ',trim(spc_name(isp))
  	      
	      call get_GOCART_indentity(spc_name(isp),ident)
  	      if(ident > 0) then
  		   iespc=iespc+1
  		   emiss_spc_name(iespc,antro)	  = GOCART_spc_name(ident)
  		   emiss_g(iespc)%src_antro(:,:,1)= GOCART_g(ident)%src(:,:,1)
  		   found_emiss_spc(iespc,antro)   = 1
  	      endif
       
       endif

       !===> if not have, try CEDS
        if(ident == -1 .and. use_ceds== 1) then

              print*,'- searching CEDS antro emission for specie= ',trim(spc_name(isp))

              call get_ceds_indentity(spc_name(isp),ident)

              if(ident > 0) then
                   iespc=iespc+1
                   emiss_spc_name(iespc,antro)    = ceds_spc_name(ident)
                   emiss_g(iespc)%src_antro(:,:,1)= ceds_g(ident)%src(:,:,1)
                   found_emiss_spc(iespc,antro)   = 1

              else
                   call convert_inventory_to_mech_name(isp,iespc,ident,'antro','ceds',spc_name(isp))
        
!                  if(ident == -1) then
!                  !- if i am here, I am an aerosol.
!                     call
!                     convert_edgar_to_aer(isp,iespc,ident,spc_name(isp),n1,n2)
!                  endif
              endif

       endif

       !===> try  STREETS
       if(ident == -1 .and. use_streets== 1) then
        
              print*,'- searching STREETS antro emission for specie=',trim(spc_name(isp))
        
              call get_STREETS_indentity(spc_name(isp),ident)
              if(ident > 0) then
                   iespc=iespc+1
                   emiss_spc_name(iespc,antro)    = STREETS_spc_name(ident)
                   emiss_g(iespc)%src_antro(:,:,1)= STREETS_g(ident)%src(:,:,1)
                   found_emiss_spc(iespc,antro)   = 1
              endif

       endif

       !===> try  SEAC4RS
       if(ident == -1 .and. use_seac4rs== 1) then
        
              print*,'- searching SEAC4RS antro emission for specie=',trim(spc_name(isp))
        
              call get_SEAC4RS_indentity(spc_name(isp),ident)
              if(ident > 0) then
                   iespc=iespc+1
                   emiss_spc_name(iespc,antro)    = SEAC4RS_spc_name(ident)
                   emiss_g(iespc)%src_antro(:,:,1)= SEAC4RS_g(ident)%src(:,:,1)
                   found_emiss_spc(iespc,antro)   = 1
              endif

       endif

       if(use_fwbawb == 1 ) then     
  	     print*,'- searching FWB and AWB antro emission for specie= ',trim(spc_name(isp))
  	     call get_fwbawb_indentity(spc_name(isp),ident_fwbawb)
  	     if(ident_fwbawb > 0) then
  	       if(ident == -1) then 
	           iespc=iespc+1
  	           emiss_spc_name(iespc,antro)    = fwbawb_spc_name(ident_fwbawb)
    	           found_emiss_spc(iespc,antro)   = 1
		   ident = ident_fwbawb
	       endif	
	       emiss_g(iespc)%src_antro(:,:,1) = emiss_g(iespc)%src_antro(:,:,1) + &
	                                         fwbawb_g(ident_fwbawb)%src(:,:,1)
             else
               !- not working yet
	       !call convert_inventory_to_mech_name(isp,iespc,ident,'antro','fwbawb')
	       !if(ident == -1) then
               !  !- if i am here, I might be an aerosol.
	       !  call convert_AeM_to_aer(isp,iespc,ident,'antro' ,spc_name(isp),n1,n2)
               !endif
	    endif
       endif

       if(ident == -1) then
  	  print*,'No antro emissions for "',trim(spc_name(isp)), '" was found'
       endif

     endif !- if spc_alloc

   enddo !- do nspecies

endif
!---------------------------------------------------------------end 
!
!- Part 2 : Biom. Burning sources -----------------------------start 
!- in this section, the bio burn sources will be defined for all species
!- in which exist data available
if(use_gfedv2 == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) then
    print*,'- searching bio burn emission for specie= ',trim(spc_name(isp))

    !- try gettting data from GFEDv2
    !- first get the address of specie (isp) at the GFEDv2 'space' (ident) 
     call get_GFEDv2_indentity(spc_name(isp),ident)
     if(ident > 0) then

       ! ident > 0 => there exist source emission from retro database
       iespc=iespc+1
       emiss_spc_name(iespc,bburn)         = gfedv2_spc_name(ident)
       emiss_g(iespc)%src_bburn(:,:,1)=gfedv2_g(ident)%src(:,:,1)
       found_emiss_spc(iespc,bburn) = 1

     else
      !------------------------------------------------  src type  ,inv. name

       call convert_inventory_to_mech_name(isp,iespc,ident,'bburn'  ,'AeM',spc_name(isp))
     
     endif ! ident > 0
       
  endif ! if species
 enddo !loop species
endif 
!---------------------------------------------------------------end 

!- Part 3 : Biom. Burning sources -----------------------------start 
!- in this section, the bio burn sources will be defined for all species
!- in which exist data available
if(use_bbem /= 0) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) then
!KY  print*,'- searching bio-burn emission for specie= ',trim(spc_name(isp))

     !- try gettting data from 3BEM
     !- first get the address of specie (isp) at the 3BEM 'space' (ident) 
     call get_bbbem_indentity(spc_name(isp),ident)
     if(ident > 0) then
       ! ident > 0 => there exist source emission from retro database
       iespc=iespc+1
       emiss_spc_name(iespc,bburn)    = bbbem_spc_name(ident)
       emiss_g(iespc)%src_bburn(:,:,1)= bbbem_g(ident)%src(:,:,1)
       found_emiss_spc(iespc,bburn)   = 1
!KY    print*,"BB=",emiss_spc_name(iespc,bburn),iespc,bbbEM_spc_name(ident),ident
     else
      !------------------------------------------------  
      call convert_inventory_to_mech_name(isp,iespc,ident,'bburn'  ,'AeM',spc_name(isp))

      if(ident == -1) then
        !- if i am here, I might be an aerosol.
	call convert_AeM_to_aer(isp,iespc,ident,'bburn' ,spc_name(isp),n1,n2)
      endif
      
     endif ! ident > 0
  endif
 enddo
endif
!---------------------------------------------------------------end 

!- Part 4 : Plumerise data for biom. burn. sources -----------start 
if(use_bbem_plumerise == 1) then
  
 !- this section is for the 3BEM version 1
 if(use_bbem == 1) then
      use_mean_fraction_plumerise = 1
      iespc=0
      if( use_mean_fraction_plumerise == 0 ) then
   	 !- loop at chem1-list of species and check only the one will have sources
   	 do isp=1,nspecies
   	  if(spc_alloc(1,isp) == 1) then
   	    print*,'- searching PLUME bio burn emission for specie= ',trim(spc_name(isp))
   	    call get_bbbem_indentity(spc_name(isp),ident)
   	    if(ident > 0) then
   		iespc=iespc+1
   	    
   		do iv=1,nveg_agreg
   		   
   		   emiss_plume_name(iespc,iv)	= bbbem_spc_name(ident)!//'_'//spc_suf(iv)
   		   !print*,'emiss_plume_name=',iespc,iv, emiss_plume_name(iespc,iv)
   		   
   		   emiss_plume_g   (iespc,iv)%src_plume(:,:)= bbbem_plume_g(ident,iv)%src(:,:,1)       
   		   
   		   found_emiss_plume(iespc,iv) = 1
   		   print*,'found plume=',emiss_plume_name(iespc,iv),iespc
   		enddo
   	    endif
   	   endif
   	 enddo
      else
   	 iespc=iespc+1
   	
   	 do iv=1,nveg_agreg
   	    
   	    emiss_plume_name(iespc,iv)   = 'mean_fct'
   	    !print*,'emiss_plume_name=',iespc,iv, emiss_plume_name(iespc,iv)
   	    
   	    emiss_plume_g   (iespc,iv)%src_plume(:,:)= bbbem_plume_mean_g(iv)%src(:,:,1)	
   	    
   	    found_emiss_plume(iespc,iv) = 1
   	    print*,'found plume=',emiss_plume_name(iespc,iv),iespc
   	 enddo
      endif
      ! special section for the aggregated fire size
      iespc=iespc+1
      do iv=1,nveg_agreg
   	  emiss_plume_name(iespc,iv)= 'firesize'!//'_'//trim(spc_suf(iv))
       
   	  emiss_plume_g   (iespc,iv)%src_plume(:,:)=  bbbem_plume_fire_prop_g(qarea_agreg,iv)%fire_prop(:,:)	   
       
   	  found_emiss_plume(iespc,iv) = 1
   	  print*,'found plume=',emiss_plume_name(iespc,iv),iespc
      enddo
 
 else if(use_bbem == 2) then ! new section for FRP methodology
      iespc=0
      iv = 1 ! aggr vegetation is only 1 for this method
!     ---------------------------------------------------------------------------------------
!     name	     UNit	 description (portuguese)
!     ---------------------------------------------------------------------------------------
!     meanFRP	     MW 	 Valor medio de FRP dos fogos detectados dentro da grade.
!     std FRP        MW
!     meanFsize      km2	 area media dos fogos detectados na grade.
!     std Fsize      km2
!     MediaEC	     kW/m2	 valor medio da energia convectiva do fogo.
!     FsizeTotal     km2	 area total dos fogos detetados na grade.
!     FRE	     MJ 	 Energia radiativa do fogo (Representa o total de biomassa
!    				  consumida pelo fogo).
!     NOBS	     -  	 Numero de fogos detetados dentro da grade.
!     ---------------------------------------------------------------------------------------
      
      !- fraction of the total mass emitted during the flaming phase
      iespc=iespc+1	
      emiss_plume_name (iespc,iv)             = 'flam_frac'          !'mean_fct', !  RAR: the name was changed
      emiss_plume_g    (iespc,iv)%src_plume(:,:)= bbbem_plume_mean_g(iv)%src(:,:,1)	 
      found_emiss_plume(iespc,iv)		= 1  
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc
      !- mean _and_ std of FRP
      iespc=iespc+1	
      emiss_plume_name (iespc,iv)		= 'mean_frp'
      emiss_plume_g    (iespc,iv)%src_plume(:,:)= bbbem_plume_fire_prop_g(QFRPWIdx,iv)%fire_prop(:,:)	 
      found_emiss_plume(iespc,iv)		= 1  
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc
      
      iespc=iespc+1	
      emiss_plume_name (iespc,iv)		= 'std_frp'
      emiss_plume_g    (iespc,iv)%src_plume(:,:)= bbbem_plume_fire_prop_g(DFRPWIdx,iv)%fire_prop(:,:)	 
      found_emiss_plume(iespc,iv)		= 1  
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc

      !- mean _and_ std of size of the fires
      iespc=iespc+1	
      emiss_plume_name (iespc,iv)		= 'mean_size'
      emiss_plume_g    (iespc,iv)%src_plume(:,:)= bbbem_plume_fire_prop_g(QSIZEIdx,iv)%fire_prop(:,:)	 
      found_emiss_plume(iespc,iv)		= 1  
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc
      
      iespc=iespc+1	
      emiss_plume_name (iespc,iv)		= 'std_size'
      emiss_plume_g    (iespc,iv)%src_plume(:,:)= bbbem_plume_fire_prop_g(DSIZEIdx,iv)%fire_prop(:,:)	 
      found_emiss_plume(iespc,iv)		= 1  
      print*,'found plume=',emiss_plume_name(iespc,iv),iespc

 endif
endif

!---------------------------------------------------------------end 

!- Part 5 : Biogenic data  sources                 -----------start 
if(use_bioge == 1 .or. use_bioge == 2 ) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
  do isp=1,nspecies
     if(spc_alloc(1,isp) == 1) then
         print*,'- searching biogenic emission for specie= ',trim(spc_name(isp))

         !- try gettting data from bioge
         !- first get the address of specie (isp) at the bioge 'space' (ident)          
         if (use_bioge == 1 ) then 
            call get_bioge_indentity(spc_name(isp),ident)
	 elseif (use_bioge == 2) then
	    call get_megan_indentity(spc_name(isp),ident)
	 endif
	 if(ident > 0) then
             ! ident > 0 => there exist source emission from biogenic database
             iespc=iespc+1
	     if (use_bioge == 1 ) then	     
               emiss_spc_name(iespc,bioge)	 = bioge_spc_name(ident)
               emiss_g(iespc)%src_bioge(:,:,1)=bioge_g(ident)%src(:,:,1)
               found_emiss_spc(iespc,bioge) = 1
               print*,emiss_spc_name(iespc,bioge),iespc,bioge_spc_name(ident),ident
             elseif (use_bioge == 2) then
	       emiss_spc_name(iespc,bioge)	 = megan_spc_name(ident)
               emiss_g(iespc)%src_bioge(:,:,1)=megan_g(ident)%src(:,:,1)
               found_emiss_spc(iespc,bioge) = 1
               print*,emiss_spc_name(iespc,bioge),iespc,megan_spc_name(ident),ident
	     endif
	 else
	     !------------------------------------------------  src type ,inv. name
        
	     if (use_bioge == 1 ) then
                 call convert_inventory_to_mech_name(isp,iespc,ident,'bioge' ,'bioge',spc_name(isp))
	     elseif (use_bioge == 2) then
	         call convert_inventory_to_mech_name(isp,iespc,ident,'megan' ,'megan',spc_name(isp))
	     endif
     
         endif ! ident > 0
  	 
     endif ! if species
  enddo !loop species
endif 
!---------------------------------------------------------------end 

!- Part 6 : Volcanic eruption data  sources                 -----------start 
if(use_volcanoes == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
  do isp=1,nspecies
     if(spc_alloc(1,isp) == 1) then
         print*,'- searching eruption volcanic emission for specie= ',trim(spc_name(isp))

         !- try gettting data from geoge
         !- first get the address of specie (isp) at the bioge 'space' (ident) 
          call get_volc_indentity(spc_name(isp),ident)
         
	 if(ident > 0) then
             ! ident > 0 => there exist source emission from volcanoes database
             iespc=iespc+1
             emiss_spc_name(iespc,geoge)    = volcanoes_spc_name(ident)
             emiss_g(iespc)%src_geoge(:,:,1)=volcanoes_g(ident)%src(:,:,1)
             found_emiss_spc(iespc,geoge) = 1
             print*,emiss_spc_name(iespc,geoge),iespc,volcanoes_spc_name(ident),ident
         
	 else
         
  	   call convert_volc_erup_to_aer(isp,iespc,ident,spc_name(isp))
       
         endif ! ident > 0
  	 
     endif ! if species
  enddo !loop species
endif 
!---------------------------------------------------------------end 

!- Part 7 : Volcanic DEGASSING data  sources       -----------start 
if(use_degass_volcanoes == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
  do isp=1,nspecies
     if(spc_alloc(1,isp) == 1) then
         print*,'- searching DEGASSING volcanic emission for specie= ',trim(spc_name(isp))

         !- try gettting data from geoge
         !- first get the address of specie (isp) at the bioge 'space' (ident) 
          call get_degass_volc_indentity(spc_name(isp),ident)
         
	 if(ident > 0) then
             ! ident > 0 => there exist source emission from volcanoes database
             iespc=iespc+1
             emiss_spc_name(iespc,geoge)	 = volc_degassing_spc_name(ident)
             emiss_g(iespc)%src_geoge(:,:,1)=degassing_g(ident)%src(:,:,1)
             found_emiss_spc(iespc,geoge) = 1
             print*,emiss_spc_name(iespc,geoge),iespc,volcanoes_spc_name(ident),ident
         
	 else
         
	   !  call convert_geoge_to_racm(isp,iespc,ident)
         
         endif ! ident > 0
  	 
     endif ! if species
  enddo !loop species
endif 
!---------------------------------------------------------------end 


!- Part 8 : WB sources -----------------------------start 
if(use_wb == 1) then
 iespc=0
 !- loop at chem1-list of species and check only the one will have sources
 do isp=1,nspecies
  if(spc_alloc(1,isp) == 1) then
    print*,'- searching WB emission for specie=',trim(spc_name(isp));call flush(6)
     !- first get the address of specie (isp) at the 3BEM 'space' (ident) 
     call get_wb_indentity(spc_name(isp),ident)
     if(ident > 0) then
       ! ident > 0 => there exist source emission from retro database
       iespc=iespc+1
       emiss_spc_name(iespc,bburn)          = wb_spc_name(ident)
       emiss_g(iespc)%src_bburn(:,:,1)= wb_g(ident)%src(:,:,1)
       found_emiss_spc(iespc,bburn)   = 1
       print*,emiss_spc_name(iespc,bburn),iespc,wb_spc_name(ident),ident;call flush(6)
     else
      !------------------------------------------------  src type  ,inv. name
      !call convert_inventory_to_mech_name(isp,iespc,ident,'bburn'  ,'AeM',spc_name(isp))
     endif ! ident > 0
  endif
 enddo
endif
!---------------------------------------------------------------end 

end subroutine select_data
!-----------------------------------------------------------------------------

subroutine output(ng,nvert,ihour,iday,imon,iyear,ngrids, nxp, nyp, nzp,ihtran)
use chem1_list, only: chemical_mechanism
use grid_dims_out
use emiss_vars_emissions, only : emiss_nspecies,number_sources,emiss_spc_name&
                                ,emiss_g,found_emiss_spc&
				,emiss_plume_g, found_emiss_plume
use bbbem_plumerise, only: nveg_agreg
				
use volcanoes_emissions, only	: INJH,DURA,volcanoesP_g, ELEV_ERUPTION=>ELEV
		
use volc_degassing_emissions, only : PLUM_HEIGTH,degassingP_g,ELEV_DEGASS=>ELEV
implicit none
integer, intent(in) :: ng,ihour,iday,imon,iyear,nvert,ngrids, nxp, nyp, nzp,ihtran

!- local arrays, ...
character*240 wfln(3)
character*240 wfln_maproj ! NUWRF EMK for plotting
integer isp,nrec,iftimes,nsrc,iunit_bin,iv,iunit_out,output_byte_size
character*2 ccgrid
real, allocatable, dimension(:,:,:) :: routgrads
real, pointer, dimension(:,:,:) :: src_dummy
real, pointer, dimension(:,:) :: src_dummy_2d
real, dimension(nxp,nyp) :: dummy
real :: real_byte_size

INQUIRE (IOLENGTH=output_byte_size) real_byte_size  ! Inquire by output list
print*,"output_byte_size=",output_byte_size

!unit of bin data file (visualization) 
iunit_bin=19
!unit of output data file (for the atmos model) 
iunit_out=20

!- allocate space for output array
allocate (routgrads(nxgrads(ng),nygrads(ng),nvert))

!- build the files names for grads/output format
iftimes=10000*ihour
write(ccgrid,'(a1,i1)') 'g',ng

!-- gra file
call makefnam(wfln(1),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid,'gra')
!- ctl file
call makefnam(wfln(2),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid,'ctl')
!- vfm or hdf file
call makefnam(wfln(3),chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid, &
	     chem_out_format(1:len_trim(chem_out_format)))
! NUWRF EMK...For plotting
call makefnam(wfln_maproj,chem_out_prefix(1:len_trim(chem_out_prefix))//' '&
             ,0,iyear,imon,iday,iftimes,'T',ccgrid,'maproj')
call write_maproj(ng,iunit_bin,wfln_maproj)

!- open and write the grads control file
print*,"ihtran=",ihtran
   call write_ctl(ng,iunit_bin,wfln(:),ihour,iday,imon,iyear,nvert, ihtran)

!- open the binary data file for GRADS vizualization file
   open(iunit_bin,file=wfln(1),form='unformatted',access='direct',status='unknown',  &
        recl=output_byte_size*(nxb(ng)-nxa(ng)+1)*(nyb(ng)-nya(ng)+1))	  
   nrec=0
  
!- open the output file at native grid to be read by the atmos model
   open(iunit_out,file=wfln(3),form='formatted',status='replace')
   isp  = 0
   nsrc = 0
   iv   = 0
   call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
 
   do isp=1,emiss_nspecies 
    do nsrc=1,number_sources
       
       if(found_emiss_spc(isp,nsrc) == 0) cycle ! if not found, cycle
       
       
       if(nsrc==1) src_dummy => emiss_g(isp)%src_antro(:,:,:)
       if(nsrc==2) src_dummy => emiss_g(isp)%src_bburn(:,:,:)
       if(nsrc==3) src_dummy => emiss_g(isp)%src_bioge(:,:,:)
       if(nsrc==4) src_dummy => emiss_g(isp)%src_geoge(:,:,:)
        
       !- this is the output at native grid to be read by the atmos model
       !--- V-format 
       if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
        call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
	call vforec(iunit_out,src_dummy(:,:,1),nxp*nyp,24,dummy,'LIN')
       
       endif
       !--- HDF 5 format

      !>>>>> inserir chamada para formato HDF5 : escrever  o array: src_dummy(:,:,1)

       
       !- this output is for GRADS vizualization file
       !--- project the grid if needed
!KY    call proj_rams_to_grads(nxp        ,nyp        , nvert     &
!KY                     ,nxgrads(ng),nygrads(ng), nvert        &
!KY                     ,rmi,iinf,jinf		             &
!KY                     ,src_dummy                             &
!KY      ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
!KY    print*,'emissions rec=',nrec, emiss_spc_name(isp,nsrc)
!KY    call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
!KY                   nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
    enddo
   enddo
!- plumerise section
   nsrc = 2 ! bburn only
   !---- plumerise section
   
   if(use_bbem_plumerise == 1) then
    do isp=1,emiss_nspecies+1 
       do iv=1,nveg_agreg
          
	  !print*,' isp - iv=',isp,iv
	  !print*,'found=',found_emiss_plume(isp,iv)
          
          if(found_emiss_plume(isp,iv) == 0) cycle
          src_dummy_2d => emiss_plume_g(isp,iv)%src_plume(:,:)
          !print*,'emiss_nspecies=',emiss_nspecies
          !print*,'isp=',isp, 'emiss_spc_name=',emiss_spc_name(isp,bburn);call flush(6)
          !- this is the output at native grid to be read by the atmos model
          !--- V-format 
          if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
           call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
           call vforec(iunit_out,src_dummy_2d(:,:),nxp*nyp,24,dummy,'LIN')
         
          endif
          !--- HDF 5 format

!>>>>>          ! inserir chamada para formato HDF5 : escrever  o array: src_dummy(:,:,1)



          !- this out put is for GRADS vizualization file
          !--- project the grid if needed
!KY       call proj_rams_to_grads(nxp        ,nyp   , nvert     &
!KY                        ,nxgrads(ng),nygrads(ng), nvert        &
!KY                        ,rmi,iinf,jinf		                &
!KY                        ,src_dummy_2d                          &
!KY         ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
       
!KY       print*,'plume rec=',nrec
!KY       call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
!KY                   nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
       enddo
    enddo
   endif
 !- volcanoes properties section
   nsrc = 4 ! geoge only
   
   if(use_volcanoes == 1) then
          
	  
	  ! writing - INJ Height and duration
      do isp=1,3
	  
	  if(isp==1) iv=INJH 
	  if(isp==2) iv=ELEV_ERUPTION 
	  if(isp==3) iv=DURA
	  src_dummy_2d => volcanoesP_g(iv )%prop
          
          !--- V-format 
          if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
           call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
           call vforec(iunit_out,src_dummy_2d(:,:),nxp*nyp,24,dummy,'LIN')
         
          endif

          !- this output is for GRADS vizualization file
          !--- project the grid if needed
          call proj_rams_to_grads(nxp	     ,nyp   , nvert	    &
 	  			 ,nxgrads(ng),nygrads(ng), nvert    &
 	  			 ,rmi,iinf,jinf 		    &
 	  			 ,src_dummy_2d  	            &
	  			 ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
           
           call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
          	       nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
	   print*,'eruption volc rec=',nrec
       enddo
    
   endif

 !- degassing volcanoes properties section
   nsrc = 4 ! geoge only
   
   if(use_degass_volcanoes == 1) then
          
	  ! writing - Cloud top and vent elevation
      do isp=1,2
	  if(isp==1) iv=plum_heigth 
	  if(isp==2) iv=ELEV_DEGASS 
	  
	  src_dummy_2d => degassingP_g(iv)%prop  	 
          
          !--- V-format 
          if(chem_out_format(1:len_trim(chem_out_format)) == 'vfm') then
           call write_header(iunit_out,ihour,iday,imon,iyear,ng,nxp,nyp,nvert,isp,nsrc,iv)
           call vforec(iunit_out,src_dummy_2d(:,:),nxp*nyp,24,dummy,'LIN')
         
          endif

          !- this output is for GRADS vizualization file
          !--- project the grid if needed
          call proj_rams_to_grads(nxp	     ,nyp   , nvert	  &
 	  			 ,nxgrads(ng),nygrads(ng), nvert  &
 	  			 ,rmi,iinf,jinf 	          &
 	  			 ,src_dummy_2d  	          &
	  			 ,routgrads,trim(proj_to_ll(1:len_trim(proj_to_ll))))
           
           call write_bin(iunit_bin,nrec,nxgrads(ng),nygrads(ng),nvert,&
          	       nxa(ng),nxb(ng),nya(ng),nyb(ng),routgrads)
           print*,'degassing volc rec=',nrec
       enddo
    
   endif


close(iunit_bin)
close(iunit_out)

deallocate(routgrads)


if(trim(special_output_to_wrf)=='YES' .or. trim(special_output_to_wrf)=='yes' .or. &
   trim(grid_type)=='fim' .or. trim(grid_type)=='fv3') then
!if(chemical_mechanism)=='RADM_WRF_FIM')then
#if RADM_WRF_FIM || RADM_FV3
   print *, 'CONVERT TO WRF/FIM/FV3 '
   call write_cptec_to_wrf_fim(ng,wfln(3),iday,imon,iyear)
#else
   print*, 'Module WRF not installed. See how to compile it in INSTALL doc '
   stop 555
#endif

endif


end subroutine output
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
subroutine gen_regional_grid_type(ihour,iday,imon,iyear)

use mem_grid
use grid_dims_out 
use wps_util 
implicit none
integer, intent(in) :: ihour,iday,imon,iyear

integer :: i,iv,ng,nfn,nfiles,ivar_type,nvert


real, allocatable, dimension(:,:,:) :: r2d
real, allocatable, dimension(:,:) :: cornr_lat, cornr_lon ! Corner lat/lons for fv3/wps type ceds emiss case, saved in buffer
real, allocatable, dimension(:,:) :: area

!- determine the grid configuration
nnzp(1:ngrids)=1

if_adap=0 ! sigmaz
if(trim(grid_type) =='cartesian') ihtran = 0
if(trim(grid_type) =='polar')     ihtran = 1
if(trim(grid_type) =='lambert')   ihtran = 2
if(trim(grid_type) =='mercator')  ihtran = 3
if(trim(grid_type) =='rotated_ll')  ihtran = 4
if(trim(grid_type) =='lat-lon')  ihtran = 5

print*,'Projecion=',trim(grid_type)
print*,'Grid size=',ngrids, nnxp(1:ngrids),nnyp(1:ngrids)

if( (maxval(nnxp(1:ngrids)) .gt. nxpmax) .or. &
    (maxval(nnyp(1:ngrids)) .gt. nypmax) ) then
    print*,'too many nnxp or nnyp points'
    print*,'increase NXPMAX/NYPMAX at grid_dims.f90'
    stop 22
endif

allocate(grid_g(ngrids),gridm_g(ngrids))
do ng=1,ngrids
   call nullify_grid(grid_g(ng)) ; call nullify_grid(gridm_g(ng))
   call alloc_grid(grid_g(ng),nnzp(ng),nnxp(ng),nnyp(ng),ng,if_adap) 
   call alloc_grid(gridm_g(ng),1,1,1,ng,if_adap)
   
!srf   call filltab_grid(grid_g(ng),gridm_g(ng),0  &
!srf          ,nnzp(ng),nnxp(ng),nnyp(ng),ng) 

enddo
!- Basic grid coordinate setup
!print*,'going to gridsetup' ; call flush(6)
   call grid_setup(1)
   call grid_setup(2)
! --- All model informations have been collected

! NU-WRF WPS map projection
#ifdef WPS
call wps_calc_latlon() 
#endif

! -----------------
! -  start GRID LOOP   -
! -----------------

do ng=1,ngrids
  allocate(r2d(nnxp(ng),nnyp(ng),3))
  nfn=1
  ! determine geographical informations of the grids (lat,lon) of T-points 
  ! and land fraction
  r2d(:,:,1)= grid_g(ng)%glat(:,:)
  r2d(:,:,2)= grid_g(ng)%glon(:,:)
  r2d(:,:,3)= 1. ! rland, not defined a priori

#ifdef WPS
print*,"cheque r2d with NUWRF impl"
  allocate(cornr_lat(nnxp(ng)+1,nnyp(ng)+1))
  allocate(cornr_lon(nnxp(ng)+1,nnyp(ng)+1))
  cornr_lat(:,:)= wps_grids(ng)%lat_x(:,:)
  cornr_lon(:,:)= wps_grids(ng)%lon_x(:,:)
  call fv3_ltln_buff(nnxp(ng)+1,nnyp(ng)+1,cornr_lat,cornr_lon,0) ! Buffer corner lat,lons into memory
! NUWRF...Copy WPS map projection values
  r2d(:,:,1)= wps_grids(ng)%lat_m(:,:)
  r2d(:,:,2)= wps_grids(ng)%lon_m(:,:)
  r2d(:,:,3)= 1. ! rland, not defined a priori  
 
! For Cassini grid, get gridded area (m^2) on sphere, save in r2d(:,:,third dimension=3)
     if(trim(grid_type) =='lat-lon')then
     call get_area_byX(r2d,nnxp(ng),nnyp(ng),3,cornr_lat,cornr_lon)
! Note, third dimension of r2d is now area (meter**2) for this grid type
     endif

  deallocate(cornr_lat)
  deallocate(cornr_lon)
#endif

  print *,'ng = ',ng
  print *,'nnxp(ng),nnyp(ng) = ',nnxp(ng),nnyp(ng)
  print *,'lower-left lat,lon:  ',r2d(1,1,1),r2d(1,1,2)
  print *,'upper-left lat,lon:  ',r2d(1,nnyp(ng),1),r2d(1,nnyp(ng),2)
  print *,'lower-right lat,lon: ',r2d(nnxp(ng),1,1),r2d(nnxp(ng),1,2)
  print *,'upper-right lat,lon: ', &
       r2d(nnxp(ng),nnyp(ng),1),r2d(nnxp(ng),nnyp(ng),2)

!print*,'lat= ',r2d(:,:,1)
  !print*,'lon= ',r2d(:,:,2); call flush(6)
  
!  do iv=1,3
!        !call determine_dim_var(car_var(iv),ivar_type)
!        !print*,car_var(iv),ivar_type
!        call ep_getvar(iv,car_var(iv),dim_var(iv),name_var(iv),units_var(iv)&
!	              ,r2d(:,:,iv), nnxp(ng),nnyp(ng),nnzp(ng),nzg,npatch&
!		      ,ng,fnames(nfn)(1:len_trim(fnames(nfn))),ivar_type)
!  enddo

  !.................
  ! determine the output dimension and arrays:
  !
  call geo_grid(nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2),  &
               dep_glon(1,ng),dep_glon(2,ng),	  &
               dep_glat(1,ng),dep_glat(2,ng),	  &
               rlatmin,rlatmax,rlonmin,rlonmax,   &
               nxgrads(ng),nygrads(ng), 	  &
               proj_to_ll(1:len_trim(proj_to_ll)))
  !.................            
  call Matriz_interp(ng,nxgrads(ng),nygrads(ng),   &
        	nnxp(ng),nnyp(ng),  	           &
        	dep_glat(1,ng),dep_glat(2,ng),     &
        	dep_glon(1,ng),dep_glon(2,ng),     &
        	iinf,jinf,rmi,  		   &
  		proj_to_ll(1:len_trim(proj_to_ll)),'VMP') ! mean_type='VMP' ! vizinho mais proximo
  	       !proj_to_ll(1:len_trim(proj_to_ll)),mean_type(1:len_trim(mean_type)))

  !_................
  call define_lim(ng,nxgrads(ng),nygrads(ng),	       &
    	         dep_glat(1,ng),dep_glat(2,ng),        &
    	         dep_glon(1,ng),dep_glon(2,ng),        &
    	         lati(ng),latf(ng),loni(ng),lonf(ng),  &
    	         nxa(ng),nxb(ng),nya(ng),nyb(ng),      &
    	         proj_to_ll(1:len_trim(proj_to_ll)),   &
    	         nnxp(ng),nnyp(ng),r2d(:,:,1),r2d(:,:,2))
  !.................
  !go to 200
  ! calculation of the sources emission:
  !nvert = nnzp(ng) !use this for 3d sources
  nvert = 1         !use this for 2d sources
  call prep_sources(ng,nnxp(ng),nnyp(ng),nvert &
                  ,r2d(:,:,1) & ! lat
		  ,r2d(:,:,2) & ! lon
		  ,r2d(:,:,3) & ! land
                  ,ihour,iday,imon,iyear)
  200 continue
  !- collect data that will be send out
  call select_data(ng,nnxp(ng),nnyp(ng),nvert)
  
  
  !- performs the output at native and projected grids (for vizualization, if required)
  call output(ng,nvert,ihour,iday,imon,iyear,ngrids, nnxp(ng),nnyp(ng), nnzp(ng),ihtran)


  deallocate(r2d)
enddo ! enddo ngrids
end subroutine gen_regional_grid_type
!-----------------------------------------------------------------------------

subroutine convert_inventory_to_mech_name(isp,iespc,ident,source_type,inventory_type,spc_name)

!implicit none 
use chem1_list, only: chemical_mechanism
use grid_dims_out, only : pond
integer, intent(in) :: isp
integer, intent(inout) :: iespc,ident
character (len=*), intent(inout)  :: spc_name  !kml 

character (len=*) source_type,inventory_type

if (trim(chemical_mechanism) == 'RACM') then 
         
   if (pond == 0) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_racm(isp,iespc,ident,spc_name) 
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_racm(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_racm(isp,iespc,ident,spc_name)  
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_racm(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_racm(isp,iespc,ident,spc_name)		
   elseif (pond == 1) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_racm_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_racm_reac(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_racm_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_racm_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_racm_reac(isp,iespc,ident,spc_name)
   endif
   return
elseif (trim(chemical_mechanism) == 'RADM_WRF_FIM'.or.trim(chemical_mechanism) == 'RADM_FV3') then
         
   if (pond == 0) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_wrf(isp,iespc,ident,spc_name) 
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_wrf(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_wrf(isp,iespc,ident,spc_name)  
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_wrf(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_wrf(isp,iespc,ident,spc_name)		
      if(trim(inventory_type) == 'ceds' )   call convert_ceds_to_wrf(isp,iespc,ident,spc_name)
!     if(trim(inventory_type) == 'ceds' )then
!     write(6,*)'before convrt-isp,iespc,ident,spc_name=',isp,iespc,ident,spc_name
!  call convert_ceds_to_wrf(isp,iespc,ident,spc_name)
!     write(6,*)'after convrt-isp,iespc,ident,spc_name=',isp,iespc,ident,spc_name
!     call flush(6)
!     endif
   elseif (pond == 1) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_wrf_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_wrf_reac(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_wrf_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_wrf_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_wrf_reac(isp,iespc,ident,spc_name)
!     if(trim(inventory_type) == 'ceds' )   call convert_ceds_to_wrf_reac(isp,iespc,ident,spc_name)
   endif
   return
elseif  (trim(chemical_mechanism) == 'CB07') then 

   if(trim(inventory_type) == 'retro')   call convert_retro_to_CB07(isp,iespc,ident,spc_name) 
   if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_CB07(isp,iespc,ident, 'bburn',spc_name)
   if(trim(inventory_type) == 'bioge')   call convert_bioge_to_CB07(isp,iespc,ident,spc_name)
   if(trim(inventory_type) == 'megan')   call convert_megan_to_CB07(isp,iespc,ident,spc_name)
   return

elseif (trim(chemical_mechanism) == 'tracer' .or. trim(chemical_mechanism) == 'CO2' ) then 

    print*,'chemical mechanism=',trim(chemical_mechanism)
    print*,'doing nothing'
 
    return

elseif (trim(chemical_mechanism) == 'RELACS' .or. trim(chemical_mechanism) == 'RELACS_MX') then

   if (pond == 0) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_relacs(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_relacs(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_relacs(isp,iespc,ident,spc_name)      
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_relacs(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_relacs(isp,iespc,ident,spc_name)
   elseif (pond == 1) then
      if(trim(inventory_type) == 'retro')   call convert_retro_to_relacs_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'AeM'  )   call convert_AeM_to_relacs_reac(isp,iespc,ident, 'bburn',spc_name)
      if(trim(inventory_type) == 'bioge')   call convert_bioge_to_relacs_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'edgar')   call convert_edgar_to_relacs_reac(isp,iespc,ident,spc_name)
      if(trim(inventory_type) == 'megan')   call convert_megan_to_relacs_reac(isp,iespc,ident,spc_name)
   endif
   return
   
else
    
    print*,'Unknow mechanism type'
    stop 222


endif    


end subroutine convert_inventory_to_mech_name
!-----------------------------------------------------------------------------




