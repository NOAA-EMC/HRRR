!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
module volc_degassing_emissions
! ------------------------------------------------
  integer, parameter :: nvolcanoes=10000
  integer, parameter :: maxnspecies = 200 , nspecies = 1
  integer, parameter ::         &
  SO2  	    = 1 
  
  character(LEN=25),dimension(nspecies),parameter :: spc_name= &
  ! '1234567890123456789012345'
  (/                          & 
  'SO2                       '&
  /)
!---
  type degassing_vars
     real, pointer, dimension(:,:,:)  :: src
  end type degassing_vars

  type (degassing_vars), allocatable :: degassing_g(:)
!---  
  type volc_vars  
     real, pointer, dimension(:)  :: volcano
  end type volc_vars

  type (volc_vars), allocatable :: volc_prop(:)

  integer, parameter ::  nprop = 9

  integer, parameter ::         &
  LATI  	  =1  &
 ,LONG  	  =2  &
 ,ELEV  	  =3  &
 ,BEGT  	  =4  &
 ,CRAT  	  =5  &
 ,HEAT  	  =6  &
 ,plum_heigth  	  =7  &
 ,DURA  	  =8  & 
 ,ESO2  	  =9
!---
  type volc_vars_P  
     real, pointer, dimension(:,:)  :: prop
  end type volc_vars_P

  type (volc_vars_P), allocatable :: degassingP_g(:)

contains
	
  !---------------------------------------------------------------
  subroutine alloc_volcanoes(degassing_g,degassingP_g,n1,n2,n3)
    implicit none
    type (degassing_vars),dimension(nspecies)  :: degassing_g
    type (volc_vars_P),dimension(nprop)  :: degassingP_g
    integer,intent(in) :: n1,n2,n3
    integer ispc,iprop
    
    do ispc=1,nspecies
     allocate (degassing_g(ispc)%src(n1,n2,n3))
      degassing_g(ispc)%src(:,:,:)=0.
    enddo
    do iprop=1,nprop
     allocate (degassingP_g(iprop)%prop(n1,n2))
      degassingP_g(iprop)%prop(:,:)=0.
    enddo
  end subroutine alloc_volcanoes

  !---------------------------------------------------------------
  subroutine nullify_volcanoes(degassing_g,degassingP_g)
    implicit none
    type (degassing_vars),dimension(nspecies)  :: degassing_g
    type (volc_vars_P)   ,dimension(nprop)  :: degassingP_g
    integer ispc,iprop

    do ispc=1,nspecies
       if (associated(degassing_g(ispc)%src))    nullify (degassing_g(ispc)%src)
    enddo
    do iprop=1,nprop
     if (associated(degassingP_g(iprop)%prop))    nullify (degassingP_g(iprop)%prop)
    enddo
  end subroutine nullify_volcanoes

  !---------------------------------------------------------------
  subroutine alloc_volc_prop(volc_prop)
    implicit none
    type (volc_vars),dimension(nprop)  :: volc_prop
    integer i
    
    do i=1,nprop
     allocate (volc_prop(i)%volcano(nvolcanoes))
    enddo
  end subroutine alloc_volc_prop

  !---------------------------------------------------------------
  subroutine nullify_volc_prop(volc_prop)
    implicit none
    type (volc_vars),dimension(nprop)  :: volc_prop
    integer i

    do i=1,nprop
       if (associated(volc_prop(i)%volcano))   nullify (volc_prop(i)%volcano)
    enddo
  end subroutine nullify_volc_prop

  !---------------------------------------------------------------
   subroutine mem_volc_prop()
    implicit none
    integer i

    if(.not. allocated(volc_prop)) allocate(volc_prop(nprop))
    call nullify_volc_prop(volc_prop(:))      
    call alloc_volc_prop  (volc_prop(:)) 
  end subroutine mem_volc_prop

  !---------------------------------------------------------------
  subroutine get_dimension_per_file_volcano(volc_dir,iyear,imon,iday&
  			     ,nevents)
  use netcdf
  implicit none

  integer, intent(in) ::iyear,imon,iday
  character(len=*), intent(in) ::volc_dir
  integer, intent(out) ::nevents
  character (len=240) :: filename
  integer ncid
  integer current_date,neventsID
  character(len=4) ::  cyear
  

  logical :: there

  
  nevents=0

  current_date=iyear*10000+imon*100+iday
  
  write(cyear(1:4),10) iyear
  10 format (i4.4)
  
  filename=trim(volc_dir)//'/volc_so2_'//cyear//'.nc'
  inquire(file=trim(filename),exist=there)
  if (.not. there) then  
    print*,'=============================================================='
    print*,'Volcanic data not found', TRIM(filename)
    print*,'null data will set in the emission files'
    print*,'=============================================================='
    
    return
  endif
  print*,' ============= volcanoes ====================================='
  print*,' opening degassing volc data:',current_date,trim(filename)
  
  ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
  call check(NF90_OPEN(filename, NF90_NOWRITE, ncid))
  
  call check(nf90_inq_dimid(ncid, "nevents", neventsID))
  call check(nf90_inquire_dimension(ncid, neventsID, len = nevents))
  
 
  
  print*,'nevents found:', nevents
  
  
  end subroutine get_dimension_per_file_volcano
  !---------------------------------------------------------------
  subroutine get_emission_per_volcano(volc_dir,actual_number_volc,iyear,imon,iday&
  			     ,qsc,qlong,qlati,qelev,qBEGT,qcrat,qheat,qplum_heigth &
  			     ,qdura,QESO2,nevents)
  use netcdf
  implicit none
  
  !include 'netcdf.inc'
  integer, intent(in) ::iyear,imon,iday,nevents
  character(len=*), intent(in) ::volc_dir
  integer, intent(out) ::actual_number_volc
  
  real,    dimension(nvolcanoes) , intent(out):: qlong,qlati,qelev,qBEGT,qcrat,qheat,qplum_heigth &
  			     ,qdura  ,QESO2    
  real,    dimension(nvolcanoes,nspecies), intent(out) :: qsc
  character (len=240) :: filename
  integer i1,i2,iesp,ifoc,ncid,var_id
  integer,allocatable, dimension (:) :: date
  real, allocatable, dimension (:) :: cloud_column_height,so2,lon,lat,elevation

  integer nd1,nd2,current_date
  character(len=4) ::  cyear

  logical :: there

  allocate (cloud_column_height(nevents),so2(nevents),&
  	    lon(nevents),lat(nevents),elevation(nevents),date(nevents))
  cloud_column_height=0.;so2=0.;lon=0.;lat=0.;elevation=0.;date=0

  current_date=iyear*10000+imon*100+iday
  
  write(cyear(1:4),10) iyear
  10 format (i4.4)
  
  filename=trim(volc_dir)//'/volc_so2_'//cyear//'.nc'
  inquire(file=trim(filename),exist=there)
  if (.not. there) then  
    print*,'=============================================================='
    print*,'Volcanic data not found', TRIM(filename)
    print*,'null data will set in the emission files'
    print*,'=============================================================='
    
    return
  endif
  print*,' ============= volcanoes ====================================='
  print*,' opening degassing volc data:',current_date,trim(filename)
  
  ! Open the file. NF90_NOWRITE tells netCDF we want read-only access to the file.
  call check(NF90_OPEN(filename, NF90_NOWRITE, ncid))
  
  ! Get the varid of the data variable, based on its name.
  call check( nf90_inq_varid(ncid, 'date', var_id) )
  call check( nf90_get_var(ncid, var_id, date  ) )
    
  !- get the number of volcanos emitting on the current day
  do i1=1,nevents
  if(date(i1) == current_date) then
   nd1=i1;exit
  endif
  enddo
  do i1=nevents,1,-1
  if(date(i1) == current_date) then
   nd2=i1;exit
  endif
  enddo
  
  actual_number_volc=nd2-nd1+1
  if(actual_number_volc>nvolcanoes) stop 'actual_number_volc>nvolcanoes' 
  
  print*,'Amount of volcanoes found:', actual_number_volc,date(nd1),date(nd2)
  
  !- get data 
   call check( nf90_inq_varid(ncid, 'so2', var_id) )
   call check( nf90_get_var(ncid, var_id, so2  ) )

   call check( nf90_inq_varid(ncid, 'lat', var_id) )
   call check( nf90_get_var(ncid, var_id, lat  ) )

   call check( nf90_inq_varid(ncid, 'lon', var_id) )
   call check( nf90_get_var(ncid, var_id, lon  ) )

   call check( nf90_inq_varid(ncid, 'elevation', var_id) ) !meters
   call check( nf90_get_var(ncid, var_id, elevation  ) )!meters

   call check( nf90_inq_varid(ncid, 'cloud_column_height', var_id) )!meters
   call check( nf90_get_var(ncid, var_id, cloud_column_height  ) )  !meters

  !- calculates the emission by volcano
  
  do ifoc = 1,actual_number_volc
  	QLONG(ifoc)=lon      (nd1+ifoc-1)
  	QLATI(ifoc)=lat      (nd1+ifoc-1)
  	QELEV(ifoc)=elevation(nd1+ifoc-1)!meters asl
  	QESO2(ifoc)=so2      (nd1+ifoc-1)*1.e+6 !from kiloton to kg
 	Qplum_heigth(ifoc)=cloud_column_height(nd1+ifoc-1)!meters asl
  	
  !not defined
  	QBEGT(ifoc)=-999.
  	QCRAT(ifoc)=-999.
  	QHEAT(ifoc)=-999.
  	QDURA(ifoc)=86400. ! emission per day
  !not defined
       if(ifoc==1) print*,'Volcano   ---   LONG    --- LATI   ---- ELEV(m)    --- plum_heigth(m)  ---  ESO2'
       write(*,100) ifoc ,QLONG(ifoc), QLATI(ifoc), QELEV(ifoc), Qplum_heigth(ifoc), QESO2(ifoc)
       100 format(1x,i6,5F13.2)
       
  	do iesp=1,nspecies
  	 
  	     qsc(ifoc,iesp) = QESO2(ifoc) !already in kg
  	 
       enddo
       !print*,qsc(ifoc,ash)
       
  enddo
  print*,'------------- end volcanoes list -----------------'
  deallocate( date,cloud_column_height,so2,lon,lat,elevation)
  
  end subroutine get_emission_per_volcano
  !---------------------------------------------------------------
  subroutine interp_volc_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon,rlat&
  			   ,rlon,rland,nspecies_actual&
  			   ,actual_number_volc,nvolcanoes  &
  			   ,qsc,qlon,qlat,qelev,qBEGT,qcrat,qheat,qplum_heigth &
  			   ,qdura,QESO2)
  			   
  use grid_dims_out, only: grid_type
  use mem_grid, only : grid_g
  implicit none
  integer, intent(in) ::actual_number_volc,nspecies_actual,nvolcanoes
  
  integer, intent(in) :: ng,n1,n2,n3
  integer :: igbox,jgbox
  real deltax,deltay,plat,plon
  real,    dimension(nvolcanoes) , intent(in):: &
  		 qlon,qlat,qelev,qBEGT&
  		,qcrat,qheat,qplum_heigth ,qdura  ,QESO2    
  real,    dimension(nvolcanoes,nspecies_actual), intent(in) :: qsc
  real xt(n1),yt(n2),xm(n1),ym(n2)
  real, dimension(n1,n2):: rlat,rlon,rland
  real, allocatable, dimension(:,:):: grid_area
  integer,dimension(n1,n2) :: num_volc_per_gbox
  integer,dimension(actual_number_volc) :: igbox_1d,jgbox_1d
  integer i,j,iesp,ifoc
  real wlon,elon,xlon,splon       !tks
  
  num_volc_per_gbox=0

  wlon=minval(rlon)   !tks
  elon=maxval(rlon)   !tks
  
  do ifoc = 1,actual_number_volc

     if(grid_type == 'rams' .or. grid_type == 'polar') then
  	     call get_ij_rams(n1,n2,xt,yt,xm,ym,plat,plon,deltax,deltay&
  			     ,qlon(ifoc),qlat(ifoc),igbox,jgbox)

     elseif(grid_type == 'lambert'.or. grid_type == 'mercator' ) then              !tks
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

     elseif(grid_type == 'fv3') then
  	     call get_ij_fv3(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)

     elseif(grid_type == 'gg') then
  	     call get_ij_gg(n1,n2,rlat,rlon,qlon(ifoc),qlat(ifoc),igbox,jgbox)
     else
  	     stop 'unknown grid type at volcanoes degass emissions '
     endif

     
     ! print*,'fire=',ifoc,igbox,jgbox,rlat(igbox,jgbox),rlon(igbox,jgbox)
     !print *,'ycit=',rlat(igbox,jgbox)
     !print *,'xcit=',rlon(igbox,jgbox)!,area(nprg),test  !lat,lon
     !print*,'dummy=main(xcit,ycit)' 
     igbox_1d(ifoc)=igbox
     jgbox_1d(ifoc)=jgbox

     if(igbox == -1 .or. jgbox == -1 ) cycle
         !print*,'volc=',ifoc,igbox,jgbox,rlat(igbox,jgbox),rlon(igbox,jgbox)
 
    
    
    !- number of volcanos per grid box
     num_volc_per_gbox(igbox,jgbox)=num_volc_per_gbox(igbox,jgbox)+1
       
    !- here plum_heigth is the depth of the plume above the local terrain
    ! degassingP_g(plum_heigth )%prop(igbox,jgbox) = degassingP_g(plum_heigth )%prop(igbox,jgbox)+ &
    !
    ! 		 volc_prop(plum_heigth)%volcano(ifoc) - volc_prop(elev)%volcano(ifoc)

     do iesp=1,nspecies_actual
    !- total mass emitted  per gridbox
  	 degassing_g(iesp)%src(igbox,jgbox,1)	 = degassing_g(iesp)%src(igbox,jgbox,1) &
  						  + qsc(ifoc,iesp)						  
     enddo ! iesp loop
  
  enddo ! ifoc loop

  !
  !- get the mean cloud column weighted by SO2 mass for cases with more than one volcano per grid box
  do ifoc = 1,actual_number_volc
   igbox=  igbox_1d(ifoc)
   jgbox=  jgbox_1d(ifoc)
   if(igbox == -1 .or. jgbox == -1 ) cycle

    !!
    !!- here plum_heigth is the depth of the plume above the local terrain
    !! degassingP_g(plum_heigth )%prop(igbox,jgbox) = degassingP_g(plum_heigth )%prop(igbox,jgbox) +&
    !!                       (volc_prop(plum_heigth)%volcano(ifoc) - volc_prop(elev)%volcano(ifoc))*&
    !!                                              qsc(ifoc,SO2)/degassing_g(SO2)%src(igbox,jgbox,1)
    
    !- we will keep the the depth of the plume above the sea level
     degassingP_g(plum_heigth )%prop(igbox,jgbox) = degassingP_g(plum_heigth )%prop(igbox,jgbox) +&
                                                  volc_prop(plum_heigth)%volcano(ifoc)*&
                                                  qsc(ifoc,SO2)/degassing_g(SO2)%src(igbox,jgbox,1)
    !- elevation
     degassingP_g(elev )%prop(igbox,jgbox) = degassingP_g(elev )%prop(igbox,jgbox) +&
                                             volc_prop(elev)%volcano(ifoc)*&
                                             qsc(ifoc,SO2)/degassing_g(SO2)%src(igbox,jgbox,1)
    
    
  !print*,ifoc,igbox,jgbox
  !print*,degassingP_g(plum_heigth )%prop(igbox,jgbox),(volc_prop(plum_heigth)%volcano(ifoc) - &
  !volc_prop(elev)%volcano(ifoc)),qsc(ifoc,SO2)/degassing_g(SO2)%src(igbox,jgbox,1)

  enddo ! ifoc loop
  !
  !- get the mean cloud column for cases with more than one volcano per grid box
  !do i=1,n1
  !  do j=1,n2
  !   if(num_volc_per_gbox(i,j)>1) then 
  !	 degassingP_g(plum_heigth )%prop(i,j) = degassingP_g(plum_heigth )%prop(i,j) &
  !							/float(num_volc_per_gbox(i,j))
  !	 print*,'ng2=',i,j,num_volc_per_gbox(i,j),degassingP_g(plum_heigth )%prop(i,j)
  !  endif
  !  enddo
  !enddo
  print*,'Max plume height:',maxval(degassingP_g(plum_heigth )%prop(:,:))
  !don't change of place these lines below ...
  !-------------- convert to surface flux (kg/m^2)
  !- calculate the grib box area
  allocate(grid_area(n1,n2))
  if(grid_type == 'rams' .or. grid_type == 'polar') then
  	   call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)!
  
   elseif(grid_type == 'lambert' .or. grid_type == 'mercator' ) then
  	  grid_area(:,:)=1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
   
   elseif(grid_type == 'll') then
 	   call get_area_ll(grid_area,n1,n2) 
  
   elseif(grid_type == 'fim') then
  	   call get_area_fim(grid_area,n1,n2) 
  
 elseif(grid_type == 'fv3') then
           do j=1,n2
           do i=1,n1
           grid_area(i,j)=rland(i,j)
           enddo
           enddo

   elseif(grid_type == 'gg') then
         call get_area_gg(grid_area,n1,n2,rlat,rlon) 
  
   else
       stop 'unknown grid type at volc degass emissions '
   endif

   ! convert from mass [kg/day] to flux [kg/m^2/day]
   do iesp=1,nspecies_actual
        degassing_g(iesp)%src(:,:,1) = degassing_g(iesp)%src(:,:,1)/grid_area(:,:)
        print*,'volc_degass=',iesp,maxval(degassing_g(iesp)%src(:,:,1))!,maxval(grid_area)
   enddo
  !  
  deallocate(grid_area)

  end subroutine interp_volc_to_model
  !---------------------------------------------------------------

end module volc_degassing_emissions
  !---------------------------------------------------------------  
  !---------------------------------------------------------------  
  !---------------------------------------------------------------  
  subroutine process_degassing_volc(iyear,imon,iday,ihour,ng,ngrids,n1,n2,n3,rlat,rlon,rland&
                        ,deltax,deltay,xt,yt,xm,ym,plat,plon)
  use grid_dims_out, only : degass_volc_data_dir
  use volc_degassing_emissions, only :	volc_prop,nvolcanoes,nspecies,&	
  	 LATI,LONG,ELEV,BEGT,CRAT,HEAT,plum_heigth,DURA,ESO2,mem_volc_prop,&
	 get_emission_per_volcano,interp_volc_to_model, get_dimension_per_file_volcano
  implicit none
  integer, intent (in) :: iyear,imon,iday,ihour,ng,n1,n2,n3,ngrids
  real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
  real, intent (in) :: deltax,deltay
  real, intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2),plat,plon
  real, dimension(nvolcanoes,nspecies) :: qsc
  integer, save :: actual_number_volc	
  integer :: nevents
  
  if(ng == 1) then ! just for the first grid is needed

!- allocate memory for fires properties.
    call mem_volc_prop ()
    
    call get_dimension_per_file_volcano(degass_volc_data_dir&
                 ,iyear,imon,iday&
		 , nevents  )

  !-- process the source emission estimate for each valid fire count and accumulate per
  !-- model grid box
    call get_emission_per_volcano(degass_volc_data_dir&
                 , actual_number_volc,iyear,imon,iday&
		 , qsc  		    &
		 , volc_prop(LONG)%volcano  &
                 , volc_prop(LATI)%volcano  &  
		 , volc_prop(ELEV)%volcano  &  
		 , volc_prop(BEGT)%volcano  &	 
		 , volc_prop(CRAT)%volcano  &
		 , volc_prop(HEAT)%volcano  &
		 , volc_prop(plum_heigth)%volcano  &
		 , volc_prop(DURA)%volcano  &
		 , volc_prop(ESO2)%volcano  &
		 , nevents  )	 


  endif !ng==1			
  !- performs the "gridding"
   call interp_volc_to_model(ng,n1,n2,n3,xt,yt,xm,ym,deltax,deltay,plat,plon&
                 , rlat,rlon,rland  &
                 , nspecies,actual_number_volc,nvolcanoes &
                 , qsc                      &
		 , volc_prop(LONG)%volcano  &
                 , volc_prop(LATI)%volcano  &  
		 , volc_prop(ELEV)%volcano  &  
		 , volc_prop(BEGT)%volcano  &	 
		 , volc_prop(CRAT)%volcano  &
		 , volc_prop(HEAT)%volcano  &
		 , volc_prop(plum_heigth)%volcano  &
		 , volc_prop(DURA)%volcano  &
		 , volc_prop(ESO2)%volcano  )	 

   end subroutine process_degassing_volc
			
  !---------------------------------------------------------------
 
  subroutine mem_degassing_volcanoes(n1,n2,n3)
    use volc_degassing_emissions
    implicit none
    integer i,j
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(degassing_g )) allocate(degassing_g (nspecies))
    if(.not. allocated(degassingP_g)) allocate(degassingP_g(nprop))
   
    call nullify_volcanoes(degassing_g(:),degassingP_g(:))      
    call alloc_volcanoes  (degassing_g(:),degassingP_g(:),n1,n2,n3) 
    
    
  end subroutine mem_degassing_volcanoes
  !---------------------------------------------------------------

  subroutine get_degass_volc_indentity(spc_name,ident)
  !use chem1_list
  use volc_degassing_emissions, only : volcanoes_nspecies=>nspecies& 
  				      ,volcanoes_spc_name=>spc_name
  implicit none
  integer isp
  character (len=*), intent(in)  :: spc_name
  integer	   , intent(out) :: ident
  
  do isp = 1,volcanoes_nspecies
    ident=-1
    if(spc_name == volcanoes_spc_name(isp)) then
  	print*,'==>volcanoes found for ',spc_name
  	ident=isp
  	return
     endif
  enddo
  
  !print*,'chem1-list specie ',trim(spc_name), ' does not match of any one of bbbem'
  !print*,'ident=',ident
  !stop 444
  end subroutine get_degass_volc_indentity
  !---------------------------------------------------------------
  
  subroutine check(status)
     use netcdf
     integer, intent ( in) :: status
    
     !if(status /= 0) then
     if(status /= nf90_noerr) then
       print *, trim(nf90_strerror(status))
       stop 'netcdf error'
       !stop 2
     end if
     
        
  end subroutine check
  !---------------------------------------------------------------
  
  
  
  
  
  
  
  
  
