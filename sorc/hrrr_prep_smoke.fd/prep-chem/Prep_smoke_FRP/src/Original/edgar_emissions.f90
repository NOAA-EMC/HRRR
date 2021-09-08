!#######################################################################!
!  BRAMS/BAM - CPTEC/INPE - WRF-Chem - FIM-Chem emission models         ! 
!  version 1.8.3: Sep 2017                                              !
!  Coded by Saulo Freitas and Karla Longo                               !
!  brams support: brams_help@cptec.inpe.br - http://brams.cptec.inpe.br !
!#######################################################################!

module edgar_emissions
!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 14

integer, parameter ::                 &
       CO          = 1  	      &
    ,  NOX         = 2                &
    ,  CO2         = 3                &
    ,  CH4	   = 4                &
    ,  SO2         = 5  	      &
    ,  N2O         = 6		      &
    ,  SF6	   = 7		      &
    ,  NMVOC	   = 8                &
    ,  SO4         = 9                &
    ,  URBAN2	   = 10               & !PM25
    ,  URBAN3      = 11               & !PM10
    ,  BC	   = 12               & 
    ,  OC          = 13               & 
    ,  NH3         = 14                 

!---------------------------------------------------------------------------
character(LEN=20),dimension(nspecies),parameter :: spc_name= &
! '12345678901234567890'
(/                         &
  'CO                  '   &	
, 'NOX                 '   &
, 'CO2                 '   &
, 'CH4                 '   &
, 'SO2                 '   &
, 'N2O                 '   &
, 'SF6                 '   &
, 'NMVOC               '   &
, 'SO4                 '   &
, 'URBAN2              '   &
, 'URBAN3              '   &
, 'BC                  '   &
, 'OC                  '   &
, 'NH3                 '   &
/)

real, dimension(:,:,:,:),allocatable :: RAWsrc

!---------------------------------------------------------------------------

  type edgar_vars   
     real, pointer, dimension(:,:,:)  :: src
!-----------

  end type edgar_vars

  type (edgar_vars), allocatable :: edgar_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_edgar(edgar,n1,n2,n3)

    implicit none

    type (edgar_vars),dimension(nspecies)  :: edgar
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (edgar(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_edgar

  !---------------------------------------------------------------

  subroutine nullify_edgar(edgar)

    implicit none

    type (edgar_vars),dimension(nspecies)  :: edgar
    integer ispc

    do ispc=1,nspecies
       if (associated(edgar(ispc)%src))    nullify (edgar(ispc)%src)
    enddo

    return
  end subroutine nullify_edgar

end module edgar_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_edgar(n1,n2,n3)
    use edgar_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

    if(.not. allocated(edgar_g)) allocate(edgar_g(nspecies))
    !do i=1,nspecies
    ! if(associated(edgar_g(i)%src)) deallocate(edgar_g(i)%src)
    !enddo

    call nullify_edgar(edgar_g(:))      
    call alloc_edgar  (edgar_g(:),n1,n2,n3) 
  end subroutine mem_edgar

!---------------------------------------------------------------
subroutine read_edgar_antro(ihour,rhour,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat&
                           ,rlon,rland,deltax,deltay&
                           ,xt,yt,xm,ym,plat,plon)

use edgar_emissions

use grid_dims_out, only: &
grid_type,               &
edgar_data_dir,          & 
fim_data_dir,            & 
fv3_data_dir,            & 
user_data_dir,           &
use_edgar,               &
cites_mobile_urban_emissions_file

use util_geometry, only: &
T_mobileSrc,             &
loadMobileSrcMap,        &
polyToModel

use mem_grid, only :grid_g

implicit none

integer :: nlon    
integer :: nlat   
  
integer :: nmonths 
real   :: ilatn   
real   :: ilonn   


integer, intent (in) :: iyear
integer, intent (in) :: imon
integer, intent (in) :: iday
integer, intent (in) :: ng
integer, intent (in) :: n1
integer, intent (in) :: n2
integer, intent (in) :: n3
integer, intent (in) :: ngrids
integer, intent (in) :: ihour
integer, intent (in) :: rhour

real, intent (in), dimension(n1,n2) :: rlat
real, intent (in), dimension(n1,n2) :: rlon
real, intent (in), dimension(n1,n2) :: rland

real, intent (in) :: deltax
real, intent (in) :: deltay

real,intent(in) :: xt(n1)
real,intent(in) :: yt(n2)
real,intent(in) :: xm(n1) 
real,intent(in) :: ym(n2)

character(len=250) :: prefix
character(len=250) :: suffix
character(len=250) :: filename(nspecies)
character(len=180) :: dummy

real,dimension(:),allocatable :: longedgar
real,dimension(:),allocatable :: latedgar

real :: plat
real :: plon

integer :: imonx
integer :: i
integer :: j  
integer :: ispc
integer :: nc
integer :: iread
integer :: im
integer :: ilaea
integer :: jlaea
integer :: fstat
integer :: i1
integer :: i2
integer :: j1
integer :: j2
integer :: ic
integer :: jc
integer :: k
   
real :: rrlat
real :: rrlon
real :: dlon1
real :: dlon2
real :: dlat1
real :: dlat2
real :: TX(nspecies)
real :: TX4(nspecies)
real :: lat
real :: lon
real :: rdummy
!real :: src_dummy(nmonths)

real, allocatable, dimension(:,:)     :: grid_area

real, allocatable, dimension(:,:) :: NOXAlonso
real, allocatable, dimension(:,:) :: COAlonso

type(T_mobileSrc), pointer, dimension(:) :: iMobileSrc

INTEGER :: edgar_ver
REAL :: ires,jres

integer :: flag_cuba_update
integer :: grade_cuba
integer :: termos_cuba

!Cuba
real, allocatable, dimension(:,:) :: NOXMade
real, allocatable, dimension(:,:) :: COMade
real, allocatable, dimension(:,:) :: NMVOCMade

integer, parameter ::  nlon_cuba = 130, nlat_cuba= 50, n_ps_cuba=8
real :: EMISS_CUBA,EMISS_CUBA2, EMISS_CUBA_PS, EMISS_CUBA_PS2
real, parameter:: ilatn_cuba=.1,ilonn_cuba=.1
real :: area_cuba
real :: accum_emission
integer :: ipoint,jpoint, ipoint_ANT, jpoint_ANT
character*240 prefix_c,suffix_c,filename2(nspecies)
! Stu variables for HTAP-FV3 option 10/13/17
        integer :: ifim_case, ifv3_case ! Emiss normalized by area in sums, othwise interpolation done!
        integer, allocatable, dimension(:,:) :: htap_fimN

flag_cuba_update = 0
grade_cuba = 0
termos_cuba=0
   
!   print*,"not doing EDGAR = returning" ; return
   
   
    
   ! SELECT CASE(use_edgar)
   ! CASE(1)
   !    edgar_ver = 3
   ! CASE(2)   
   !    edgar_ver = 4
   ! CASE(3)   
   !    edgar_ver = 5
   ! END SELECT
    	
    !SELECT CASE(edgar_ver)	
    IF(use_edgar==1) THEN
    ! CASE(3)
        nmonths = 1  		
        prefix='edg_'
        suffix='_2000_total.txt'
        ires = 1.
        jres = 1.
	nlon=360
	nlat=180
	ilonn=1.
	ilatn=1.
	allocate(longedgar(nlon))
	allocate(latedgar(nlat))
	if( .not. allocated (RAWsrc)) THEN
		allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )
		RAWsrc = 0.0
	endif

	do j=1,nlon;  longEdgar(j)=-179.5 + (j-1)*ilonn; enddo
	do i=1,nlat;   latEdgar(i)= -89.5 + (i-1)*ilatn; enddo
	
	if(ng == 1) then	
		call edgar3(nlon,nlat,nspecies,nmonths,spc_name,RAWsrc,edgar_ver,prefix,suffix,edgar_data_dir,ires,jres)
	endif



    !CASE(4 : 5)
    ELSEIF(use_edgar == 2 .or. use_edgar == 3) then 
        nmonths = 12	
        prefix='EDGAR'
        suffix='_2005.h5'	
        ires = .1
        jres = .1
	ilonn=0.1
	ilatn=0.1
	nlon=3600
	nlat=1800
	allocate(longedgar(nlon))
	allocate(latedgar(nlat))
	if( .not. allocated (RAWsrc)) THEN
		allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )
		RAWsrc = 0.0
	endif

	do j=1,nlon;  longEdgar(j)=-179.95 + (j-1)*ilonn; enddo
	do i=1,nlat;   latEdgar(i)= -89.95 + (i-1)*ilatn; enddo	
	
	if(ng==1) then
	   call edgar4(nlon,nlat,nspecies,nmonths,spc_name,RAWsrc,prefix,suffix,edgar_data_dir)
	endif
        !do ispc=1,nspecies
	! do imonx=1,12
	!  print*,"edgar4",ispc,imonx,maxval(RAWsrc(:,:,imonx,ispc))
        ! enddo
	!enddo

	IF( use_edgar == 3) then

          nmonths = 12
          PREFIX="EDGAR-HTAP_"
          SUFFIX="_2010.h5"
          ires = .1
          jres = .1
          ilonn=0.1
          ilatn=0.1
          nlon=3600
          nlat=1800
	  !----
          ifim_case=0
          ifv3_case=0
	  select case(grid_type)
    case('fim')
           ifim_case=1
           if( .not. allocated (htap_fimN)) then
                  allocate(  htap_fimN(nlon,nlat) )
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
    case('fv3')
           ifv3_case=1
           if( .not. allocated (grid_area)) then
          	  allocate(  grid_area(n1,n2) )
           accum_emission = 0.
           do j=1,n2
           do i=1,n1
           grid_area(i,j)=rland(i,j)
   	  accum_emission = accum_emission + grid_area(i,j)
           enddo
           enddo
           endif
           write(*,*)'FV3 tile=',ng,', area sum=',accum_emission
           call flush(6)
    end select
	  !end select
          if( .not. allocated(longedgar)) allocate(longedgar(nlon))
          if( .not. allocated(latedgar )) allocate(latedgar(nlat))
          if( .not. allocated (RAWsrc)) then
          	  allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )
          	  RAWsrc = 0.0
          endif

          do j=1,nlon;  longEdgar(j)=-179.95 + (j-1)*ilonn; enddo
          do i=1,nlat;   latEdgar(i)= -89.95 + (i-1)*ilatn; enddo 

          write(6,*)'Before call to edgar_htap';CALL FLUSH(6)
          
	  if(ng==1) then
             call edgar_htap(nlon,nlat,nspecies,nmonths,spc_name,RAWsrc,prefix,suffix,edgar_data_dir)
          endif
          write(6,*)'After call to edgar_htap';CALL FLUSH(6)
	  
          do ispc=1,nspecies
	   do imonx=1,12
	     print*,"edgarhtap",ispc,imonx,maxval(RAWsrc(:,:,imonx,ispc))
           enddo
	  enddo

        ENDIF
    ELSE
    !CASE DEFAULT	
       STOP 'EDGAR version not allowed, check in: edgar_emissions.f90'
    !END SELECT  
    ENDIF
    
    
    !--- performs the interpolation to the model grid box	
    TX = 0.0
    do i=1,n1
   		do j=1,n2
  if(ifv3_case.EQ.1)then 			
        call grid_htap2fv3(i,j,rlat(i,j),rlon(i,j),n1,n2,nlon,nlat,imon,nmonths,nspecies,ilonn,ilatn, &
             latedgar,longedgar,RAWsrc,grid_area,tx)
  else
    if(ifim_case.EQ.0)then 			
			call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longedgar,latedgar, &
		  			ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)

     			call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn,   &
	      			       imon,nmonths,nspecies,RAWsrc,tx(1:nspecies))
    else
        call grid_htap2fim(i,j,n1,n2,nlon,nlat,htap_fimN,imon,nmonths,nspecies,ilonn,ilatn, &
             latedgar,RAWsrc,grid_area,tx(1:nspecies))
    endif
  endif

 			! TX is the value interpolated to the model grid box.
			!-obs: the raw data is  the monthly mean and the units are in kg /(m^2 s)
     			do ispc = 1, nspecies 
     				edgar_g(ispc)%src(i,j,1)=TX(ispc)*86400. ! convert to kg/m2/day
     			enddo
                        !
			if(use_edgar == 1 .or. use_edgar == 2) then
			   !- special section for urban aerosols 
                           edgar_g(URBAN2)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(2.95* 1.e-3) !=> kg[URBAN2]/m2/day 
                           edgar_g(URBAN3)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(8.77* 1.e-3) !=> kg[PM10]/m2/day 
                          
			endif

   		enddo ! j =1,n2
    enddo ! i=1,n1




    if(trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'NONE' .and. &
        trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'none'  ) then
   		
		print*,'----------------------------------------------------------------------'
   		print*,'Using urban emissions updating for South America'
              print*,'file=',trim(user_data_dir(1:len_trim(user_data_dir)))//'/'//trim(cites_mobile_urban_emissions_file)
		
		!------ calculate the grib box area, if necessary
          if( .not. allocated (grid_area)) then
   		allocate(grid_area(n1,n2))
          endif
		
		select case(grid_type)
		case('rams')
			call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
		case('polar')
			call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
		case('ll')
			call get_area_ll(grid_area,n1,n2)
		case('lambert')
			grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
		case('mercator')
			grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
		case('fim')
			call get_area_fim(grid_area,n1,n2) 
		case('gg')
			call get_area_gg(grid_area,n1,n2,rlat,rlon) 
		case default
			stop 'grid_type wrong'	
		end select
	
	 allocate(NOXAlonso(n1,n2))
   	 allocate(COAlonso(n1,n2))
   
   	 NOXAlonso = 0.0
   	 COAlonso  = 0.0
   
      call loadMobileSrcMap(trim(user_data_dir)//'/'//trim(cites_mobile_urban_emissions_file), iMobileSrc)
	
	 call polyToModel(iMobileSrc,ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat,grid_type, COAlonso, NOXAlonso)
	
	 do j = 1, n2
   		do i = 1, n1
		
			COAlonso(i,j)  = ((COAlonso(i,j)*1e7)/365.) / grid_area(i,j)  ! kg/m2/dia
			NOXAlonso(i,j) = ((NOXAlonso(i,j)*1e7)/365.) / grid_area(i,j) ! kg/m2/dia
		
			if(NOXAlonso(i,j) .gt. 0)then
				do ispc=1,nspecies
					if (ispc.ne.NOX .and. ispc.ne.CO .and. edgar_g(NOX)%src(i,j,1) .ne. 0.) then
			        		edgar_g(ispc)%src(i,j,1) = (  NOXAlonso(i,j) / edgar_g(NOX)%src(i,j,1) )  * edgar_g(ispc)%src(i,j,1) ! nox ratio 
					end if 
				end do

				edgar_g(NOX)%src(i,j,1)  = NOXAlonso(i,j)
				edgar_g(CO)%src(i,j,1)   = COAlonso(i,j)
				!
				!srf- special section for urban aerosols 
                                if(use_edgar == 1 .or. use_edgar == 2) then
				  edgar_g(URBAN2)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(2.95* 1.e-3) !=> kg[PM25]/m2/day 
                                  edgar_g(URBAN3)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(8.77* 1.e-3) !=> kg[PM10]/m2/day 
                                endif

			endif
	
		enddo
   	 enddo
	
	 deallocate(NOXAlonso, COAlonso)
   	 deallocate(grid_area)

     endif ! if(trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'NONE'
	
     !- deallocate memory that we do not need anymore    
     if (ng==ngrids) deallocate (RAWsrc)
 
     if(grid_type == 'rams' .or. grid_type == 'polar') then ! only for 'rams' until rland is also defined for others grids 
    		do ispc=1,nspecies 
       			call apply_land_restriction(n1,n2,rland,edgar_g(ispc)%src(:,:,1))
    		enddo  
     endif
	
     !--------------------------------------Cuba Inventories--------------------------------

     if(flag_cuba_update == 1) then

		!------ calculate the grib box area, if necessary

         if(.NOT. allocated(grid_area)) allocate(grid_area(n1,n2))

		
		select case(grid_type)
		case('rams')
			call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
		case('polar')
			call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
		case('ll')
			call get_area_ll(grid_area,n1,n2)
		case('lambert')
			grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
		case('mercator')
			grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
		case('fim')
			call get_area_fim(grid_area,n1,n2) 
		case('gg')
			call get_area_gg(grid_area,n1,n2,rlat,rlon) 
		case default
			stop 'grid_type wrong'	
		end select
	
          if (grade_cuba==0) then


             !- name of dataset
             prefix_c='emis_cuba_'
             suffix_c='.csv'
           
             do ispc=1,nspecies

        	 if(trim(spc_name(ispc))=='URBAN2' .or. trim(spc_name(ispc))=='URBAN3') cycle

        	 nc=len_trim(spc_name(ispc))
        	 filename2(ispc)=trim(user_data_dir)//'/'//trim(prefix_c)//spc_name(ispc)(1:nc)//suffix_c
        	 print *,'opening   ', trim(filename2(ispc)),' - specie= ',trim(spc_name(ispc))
        	 open(12,file=trim(filename2(ispc)),status='old')

              !read  header

        	 do iread=1,nlon_cuba*nlat_cuba !maximum number of points for nlon,nlat
        	     EMISS_CUBA=0
        	     EMISS_CUBA2=0
        	    read(12,*,end=12) lon,lat,EMISS_CUBA,area_cuba
        	     rrlon = lon
        	     rrlat = lat
        	     call update_emissions_by_point(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
        				 ,rrlon,rrlat,ipoint,jpoint,grid_type)
        		    edgar_g(ispc)%src(ipoint,jpoint,1) = 0.
        		 if(ipoint /= -999 .and. jpoint /= -999) then
        		    EMISS_CUBA2 = ((EMISS_CUBA*1e6)/365)/(grid_area(ipoint,jpoint))
        		    edgar_g(ispc)%src(ipoint,jpoint,1) = EMISS_CUBA2	 
        		 endif
        	 enddo !of iread

        	 12 close(12)
             enddo
          else if (grade_cuba == 1) then
		
   	      allocate(NOXMade(n1,n2))
   	      allocate(COMade(n1,n2))
   	      allocate(NMVOCMade(n1,n2))
   	 
   	      NOXMade = 0.
   	      COMade  = 0.
   	      NMVOCMade  = 0.
   	 
   	      call loadMobileSrcMap(trim(user_data_dir)//'/Cuba_prov_Emissions.asc', iMobileSrc, 1 )
   	      
   	      call polyToModel(iMobileSrc,ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat,grid_type, COMade, NOXMade, NMVOCMade)
   	      
   	      do j = 1, n2
   		      do i = 1, n1
   		      
   			      COMade(i,j)  = ((COMade(i,j)*1e7)/365.) / grid_area(i,j)  ! kg/m2/dia
   			      NOXMade(i,j) = ((NOXMade(i,j)*1e7)/365.) / grid_area(i,j) ! kg/m2/dia
   			      NMVOCMade(i,j) = ((NMVOCMade(i,j)*1e7)/365.) / grid_area(i,j) ! kg/m2/dia
   			      if(COMade(i,j) .gt. 0.)then
   				      edgar_g(NOX)%src(i,j,1)  = NOXMade(i,j)
   				      edgar_g(CO)%src(i,j,1)   = COMade(i,j)
   				      edgar_g(NMVOC)%src(i,j,1)   = NMVOCMade(i,j)
   			      end if
   				      !
   				      !srf- special section for urban aerosols 
				      if(use_edgar == 1 .or. use_edgar == 2) then
   				        edgar_g(URBAN2)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
   							 *(2.95* 1.e-3) !=> kg[PM25]/m2/day 
   				        edgar_g(URBAN3)%src(i,j,1)=edgar_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
   							 *(8.77* 1.e-3) !=> kg[PM10]/m2/day 
   				      endif
   	      
   		      end do
   	      end do
	      deallocate(NOXMade, COMade, NMVOCMade)	
          end if

   	   if (termos_cuba == 1) then
   	   !- name of dataset
   	   prefix_c='emis_cuba_ps_'
   	   suffix_c='.csv'
   	 

   	   do ispc=1,nspecies
   	   
   		 
   	 
   	     if(trim(spc_name(ispc))=='URBAN2' .or. trim(spc_name(ispc))=='URBAN3') cycle
   	 
   	     nc=len_trim(spc_name(ispc))
   	     filename2(ispc)=trim(user_data_dir)//'/'//trim(prefix_c)//spc_name(ispc)(1:nc)//suffix_c
   		 
   	     print *,'opening	', trim(filename2(ispc)),' - specie= ',trim(spc_name(ispc))
   	     open(13,file=trim(filename2(ispc)),status='old')
   	 
   	      !read  header
   	 
   	     do iread=1, n_ps_cuba !maximum number of points for nlon,nlat
   		     
   		 read(13,*,end=13) lon,lat,EMISS_CUBA
   		     rrlon = lon
   		     rrlat = lat
   		     call update_emissions_by_point(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
   					 ,rrlon,rrlat,ipoint,jpoint,grid_type)
   		     if(ipoint /= -999 .and. jpoint /= -999) then
   		      accum_emission = 0.
   		      if(ipoint_ANT==ipoint .and. jpoint_ANT==jpoint) accum_emission=edgar_g(ispc)%src(ipoint,jpoint,1)
   		      EMISS_CUBA2 = ((EMISS_CUBA*1e6)/365)/(grid_area(ipoint,jpoint))
   		      edgar_g(ispc)%src(ipoint,jpoint,1) = EMISS_CUBA2+accum_emission
            
                      ipoint_ANT=ipoint
                      jpoint_ANT=jpoint
                      endif
             enddo !of iread
              13 close(13)
           enddo
    end if

   deallocate(grid_area)
   deallocate(longedgar)
   deallocate(latedgar)

  endif

end subroutine read_edgar_antro
!------------------------------------------------------------------------------
subroutine edgar3(nlon,nlat,nspecies,nmonths,spc_name,RAWsrc,edgar_ver,prefix,suffix,edgar_data_dir,ires,jres)
implicit none

 integer, intent(in)::nlon,nlat,nspecies,nmonths,edgar_ver
 real, intent(in)::ires,jres
 character(len=*),intent(in)::spc_name(nspecies),prefix,suffix,edgar_data_dir
 real, intent(out)::RAWsrc(nlon,nlat,nmonths,nspecies)
 
 real::lat,lon,src_dummy(nmonths)
 character(len=240)::filename
 integer :: ispc,ilaea,jlaea,iread,im, fstat
 character *10 dummy




	do ispc=1,nspecies

		filename=trim(edgar_data_dir)//'/'//trim(prefix)//trim(spc_name(ispc))//suffix
		print *,'opening EDGAR emissions', trim(filename),' - specie= ',trim(spc_name(ispc))
		print*,'----------------------------------------------------------------------'
		if(trim(spc_name(ispc))=='URBAN2' .or. trim(spc_name(ispc))=='URBAN3' .or. &
		trim(spc_name(ispc))=='NH3') cycle
		
		open(11,file=trim(filename),status='old')
			!read  header
			read(11,*) dummy
			DO WHILE (dummy /= '#Values')
				read(11,*) dummy 
				!print*,dummy     
			enddo
       
		
			open(11,file=trim(filename),status='old')			
			DO iread=1,nlon*nlat !maximum number of points for nlon,nlat    
				read(11,*,iostat=fstat) lon,lat,(src_dummy(im),im=1,nmonths)
				if(fstat .ne. 0)exit
					!print*,lon,lat,(src_dummy(im),im=1,nmonths)
					!stop 33
					ilaea = nint(((lon - (-180.))/ires)+ 1 ) ! 2. = 1/resolution
					jlaea = nint(((lat - ( -90.))/jres)+ 1 )  ! 0.25 = 0.5*resolution
					ilaea= max(ilaea,1)

					if(ilaea < 1 .or. ilaea > nlon) stop 33
				
					if(jlaea < 1 .or. jlaea > nlat) stop 35
     				
					RAWsrc(ilaea,jlaea,:,ispc)=src_dummy(:)	
				
				
			END DO
			close(11)

		

	enddo

end subroutine edgar3
!------------------------------------------------------------------------------------
!> @author
!> GMAI
!> @version
!> 1.0
!> @date
!> 05/04/2012
!> @brief
!> DESCRIPTION
!> Reading function of the hd5 input file from EDGAR version 4.1 with attributes and &
!> organized by emission sources (stationary sources, mobile sources, aviation sources) to be read by the program PREP-CHEM-SRC.

!----------------------------------------------------------------------------------
subroutine edgar4(nlon,nlat,nspecies,nmonths,spc_name,RAWsrc,prefix,suffix,edgar_data_dir)
use hdf5
implicit none
 !variaveis de entrada e saída
 integer, intent(in):: nlon,nlat,nspecies,nmonths
 character(len=*), intent(in)::spc_name(nspecies)
 real,intent(out):: RAWsrc(nlon,nlat,nmonths,nspecies)
 character(len=*), intent(in)::prefix,suffix,edgar_data_dir
 
!variaveis do hdf5
 character(len=50) ::dsetname
 integer(hid_t):: file_id, dset_id, group_id, dtype_id
 integer(hsize_t) :: data_dims(2) 
 character(len=240)::filenameed
 integer :: error, printflags,ispc,ifontes
 real,dimension(:,:),allocatable::soma
 real,dimension(:,:,:),allocatable::somat
 integer(hid_t)::dcpl
 integer,dimension(1:1)::cd_values
 integer(size_t)::nelmts
 integer:: flags, filter_id, filter_info_both,filter_info
 integer, parameter:: fontes=3
 integer(size_t),parameter:: MaxChrlen=80
 character(len=MaxChrlen):: name
 integer, parameter :: STATIONARY_SOURCES = 1, &
	MOBILE_SOURCES = 2, &
	AVIATION_SOURCES = 3
 character(LEN=19),dimension(fontes), parameter :: &
	groupname=(/ &
	'STATIONARY_SOURCES', &
	'MOBILE_SOURCES    ', &
	'AVIATION_SOURCES  '/)
 integer :: imonx

  allocate(soma(nlon,nlat))
  allocate(somat(nlon,nlat,nmonths))

  !variavel responsavel por mostrar mensagens de erro do hdf5, valor 0 implica que sua funcao esta desativada
  printflags=0
	
  filenameed=trim(edgar_data_dir)//'/'//trim(prefix)//suffix
  print*, trim(filenameed)
  !inicializa interface do hdf5
  call h5open_f(error)
  print*, 'After h5open_f call'
    if(error==0)then
      !nao exibe mensagem de erro do hdf5 caso ocorra um
      call h5eset_auto_f(printflags, error)
      !abre arquivo com extensao hdf5
      call h5fopen_f(filenameed, H5F_ACC_RDONLY_F, file_id, error)
        if(error==0)then
          !nao exibe mensagem de erro do hdf5 caso ocorra um
          call h5eset_auto_f(printflags, error)
          !loop referentes as especies
          do ispc=1,nspecies
            soma=0.
            somat=0.
            dsetname=trim(spc_name(ispc))
            
            if(trim(spc_name(ispc))=='URBAN2' .or. &
               trim(spc_name(ispc))=='URBAN3' .or. trim(spc_name(ispc))=='SO4') cycle
            
            do ifontes=1, fontes
             
              print*,'----------------------------------------------------------------------'
	      print *,'opening EDGAR emissions', trim(filenameed),' - group=',trim(groupname(ifontes)),&
                ' - specie= ',trim(spc_name(ispc))
             
	      !abre o grupo fontes fixas dentro arquivo hdf5
              call h5gopen_f(file_id, groupname(ifontes), group_id, error)
                if(error==0) then
                  !nao exibe mensagem de erro do hdf5 caso ocorra um
                  call h5eset_auto_f(printflags, error)
                  !abre o dsetname(dsetname=especie)
                  call h5dopen_f(group_id, dsetname, dset_id, error)
                    if(error==0) then
                    !nao exibe mensagem de erro do hdf5 caso ocorra um
                      call h5eset_auto_f(printflags, error)
                      !captura o valor do dset
                      call h5dget_create_plist_f(dset_id,dcpl,error)
                        nelmts = 1
                        call H5Pget_filter_f(dcpl, 0, flags, nelmts,&
                        cd_values, MaxChrLen, name, filter_id, error)
                        if(error==0) then
                          !nao exibe mensagem de erro do hdf5 caso ocorra um
                          call h5eset_auto_f(printflags, error)
                          !leitura da soma de fontes fixas
                          call h5dread_f(dset_id, H5T_IEEE_F32LE, soma,&
                          data_dims, error)
                            if(error==0) then
                              !nao exibe mensagem de erro do hdf5 caso ocorra um
                              call h5eset_auto_f(printflags, error)
                
                              print *,dsetname,' ',maxval (soma),minval (soma)
                            
                              somat(:,:,1)=somat(:,:,1)+soma(:,:)
                              
                            endif
                        endif
                      !fecha o dcpl
                      call h5pclose_f(dcpl,error)
                    endif
                  !fecha o dset
                  call h5dclose_f(dset_id,error)
                endif
      
              !fecha o grupo fontes fixas
              call h5gclose_f(group_id, error)
            enddo
	    do imonx = 1,nmonths
              RAWsrc(:,:,imonx,ispc)=somat(:,:,1)
	    enddo
          enddo
          
          deallocate(soma)
          deallocate(somat)
        else
          print*,'EDGAR4 file not found: ', trim(filenameed)
          print*,'check the path of this HDF5 file'
          stop "EDGAR4 routine 1"
        endif
      call h5fclose_f(file_id, error)
    else 
      print*,'HDF5 library files not found'
      stop "EDGAR4 routine 2"
    endif
    !fecha a inicialização do hdf5
  call h5close_f(error)



end subroutine edgar4

!---------------------------------------------------------------
subroutine  edgar_htap(nx,ny,nspecies,nmonths,spc_name,RAWsrc,prefix,suffix,edgar_data_dir)


 use hdf5
 implicit none
 integer, intent(in) :: nx,ny,nmonths,nspecies
 real,intent(inout):: RAWsrc(nx,ny,nmonths,nspecies)
 character(len=*), intent(in)::prefix,suffix,edgar_data_dir
 character(len=*), intent(in)::spc_name(nspecies)

 !integer, parameter :: NX=3600
 !integer, parameter :: NY=1800
 !integer, parameter :: NZ=1
 !integer, parameter :: NT=12
 integer, parameter :: NSETOR=5
 !integer, parameter :: NSPECIES=9
 integer :: i
 integer :: j
 !integer :: imonth
 integer :: isetor
 integer :: ispc
 real, dimension(:,:,:), allocatable :: src_dummy
 character(len=11), dimension(NSETOR) :: setor=(/&
	'AGRICULTURE',&
	'ENERGY     ',&
	'INDUSTRY   ',&
	'RESIDENTIAL',&
	'TRANSPORT  '/) 
! character(len=5), dimension(NSPECIES) :: specie=(/&
!	'BC',&
!	'CO',&
!	'NH3',&
!	'NMVOC',&
!	'NOx',&
!	'OC',&
!	'PM10',&
!	'PM2.5',&
!	'SO2'/)
! character(len=25), parameter :: PREFIX="EDGAR-HTAP_"
! character(len=25), parameter :: SUFIX="_2010.h5"
! character(len=255), parameter :: DIR="/scratchin/grupos/catt-brams/home/valter.oliveira/PROJETOS/MATRIX/src/dados/"


 character(len=255) :: filename
 logical :: file_exists
 !variaveis do hdf5
 integer(HID_T) :: file_id
 integer(HID_T) :: dset_id 
 integer(HSIZE_T) :: data_dims(4)
 integer :: hdferr
 character(len=25) :: dsetname


   !- if a specie is included in this HTAP inventory, it will be written over the previously
   !- quantity defined by EDGAR 4 inventory
   do ispc=1, NSPECIES !loop de especies
   
     !- species not included in this inventory
     if(trim(spc_name(ispc))=='CO2' .or. &
     	trim(spc_name(ispc))=='CH4' .or. &
     	trim(spc_name(ispc))=='SO4' .or. &
     	trim(spc_name(ispc))=='N2O' .or. &
     	trim(spc_name(ispc))=='SF6'  )  then
        cycle
     else
     	RAWsrc(:,:,:,ispc) = 0.0
     endif   
   enddo   
   !
 

   allocate(src_dummy(NX,NY,nmonths))
   call h5open_f(hdferr) !inicializa o hdf5

   do isetor=1, NSETOR !loop de setores
      
      filename=trim(edgar_data_dir)//"/"//trim(PREFIX)//trim(setor(isetor))//trim(suffix)
      !print*,"filename=",filename;pause 333
      inquire(file=trim(filename), exist=file_exists)
      if(file_exists) then
         call h5fopen_f(trim(filename),H5F_ACC_RDONLY_F, file_id, hdferr)  !abre o arquivo
            
            do ispc=1, NSPECIES !loop de especies
               !dsetname=trim(specie(ispc))
               dsetname=trim(spc_name(ispc))
               
	       !- species not included in this inventory => cycle
	       if(trim(spc_name(ispc))=='CO2' .or. &
	          trim(spc_name(ispc))=='CH4' .or. &
	          trim(spc_name(ispc))=='SO4' .or. &
	          trim(spc_name(ispc))=='N2O' .or. &
	          trim(spc_name(ispc))=='SF6'  ) cycle
	       
	       !- species included in this inventory with different name
	       if(trim(spc_name(ispc))=='URBAN2')dsetname="PM2.5"
	       if(trim(spc_name(ispc))=='URBAN3')dsetname="PM10"
               
	       !- agriculture inventory only has inventory for NH3 
	       if(trim(setor(isetor)) .eq. 'AGRICULTURE' .and. trim(spc_name(ispc)) .ne. 'NH3') cycle
                
               call h5dopen_f(file_id, trim(dsetname), dset_id, hdferr)  !abre o dataset

               call h5dread_f(dset_id, H5T_NATIVE_REAL, src_dummy, data_dims, hdferr)  !faz a leitura do dataset
               
               do i=1,nmonths
                  print*,trim(setor(isetor)),"   ",trim(dsetname),i, " MAX ",maxval(src_dummy(:,:,i))
!                 print*,trim(setor(isetor)),"   ",trim(dsetname),i, " MIN ",minval(src_dummy(:,:,i))                    
                  !-accumulate over sectors
		  RAWsrc(:,:,i,ispc) = RAWsrc(:,:,i,ispc) + src_dummy(:,:,i)
	       enddo

               call h5dclose_f(dset_id,hdferr)  !fecha o dataset
                
            enddo
         call h5fclose_f(file_id, hdferr)  !fecha o arquivo

      else
         print*, "EDGAR_HTAP file not found: ",trim(filename)
	 stop 
      endif
   enddo
 call h5close_f(hdferr)
 deallocate(src_dummy)
end subroutine  edgar_htap
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_edgar_indentity(spc_name,ident)
!use chem1_list
use edgar_emissions, only :  edgar_nspecies=>nspecies&
                            ,edgar_spc_name=>spc_name&
, edgar_CO     => CO	  & ! 1
, edgar_NOX    => NOX	  & ! 2  
, edgar_CO2    => CO2	  &   
, edgar_CH4    => CH4	  &  
, edgar_SO2    => SO2	  & 
, edgar_N2O    => N2O	  &
, edgar_SF6    => SF6	  &
, edgar_NMVOC  => NMVOC   &
, edgar_SO4    => SO4     &
, edgar_URBAN2 => URBAN2  &
, edgar_URBAN3 => URBAN3  &
, edgar_BC     => BC      &
, edgar_OC     => OC      &
, edgar_NH3    => NH3       ! 14


implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,edgar_nspecies
  ident=-1
  if(spc_name == edgar_spc_name(isp)) then
      print*,'==>edgar found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match if any one of edgar'
!stop 444
end subroutine get_edgar_indentity
!---------------------------------------------------------------
        subroutine grid_htap2fim(i,j,n1,n2,nlon,nlat,htap_fimN,imon,nmonths,nspecies,ilonn,ilatn, &
             latedgar,RAWsrc,grid_area,tx)
implicit none
integer, dimension(nlon,nlat) :: htap_fimN
integer nlat,nlon,i,j,imon,nmonths,nspecies,n1,n2
real, dimension(nlon,nlat,nmonths,nspecies) :: RAWsrc
real, dimension(n1,n2) :: grid_area
real, dimension(nlat) :: latedgar
real tx(nspecies)
real ilonn,ilatn
!-local var
integer ncount,ii,jj
real dlonr,dlatr,d2r,rearth
   rearth=6371.67e3  ! earth radius in meters, need in same units as variable grid_area
   d2r = 4.0*ATAN(1.0)/180.0
   dlonr=d2r*(ilonn)*rearth
   dlatr=d2r*(ilatn)*rearth
ncount = 0
TX(:)  = 0.
do jj = 1,nlat
   do ii = 1,nlon
    if(htap_fimN(ii,jj).eq.i)then
     	    ncount = ncount + 1
! Convert from kg/m(2)/sec to kg/sec and sum over FIM grid = i
     	    TX(:) = TX(:) + cos(latedgar(jj)*d2r)*dlonr*dlatr*RAWsrc(ii,jj,imon,:)  
    endif
   enddo
enddo
if(ncount.eq.0)STOP'NCOUNT=0 in grid_htap2fim'
! Convert back to kg/m(2)/sec - now normalized over FIM grid
TX(:) = TX(:) / grid_area(i,j)
end subroutine grid_htap2fim
!---------------------------------------------------------------
        subroutine grid_htap2fv3(i,j,xlat,xlon,n1,n2,nlon,nlat,imon, &
     nmonths,nspecies,ilonn,ilatn,latedgar,lonedgar,RAWsrc,grid_area,tx)
implicit none
integer nlat,nlon,i,j,imon,nmonths,nspecies,n1,n2
real, dimension(nlon,nlat,nmonths,nspecies) :: RAWsrc
real, dimension(n1,n2) :: grid_area
real, intent(in) :: latedgar(nlat)
real, intent(in) :: lonedgar(nlon)
real, intent(in) :: xlat,xlon
real, intent(out) :: tx(nspecies)
real ilonn,ilatn
real*8, dimension(6) :: XPLY,YPLY
real*8  :: MINDST,XLOC,YLOC,X1P,AREA,DET,lon_mn,lon_mx
!-local var
integer ncount,ii,jj,imin,imax,jmin,jmax,k
real, allocatable, dimension(:,:) :: fv3lat,fv3lon
real dlonr,dlatr,d2r,rearth
   rearth=6371.67e3  ! earth radius in meters, need in same units as variable grid_area
   d2r = 4.0*ATAN(1.0)/180.0
   dlonr=d2r*(ilonn)*rearth
   dlatr=d2r*(ilatn)*rearth
ncount = 0
TX(:)  = 0.
  if( .not. allocated (fv3lat))then
  allocate(fv3lat(n1+1,n2+1))
  allocate(fv3lon(n1+1,n2+1))
 endif
  call fv3_ltln_buff(n1+1,n2+1,fv3lat,fv3lon,1)

!         do j=1,nlon;  longEdgar(j)=-179.95 + (j-1)*ilonn; enddo
!         do i=1,nlat;   latEdgar(i)= -89.95 + (i-1)*ilatn; enddo 
   xply(1)=fv3lon(i,j)
   xply(2)=fv3lon(i,j+1)
   xply(3)=fv3lon(i+1,j+1)
   xply(4)=fv3lon(i+1,j)
   xply(5)=xply(1)
   xply(6)=xply(2)
   yply(1)=fv3lat(i,j)
   yply(2)=fv3lat(i,j+1)
   yply(3)=fv3lat(i+1,j+1)
   yply(4)=fv3lat(i+1,j)
   yply(5)=yply(1)
   yply(6)=yply(2)
   imin=nlon+1
   imax=0
   jmin=nlat+1
   jmax=0
   lon_mn=1.D33
   lon_mx=-1.D33
   do ii=1,4
!  imin=min(imin,int((xply(ii)+180.)/ilonn+1))
!  jmin=min(jmin,int((yply(ii)+90.)/ilatn+1))
   imin=max(1,min(imin,int((xply(ii)+180.)/ilonn)))
   jmin=max(1,min(jmin,int((yply(ii)+90.)/ilatn)))
   imax=min(nlon,max(imax,int((xply(ii)+180.)/ilonn+1)))
   jmax=min(nlat,max(jmax,int((yply(ii)+90.)/ilatn+1)))
   lon_mn=dmin1(lon_mn,xply(ii))
   lon_mx=dmax1(lon_mx,xply(ii))
   enddo
! Kick out grids that cross date line - WARNING!!! no dateline emissions
   if(abs(lon_mx-lon_mn).gt.179.)then
   write(*,*)'dateline crossing - No data fill'
     WRITE(*,*)'imin,imax,jmin,jmax=',imin,imax,jmin,jmax
     WRITE(*,'(A,1P4D14.6,A,4D14.6)')'xply=',xply(1),xply(2),xply(3),xply(4), &
     '  yply=',yply(1),yply(2),yply(3),yply(4)
     WRITE(*,'(A,2I6,2F13.5,I6)')'DL,i,j,lon,lat,ncount=',i,j,XLON,XLAT,ncount
     call flush(6)
     return
     endif
   do jj = jmin,jmax
   do ii = imin,imax
   xloc=lonedgar(ii)
   yloc=latedgar(jj)
      MINDST=1.D33
      DO K=2,5
      X1P=DET( xply(K-1 ),xply( K),xply( K+1), yply( K-1), yply( K), yply(K+1))
      AREA=DET( xply( K),xply( K+1),XLOC, yply( K), yply(K+1), YLOC)
      MINDST=DMIN1(MINDST,AREA*X1P)
      ENDDO
!  if(i.eq.20.and.j.eq.93)then
!    WRITE(*,'(A,2I6,2F13.5,1Pd11.3,0P2I5,2F13.5)')'IN,i,j,lon,lat,mindst=', &
!    i,j,XLON,XLAT,MINDST,ii,jj,xloc,yloc
!    call flush(6)
!    endif
          IF(MINDST.GE.0.)THEN
     ncount = ncount + 1
! Convert from kg/m(2)/sec to kg/sec and sum over FIM grid = i
     	    TX(:) = TX(:) + cos(latedgar(jj)*d2r)*dlonr*dlatr*RAWsrc(ii,jj,imon,:)  
    endif
   enddo
enddo
!    WRITE(*,'(A,2I6,2F13.5,I6)')'IN,i,j,lon,lat,ncount=',i,j,XLON,XLAT,ncount
!    call flush(6)
if(ncount.eq.0)then
   write(*,*)'NCOUNT=0 - No data fill'
     WRITE(*,*)'imin,imax,jmin,jmax=',imin,imax,jmin,jmax
     WRITE(*,'(A,1P4D14.6,A,4D14.6)')'xply=',xply(1),xply(2),xply(3),xply(4), &
     '  yply=',yply(1),yply(2),yply(3),yply(4)
     WRITE(*,'(A,2I6,2F13.5,I6)')'OUT,i,j,lon,lat,ncount=',i,j,XLON,XLAT,ncount
     call flush(6)
!  STOP'NCOUNT=0 in grid_htap2fv3'
     endif
! Convert back to kg/m(2)/sec - now normalized over FIM grid
TX(:) = TX(:) / grid_area(i,j)
end subroutine grid_htap2fv3
!---------------------------------------------------------------
