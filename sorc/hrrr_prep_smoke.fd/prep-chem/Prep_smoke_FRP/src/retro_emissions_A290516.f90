!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module retro_emissions

!---------------------------------------------------------------------------
integer, parameter :: maxnspecies = 200 , nspecies = 26

integer, parameter ::         &
 ACIDS			  =1  &
,ALCOHOLS		  =2  &
,BENZENE		  =3  &
,C2H2			  =4  &
,C2H4			  =5  &
,C2H6			  =6  &
,C3H6			  =7  &
,C3H8			  =8  &
,C4H10  		  =9  &
,C5H12  		  =10 &
,C6H14_PLUS_HIGHER_ALKANES=11 &
,CHLORINATED_HYDROCARBONS =12 &
,CO			  =13 &
,ESTERS 		  =14 &
,ETHERS 		  =15 &
,KETONES		  =16 &
,METHANAL		  =17 &
,NOX			  =18 &
,OTHER_ALKANALS 	  =19 &
,OTHER_AROMATICS	  =20 &
,OTHER_VOC		  =21 &
,TOLUENE		  =22 &
,TRIMETHYLBENZENES	  =23 &
,XYLENE 		  =24 &
!- aerosol section     
,URBAN2			=25 &  ! PM25
,URBAN3			=26    ! PM10


!---------------------------------------------------------------------------
character(LEN=25),dimension(nspecies),parameter :: spc_name= &
! '1234567890123456789012345'
(/                           & 
  'ACIDS                    '&
, 'ALCOHOLS                 '&
, 'BENZENE                  '&
, 'C2H2                     '&
, 'C2H4                     '&
, 'C2H6                     '&
, 'C3H6                     '&
, 'C3H8                     '&
, 'C4H10                    '&
, 'C5H12                    '&
, 'C6H14_PLUS_HIGHER_ALKANES'&
, 'CHLORINATED_HYDROCARBONS '&
, 'CO                       '&
, 'ESTERS                   '&
, 'ETHERS                   '&
, 'KETONES                  '&
, 'METHANAL                 '&
, 'NOX                      '&
, 'OTHER_ALKANALS           '&
, 'OTHER_AROMATICS          '&
, 'OTHER_VOC                '&
, 'TOLUENE                  '&
, 'TRIMETHYLBENZENES        '&
, 'XYLENE                   '&
, 'URBAN2                   '&
, 'URBAN3                   '&
/)
!---------------------------------------------------------------------------
real, dimension(:,:,:,:),allocatable :: RAWsrc
  type retro_vars   
     real, pointer, dimension(:,:,:)  :: src

  end type retro_vars

  type (retro_vars), allocatable :: retro_g(:)

contains
  !---------------------------------------------------------------

  subroutine alloc_retro(retro,n1,n2,n3)

    implicit none

    type (retro_vars),dimension(nspecies)  :: retro
    integer,intent(in) :: n1,n2,n3
    integer ispc
    
    do ispc=1,nspecies
     allocate (retro(ispc)%src(n1,n2,n3))
    enddo

    return
  end subroutine alloc_retro

  !---------------------------------------------------------------

  subroutine nullify_retro(retro)

    implicit none

    type (retro_vars),dimension(nspecies)  :: retro
    integer ispc

    do ispc=1,nspecies
       if (associated(retro(ispc)%src))    nullify (retro(ispc)%src)
    enddo

    return
  end subroutine nullify_retro

end module retro_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
  subroutine mem_retro(n1,n2,n3)
    use retro_emissions
    implicit none
    integer i
    integer, intent(in) :: n1,n2,n3

     if(.not. allocated(retro_g)) allocate(retro_g(nspecies))
!   
    !do i=1,nspecies
    ! if(associated(retro_g(i)%src)) deallocate(retro_g(i)%src)
    !enddo
    call nullify_retro(retro_g(:))      
    call alloc_retro  (retro_g(:),n1,n2,n3) 
  end subroutine mem_retro

!---------------------------------------------------------------
!---------------------------------------------------------------

subroutine read_retro_antro(ihour,rhour,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)

use retro_emissions

use grid_dims_out, only: &
grid_type,               &
retro_data_dir,          & 
user_data_dir

use util_geometry, only: &
T_mobileSrc,             &
loadMobileSrcMap,        &
polyToModel

use mem_grid, only :grid_g, stdlat2

implicit none

integer, parameter :: nlon    = 720
integer, parameter :: nlat    = 360
integer, parameter :: nmonths = 12 
real, parameter    :: ilatn   = 0.5 
real, parameter    :: ilonn   = 0.5

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
character(len=255) :: line

real :: longRETRO(nlon)
real :: latRETRO(nlat)
real :: plat
real :: plon
 
integer :: i
integer :: j
integer :: k    !Angel 290516  
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
integer :: flag_rodovias
integer :: irunway
integer :: jrunway
integer :: dutraA
integer :: dutraB
integer :: dutraC
integer :: dutraD
integer :: dutraE
integer :: dutraF
integer :: dutraG
integer :: Istat
integer :: flag_inventario_RJ    !MArcelo 130516
integer, dimension(5) :: pontos_MRJ    ! Angel 290516   !pontos de cada uno de los 5 tipos de vias.
   
real :: rrlat
real :: rrlon
real :: dlon1
real :: dlon2
real :: dlat1
real :: dlat2
real :: TX(nspecies)
real :: lat
real :: lon
real :: rdummy
real :: src_dummy(nmonths)
real :: ratio_R
real :: rodlon
real :: rodlat
real :: CO_MRJ    !Angel 290516
real :: NOX_MRJ    !Angel 290516
real :: old_emis
real :: CO_via     !Angel 290516
real :: NOX_via     !Angel 290516
real, dimension(5) :: porcentage    !Angel 290516 %de emision x tipo de via



real, allocatable, dimension(:,:)     :: grid_area
real, allocatable, dimension(:,:,:)   :: old_value_R

real, allocatable, dimension(:,:) :: NOXAlonso
real, allocatable, dimension(:,:) :: COAlonso

type(T_mobileSrc), pointer, dimension(:) :: iMobileSrc




!-------------------------------------------------------

flag_inventario_RJ = 1 !Informa√ß√£o do invent√°rio local por vias para os jogos olimpicos

!-------------------------------------------------------

	


    flag_rodovias = 0




	!-lat e lon RETRO (corner mais ao sul e mais a oeste)	
	do i=1,nlon 
		longRETRO(i)=-179.75 + (i-1)*ilonn
	enddo 
	
	do j=1,nlat 
		latRETRO (j)= -89.75 + (j-1)*ilatn 
	enddo

	if( .not. allocated (RAWsrc)) then 
	       allocate(  RAWsrc(nlon,nlat,nmonths,nspecies) )
	       RAWsrc =0.
	endif
	
	if(ng==1) then
		prefix='RETRO_edg_anthro_'
		suffix='_2000.0.5x0.5.txt'
		
		do ispc=1,nspecies 
      			
			!-srf :  there is not emission data for these species (urb aer)
                        if(trim(spc_name(ispc))=='URBAN2' .or. trim(spc_name(ispc))=='URBAN3') cycle

			print*,'================================================================='
      			nc=len_trim(spc_name(ispc))
      			filename(ispc)=trim(retro_data_dir)//'/'//trim(prefix)//spc_name(ispc)(1:nc)//suffix
      			print *,'RETRO source opening   ', trim(filename(ispc)),' - specie= ',trim(spc_name(ispc))
      			open(110,file=trim(filename(ispc)),status='old')
    
     			!read  header
      			do i=1,28
	    			read(110,*) dummy
      			enddo
			
			do iread=1,nlon*nlat !maximum number of points for nlon,nlat 
   
   	 			read(110,*,iostat=fstat) lon,lat,(src_dummy(im),im=1,nmonths),rdummy
   	 			
				if(fstat .ne. 0)exit
				
   	 			ilaea = nint(2.*(lon - (-180. + 0.25) )) + 1 ! 2. = 1/resolution
   	 			jlaea = nint(2.*(lat - ( -90. + 0.25) )) + 1 ! 0.25 = 0.5*resolution
   	 			
				ilaea= max(ilaea,1)
   	 			
				if(ilaea < 1 .or. ilaea > 720) stop 33
   	 			if(jlaea < 1 .or. jlaea > 360) stop 35
   	 
	 			!if(lon .gt. -50. .and. lon .lt. -40.) then
	 			!	if(lat .gt. -30. .and. lat .lt. -20.) then
	 			!		if(ispc.eq.13) then 
	 			!      		testx=0.
	 			!      		do i=1,12
	 			!      			testx=testx+src_dummy(i)*30.
	 			!      		enddo
	 			!      		area=  cos(lat*3.1415/180.) * (6367000.**2) *0.5 * 0.5*(3.1415/180.)**2
	 			!      		testx=testx*86400.*area*1.e-6 
	 			!      		if(testx>50.)print*,'Gg/year=',lon,lat,testx,area
               			!--------------------------------------------------------------------
	       			!sao paulo
               			!Gg/year=   -46.75000  -23.75000  561.9854  2.8255962E+09
               			!Gg/year=   -46.75000  -23.25000  179.2299  2.8363377E+09
               			!Gg/year=   -46.25000  -23.75000  200.0330  2.8255962E+09
               			!Gg/year=   -46.25000  -23.25000  79.38746  2.8363377E+09
              			!total 4 boxes = 1020 Gg/year
              			!--------------------------------------------------------------------
   	 			!endif;endif;endif
	 
				RAWsrc(ilaea,jlaea,:,ispc)=src_dummy(:)
      
      			enddo ! do iread=1,nlon*nlat
			
			11 close(110)
			
		end do !ispc=1,nspecies
		
	endif
	
	!--- performs the interpolation to model grid box
	
	TX = 0
 
 	do i=1,n1
   		do j=1,n2
     			
			call get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longRETRO,latRETRO, &
		  			ilatn, ilonn ,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)
				      
     			call interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn,   &
	      			       imon,nmonths,nspecies,RAWsrc,tx(1:nspecies))
  
			
 			! TX is the value interpolated to the model grid box.
			!-obs: the raw data is  the monthly mean and the units are in kg /(m^2 s)
     			do ispc = 1, nspecies 
     				retro_g(ispc)%src(i,j,1)=TX(ispc)*86400. ! convert to kg/m2/day
     			enddo
                        !
			!srf- special section for urban aerosols 
                        retro_g(URBAN2)%src(i,j,1)=retro_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(2.95* 1.e-3) !=> kg[URBAN2]/m2/day 
                        retro_g(URBAN3)%src(i,j,1)=retro_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(8.77* 1.e-3) !=> kg[PM10]/m2/day 
                        !-end - special section for urban aerosols 
			
			

   		enddo ! j =1,n2
 	enddo ! i=1,n1

	if(trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'NONE' .and. &
   	   trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'none'  ) then

		!------ calculate the grib box area, if necessary
   		allocate(grid_area(n1,n2))
		
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


		!######################################### Highways ##################################################
 		if (flag_rodovias == 1) then
			print*,'----------------------------------------------------------------------'
   			print*,'Using Highways'
  	 		print*,'file=',trim(user_data_dir(1:len_trim(user_data_dir)))//'/Rodovias.dat'
 			print*,'Grid: ',ng

			!--------------------------------
			Istat = 0
			dutraA = 0
			dutraB = 0
			dutraC = 0
			dutraD = 0
			dutraE = 0
			dutraF = 0
			dutraG = 0
			!--------------------------------

			if(.NOT. allocated(grid_area))allocate(grid_area(n1,n2))
			if(.NOT. allocated(old_value_R))allocate(old_value_R(n1,n2,n3))

 			OPEN(UNIT = 12, FILE = trim(user_data_dir(1:len_trim(user_data_dir)))//'/Rodovias.dat',STATUS = 'OLD',IOSTAT = Istat)

			!###################MARCA AS ESTRADAS COM VALOR PADRAO###############
 			DO
 				READ (12,FMT='(A255)',IOSTAT = Istat)line
 				IF (Istat.ne.0) exit
 				READ (line,*)LON,LAT
 				rodlat= LAT
 				rodlon= LON
 				call update_emissions_by_runway(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                	,rodlon,rodlat,irunway,jrunway,grid_type,stdlat2)
				old_value_R(irunway,jrunway,1) = retro_g(CO)%src(irunway,jrunway,1)
				!----------------------------------DUTRA--------------------------------------
				!Trecho A
	 			IF (LAT.gt.-23.53.and.LAT.lt.-23.43.and.LON.gt.-46.59.and.LON.lt.-46.28) then
		 			IF (LON.gt.-46.4) then
		  				retro_g(CO)%src(irunway,jrunway,1)=777
		 				GO TO 100
		 			ENDIF
	 			ENDIF
				!Trecho B
	 			IF (LAT.gt.-23.41.and.LAT.lt.-23.26.and.LON.gt.-46.28.and.LON.lt.-45.89) then
	 				IF (LON.lt.-46.15.and.LAT.gt.-23.33) GO TO 50
	 				IF (LON.gt.-45.98.and.LAT.lt.-23.31) GO TO 50

	  				retro_g(CO)%src(irunway,jrunway,1)=888
	 				GO TO 100
50 CONTINUE
	 			ENDIF
				!Trecho C
	 			IF (LAT.gt.-23.25.and.LAT.lt.-22.80.and.LON.gt.-45.89.and.LON.lt.-45.27) then
	 				IF (LON.lt.-45.80.and.LAT.gt.-23.10) GO TO 55
	 				IF (LON.gt.-45.60.and.LAT.lt.-23.10) GO TO 55
	 				IF (LON.lt.-45.50.and.LAT.gt.-22.90) GO TO 55
	 				IF (LON.gt.-45.62.and.LAT.lt.-23.00) GO TO 55
	  					retro_g(CO)%src(irunway,jrunway,1)=666
	 				GO TO 100
55 CONTINUE
	 			ENDIF
				!Trecho D
	 			IF (LAT.gt.-22.95.and.LAT.lt.-22.52.and.LON.gt.-45.27.and.LON.lt.-44.79) then
	   				IF (LAT.lt.-22.90) GO TO 60
	   				IF (LON.lt.-45.12.and.LAT.gt.-22.74) GO TO 60
	   				IF (LON.gt.-45.12.and.LAT.gt.-22.66) GO TO 60

	  				retro_g(CO)%src(irunway,jrunway,1)=555
	 				GO TO 100
60 CONTINUE
	 			ENDIF
				!Trecho E
	 			IF (LAT.gt.-22.64.and.LAT.lt.-22.43.and.LON.gt.-44.79.and.LON.lt.-44.03) then
	  				IF (LON.gt.-44.55.and.LON.lt.-44.40.and.LAT.lt.-22.50) GO TO 70
	  				IF (LON.gt.-44.30.and.LON.lt.-44.18.and.LAT.lt.-22.56) GO TO 70
	  				IF (LON.gt.-44.20.and.LAT.gt.-22.57) GO TO 70
	  				retro_g(CO)%src(irunway,jrunway,1)=444
	 				GO TO 100
70 CONTINUE
	 			ENDIF
				!Trecho F
	 			IF (LAT.gt.-22.87.and.LAT.lt.-22.51.and.LON.gt.-44.03.and.LON.lt.-43.44) then
	  				IF (LON.gt.-44.10.and.LON.lt.-43.90.and.LAT.lt.-22.66) GO TO 80
	  				IF (LON.gt.-43.90.and.LON.lt.-43.70.and.LAT.lt.-22.80) GO TO 80
	  				IF (LON.gt.-44.10.and.LAT.gt.-22.55) GO TO 80
	  				IF (LON.gt.-43.90.and.LAT.gt.-22.64) GO TO 80
	  				IF (LON.gt.-43.70.and.LAT.gt.-22.80) GO TO 80
	   				retro_g(CO)%src(irunway,jrunway,1)=333
	 				GO TO 100
80 CONTINUE
	 			ENDIF
				!Trecho G
	 			IF (LAT.gt.-22.87.and.LAT.lt.-22.44.and.LON.gt.-43.44.and.LON.lt.-43.31) then
	  				IF (LAT.gt.-22.80) GO TO 90

	  				retro_g(CO)%src(irunway,jrunway,1)=222
	 				GO TO 100
90 CONTINUE
	 			ENDIF

				!----------------------------------------------------------------------------
	 			retro_g(CO)%src(irunway,jrunway,1)=111
100 CONTINUE
 			ENDDO
 			CLOSE (12)

			!----------------------------------------------------------------------------


			do i=1,n1
  				do j=1,n2
  					!Trecho A
				  	IF (retro_g(CO)%src(i,j,1).eq.777) then
				      		retro_g(CO)%src(i,j,1) = ((0.14 * 1E7)/365)/(grid_area(i,j))
				      		retro_g(NOX)%src(i,j,1) = ((0.02 * 1E7)/365)/(grid_area(i,j))
				      		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
				      		print*, 'RATIO = ',ratio_R
					     
					     	DO ispc=1,nspecies,1
							if (ispc.NE.18) then
								if (ispc.NE.13) then
							 		retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
								endif
							endif
					     	ENDDO
				 	ENDIF
  					!Trecho B
   					IF (retro_g(CO)%src(i,j,1).eq.888) then
				       		retro_g(CO)%src(i,j,1) = ((0.09 * 1E7)/365)/(grid_area(i,j))
				       		retro_g(NOX)%src(i,j,1) = ((0.014 * 1E7)/365)/(grid_area(i,j))
				       		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
       
             					DO ispc=1,nspecies,1
		        				if (ispc.NE.18) then
		        					if (ispc.NE.13) then
		         						retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
		        					endif
		        				endif
             					ENDDO


   					ENDIF


  					!Trecho C
   					IF (retro_g(CO)%src(i,j,1).eq.666) then
				       		retro_g(CO)%src(i,j,1) = ((0.09 * 1E7)/365)/(grid_area(i,j))
				       		retro_g(NOX)%src(i,j,1) = ((0.014 * 1E7)/365)/(grid_area(i,j))
				       		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
      
             					DO ispc=1,nspecies,1
                					if (ispc.NE.18) then
                						if (ispc.NE.13) then
                 							retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
                						endif
                					endif
             					ENDDO


  					ENDIF

  					!Trecho D
   					IF (retro_g(CO)%src(i,j,1).eq.555) then
       						retro_g(CO)%src(i,j,1) = ((0.18 * 1E7)/365)/(grid_area(i,j))
       						retro_g(NOX)%src(i,j,1) = ((0.03 * 1E7)/365)/(grid_area(i,j))
       						ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
       
             					DO ispc=1,nspecies,1
                					if (ispc.NE.18) then
                						if (ispc.NE.13) then
                 							retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
                						endif
                					endif
             					ENDDO


  					ENDIF

				 	!Trecho E
				   	IF (retro_g(CO)%src(i,j,1).eq.444) then
				       		retro_g(CO)%src(i,j,1) = ((0.045 * 1E7)/365)/(grid_area(i,j))
				       		retro_g(NOX)%src(i,j,1) = ((0.007 * 1E7)/365)/(grid_area(i,j))
				       		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
				      
					     	DO ispc=1,nspecies,1
							if (ispc.NE.18) then
								if (ispc.NE.13) then
						 			retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
								endif
								endif
					     	ENDDO


				  	ENDIF

				  	!Trecho F
				   	IF (retro_g(CO)%src(i,j,1).eq.333) then
				       		retro_g(CO)%src(i,j,1) = ((0.059 * 1E7)/365)/(grid_area(i,j))
				       		retro_g(NOX)%src(i,j,1) = ((0.009 * 1E7)/365)/(grid_area(i,j))
				       		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
				       	
					     	DO ispc=1,nspecies,1
							if (ispc.NE.18) then
								if (ispc.NE.13) then
						 			retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
								endif
							endif
					     	ENDDO


				  	ENDIF

				  	!Trecho G
				   	IF (retro_g(CO)%src(i,j,1).eq.222) then
						retro_g(CO)%src(i,j,1) = ((0.140 * 1E7)/365)/(grid_area(i,j))
				       		retro_g(NOX)%src(i,j,1) = ((0.023 * 1E7)/365)/(grid_area(i,j))
				       		ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
				       
					     	DO ispc=1,nspecies,1
							if (ispc.NE.18) then
								if (ispc.NE.13) then
						 			retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
								endif
							endif
					     	ENDDO

				  	ENDIF
 					IF (retro_g(CO)%src(i,j,1).eq.111) then
        					retro_g(CO)%src(i,j,1) = ((0.09 * 1E7)/365)/(grid_area(i,j))
       						retro_g(NOX)%src(i,j,1) = ((0.014 * 1E7)/365)/(grid_area(i,j))
       						ratio_R = retro_g(CO)%src(i,j,1)/old_value_R(i,j,1)
       
             					DO ispc=1,nspecies,1
                					if (ispc.NE.18) then
                						if (ispc.NE.13) then
                  							retro_g(ispc)%src(i,j,1)=ratio_R*retro_g(ispc)%src(i,j,1)
                						endif
                					endif
             					ENDDO

  					ENDIF

  				enddo
			enddo
			do i=1,n1
  				do j=1,n2
     					DO ispc=1,nspecies,1
     						IF (retro_g(ispc)%src(i,j,1).gt.1) THEN
     							retro_g(ispc)%src(i,j,1) = 0
     						ENDIF
     					ENDDO
  				enddo
			enddo
 
		endif




	
   		
		print*,'----------------------------------------------------------------------'
   		print*,'Using urban emissions updating for South America'
  	 	print*,'file=',trim(user_data_dir(1:len_trim(user_data_dir)))//'/SA_citiesMobileUrbanEmissions.asc'
	
		allocate(NOXAlonso(n1,n2))
   		allocate(COAlonso(n1,n2))
   
   		NOXAlonso = 0.
   		COAlonso  = 0.
   
   		call loadMobileSrcMap(trim(user_data_dir)//'/SA_citiesMobileUrbanEmissions.asc', iMobileSrc)
	
		call polyToModel(iMobileSrc,ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat,grid_type, COAlonso, NOXAlonso)
	
		do j = 1, n2
   			do i = 1, n1
		
				COAlonso(i,j)  = ((COAlonso(i,j)*1e7)/365.) / grid_area(i,j)  ! kg/m2/dia
				NOXAlonso(i,j) = ((NOXAlonso(i,j)*1e7)/365.) / grid_area(i,j) ! kg/m2/dia
		
				if(NOXAlonso(i,j) .gt. 0)then
					do ispc=1,nspecies
						if (ispc.ne.NOX .and. ispc.ne.CO .and. retro_g(NOX)%src(i,j,1) .ne. 0.) then
			        			retro_g(ispc)%src(i,j,1) = (  NOXAlonso(i,j) / retro_g(NOX)%src(i,j,1) )  * retro_g(ispc)%src(i,j,1) ! nox ratio 
						end if 
					end do

					retro_g(NOX)%src(i,j,1)  = NOXAlonso(i,j)
					retro_g(CO)%src(i,j,1)   = COAlonso(i,j)
					!
					!srf- special section for urban aerosols 
                                	retro_g(URBAN2)%src(i,j,1)=retro_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(2.95* 1.e-3) !=> kg[PM25]/m2/day 
                                	retro_g(URBAN3)%src(i,j,1)=retro_g(CO)%src(i,j,1)*(1./(28.*1e-3)) & ! mole[CO]/m2/day
                                                   *(8.77* 1.e-3) !=> kg[PM10]/m2/day 
                                	!-end - special section for urban aerosols 

				end if
	
			end do
   		end do
	
		deallocate(NOXAlonso, COAlonso)
   		deallocate(grid_area)
	
	endif ! if(trim(user_data_dir(1:len_trim(user_data_dir)) ) /= 'NONE'











!############# As emissıes para o municÌpio de RJ v„o ser georeferenciadas, por vias ############



if (flag_inventario_RJ == 1) then

                        print*,'----------------------------------------------------------------------'
                        print*,'Usando inventario local para o RJ (JO)'
                        print*,'Grid: ',ng


 !------ calculate the grib box area, if necessary
                allocate(grid_area(n1,n2))

                select case(grid_type)
                case('rams')
                        call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
                case('polar')
                        call get_area_rams(grid_area,n1,n2,xt,yt,xm,ym)
                case('ll')
                        call get_area_ll(grid_area,n1,n2)
                case('lambert')
                        grid_area(:,:) = 1./(grid_g(ng)%dxt(:,:)*grid_g(ng)%dyt(:,:))
                case('fim')
                        call get_area_fim(grid_area,n1,n2)
                case('gg')
                        call get_area_gg(grid_area,n1,n2,rlat,rlon)
                case default
                        stop 'grid_type wrong'
                end select




                 OPEN(UNIT = 13, FILE = '../extra/UserData/Estrutural.dat',STATUS = 'OLD')
                 OPEN(UNIT = 14, FILE = '../extra/UserData/Primaria.dat',STATUS = 'OLD')
                 OPEN(UNIT = 15, FILE = '../extra/UserData/Secundaria.dat',STATUS = 'OLD')
                 OPEN(UNIT = 16, FILE = '../extra/UserData/Coletora.dat',STATUS = 'OLD')
                 OPEN(UNIT = 17, FILE = '../extra/UserData/Local.dat',STATUS = 'OLD')

                 
!## Quantidade de pontos para cada tipo de vias onde v„o ser distribuÌdas as emissıes. 

                 pontos_MRJ(1) = 2950   !Estrutural 290516, Angel
                 pontos_MRJ(2) = 3216   !Primarias
                 pontos_MRJ(3) = 3588   !Secundarias
                 pontos_MRJ(4) = 9099   !Coletoras
                 pontos_MRJ(5) = 31802  !Locais

!## Emissıes para o municÌpios de Rio de Janeiro (10^7 kg/ano)
                 CO_MRJ = 2.78     
                 NOX_MRJ = 1.76  

!## Porcentagem das emissıes para cada tipo de via.

                 porcentage(1) = 0.395
                 porcentage(2) = 0.164
                 porcentage(3) = 0.130
                 porcentage(4) = 0.112
                 porcentage(5) = 0.199


 !## Ciclo para incluir os dados de emissıes nas vias por tipo.     


     DO i=1,5

       CO_via= (((CO_MRJ*porcentage(i))/pontos_MRJ(i))*1e7)/365
       NOX_via= (((NOX_MRJ*porcentage(i)/pontos_MRJ(i))*1e7)/365

       do j=1,pontos_MRJ(i)
            READ (i+12,*)LON,LAT
            rodlat= LAT
            rodlon= LON

call update_emissions_by_point(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                        ,rodlon,rodlat,irunway,jrunway,grid_type)
                                if (irunway.ne.-999.and.jrunway.ne.-999) then

                                retro_g(CO)%src(irunway,jrunway,1)=retro_g(CO)%src(irunway,jrunway,1)+(CO_via/grid_area(irunway,jrunway))

                                old_emis=retro_g(NOX)%src(irunway,jrunway,1)

                                retro_g(NOX)%src(irunway,jrunway,1)=retro_g(NOX)%src(irunway,jrunway,1)+(NOX_via/grid_area(irunway,jrunway))
                                print*,rodlon,rodlat,irunway,jrunway  
                        
                                do ispc=1,nspecies
                                                if (ispc.ne.NOX .and. ispc.ne.CO .and. ispc.ne.URBAN3 .and. ispc.ne.URBAN2 .and. retro_g(NOX)%src(irunway,jrunway,1) .ne. 0) then
                                                         retro_g(ispc)%src(irunway,jrunway,1) = (  retro_g(NOX)%src(irunway,jrunway,1) / old_emis )  * retro_g(ispc)%src(irunway,jrunway,1)
                                                end if
                                end do
                                
endif
   

       end do
     END DO 

endif !flag_inventario_RJ	
	!- deallocate memory that we do not need anymore    
	
if(ng == ngrids) deallocate (RAWsrc)
 
	if(grid_type == 'rams' .or. grid_type == 'polar') then ! only for 'rams' until rland is also defined for others grids 
    		do ispc=1,nspecies 
       			call apply_land_restriction(n1,n2,rland,retro_g(ispc)%src(:,:,1))
    		enddo  
 	endif
	

end subroutine read_retro_antro
!---------------------------------------------------------------
subroutine get_index1(i,j,nlon,nlat,n1,n2,rlat,rlon,longRETRO,latRETRO&
,ilatn, ilonn,dlat1,dlat2,dlon1,dlon2,i1,j1,i2,j2,ic,jc)
implicit none
integer nlon,nlat,n1,n2,i,j,jj,j1,j2,ii,i1,i2,ic,jc,ix,jx
real, dimension(n1,n2) :: rlat,rlon
real longRETRO(nlon),latRETRO(nlat)
real rrlat,rrlon,dlat1,dlat2,dlon1,dlon2,ilatn, ilonn
real difflon   !tks

    rrlat=rlat(i,j)
    rrlon=rlon(i,j)

    if(rrlon.lt.0)rrlon=360.+rrlon    
    difflon=(rrlon-longRETRO(1))
    if(difflon.lt.0.)difflon=abs(difflon)
    if(difflon.ge.360.)difflon=difflon-360.
    
!-new/faster way
!old ic =   (nint((rrlon-longRETRO(1))/ilonn)) + 1
 ic =   (nint(difflon/ilonn)) + 1       
 jc =   (nint((rrlat-latRETRO (1))/ilatn)) + 1

return
!------------------- old way ------------------------


   
    do ii= 1,nlon
     if(rrlon .le. longRETRO(ii) ) exit
    enddo
     i1 = ii-1
     i2 = ii

 
     if(i1.eq.0) then 
     	i1=nlon
     	dlon1=     rrlon - longRETRO(i1) +360.
     else
     	dlon1=     rrlon - longRETRO(i1)
     endif
     
     if(i2.gt.nlon) then
     	   i2=1
     	   dlon2= - ( rrlon - longRETRO(i2) - 360.)
     else
     	   dlon2= - ( rrlon - longRETRO(i2) )
     endif
 
    do jj= 2,nlat-1
     if(rrlat .le. latRETRO(jj)  ) exit
    enddo
     j1= jj-1
     j2= jj
     
     dlat1=	rrlat - latRETRO(j1)
     dlat2= - ( rrlat - latRETRO(j2) )
     
     jc=j1
     ic=i1
     if(dlon1.gt.dlon2) ic=i2
     if(dlat1.gt.dlat2) jc=j2

!print*,ix,jx,ic,jc

end subroutine get_index1



!---------------------------------------------------------------
subroutine get_retro_indentity(spc_name,ident)
!use chem1_list
use retro_emissions, only :  retro_nspecies=>nspecies&
                            ,retro_spc_name=>spc_name

implicit none
integer isp
character (len=*), intent(in)  :: spc_name
integer          , intent(out) :: ident

do isp = 1,retro_nspecies
  ident=-1
  if(spc_name == retro_spc_name(isp)) then
      print*,'==>retro found for ',spc_name
      ident=isp
      return
   endif
enddo

!print*,'chem1-list specie ',trim(spc_name), ' does not match any one of retro'
!print*,'ident=',ident
!stop 444
end subroutine get_retro_indentity
!---------------------------------------------------------------
subroutine interpol2(i,j,n1,n2,rlon,rlat,ic,jc,nlat,nlon,ilatn,ilonn&
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
     	    ncount = ncount + 1
     	    TX(:) = TX(:) + RAWsrc(ii,jj,imon,:)  
   enddo
enddo
TX(:) = TX(:) / (float(ncount) + 1.E-10) ! interpolated rate
end subroutine interpol2
