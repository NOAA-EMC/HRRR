!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!
!  Created by Rafael Stockler: 19/oct/2010
!  Version: 1.0.0
!  Rotinas para WRF
!---------------------------------------------------------------------------

#if RADM_WRF_FIM || RADM_FV3

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!---------------- WRF or FIM Pos-Processing        -----------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
subroutine write_cptec_to_wrf_fim(ng,file_in,iday,imon,iyear)
use chem1_list
use grid_dims_out, only: grid_type,use_bbem  

implicit none
integer, parameter :: nsrc=4!,nspecies=79, 
integer, intent(in) :: ng
real, allocatable :: var2d(:,:,:,:),plume_mean_fct(:,:,:)  &
                    ,plume_firesize(:,:,:),volc_prop(:,:,:)&
		    ,volc_degass_prop(:,:,:),plume_fre(:,:,:)
character(len=*) file_in

integer :: iunit,ispc,isrc,iveg_agreg,i,iday,imon,iyear
character(len=20) read_spc_name,read_src_name,section,read_units,date
character(len=20) read_mean,read_veg_name,c_dummy
integer read_ident_chem_mec,read_ident_src,dummy,read_ident_veg,ii,iii
integer read_ident_aer,read_aer_mode, imode
real ash_size_distr_dummy(10)
integer nxp,nyp
real dep_glon(2), dep_glat(2)
integer     :: &
  antro   = 01, & ! anthropogenic sources
  bburn   = 02, & ! biomass burning sources 
  bioge   = 03, & ! biogenic sources ! must be equal to "nsrc"
  geoge   = 04    ! geoge sources ! must be equal to "nsrc"


iunit=2


if(trim(chemical_mechanism)/='RADM_WRF_FIM')then
 print*,'==> WRF or FIM output only prepared for RADM chemical mechanism'
 stop 'compile again using CHEM=RADM_WRF_FIM'
endif

! input file name

open(iunit,status='old',form='formatted',file=file_in) 

read(iunit,*)  nxp,(dep_glon(i),i=1,2)
read(iunit,*)  nyp,(dep_glat(i),i=1,2)
read(iunit,*)  date

print*,'-----------------------------------------------------'
print*,'-----------------------------------------------------'
print*,'WRF/FIM/FV3 pos-proc=',nxp,nyp,trim(file_in)


allocate(var2d(nxp,nyp,nspecies,nsrc));var2d=0.
allocate(plume_mean_fct(nxp,nyp,4));plume_mean_fct=0.
allocate(plume_firesize(nxp,nyp,4));plume_firesize=0.
allocate(plume_fre     (nxp,nyp,5));plume_fre     =0.

allocate(volc_prop       (nxp,nyp,3));volc_prop   =0.
allocate(volc_degass_prop(nxp,nyp,3));volc_degass_prop=0.
ii=0; iii=0
   do i=1,nspecies*nsrc*5
       read(iunit,*,end=100) section

         !- emission section  --------------------------------
       if(trim(section) == 'emission' 		&
         .OR. trim(section) == 'chemistry' ) then
	 
         read(iunit,*) , read_spc_name &
		       , read_ident_chem_mec &
		       , read_src_name       &
		       , read_ident_src      &
		       , read_units
         
	 ispc = read_ident_chem_mec
         isrc = read_ident_src
         print*,read_spc_name,read_src_name,ispc,isrc
         if(trim(read_spc_name)=='ASH' ) then
	   read(iunit,*) c_dummy
	   read(iunit,*) ash_size_distr_dummy(:)
	   print*,"ash_size_distr=",ash_size_distr_dummy
	 endif
	 CALL vfirec(iunit,var2d(1,1,ispc,isrc),nxp*nyp,'LIN')
	 
	 where( var2d(:,:,ispc,isrc) <0.)  var2d(:,:,ispc,isrc)=0.
         !print*,var2d
       
       elseif (trim(section) == 'aerosol') then
       
         READ(iunit,*)   read_spc_name       &
               , read_ident_aer      &
               , read_aer_mode       &
               , read_src_name       &
               , read_ident_src      &
               , read_units
          ispc = read_ident_aer
          imode= read_aer_mode
          isrc = read_ident_src         
	 
         print*,read_spc_name,read_src_name,ispc,isrc
         if(trim(read_spc_name)=='V_ASH1' ) then
	   read(iunit,*) c_dummy
	   read(iunit,*) ash_size_distr_dummy(:)
	   !print*,"ash_size_distr_dummy(:)=",ash_size_distr_dummy
	 endif
         
	 CALL vfirec(iunit,var2d(1,1,ispc,isrc),nxp*nyp,'LIN')
	 
	 where( var2d(:,:,ispc,isrc) <0.)  var2d(:,:,ispc,isrc)=0.
         !print*,var2d      
	
       !- plume section  --------------------------------
       elseif(trim(section) == 'plume') then

         read(iunit,*) , read_mean &
		       , dummy &
		       , read_veg_name       &
		       , read_ident_veg      &
		       , read_units
         print*,'reading ',read_mean,' for ',read_veg_name

	 iveg_agreg = read_ident_veg       
         if(trim(read_mean) == 'mean_fct' ) then
	   call vfirec(iunit,plume_mean_fct(:,:,iveg_agreg),nxp*nyp,'LIN')
	   where( plume_mean_fct(:,:,iveg_agreg) <0.) plume_mean_fct(:,:,iveg_agreg)=0.
           !print*,plume_mean_fct

	 elseif(trim(read_mean) == 'firesize' ) then
	   call vfirec(iunit,plume_firesize(:,:,iveg_agreg),nxp*nyp,'LIN')
 	   where( plume_firesize(:,:,iveg_agreg) <0.) plume_firesize(:,:,iveg_agreg)=0.
           !print*,plume_firesize
        endif
       
       !- plume FRE section  --------------------------------
       elseif(trim(section) == 'plumefre') then
           read(iunit,*)   read_mean 
           print*,"PLUME FRE section: reading ",read_mean
! RAR: changed the name
!	   if(TRIM(read_mean) == 'mean_fct' ) then

       if(TRIM(read_mean) == 'flam_frac' ) then
    	   call vfirec(iunit,plume_fre(:,:,1),nxp*nyp,'LIN')
	   where( plume_fre(:,:,1) <0.) plume_fre(:,:,1)=0.
           
	   elseif(TRIM(read_mean) == 'mean_frp' ) then
    	   call vfirec(iunit,plume_fre(:,:,2),nxp*nyp,'LIN')
	   where( plume_fre(:,:,2) <0.) plume_fre(:,:,2)=0.
	   
	   elseif(TRIM(read_mean) == 'std_frp' ) then
    	   call vfirec(iunit,plume_fre(:,:,3),nxp*nyp,'LIN')
	   where( plume_fre(:,:,3) <0.) plume_fre(:,:,3)=0.

	   elseif(TRIM(read_mean) == 'mean_size' ) then
    	   call vfirec(iunit,plume_fre(:,:,4),nxp*nyp,'LIN')
	   where( plume_fre(:,:,4) <0.) plume_fre(:,:,4)=0.
	   
	   elseif(TRIM(read_mean) == 'std_size' ) then
    	   call vfirec(iunit,plume_fre(:,:,5),nxp*nyp,'LIN')
	   where( plume_fre(:,:,5) <0.) plume_fre(:,:,5)=0.
           else
	    stop "unknown read_mean parameter at plumefre section"
	   endif
      
       !- volcanoes eruption section  --------------------------------
       elseif(trim(section) == 'volcanic-eruption') then
        ii=ii+1
	 read(iunit,*) , read_mean &
		       , read_units 
         print*,'reading ',read_mean,' for volcanoes eruption'
	 call vfirec(iunit,volc_prop(:,:,ii),nxp*nyp,'LIN')
 	 where( volc_prop(:,:,ii) <0.) volc_prop(:,:,ii)=0.
	 print*,'maxval=',maxval(volc_prop(:,:,ii))
      
      !- volcanoes  degassing section  --------------------------------
       elseif(trim(section) == 'volcanic-degassing') then
        iii=iii+1
	 read(iunit,*) , read_mean &
		       , read_units 
         print*,'reading ',read_mean,' for volcanoes degassing'
	 call vfirec(iunit,volc_degass_prop(:,:,iii),nxp*nyp,'LIN')
 	 where( volc_prop(:,:,iii) <0.) volc_degass_prop(:,:,iii)=0.
	 print*,'maxval=',maxval(volc_degass_prop(:,:,iii))	
       endif

    enddo
100 continue
!- split bburn emissions into flaming/smoldering parts
 call emis_flam_smold_WRF(nxp,nyp,nspecies,nsrc,var2d,plume_mean_fct&
                          ,plume_firesize,plume_fre)


if(trim(grid_type)=='fim' .or. trim(grid_type)=='fv3') then
    print*,"==> processing emission files for FIM model" 
    call write_to_FIM(ng,nxp,nyp,nspecies,nsrc,var2d,plume_mean_fct&
           ,plume_firesize,volc_prop,file_in,iday,imon,iyear,volc_degass_prop&
	   ,ash_size_distr_dummy,plume_fre)

else

!- convert to WRF Format 
    print*,"==> processing emission files for WRF-Chem model" 
    call write_to_WRFCHEM(nxp,nyp,nspecies,nsrc,var2d,plume_mean_fct&
           ,plume_firesize,volc_prop,file_in,iday,imon,iyear,volc_degass_prop&
	   ,ash_size_distr_dummy,plume_fre)

endif


close(iunit)
end subroutine write_cptec_to_wrf_fim
!-------------------------------------------------------------------------
  subroutine emis_flam_smold_WRF(n2,n3,nspecies,nsrc,var2d,plume_mean_fct&
             ,plume_firesize,plume_fre)
    use grid_dims_out, only: use_bbem  
    implicit none
    integer,intent(IN) :: n2,n3,nspecies,nsrc
    real,dimension(n2,n3) :: smold_frac 
    real,dimension(n2,n3,4) :: plume_mean_fct,plume_firesize
    real,dimension(n2,n3,5) :: plume_fre
    real,dimension(n2,n3,nspecies,nsrc) :: var2d 
    
    integer     :: &
      antro   = 01, & ! anthropogenic sources
      bburn   = 02, & ! biomass burning sources 
      bioge   = 03    ! biogenic sources 
    integer, parameter :: nveg_agreg      = 4
    integer, parameter :: tropical_forest = 1
    integer, parameter :: boreal_forest   = 2
    integer, parameter :: savannah        = 3
    integer, parameter :: grassland       = 4

    integer iv,ispc,i,j
    
    
    
    !-----  
    !- calcula a emissao smoldering e fatores para obtencao da fracao
    !- flaming em funcao da emissao smoldering
    if(use_bbem==1)then

       smold_frac(:,:) = 1.- ( plume_mean_fct(:,:,tropical_forest) + &
  			       plume_mean_fct(:,:,boreal_forest  ) + &
  			       plume_mean_fct(:,:,savannah	) + &
  			       plume_mean_fct(:,:,grassland	)   )	 
   
    elseif(use_bbem==2)then
   
      smold_frac(:,:) = 1.- plume_fre(:,:,1)
    
    endif  
    
    do ispc = 1,nspecies
  	!- convert from 'total' emisson to 'smoldering'
	var2d(:,:,ispc,bburn) = smold_frac(:,:) * var2d(:,:,ispc,bburn) 
	 !do i=1,n2; do j=1,n3
	 ! if(smold_frac(i,j) < 1. .and. var2d(i,j,ispc,bburn) > 0.)  print*,smold_frac(i,j) ,var2d(i,j,ispc,bburn)
	 !enddo;enddo
  						     
    enddo

    !- convert from flaming fraction to relationship with the phase smoldering emission amount
    if(use_bbem==1)then
    
     do iv = 1, nveg_agreg
         plume_mean_fct(:,:,iv) = plume_mean_fct(:,:,iv)/(1.e-8+smold_frac(:,:))
     enddo
    
    elseif(use_bbem==2)then

! RAR: 
 !    if maxval(plume_fre(:,:,1))>1.0) then
        write(*,*) 'max(plume_fre): ',maxval(plume_fre(:,:,1))
  !   end if

     plume_fre(:,:,1)=plume_fre(:,:,1)/(1.e-8+smold_frac(:,:))

!    if maxval(plume_fre(:,:,1))>1.0) then
         write(*,*) '2:max(plume_fre): ',maxval(plume_fre(:,:,1))
         write(*,*) 'min(smold_frac(:,:)): ',minval(smold_frac(:,:))
 !   end if

    endif
       
  end subroutine  emis_flam_smold_WRF    
!---------------------------------------------------------
subroutine write_to_FIM(ng,n2,n3,nspecies,nsrc,var2d,plume_mean_fct &
                             ,plume_firesize,volc_prop,file_in,iday,imon,iyear&
			     ,volc_degass_prop,ash_size_distr,plume_fre)

                            
    use grid_dims_out
    use chem1_list, only : spc_name, weight, BBURN2,BBURN3,OC,BC,URBAN2,URBAN3
    use gocart_backgr, only :  gocart_bg_nspecies=>nspecies&
                              ,gocart_bg_spc_name=>spc_name&
			      ,nlevels_netcdf,gocart_bg_g,lev
    use afwa_erodab, only :  afwa_erod_nspecies=>nspecies&
                              ,afwa_erod_spc_name=>spc_name&
			      ,afwa_erod_g
    implicit none
    integer,intent(IN) :: ng,n2,n3,nspecies,nsrc
    real,dimension(n2,n3) :: smold_frac
    real,dimension(n2,n3,5) :: plume_fre
    real,dimension(n2,n3,4) :: plume_mean_fct,plume_firesize
    real,dimension(n2,n3,3) :: volc_prop,volc_degass_prop
    real,dimension(55,n2,n3) :: gocartbg_level
    real,dimension(n2,n3,55) :: gocartbg_level_new
    real,dimension(n2,n3,nspecies,nsrc) :: var2d
    real, dimension(n2,n3) :: dummy
    character(*) ::file_in
    character(len=240) :: fileAB,fileBB,fileBG,Fprefix
    integer     :: &
      antro   = 01, & ! anthropogenic sources
      bburn   = 02, & ! biomass burning sources
      bioge   = 03 ,&   ! biogenic sources
      geoge   = 04    ! biogenic sources
    integer, parameter :: nveg_agreg      = 4
    integer, parameter :: tropical_forest = 1
    integer, parameter :: boreal_forest   = 2
    integer, parameter :: savannah        = 3
    integer, parameter :: grassland       = 4

    integer, parameter :: nspecies_wrf = 29
    real :: ash_size_distr(10)
    integer ispc,isrc,ispc_wrf,iv,iveg,ifound,iday,imon,iyear
    character(len=20) ename
    character(len=10) cdummy
    integer itime,i,j,k
    integer istatus,system
    logical exists
    real fx
!from     /home/poluicao/WRFV2-CHEM/chem/module_input_chem_data.F
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies_wrf) :: spc_name_wrf=(/ &
 'SO2     ' &!
,'NO2     ' &!
,'NO      ' &!
,'ALD     ' &!
,'HCHO    ' &!
,'ORA2    ' &!
,'NH3     '  &! XXXXX
,'HC3     ' &!
,'HC5     ' &!
,'HC8     ' &!
,'ETH     ' &!
,'CO      ' &!
,'OL2     ' &! XXXXX
,'OLT     ' &!
,'OLI     ' &!
,'TOL     ' &!
,'XYL     ' &!
,'KET     ' &!
,'CSL     ' &!
,'ISO     ' &!
,'BBURN2  ' &!PM2.5
,'BBURN3  ' &!PM10.
,'URBAN2  ' &!PM2.5
,'URBAN3  ' &!PM10.
,'OC      ' &!
,'BC      ' &
,'DMS     ' &
,'SO4     ' &! SULF changed SO4
,'ASH     ' &

/)
!
!,'PM25I ' &!
!,'PM25J ' &!
!,'SO4I	' &!
!,'SO4J	' &!
!,'NO3I	' &!
!,'NO3J	' &!
!,'ORGI	' &!
!,'ORGJ	' &!
!,'ECI	' &!
!,'ECJ	' &!
!,'PM10	'


!antro + bioge section
ename='XXXXXXXX'
itime = 0
!fileAB=file_in(1:len_trim(file_in)-4)//'-ab.bin'

!open (91,file=fileAB,form='unformatted')
!     write(91)nspecies_wrf
!     write(91)ename
!     write(91)itime
! 10/15/17 put fv3 output into separate "tile" directories
 
  Fprefix='./'
  if(trim(grid_type)=='fv3') then
  write(Fprefix,'(A4,I1,A1)')'tile',ng,'/'
      INQUIRE(FILE=trim(Fprefix),EXIST=EXISTS)
    if(.not.exists)then
    istatus=SYSTEM('mkdir -p -v '//trim(Fprefix))
    endif
  endif
iv=0
do ispc_wrf=1,nspecies_wrf
  !print*,'spc_name_wrf(ispc_wrf)',spc_name_wrf(ispc_wrf)
  ifound=0
  do ispc = 1,nspecies
     !if(spc_alloc(src,ispc) == 0) cycle

     if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
     	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BBURN3') then
	    ifound=1
	    cycle  
	 endif
         
	 !print*,'FOUND ',spc_name_wrf(ispc_wrf),spc_name(ispc)
         !-for WRF/FIM, 'biogenic' emissions for CO is not included
	 !- 
	 if(trim(spc_name(ispc)) == 'CO') then
	     dummy(:,:) = var2d(:,:,ispc,antro)
	 else
	     dummy(:,:) = var2d(:,:,ispc,antro)+var2d(:,:,ispc,bioge) &
         + var2d(:,:,ispc,geoge)
	 endif

	 if(trim(spc_name_wrf(ispc_wrf)) == 'URBAN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'URBAN3' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'OC'   .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BC'  ) then
	    fx= 1.e+9  / 86400.	 !  ug/(m^2 sec)
	 else
	    fx= 1.e+3 * 1.e6 / weight(ispc) / 24. ! emission unit: mol/(km^2 hour)
	 endif

         fileAB=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-'&
         //trim(spc_name_wrf(ispc_wrf))//'-ab.bin'
	 open (91,file=fileAB,form='unformatted')
	 print*, 'writing AB specie= ',spc_name(ispc), maxval(dummy), minval(dummy),iv+1
         write(91) dummy * fx
         iv=iv+1
	 ifound=1
	 close(91)
	 exit
     endif
  enddo
  !if(ifound == 0) then
   ! print*,'NOT FOUND, zero emission',spc_name_wrf(ispc_wrf)
  !   dummy(:,:) =0.
  !   write(91) dummy
  !   iv=iv+1
  !   ifound=1
  !endif

enddo
print*,'Anthro + biogenic emissions for WRF-Chem are in the file: ',fileAB
print*,"-------------------------------------------------------------"

!bburn + plumerise section
print*,'=> bburn + plumerise section'
!fileBB=file_in(1:len_trim(file_in)-4)//'-bb.bin'
!open (91,file=fileBB,form='unformatted')
!     write(91)nspecies_wrf + 8
!     write(91)ename
!     write(91)itime
iv=0
do ispc_wrf=1,nspecies_wrf

   ifound=0
   do ispc = 1,nspecies
     if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
     	 if(trim(spc_name_wrf(ispc_wrf)) == 'URBAN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'URBAN3') then
	    ifound=1
	    cycle
	 endif
        ! print*,' specie=',trim(spc_name(ispc))
         dummy(:,:) = var2d(:,:,ispc,bburn)


	 !-PM25 for wrf:

	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' ) then

	 dummy(:,:) = max(0.,var2d(:,:,BBURN2,bburn)  -  &
	                     var2d(:,:,OC  ,bburn)  -  &
	                     var2d(:,:,BC  ,bburn)     )

	 endif

	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BBURN3' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'OC'   .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BC'  ) then
	    fx= 1.e+9  / 86400.	 !  ug/(m^2 sec)
	 else
	    fx= 1.e+3 * 1.e6 / weight(ispc) / 24.! emission unit: mol/(km^2 hour)
	 endif

         print*,' specie=',trim(spc_name(ispc)),maxval(dummy*fx),maxloc(dummy*fx)!,,minval(dummy*fx)

         fileBB=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-'&
         //trim(spc_name_wrf(ispc_wrf))//'-bb.bin'
	 print*, 'writing BB specie= ',trim(spc_name(ispc)), maxval(dummy), minval(dummy)
         open (91,file=fileBB,form='unformatted')
	 write(91) dummy*fx
	 iv=iv+1
	 ifound=1
        exit
     endif

  enddo


!  if(ifound == 0) then
!     !print*,'not found, zero emission ',spc_name_wrf(ispc_wrf)
!     dummy(:,:) =0.
!     write(91) dummy
!     iv=iv+1
!     ifound=1
!  endif

enddo
fileBB=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-plume.bin'
open (91,file=fileBB,form='unformatted')
if(use_bbem==1) then
  do iveg=1,nveg_agreg
   	 print*, 'writing BB flam fracion for veg= ',iveg,&
	          maxval( plume_mean_fct(:,:,iveg)),minval( plume_mean_fct(:,:,iveg))
         write(91) plume_mean_fct(:,:,iveg)
	 iv=iv+1
	 !print*,'iveg plume_fct', iveg,maxval(plume_mean_fct(:,:,iveg)),iv
  enddo
  do iveg=1,nveg_agreg
   	 print*, 'writing BB fire size for veg= ',iveg,&
	          maxval( plume_firesize(:,:,iveg)),minval(plume_firesize (:,:,iveg))
         write(91) plume_firesize(:,:,iveg)
	 iv=iv+1
	 !print*,'iveg plume_firesize', iveg,maxval(plume_firesize(:,:,iveg)),iv
  enddo
elseif(use_bbem==2) then
  do i=1,5
    if(i==1)cdummy="flam_frac"
    if(i==2)cdummy="mean_frp "
    if(i==3)cdummy="std_frp  "
    if(i==4)cdummy="mean_size"
    if(i==5)cdummy="std_size "
    print*,"writing BB fields for FRE: ",cdummy(1:len(cdummy)),&
  	    maxval( plume_fre(:,:,i)),minval(plume_fre(:,:,i))
    write(91) plume_fre(:,:,i)
    iv=iv+1
  enddo
endif

close (91)
!print*,'BB: total 2d fields=',iv
!print*,' Biomass burning emission + plumerise data for wrf: ',fileBB
print*,"-------------------------------------------------------------"
print*,'Biomass burning emission + plumerise data for FIM-Chem are in the file: ',fileBB


!-------------------------- volcanoes -------------------------
if(use_volcanoes == 1) then
   print*,'=> Volcanoes Eruption Section '
   ename='volc-eruption'
   fileBB=trim(Fprefix)//file_in(1:len_trim(file_in)-4)//'-volc.bin'
   open (91,file=fileBB,form='unformatted')
     write(91) 5 !fields
     write(91)ename
     write(91)begin_eruption
     write(91) ash_size_distr(:)
     
     iv=0
     do ispc_wrf=1,nspecies_wrf
      do ispc = 1,nspecies
        if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
         if(trim(spc_name_wrf(ispc_wrf)) == 'ASH' .or. &
            trim(spc_name_wrf(ispc_wrf)) == 'SO2'      ) then
	 dummy=0.
	 print*,' specie=',trim(spc_name(ispc))
	 
         do j=1,n3 ; do i=1,n2
          if( volc_prop(i,j,2) > 0.001) &! time duration
	  
          dummy(i,j) = var2d(i,j,ispc,geoge)*1.e+9  / volc_prop(i,j,3) !  ug/(m^2 sec)
         
         enddo;enddo
	 write(91) dummy
       endif;endif
     enddo;enddo
     ! inj height
     write(91) volc_prop(:,:,1)
     ! elvation
     write(91) volc_prop(:,:,2)
     ! duration
     write(91) volc_prop(:,:,3)

   close (91)
print*,'volcanic SO2 and ASH emission, height, elevation and duration  data for FIM: ',fileBB
print*,'volcanic begin time=',begin_eruption
print*,'Volc ASH size distribution (fraction)'
print*, ash_size_distr(:)

endif
!---------------------------------------------------------------------------

if(use_degass_volcanoes == 1) then
   ename='volc-degass'
   begin_eruption= "NOT-DEFINED"  ! dummy
   fileBB=trim(Fprefix)//file_in(1:len_trim(file_in)-4)//'-volc.bin'
   open (91,file=fileBB,form='unformatted')
     write(91) 3 !fields
     write(91)ename
     write(91)begin_eruption
     iv=0
     do ispc_wrf=1,nspecies_wrf
      do ispc = 1,nspecies
        if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
         if(trim(spc_name_wrf(ispc_wrf)) == 'SO2') then
	 dummy=0.
	 print*,' specie=',trim(spc_name(ispc))
	 do j=1,n3 ; do i=1,n2
          if( volc_degass_prop(i,j,2) > 0.001) &! time duration
	  
          dummy(i,j) = var2d(i,j,ispc,geoge) / volc_degass_prop(i,j,2) !  kg/(m^2 sec)
         enddo;enddo
	 write(91) dummy
       endif;endif
     enddo;enddo
     ! inj height
     write(91) volc_degass_prop(:,:,1)
     ! elevation
     write(91) volc_degass_prop(:,:,2)
   close (91)
print*,'volcanic degassing SO2 emission, height and elevation data for wrf: ',fileBB
endif
!---------------------------------------------------------------------------

!return  !<<<<<<<<<<
!----------- GOCART background data --------------
if(use_gocart_bg == 1) then

!fileBG=file_in(1:len_trim(file_in)-4)//'-gocartBG.bin'
!open (91,file=fileBG,form='unformatted')

 do ispc=1,gocart_bg_nspecies

fileBG=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-gocartBG-'//trim(gocart_bg_spc_name(ispc))//'.bin'
!open (91,file=fileBG,form='unformatted')
   print*,'writing spc=',gocart_bg_spc_name(ispc),n2,n3,nlevels_netcdf(ispc)

   !-DMS
   if(trim(gocart_bg_spc_name(ispc))=='DMS') then
open (91,file=fileBG,form='unformatted')

       print*,'DMS for month=',imon
       print*,'gocart_bg_spc_name',gocart_bg_spc_name(ispc)
       write(91) gocart_bg_g(ispc)%src(1:n2,1:n3,imon)
   close(91)
       cycle
   endif

   !special section of writing 'lev'
   !if(trim(gocart_bg_spc_name(ispc))=='H2O2') then
   !    write(91) lev(1:nlevels_netcdf(ispc))
   !endif

    if (trim(gocart_bg_spc_name(ispc))=='OH'.or.trim(gocart_bg_spc_name(ispc))=='H2O2'.or.trim(gocart_bg_spc_name(ispc))=='NO3') then
open (91,file=fileBG,form='unformatted')
          print *,minval( gocart_bg_g(ispc)%src(:,:,15))
          print *,maxval( gocart_bg_g(ispc)%src(:,:,15))
        do i=1,n2
         do j=1,n3
         do k=1,55

    !gocartbg_level(k,i,j)=gocart_bg_g(ispc)%src(i,j,k)
    gocartbg_level_new(i,j,k)=gocart_bg_g(ispc)%src(i,j,k)
         enddo
         enddo
        enddo
   !write(91) gocartbg_level(1:nlevels_netcdf(ispc),1:n2,1:n3)
   write(91) gocartbg_level_new(1:n2,1:n3,1:nlevels_netcdf(ispc))
   elseif (trim(gocart_bg_spc_name(ispc))=='EROD')then !11/4/17 Split erodable fractions into 3 separate files
  do i=1,nlevels_netcdf(ispc)
   write(cdummy,'(A4,I1)')'erod',i
   fileBG=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-gocartBG-'//cdummy(1:5)//'.bin'
   open (91,file=fileBG,form='unformatted')
   dummy(:,:)=gocart_bg_g(ispc)%src(:,:,i)
   print *, 'erod',i,maxval(dummy),minval(dummy)
!  write(91) gocart_bg_g(ispc)%src(1:n2,1:n3,i)
   write(91) dummy
   close(91)
  enddo
   else
   STOP'Unexpected gocartBG species'
   endif
 enddo
close (91)

endif

!    use gocart_backgr, only :  gocart_bg_nspecies=>nspecies&
!                              ,gocart_bg_spc_name=>spc_name&
!			      ,nlevels_netcdf,gocart_bg_g,lev!
!----------- AFWA erodability data --------------
if(use_afwa_erod == 1) then
!   use afwa_erodab, only :  afwa_erod_nspecies=>nspecies&
!                             ,afwa_erod_spc_name=>spc_name&
!		      ,afwa_erod_g

print*,'=> ------------------------------------------------------'
print*,'=> AFWA Erodability  Section'

 do ispc=1,afwa_erod_nspecies
         fileBG=trim(Fprefix)//file_in(1:len_trim(file_in)-7)//'-'&
         //trim(afwa_erod_spc_name(ispc))//'.bin'
open (91,file=fileBG,form='unformatted')
   print*,'writing spc = ', trim(afwa_erod_spc_name(ispc)),n2,n3
       write(91) afwa_erod_g(ispc)%src(1:n2,1:n3,1)
close (91)
 enddo

endif

end subroutine write_to_FIM
!---------------------------------------------------------
subroutine write_to_WRFCHEM(n2,n3,nspecies,nsrc,var2d,plume_mean_fct &
                           ,plume_firesize,volc_prop,file_in,iday,imon,iyear&
			   ,volc_degass_prop,ash_size_distr,plume_fre)
    use grid_dims_out
    use chem1_list, only : spc_name, weight, BBURN2,BBURN3,OC,BC, URBAN2,URBAN3
    use gocart_backgr, only :  gocart_bg_nspecies=>nspecies&
                              ,gocart_bg_spc_name=>spc_name&
			      ,nlevels_netcdf,gocart_bg_g,lev
    implicit none
    integer,intent(IN) :: n2,n3,nspecies,nsrc
    real,dimension(n2,n3) :: smold_frac
    real,dimension(n2,n3,4) :: plume_mean_fct,plume_firesize
    real,dimension(n2,n3,5) :: plume_fre
    real,dimension(n2,n3,3) :: volc_prop,volc_degass_prop
    real,dimension(n2,n3,nspecies,nsrc) :: var2d
    real, dimension(n2,n3) :: dummy
    character(*) ::file_in
    character(len=240) :: fileAB,fileBB,fileBG
    integer     ::  &
      antro   = 01, & ! anthropogenic sources
      bburn   = 02, & ! biomass burning sources
      bioge   = 03 ,&   ! biogenic sources
      geoge   = 04    ! biogenic sources
    integer, parameter :: nveg_agreg      = 4
    integer, parameter :: tropical_forest = 1
    integer, parameter :: boreal_forest   = 2
    integer, parameter :: savannah        = 3
    integer, parameter :: grassland       = 4

    integer, parameter :: nspecies_wrf = 29

    integer ispc,isrc,ispc_wrf,iv,iveg,ifound,iday,imon,iyear,i,j
    character(len=20) ename
    character(len=10) cdummy
    real ash_size_distr(10)
    integer itime
    real fx
!from     /home/poluicao/WRFV2-CHEM/chem/module_input_chem_data.F
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies_wrf) :: spc_name_wrf=(/ &
 'SO2     ' &!
,'NO2     ' &!
,'NO      ' &!
,'ALD     ' &!
,'HCHO    ' &!
,'ORA2    ' &!
,'NH3     ' &! XXXXX
,'HC3     ' &!
,'HC5     ' &!
,'HC8     ' &!
,'ETH     ' &!
,'CO      ' &!
,'OL2     ' &! XXXXX
,'OLT     ' &!
,'OLI     ' &!
,'TOL     ' &!
,'XYL     ' &!
,'KET     ' &!
,'CSL     ' &!
,'ISO     ' &!
,'BBURN2  ' &!
,'BBURN3  ' &!)
,'URBAN2  '&
,'URBAN3  '&
,'OC      ' &!
,'BC      ' &
,'DMS     ' &
,'SO4     ' &! SO4
,'ASH     ' &

/)

!
!,'PM25I ' &!
!,'PM25J ' &!
!,'SO4I	' &!
!,'SO4J	' &!
!,'NO3I	' &!
!,'NO3J	' &!
!,'ORGI	' &!
!,'ORGJ	' &!
!,'ECI	' &!
!,'ECJ	' &!
!,'PM10	'


!- antro + bioge section
print*,'=> anthro + biogenic section'

ename='XXXXXXXX'
itime = 0
fileAB=file_in(1:len_trim(file_in)-4)//'-ab.bin'

open (91,file=fileAB,form='unformatted')
     write(91)nspecies_wrf
     write(91)ename
     write(91)itime
iv=0
do ispc_wrf=1,nspecies_wrf
  !print*,'spc_name_wrf(ispc_wrf)',spc_name_wrf(ispc_wrf)
  ifound=0
  do ispc = 1,nspecies
     !if(spc_alloc(src,ispc) == 0) cycle

     !print*,'YY    ',spc_name(ispc),spc_name_wrf(ispc_wrf)

     if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
     
     	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BBURN3') then
	    ifound=1
	    cycle
	 endif
	 
         !print*,'FOUND ',spc_name_wrf(ispc_wrf),spc_name(ispc)
	 
         !-for WRF/FIM, 'biogenic' emissions for CO is not included
	 !- 
	 if(trim(spc_name(ispc)) == 'CO') then
	     dummy(:,:) = var2d(:,:,ispc,antro)
	 else
	     dummy(:,:) = var2d(:,:,ispc,antro)+var2d(:,:,ispc,bioge)
	 endif
	    
	 if(trim(spc_name_wrf(ispc_wrf)) == 'URBAN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'URBAN3' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'OC'   .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BC'  ) then
	    fx= 1.e+9  / 86400.	 !  ug/(m^2 sec)
	 else
	    fx= 1.e+3 * 1.e6 / weight(ispc) / 24. ! emission unit: mol/(km^2 hour)
	 endif

	 print*, 'writing AB specie= ',spc_name(ispc), maxval(dummy), minval(dummy),iv+1

         write(91) dummy * fx
         iv=iv+1
	 ifound=1
	 exit
     endif
  enddo
  if(ifound == 0) then
    print*,'NOT FOUND, zero emission for ',spc_name_wrf(ispc_wrf),iv+1
     dummy(:,:) =0.
     write(91) dummy
     iv=iv+1
     ifound=1
  endif

enddo
close (91)
print*,'Anthro + biogenic emissions for WRF-Chem are in the file: ',fileAB
print*,"-------------------------------------------------------------"

!- bburn + plumerise section
print*,'=> bburn + plumerise section'

fileBB=file_in(1:len_trim(file_in)-4)//'-bb.bin'
open (91,file=fileBB,form='unformatted')
     write(91)nspecies_wrf + 8
     write(91)ename
     write(91)itime
iv=0
do ispc_wrf=1,nspecies_wrf

   ifound=0
   do ispc = 1,nspecies
     if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
     	 if(trim(spc_name_wrf(ispc_wrf)) == 'URBAN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'URBAN3') then
	    ifound=1
	    cycle
	 endif
         !print*,' specie=',trim(spc_name(ispc))
         dummy(:,:) = var2d(:,:,ispc,bburn)


	 !-PM25 for wrf:

	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' ) then

	 dummy(:,:) = max(0.,var2d(:,:,BBURN2,bburn)  -  &
	                     var2d(:,:,OC  ,bburn)  -  &
	                     var2d(:,:,BC  ,bburn)     )

	 endif

	 if(trim(spc_name_wrf(ispc_wrf)) == 'BBURN2' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BBURN3' .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'OC'   .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BC'  ) then
	    fx= 1.e+9  / 86400.	 !  ug/(m^2 sec)
	 else
	    fx= 1.e+3 * 1.e6 / weight(ispc) / 24.! emission unit: mol/(km^2 hour)
	 endif
	 
	 print*, 'writing BB specie= ',trim(spc_name(ispc)), maxval(dummy), minval(dummy)

	 write(91) dummy*fx
	 iv=iv+1
	 ifound=1
	 !print*,'spc=',iv,trim(spc_name(ispc))
        exit
     endif

  enddo


  if(ifound == 0) then
     !print*,'not found, zero emission ',spc_name_wrf(ispc_wrf)
     dummy(:,:) =0.
     write(91) dummy
     iv=iv+1
     ifound=1
  endif

enddo
if(use_bbem==1) then
  do iveg=1,nveg_agreg
   	 print*, 'writing BB flam fracion for veg= ',iveg,&
	          maxval( plume_mean_fct(:,:,iveg)),minval( plume_mean_fct(:,:,iveg))
         write(91) plume_mean_fct(:,:,iveg)
	 iv=iv+1
	 !print*,'iveg plume_fct', iveg,maxval(plume_mean_fct(:,:,iveg)),iv
  enddo
  do iveg=1,nveg_agreg
   	 print*, 'writing BB fire size for veg= ',iveg,&
	          maxval( plume_firesize(:,:,iveg)),minval(plume_firesize (:,:,iveg))
         write(91) plume_firesize(:,:,iveg)
	 iv=iv+1
	 !print*,'iveg plume_firesize', iveg,maxval(plume_firesize(:,:,iveg)),iv
  enddo
elseif(use_bbem==2) then
  do i=1,5
    if(i==1)cdummy="flam_frac"
    if(i==2)cdummy="mean_frp "
    if(i==3)cdummy="std_frp  "
    if(i==4)cdummy="mean_size"
    if(i==5)cdummy="std_size "
    print*,"writing BB fields for FRE: ",cdummy(1:len(cdummy)),&
  	    maxval( plume_fre(:,:,i)),minval(plume_fre(:,:,i))
    write(91) plume_fre(:,:,i)
    iv=iv+1
  enddo
endif

close (91)
print*,"-------------------------------------------------------------"
print*,'Biomass burning emission + plumerise data for WRF-Chem are in the file: ',fileBB

!-------------------------- volcanoes -------------------------

if(use_volcanoes == 1) then
   print*,'=> ------------------------------------------------------'
   print*,'=> Volcanoes Eruption Section'
   ename='volc-eruption'
   fileBB=file_in(1:len_trim(file_in)-4)//'-volc.bin'
   open (91,file=fileBB,form='unformatted')
     write(91) 5 !fields
     write(91)ename
     write(91) trim(begin_eruption)
     write(91) ash_size_distr(:)
     print*,'ash_size_distr=',ash_size_distr(:),trim(begin_eruption)

     iv=0
     do ispc_wrf=1,nspecies_wrf
      do ispc = 1,nspecies
        if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
         if(trim(spc_name_wrf(ispc_wrf)) == 'ASH' .or. &
            trim(spc_name_wrf(ispc_wrf)) == 'SO2'      ) then
	 dummy=0.
	 print*,' specie=',trim(spc_name(ispc))
	 
         do j=1,n3 ; do i=1,n2
          if( volc_prop(i,j,3) > 0.001) then ! time duration

	   if( trim(spc_name_wrf(ispc_wrf)) == 'SO2') then
            fx= 1.e+3 * 1.e6 / weight(ispc) *3600.
            dummy(i,j) = var2d(i,j,ispc,geoge)*fx  / volc_prop(i,j,3) !  mol/(km^2 hour)

           else

            dummy(i,j) = var2d(i,j,ispc,geoge)*1.e+9  / volc_prop(i,j,3) !  ug/(m^2 sec)
           endif
          endif

         enddo;enddo
	 write(91) dummy
	 print*,'max = ',maxval(dummy), trim(spc_name_wrf(ispc_wrf))
       endif;endif
     enddo;enddo
     ! inj height
     write(91) volc_prop(:,:,1)
     ! elvation
     write(91) volc_prop(:,:,2)
     ! duration
     write(91) volc_prop(:,:,3)

   close (91)
print*,'volcanic SO2 and ASH emission, height,elevation and duration   data for WRF: ',fileBB
print*,'volcanic begin time=',begin_eruption
print*,'Volc ASH size distribution (fraction)'
print*, ash_size_distr(:)

endif
!---------------------------------------------------------------------------

if(use_degass_volcanoes == 1) then
   ename='volc-degass'
   begin_eruption= "NOT-DEFINED"  ! dummy
   fileBB=file_in(1:len_trim(file_in)-4)//'-volc.bin'
   open (91,file=fileBB,form='unformatted')
     write(91) 3 !fields
     write(91)ename
     write(91)begin_eruption
     iv=0
     do ispc_wrf=1,nspecies_wrf
      do ispc = 1,nspecies
        if(trim(spc_name_wrf(ispc_wrf)) == trim(spc_name(ispc))) then
         if(trim(spc_name_wrf(ispc_wrf)) == 'SO2') then
	 dummy=0.
	 print*,' specie=',trim(spc_name(ispc))
	 do j=1,n3 ; do i=1,n2
          if( volc_degass_prop(i,j,2) > 0.001) &! time duration
	  
          dummy(i,j) = var2d(i,j,ispc,geoge) / volc_degass_prop(i,j,2) !  kg/(m^2 sec)
         enddo;enddo
	 write(91) dummy
       endif;endif
     enddo;enddo
     ! inj height
     write(91) volc_degass_prop(:,:,1)
     ! elevation
     write(91) volc_degass_prop(:,:,2)
   close (91)
print*,'volcanic degassing SO2 emission, height and elevation data for wrf: ',fileBB
endif
!---------------------------------------------------------------------------

!print*,' The files *.ctl, *.gra and *.vfm are only auxiliary files'
!print*,' and, actually, are not used by WRF model.'


!----------- GOCART background data --------------
if(use_gocart_bg == 1) then

print*,'=> ------------------------------------------------------'
print*,'=> Gocart Background  Section'


fileBG=file_in(1:len_trim(file_in)-4)//'-gocartBG.bin'
open (91,file=fileBG,form='unformatted')
 do ispc=1,gocart_bg_nspecies
   print*,'writing spc = ', trim(gocart_bg_spc_name(ispc)),n2,n3,nlevels_netcdf(ispc)

   !-DMS
   if(trim(gocart_bg_spc_name(ispc))=='DMS') then
       print*,'DMS for month = ',imon
       print*,'gocart_bg_spc_name ', trim(gocart_bg_spc_name(ispc))
       write(91) gocart_bg_g(ispc)%src(1:n2,1:n3,imon)
       cycle
   endif

   !special section of writing 'lev'
   if(trim(gocart_bg_spc_name(ispc))=='H2O2') then
       write(91) lev(1:nlevels_netcdf(ispc))
   endif


   write(91) gocart_bg_g(ispc)%src(1:n2,1:n3,1:nlevels_netcdf(ispc))

 enddo
close (91)

endif


end subroutine write_to_WRFCHEM
!******************  *******************  ******************************
      SUBROUTINE PINPOL(NN,X,Y,XPOINT,YPOINT,MINDST)
! SUBPROGRAM PINPOL
! CHECK IF POINT IS INSIDE A GENERAL POLYGON
! Reference: "A point-in-polygon program", S.W. Sloan, Advances in
! Engineering Software, 1985, Vol. 7, No. 1, pages 45-47
!
! INPUT PARAMETERS:
! 'N' I IS THE NUMBER OF SIDES/VERTICES DEFINING THE POLYGON
! 'SMALLD' IS A SMALL DOUBLE PRECISION NUMBER
! 'LARGED' IS A LARGE DOUBLE PRECISION NUMBER
! 'X' IS A VECTOR OF NODAL X--COORDS (ANTICLOCKWISE ORDER)
! 'Y' IS A VECTOR OF NODAL Y--COORDS (ANTICLOCKWISE ORDER)
! BOTH OF THESE VECTORS MUST BE OF LENGTH N+2 WHERE
! X( N+1 )=X( 1 ), X( N+2 )=X( 2 )
! Y( N+1 )=Y( 1 ), Y( N+2 )=Y( 2 )
! 'XPOINT' IS THE X-COORD OF THE POINT TO BE TESTED
! 'YPOINT' IS THE Y-COORD OF THE POINT TO BE TESTED
! 
! OUTPUT PARAMETERS:
!
! 'MINDST'THE DISTANCE FROM THE POINT TO THE NEAREST POINT
! ON THE POLYGON
! IF 'MINDST' IS LT ZERO THEN POINT IS OUTSIDE THE POLYGON
! IF 'MINDST' IS EQ ZERO THEN POINT IS ON A SIDE OF THE POLYGON
! IF 'MINDST' IS GT ZERO THEN POINT IS INSIDE THE POLYGON
!
! NOTES:
!
! THIS IS AN IMPROVED VERSION OF THE ALGORTIHM OF NORDBECK AND RYSTEDT
!23456789012345678901234567890123456789012345678901234567890123456789012
!
!     INTEGER  :: N,I,J
      INTEGER  :: NN
      DOUBLE PRECISION  :: X(*),Y(*),XPOINT,YPOINT,SMALLD,LARGED,D,AREA,DET, &
      MINDST, X1, Y1, X1P, Y1P, X21, Y21, T, DX, DY, C00000, C00001
      LOGICAL  :: SNEAR
!
      PARAMETER( C00000=0.0D0, C00001=1.0D0)
      PARAMETER( SMALLD=1.0D-33, LARGED=1.0D33)
!
! 'SNEAR' IS .TRUE. IF DISTANCE TO NEAREST SIDE IS LESS THAN
! DISTANCE TO NEAREST VERTEX
! 'SNEAR' IS .FALSE. IF DISTANCE TO NEAREST VERTEX IS LESS THAN
! DISTANCE TO NEAREST SIDE
! 'MINDST' IS SQUARE OP DISTANCE TO CLOSEST POINT ON THE POLYGON
!
      MINDST=LARGED
! LOOP OVER EACH SIDE DEFINING POLYGON
!
      DO 10 I=1,NN
!
! START OF SIDE HAS COORDS (X1, Y1 )
! END OF SIDE HAS COORDS (X2, 12 )
! POINT HAS COORDS (XPOINT, YPOINT)
      X1=X( I)
      Y1=Y( I)
      X21=X( I+1 )-X1
      Y21=Y( I+1 )-Y1
      X1P=X1-XPOINT
      Y1P=Y1-YPOINT
! POINTS ON INFINITE LINE DEFINED BY
! X=X1+T*( X1-X2)
! Y=Y1+T*(Y1-Y2 )
! WHERE
! T=O AT (X1,Y1)
! T=1 AT (X2, Y2)
! FIND WHERE NORMAL PASSING THROUGH (XPOINT, YPOINT)
! INTERSECTS INFINITE LINE
      T=-(X1P*X21+Y1P*Y21 )/(X21*X21+Y21*Y21)
!     WRITE(*,*)'I,X1,Y1,XPNT,YPNT,T=',I,X1,Y1,XPOINT,YPOINT,T
!     CALL FLUSH(6)
      IF(T.LT.C00000)THEN
! NORMAL DOES NOT INTERSECT SIDE
! POINT IS CLOSEST TO VERTEX (X1, Y1 )
! COMPUTE SQUARE OF DISTANCE TO THIS VERTEX
      D=X1P*X1P+Y1P*Y1P
      IF(D.LT.MINDST)THEN
! POINT IS CLOSER TO (X1, Y1) THAN ANY OTHER VERTEX OR
! SIDE
!
      SNEAR= .FALSE.
      MINDST=D
      J=I
      END IF
      ELSE IF(T.LE.C00001)THEN
!
! NORMAL INTERSECTS SIDE
!
      DX=X1P+T*X21
      DY=Y1P+T*Y21
      D=DX*DX+DY*DY
      IF(D.LT.MINDST)THEN
! POINT IS CLOSER TO THIS SIDE THAN TO ANY
! OTHER SIDE OR VERTEX
      SNEAR=.TRUE.
      MINDST=D
      J=I
      END IF
      END IF
!     WRITE(*,*)'SNEAR,D,MINDST=',SNEAR,D,MINDST
 10   CONTINUE
!     CALL FLUSH(6)
!     ISTOP=1
!     IF(ISTOP.EQ.1)STOP999
      MINDST=SQRT( MINDST )
      IF(MINDST.LT.SMALLD)THEN
!
! POINT IS ON SIDE OF POLYGON
!
      MINDST=C00000
      ELSE
      IF( SNEAR)THEN
!
! POINT IS CLOSER TO ITS NEAREST SIDE THAN TO ITS NEAREST
! VERTEX, CHECK IF POINT IS TO LEFT OR RIGHT OF THIS SIDE
! IF POINT IS TO LEFT OF SIDE IT IS INSIDE POLYGON, ELSE
! POINT IS OUTSIDE POLYGON
!
      AREA=DET(X( J),X( J+1 ),XPOINT, Y(J), Y( J+1), YPOINT)
!     WRITE(*,*)'J,MINDST,AREA=',J,MINDST,AREA
!     WRITE(*,*)'X( J),X( J+1 ),XPOINT=',X( J),X( J+1 ),XPOINT
!     WRITE(*,*)'Y( J),Y( J+1 ),YPOINT=',Y( J),Y( J+1 ),YPOINT
!     CALL FLUSH(6)
      MINDST=SIGN( MINDST ,AREA)
      ELSE
!
! POINT IS CLOSER TO ITS NEAREST VERTEX THAN ITS NEAREST SIDE,
! CHECK IF NEAREST VERTEX IS CONCAVE
! IF THE NEAREST VERTEX IS CONCAVE THEN POINT IS INSIDE THE
! POLYGON, ELSE THE POINT IS OUTSIDE THE POLYGON
      IF( J .EQ.1)THEN
      J=NN+1
      END IF
      AREA=DET( X(J+1 ),X( J),X( J-1), Y( J+1), Y( J), Y(J-1))
!     WRITE(*,*)'J,MINDST,AREA=',J,MINDST,AREA
!     WRITE(*,*)'X( J),X( J+1 ),X(J-1)=',X( J),X( J+1 ),X(J-1)
!     WRITE(*,*)'Y( J),Y( J+1 ),Y(J-1)=',Y( J),Y( J+1 ),Y(J-1)
!     CALL FLUSH(6)
      MINDST=SIGN( MINDST ,AREA)
      END IF
      END IF
      END SUBROUTINE PINPOL
!******************  *******************  ******************************
      FUNCTION DET(X1,X2,X3,Y1,Y2,Y3)
!
! COMPUTE TWICE THE AREA OF THE TRIANGLE DEFINIED BY THREE POINTS
! WITH COORDS (X1, Y1 ), (X2, Y2) AND (X3, Y3) USING DETERMINANT
! FORMULA
! INPUT PARAMETERS:
!  'X1,Y1' COORDS OF POINT 1
!  'X2,Y2' COORDS OF POINT 2
!  'X3,Y3' COORDS OF POINT 3
! OUTPUT PARAMETERS,
! 'DET' TWICE THE AREA OF THE TRIANGLE DEFINED BY THE THREE POINTS
! NOTES:
! DET IS POSITIVE IF POINTS 1,2 AND 3 DEFINE TRIANGLE IN
! ANTICLOCKWISE ORDER
! DET IS NEGATIVE IF POINTS 1,2 AND 3 DEFINE TRIANGLE IN
! CLOCKWISE ORDER
! DET IS ZERO IF AT LEAST TWO OF THE POINTS ARE COINCIDENT OR IF
! ALL THREE POINTS ARE COLLINEAR
!
!
      DOUBLE PRECISION  :: X1, X2, X3, Y1, Y2, Y3, DET
!
      DET=(X1-X3 )*(Y2-Y3 )-(X2-X3 )*(Y1-Y3)
!
      END FUNCTION DET
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!---------------------------------------------------------

#endif

