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
allocate(plume_fre     (nxp,nyp,6));plume_fre     =0.

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
!KY      print*,read_spc_name,read_src_name,ispc,isrc
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

	   elseif(TRIM(read_mean) == 'mean_fsize' ) then
    	   call vfirec(iunit,plume_fre(:,:,4),nxp*nyp,'LIN')
	   where( plume_fre(:,:,4) <0.) plume_fre(:,:,4)=0.
	   
	   elseif(TRIM(read_mean) == 'std_fsize' ) then
    	   call vfirec(iunit,plume_fre(:,:,5),nxp*nyp,'LIN')
	   where( plume_fre(:,:,5) <0.) plume_fre(:,:,5)=0.
           else
	    stop "unknown read_mean parameter at plumefre section"
	   endif
      
       endif

    enddo
100 continue
!- split bburn emissions into flaming/smoldering parts
 call emis_flam_smold_WRF(nxp,nyp,nspecies,nsrc,var2d,plume_mean_fct&
                          ,plume_firesize,plume_fre)


!- convert to WRF Format 
    print*,"==> processing emission files for WRF-Chem model" 
    call write_to_WRFCHEM(nxp,nyp,nspecies,nsrc,var2d,plume_mean_fct&
           ,plume_firesize,volc_prop,file_in,iday,imon,iyear,volc_degass_prop&
	   ,ash_size_distr_dummy,plume_fre)


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
!---------------------------------------------------------
subroutine write_to_WRFCHEM(n2,n3,nspecies,nsrc,var2d,plume_mean_fct &
                           ,plume_firesize,volc_prop,file_in,iday,imon,iyear&
			   ,volc_degass_prop,ash_size_distr,plume_fre)
    use grid_dims_out
    use chem1_list, only : spc_name, weight, BBURN2,BBURN3,OC,BC,URBAN2,URBAN3,SMOKE
    implicit none
    integer,intent(IN) :: n2,n3,nspecies,nsrc
    real,dimension(n2,n3) :: smold_frac
    real,dimension(n2,n3,4) :: plume_mean_fct,plume_firesize
    real,dimension(n2,n3,6) :: plume_fre
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

!KY    integer, parameter :: nspecies_wrf = 29
    integer, parameter :: nspecies_wrf = 7

    integer ispc,isrc,ispc_wrf,iv,iveg,ifound,iday,imon,iyear,i,j
    character(len=20) ename
    character(len=10) cdummy
    real ash_size_distr(10)
    integer itime
    real fx
!from     /home/poluicao/WRFV2-CHEM/chem/module_input_chem_data.F
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies_wrf) :: spc_name_wrf=(/ &
 'NO      ' &!
,'CO      ' &!
,'BBURN2  ' &!
,'BBURN3  ' &!
,'OC      ' &!
,'BC      ' &!
,'SMOKE   ' &!
!KY  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies_wrf) :: spc_name_wrf=(/ &
 !KY'SO2     ' &!
!KY,'NO2     ' &!
!KY,'NO      ' &!
!KY,'ALD     ' &!
!KY,'HCHO    ' &!
!KY,'ORA2    ' &!
!KY,'NH3     ' &! XXXXX
!KY,'HC3     ' &!
!KY,'HC5     ' &!
!KY,'HC8     ' &!
!KY,'ETH     ' &!
!KY,'CO      ' &!
!KY,'OL2     ' &! XXXXX
!KY,'OLT     ' &!
!KY,'OLI     ' &!
!KY,'TOL     ' &!
!KY,'XYL     ' &!
!KY,'KET     ' &!
!KY,'CSL     ' &!
!KY,'ISO     ' &!
!KY,'BBURN2  ' &!
!KY,'BBURN3  ' &!)
!KY,'URBAN2  '&
!KY,'URBAN3  '&
!KY,'OC      ' &!
!KY,'BC      ' &
!KY,'DMS     ' &
!KY,'SO4     ' &! SO4
!KY,'ASH     ' &

/)

  ! EMK NUWRF...Extra emissions for WRF-Chem not included in community code.
  !integer,parameter :: nspecies_wrf_extra = 0
  integer,parameter :: nspecies_wrf_extra = 6
  CHARACTER(LEN=8),PARAMETER,DIMENSION(nspecies_wrf_extra) :: &
       spc_name_wrf_extra=(/ &
 'ETE     ' &
,'CH4     ' &
,'CO2     ' &
,'API     ' &
,'LIM     ' &
,'ORA1    ' &
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
!KYprint*,'=> anthro + biogenic section'

ename='XXXXXXXX'
itime = 0

!- bburn + plumerise section
print*,'=> bburn + plumerise section'

fileBB=file_in(1:len_trim(file_in)-4)//'-bb.bin'
open (91,file=fileBB,form='unformatted')
! NUWRF EMK...Add extra species
     write(91)nspecies_wrf + 8 + nspecies_wrf_extra
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
	    trim(spc_name_wrf(ispc_wrf)) == 'SMOKE'   .or. &
	    trim(spc_name_wrf(ispc_wrf)) == 'BC'  ) then
	    fx= 1.e+9  / 86400.	 !  ug/(m^2 sec)
	 else
	    fx= 1.e+3 * 1.e6 / weight(ispc) / 24.! emission unit: mol/(km^2 hour)
	 endif
	 
	 !-SMOKE        

	 if(trim(spc_name_wrf(ispc_wrf)) == 'SMOKE' ) then

	 dummy(:,:) = var2d(:,:,BBURN2,bburn)  ! BBURN2 = PM2.5 + OC +BC

	 print*,'bburn2= ',trim(spc_name_wrf(3)), maxval(var2d(:,:,BBURN2,bburn)),minval(var2d(:,:,BBURN2,bburn))
	 print*, 'oc   = ',trim(spc_name_wrf(5)), maxval(var2d(:,:,OC  ,bburn)), minval(var2d(:,:,OC,bburn))
	 print*, 'bc   = ',trim(spc_name_wrf(6)), maxval(var2d(:,:,BC  ,bburn)), minval(var2d(:,:,BC,bburn))
	 print*, 'smoke= ',trim(spc_name_wrf(ispc_wrf)), maxval(dummy), minval(dummy)
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
     print*,'not found, zero emission, no binary write ',spc_name_wrf(ispc_wrf)
     dummy(:,:) =0.
!    write(91) dummy
     iv=iv+1
     ifound=1
  endif

enddo
  if(use_bbem==1) then
! if(use_bbem==1 .or. use_bbem==2) then
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
!KY   surface smoke

     plume_fre(:,:,6)= (var2d(:,:,BBURN2,bburn))*1.e+9  / 86400.

        write(*,*) 'max(plume_fre(:,:,6)): ',maxval(plume_fre(:,:,6))
        write(*,*) 'min(plume_fre(:,:,6)): ',minval(plume_fre(:,:,6))

    do i=1,6
      if(i==1)cdummy="flam_frac"
      if(i==2)cdummy="mean_frp "
      if(i==3)cdummy="std_frp  "
      if(i==4)cdummy="mean_fsize"
      if(i==5)cdummy="std_fsize "
      if(i==6)cdummy="ebb_smoke"
      print*,"writing BB fields for FRE: ",cdummy(1:len(cdummy)),&
    	    maxval( plume_fre(:,:,i)),minval(plume_fre(:,:,i))
      write(91) plume_fre(:,:,i)
      iv=iv+1
    enddo
endif

! NUWRF EMK...Add extra GFEDV3 species
! SAM 3/5/18 Tack extra species onto end of binary file - convert_emiss needs modification to accomodate
!KYdo ispc_wrf=1,nspecies_wrf_extra
!KY   ifound=0
!KY   do ispc = 1,nspecies
!KY      if(trim(spc_name_wrf_extra(ispc_wrf)) == trim(spc_name(ispc))) then
!KY         dummy(:,:) = var2d(:,:,ispc,bburn)
!KY         print*, 'writing BB specie= ',trim(spc_name(ispc)), maxval(dummy), minval(dummy)
!KY         ! NUWRF EMK...Change DM units from kg/m^2/day to kg/km^2/hour. We don't
!KY         ! have a molecular weight for this category.
!KY         if (trim(spc_name_wrf_extra(ispc_wrf)) == 'DM') then
!KY            fx = 1.e6 / 24.
!KY         else
!KY            fx= 1.e+3 * 1.e6 / weight(ispc) / 24.! emission unit: mol/(km^2 hour)
!KY         end if
!KY         write(91) dummy*fx
!KY         iv=iv+1
!KY         ifound=1
!KY         exit
!KY      endif
!KYenddo
!KY   if(ifound == 0) then
!KY     print*,'not found, zero emission, no binary write ',spc_name_wrf_extra(ispc_wrf)
!KY!     write(91) dummy
!KY      iv=iv+1
!KY      ifound=1
!KY   endif
!KYenddo
!KY! NUWRF EMK END...Add extra GFEDV3 species

close (91)
print*,"-------------------------------------------------------------"
print*,'Biomass burning emission + plumerise data for WRF-Chem are in the file: ',fileBB

!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
end subroutine write_to_WRFCHEM
!---------------------------------------------------------

#endif

