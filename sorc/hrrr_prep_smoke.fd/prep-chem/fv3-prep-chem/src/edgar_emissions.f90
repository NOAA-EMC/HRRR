!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module edgar_emissions
  !---------------------------------------------------------------------------
  integer, parameter :: maxnspecies = 200 , nspecies = 14

  integer, parameter ::                 &
          CO          = 1             &
       ,  NOX         = 2                &
       ,  CO2         = 3                &
       ,  CH4      = 4                &
       ,  SO2         = 5             &
       ,  N2O         = 6                     &
       ,  SF6      = 7                &
       ,  NMVOC    = 8                &
       ,  SO4         = 9                &
       ,  URBAN2           = 10               & !PM25
       ,  URBAN3      = 11               & !PM10
       ,  BC       = 12               & 
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

    write(0,*) 'ERROR: Cannot use edgar in this executable.'
    call c_abort
  end subroutine alloc_edgar

  !---------------------------------------------------------------

  subroutine nullify_edgar(edgar)

    implicit none

    type (edgar_vars),dimension(nspecies)  :: edgar
    integer ispc

    write(0,*) 'ERROR: Cannot use edgar in this executable.'
    call c_abort
  end subroutine nullify_edgar

end module edgar_emissions

!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine mem_edgar(n1,n2,n3)
  use edgar_emissions
  implicit none
  integer i
  integer, intent(in) :: n1,n2,n3

  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
end subroutine mem_edgar

!---------------------------------------------------------------
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
       use_edgar

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

  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort

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
rawsrc=0
  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
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
  implicit none
  !variaveis de entrada e saÃ­da
  integer, intent(in):: nlon,nlat,nspecies,nmonths
  character(len=*), intent(in)::spc_name(nspecies)
  real,intent(out):: RAWsrc(nlon,nlat,nmonths,nspecies)
  character(len=*), intent(in)::prefix,suffix,edgar_data_dir

  !variaveis do hdf5
  character(len=50) ::dsetname
!  integer(hid_t):: file_id, dset_id, group_id, dtype_id
!  integer(hsize_t) :: data_dims(2) 
  character(len=240)::filenameed
  integer :: error, printflags,ispc,ifontes
  real,dimension(:,:),allocatable::soma
  real,dimension(:,:,:),allocatable::somat
!  integer(hid_t)::dcpl
  integer,dimension(1:1)::cd_values
!  integer(size_t)::nelmts
  integer:: flags, filter_id, filter_info_both,filter_info
  integer, parameter:: fontes=3
!  integer(size_t),parameter:: MaxChrlen=80
!  character(len=MaxChrlen):: name
  integer, parameter :: STATIONARY_SOURCES = 1, &
       MOBILE_SOURCES = 2, &
       AVIATION_SOURCES = 3
  character(LEN=19),dimension(fontes), parameter :: &
       groupname=(/ &
       'STATIONARY_SOURCES', &
       'MOBILE_SOURCES    ', &
       'AVIATION_SOURCES  '/)
  integer :: imonx
rawsrc=0
  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
end subroutine edgar4

!---------------------------------------------------------------
subroutine  edgar_htap(nx,ny,nspecies,nmonths,spc_name,RAWsrc,prefix,suffix,edgar_data_dir)


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
  !     'BC',&
  !     'CO',&
  !     'NH3',&
  !     'NMVOC',&
  !     'NOx',&
  !     'OC',&
  !     'PM10',&
  !     'PM2.5',&
  !     'SO2'/)
  ! character(len=25), parameter :: PREFIX="EDGAR-HTAP_"
  ! character(len=25), parameter :: SUFIX="_2010.h5"
  ! character(len=255), parameter :: DIR="/scratchin/grupos/catt-brams/home/valter.oliveira/PROJETOS/MATRIX/src/dados/"


  character(len=255) :: filename
  logical :: file_exists
  !variaveis do hdf5
!  integer(HID_T) :: file_id
!  integer(HID_T) :: dset_id 
!  integer(HSIZE_T) :: data_dims(4)
  integer :: hdferr
  character(len=25) :: dsetname

  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
end subroutine  edgar_htap
!---------------------------------------------------------------
!---------------------------------------------------------------
subroutine get_edgar_indentity(spc_name,ident)
  !use chem1_list
  use edgar_emissions, only :  edgar_nspecies=>nspecies&
       ,edgar_spc_name=>spc_name&
       , edgar_CO     => CO       & ! 1
       , edgar_NOX    => NOX      & ! 2  
       , edgar_CO2    => CO2      &   
       , edgar_CH4    => CH4      &  
       , edgar_SO2    => SO2      & 
       , edgar_N2O    => N2O      &
       , edgar_SF6    => SF6      &
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
ident=0
  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
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
  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
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

tx=0
  write(0,*) 'ERROR: Cannot use edgar in this executable.'
  call c_abort
end subroutine grid_htap2fv3
!---------------------------------------------------------------
