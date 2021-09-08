!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

module diurnal_cycle_fwb

contains 

subroutine cycle_fwb(ihour,rhour,iyear,imon,iday,ng,ngrids,n1,n2,n3,rlat,rlon,rland,deltax,deltay&
                            ,xt,yt,xm,ym,plat,plon)
use grid_dims_out, only: grid_type,FWBAWB_data_dir
use FWBAWB_emissions
use util_geometry

implicit none
integer, parameter ::  nlon = 720, nlat=360, nmonths=12 
integer, intent (in) :: iyear,imon,iday,ng,n1,n2,n3,ngrids,ihour
integer, intent (inout) :: rhour
real, intent (in), dimension(n1,n2)  :: rlat,rlon,rland
real, allocatable, dimension(:,:):: grid_area
real, intent (in) :: deltax,deltay
real,intent(in) ::  xt(n1), yt(n2),xm(n1), ym(n2)
integer :: ispc,i,j,icity,jcity,ipol,jpol
real plat,plon,peso,pesoS,pesoN,pesoV,rrlat,rrlon,peso2
CHARACTER (20) :: City_name
real SantiagoLON1,SantiagoLON2,SantiagoLAT1,SantiagoLAT2
integer ip11,ip12,ip21,ip22,jp11,jp12,jp21,jp22
INTEGER :: points,grid_count,ret,ipoly,jpoly
REAL :: x_grid_type(n1),y_grid_type(n2)

    
!-----------------------------CO--------------------------------
!			SEGUINDO CETESB
!---------------------------------------------------------------
     

  if (rhour.eq.-1) then
     rhour=23
  endif
  if (rhour.eq.-2) then
     rhour=22
  endif
  if (rhour.eq.-3) then
     rhour=21
  endif
  if (rhour.eq.-4) then
     rhour=20
  endif

PRINT*, rhour
    select case (rhour)
    case (0)
     peso= 0.018
     pesoN=0.027
    case (1)
     peso= 0.015
     pesoN=0.016
    case (2)
     peso= 0.020
     pesoN=0.019
    case (3)
     peso= 0.025
     pesoN=0.023
    case (4)
     peso= 0.031
     pesoN=0.027
    case (5)
     peso= 0.036
     pesoN=0.031
    case (6)
     peso= 0.041
     pesoN=0.035
    case (7)
     peso= 0.045
     pesoN=0.040
    case (8)
     peso= 0.048
     pesoN=0.044
    case (9)
     peso= 0.050
     pesoN=0.048
    case (10)
     peso= 0.050
     pesoN=0.051
    case (11)
     peso= 0.049
     pesoN=0.054
    case (12)
     peso= 0.047
     pesoN=0.056
    case (13)
     peso= 0.047
     pesoN=0.057
    case (14)
     peso= 0.048
     pesoN=0.057
    case (15) 
     peso= 0.050
     pesoN=0.057
    case (16)
     peso= 0.053
     pesoN=0.056
    case (17)
     peso= 0.056
     pesoN=0.054
    case (18)
     peso= 0.058
     pesoN=0.051
    case (19)
     peso= 0.056
     pesoN=0.048
    case (20)
     peso= 0.052
     pesoN=0.044
    case (21)
     peso= 0.045
     pesoN=0.040
    case (22)
     peso= 0.035
     pesoN=0.035
    case (23)
     peso= 0.026
     pesoN=0.031
    end select

PRINT*,'*****************************************************************'
PRINT*,'DIURNAL CYCLE - UTC TIME=',ihour,'LOCAL TIME=',rhour,'PESO=',peso
PRINT*,'*****************************************************************'
    

!---------------------------------CALCULO I,J DOS LIMITES URBANOS DE SANTIAGO-------------------------------!
 SantiagoLAT1=-33.1
 SantiagoLAT2=-33.7
 SantiagoLON1=-71
 SantiagoLON2=-70.3
 City_name='Santiago'
 call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                ,SantiagoLON1,SantiagoLAT1,ip11,jp11,City_name,grid_type)
 print*,ip11,jp11
 call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                ,SantiagoLON1,SantiagoLAT2,ip12,jp12,City_name,grid_type)
 print*,ip12,jp12
 call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                ,SantiagoLON2,SantiagoLAT1,ip21,jp21,City_name,grid_type)
 print*,ip21,jp21
 call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat&
                                ,SantiagoLON2,SantiagoLAT2,ip22,jp22,City_name,grid_type)
 print*,ip22,jp22
!-----------------------------------------------------------------------------------------------------------!
 


do i=1,n1
  do j=1,n2

do ispc = 1, nspecies


if (ispc.eq.13) then
FWBAWB_g(ispc)%src(i,j,1)=(FWBAWB_g(ispc)%src(i,j,1)*peso)/3600
endif
if (ispc.eq.18) then
FWBAWB_g(ispc)%src(i,j,1)=(FWBAWB_g(ispc)%src(i,j,1)*pesoN)/3600
endif
if (ispc.ne.13.and.ispc.ne.18) then
FWBAWB_g(ispc)%src(i,j,1)=(FWBAWB_g(ispc)%src(i,j,1)*peso)/3600
endif

enddo

  enddo
enddo 


end subroutine cycle_fwb
end module diurnal_cycle_fwb
