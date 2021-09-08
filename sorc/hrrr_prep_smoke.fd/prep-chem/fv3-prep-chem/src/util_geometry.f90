!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

MODULE util_geometry

IMPLICIT NONE

 TYPE pts_city

	REAL*4  :: y, x
	REAL*4  :: ang
	
 END TYPE pts_city

 TYPE T_mobileSrc
 
 	character(len=60)    		  :: cityName
  	character(len=60)  		  :: region
  	real		     		  :: area
  	real		     		  :: urbanArea
	real				  :: COEmission
	real				  :: NOXEmission
	real				  :: NMVOCEmission
  	real, dimension(2) 		  :: lowerBorder
  	real, dimension(2) 		  :: upperBorder
 	integer	     		          :: nUrbanVertexs
  	real, dimension(:), pointer   :: urbanXVertex
  	real, dimension(:), pointer   :: urbanYVertex

 END TYPE T_mobileSrc

TYPE(pts_city), ALLOCATABLE, DIMENSION(:) :: p_city
TYPE(pts_city)                            :: p_local
INTEGER                                   :: qte_p_city
INTEGER                                   :: max_x, max_y, min_x, min_y

CONTAINS

!****************************************************************************

SUBROUTINE PN_POLY(pol, qte, pt, ret)

 IMPLICIT NONE

 TYPE(pts_city), DIMENSION(:), INTENT(IN)  :: pol 
 TYPE(pts_city), INTENT(INOUT) :: pt
 INTEGER, INTENT(IN)         :: qte
 INTEGER, INTENT(OUT)        :: ret

 INTEGER :: i, cn
 REAL    :: vt

 cn = 0
 ret = 0
 
 DO i = 1, qte
 	IF( (pol(i)%y .LE. pt%y) .AND. (pol(i+1)%y .GT. pt%y) .OR. &
	    (pol(i)%y .GT. pt%y) .AND. (pol(i+1)%y .LE. pt%y))THEN
	    	vt = (pt%y - pol(i)%y)/(pol(i+1)%y - pol(i)%y)
		IF(pt%x .LT. (pol(i)%x + vt * (pol(i+1)%x - pol(i)%x))) cn = cn	+ 1
	ENDIF
 END DO
	   	
 ret = IAND(cn, 1)

 RETURN

END SUBROUTINE PN_POLY

!****************************************************************************
SUBROUTINE SET_ANCOR(pol, qte, max_x, max_y, min_x, min_y)

 IMPLICIT NONE
 
 TYPE(pts_city), DIMENSION(:), INTENT(INOUT) :: pol
 INTEGER, INTENT(INOUT)                      :: max_x, max_y, min_x, min_y
 INTEGER, INTENT(IN)           		     :: qte
 
 TYPE(pts_city) :: ancora
 INTEGER        :: i, j 
 
 ancora = pol(1)
 
 max_x = ancora%x
 min_x = ancora%x
 
 max_y = ancora%y
 min_y = ancora%y 
  
 DO i = 2, qte
 	IF(ancora%y .GT. pol(i)%y)THEN
		ancora = pol(i)
	ELSE
		IF(ancora%y .EQ. pol(i)%y)THEN
			IF(ancora%x .EQ. pol(i)%x) ancora = pol(i)
		END IF
	END IF
	
	IF(min_y .GT. pol(i)%y) min_y = pol(i)%y 
 	IF(max_y .LT. pol(i)%y) max_y = pol(i)%y
	IF(min_x .GT. pol(i)%x) min_x = pol(i)%x	
	IF(max_x .LT. pol(i)%x) max_x = pol(i)%x 
 END DO

 DO i = 1, qte
 	CALL CALC_THETA(ancora, pol(i))
 END DO

 pol(qte+1) = pol(1)

END SUBROUTINE SET_ANCOR

!****************************************************************************

SUBROUTINE CALC_THETA(P0, P1)

 IMPLICIT NONE

 TYPE(pts_city), INTENT(IN)  :: P0
 TYPE(pts_city), INTENT(INOUT) :: P1

 REAL*4 :: dx, dy, ax, ay, t

 dx = P1%x - P0%x
 dy = P1%y - P0%y

 ax = ABS(dx)
 ay = ABS(dy)

 IF(ax+ay == 0)THEN
         t = 0
 ELSE
         t = dy/(ax+ay)
 END IF

 IF(dx .LT. 0)THEN
         t = t -2
 ELSE
         t = t + 4
 END IF

 P1%ang = t * 90.

END SUBROUTINE CALC_THETA


SUBROUTINE loadMobileSrcMap(fileName, iMobile, voc_flag)
 
  !# parameter
  character(len=*),                         intent(in)  :: fileName
  type(T_mobileSrc), pointer, dimension(:) :: iMobile
  
  !# local
  integer :: nCities
  integer :: idx0
  integer :: idx1
  character :: dummyChar
  integer, optional :: voc_flag
  
  	open(222, file=fileName, status='old')
	read(222,*)nCities
	
	allocate(iMobile(nCities))
	
	do idx0 = 1, nCities
		read(222,*) iMobile(idx0)%cityName, dummyChar, iMobile(idx0)%region
		read(222,*) iMobile(idx0)%area, iMobile(idx0)%urbanArea
                if (present(voc_flag)) then
			read(222,*) iMobile(idx0)%COEmission, iMobile(idx0)%NOXEmission, iMobile(idx0)%NMVOCEmission
                else
			read(222,*) iMobile(idx0)%COEmission, iMobile(idx0)%NOXEmission
		end if
 		read(222,*) iMobile(idx0)%lowerBorder, iMobile(idx0)%upperBorder
		read(222,*) iMobile(idx0)%nUrbanVertexs
		
		print*, 'processing city: ', idx0, trim(iMobile(idx0)%cityName)
		
		allocate(iMobile(idx0)%urbanXVertex(iMobile(idx0)%nUrbanVertexs))
		allocate(iMobile(idx0)%urbanYVertex(iMobile(idx0)%nUrbanVertexs))
		
		do idx1 = 1, iMobile(idx0)%nUrbanVertexs
			read(222,*)iMobile(idx0)%urbanXVertex(idx1), iMobile(idx0)%urbanYVertex(idx1)
		end do
	
	end do
	
	close(222)
 
END SUBROUTINE loadMobileSrcMap

SUBROUTINE polyToModel(iMobileSrc,ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat,grid_type, iCO, iNOX, iNMVOC)
 
 !# parameters
 type(T_mobileSrc), pointer, dimension(:)  :: iMobileSrc
 character(len=*),                         intent(in)  :: grid_type
 integer,                                  intent(in)  :: ng
 integer,                                  intent(in)  :: n1
 integer,                                  intent(in)  :: n2
 real, 					   intent(in)  :: deltax
 real, 					   intent(in)  :: deltay
 real, 					   intent(in)  :: plat
 real, 					   intent(in)  :: plon
 real, 					   intent(in)  :: xt(n1)
 real, 					   intent(in)  :: yt(n2)
 real, dimension(n1,n2), 		   intent(in)  :: rlon
 real, dimension(n1,n2), 		   intent(in)  :: rlat
 real, dimension(n1,n2),                   intent(out) :: iCO
 real, dimension(n1,n2),                   intent(out) :: iNOX
 real, dimension(n1,n2), optional,         intent(out) :: iNMVOC

 !# local
! real, parameter 		      :: resolX = 0.0083
! real, parameter 		      :: resolY = 0.0083
 real, parameter 		      :: resolX = 0.00021
 real, parameter 		      :: resolY = 0.00021
 integer 			      :: idx0
 integer 			      :: localX
 integer 			      :: localY
 integer 			      :: localNX
 integer 			      :: localNY
 integer 			      :: isInside
 integer 			      :: lat
 integer 			      :: lon
 integer 			      :: totalPixels
 real   			      :: localILat
 real    			      :: localILon
 real    			      :: sumCO
 real    			      :: sumNOX
 real    			      :: sumNMVOC 
 integer, allocatable, dimension(:,:) :: localGrid
 
 
 
 	allocate(localGrid(n1,n2))
	
	localGrid = 0.
 
 	do idx0 = 1, size(iMobileSrc)
	
		localILat = iMobileSrc(idx0)%upperBorder(2)
		localILon = iMobileSrc(idx0)%upperBorder(1)
		
		call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat, &
                          		      localILon,localILat,localX,localY,'none',grid_type)
		
		localILat = iMobileSrc(idx0)%lowerBorder(2)
		localILon = iMobileSrc(idx0)%lowerBorder(1)
		
		call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat, &
                          		      localILon,localILat,localX,localY,'none',grid_type)
		if(localX .eq. -999 .or. localY .eq. -999)then
		print *, 'pulei2', trim(iMobileSrc(idx0)%cityName)
		cycle
		end if
		
		localNX = abs(ceiling( (iMobileSrc(idx0)%upperBorder(1)-iMobileSrc(idx0)%lowerBorder(1))/resolX ))
		localNY = abs(ceiling( (iMobileSrc(idx0)%upperBorder(2)-iMobileSrc(idx0)%lowerBorder(2))/resolY ))
		
		totalPixels = 0
		localGrid   = 0.
		
		print*, 'city: ', idx0, trim(iMobileSrc(idx0)%cityName), ' - ', trim(iMobileSrc(idx0)%region)
		
		do lat = 1, localNY
			do lon = 1, localNX
				
				call insidePolygon(iMobileSrc(idx0)%urbanXVertex, iMobileSrc(idx0)%urbanYVertex, localILon, localILat, isInside)
				
				if(isInside .ne. 0)then
				
					call update_emissions_by_city(ng,n1,n2,xt,yt,deltax,deltay,plat,plon,rlon,rlat, &
                          					      localILon,localILat,localX,localY,'none',grid_type)
								
					if(localX .ne. -999 .and. localY .ne. -999)then
						
						localGrid(localX,localY) = 1 + localGrid(localX,localY)
				
						totalPixels = 1 + totalPixels
					end if
				
				end if
				
				localILon = localILon + resolX
			end do
			localILon = iMobileSrc(idx0)%lowerBorder(1)
			localILat = localILat + resolY
		end do
		
		print*, 'total pixels: ', totalPixels
		if(totalPixels .lt. 1) cycle
		
		sumCO  = 0.
		sumNOX = 0.
		sumNMVOC = 0.
		
		do lat = 1, n2
			do lon = 1, n1
				if(localGrid(lon,lat) .gt. 0.)then
					
					iCO(lon, lat)   = ( iMobileSrc(idx0)%COEmission * (localGrid(lon,lat)/(totalPixels*1.)) ) + iCO(lon, lat)
!					iCO(lon, lat)   = ( iMobileSrc(idx0)%COEmission * (localGrid(lon,lat)/(totalPixels -1.)) ) + iCO(lon, lat)
				sumCO  = iCO(lon, lat) + sumCO
					
					if(iCO(lon,lat) .lt. 0.)then
						print*, iMobileSrc(idx0)%COEmission, localGrid(lon,lat)
						stop  'emissao < 0'
					end if
					
					iNOX(lon, lat)  = ( (iMobileSrc(idx0)%NOXEmission)  * ((localGrid(lon,lat))/(totalPixels*1.)) ) + iNOX(lon, lat)
					sumNOX = iNOX(lon,lat) + sumNOX

                           if (present(iNMVOC)) then
					iNMVOC(lon, lat)  = ( (iMobileSrc(idx0)%NMVOCEmission)  * ((localGrid(lon,lat))/(totalPixels*1.)) ) + iNMVOC(lon, lat)
					sumNMVOC = iNMVOC(lon,lat) + sumNMVOC
                           end if
					
				end if
			
			
			end do
		end do
		
		
		! a soma das emissoes anteriores inviabiliza a conservacao de massa.
		
		!if( sumCO .ne. iMobileSrc(idx0)%COEmission) then
		!	print*, 'checking mass conservation>>> ',sumCO, iMobileSrc(idx0)%COEmission
		!else
		!	print*, 'checking mass conservation: ',sumCO, iMobileSrc(idx0)%COEmission
		!end if
		
		! Multiplicar emissoes anteriores por fator de correcao da atual?
		!if(sumCO  .ne. iMobileSrc(idx0)%COEmission)then
		!	iCO = iCO * (iMobileSrc(idx0)%COEmission/sumCO)
		!end if
					
		!if(sumNOX  .ne. iMobileSrc(idx0)%NOXEmission)then
		!	iNOX = iNOX * (iMobileSrc(idx0)%emissionNOX/sumNOX)
		!end if
					
	end do
	
	deallocate(localGrid)
  
 
 END SUBROUTINE polyToModel
 
 SUBROUTINE insidePolygon(xVertex, yVertex, pX, pY, isInside)
 
  	!# parameters
	real, dimension(:), intent(in)  :: xVertex
	real, dimension(:), intent(in)  :: yVertex
	real,               intent(in)  :: pX
	real,               intent(in)  :: pY
	integer,            intent(out) :: isInside
	
	!# local
	integer :: idx0
	real    :: dY
	
	
		isInside = 0
	
		if(size(xVertex) .ne. size(yVertex)) stop 'fatal error: subroutine insidePolygon - vertexs arrays are not conformable'
		
		do idx0 = 1, size(xVertex)-1
		
			if( (yVertex(idx0) .le. pY .and. yVertex(idx0+1) .gt. pY) .or. &
			    (yVertex(idx0) .gt. pY .and. yVertex(idx0+1) .le. pY)        )then
			    
				dY = (pY - yVertex(idx0)) / (yVertex(idx0+1) - yVertex(idx0)) 
				
				if( pX .lt. ( xVertex(idx0) + dY * (xVertex(idx0+1) - xVertex(idx0)) ) ) isInside = 1 + isInside 	    
			    
			end if
		
		end do
		
		isInside = iand(isInside,1)
		
 END SUBROUTINE insidePolygon

!****************************************************************************

END MODULE util_geometry
