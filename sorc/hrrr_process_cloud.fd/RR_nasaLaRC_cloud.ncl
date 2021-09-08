load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  nlon=648 
  nlat=647  
  nlon2=324 
  nlat2=323  

  lon = fbinrecread("./NASALaRC_cloud.bin",1,(/nlat,nlon/),"float")
  lat = fbinrecread("./NASALaRC_cloud.bin",2,(/nlat,nlon/),"float")
  pcld= fbinrecread("./NASALaRC_cloud.bin",4,(/nlat,nlon/),"float")
  tcld= fbinrecread("./NASALaRC_cloud.bin",5,(/nlat,nlon/),"float")
  frac= fbinrecread("./NASALaRC_cloud.bin",6,(/nlat,nlon/),"float")
  wlwp= fbinrecread("./NASALaRC_cloud.bin",7,(/nlat,nlon/),"float")

  cover = new ((/nlat2,nlon2/), typeof(pcld))
  cover2 = new ((/nlat2,nlon2/), typeof(pcld))
  cover3 = new ((/nlat2,nlon2/), typeof(pcld))
  cover4 = new ((/nlat2,nlon2/), typeof(pcld))
  clon = new ((/nlat2,nlon2/), typeof(pcld))
  clat = new ((/nlat2,nlon2/), typeof(pcld))
  cover = 99999.0
  do j=0,nlat2-1
  do i=0,nlon2-1
     cover(j,i) = pcld(j*2,i*2)
     clon(j,i) = lon(j*2,i*2)
     clat(j,i) = lat(j*2,i*2)
     if( cover(j,i) .gt. 99998.0 ) then
        cover(j,i) = 1150.0
     end if
     if( cover(j,i) .lt. 100.0 ) then
        cover(j,i) = 100.0
     end if
  end do
  end do

  delete(pcld)
  delete(lon)
  delete(lat)
  do j=0,nlat2-1
  do i=0,nlon2-1
     cover2(j,i) = tcld(j*2,i*2)
     cover3(j,i) = frac(j*2,i*2)
     cover4(j,i) = wlwp(j*2,i*2)
     if( cover2(j,i) .gt. 99998.0 ) then
        cover2(j,i) = 350.0
     end if
     if( cover2(j,i) .lt. 50.0 ) then
        cover2(j,i) = 340.0
     end if
     if( cover3(j,i) .lt. 0.0 ) then
        cover3(j,i) = 0.0
     end if
     if( cover3(j,i) .gt. 110.0 ) then
        cover3(j,i) = 0.0
     end if
     if( cover4(j,i) .lt. 0.0 ) then
        cover4(j,i) = 7000.0
     end if
     if( cover4(j,i) .gt. 7010.0 ) then
        cover4(j,i) = 9600.0
     end if
  end do
  end do

  delete(tcld)
  delete(frac)
  delete(wlwp)

  xwks = gsn_open_wks("pdf","RR_nasaLaRC_cloud")
  gsn_define_colormap(xwks,"cosam12")
 
  resources                  =  True              ; plot mods desired
  resources@sfXArray         = clon
  resources@sfYArray         = clat


  resources@cnMonoLineColor    = False
  resources@tiMainString       ="RR NASA LaRC"

  resources@mpProjection       = "LambertConformal" ; choose projection
  resources@mpLimitMode        = "Corners"          ; choose range of map
  resources@mpLeftCornerLatF      = 0.0
  resources@mpLeftCornerLonF      = -131.
  resources@mpRightCornerLatF     = 59.0
  resources@mpRightCornerLonF     = -1.0

  resources@mpLambertParallel1F   = 44.99997       ; ncl adds from grib file
  resources@mpLambertParallel2F   = 44.99997       ; ncl adds from grib file
  resources@mpLambertMeridianF    = -99.17999      ; ncl adds from grib file
  resources@pmTickMarkDisplayMode = "Always"
  resources@mpFillOn              =  False             ; turn off map fill
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@cnFillOn              = True               ; turn on color
  resources@mpGridLineDashPattern = 2                  ; lat/lon lines as dashed
  resources@cnLinesOn             = False              ; turn off contour lines
  resources@cnLineLabelsOn        = False   
  resources@cnConstFLabelPerimOn  = False               ; 
  resources@mpPerimOn = True
  
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

  resources@cnFillColors          = (/-1,2,3,4,5,6,7,8,9,10,11,12/)
  resources@cnFillDrawOrder       = "PreDraw"         ; make sure fill map on top
; 
  resources@cnLevelSelectionMode = "ExplicitLevels"     ;set explicit contour levels

  resources@vpXF       = 0.1
  resources@vpYF       = 0.8
  resources@vpWidthF   = 0.6
  resources@vpHeightF  = 0.6

  resources@tiMainString       ="RR NASA LaRC"
;  resources@cnLevels    = (/1160., 1100.0, 1000.0,800.0,600.0,400.0,200.0,100.0,50.0/)	; set levels
;  plot = gsn_contour_map(xwks, cover, resources)
;  resources@cnLevels    = (/360., 345.0, 335.0,310.0,300.0,280.0,260.0,220.0,200.0/)	; set levels
;  plot = gsn_contour_map(xwks, cover2, resources)
  resources@cnLevels    = (/-10.0,-5.0,-1.0,10.0,40.0,60.0,80.0,115.0,200.0/)	; set levels
;  resources@cnLevels    = (/-10.0,-5.0,-1.0,0.0,20.0,40.0,60.0,80.0,115.0/)	; set levels
  plot = gsn_contour_map(xwks, cover3, resources)
;  resources@cnLevels    = (/-10.0,0.0,400.0,2000.0,4000.0,6500.0,7500.0,8500.0,9500.0/)
;  plot = gsn_contour_map(xwks, cover4, resources)

  delete(plot)
  delete(resources)
end

