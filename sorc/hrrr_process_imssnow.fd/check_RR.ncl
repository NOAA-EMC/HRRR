load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"    ; plot. Include before
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  cdf_geo = addfile("/lfs0/projects/wrfruc/mhu/RR13/DOMAINS/wrfrr13_cycle/static/geo_em.d01.nc","r")
  lon  = cdf_geo->XLONG_M(0,:,:)
  lat  = cdf_geo->XLAT_M(0,:,:)

  nx=648
  ny=647
  nsig=50
  field = fbinread("seaiceRR.dat",(/ny,nx/),"float")
  print("ny="+ny)
  print("nx="+nx)
;  print("field="+field(0,499))

;  xwks = gsn_open_wks("x11","gsun02n")      ; Open an X11 workstation.
  xwks = gsn_open_wks("pdf","seaice")      ; Open an X11 workstation.
  gsn_define_colormap(xwks,"cosam12")
  resources                 = True       ; Indicate you want to set some
  resources@sfXArray         = lon
  resources@sfYArray         = lat
  resources@mpProjection       = "LambertConformal" ; choose projection
  resources@mpLimitMode        = "Corners"          ; choose range of map
  resources@mpLeftCornerLatF      = 5.0 
  resources@mpLeftCornerLonF      = -140.
  resources@mpRightCornerLatF     = 60.0
  resources@mpRightCornerLonF     = 0.0

  resources@mpLambertParallel1F   = 44.99997       ; ncl adds from grib file
  resources@mpLambertParallel2F   = 44.99997       ; ncl adds from grib file
  resources@mpLambertMeridianF    = -99.17999      ; ncl adds from grib file
  resources@pmTickMarkDisplayMode = "Always"
  resources@mpFillOn              =  False             ; turn off map fill
  resources@mpOutlineDrawOrder    = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets = "GeophysicalAndUSStates" ; state boundaries
  resources@mpGridLineDashPattern = 2                  ; lat/lon lines as dashed
  resources@mpPerimOn = True

  resources@cnLinesOn             = False              ; turn off contour lines
  resources@cnLineLabelsOn        = False   
  resources@cnConstFLabelPerimOn  = False               ; 
;  resources@cnMonoLineColor = False      ; Turn off the drawing of
                                         ; contours lines in one color.
  resources@cnFillOn            = True          ; turn on color
  resources@tiMainString    = "sea ice in RR "  ; Create a title.

;  resources@cnLineLabelDensityF   = 3               ; increase the number of line labels/line
;  resources@cnLineLabelInterval   = 1               ; labels for every line (default=2)

;  resources@gsnContourNegLineDashPattern = 1       ; sets negative contours to dash pattern 1

  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

  resources@cnFillColors          = (/-1,2,3,4,5,6,7,8,9,10,11,12,13,14/)
  resources@cnFillDrawOrder       = "PreDraw"         ; make sure fill map on top

  resources@cnLevelSelectionMode = "ExplicitLevels"     ;set explicit contour levels
;  resources@cnLevels    = (/-1.0,0.0,1.0,10.0,20.0,30.0,40.0,50.0,80.0,100.0/)	; set levels
  resources@cnLevels    = (/-1.0,0.0,0.2,0.4,0.6,0.8,0.9,1.0,1.1,1.2/)	; set levels


  resources@vpXF       = 0.1
  resources@vpYF       = 0.8
  resources@vpWidthF   = 0.6
  resources@vpHeightF  = 0.6
;  resources@cnLevelSpacingF = 0.2                      ; contour interval
  plot = gsn_contour_map(xwks,field(:,:),resources)  ; Draw a contour plot.

end

