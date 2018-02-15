load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

  cdf_file = addfile("/scratch3/BMC/wrfruc/mhu/rapcode/GSI_r1216/data_process/mosaic/test/geo_em.d01.nc.cdf","r")
  lon = cdf_file->XLONG_M(0,:,:)
  lat = cdf_file->XLAT_M(0,:,:)
 
  ny=dimsizes(lon(:,1))
  nx=dimsizes(lat(1,:))

  print(ny)
  print(nx)
;  asciiwrite("nx.ascii",nx)
;  asciiwrite("ny.ascii",ny)


;  mydata = fbinrecread("dbz_lightning.dat",0,(/ny,nx/),"float")
  mydata = fbinrecread("CompositeRefInGSI.dat",0,(/ny,nx/),"float")

   print(mydata(1,1))
   print(mydata(100,100))
  xwks = gsn_open_wks("pdf","my_rr_mosaic")
  gsn_define_colormap(xwks,"cosam12")
 
  resources                    =  True              ; plot mods desired
  resources@cnMonoLineColor    = False
  resources@tiMainString       ="Composite Reflectivity"

  resources@sfXArray         = lon
  resources@sfYArray         = lat


  resources@mpProjection       = "LambertConformal" ; choose projection
  resources@mpLimitMode        = "Corners"          ; choose range of map
  resources@mpLeftCornerLatF      = 13.0
  resources@mpLeftCornerLonF      = -130.
  resources@mpRightCornerLatF     = 45.0
  resources@mpRightCornerLonF     = -30.0

  resources@mpLambertParallel1F   = 45.0       ; ncl adds from grib file
  resources@mpLambertParallel2F   = 45.0       ; ncl adds from grib file
  resources@mpLambertMeridianF    = -99.12     ; ncl adds from grib file
  resources@pmTickMarkDisplayMode    = "Always"
  resources@mpFillOn                 =  False             ; turn off map fill
  resources@mpOutlineDrawOrder       = "PostDraw"         ; continent outline last
  resources@mpOutlineBoundarySets    = "GeophysicalAndUSStates" ; state boundaries
  resources@tfDoNDCOverlay           = False               ; don't transform
  resources@cnFillOn                 = True               ; turn on color
  resources@cnLineLabelsOn      = False
  resources@mpGridLineDashPattern  = 2                  ; lat/lon lines as dashed
  resources@cnLinesOn                = False              ; turn off contour lines
;  resources@gsnSpreadColors          = True               ; full range of colormap
  resources@cnConstFLabelPerimOn      = False               ; 
  
  resources@gsnMaximize       =   True
  resources@pmLabelBarDisplayMode = "Always" ; Turn on label bar.
  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

  resources@cnFillColors         = (/-1,2,3,4,5,6,7,8,9,10,11,12/)
  resources@cnFillDrawOrder       = "PreDraw"         ; make sure fill map on top
;
  resources@cnLevelSelectionMode = "ExplicitLevels"     ;set explicit contour levels
  resources@cnLevels    = (/-15.0,-5.0,0.,10.,15.,20.,25.,30.,35.,40.,50./)	; set levels

;  resources@cnLevelSelectionMode = "ManualLevels"   ; manually set the contour levels
                                                    ; with the following 3 resources
;  resources@cnMinLevelValF  = 1.			; set the minimum contour level
;  resources@cnMaxLevelValF  = 19.			; set the maximum contour level
;  resources@cnLevelSpacingF = 2.			; set the interval between contours

  plot = gsn_contour_map(xwks, mydata, resources)

  delete(plot)
  delete(mydata)
  delete(resources)
end

