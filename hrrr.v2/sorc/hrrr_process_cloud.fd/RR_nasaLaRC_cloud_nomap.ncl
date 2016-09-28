load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin

;  nlon=758 
;  nlat=567  
;  nlon2=379 
;  nlat2=283  
  nlon=648 
  nlat=647  
  nlon2=324 
  nlat2=323  

;  pcld= fbinrecread("/mnt/lfs0/projects/wrfruc/mhu/temp/obsprd/NASALaRC_cloud.bin",4,(/nlat,nlon/),"float")
  pcld= fbinrecread("/mnt/lfs1/projects/nrtrr/RR13/DOMAINS/wrfrr13_cycle/2010090120/obsprd/NASALaRC_cloud.bin",4,(/nlat,nlon/),"float")

  cover = new ((/nlat2,nlon2/), typeof(pcld))
  cover = 99999.0
  do j=0,nlat2-1
  do i=0,nlon2-1
     cover(j,i) = pcld(j*2,i*2)
     if( cover(j,i) .gt. 99998.0 ) then
        cover(j,i) = 1100.0
     end if
     if( cover(j,i) .lt. 200.0 ) then
        cover(j,i) = 200.0
     end if
  end do
  end do

  xwks = gsn_open_wks("pdf","RR_nasaLaRC_cloud")
  gsn_define_colormap(xwks,"cosam12")
 
  resources                  =  True              ; plot mods desired

  resources@cnMonoLineColor    = False
  resources@tiMainString       ="radar reflectivity"

  resources@cnFillOn                 = True               ; turn on color
  resources@cnLinesOn                = False              ; turn off contour lines
  resources@cnLineLabelsOn        = False
  resources@cnConstFLabelPerimOn      = False               ;

  resources@lbPerimOn             = False    ; Turn off perimeter on label bar.

;  resources@cnFillColors         = (/-1,2,3,4,5,6,7,8,9,10,11,12,13,14/)
;  resources@cnFillDrawOrder       = "PreDraw"         ; make sure fill map on top
;
;  resources@cnLevelSelectionMode = "ExplicitLevels"     ;set explicit contour levels
; resources@cnLevels    = (/-100.,-10.0,-5.0, 1.0,10.0,20.0,30.0,40.0,50.0,60.0/) ;

  resources@vpXF       = 0.1
  resources@vpYF       = 0.8
  resources@vpWidthF   = 0.8
  resources@vpHeightF  = 0.5

  resources@tiMainString       ="RR NASA LaRC"
  plot = gsn_contour(xwks, cover, resources)

  delete(plot)
  delete(resources)
end

