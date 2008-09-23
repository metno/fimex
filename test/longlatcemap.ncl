;*************************************************
; ce_3.ncl
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"    
;************************************************
begin
;************************************************
; open file and read in data
;************************************************
  lonsize = 180
  latsize = 90
  data = asciiread("outData.txt",(/lonsize*latsize, 3/),"float")
  pf = new((/latsize+1,lonsize+1/),float)      ;
  pf!1 = "lon"                       ; name dim0 data
  pf!0 = "lat"
  pf&lon = new (lonsize+1, float)
  pf&lat = new (latsize+1, float)
  pf&lon@units = "degrees_east"
  pf&lat@units = "degrees_north"
; name dim1 lon
  do i = 0, lonsize
    pf&lon(i) = (i / 2.) - 30
  end do
  do i = 0, latsize
    pf&lat(i) = (i / 2.) + 30.
  end do
  do i=0,latsize*lonsize -1
    if (data(i,2) .ne. 0) then
      pf({data(i,1)},{data(i,0)}) = (/data(i,2)/)
    end if
  end do
;  print (pf)

  pf@nlon  = dimsizes(pf&lon)   ; and pf variables.
  pf@nlat  = dimsizes(pf&lat)

;************************************************
; create plot
;************************************************
  wks = gsn_open_wks("eps" ,"ce")                ; open a ps file
  gsn_define_colormap(wks,"BlAqGrYeOrRe")        ; choose colormap

  res                       = True     ; plot mods desired

  res@cnFillOn              = True     ; turn on color fill
  res@cnLinesOn             = False    ; turn of contour lines
  res@gsnSpreadColors       = True     ; use full range of color map
  res@lbLabelStride         = 4

  res@pmTickMarkDisplayMode = "Always"; use NCL default lat/lon labels

  res@gsnAddCyclic          = False    ; data already has cyclic point
				       ; this must also be set for any zoom
  res@gsnSpreadColors             = True             ; use full color map
  res@mpDataBaseVersion = "Ncarg4_1" ; get a up to date map-database
  res@mpOutlineBoundarySets = "allBoundaries" 
  res@mpProjection      = "Mercator"       ; choose projection
;  res@mpProjection      = "Stereographic"       ; choose projection
;  res@mpCenterLonF  = -32.
;  res@mpCenterLatF  = 90.
  res@mpGridLatSpacingF = 5.               ; spacing for lat lines
  res@mpGridLonSpacingF = 5.               ; spacing for lon lines


  res@cnRasterModeOn = "True"
  res@mpGridAndLimbOn= "True"
  res@mpGridMaskMode         = "MaskNone"  ; Draw grid over ocean.
  res@mpGridLineDashPattern  = 2                  ; lat/lon lines as dashed


; note that the gsn_csm_*map_ce templates automatically set 
; res@mpLimitMode="LatLon" for you. If you are plotting a different projection,
; you may have to set this resource.
;  res@gsnPolar   = "NH"
  res@mpLimitMode = "LatLon"
  res@mpMinLatF            = 30.      ; range to zoom in on
  res@mpMaxLatF            = 70.
;  res@mpMinLatF            = 40.      ; range to zoom in on for stereographic
;  res@mpMaxLatF            = (latsize / 2.) + 30.
  res@mpMinLonF            =  -30.
  res@mpMaxLonF            = (lonsize / 2.) - 30.

  res@cnLevelSelectionMode = "ExplicitLevels"
  res@cnLevels = (/0,10,20,30,40,50,60,70,80,90,100/)
  plot = gsn_csm_contour_map(wks,pf(:,:), res)
;************************************************
end
