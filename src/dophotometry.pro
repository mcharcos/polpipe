; NAME:
;     DOPHOTOMETRY - Version 1.0
;
; PURPOSE: Calculate the photometry for a set of data from a directory path or manifest file

; CALLING SEQUENCE:
;     DOPHOTOMETRY, INDATA, RECURSIVE=RECURSIVE
;
; INPUTS:
;     INDATA    - Path containing the data, Input manifest name or array of file names with the data
;     RECURSIVE - When indata is a path, recursive indicates if the files have to be search recursively in the tree
;     
;
; OUTPUTS:
;     Output fits files are created containing the information of the photometry
;
; SIDE EFFECTS:
;     None.
;
; RESTRICTIONS:
;     None.
;
; PROCEDURE:
;     Call RunPhotometry object to perform the photometry
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, October 11th 2013

PRO DoPhotometry, indata, recursive=recursive, aperture=aperture, background=background, overwrite=overwrite, $
                          dofwhm=dofwhm, interactive=interactive, profile=profile, keyword=keyword, keypath=keypath, dataqual=dataqual, trackpos=trackpos
  
  if not keyword_set(profile) then profile='MOFFAT'
  profile = strtrim(strupcase(profile),2)
  if not keyword_set(dataqual) then dataqual='SUPERCALIFRAGILISTICO'
  if not keyword_set(trackpos) then trackpos = 0
  
  aperts=[3,4,5,6,7,8,9,10,12,15,20]  ; needs one more value
  if keyword_set(aperture) then begin
    aperture = fix(aperture)
    k = where(aperture eq aperts)
    if k[0] ne -1 then begin
      aperts = [aperts,25]
      apertidx = k[0]
    endif else begin
      kpos = where(aperts-aperture gt 0)
      if kpos[0] eq -1 then begin
        aperts = [aperts,aperture]
        apertidx = n_elements(aperts)-1
      endif else begin 
        knear = min(kpos)
        newaperts = [aperture,aperts[knear:n_elements(aperts)-1]]
        if knear gt 0 then newaperts = [aperts[0:knear-1],newaperts]
        aperts = newaperts
        apertidx = knear
      endelse   
    endelse
  endif else begin
    aperts=[3,4,5,6,7,8,9,10,12,15,20,25]
    apertidx=10
  endelse
  back = [aperts[apertidx]+3,aperts[apertidx]+13]
  if keyword_set(background) then begin
    if n_elements(background) eq 2 then begin
      if background[0] lt background[1] then back=background else $
        if background[1] lt background[0] then back = [background[1], background[0]] else $
        print, 'Wrong input background: ['+strtrim(background[0],2)+','+strtrim(background[1],2)+'], using default back=['+strtrim(back[0],2)+','+strtrim(back[1],2)+']'
    endif
  endif
  
  photometry = OBJ_NEW('RUNPHOTOMETRY', $
                        keycommon=[{keyname:'DATAQUAL',keyval:dataqual,type:'diff'},{keyname:'FILTER',keyval:'',type:'same'}], $  
                        keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
                        keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}], $
                        $ ;Npeaks=2*Nstars
                        Npeaks=1, aperts=aperts, apertidx=apertidx, back=back, dofwhm=keyword_set(dofwhm), profile=profile, $
                        interactive=keyword_set(interactive), trackpos=trackpos,debug=1)
  
  ; Add the type of input
  if not keyword_set(keyword) then keyword=0
  if not keyword_set(keypath) then keypath=0
  for i=0,n_elements(indata)-1 do begin
    foundfiles = findproducts(indata[i],recursive=keyword_set(recursive))
    if foundfiles.count gt 0 then photometry->addList, foundfiles.files, recursive=keyword_set(recursive), keyword=keyword, keypath=keypath
  endfor
  
  if keyword_set(overwrite) then prodfolder='' else prodfolder='photometry'
  ;photometry->Run,exptimeKey='FRMTIME', gainScale=1.0e6, prodfolder=prodfolder
  photometry->Run,prodfolder=prodfolder
  
END