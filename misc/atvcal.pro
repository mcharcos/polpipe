; NAME:
;     ATVCAL - Version 1.0
;
; PURPOSE: Change the object type of the fits files of the input path

; CALLING SEQUENCE:
;     ATVCAL, PATH, RECURSIVE=RECURSIVE, OBSTYPE=OBSTYPE
;
; INPUTS:
;     PATH    - Path containing the data, Input manifest name or array of file names with the data
;     RECURSIVE - When indata is a path, recursive indicates if the files have to be search recursively in the tree
;     SELKEYS   - Array of strings as KEYWORD=VALUE to select what type of data is displayed
;     SCALE     - Displays the scale image 
;
; OUTPUTS:
;     None
;
; SIDE EFFECTS:
;     None.
;
; RESTRICTIONS:
;     None.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, August 28th 2013


PRO atvCal, path, recursive=recursive, selkeys=selkeys, scale=scale, oplotxy=oplotxy, oplotradec=oplotradec, profile=profile
  
  ; Default path is current path if not set
  if not keyword_set(path) then path='./'
  if keyword_set(obstype) then obstype=strupcase(obstype)
  if not keyword_set(scale) then scale = double(1.)
  
  COMPILE_OPT HIDDEN
  
  ; Exit if path does not exist
  for i=0,n_elements(path)-1 do begin
    print,'Checking '+path[i]+'...'
    if file_test(path[i],/directory) eq 0 then begin
      print,'    is not a directory'
      if file_test(path[i]) then begin
        print,'    is a file'
        if not keyword_set(foundfiles) then foundfiles = path[i] else foundfiles = [foundfiles, path[i]]
      endif
    endif else begin
      print,'    is a directory'
      foundfiles_aux = findfiles('*.fits',root=path[i],count=Ncount,recurse=keyword_set(recursive))
      if Ncount gt 0 then begin
        if not keyword_set(foundfiles) then foundfiles = foundfiles_aux else foundfiles = [foundfiles,foundfiles_aux]
      endif
    endelse
  endfor
  
  if not keyword_set(foundfiles) then begin
    print,'Input path is not set correctly: '+path
    print,'Try recursice mode if your input is a path'
    return
  endif
  Ncount = n_elements(foundfiles)
  

  for i=0,Ncount-1 do begin
    im = readfits(foundfiles[i],h,/silent)
    doatv=1
    
    ; Check the keyword restrictions
    if keyword_set(selkeys) then begin
      for j=0,n_elements(selkeys)-1 do begin
        key_arr = strsplit(selkeys[j],'=',/extract)
        if n_elements(key_arr) eq 2 then begin
          keyread = strupcase(strtrim(sxpar(h,key_arr[0]),2))
          if keyread ne strupcase(strtrim(key_arr[1],2)) then doatv=0 
        endif
      endfor
    endif
    
    ; Create a temporary image if the file is raw data with 4 planes
    imcomb = im
    if doatv eq 1 then begin
      s = size(im)
      if s[0] eq 3 then begin
        imcomb = im[*,*,0]
        if s[3] eq 4 then begin
          imcomb = im[*,*,0]-im[*,*,1]+im[*,*,3]-im[*,*,2]
        endif
      endif
    endif
    imcomb = scale*imcomb
    
    ; Display the image if it matches the keywords
    if doatv eq 1 then begin
      print,'======> '+foundfiles[i]
      print,'DATAQUAL='+strtrim(sxpar(h,'DATAQUAL'),2)
      print,'OBJECT='+strtrim(sxpar(h,'OBJECT'),2)
      obstype_read = strupcase(strtrim(sxpar(h,'OBSTYPE'),2))
      print,'OBSTYPE='+obstype_read
      procstat_read = strupcase(strtrim(sxpar(h,'PROCSTAT'),2))
      print,'PROCSTAT='+procstat_read
      
      atv_plus,imcomb,header=h
      common atv_state, state
      state.imagename = foundfiles[i]
      
      ; Plot the stars
      x = sxpar(h,'ST0X',count=ncount1)
      y = sxpar(h,'ST0Y',count=ncount2)
      
      ; Set the aperture
      state.aprad = 5
      valaux = sxpar(h,'STARAP',count=ncount)
      if ncount ne 0 then state.aprad=valaux
      valaux = sxpar(h,'STARBCK1',count=ncount)
      if ncount ne 0 then state.innersky=valaux else state.innersky=state.aprad+3
      valaux = sxpar(h,'STARBCK2',count=ncount)
      if ncount ne 0 then state.outersky=valaux else state.outersky=state.innersky+3
      
      if ncount1*ncount2 ne 0 and keyword_set(profile) then atv_apphot_toside, [x,y]
      
      stari=0
      while ncount1*ncount2 ne 0 do begin 
        atvplot, [x], [y], psym=4
        f = sxpar(h,'ST'+strtrim(stari,2)+'F',count=ncount3)
        if ncount3 ne 0 then atvxyouts,[x]+4,[y]+4,strtrim(f,2),charsize=2
        stari=stari+1
        x = sxpar(h,'ST'+strtrim(stari,2)+'X',count=ncount1)
        y = sxpar(h,'ST'+strtrim(stari,2)+'Y',count=ncount2)
      endwhile
       
      
      if keyword_set(oplotxy) then begin
        Nplot=n_elements(oplotxy)/2
        for pcount=0,nplot-1 do begin
          atvplot, [oplotxy[2*pcount]],[oplotxy[2*pcount+1]], psym=2
        endfor
      endif
      
      if keyword_set(oplotradec) then begin
        Nplot=n_elements(oplotradec)/2
        for pcount=0,nplot-1 do begin
          pixelval = radec2pix(h,[oplotradec[2*pcount],oplotradec[2*pcount+1]])
          atvplot, [pixelval[0]],[pixelval[1]], psym=5
        endfor
      endif
      
      crpix1=sxpar(h,'CRPIX1')
      crpix2=sxpar(h,'CRPIX2')
      crval1=sxpar(h,'CRVAL1')
      crval2=sxpar(h,'CRVAL2')
      cdelt1=sxpar(h,'CDELT1')
      cdelt2=sxpar(h,'CDELT2')
      print,'-----------------------------'
      print,'-- World Coordinate System --'
      print,'-----------------------------' 
      print,'CRPIX=['+strtrim(crpix1,2)+','+strtrim(crpix2,2)+']'
      print,'CRVAL=['+strtrim(crval1,2)+','+strtrim(crval2,2)+']'
      print,'CDELT=['+strtrim(cdelt1,2)+','+strtrim(cdelt2,2)+']'
      print,''
      atvplot,[crpix1],[crpix2],psym=6
      atvxyouts,[crpix1]+4,[crpix2]+4,'CRPIX',charsize=2
      
      ; Set default values of atv
      ; RA-DEC display in degrees
      state.display_base60=0
      ; I was trying here to overplot the compass
      ;atv_plotwindow
      ;atv_plot1compass,1
      
      print,'Press q over the display when done'
      atv_activate
    endif 
  endfor
  
  if (xregistered('atv', /noshow)) then atv_shutdown
  
END
