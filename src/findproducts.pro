; NAME:
;     FINDPRODUCTS - Version 1.0
;
; PURPOSE: Find the fits files associated to a specific product either by the path or OBJCLASS keyword
;     
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, October 11th 2013
FUNCTION findproducts, inpath, recursive=recursive, keyword=keyword, keypath=keypath
  
  if not keyword_set(inpath) then inpath='./'
  
  if file_test(inpath, /directory) then begin
    path = file_expand_path(inpath)
    foundfiles = findfiles('*.fit*',root=path, recurse=keyword_set(recursive), count=Ncount)
  endif else begin
    foundfiles = inpath
    Ncount = 1
    path = file_expand_path(file_dirname(foundfiles))
  endelse
  
  if Ncount eq 0 then begin
    print,'No file found at '+path
    return, {path:path, files:'', count:0}
  endif
  
  if not keyword_set(keyword) and not keyword_set(keypath) then return, {path:path, files:foundfiles, count:Ncount}
  
  flist=''
  if keyword_set(keyword) then key0 = strtrim(keyword,2)
  for i=0,Ncount-1 do begin
    fname = foundfiles[i]
    fgood = 1
    if keyword_set(keyword) or keyword_set(keypath) then fgood=0
    if keyword_set(keyword) then begin
      im = readfits(fname,h)
      readkey = sxpar(h,'OBJCLASS')
      for j=0,n_elements(keyword) do begin
        if strtrim(readkey,2) eq key0[j] then fgood = 1
      endfor
    endif
    if keyword_set(keypath) then begin
      for j=0,n_elements(keypath) do begin
        idxcmp = strpos(fname,keypath[j])
        if idxcmp ne -1 then fgood = 1
      endfor
    endif
    if fgood eq 1 then flist = [flist,fname]
  endfor
  
  Nf = n_elements(flist)
  if Nf eq 0 then return,{path:path, files:'', count:0}
  
  return,{path:path, files:flist[1:Nf-1], count:Nf-1}
  
END