; NAME:
;     DOINSTPOL - Version 1.0
;
; PURPOSE: 

; CALLING SEQUENCE:
;     DOSERKOWSKI, INDATA, RECURSIVE=RECURSIVE
;
; INPUTS:
;     INDATA    - Path containing the data, Input manifest name or array of file names with the data
;     RECURSIVE - When indata is a path, recursive indicates if the files have to be search recursively in the tree
;     
;
; OUTPUTS:
;     Output fits files are created containing the information of the corrections
;
; SIDE EFFECTS:
;     None.
;
; RESTRICTIONS:
;     None.
;
; PROCEDURE:
;     Remove instrumental polarization using polarized and unpolarized standards in the input data;
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, February 6th 2014

PRO DoInstPol, indata, recursive=recursive, keyword=keyword, keypath=keypath, dataqual=dataqual, interactive=interactive
  
  if not keyword_set(dataqual) then dataqual='SUPERCALIFRAGILISTICO'
  
  object = OBJ_NEW('RUNINSTRUMENTALPOL', $
                        keycommon=[{keyname:'DATAQUAL',keyval:dataqual,type:'diff'},{keyname:'OBJCLASS',keyval:'RUNINSTRUMENTALPOL',type:'diff'}], $  
                        keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
                        keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}], interactive=keyword_set(interactive), debug=1)
  
  ; Add the type of input
  if not keyword_set(keyword) then keyword=0
  if not keyword_set(keypath) then keypath=0
  
  for i=0,n_elements(indata)-1 do begin
    foundfiles = findproducts(indata[i],recursive=keyword_set(recursive))
    if foundfiles.count gt 0 then object->addList, foundfiles.files, recursive=keyword_set(recursive), keyword=keyword, keypath=keypath, /all
  endfor
  
  object->Run,prodfolder='instpol'
  
END