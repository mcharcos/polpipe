; NAME:
;     DOPOLARIMETRY - Version 1.0
;
; PURPOSE: 

; CALLING SEQUENCE:
;     DOPOLARIMETRY, INDATA, RECURSIVE=RECURSIVE
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
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, October 15th 2013

PRO DoPolarimetry, indata, recursive=recursive, keyword=keyword, keypath=keypath, dataqual=dataqual, interactive=interactive, product=product, overwrite=overwrite
  
  if not keyword_set(dataqual) then dataqual='SUPERCALIFRAGILISTICO'
  if keyword_set(product) then objclass='SUPERCALIFRAGILISTICO' else objclass='RUNPOLARIMETRY'
  
  object = OBJ_NEW('RUNPOLARIMETRY', $
                        keycommon=[{keyname:'OBJCLASS',keyval:objclass,type:'diff'},{keyname:'DATAQUAL',keyval:dataqual,type:'diff'},{keyname:'FILTER',keyval:'',type:'same'},{keyname:'OBJECT',keyval:'',type:'same'}], $  
                        keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
                        keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}], interactive=keyword_set(interactive), debug=1)
  
  ; Add the type of input
  if not keyword_set(keyword) then keyword=0
  if not keyword_set(keypath) then keypath=0
  
  if keyword_set(product) then begin
    object->AddProduct,indata[0]
  endif else begin
    for i=0,n_elements(indata)-1 do begin
      foundfiles = findproducts(indata[i],recursive=keyword_set(recursive))
      if foundfiles.count gt 0 then object->addList, foundfiles.files, recursive=keyword_set(recursive), keyword=keyword, keypath=keypath, /all
    endfor
  endelse
  
  if keyword_set(overwrite) then begin
    object->Run,prodfolder=''
  endif else begin
    object->Run,prodfolder='polarimetry'
  endelse
END