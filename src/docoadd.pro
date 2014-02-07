; NAME:
;     DOCOADD - Version 1.0
;
; PURPOSE: 

; CALLING SEQUENCE:
;     DOCOADD, INDATA, RECURSIVE=RECURSIVE
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

PRO DoCoadd, indata, recursive=recursive, keyword=keyword, keypath=keypath
  
  object = OBJ_NEW('RUNCOADD', $
                        keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $   ;,{keyname:'OBJCLASS',keyval:'',type:'none'}], $
                        keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
                        keyancillary=[{keyname:'OBJECT',keyval:'DARK',type:'fixed'}], debug=1)
  
  ; Add the type of input
  if not keyword_set(keyword) then keyword=0
  if not keyword_set(keypath) then keypath=0
  
  for i=0,n_elements(indata)-1 do begin
    foundfiles = findproducts(indata[i],recursive=keyword_set(recursive))
    print,'-----------------'
    print,foundfiles.files
    print,'-----------------'
    if foundfiles.count gt 0 then object->addList, foundfiles.files, recursive=keyword_set(recursive), keyword=keyword, keypath=keypath, /all
  endfor
  
  object->Run,prodfolder='coadded'
  
END