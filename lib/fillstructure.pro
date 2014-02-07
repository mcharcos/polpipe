;****************************************************************************
;     FILLSTRUCTURE - Set the values of the input structure that are entered
;                     in the extraproperties
;           Example:
;                     - instruct = {a:5 (integer), b:1.0 (float), c:'hello' (string)}
;                     - fillstructure(instruct, a=3, c='bye') returns, {a:3, b:1.0, c:'bye'}
;****************************************************************************
FUNCTION fillstructure, instruct, _EXTRA=extraProperties
  
  defaultstruct = instruct
  if n_elements(extraProperties) EQ 0 then return, defaultstruct
  
  listofpointers = ['starphot','starephot']

  properties = Tag_Names(extraProperties)
  Ntags = n_elements(properties)
  
  tagstruct = Tag_Names(defaultstruct)

  ; Loop through the various properties and their values.
  for j=0L,Ntags-1 do begin 
     theProperty = properties[j]

     index = Where(tagstruct eq theProperty, count)
     index = index[0]
     if count gt 1 then begin
       print, "Ambiguous keyword: " + theProperty + ". Use more characters in it's specification."
       print, '    only the first element will be updated
     endif
     if count eq 0 then print, 'Keyword ('+theProperty+') not found.'
     if count ge 1 then begin
       if size(defaultstruct.(index),/type) ne 10 then begin
	 defaultstruct.(index) = extraProperties.(j) 
       endif else begin 
	 if size(extraProperties.(j),/type) eq 10 then begin
	   defaultstruct.(index) = extraProperties.(j)
	 endif else begin
	   if defaultstruct.(index) eq ptr_new() then defaultstruct.(index) = ptr_new(extraProperties.(j)) $
	   else *(defaultstruct.(index)) = extraProperties.(j)
	 endelse
       endelse
     endif
  endfor
  
  return,defaultstruct
  
END
