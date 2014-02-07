; NAME:
;     FMLOF - Version 1.0
;
; PURPOSE: Return filter and object name from file name
;     
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, October 11th 2013
FUNCTION FMLOF, fname, flatname=flatname, darkname=darkname

  restype = {object:'',filter:'', hwp:0.0, error:1}
  if not keyword_set(flatname) then flatname='flat'
  if not keyword_set(darkname) then darkname=['dark','bias']
  
  ; Check if it is a flat file
  ;if strpos(strlowcase(fname),flatname) ne -1 then begin
  ;  restype.object = 'FLAT'
  ;  
  ;  ; I think we have to check for the filter
  ;  restype.error = 0
  ;  return, restype
  ;endif
  
  ; Check if it is a dark file
  for i=0,n_elements(darkname)-1 do begin
    if strpos(strlowcase(fname),darkname[i]) ne -1 then begin
      restype.object = 'DARK'
      restype.error = 0
      return, restype
    endif
  endfor
  
  if file_test(fname) then begin
    im = readfits(fname,h,/silent)
    readobj = strtrim(sxpar(h,'OBJECT', count=ncount1),2)
    readfilt = strtrim(sxpar(h,'FILTER', count=ncount2),2)
    readhwp = sxpar(h,'HWP', count=ncount3)
    if ncount1*ncount2*ncount3 ne 0 then begin
      restype.object = readobj
      restype.filter = readfilt
      restype.hwp = readhwp
      restype.error=0
      return,restype
    endif
  endif
  
  name_arr = strsplit(file_basename(fname),'_',/extract)
  Narr = n_elements(name_arr)
  
  print,'   ... file name type is '+strtrim(Narr,2)+' segments'
  SWITCH Narr of
    2:
    3: begin
        
        if Narr eq 2 then begin
          obj_arr = strsplit(name_arr[0],'-',/extract)
          restype.object = obj_arr[0]
          restype.filter = strmid(name_arr[0],strlen(name_arr[0])-1,1)
          idxhwp = 1
        endif else begin
          restype.object = name_arr[0]
          restype.filter = strmid(name_arr[1],strlen(name_arr[1])-1,1)
          idxhwp = 2
        endelse
        restype.error = 0
        CASE fix(name_arr[idxhwp]) of
          0: restype.hwp=0.0
          225: restype.hwp=22.5
          45: restype.hwp=45.0
          450: restype.hwp=45.0
          675: restype.hwp=67.5
          90: restype.hwp=90.
          1125: restype.hwp=112.5
          135: restype.hwp=135.0
          1575: restype.hwp=157.5
          else: begin
                print,'HWP is not valid: '+strtrim(fix(name_arr[idxhwp]),2)
                restype.error = 1
              end
        ENDCASE
        break
      end
    4:
    5: begin
        if Narr eq 5 then addi = 1 else addi = 0
        restype.filter = name_arr[1+addi]
        restype.error = 0
        CASE fix(name_arr[2+addi]) of
          1: restype.hwp=0.0
          2: restype.hwp=22.5
          3: restype.hwp=45.0
          4: restype.hwp=67.5
          5: restype.hwp=90.
          6: restype.hwp=112.5
          7: restype.hwp=135.0
          8: restype.hwp=157.5
          else: begin
                print,'HWP is not valid: '+strtrim(fix(name_arr[2+addi]),2)
                restype.error = 1
              end
        ENDCASE
        restype.object = name_arr[0]
        if Narr eq 5 then restype.object = restype.object+name_arr[1]
        break
      end
    else: begin
          print,'FMLOF: WARNING!! File name does not follow standards - '+strtrim(Narr,2)+' strings found'
          print,'    Must follow naming convention OBJECT_FILTER_HWP_FILENUMBER'
        end
  ENDSWITCH
  
  return, restype
  
END
  
  
