; NAME:
;     CLASSDEF - Version 1.0
;
; PURPOSE: Create a General class that is used to defined general methods that are likely common
;          to a large range of classes
;
; CLASS ATTRIBUTES:       
;
;     + debug:        1 if debugging or 0 if not
;     
;
; CLASS METHODS:
;     + INIT: Initialize object based on the initvals fits/text file or array. 
;     + CLEANUP: Clean heaps. Need to be implemented by child classes
;     + MESSAGE: Output a message according to the priority (INFO|WARNING|ERROR|DEBUG)
;     + GetProperty: Get the value of the requested attribute
;     + SetProperty: Set the value of the requested attribute
;     + Copy: Copy the information of an object into itself
;     + Equal: Copy the information of an object into itself
;     + Toarray: Creates an array of strings containing information about the object. 
;     + Fromarray: Load the information from the array of strings containing information the labels and their values dim=[2,X]. 
;     + FromFits: Initialize structure fields from the header of an input fits file.
;     + ToTextFile: Save object information to txt file
;     + FromTextFile: Load object information from a txt file
;     
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, May 16th 2012


;******************************************************************************
;     MESSAGE -  Output a message according to the priority (INFO|WARNING|ERROR|DEBUG)
;******************************************************************************
PRO classdef::Message, msg, method=method, priority=priority
  
  if keyword_set(priority) eq 0 then priority='INFO'
  priority = strupcase(priority)
  if keyword_set(method) then method = OBJ_CLASS(self)+'::'+strupcase(method)+' - ' else method = ''
  
  ; May be a way to find what is calling the routine.
  ; May be even a way to know the level of call
  ;help, calls=list
  ;caller=strmid(list[1],0,strpos(list[1],' '))
  
  SWITCH priority of
    'I':
    'INF':
    'INFO': begin
            print,'INFO::'+method+msg[0]
	    for i=1,n_elements(msg)-1 do print,'       '+msg[i]
	    break
	  end
    'W':
    'WARN':
    'WARNING': begin
               print,'WARNING::'+method+msg[0]
	       for i=1,n_elements(msg)-1 do print,'       '+msg[i]
	       break
	     end
    'E':
    'ERR':
    'ERROR': begin
             ok = Dialog_Message([method,msg])
	     break
	   end
    'D':
    'DEB':
    'DEBUG': begin
             if self.debug eq 1 then for i=0,n_elements(msg)-1 do print,'DEBUG::'+method+msg[i]
	     break
	   end
    else: begin
          print,'Priority '+priority+' is not recognized in '+OBJ_CLASS(self)+'::Message'
	end
  ENDSWITCH
  
END

;******************************************************************************
;     GETPROPERTY -  Return a property of the object
;******************************************************************************
FUNCTION classdef::GetProperty, _Extra=extraKeyword

     ; Only one property at a time can be returned.

     IF N_Elements(extraKeyword) EQ 0 THEN begin
       self->Message, 'Must indicate which property to return.',priority='WARNING',method='GetProperty'
       return,{error:1,key:ptr_new(),index:-1}
     endif
     IF N_Tags(extraKeyword) GT 1 THEN begin
       self->Message, 'Only one property at a time can be returned.',priority='WARNING',method='GetProperty'
       return,{error:1,key:ptr_new(),index:-1}
     endif

     ; Pull keyword out of extra structure. It will be in UPPERCASE characters.

     keyword = (Tag_Names(extraKeyword))[0]

     ; Obtain a structure definition of the object class.

     ok =  Execute("struct = {" + Obj_Class(self) + "}")

     ; There should be only one match to the structure fields. If there
     ; are more, then you have used an ambiguous keyword and you need more
     ; characters in the keyword abbreviation.

     index = Where(StrPos(Tag_Names(struct), keyword) EQ 0, count)
     index = index[0]
     IF count GT 1 THEN begin
       self->Message, 'Ambiguous keyword ('+keyword+') Use more characters in its specification.',priority='WARNING',method='GetProperty'
       return,{error:1,key:ptr_new(),index:-1}
     endif
     IF count EQ 0 THEN begin
       ; First check if the requested keyword is one of the cmpkeynames list
       if self.cmpkeynames ne ptr_new() then begin
         k = where(*self.cmpkeynames eq keyword)
	 if k[0] ne -1 then begin
	   return,{error:0,key:(*self.cmpkeyvalues)[k[0]],index:-1}
         endif
       endif
       self->Message, 'Keyword ('+keyword+') not found.',priority='WARNING',method='GetProperty'
       return,{error:1,key:ptr_new(),index:-1}
     endif

     RETURN, {error:0,key:self.(index),index:index}
END

;******************************************************************************
;     SETPROPERTY -  Set a property of the object
;******************************************************************************
PRO classdef::SetProperty, _Extra=extraProperties

     ; Error handling.
     Catch, theError
     IF theError NE 0 THEN BEGIN
        Catch, /Cancel
	self->Message, !Error_State.MSG,priority='WARNING',method='SetProperty'
        RETURN
     ENDIF

     IF N_Elements(extraProperties) EQ 0 THEN self->Message, 'Must indicate which property to set.', priority='WARNING',method='SetProperty'
     properties = Tag_Names(extraProperties)

     ; Obtain a structure definition of the object class.

     ok =  Execute("struct = {" + Obj_Class(self) + "}")
     taglist = Tag_Names(struct)
     
     ; Loop through the various properties and their values.
     FOR j=0L,N_Tags(extraProperties)-1 DO BEGIN
        theProperty = properties[j]
	self->Message,'Setting attribute '+theProperty,priority='DEBUG',method='SetProperty'
        index = Where(StrPos(taglist, theProperty ) EQ 0, count)
        index = index[0]
        IF count GT 1 THEN begin
	  self->Message, 'Ambiguous keyword: ' + theProperty + '. Use more characters in its specification.',priority='WARNING',method='SetProperty'
	endif
	IF count EQ 0 THEN begin
	  self->Message, 'Keyword ('+keyword+') not found.',priority='WARNING',method='SetProperty'
	endif
	IF count EQ 1 then begin
	  self.(index) = extraProperties.(j)
	endif
     ENDFOR
END


;******************************************************************************
;     COPY -  Copy the information of an object into itself
;******************************************************************************
FUNCTION classdef::Copy, cmpobj, _Extra=extraKeyword
  
  self->Message, 'Starting...',priority='DEBUG',method='Copy' 
  
  if N_Elements(extraKeyword) EQ 0 then begin
    ;self->Message, 'Must indicate which property to return.',priority='WARNING',method='Copy'
    ;return,0  ;{error:1,key:ptr_new()}
    self->Message, 'Keywords to be copied not specified',priority='INFO',method='Copy'
    self->Message, 'Copying all keywords from input '+OBJ_CLASS(cmpobj)+' object',priority='INFO',method='Copy'
    ok =  Execute("struct = {" + Obj_Class(cmpobj) + "}")
    properties = Tag_Names(struct)
    Ntags = N_Tags(struct)
  endif else begin
    properties = Tag_Names(extraKeyword)
    Ntags = N_Tags(extraKeyword)
  endelse
  
  FOR j=0L,Ntags-1 DO BEGIN
    theProperty = properties[j]
    ok =  Execute('struct_cmpobj = cmpobj->GetProperty(/'+theProperty+')')
    ok =  Execute('struct_self = self->GetProperty(/'+theProperty+')')
    if struct_cmpobj.error eq 0 and struct_self.error eq 0 then begin
      if struct_self.index ne -1 then begin
	self->Message, 'Copying keyword '+theProperty,priority='DEBUG',method='Copy'
	self.(struct_self.index) = struct_cmpobj.key
      endif
    endif
  ENDFOR
  
  self->Message, 'Done',priority='DEBUG',method='Copy'
  
  return, 1
  
END

;****************************************************************************
;     EQUAL - Compares two objects and return 1 if they have 
;                   the same configuration or 0 otherwise
;****************************************************************************
FUNCTION classdef::Equal, cmpobj
  
  self->Message,'Not yet implemented',priority='ERROR',method='EQUAL'
  
  return,1

END


;****************************************************************************
;     TOARRAY - Creates an array of strings containing information about the object. 
;****************************************************************************
FUNCTION classdef::toArray, out=out, fname=fname, labels=labels, structure=structure, exclude=exclude, mergearr=mergearr, filebase=filebase
  
  self->Message,'Starting...',priority='DEBUG',method='toArray'
  
  if keyword_set(structure) then struct = structure $
  else ok =  Execute("struct = {" + Obj_Class(self) + "}")
  
  ; Read the tag names of the structure of object
  tagnames = strlowcase(TAG_NAMES(struct))
  
  ; Define what is the index of the structure attribute lists to be removed
  ; set to -1 if not exclusion or non element with that name is found
  idxexclude = [-1]
  if keyword_set(exclude) then begin
    for i= 0, n_elements(exclude)-1 do begin
      self->Message,'Excluding '+exclude[i]+'...',priority='DEBUG',method='TOARRAY'
      currentidxexcl = where(tagnames eq strlowcase(exclude[i]))
      if currentidxexcl[0] ne -1 then begin
        self->Message,'       of index(es) ['+strjoin(strtrim(currentidxexcl,2),',')+']',priority='DEBUG',method='TOARRAY'
	if idxexclude[0] eq -1 then idxexclude = currentidxexcl $
	else idxexclude = [idxexclude,currentidxexcl]
      endif
    endfor
  endif 
  
  Ntags = n_elements(tagnames)
  tagtypes = intarr(Ntags)
  tagsizes = intarr(Ntags)
  for i=0,Ntags-1 do begin
    if keyword_set(structure) then begin
      tagtypes[i] = size(struct.(i),/type)
      tagsizes[i] = n_elements(struct.(i))
    endif else begin
      tagtypes[i] = size(self.(i),/type)
      tagsizes[i] = n_elements(self.(i)) ; we assume here that there are not 2D or 3D arrays
                                           ; we could handle this in the future by defining a
					   ; notation like tagname::idx1::idx2::idx::3
    endelse
  endfor
  
  ; Treat first elements that are not arrays
  k = where(tagtypes ne 8 and tagtypes ne 10 and tagtypes ne 11 and tagsizes eq 1)
  if k[0] ne -1 then k = SetDifference(k,idxexclude)
  if k[0] eq -1 then begin
    self->Message,'There must always be an attribute of standard types (string, float or integer)',priority='WARN',method='toArray'
    self->Message,'this should never happen',priority='ERROR',method='toArray'  
    return,0
  endif
  Nouttags = n_elements(k)
  resarr = strarr(Nouttags)
  labels_arr = tagnames[k]
  self->Message,'LABELS (AFTER ELEMENTS) = ['+strjoin(labels_arr,',')+']',priority='DEBUG',method='TOARRAY'
  for i=0,Nouttags-1 do begin
    if keyword_set(structure) then begin
      if keyword_set(filebase) then resarr[i] = file_basename(strtrim(struct.(k[i]),2)) else resarr[i] = strtrim(struct.(k[i]),2)
    endif else begin
      if keyword_set(filebase) then resarr[i] = file_basename(strtrim(self.(k[i]),2)) else resarr[i] = strtrim(self.(k[i]),2)
    endelse
    self->Message,'Converting to Array '+labels_arr[i]+'='+resarr[i]+'...',priority='DEBUG',method='toArray'
  endfor
  
  ; Treat elements of the structure that are arrays
  k = where(tagtypes ne 8 and tagtypes ne 10 and tagtypes ne 11 and tagsizes gt 1)
  if k[0] ne -1 then k = SetDifference(k,idxexclude)
  if k[0] ne -1 then begin
    for i=0,n_elements(k)-1 do begin
      auxlabels = 'supercalifragilistico'
      auxres = 'supercalifragilistico'
      if keyword_set(structure) then auxarr = struct.(k[i]) else auxarr = self.(k[i])
      arrtype = size(auxarr,/type)
      if arrtype ne 8 and arrtype ne 10 and arrtype ne 11 then begin
	self->Message,'Converting to Array '+tagnames[k[i]]+'=',priority='DEBUG',method='toArray'
	for j=0,n_elements(auxarr)-1 do begin
	  if keyword_set(mergearr) then begin
	    if auxlabels[0] eq 'supercalifragilistico' then auxlabels = [tagnames[k[i]]] else auxlabels = [auxlabels+':'+strtrim(j,2)]
	    if auxres[0] eq 'supercalifragilistico' then begin
	      if keyword_set(filebase) then auxres = [file_basename(strtrim(auxarr[j],2))] else auxres = [strtrim(auxarr[j],2)]
	    endif else begin
	      if keyword_set(filebase) then auxres = [auxres+'|'+file_basename(strtrim(auxarr[j],2))] else auxres = [auxres+' | '+strtrim(auxarr[j],2)]
	    endelse
	  endif else begin
	    if auxlabels[0] eq 'supercalifragilistico' then auxlabels = [tagnames[k[i]]+':'+strtrim(j,2)] else auxlabels = [auxlabels,tagnames[k[i]]+':'+strtrim(j,2)]
	    if auxres[0] eq 'supercalifragilistico' then begin
	      if keyword_set(filebase) then auxres = [file_basename(strtrim(auxarr[j],2))] else auxres = [strtrim(auxarr[j],2)]
	    endif else begin
	      if keyword_set(filebase) then auxres = [auxres,file_basename(strtrim(auxarr[j],2))] else auxres = [auxres,strtrim(auxarr[j],2)]
	    endelse
	  endelse
	  self->Message,'       index #'+strtrim(j,2)+': '+strtrim(auxarr[j],2),priority='DEBUG',method='toArray'
	endfor
	labels_arr = [labels_arr,auxlabels]
	resarr = [resarr,auxres]
      endif
    endfor
  endif
  self->Message,'LABELS (AFTER ARRAYS) = ['+strjoin(labels_arr,',')+']',priority='DEBUG',method='TOARRAY'

  k = where(tagtypes eq 10)
  if k[0] ne -1 then k = SetDifference(k,idxexclude)
  if k[0] ne -1 then begin
    for i=0,n_elements(k)-1 do begin
      auxlabels = 'supercalifragilistico'
      auxres = 'supercalifragilistico'
      if keyword_set(structure) then ptraux = struct.(k[i]) else ptraux = self.(k[i])
      if ptraux ne ptr_new() then begin
	if keyword_set(structure) then auxarr = *(struct.(k[i])) else auxarr = *(self.(k[i]))
	arrtype = size(auxarr,/type)
	if arrtype ne 8 and arrtype ne 10 and arrtype ne 11 then begin
	  self->Message,'Converting to Array '+tagnames[k[i]]+'=',priority='DEBUG',method='toArray'
	  for j=0,n_elements(auxarr)-1 do begin
	    if keyword_set(mergearr) then begin
	      if auxlabels[0] eq 'supercalifragilistico' then auxlabels = [tagnames[k[i]]] else auxlabels = [auxlabels+':'+strtrim(j,2)]
	      if auxres[0] eq 'supercalifragilistico' then begin
	        if keyword_set(filebase) then auxres = [file_basename(strtrim(auxarr[j],2))] else auxres = [strtrim(auxarr[j],2)]
	      endif else begin
	        if keyword_set(filebase) then auxres = [auxres+'|'+file_basename(strtrim(auxarr[j],2))] else auxres = [auxres+' | '+strtrim(auxarr[j],2)]
	      endelse
	    endif else begin
	      if auxlabels[0] eq 'supercalifragilistico' then auxlabels = [tagnames[k[i]]+':'+strtrim(j,2)] else auxlabels = [auxlabels,tagnames[k[i]]+':'+strtrim(j,2)]
	      if auxres[0] eq 'supercalifragilistico' then begin
	        if keyword_set(filebase) then auxres = [file_basename(strtrim(auxarr[j],2))] else auxres = [strtrim(auxarr[j],2)]
	      endif else begin
	        if keyword_set(filebase) then auxres = [auxres,file_basename(strtrim(auxarr[j],2))] else auxres = [auxres,strtrim(auxarr[j],2)]
	      endelse
	    endelse
	    
	    self->Message,'       index #'+strtrim(j,2)+': '+strtrim(auxarr[j],2),priority='DEBUG',method='toArray'
	  endfor
          labels_arr = [labels_arr,auxlabels]
          resarr = [resarr,auxres]
	endif
      endif
    endfor
  endif
  self->Message,'LABELS (AFTER POINTERS) = ['+strjoin(labels_arr,',')+']',priority='DEBUG',method='TOARRAY'
  
  
  if keyword_set(labels) then begin
    resarr = [[labels_arr],[resarr]]
  endif
  
  if keyword_set(out) then begin
    print,strjoin(resarr,STRING(9B))
  endif
  
  if keyword_set(fname) then begin
    OPENW,inunit,fname,/GET_LUN
    printf,inunit,strjoin(resarr,STRING(9B))
    FREE_LUN,inunit
  endif
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='TOARRAY'
  
  return, resarr
  
END


;****************************************************************************
;     FROMARRAY - Load the information from the array of strings containing 
;                 information the labels and their values dim=[2,X] 
;                 Returns 1 if the data is loaded correctly or 0 otherwise
;****************************************************************************
FUNCTION classdef::FromArray, inarr, struct_ptr=struct_ptr
  
  self->Message,'Starting...',priority='DEBUG',method='fromArray'
  
  s = size(inarr)
  if s[0] ne 2 or s[1] ne 2 then begin
    self->Message,'Wrong array format',priority='WARNING',method='fromArray'
    return,0
  endif

  if keyword_set(struct_ptr) then struct=*struct_ptr  $
  else ok =  Execute("struct = {" + Obj_Class(self) + "}")
  
  taglist = strlowcase(Tag_Names(struct))
  labels_arr = strlowcase(inarr[0,*])
  valarr = inarr[1,*]
  
  for i=0,s[2]-1 do begin
    self->Message,'Reading '+labels_arr[i]+' ('+strtrim(i,2)+'/'+strtrim(s[2],2)+')...',priority='DEBUG',method='fromArray'
    self->Message,'     --> value = '+valarr[i],priority='DEBUG',method='fromArray'
    ;k = where(taglist eq labels_arr[i])
    k = Where(StrPos(taglist, labels_arr[i] ) EQ 0, count)
    if k[0] ne -1 then begin
      self->Message,'        attribute '+labels_arr[i]+' was found!',priority='WARNING',method='fromArray'
      if keyword_set(struct_ptr) then (*struct_ptr).(k[0]) = valarr[i] else self.(k[0]) = valarr[i]
    endif else begin
      auxsplitarr = strsplit(labels_arr[i],':',/extract,count=ncount)
      if ncount eq 2 then begin
	auxk = Where(StrPos(taglist, auxsplitarr[0] ) EQ 0, count)
	readidx = fix(auxsplitarr[1])
	if auxk[0] ne -1 then begin 
	  if keyword_set(struct_ptr) then auxattribute = (*struct_ptr).(auxk[0])  $
	  else auxattribute = self.(auxk[0])
	  ; The attribute stores an array. There are two options
	  ; either it is an array itself or it is a pointer
	  if size(auxattribute,/type) eq 10 then begin
	    ; DefaultNullValue should return something different from notfound since we 
	    ; know we are considering an attribute of the object. Otherwise, there is a problem 
	    ; with the code
	    defvalue = self->DefaultNullValue(auxsplitarr[0])
	    if defvalue eq 'notfound' then begin
	      self->Message,'Default value for '+auxsplitarr[0]+' was not found in structure',priority='WARNING',method='fromArray'
	      help,auxattribute
	      return,0
	    endif
	    auxarr = replicate(defvalue,readidx+1)
	    if auxattribute eq ptr_new() then begin
	      if keyword_set(struct_ptr) then (*struct_ptr).(auxk[0]) = ptr_new(auxarr) else self.(auxk[0]) = ptr_new(auxarr)
	    endif else begin
	      Nself = n_elements(*auxattribute)
	      if Nself le readidx then auxarr[0:Nself-1] = *auxattribute else auxarr = *auxattribute
	    endelse
	    auxarr[readidx] = valarr[i]
	    *(self.(auxk[0])) = auxarr
	  endif else begin
	    if readidx gt n_elements(self.(auxk[0])) then return,0
	    if keyword_set(struct_ptr) then (*struct_ptr).(auxk[0])[readidx] = valarr[i] else self.(auxk[0])[readidx] = valarr[i]
	  endelse
	endif
      endif
    endelse
  endfor

  
  return,1
  
END

;****************************************************************************
;     FROMSAVFILE - Initialize structure fields from the a sav file
;                   We assume that the sav file contain an array with the information
;   	    	    of the object. Arrname is a string with the name of the variable to be read
;****************************************************************************
FUNCTION classdef::FromSavFile, fname, arrname
  
  ; It  is not going to work for all the classes. It depends on toArray which not always save all the parameters
  return, 0
  
  restore,fname
  
  ok = execute('res = self->FromArray('+arrname+')')
  
  if ok eq 0 then return,0
  
  return,res
  
END

;****************************************************************************
;     TOSAVFILE - Save object information to an idl sav file
;****************************************************************************
PRO classdef::ToSavFile, outfname, arrname=arrname
  
  ; It is totally useless since toArray is very class dependent specially for complex classes
  return
  
  resarr = self->ToArray(/labels)
    
  if keyword_set(arrname) eq 0 then begin
    save,resarr,outfname
  endif else begin
    ok = execute(arrname+'=resarr')
    if ok eq 1 then ok=execute('save,'+arrname+','+outfname)
  endelse
    
END

;****************************************************************************
;     FROMFITS - Initialize structure fields from the header of an input fits file
;                This only reads the main keywords from the image extension
;                and it does not perform any read in table extensions used by stdphot
;****************************************************************************
FUNCTION classdef::FromFits, fname, header=header, outpath=outpath, outfname=outfname, noproducts=noproducts
  
  self->Message,'Not yet implemented',priority='ERROR',method='FromFits'
  
  return,1
  
END

;****************************************************************************
;     TOTEXTFILE - Save object information to txt file
;****************************************************************************
PRO classdef::ToTextFile, outfname
  
  straux = transpose(self->Array(/label))
  
  self->Message,strjoin(straux,STRING(9B)),priority='DEBUG',method='ToTextFile'
  
  if keyword_set(nosave) eq 0 then begin
    self->Message,'Saving results to '+outfname,priority='INFO',method='ToTextFile'
    OPENW,outunit,outfname,/GET_LUN
    printf,outunit,straux
    FREE_LUN,outunit
  endif
    
END

;****************************************************************************
;     FROMTEXTFILE - Load object information from a txt file
;****************************************************************************
FUNCTION classdef::FromTextFile, fname
  
  self->Message,'Starting...',priority='DEBUG',method='FromTextFile'
  
  ; It is assumed that if the information fails to load from the text
  ; file the object is set to incomplete because it is not in the state we expected                            
  self.completed = 0
  
  if FILE_TEST(fname) eq 0 then begin
    self->Message,'Input file does not exist ('+fname+')',priority='WARNING',method='FromTextFile'
    return,0
  endif
  
  readline = ''
  OPENR,inunit,fname,/GET_LUN
  while not eof(inunit) and strmid(strupcase(readline),0,9) ne '# TAGKEYS' do begin
    readf,inunit,readline
  endwhile
  if eof(inunit) then begin
    self->Message,'EOF reached before expected',priority='WARNING',method='FromTextFile'
    return,0
  endif
  readf,inunit,readline
  labels_arr = strsplit(readline,STRING(9B),/extract)
  
  if eof(inunit) then begin
    self->Message,'EOF reached before expected',priority='WARNING',method='FromTextFile'
    return,0
  endif
  readf,inunit,readline
  values_arr = strsplit(readline,STRING(9B),/extract)
  FREE_LUN,inunit
  
  if n_elements(labels_arr) ne n_elements(values_arr) then begin
    self->Message, 'Dimension of labels ('+strtrim(n_elements(labels_arr),2)+') does not match dimension of values ('+strtrim(n_elements(values_arr),2)+')',priority='WARNING',method='FromTextFile'
    return,0
  endif
  
  if self->FromArray(transpose([[labels_arr],[values_arr]])) eq 0 then begin
    self->Message,'Unable to load arrays:',priority='DEBUG',method='FromTextFile'
    self->Message,'LABELS:',priority='DEBUG',method='FromTextFile'
    self->Message,strjoin(labels_arr,STRING(9B)),priority='DEBUG',method='FromTextFile'
    self->Message,'VALUES:',priority='DEBUG',method='FromTextFile'
    self->Message,strjoin(values_arr,STRING(9B)),priority='DEBUG',method='FromTextFile'
    return,0
  endif
  return,1
  
END

;****************************************************************************
;     DEFAULTGROUP - Return a default structure storing the data of a group
;****************************************************************************
FUNCTION classdef::DefaultGroup, _EXTRA=extraProperties
  
  defaultgroup = {name:'', $
                  keycom:ptr_new(), $
		  keyval:ptr_new(), $
		  indata:ptr_new(), $
		  keyanc:ptr_new(), $
		  ancillary:ptr_new(), $
		  valid:0}

  return, fillstructure(defaultgroup,_EXTRA=extraProperties)

END

;****************************************************************************
;     VERIFYKEY - Check if the keywords of the header meet the input
;                 keyword requirement list. 
;****************************************************************************
FUNCTION classdef::verifyKey,header, keylist
  
  self->Message,'Starting',priority='DEBUG',method='VERIFYKEY'
  
  newlist = keylist
  
  ; Check if it has the same values  
  Nkey = n_elements(keylist)
  for i=0,Nkey-1 do begin
    currentType = (keylist[i]).type
    currentKey = (keylist[i]).keyname
    currentVal = strupcase((keylist[i]).keyval)
    readkey = strupcase(strtrim(sxpar(header,currentKey),2))
    
    self->Message,['>>>>>>>> Checking '+currentKey,'Value is '+readKey],priority='DEBUG',method='VERIFYKEY'
    if self.debug then help,keylist[i],/struct
    SWITCH currentType of
      'none': begin
              if readkey ne '0' then begin
		self->Message,'Keyword '+currentKey+' does not match ('+readkey+'<>'+currentVal+')',priority='INFO',method='verifyKey'
		return, {meet:0,keylist:keylist}
	      endif
	      break
	    end
      'diff': begin
              valarr = strsplit(currentVal,'|',/extract)
	      kval = where(valarr eq readkey)
	      if kval[0] ne -1 then begin
		self->Message,'Skipped because keyword '+currentKey+' is '+currentVal,priority='INFO',method='verifyKey'
		return, {meet:0,keylist:keylist}
	      endif
	      break
	    end
      'fixed': begin
              if currentVal ne '' then begin
		 valarr = strsplit(currentVal,'|',/extract)
		 kval = where(valarr eq readkey)
	         if kval[0] eq -1 then begin
		    self->Message,'Keyword '+currentKey+' does not match ('+readkey+'<>'+currentVal+')',priority='INFO',method='verifyKey'
		    return, {meet:0,keylist:keylist}
		 endif
	       endif
	    break
	     end
      'same' : begin ; we are going to assume that if the type is else it is the same as "fixed"
		if currentVal ne '' then begin
		  valarr = strsplit(currentVal,'|',/extract)
		  kval = where(valarr eq readkey)
	          if kval[0] eq -1 then begin
		    self->Message,'Keyword '+currentKey+' does not match ('+readkey+'<>'+currentVal+')',priority='INFO',method='verifyKey'
		    return, {meet:0,keylist:keylist}
		  endif 
		endif else begin
		  self->Message,'Keyword '+currentKey+' is updated to value '+readkey,priority='INFO',method='verifyKey'
		  newlist[i].keyval = readkey
		endelse
		break
	      end
	else: begin
	        self->Message,'Keyword type '+currentType+' is not valid',priority='INFO',method='verifyKey'
		return, {meet:0,keylist:keylist}
		break
	     end
    ENDSWITCH
    
    self->Message,['      ... matched'],priority='DEBUG',method='VERIFYKEY'
  endfor
  
  return, {meet:1,keylist:newlist}
END

;****************************************************************************
;     IsFileInGroup - Check if file is part of the input group by checking
;                     the keyword requirement list of the group
;                     The format of the group is given by DefaultGroup
;****************************************************************************
FUNCTION classdef::IsFileInGroup, fname, group, ancillary=ancillary
  
  self->Message,'Starting',priority='DEBUG',method='ISFILEINGROUP'
  
  ; Read header of fits file
  self->Message,'Reading file '+fname,priority='DEBUG',method='ISFILEINGROUP'
  im = readfits(fname,h,/silent)
    
  ; Check the common requirements
  self->Message,['/////////////////\\\\\\\\\\\\\\\\','Checking common requirements'],priority='DEBUG',method='ISFILEINGROUP'
  if group.keycom ne ptr_new() then begin
    rescommon = self->verifyKey(h,*group.keycom)
    if rescommon.meet eq 0 then begin
      self->Message,'File did not meet the common requirements',priority='DEBUG',method='IsFileInGroup
      return, 0
    endif 
  endif
  
  ; Check the data requirements if no ancillary keyword set
  if group.keyval ne ptr_new() and not keyword_set(ancillary) then begin
    self->Message,['/////////////////\\\\\\\\\\\\\\\\','Checking data requirements'],priority='DEBUG',method='ISFILEINGROUP'
    rescommon = self->verifyKey(h,*group.keyval)
    if rescommon.meet eq 0 then begin
      self->Message,'File did not meet the data requirements',priority='DEBUG',method='IsFileInGroup
      return, 0
    endif 
  endif
  
  ; Check the ancillary requirements if ancillary keyword set
  if group.keyanc ne ptr_new() and keyword_set(ancillary) then begin
    self->Message,['/////////////////\\\\\\\\\\\\\\\\','Checking ancillary requirements'],priority='DEBUG',method='ISFILEINGROUP'
    rescommon = self->verifyKey(h,*group.keyanc)
    if rescommon.meet eq 0 then begin
      self->Message,'File did not meet the ancillary requirements',priority='DEBUG',method='IsFileInGroup
      return, 0
    endif 
  endif
    
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='ISFILEINGROUP'
  return,1
  
END


;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO classdef::cleanup
  
  if self.flag ne ptr_new() then ptr_free,self.flag
  
END

;****************************************************************************
;     INIT - Initialize object based on the data input that can be an array,
;            a fits file or a text ascii file
;****************************************************************************
FUNCTION classdef::init, initvals=initvals, debug=debug, _Extra=extraKeyword
  
  self.debug = 0
  if keyword_set(debug) then begin
    self.debug = debug 
  endif else begin
    if getenv('IDL_DEBUG') eq '1' then self.debug = 1 else self.debug = 0
  endelse
  
  self->Message,'Starting...',priority='DEBUG',method='INIT'
  
  ; Initialize the flags to a single element 1
  self.flags = ptr_new([1])
  
  if keyword_set(initvals) then begin
    ; The only options for initvals are strings
    if size(initvals,/type) ne 7 then begin
      self->Message,'INITVALS attribute can only be a string array or value',priority='ERROR',method='INIT'
      return,0
    endif
    
    
    if n_elements(initvals) eq 1 then begin
      ; Here initvals is the name of a txt or fits file containing the information of the object
      
      ; Check if the file exist in the filesystem
      if not FILE_TEST(initvals) then begin
	self->Message,'File does not exist: '+initvals,priority='ERROR',method='INIT'
	return, 0
      endif
      self->Message,'Object loaded from file '+initvals,priority='DEBUG',method='INIT'
      
      ; If it is a text file we call FromTextFile to initialize the values of the objects
      ; if it is a fits file we call FromFits to initialize the values of the objects
      auxpos = strpos(initvals,'.fit')
      if auxpos[0] eq -1 then begin
	if self->FromTextFile(initvals) eq 0 then return,0
      endif else begin
        if self->FromFits(initvals) eq 0 then return,0
      endelse

    endif else begin
      ; Here we assume that is an array that can be parsed by fromArray method
      if self->FromArray(initvals) eq 0 then return,0
    endelse
  endif
  
  ; Now we update the values input in extraKeyword. These will overwrite
  ; the one in initvals (if used)
  IF N_Elements(extraKeyword) GT 0 THEN self->SetProperty, _Extra=extraKeyword
  
  return, 1
  
END

;****************************************************************************
;     CLASSDEF__DEFINE - Define the class structure for the class catalogue
;****************************************************************************

PRO classdef__define

struct={classdef, $
        flags:ptr_new(), $
	debug:0 $ 	
       }

END
