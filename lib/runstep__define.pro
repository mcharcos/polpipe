; NAME:
;     RUNSTEP - Version 1.0
;
; PURPOSE: Run a data procesing step
;
; CLASS ATTRIBUTES:       
;
; CLASS METHODS:
;     + INIT: Initialize object  
;     + CLEANUP: Clean heaps. 
;     + Run: Start the process
;     
; INHEREITS METHODS:
;     + From CLASSDEF
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, January 22nd 2013

;****************************************************************************
;     GROUPARRAY - Creates an array of strings containing information about the object. 
;****************************************************************************
FUNCTION runstep::GroupArray,labels=labels, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='GROUPARRAY'
  
  if self.outdata eq ptr_new() then resarr = '' else resarr = 'done'
  if keyword_set(labels) then resarr = [OBJ_CLASS(self),resarr]
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='GROUPARRAY'
  
  return, resarr
  
END

;****************************************************************************
;     CREATEGROUP - Return a default structure storing the data of a group
;****************************************************************************
FUNCTION runstep::CreateGroup
  
  self->Message,'Starting',priority='DEBUG',method='CREATEGROUP'
    
  ; We have to check if the indata is not empty
  if self.indata eq ptr_new() then begin
    self->Message,'Input data array is null',priority='DEBUG',method='CREATEGROUP'
    fname = 'default' 
  endif else begin
    if n_elements(*self.indata) gt 0 then begin
      fname = (*self.indata)[0]
    endif else begin
      self->Message,'Input data array is empty',priority='DEBUG',method='CREATEGROUP'
      fname = 'default'
    endelse
  endelse
  
  
  newgroup = self->DefaultGroup(name=file_basename(fname), $
                  keycom=self.keycommon, $
		  keyval=self.keylist, $
		  indata=self.indata, $
		  keyanc=self.keyancillary, $
		  ancillary=self.ancillary, $
		  valid=1)

  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='CREATEGROUP'
  return, newgroup

END

;******************************************************************************
;     DUPLICATE -  Create a new object with some of the information of self
;******************************************************************************
FUNCTION runstep::Duplicate
  
  caseopt = (self.keycommon ne ptr_new())*100 + (self.keylist ne ptr_new())*10 + (self.keyancillary ne ptr_new())
  
  CASE caseopt of
    0:   newrunstep = OBJ_NEW(OBJ_CLASS(self))
    1:   newrunstep = OBJ_NEW(OBJ_CLASS(self), keyancillary=*self.keyancillary)
    10:  newrunstep = OBJ_NEW(OBJ_CLASS(self), keylist=*self.keylist)
    11:  newrunstep = OBJ_NEW(OBJ_CLASS(self), keylist=*self.keylist, keyancillary=*self.keyancillary)
    100: newrunstep = OBJ_NEW(OBJ_CLASS(self), keycommon=*self.keycommon)
    101: newrunstep = OBJ_NEW(OBJ_CLASS(self), keycommon=*self.keycommon, keyancillary=*self.keyancillary)
    110: newrunstep = OBJ_NEW(OBJ_CLASS(self), keycommon=*self.keycommon, keylist=*self.keylist)
    111: newrunstep = OBJ_NEW(OBJ_CLASS(self), keycommon=*self.keycommon, keylist=*self.keylist, keyancillary=*self.keyancillary)
    else: ; I guess this is not possible, no?
  ENDCASE
  
  return,newrunstep
  
END

;****************************************************************************
;     TOARRAY - Creates an array of strings containing information about the object. 
;****************************************************************************
FUNCTION runstep::toArray, out=out, fname=fname, labels=labels, exclude=exclude, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='TOARRAY'
  
  exclude0 = ['indata','outdata','ancillary']
  if keyword_set(exclude) then exclude0 = [exclude,exlcude0]
  
  ; First we add to the table all elements except the input data and ancillary names
  resarr = self->classdef::toArray(labels=keyword_set(labels), exclude=exclude0, _EXTRA=extraKeywords) 
  
  ; Now we create an array for the input data names
  incell = '['
  inpathcell = ''
  if self.indata ne ptr_new() then begin
    if n_elements(*self.indata) gt 0 then begin
      ; We assume that the path is going to be the same for all file names
      inpathcell = file_dirname((*self.indata)[0]) 
      Nin = n_elements(*self.indata)
      for i=0,Nin-2 do begin
	incell = incell + file_basename((*self.indata)[i]) + ','
      endfor
      if Nin gt 1 then incell = incell + file_basename((*self.indata)[Nin-1])
    endif
  endif
  incell = incell + ']'
  
  ; Now we concatenate this result with the previous one
  if keyword_set(labels) then begin
    inarr = [['InPath','InData'],[inpathcell,incell]]
    resarr = transpose([[transpose(resarr)],[transpose(inarr)]])
  endif else begin
    resarr = [resarr,incell]
  endelse
  
  ; Now we create an array for the ancillary data names
  incell = '['
  inpathcell = ''
  if self.ancillary ne ptr_new() then begin
    if n_elements(*self.ancillary) gt 0 then begin
      ; We assume that the path is going to be the same for all file names
      inpathcell = file_dirname((*self.ancillary)[0]) 
      Nin = n_elements(*self.ancillary)
      for i=0,Nin-2 do begin
	incell = incell + file_basename((*self.ancillary)[i]) + ','
      endfor
      if Nin gt 1 then incell = incell + file_basename((*self.ancillary)[Nin-1])
    endif
  endif
  incell = incell + ']'
  
  ; Now we concatenate this result with the previous one
  if keyword_set(labels) then begin
    inarr = [['AncPath','AncData'],[inpathcell,incell]]
    resarr = transpose([[transpose(resarr)],[transpose(inarr)]])
  endif else begin
    resarr = [resarr,incell]
  endelse
  
  
  if keyword_set(out) then begin
    print,strjoin(resarr,STRING(9B))
  endif
  
  if keyword_set(fname) then begin
    OPENW,inunit,fname,/GET_LUN
    printf,inunit,strjoin(resarr,STRING(9B))
    FREE_LUN,inunit
  endif
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='TOARRAY'
  
  return,resarr
  
END

;****************************************************************************
;     IsThereData - Check if the array of data is initialized and contain 
;                   elements. By default it checks input data but
;                   keywords ancillary or outdata can be set for other data
;                   It returns a structure containing the requested pointer and
;                   array
;****************************************************************************
FUNCTION runstep::IsThereData, ancillary=ancillary, outdata=outdata
  
  self->Message,'Starting',priority='DEBUG',method='ISTHEREDATA'
  
  ptrdata = self.indata
  if keyword_set(ancillary) then begin
    self->Message,'Ancillary is on',priority='DEBUG',method='ISTHEREDATA'
    ptrdata = self.ancillary
  endif
  if keyword_set(outdata) then begin
    self->Message,'Output is on',priority='DEBUG',method='ISTHEREDATA'
    ptrdata = self.outdata
  endif
  if ptrdata eq ptr_new() then begin
    self->Message,'Data to be combined was not initialized: Null pointer',priority='DEBUG',method='IsThereData'
    return, {error:1, data:-1, pointer:ptr_new(), Ndata:-1}
  endif
  
  Ndata = n_elements(*ptrdata)
  if Ndata eq 0 then begin
    self->Message,'Data array to be combined is empty',priority='DEBUG',method='IsThereData'
    return, {error:2, data:-1, pointer:ptrdata, Ndata:0}
  endif
  
  return, {error:0, data:*ptrdata, pointer:ptrdata, Ndata:Ndata}  
  
END

;****************************************************************************
;     IsFileInData - Check if file is part of the indata array according to keylist
;****************************************************************************
FUNCTION runstep::IsFileInData, fname, ancillary=ancillary
  
  self->Message,'Starting',priority='DEBUG',method='ISFILEINDATA'
  
  isfilehere = self->IsFileInGroup(fname,self->CreateGroup(),ancillary=keyword_set(ancillary))
  return, isfilehere
  
  ; When sure that it works, just remove the lines below in this function
  ptrdata = self.indata
  ptrkeylist = self.keylist
  if keyword_set(ancillary) then begin
    ptrdata = self.ancillary
    ptrkeylist = self.keyancillary
  endif
  
  ; Check if there is data. If there is not, we assume that the file matches the object
  ; since it does not have any data to compare to
  ; I remove this part because it turns out that if we do this check, the first file
  ; will always be part of the group, even though it may have a none desired keywords
  ;if ptrdata eq ptr_new() then begin
  ;  self->Message,'Data array was not initialized: Null pointer',priority='DEBUG',method='IsFileInData'
  ;  return, 1
  ;endif
  ;Ndata = n_elements(*ptrdata)
  ;if Ndata eq 0 then begin
  ;  self->Message,'Data array is empty',priority='DEBUG',method='IsFileInData'
  ;  return, 1
  ;endif
  
  ; Check if there is a list of keywords in keylist. If not, we assume the file 
  ; matches the object since there is no key to be compared
  if ptrkeylist eq ptr_new() then begin
    self->Message,'Keyword list was not initialized: Null pointer',priority='DEBUG',method='IsFileInData'
    return, 1
  endif
  Nkey = n_elements(*ptrkeylist)
  if Nkey eq 0 then begin
    self->Message,'Keyword list array to be combined is empty',priority='DEBUG',method='IsFileInData'
    return, 1
  endif
  
  ; Read header of fits file
  self->Message,'Reading file '+fname,priority='DEBUG',method='IsFileInData'
  im = readfits(fname,h,/silent)
  
  ; Check if it has the same values  
  for i=0,Nkey-1 do begin
    currentType = ((*ptrkeylist)[i]).type
    currentKey = ((*ptrkeylist)[i]).keyname
    currentVal = ((*ptrkeylist)[i]).keyval
    readkey = strtrim(sxpar(h,currentKey),2)
    SWITCH currentType of
      'none': begin
              if readkey ne '0' then begin
		self->Message,'Keyword '+currentKey+' does not match ('+readkey+'<>'+currentVal+')',priority='INFO',method='IsFileInData'
		return,0
	      endif
	      break
	    end
      'diff': begin ; we are going to assume that if the type is else it is the same as same
	      if strupcase(readkey) eq  strupcase(currentVal) then begin
		self->Message,'Keyword '+currentKey+' matches restricted keyword ('+strupcase(readkey)+'='+strupcase(currentVal)+')',priority='INFO',method='IsFileInData'
		return,0
	      endif
	      break
	    end
      'same' :
      'fixed':
      else: begin ; we are going to assume that if the type is else it is the same as same
	    if strupcase(readkey) ne  strupcase(currentVal) and currentVal ne '' then begin
	      self->Message,'Keyword '+currentKey+' does not match ('+strupcase(readkey)+'<>'+strupcase(currentVal)+')',priority='INFO',method='IsFileInData'
	      return,0
	    endif
	    break
	  end
    ENDSWITCH
  endfor
  
  return,1
  
END

;****************************************************************************
;     COMPLETEKEYS - Complete the array of the keylist keywords
;****************************************************************************
PRO runstep::completeKeys,fname, ancillary=ancillary
  
  self->Message,['-----','Starting...'],priority='DEBUG',method='COMPLETEKEYS'
  
  keylist = self.keylist
  if keyword_set(ancillary) then begin
    keylist = self.keyancillary
  endif
  
  ; Check if there is a list of keywords in keylist. 
  if keylist eq ptr_new() then begin
    self->Message,'Keyword list was not initialized: Null pointer',priority='DEBUG',method='completeKeys'
    return
  endif
  Nkey = n_elements(*keylist)
  if Nkey eq 0 then begin
    self->Message,'Keyword list array to be combined is empty',priority='DEBUG',method='completeKeys'
    return
  endif
  
  im = readfits(fname,h,/silent)
  
  ; Complete key values
  for i=0,Nkey-1 do begin
    currentType = ((*keylist)[i]).type
    if currentType eq 'same' then begin
      currentKey = ((*keylist)[i]).keyname
      (*keylist)[i].keyval = strtrim(sxpar(h,currentKey),2)
    endif
  endfor
  
  
END

;****************************************************************************
;     ADDFILE - Add file if it has the same properties as previous
;****************************************************************************
PRO runstep::addFile,fname, ancillary=ancillary
  
  self->Message,'Starting',priority='DEBUG',method='ADDFILE'
  dataptr = self.indata
  if keyword_set(ancillary) then begin
    dataptr = self.ancillary
    self->Message,'Ancillary option was set',priority='DEBUG',method='ADDFILE'
  endif
  
  ; Initialize the array of indata or ancillary data if the pointer is null because
  ; this mean that no data was there before.
  if dataptr eq ptr_new() then begin
    if self->IsFileInData(fname, ancillary=keyword_set(ancillary)) eq 1 then begin
      self->Message,'Data pointer is empty... initializing',priority='DEBUG',method='ADDFILE'
      if keyword_set(ancillary) then self.ancillary = ptr_new([fname]) else self.indata = ptr_new([fname])
      msg = 'Adding file'
      if keyword_set(ancillary) then msg = msg + ' as ancillary'
      self->Message,msg+' '+fname,priority='DEBUG',method='ADDFILE'

      ; Complete first the keys based on the first file
      self->completeKeys,fname
    endif else begin
      msg = 'Problem inserting first file'
      if keyword_set(ancillary) then msg = msg + ' as ancillary'
      self->Message,msg+': '+fname,priority='DEBUG',method='ADDFILE'
    endelse
    return
  endif
  
  if self->IsFileInData(fname, ancillary=keyword_set(ancillary)) eq 1 then begin
    self->Message,'Adding file '+fname,priority='DEBUG',method='ADDFILE'
    k = where(*dataptr eq fname)
    if k[0] ne -1 then begin
      msg = 'File already existed in object'
      if keyword_set(ancillary) then msg = msg + ' as ancillary'
      self->Message,msg+': '+fname,priority='INFO',method='ADDFILE'
    endif else begin
      msg = 'File did not exist in object.'
      if keyword_set(ancillary) then msg = msg + ' as ancillary'
      self->Message,msg+' Adding: '+fname,priority='DEBUG',method='ADDFILE'
      *dataptr = [*dataptr,fname]
    endelse
  endif
  
END


;****************************************************************************
;     CREATEPATH - Returns a path directory to save the  products
;****************************************************************************
FUNCTION runstep::createPath, path, prodfolder=prodfolder
  
  if n_elements(prodfolder) eq 0 then prodfolder = 'reduce'
  
  ; Check first if it is already a path for the products.
  ; We assume it is if it ends with prodfolder
  tree = strsplit(path,path_sep(),/extract)
  if tree[n_elements(tree)-1] eq prodfolder then return,path
  
  ; If it does not exist we create a directory under named reduce
  ; We want to avoid adding all the products in the same folder because
  ; reading that folder would take more and more time.
  prodpath = path+'/'+prodfolder+'/'
  
  if file_test(prodpath,/directory) eq 0 then begin
    self->Message,'Creating product directory: '+prodpath,priority='INFO',method='CREATEPATH'
    file_mkdir,prodpath
  endif
  
  return,prodpath
  
END

;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION runstep::createFname, fnames, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
  
  ; Separate path from fnames
  rootpath = './'
  subfname = fnames[0]
  strpos = strpos(fnames[0],path_sep(),/reverse_search)
  if strpos ge 0 then begin
    rootpath = strmid(fnames[0],0,strpos+1)
    subfname = strmid(fnames[0],strpos+1,strlen(fnames[0])-strpos-1)
  endif
  
  ; Separate extension and rootname
  rootname = subfname
  extension = 'fits'
  strpos = strpos(subfname,'.',/reverse_search)
  if strpos ge 0 then begin
    rootname = strmid(subfname,0,strpos)
    extension = strmid(subfname,strpos+1,strlen(subfname)-strpos)
  endif
  self->Message,['ROOTPATH='+rootpath,'SUBFNAME='+subfname,'ROOTNAME='+rootname,'EXTENSION='+extension],priority='DEBUG',method='CREATEFNAME'
  previousclass = ''
  strpos = strpos(subfname,'--',/reverse_search)
  if strpos ge 0 then begin
    rootname = strmid(rootname,0,strpos)
    previousclass = strmid(rootname,strpos+1,strlen(rootname)-strpos)
  endif
  self->Message,['ROOTNAME changed to '+rootname],priority='DEBUG',method='CREATEFNAME'
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='CREATEFNAME'
  
  prodpath = self->createPath(rootpath, _EXTRA=extraKeywords)
  self->Message,['PRODPATH='+prodpath],priority='DEBUG',method='CREATEFNAME'
  
  return, prodpath+rootname+'--'+strlowcase(OBJ_CLASS(self))+'.'+extension
  
END


;****************************************************************************
;     SXADDLONGSTR - Return the header updated with the new string
;                    that is decomposed in multiple keywords if it is
;                    longer than the maximum characters
;****************************************************************************
FUNCTION runstep::SxAddLongStr, header, keyname, strvalue, keylength=keylength
  
  if not keyword_set(keylength) then keylength = 68
  
  hout = header
  Nstr=float(strlen(strvalue))/keylength
  for ipath=0,fix(Nstr)-1 do begin
    sxaddpar,hout,keyname+strtrim(ipath,2),strmid(strvalue,keylength*ipath,keylength)
  endfor
  if Nstr ne fix(Nstr) then begin
    sxaddpar,hout,keyname+strtrim(ipath,2),strmid(strvalue,keylength*ipath,keylength)
  endif
  
  return, hout
  
END

;****************************************************************************
;     SXLONGSTR - Return the value of the keyword from the header that
;                 was updated using sxaddlongstr
;****************************************************************************
FUNCTION runstep::SxLongStr, header, keyname, keylength=keylength
  
  if not keyword_set(keylength) then keylength = 68
    
  ipath=0
  readpath=sxpar(header,keyname+strtrim(ipath,2),count=Nreadp)
  resstr=''
  while Nreadp gt 0 do begin
    resstr=resstr+readpath
    ipath = ipath + 1
    readpath=sxpar(header,keyname+strtrim(ipath,2),count=Nreadp)
  endwhile
  
  return, resstr
  
END


;****************************************************************************
;     GIVEOUTPUTS - Return the output files from this step
;****************************************************************************
FUNCTION runstep::GiveOutputs, ancillary=ancillary, _EXTRA=extraKeywords
  
  if keyword_set(ancillary) then begin
    ptrdata = self.ancillary
  endif else begin
    ptrdata = self.outdata
  endelse
  
  return,ptrdata
  
END

;****************************************************************************
;     GETCONFIGURATION - Returns an array of structures with the attributes
;                        that configurable
;****************************************************************************
FUNCTION runstep::GetConfiguration
  
  self->Message,'Starting',priority='DEBUG',method='GETCONFIGURATION'
  defaultres = {name:'',value:'',type:'',error:1}
  
  if self.sections eq ptr_new() then begin
    self->Message,'Section array is null',priority='DEBUG',method='GETCONFIGURATION'
    return,[defaultres]
  endif
  if n_elements(*self.sections) eq 0 then begin
    self->Message,'No elements in section array',priority='DEBUG',method='GETCONFIGURATION'
    return,[defautres]
  endif
  
  i = 0
    auxstr = '['+strjoin((*self.sections)[*,i],',')+']'
  for i = 1, n_elements(*self.sections)-1 do begin
    auxstr = auxstr + ',' + '['+strjoin((*self.sections)[*,i],',')+']'
  endfor
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='GETCONFIGURATION'
  return ,[{name:'sections',value:'auxstr',type:'array2D:int',error:0}]
  
END

;****************************************************************************
;     SETCONFIGURATION - Updates the configuration from an array of structures 
;****************************************************************************
PRO runstep::SetConfiguration, config
  
  self->Message,'Starting',priority='DEBUG',method='SETCONFIGURATION'
  
  for i = 0,n_elements(config)-1 do begin
    ok =  Execute("self->SetProperty,"+config[i].name+"="+config[i].value)
  endfor
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='SETCONFIGURATION'
  
END

;****************************************************************************
;     FILLKEY - Complete the input key from a key list keylist
;****************************************************************************
FUNCTION runstep::FillKey, key, keylist
  
  if key.type ne 'same' then begin
    return, key  
  endif
  
  newkey = key
  for i=0,n_elements(keylist)-1 do begin
    currentkey = keylist[i]
    if newkey.keyname eq currentkey.keyname then begin
      newkey.keyval = currentkey.keyval
      return, newkey
    endif
  endfor
  
  return,newkey
  
END

;****************************************************************************
;     FillKeyLists - Complete self key values from the input lists
;****************************************************************************
PRO runstep::FillKeyList, keycommon=keycommon, keylist=keylist, keyancillary=keyancillary
  
  self->Message,'Starting',priority='DEBUG',method='FillKeyLists'
  
  ; Check keycommon if requested
  if keyword_set(keycommon) then begin
    if self.keycommon ne ptr_new() then begin
      for i=0,n_elements(*self.keycommon)-1 do begin
	(*self.keycommon)[i] = self->FillKey((*self.keycommon)[i],keycommon)
      endfor
    endif
  endif
  
  ; Check keylist if requested
  if keyword_set(keylist) then begin
    if self.keylist ne ptr_new() then begin
      for i=0,n_elements(*self.keylist)-1 do begin
	(*self.keylist)[i] = self->FillKey((*self.keylist)[i],keylist)
      endfor
    endif
  endif
  
  ; Check keyancillary if requested
  if keyword_set(keyancillary) then begin
    if self.keyancillary ne ptr_new() then begin
      for i=0,n_elements(*self.keyancillary)-1 do begin
	(*self.keyancillary)[i] = self->FillKey((*self.keyancillary)[i],keyancillary)
      endfor
    endif
  endif
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='FillKeyLists'
  
END

;****************************************************************************
;     TRANSFERKEYS - Complete self key values from the lists of the input object
;****************************************************************************
PRO runstep::TransferKeys, object
  
  objtype = size(object, /type)
  CASE objtype of
    8: begin
       keycommonptr = object.keycom
       keylistptr = object.keyval
       keyancillaryptr = object.keyanc
      end
    11: begin
       keycommonptr = (object->GetProperty(/keycommon)).key
       keylistptr = (object->GetProperty(/keylist)).key
       keyancillaryptr = (object->GetProperty(/keyancillary)).key
      end
    else: return
  ENDCASE
  
  valcase = (keycommonptr eq ptr_new())*100 +(keylistptr eq ptr_new())*10 +(keyancillaryptr eq ptr_new())
  
  CASE valcase of
    0: self->FillKeyList, keycommon=*keycommonptr, keylist=*keylistptr, keyancillary=*keyancillaryptr
    1: self->FillKeyList, keycommon=*keycommonptr, keylist=*keylistptr
    10: self->FillKeyList, keycommon=*keycommonptr, keyancillary=*keyancillaryptr
    11: self->FillKeyList, keycommon=*keycommonptr
    100: self->FillKeyList, keylist=*keylistptr, keyancillary=*keyancillaryptr
    101: self->FillKeyList, keylist=*keylistptr
    110: self->FillKeyList, keycommon=*keycommonptr, keylist=*keylistptr
    111: 
    else:
  ENDCASE
  
END

;****************************************************************************
;     TRANSFEROBJ - Add files from a previous object
;****************************************************************************
PRO runstep::TransferObj, object, ancillary=ancillary, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='TRANSFEROBJ'
  
  ; Check if there is a previous step. Don't do anything if there is none
  if object eq OBJ_NEW() then begin
    self->Message,'Previous step is not set, transfer failed',priority='DEBUG',method='TRANSFEROBJ'
    return
  endif
  
  ; Read data from previous
  ptrnewdata = object->GiveOutputs(ancillary=keyword_set(ancillary), _EXTRA=extraKeywords)
  if ptrnewdata eq ptr_new() then begin
    self->Message,'Previous data pointer is null',priority='WARN',method='TRANSFERFILES'
    return
  endif
  self->Message,['Outputs from GiveOutputs are:','    -'+*ptrnewdata],priority='DEBUG',method='TRANSFEROBJ'
  
  ; Take the outputs of the previous step and add them to indata
  for i=0,n_elements(*ptrnewdata)-1 do begin
    self->Message,'Adding file '+(*ptrnewdata)[i],priority='DEBUG',method='TRANSFEROBJ'
    self->addFile,(*ptrnewdata)[i], ancillary=keyword_set(ancillary)
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='TRANSFEROBJ'
  
END

;****************************************************************************
;     TRANSFERFILES - Add files from a previous step
;****************************************************************************
PRO runstep::TransferFiles, ancillary=ancillary, object=object, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='TRANSFERFILES'
  
  ; Use either object as the object to take the data from
  ; or self.previous
  if not keyword_set(object) then begin
    if self.previous eq ptr_new() then begin
      self->Message,'No reference transfer element',priority='INFO',method='TRANSFERFILES'
      return
    endif
    if n_elements(*self.previous) eq 0 then begin
      self->Message,'Reference transfer element array is empty',priority='INFO',method='TRANSFERFILES'
      return
    endif
    
    object = *self.previous
  endif
  
  ; Clean the current data
  if keyword_set(ancillary) then begin
    ptr_free,self.ancillary
    self.ancillary = ptr_new()
  endif else begin
    ptr_free,self.indata
    self.indata = ptr_new()
  endelse
  
  ; Now call the transfer for each of the objects in the input array
  for i=0,n_elements(object)-1 do begin
    self->Message,'Transfering object '+strtrim(i,2),priority='DEBUG',method='TRANSFERFILES'
    self->TransferObj,object[i],ancillary=keyword_set(ancillary), _EXTRA=extraKeywords
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='TRANSFERFILES'
  
END

;****************************************************************************
;     ADDPATH - Check the data in a specific path and add the files 
;                to indata list according to properties
;****************************************************************************
PRO runstep::addPath,path, all=all, recursive=recursive, _Extra=extraKeyword

  if self.previous ne PTR_NEW() then begin
    if n_elements(*self.previous) gt 0 then begin
      self->Message,['Processing step depends on previous object','Cannot add data from path'],priority='WARN',method='ADDPATH'
      return
    endif
  endif
  
  if not FILE_TEST(path) then begin
    self->Message, 'Directory does not exist', priority='ERR',method='ADDPATH'
    return
  endif
  
  ; Check if there is runstep.sav file in the directory
  ; In that case we just load the calibration plan
  if FILE_TEST(path+'runstep.sav') then begin
    self->Message, ['Runstep object already exist in directory', $
                    'Loading previous RunStep'],priority='INFO',method='ADDPATH'
    junk = 1  ;self->FromSavFile(path+'runstep.sav','calsav')
    if junk eq 1 then return
  endif 
  
  foundfiles = findfiles('*.fit*',root=path,recurse=keyword_set(recursive),count=Nfound)
  
  if Nfound eq 0 then begin
    self->Message, 'No file found in directory: '+path, priority='ERR',method='ADDPATH'
    return
  endif
    
  for i=0,Nfound-1 do begin
    if keyword_set(all) then begin
      self->AddFile,foundfiles[i]
      self->AddFile,foundfiles[i],/ancillary
    endif else begin
      self->AddFile,foundfiles[i], _Extra=extraKeyword
    endelse
  endfor
  
END


;****************************************************************************
;     ADDLIST - Add a list of files or directories to the object
;****************************************************************************
PRO runstep::addList,array, all=all, _Extra=extraKeyword
  
  for i=0, n_elements(array)-1 do begin
    if file_test(array[i],/directory) then begin
      self->addPath,array[i], all=keyword_set(all), _Extra=extraKeyword
    endif else begin
      if file_test(array[i]) then begin
        if keyword_set(all) then begin
	  self->addFile,array[i]
	  self->addFile,array[i], /ancillary
        endif else begin
	  self->addFile,array[i], _Extra=extraKeyword
	endelse
      endif
    endelse
  endfor
  
END

;****************************************************************************
;     AVG_DATA - Given a list of fits file names it average the data 
;****************************************************************************
FUNCTION runstep::avg_data,fnames
  
  self->Message,'Starting: ',priority='DEBUG',method='AVG_DATA'
  self->Message,'    - '+fnames,priority='DEBUG',method='AVG_DATA'
  
  Ncomb = 0
  for i=0,n_elements(fnames)-1 do begin
    if file_test(fnames[i]) eq 1 then begin
      readarr = readfits(fnames[i],hread,/silent)
      if n_elements(resarr) eq 0 then begin
        self->Message,'Reading first data to be included',priority='DEBUG',method='AVG_DATA'
	resarr = readarr
	hres = hread
	Ncomb = 1
      endif else begin
        if n_elements(resarr) eq n_elements(readarr) then begin
	  self->Message,[fnames[i],'   adding new data to the array'],priority='DEBUG',method='AVG_DATA'
	  resarr = resarr + readarr
	  Ncomb = Ncomb + 1
	endif else begin
	  self->Message,'Array size does not match previous data: '+fnames[i],priority='WARN',method='AVG_DATA'
	endelse
      endelse
    endif else begin
      self->Message,'File does not exist: '+fnames[i],priority='WARN',method='AVG_DATA'
    endelse
  endfor
  
  if Ncomb eq 0 then begin
    self->Message,'No valid fits file was found.',priority='WARN',method='AVG_DATA'
    return,{error:1, data:-1, header:''}
  endif
  resarr = resarr/Ncomb
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='AVG_DATA'
  
  return,{error:0, data:resarr, header:hres}
END

;****************************************************************************
;     COMBINE - combine images. By default indata is combined. It can be specified
;               to combine the ancillary or outdata.
;               We are assuming for now that they are 2D images. We could also
;               implements later things like looking at the exposure time
;               Also we should be able to implement a more sophisticated way than average
;****************************************************************************
FUNCTION runstep::combine, exclude=exclude, indexes=indexes, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='COMBINE'
  
  istheredata = self->IsThereData(_EXTRA=extraKeywords)
  
  if istheredata.error ne 0 then begin
    self->Message,'Bad data definition',priority='WARN',method='COMBINE'
    return, {error:1, data:-1, header:''}
  endif
  
  Ndata = istheredata.Ndata
  data = istheredata.data
  
  ; Check the indexes to be combined
  if keyword_set(indexes) then begin
    self->Message,'Input index array',priority='DEBUG',method='COMBINE'
    k = where(indexes lt Ndata ge 0)
    if k[0] eq -1 then begin
      self->Message,'Input index array does not contain valid elements',priority='WARN',method='COMBINE'
      return, {error:1, data:-1, header:''}
    endif
    idx = indexes[k]
  endif else begin
    idx = indgen(Ndata)
  endelse
  
  ; Check for excluded indexes
  if keyword_set(exclude) then begin
    idx = SetDifference(idx,exclude)
    if idx[0] eq -1 then begin
      self->Message,'All input indexes are excluded',priority='WARN',method='COMBINE'
      return, {error:1, data:-1, header:''}
    endif
  endif
  
  resavg = self->avg_data(data[idx])
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='COMBINE'
  ; Combine using average
  return, resavg
  
END


;****************************************************************************
;     RUN - start process
;****************************************************************************
PRO runstep::run, _EXTRA=extraKeyword
  
  if self.routine.name eq '' then begin
    self->Message,'Routine was not initialized',priority='ERR',method='RUN'
    return
  endif
  
  ok = execute('a='+self.routine.name+'()')
  if ok eq 0 then begin
    self->Message,'Error executing routine '+OBJ_CLASS(self.routine.name),priority='ERR',method='RUN'
    return
  endif
  
  self->Message,'Not yet implemented',priority='DEBUG',method='RUN'
  
END

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO runstep::plot
    
  if self.outdata eq ptr_new() then begin
    self->Message,'Indata array is null',priority='DEBUG',method='PLOT'
    return
  endif
  if n_elements(*self.outdata) eq 0 then begin
    self->Message,'Indata array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  self->Message,'Displaying '+(*self.outdata)[0],priority='INFO',method='PLOT'
    
  atv,(*self.outdata)[0]
  
END

;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runstep::cleanup
  
  if self.indata ne ptr_new() then ptr_free,self.indata
  if self.outdata ne ptr_new() then ptr_free,self.outdata
  if self.ancillary ne ptr_new() then ptr_free, self.ancillary
  if self.keylist ne ptr_new() then ptr_free, self.keylist
  if self.keyancillary ne ptr_new() then ptr_free, self.keyancillary
  if self.previous ne ptr_new() then ptr_free, self.previous
  
END

;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runstep::init, keycommon=keycommon, keylist=keylist, keyancillary=keyancillary, prevtype=prevtype, section=section, _Extra=extraKeyword
  
  if self->classdef::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (classdef)',priority='ERR',method='INIT'
    return,0
  endif
  
  ; Fill the keycommon keywords
  if keyword_set(keycommon) then begin
    if size(keycommon,/type) eq 7 then begin 
      Nkey = n_elements(keycommon)
      if Nkey gt 0 then begin
	keyarr = replicate({keyname:'',keyval:'',type:'same'},Nkey)
	for i=0,Nkey-1 do begin
          keyarr[i].keyname = keycommon[i]
	endfor
	self.keycommon = ptr_new(keyarr)
      endif
    endif else begin
      self.keycommon = ptr_new(keycommon)
    endelse
  endif
  
  ; Fill the keylist keywords
  if keyword_set(keylist) then begin
    if size(keylist,/type) eq 7 then begin 
      Nkey = n_elements(keylist)
      if Nkey gt 0 then begin
	keyarr = replicate({keyname:'',keyval:'',type:'same'},Nkey)
	for i=0,Nkey-1 do begin
          keyarr[i].keyname = keylist[i]
	endfor
	self.keylist = ptr_new(keyarr)
      endif
    endif else begin
      self.keylist = ptr_new(keylist)
    endelse
  endif
  
  ; Fill the keyancillary keywords
  if keyword_set(keyancillary) then begin
    if size(keyancillary,/type) eq 7 then begin 
      Nkey = n_elements(keyancillary)
      if Nkey gt 0 then begin
	keyarr = replicate({keyname:'',keyval:'',type:'same'},Nkey)
	for i=0,Nkey-1 do begin
          keyarr[i].keyname = keyancillary[i]
	endfor
	self.keyancillary = ptr_new(keyarr)
      endif
    endif else begin
      self.keyancillary = ptr_new(keyancillary)
    endelse
  endif
  
  if not keyword_set(prevtype) then prevtype='single'
  prevtype = strlowcase(prevtype)
  
  ; Here we check that the value (if introduced by the user) is within the valid values
  ; if not set single by default
  k = where(['single','multiple'] eq prevtype)
  if k[0] eq -1 then self.prevtype = 'single' else self.prevtype = prevtype
  
  ; Definition of the section. For now I am going to take default values for the MLOF pipeline
  if keyword_set(section) then begin
    auarr1 = [13,19,400,980]
    auxarr2 = [55,15,980,980]
    self.section = ptr_new([[auxarr1],[auxarr2]])
  endif
  
  return, 1
  
END

;****************************************************************************
;     RUNSTEP__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runstep__define
  
  routinestruct = {routinestruct,name:'',type:''} 
  
  struct={runstep, $
          prevtype:'', $                                       ; Type of previous gathering. Single or Mutiple. 
	  previous:ptr_new(), $                                ; Pointer to array of previous objects in the pipeline from which the data should be taken
	  routine: routinestruct, $                            ; Name and type (TBD) of the routine to be run
	  keycommon: ptr_new(), $                              ; Array with list of keywords to check defining properties of fits files common to indata and ancillary
	  keylist: ptr_new(), $                                ; Array with list of keywords to check defining properties of fits files in indata
	  keyancillary: ptr_new(), $                           ; Array with list of keywords to check defining properties of fits files in ancillary
	  indata: ptr_new(), $                                 ; Array of names of input data of the processing step
          outdata: ptr_new(), $                                ; Array of names of output products of the processing step
	  ancillary: ptr_new(), $                              ; Array of names of ancillary data used during processing
	  sections:ptr_new(), $                                ; Pointer to an array of four element arrays with the description of the valid image
	  interactive:0, $                          	       ; Defines if the process is done in an interactive way. 
							       ; Some inherit classes may not have an interactive approach. Various values can be used to freeze the windows.
          inherits classdef}
END
