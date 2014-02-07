; NAME:
;     RUNFLATFIELD - Version 1.0
;
; PURPOSE: Remove flat field artifacts from an image
;
; CLASS ATTRIBUTES:       
;
; CLASS METHODS:
;     + Run: Start the process
;     
; INHEREITS METHODS:
;     + From RUNSTEP
;         Here, ancillary will contain the flat images
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, January 22nd 2013



;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION runflatfield::createFname, fnames, masterflat=masterflat, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
    
  if n_elements(masterflat) gt 0 then begin
    dirname = file_dirname(fnames[0])
    if dirname ne '' then dirname = dirname + '/'
    rootname = file_basename(fnames[0])
    pos = strpos(rootname,'.',/reverse_search)
    rootname = strmid(rootname,0,pos)
    
    prodpath = self->createPath(dirname, _EXTRA=extraKeywords)
  
    return, prodpath+rootname+'_'+masterflat+'masterflat.fits'
  endif
  return, self->runstep::CreateFname(fnames, _EXTRA=extraKeywords)
  
END


;****************************************************************************
;     INDEXMASTERFLAT - Fill the arrays with the indexes for each flat lists
;****************************************************************************
PRO runflatfield::IndexMasterflat, exposures=exposures, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='INDEXMASTERFLAT'
  
  ; We clean what ever master flat list we previously had
  if self.masterflat ne ptr_new() then begin
    ptr_free, self.masterflat
    self.masterflat = ptr_new()
  endif
  
  istheredata = self->IsThereData(/ancillary)
  if istheredata.error ne 0 then begin
    self->Message,'No flat fields were loaded',priority='WARN',method='RUN'
    return
  endif
  
  ; Structure defining a master flat for a specific range of exposure times
  ; fname = file name of the recorded file
  ; texp = maximum value of the exposure time that this flat serves
  ; im, header = array and header of the master flat
  ; idx = Indexes of the flats used in the ancillary array
  defres = {fname:'',texp:9999.,im:ptr_new(), header:ptr_new(), idx:ptr_new(indgen(istheredata.Ndata))}
  if not keyword_set(exposures) then begin
    self->Message,'One single flat requested',priority='INFO',method='INDEXMASTERFLAT'
    
    defres.fname = self->createFname(*self.ancillary,masterflat='0-9999', _EXTRA=extraKeywords)
    self.masterflat = ptr_new([defres])
    return
  endif
  
  ; Create an array with the exposure times of the ancillary data
  i=0
    im = readfits(istheredata.data[i],h)
    exparr=sxpar(h,'EXPTIME')
  for i=1,istheredata.Ndata-1 do begin
    im = readfits(istheredata.data[i],h)
    exparr=[exparr,sxpar(h,'EXPTIME')]
  endfor
  
  ; Remove the badflats from the list
  ; For this, I just set the exposure times to -1 and they won't be taken
  ; later when comparing to the range of the flat fields in the list
  ; I assume that the badflats list contain indexes within the appropriate range
  if self.badflats ne ptr_new() then begin
    if n_elements(*self.badflats) gt 0 then begin
      exparr[*self.badflats] = -1.
    endif
  endif
  
    
  ; Build the resulting array and fill it with the indexes of ancillary data according to their exposure times
  expsort=exposures[sort(exposures)]
  resarr = replicate(defres,n_elements(expsort)+1)
  prevt = 0
  for i=0,n_elements(expsort)-1 do begin  
    resarr[i].fname = self->createFname(*self.ancillary,masterflat=strtrim(prevt,2)+'-'+strtrim(expsort[i],2), _EXTRA=extraKeywords)
    resarr[i].texp=expsort[i]
    
    idxarr = where(exparr gt prevt and exparr le expsort[i])
    if idxarr[0] ne -1 then resarr[i].idx = ptr_new(idxarr) else resarr[i].idx = ptr_new()
    
    prevt = expsort[i]
  endfor
  
  ; Fill the last one which is all that are bigger than the previous exposure time
  i = n_elements(expsort)
    resarr[i].fname = self->createFname(*self.ancillary,masterflat=strtrim(prevt,2)+'-9999', _EXTRA=extraKeywords)
    resarr[i].texp = 9999.
    idxarr = where(exparr gt prevt)
    if idxarr[0] ne -1 then resarr[i].idx = ptr_new(idxarr) else resarr[i].idx = ptr_new()
  
  self.masterflat = ptr_new(resarr)
  
END

;****************************************************************************
;     MASTERFLAT - Create a master flat from the input flatfields
;****************************************************************************
PRO runflatfield::masterflat, _EXTRA=extraKeywords
  
  self->Message,['Starting...'],priority='DEBUG',method='MASTERFLAT'
  
  ; We fill the indexes of the array before starting creating the master flat for each exposure value
  self->IndexMasterflat, _EXTRA=extraKeywords
  
  if self.masterflat eq ptr_new() then begin
    self->Message,'Master flat array was null',priority='DEBUG',method='MASTERFLAT'
    return
  endif
  
  if n_elements(*self.masterflat) eq 0 then begin
    self->Message,'Master flat array was empty',priority='DEBUG',method='MASTERFLAT'
    return
  endif
  
  ; Before starting with the master flats, we check interactively the flats
  ; if the user requested interactive mode
  if self.interactive eq 10 then begin
    self->Plot
    
    ; Regroup the data with the new bad flats selected interactively by the user
    self->IndexMasterflat, _EXTRA=extraKeywords
  endif
  
  
  for i=0,n_elements(*self.masterflat)-1 do begin
    curmasterflat = (*self.masterflat)[i]
    if curmasterflat.idx ne ptr_new() then begin
      ; If the file already existed we just read it and if not, we combine the flats in the ancillary array
      if file_test(curmasterflat.fname) then begin
        self->Message,'Master flat already exist: '+curmasterflat.fname,priority='INFO',method='MASTERFLAT'
        curmasterflat.im = ptr_new(readfits(curmasterflat.fname, h, /silent))
        curmasterflat.header = ptr_new(h)
      endif else begin
        flatcomb = self->Combine(/ancillary, indexes=*curmasterflat.idx)
        if flatcomb.error eq 1 then begin
          self->Message,'Error creating master flat for exposure '+strtrim(curmasterflat.texp,2),priority='WARN',method='MASTERFLAT'
          curmasterflat.im = ptr_new()
          curmasterflat.header = ptr_new()
          
          ; TODO:
          ; May be we can compare the flats used here and for the saved file
        endif else begin
          fname = curmasterflat.fname
          h = flatcomb.header
          flatdata = flatcomb.data / mean(flatcomb.data)
                    
          ; Check if there are any 0 values in imflat to avoid infinity numbers in the array
          kinf=where(flatdata eq 0.)
          
          ; TODO:
          ; For now I am going to set the data to 1 where it was 0 but we may want to interpolate
          ; between nearby pixels
          if kinf[0] ne -1 then flatdata[kinf] = 1.
          
          ; Add to the header the paths and the base names of the file names
          ; of flats used for the masterflat
          for ifanc=0,n_elements(*curmasterflat.idx)-1 do begin
            fanc = (*self.ancillary)[(*curmasterflat.idx)[ifanc]]
            ; Add filename
            sxaddpar,h,'FFLAT', file_basename(fanc)
            
            ; Add path
            h = self->SxAddLongStr(h,'PFLAT', file_dirname(fanc))
          endfor
          
          
          sxaddpar, h, 'OBJCLASS',OBJ_CLASS(self)
          sxaddpar, h, 'OBJECT','MASTERFLAT'
          self->Message,'Saving Masterflat '+fname,priority='DEBUG',method='MASTERFLAT'
          writefits,fname,flatdata,h
          
          curmasterflat.im = ptr_new(flatdata)
          curmasterflat.header = ptr_new(h)
          
          ; Check if we really created the fits file. If not, set self.masterflat to null
          if file_test(fname) eq 0 then begin
            self->Message,'Problem writing master flat '+fname,priority='ERR',method='MASTERFLAT'
          endif
        endelse
      endelse
      (*self.masterflat)[i] = curmasterflat
    endif
  endfor
  
  self->Message,'DONE',priority='DEBUG',method='MASTERFLAT'
  
END

;****************************************************************************
;     RUN - start process. Extrakeywords may include exposures for different types of flats
;****************************************************************************
PRO runflatfield::run, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='RUN'
  
  if self.routine.name eq '' then begin
    self->Message,'Routine was not initialized',priority='WARN',method='RUN'
    ;return
  endif
  
  ; Check if there is input data
  istheredata = self->IsThereData(_EXTRA=extraKeywords)
  if istheredata.error ne 0 then begin
    self->Message,'Bad data definition',priority='WARN',method='RUN'
    return
  endif
  
  Ndata = istheredata.Ndata
  data = istheredata.data
  
  ; What ever happen, at this point we assume that we are going to make 
  ; a new list of output files. So, we delete the old pointer
  if self.outdata ne ptr_new() then ptr_free,self.outdata
  
  ; Create masterflat and check all possible wrong cases with the results of the master flat creation
  ; I am very conservative here and I may be killing too much the possible scenarios
  self->Masterflat, _EXTRA=extraKeywords
  errmsg = ['Data is going to be output without any flat correction', $
     	    'You may want to review your flat images']
  self.outdata = ptr_new(*self.indata)
  if self.masterflat eq ptr_new() then begin
    self->Message,['Master flat was not created (null)', errmsg],priority='WARN',method='RUN'
    return
  endif
  Nmasterflats = n_elements(*self.masterflat)
  if Nmasterflats eq 0 then begin
    self->Message,['Master flat was not created (empty)', errmsg],priority='WARN',method='RUN'
    return
  endif
  ptrflat = (*self.masterflat).im
  if Nmasterflats eq 1 and ptrflat[0] eq ptr_new() then begin
    self->Message,['Master flat was not created (null image)', errmsg],priority='WARN',method='RUN'
    return
  endif
  
  ; At this point we assume that the masterflat list has all required flats
  ; ordered by exposure time and does not contain any 0 value so we don't get
  ; infinity when dividing
  
  ; Create output names 
  self.outdata = ptr_new([self->createFname(data[0], _EXTRA=extraKeywords)])
  for i=1,Ndata-1 do *self.outdata = [*self.outdata,self->createFname(data[i], _EXTRA=extraKeywords)]
  
  exparr = (*self.masterflat).texp  
  ;imflat = (*ptrflat)[0]
  
  docontinue = 'Yes'
  for i=0,Ndata-1 do begin
    imin = readfits((*self.indata)[i],hin,/silent)
    
    imout = imin
    hout = hin
    
    ; Default values in case there is no masterflat
    sxaddpar, hout, 'MFFLAT', 'NotAvailable'
    sxaddpar, hout, 'MPFLAT', 'NotAvailable'
    
    if Nmasterflats gt 1 then begin
      ctexp = sxpar(hin,'EXPTIME')
      k = min(where(exparr gt ctexp))
      
      noflat=1
      
      ; we don't really need to check k since there should be always a exparr value of 9999,
      ; but for sanity... just in case some header contains crazy values
      if k[0] ne -1 then begin
        if (*self.masterflat)[k[0]].im ne ptr_new() then begin
          imout = imin/(*(*self.masterflat)[k[0]].im)
          
          ; take the (*self.masterflat)[k[0]]. and complete the hout header
          sxaddpar, hout, 'MFFLAT', file_basename((*self.masterflat)[k[0]].fname)
          hout = self->SxAddLongStr(hout, 'MFFLAT', file_dirname((*self.masterflat)[k[0]].fname))
          noflat=0
        endif 
      endif 
      
      if noflat eq 1 then begin
        if self.interactive eq 1 and docontinue eq 'Cancel' then begin
          docontinue = dialog_message(['File '+(*self.indata)[i],'does not contain a matching masterflat','Would you like to continue?', $
                                '','(Press Cancel to not show this message again)'], /QUESTION,/CANCEL)
          if docontinue eq 'No' then begin
            self->Message,'Process aborted per user request',priority='WARN',method='RUN'
            return
          endif
        endif
      endif
    endif
    
    sxaddpar, hout, 'OBJCLASS',OBJ_CLASS(self)
    writefits,(*self.outdata)[i],imout,hout
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END


;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runphotometry::cleanup
  
  self->runstep::cleanup

  ; TODO:
  ; clean masterflat pointers
  if self.masterflat ne ptr_new() then begin
    for i=0,n_elements(*self.masterflat)-1 do begin
      cm = (*self.masterflat)[i]
      if cm.im ne ptr_new() then ptr_free,cm.im
      if cm.header ne ptr_new() then ptr_free,cm.header
      if cm.idx ne ptr_new() then ptr_free,cm.idx
    endfor
    ptr_free,self.masterflat
  endif
  if self.badflats ne ptr_new() then ptr_free,self.badflats
  
END


;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;      GRAPHIC METHODS FOR INTERACTIVE MODE
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO runflatfield::plot, winnum
  
  self->Message,'Starting',priority='DEBUG',method='PLOT'
  self->Message,'Interactive mode is not yet implemented',priority='WARN',method='PLOT'
  
  istheredata = self->IsThereData(/ancillary)
  if istheredata.error ne 0 then begin
    self->Message,'No flat fields were loaded',priority='WARN',method='RUN'
    return
  endif
  
  if self.badflats ne ptr_new() then begin
    if n_elements(*self.badflats) eq 0 then badflats=-1 else badflats=*self.badflats
    ptr_free,self.badflats
  endif else begin
    badflats=-1
  endelse
  
  for i=0,n_elements(istheredata.Ndata)-1 do begin
    atv_plus,istheredata.data[i]
    
    k = where(badflats eq i)
    if k[0] eq -1 then begin
      doremove = dialog_message(['This flat is flagged as good','Would you like to remove this flat from the list?', $
                                 'Cancel will return the list of bad flats that you selected until now'], /QUESTION, /CANCEL)
    endif else begin
      doremove = dialog_message(['This flat is flagged as bad','Would you like to keep it as bad flat?', $
                                 'Cancel will return the list of bad flats that you selected until now'], /QUESTION, /CANCEL)
    endelse
    
    if doremove eq 'Yes' then begin
      if n_elements(resbadflats) eq 0 then resbadflats = i else resbadflats = [resbadflats,i]
    endif
    if doremove eq 'Cancel' then begin
      i = n_elements(istheredata.Ndata)
    endif
  endfor
  
  if n_elements(resbadflats) gt 0 then self.badflats = ptr_new(resbadflats)
  
  ; TODO:
  ; Here we should allow the user to browse over each group with atv and remove flats from the groups
  ; if necessary.
  ; May be as for polarimetry, we can show a plot (background of flats?) and allow to select points.
  ; It may require to push the plot_event routine to the runstep class
  
END



;****************************************************************************
;     RUNFLATFIELD__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runflatfield__define
  
  struct={runflatfield, $
          masterflat: ptr_new(), $  ; Pointer to array containing the file names of the master flats, exposure times, arrays, indexes of flats involves.
          badflats:ptr_new(), $     ; List of indexes of ancillary files to be removed from the combination list. Usually used in interactive mode
	  inherits runstep}
END
