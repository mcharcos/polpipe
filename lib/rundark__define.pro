; NAME:
;     RUNDARKD - Version 1.0
;
; PURPOSE: Remove dark current from an image
;
; CLASS ATTRIBUTES:       
;
; CLASS METHODS:
;     + Run: Start the process
;     
; INHEREITS METHODS:
;     + From RUNSTEP
;         Here, ancillary will contain the dark images
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 16th 2013



;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION rundark::createFname, fnames, masterdark=masterdark, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
    
  if keyword_set(masterdark) then begin
    dirname = file_dirname(fnames[0],/mark_directory)
    ;if dirname ne '' then dirname = dirname + '/'
    rootname = file_basename(fnames[0])
    pos = strpos(rootname,'.',/reverse_search)
    rootname = strmid(rootname,0,pos)
    
    prodpath = self->createPath(dirname, _EXTRA=extraKeywords)
  
    return, prodpath+rootname+'_masterdark.fit'
  endif
  
  self->Message,'Switch to parent method (CREATEFNAME)',priority='DEBUG',method='CREATEFNAME'
  return, self->runstep::CreateFname(fnames, _EXTRA=extraKeywords)
  
END

;****************************************************************************
;     MASTERDARK - Create a master dark from the input darks
;****************************************************************************
PRO rundark::masterdark, _EXTRA=extraKeywords
  
  self->Message,['::::::::::::::::::::::::','Starting...'],priority='DEBUG',method='masterdark'
  
  darkcomb = self->Combine(/ancillary)
  if darkcomb.error eq 1 then begin
    self->Message,'Error creating master dark',priority='WARN',method='masterdark'
    return
  endif
  
  ; we need to normalize it first
  ; self->Normalize in runstep?
  ; Remember there is something about sections of the images. 
  ;     - Are we going to save each section in different files or
  ;     - check the section each time we open the entire fits image
  
  if self.masterdark eq '' then begin
    self->Message,'Master dark array was empty... creating new file name',priority='DEBUG',method='masterdark'
    self.masterdark = self->createFname(*self.ancillary,/masterdark, _EXTRA=extraKeywords)  ; we need something smarter I believe
  endif
  
  h = darkcomb.header
  sxaddpar, h, 'OBJCLASS',OBJ_CLASS(self)
  sxaddpar, h, 'OBJECT','MASTERDARK'
  self->Message,'Saving masterdark '+self.masterdark,priority='DEBUG',method='masterdark'
  writefits,self.masterdark,darkcomb.data,h
  
  ; Check if we really created the fits file. If not, set self.masterdark to null
  if file_test(self.masterdark) eq 0 then begin
    self->Message,'Problem writing master dark',priority='WARN',method='masterdark'
    self.masterdark = ''
  endif
  
  self->Message,'DONE',priority='DEBUG',method='masterdark'
  
END

;****************************************************************************
;     RUN - start process
;****************************************************************************
PRO rundark::run, _EXTRA=extraKeywords
  
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
  
  ; Create masterdark
  self->masterdark, _EXTRA=extraKeywords
  if self.masterdark eq '' then begin
    self->Message,['Master dark was not created', $
                   'Data is going to be output without any dark correction', $
		   'You may want to review your dark images'],priority='WARN',method='RUN'
    self.outdata = ptr_new(*self.indata)
    return
  endif
  
  ; Create output names 
  self.outdata = ptr_new([self->createFname(data[0], _EXTRA=extraKeywords)])
  for i=1,Ndata-1 do *self.outdata = [*self.outdata,self->createFname(data[i], _EXTRA=extraKeywords)]
  
  ; Read master dark
  self->Message,'Reading fits file '+self.masterdark,priority='DEBUG',method='RUN'
  imdark = readfits(self.masterdark,hdark,/silent)
  
  for i=0,Ndata-1 do begin
    imin = readfits((*self.indata)[i],hin,/silent)
    imout = imin-mean(imdark)
    hout = hin
    sxaddpar, hout, 'OBJCLASS',OBJ_CLASS(self)
    writefits,(*self.outdata)[i],imout,hout
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END


;****************************************************************************
;     RUNDARK__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO rundark__define
  
  struct={rundark, $
          masterdark: '', $
	  inherits runstep}
END
