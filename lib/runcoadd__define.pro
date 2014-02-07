; NAME:
;     RUNCOADD - Version 1.0
;
; PURPOSE: Coadd all the input images
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
;     RUN - start process
;****************************************************************************
PRO runcoadd::run, _EXTRA=extraKeywords
  
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
    
  ; Create output names 
  self.outdata = ptr_new([self->createFname(data[0], _EXTRA=extraKeywords)])
  
  datacomb = self->Combine()
  hout = datacomb.header
  sxaddpar, hout, 'OBJCLASS',OBJ_CLASS(self)
  writefits,(*self.outdata)[0],datacomb.data,hout
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END


;****************************************************************************
;     RUNDARK__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runcoadd__define
  
  struct={runcoadd, $
	  inherits runstep}
END
