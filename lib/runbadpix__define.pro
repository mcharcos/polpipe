; NAME:
;     RUNBADPIX - Version 1.0
;
; PURPOSE: Remove bad pixels from an image
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
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 25th 2013



;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION runbadpix::createFname, fnames, bpmask=bpmask, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
    
  if keyword_set(bpmask) then begin
    dirname = file_dirname(fnames[0],/mark_directory)
    self->Message,'DIRNAME = '+dirname,priority='DEBUG',method='CREATEFNAME'
    ;if dirname ne '' then dirname = dirname + '/'
    rootname = file_basename(fnames[0])
    self->Message,'ROOTNAME = '+rootname,priority='DEBUG',method='CREATEFNAME'
    pos = strpos(rootname,'.',/reverse_search)
    rootname = strmid(rootname,0,pos)
    self->Message,'ROOTNAME = '+rootname,priority='DEBUG',method='CREATEFNAME'
    
    ;prodpath = self->createPath(dirname, _EXTRA=extraKeywords)
    prodpath=dirname
    self->Message,'PRODPATH = '+prodpath,priority='DEBUG',method='CREATEFNAME'
    
    return, prodpath+rootname+'_bpmask.fit'
  endif
  
  self->Message,'Switch to parent method (CREATEFNAME)',priority='DEBUG',method='CREATEFNAME'
  return, self->runstep::CreateFname(fnames, _EXTRA=extraKeywords)
  
END

;****************************************************************************
;     BPMASK - Create a master dark from the input darks and then an badpixel mask
;              darkcomb={data:float[2], header:string[], fname:String}
;****************************************************************************
PRO runbadpix::bpmask, darkcomb=darkcomb, fast=fast, _EXTRA=extraKeywords
  
  self->Message,['::::::::::::::::::::::::','Starting...'],priority='DEBUG',method='bpmask'
  
  if not keyword_set(darkcomb) then begin
    darkcomb = self->Combine(/ancillary)
    if darkcomb.error eq 1 then begin
      self->Message,'Error creating master dark',priority='WARN',method='bpmask'
      return
    endif
    
    self->Message,'Checking bad pixel file name: '+self.bpmask,priority='DEBUG',method='bpmask'
    if self.bpmask eq '' then begin
      self->Message,'Master dark array was empty... creating new file name',priority='DEBUG',method='bpmask'
      self.bpmask = self->createFname(*self.ancillary,/bpmask, _EXTRA=extraKeywords)  ; we need something smarter I believe
    endif
  endif else begin
    self.bpmask = self->createFname(darkcomb.fname,/bpmask, _EXTRA=extraKeywords)
  endelse
  
  s = size(darkcomb.data)
  maskarr = replicate(0.,s[1],s[2])
  meanval = median(darkcomb.data)
  sigma = stddev(darkcomb.data)
  
  self->Message,['DARK MEAN = '+strtrim(meanval,2),'DARK SIGMA = '+strtrim(sigma,2)],priority='DEBUG',method='bpmask'
  
  ; If the file already exist, we don't create it again
  if file_test(self.bpmask) then begin
    self->Message,'Bad pixel mask already exist',priority='INFO',method='bpmask'
    return
  endif
  
  ; Create a dark that should be close to 0
  ; From this we will check what pixels are outside Nsigma times the standard deviation
  if keyword_set(fast) then begin
    dark0 = abs(darkcomb.data - meanval)
    k = where(dark0 gt self.Nsigma*sigma) 
  endif else begin
    k = find_outliers(darkcomb.data, self.boxwidth, N_SIGMA=self.Nsigma,N_FOUND=nfound,DEBUG=self.debug)
  endelse
  if k[0] ne -1 then begin
    self->Message,'Bad pixel mask contains '+strtrim(n_elements(k),2)+' bad pixels',priority='DEBUG',method='BPMASK'
    maskarr[k] = 1.
  endif
  
  h = darkcomb.header
  sxaddpar, h, 'OBJCLASS',OBJ_CLASS(self)
  sxaddpar, h, 'OBJECT','BADPIXMASK'
  self->Message,'Saving bad pixel mask '+self.bpmask,priority='DEBUG',method='bpmask'
  writefits,self.bpmask,maskarr,h
  
  ; Check if we really created the fits file. If not, set self.bpmask to null
  if file_test(self.bpmask) eq 0 then begin
    self->Message,'Problem writing bad pixel mask',priority='WARN',method='bpmask'
    self.bpmask = ''
  endif
  
  self->Message,'DONE',priority='DEBUG',method='bpmask'
  
END

;****************************************************************************
;     CORRECTBP - Returns the array with the specific badpixel correction
;****************************************************************************
FUNCTION runbadpix::correctbp, data, index
  
  ; Read the size of the input data
  s = size(data)
  
  ; Check the row and the columns based on the input value
  if n_elements(index) eq 2 then begin
    xi = index[0]
    yi = index[1]
  endif else begin
    yi = index/s[1]
    xi = index mod s[1]
  endelse
  
  pixnext = [[xi-1,yi-1],[xi-1,yi],[xi-1,yi+1], $
             [xi,yi-1],[xi,yi+1], $
             [xi+1,yi-1],[xi+1,yi],[xi+1,yi+1]]
  
  resval = 0.
  totpix = 0.
  for i=0,n_elements(pixnext[0,*])-1 do begin
    if pixnext[0,i] ge 0 and pixnext[0,i] lt s[0] and pixnext[1,i] ge 0 and pixnext[1,i] lt s[1] then begin
      resval = resval + data[pixnext[0,i],pixnext[1,i]]
      totpix = totpix + 1.
    endif
  endfor
  if totpix gt 0. then resval = resval/totpix
  
  resdata = data
  resdata[xi,yi] = resval
  
  return,resdata
  
END
 

;****************************************************************************
;     GETCONFIGURATION - Returns an array of structures with the attributes
;                        that configurable
;****************************************************************************
FUNCTION runbadpix::GetConfiguration
  
  self->Message,'Starting',priority='DEBUG',method='GETCONFIGURATION'
  
  print,self.Nsigma,'==============='
  resarr = {name:'Nsigma',value:strtrim(self.Nsigma,2),type:'float',error:0}
    
  resparent = self->runstep::GetConfiguration()
  if resparent[0].error ne 1 then begin
    resarr = [resarr,resparent]
  endif
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='GETCONFIGURATION'
  return ,resarr
  
END

;****************************************************************************
;     RUN - start process
;****************************************************************************
PRO runbadpix::run, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='RUN'
  
  if self.routine.name eq '' then begin
    self->Message,'Routine was not initialized',priority='INFO',method='RUN'
    ;return
  endif
  
  ; Check if there is input data
  istheredata = self->IsThereData()
  if istheredata.error ne 0 then begin
    self->Message,'Bad data definition',priority='WARN',method='RUN'
    return
  endif
  self->Message,'Done with ISTHERDATA',priority='DEBUG',method='RUN'
  
  Ndata = istheredata.Ndata
  data = istheredata.data
  
  ; What ever happen, at this point we assume that we are going to make 
  ; a new list of output files. So, we delete the old pointer
  if self.outdata ne ptr_new() then ptr_free,self.outdata
  

  ; This is used to know if the if the darks should be used to make the badpixel mask or each input file.
  ; If ancillary files are in they should be dark and thus, they are used to calculate the bad pixel mask
  usedark=0
  
  if file_test(self.inbpmask) then begin
    usedark=2
    self.bpmask = self.inbpmas
  endif else begin
    isthereancillary = self->IsThereData(/ancillary)
    if isthereancillary.error eq 0 then begin
      ; Create bad pixel mask
      self->Message,'Calling BPMASK',priority='DEBUG',method='RUN'
      self->bpmask, _EXTRA=extraKeywords
      if self.bpmask eq '' then begin
        self->Message,['Bad pixel mask  was not created', $
                       'Data is going to be output without any bad pixel correction', $
                       'You may want to review your dark images'],priority='WARN',method='RUN'
        self.outdata = ptr_new(*self.indata)
        return
      endif
    endif
  endelse
  
  if usedark ne 0 then begin    
    ; Read master dark
    self->Message,'Reading fits file '+self.bpmask,priority='DEBUG',method='RUN'
    bpmaskarr = readfits(self.bpmask,hdark,/silent)
    k = where(bpmaskarr eq 1.)
    if k[0] ne -1 then begin
      Nbp = n_elements(k)
      self->Message,'Number of bad pixles is '+strtrim(Nbp,2),priority='DEBUG',method='RUN'
      usedark = 1
    endif else begin
      Nbp = 0
      self->Message,'No bad pixels',priority='DEBUG',method='RUN'
      usedark = -1
    endelse
  endif
  
  ; Create output names 
  self.outdata = ptr_new([self->createFname(data[0], _EXTRA=extraKeywords)])
  for i=1,Ndata-1 do *self.outdata = [*self.outdata,self->createFname(data[i], _EXTRA=extraKeywords)]
  
  
  for i=0,Ndata-1 do begin
    imin = readfits((*self.indata)[i],hin,/silent)
    imout = imin
    
    ; If we did not calculate the badpixel mask before from the darks,
    ; then it is time to use the current image to do so
    if usedark eq 0 then begin
      self->bpmask, darkcomb={data:imout,header:hin,fname:(*self.indata)[i]}, _EXTRA=extraKeywords
      if self.bpmask eq '' then begin
        self->Message,['Bad pixel mask  was not created for '+(*self.indata)[i], $
                       'Data is going to be output without any bad pixel correction', $
                       'You may want to review your dark images'],priority='WARN',method='RUN'
        k = -1
        Nbp=0
      endif else begin
        self->Message,'Reading fits file '+self.bpmask,priority='DEBUG',method='RUN'
        bpmaskarr = readfits(self.bpmask,hdark,/silent)
        k = where(bpmaskarr eq 1.)
        Nbp = n_elements(k)
      endelse
    endif
    
    for j=0,Nbp-1 do begin
      imout = self->correctbp(imout,k[j])
    endfor
      
    hout = hin
    sxaddpar, hout, 'OBJCLASS',OBJ_CLASS(self)
    writefits,(*self.outdata)[i],imout,hout
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END

;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runbadpix::init, Nsigma=Nsigma, boxwidth=boxwidth, bpmask=bpmask, _Extra=extraKeyword
  
  if self->runstep::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (runstep)',priority='ERR',method='INIT'
    return,0
  endif
  
  if not keyword_set(Nsigma) then self.Nsigma = 3. else self.Nsigma = Nsigma
  if not keyword_set(boxwidth) then self.boxwidth = 11. else self.boxwidth = boxwidth
  if not keyword_set(bpmask) then self.inbpmask = '' else self.inbpmask = bpmask
  
  return,1
  
END



;****************************************************************************
;     RUNBADPIX__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runbadpix__define
  
  struct={runbadpix, $
          Nsigma:5., $              ; Number of sigma over the average to identify the badpixels
          boxwidth:11.0, $          ; box size for identifying bad pixels
          inbpmask: '', $           ; Input bad pixel mask that the user want to use
          bpmask: '', $
	  inherits runstep}
END
