; NAME:
;     RUNPOLARIMETRY - Version 1.0
;
; PURPOSE: Remove instrumental polarization current from an image
;
; CLASS ATTRIBUTES:       
;
; CLASS METHODS:
;     + Run: Start the process
;     
; INHEREITS METHODS:
;     + From RUNSTEP
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, February 6th 2014



;****************************************************************************
;     DEFAULTINSTPOL - Return a default structure storing the data of the 
;                        photometry of a star
;****************************************************************************
FUNCTION runinstrumentalpol::DefaultInstPol, star, _EXTRA=extraProperties
  
  defaultphot = {name:'', $
                 fname:'',  $
                 path:'',  $
                 filter:'', $
                 pol:0.,  $
                 epol:0.,  $
                 xi:0.,  $
                 exi:0.,  $
                 pmod:0.,  $
                 ximod:0.,  $
                 bad:0,    $
                 date:'' $
                 }

  resphot = fillstructure(defaultphot,_EXTRA=extraProperties)
  
  return, resphot
  
END

;****************************************************************************
;     ADDSTD - Given a file name, the method figures out if it is a
;                 polarization or no-polarized standard and complete the attributes
;                 from the information of the header
;****************************************************************************
PRO runinstrumentalpol::AddStd, fname, _EXTRA=extraKeywords
  
  if not file_test(fname) then begin
    self->Message,'The file name do not exist: '+fname,priority='WARN', method='ADDSTD'
    return
  endif
  
  im = readfits(fname,h)
  inname = strlowcase(strtrim(sxpar(h,'OBJECT'),2))
  filter = strtrim(sxpar(h,'FILTER'),2)
  pollev = sxpar(h,'POLLEV0')
  epollev = sxpar(h,'ePOLLEV0')
  polang = sxpar(h,'POLANG0')
  epolang = sxpar(h,'ePOLANG0')
  
  stdarr = [self.polstd,self.nopolstd]
  stdlistarr = [self.polstdlist,self.nopolstdlist]
  
  for i=0,n_elements(stdlistarr)-1 do begin
    curlist = stdlistarr[i]
    if curlist ne ptr_new() then begin
        namearr = strlowcase((*curlist).name)
        k = where(inname eq namearr)
        if k[0] ne -1 then begin
            *(stdarr[i])=[*(stdarr[i]),self->DefaultInstPol(name=inname, fname=fname, filter=filter, pol=pollev,epol=epollev,xi=polang,exi=epolang)]
        endif
    endif
  endfor
  
  self->Message,'DONE',priority='DEBUG',method='ADDSTD'
    
END

;****************************************************************************
;     REFSTD - Given a file name, the method figures out if it is a
;                 polarization or no-polarized standard and complete the attributes
;                 from the information of the header
;****************************************************************************
PRO runinstrumentalpol::RefStd
  
  stdarr = [self.polstd,self.nopolstd]
  stdlistarr = [self.polstdlist,self.nopolstdlist]
  
  for i=0,n_elements(stdlistarr)-1 do begin
    curlist = stdlistarr[i]
    if n_elements(*curlist) gt 2 then begin
        (*curlist)[0] = (*curlist)[1]
    endif
  endfor
  
  self->Message,'DONE',priority='DEBUG',method='REFSTD'
    
END

;****************************************************************************
;     POLCORRECTION - Perform correction of polarization standard
;****************************************************************************
FUNCTION runinstrumentalpol::polcorrection, header
  
  
  
  return, header
  
END

;****************************************************************************
;     UNPOLCORRECTION - Perform correction of polarization standard
;****************************************************************************
FUNCTION runinstrumentalpol::unpolcorrection, header
  
  hout = header
  inname = strlowcase(strtrim(sxpar(header,'OBJECT'),2))
  pollev = sxpar(header,'POLLEV0')
  epollev = sxpar(header,'ePOLLEV0')
  polang = sxpar(header,'POLANG0')
  epolang = sxpar(header,'ePOLANG0')
  
  q = pollev*cos(2.*polang/180.*!pi)
  u = pollev*sin(2.*polang/180.*!pi)
  qstd = (*self.nopolstd)[0].pol * cos(2.*(*self.nopolstd)[0].pol/180*!pi)
  ustd = (*self.nopolstd)[0].pol * sin(2.*(*self.nopolstd)[0].pol/180*!pi)
  
  ; presumably, there should be observed in the same frame
  qcorr = qstd
  ucorr = ustd
  
  ; Now write the new values to the output header
  ; TODO: add the error measurements
  polcorr = sqrt(qcorr^2 + ucorr^2)
  polangcorr = 0.5 * atan(ucorr/qcorr)
  sxaddpar,hout,'UCPLEV0',polcorr
  sxaddpar,hout,'UCEPLEV0',0
  sxaddpar,hout,'UCPANG0',polangcorr
  sxaddpar,hout,'UCEPANG0',0
  
  ; TODO: May be later add the information of the specific standards used for the calibration
  
  
  return, hout
  
END


;****************************************************************************
;     RUN - start process
;****************************************************************************
PRO runinstrumentalpol::run, _EXTRA=extraKeywords
  
  ; Check if there is input data
  istheredata = self->IsThereData(_EXTRA=extraKeywords)
  if istheredata.error ne 0 then begin
    self->Message,'Bad data definition',priority='WARN',method='RUN'
    return
  endif
  
  Ndata = istheredata.Ndata
  data = istheredata.data
  
  ; Clean the output file names
  if self.outdata ne ptr_new() then begin
    ptr_free,self.outdata
    self.outdata = ptr_new()
  endif
  
  ; Check the input data and populate the polarized and unpolarized standard arrays
  for i=0,Ndata-1 do begin
    self->Message,'Adding file '+(*self.indata)[i],priority='DEBUG',method='RUN'
    self->AddStd,(*self.indata)[i]
  endfor
  
  ; Update the reference pol and unpol stars
  self->RefStd
  
  ; Do the correction of the data and write the results
  for i=0,Ndata-1 do begin
    refname = (*self.indata)[i]
    im = readfits(refname,hin)
    
    ; Do the instrumental correction from the values in the header
    hout = self->unpolcorrection(hin)
    
    ; Do the angle correction from the values in the header
    hout = self->polcorrection(hout)
    
    ; Create file name for the current starpol
    outfname = self->createFname(refname, _EXTRA=extraKeywords)
    if self.outdata eq ptr_new() then self.outdata = ptr_new([outfname]) $
    else *self.outdata = [*self.outdata,outfname]
    
    ; Write fits file with images/plane and header
    self->Message,'Writing file '+outfname,priority='DEBUG',method='RUN'
    writefits,outfname,im,hout
  endfor
  
  
END

;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runinstrumentalpol::cleanup
  
  self->runstep::cleanup
  
  if self.polstd ne ptr_new() then ptr_free,self.polstd
  if self.polstdlist ne ptr_new() then ptr_free,self.polstdlist
  if self.nopolstd ne ptr_new() then ptr_free,self.nopolstd
  if self.nopolstdlist ne ptr_new() then ptr_free,self.nopolstdlist
    
END
;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runinstrumentalpol::init, nopolstd=nopolstd, polstd=polstd, _Extra=extraKeyword
  
  if self->runstep::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (runstep)',priority='ERR',method='INIT'
    return,0
  endif
  
  self.polstdlist = ptr_new([{name:'hd2746', p:0.001, xi:0.}])
  if keyword_set(polstd) then *self.polstdlist = [*self.polstdlist,polstd]
  
  
  self.nopolstdlist = ptr_new([ {name:'hd432', p:0.0, xi:0.0}, $
                                {name:'hd9540', p:0.0, xi:0.0}, $
                                {name:'hd10476', p:0.0, xi:0.0}, $
                                {name:'hd18803', p:0.0, xi:0.0}, $
                                {name:'hd20630', p:0.0, xi:0.0}, $
                                {name:'hd38393', p:0.0, xi:0.0}, $
                                {name:'hd39587', p:0.0, xi:0.0}, $
                                {name:'hd42807', p:0.0, xi:0.0}, $
                                {name:'hd61421', p:0.0, xi:0.0}, $
                                {name:'hd65583', p:0.0, xi:0.0}, $
                                {name:'hd90508', p:0.0, xi:0.0}, $
                                {name:'hd98281', p:0.0, xi:0.0}, $
                                {name:'hd100623', p:0.0, xi:0.0}, $
                                {name:'hd102438', p:0.0, xi:0.0}, $
                                {name:'hd102870', p:0.0, xi:0.0}, $
                                {name:'hd114710', p:0.0, xi:0.0}, $
                                {name:'hd115617', p:0.0, xi:0.0}, $
                                {name:'hd125184', p:0.0, xi:0.0}, $
                                {name:'hd142373', p:0.0, xi:0.0}, $
                                {name:'hd144287', p:0.0, xi:0.0}, $
                                {name:'hd144579', p:0.0, xi:0.0}, $
                                {name:'hd154345', p:0.0, xi:0.0}, $
                                {name:'hd165908', p:0.0, xi:0.0}, $
                                {name:'hd185395', p:0.0, xi:0.0}, $
                                {name:'hd188512', p:0.0, xi:0.0}, $
                                {name:'hd202573', p:0.0, xi:0.0}, $
                                {name:'hd202940', p:0.0, xi:0.0}, $
                                {name:'hd210027', p:0.0, xi:0.0}, $
                                {name:'hd216956', p:0.0, xi:0.0}, $
                                {name:'zetapeg', p:0.0, xi:0.0}])
  if keyword_set(nopolstd) then *self.nopolstdlist = [*self.nopolstdlist,nopolstd]
  
  ; We initialize the arrays with the reference one
  self.polstd = ptr_new([self->DefaultInstPol()])
  self.nopolstd = ptr_new([self->DefaultInstPol()])
  
  return,1
  
END


;****************************************************************************
;     RUNPOLARIMETRY__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runinstrumentalpol__define
  
  struct={runinstrumentalpol, $
          polstd: ptr_new(), $          ; Polarization standards - 
          polstdlist: ptr_new(), $     ; All the pol standards is taken from the input data based on this array (no, ancillary files are used)
          nopolstd: ptr_new(), $        ; Non-polarized standards
          nopolstdlist: ptr_new(), $     ; All the non-pol standards is taken from the input data based on this array (no, ancillary files are used)
          inherits runstep}
END