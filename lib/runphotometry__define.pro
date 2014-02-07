; NAME:
;     RUNPHOTOMETRY - Version 1.0
;
; PURPOSE: Calculate the photometry of stars in an image
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
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 19th 2013

;****************************************************************************
;     GROUPARRAY - Creates an array of strings containing information about the object. 
;****************************************************************************
FUNCTION runphotometry::GroupArray,labels=labels, filebase=filebase, details=details, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='GROUPARRAY'
  
  finalarr = self->runstep::GroupArray(labels=keyword_set(labels),filebase=keyword_set(filebase),_EXTRA=extraKeywords)
  if self.starprop eq ptr_new() then begin
    self->Message,'No star polarization calculations, calling default Group Array from parent class',priority='INFO',method='GROUPARRAY'
    return,finalarr
  endif 
  if n_elements(*self.starprop) eq 0 then begin
    self->Message,'No star polarization calculations, calling default Group Array from parent class',priority='INFO',method='GROUPARRAY'
    return,finalarr
  endif
  
  if not keyword_set(details) then return,finalarr
  
  label_ref = ['starx','stary','Flux','eFlux','Sky','eSky']
  
  i = 0
    starpol = (*self.starprop)[i]
    label_arr = 'Star'+strtrim(i+1,2)+':'+label_ref
    resarr = [strtrim(starpol.starx,2),strtrim(starpol.stary,2),strtrim(starpol.starphot,2),strtrim(starpol.starephot,2), $
                 '['+strjoin(strtrim(starpol.sky,2),',')+']','['+strjoin(strtrim(starpol.esky,2),',')+']']
  for i=1,n_elements(*self.starprop)-1 do begin
    starpol = (*self.starprop)[i]
    label_arr = [label_arr,'Star'+strtrim(i+1,2)+':'+label_ref]
    auxarr = [strtrim(starpol.starx,2),strtrim(starpol.stary,2),strtrim(starpol.starphot,2),strtrim(starpol.starephot,2), $
              '['+strjoin(strtrim(starpol.sky,2),',')+']','['+strjoin(strtrim(starpol.esky,2),',')+']']
    resarr = [resarr,auxarr]
  endfor
    
  ; Now we add the normal information to the begining of the array
  if keyword_set(labels) then begin
    resarr = [[label_arr],[resarr]]
    finalarr = transpose([[(finalarr)],[transpose(resarr)]])
  endif else begin
    finalarr = [finalarr,resarr]
  endelse
    
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='GROUPARRAY'
  
  return, finalarr
  
END

;******************************************************************************
;     DUPLICATE -  Create a new object with some of the information of self
;                  I am sure we could make a more general duplicate function in classdef
;                  There is already a classdef:copy but we need to check it out first
;******************************************************************************
FUNCTION runphotometry::Duplicate
  
  newobj = self->runstep::Duplicate()
  
  if newobj eq obj_new() then begin
    self->Message,'Parent Duplicate function returned null object',priority='DEBUG',method='DUPLICATE'
    return,obj_new()
  endif
  
  newobj->SetProperty,dofwhm=self.dofwhm
  newobj->SetProperty,fwhm=self.fwhm
  newobj->SetProperty,aperts=self.aperts
  newobj->SetProperty,apertidx=self.apertidx
  newobj->SetProperty,back=self.back
  newobj->SetProperty,npeaks=self.npeaks
  newobj->SetProperty,bckpertime=self.bckpertime
  
  if self.starprop ne ptr_new() then begin
    if n_elements(*self.starprop) gt 0 then newobj->SetProperty,starprop=ptr_new(*self.starprop)
  endif
  
  return,newobj
  
END

;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION runphotometry::createFname, fnames, peaks=peaks, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
    
  self->Message,'Switch to parent method (CREATEFNAME)',priority='DEBUG',method='CREATEFNAME'
  fname = self->runstep::CreateFname(fnames, _EXTRA=extraKeywords)
  
  if not keyword_set(peaks) then begin
    return,fname
  endif
  
  dirname = file_dirname(fname,/mark_directory)
  rootname = file_basename(fname)
  pos = strpos(rootname,'.',/reverse_search)
  rootname = strmid(rootname,0,pos)
  
  prodpath = self->createPath(dirname, _EXTRA=extraKeywords)

  return, prodpath+rootname+'_peaks.info'
  
  
END

;****************************************************************************
;     DEFAULTSTAR - Return a default structure storing the data of the 
;                        peaks for a specific star
;****************************************************************************
FUNCTION runphotometry::DefaultStar, _EXTRA=extraProperties
  
  defaultprop = {sign:1, $
                 starphot:0., $
                 starephot:0., $
                 peak: -1., $
                 baseline: -1., $
                 fwhmx:-1., $
                 fwhmy:-1., $
                 efwhmx:-1., $
                 efwhmy:-1., $
                 fwhmang:-1., $
                 efwhmang:-1., $
		 starx:0., $
		 stary:0., $
		 sky:0., $
		 esky:0., $
                 chi2nu:0., $
                 powerlaw:0,  $     ; If Moffat profile is added
		 valid:0}

  return, fillstructure(defaultprop,_EXTRA=extraProperties)

END

;****************************************************************************
;     CHECKSTARIDX - Check if the index correspond to a star in starprop 
;                      and returns 1 if yes 0 if not
;****************************************************************************
FUNCTION runphotometry::CheckStarIdx, staridx
  
  ; Check if the staridx and starprop array are valid
  if self.starprop eq ptr_new() then begin
    self->Message,'Star propertie pointer is null',priority='DEBUG',method='CheckStarIdx'
    return,0
  endif
  if staridx lt 0 then begin    ; We do not take the 0 index because it is the reference 
                                ; which does not correspond to a real star but to the average values
    self->Message,'Wrong input index ('+strtrim(staridx,2)+'<0)',priority='DEBUG',method='CheckStarIdx'
    return,0
  endif
  ; We check that the requested index is within the starprop array
  if staridx gt n_elements(*self.starprop)-1 then begin  ; Reminder: assumption about indexes of stars in stdphot
    self->Message,'Wrong input index ('+strtrim(staridx,2)+'>'+strtrim(n_elements(*self.starprop)-1,2)+')',priority='DEBUG',method='CheckStarIdx'
    return,0
  endif
  
  return,1
  
END

;****************************************************************************
;     CHANGEAPERTURE - Change the value of the aperture array and index
;                      to match a value of the requested input aperture
;****************************************************************************
PRO runphotometry::ChangeAperture, aperture, background=background
  
  ; Here I define a value to be added to the last element
  ; if we need to add any at the end of the array
  edgeval = 5
  
  aperture = fix(aperture)
  k = where(aperture eq self.aperts)
  Naperts = n_elements(self.aperts)
  if k[0] ne -1 then begin
    self.apertidx = k[0]
    if self.apertidx eq n_elements(self.aperts)-1 then begin
      self.aperts = [(self.aperts)[1:Naperts-1],(self.aperts)[Naperts-1]+edgeval]
      self.apertidx = self.apertidx -1 
    endif
  endif else begin
    kpos = where(self.aperts-aperture gt 0)
    if kpos[0] eq -1 then begin
      self.aperts = [(self.aperts)[2:Naperts-1],aperture, aperture+edgeval]
      self.apertidx = Naperts-2
    endif else begin 
      knear = min(kpos)
      newaperts = [aperture,(self.aperts)[knear:Naperts-1]]
      if knear gt 0 then newaperts = [(self.aperts)[0:knear-1],newaperts]
      self.aperts = newaperts
      self.apertidx = knear
    endelse   
  endelse
    
  self.back = [(self.aperts)[self.apertidx]+3,(self.aperts)[self.apertidx]+13]
  if keyword_set(background) then begin
    if n_elements(background) eq 2 then begin
      if background[0] lt background[1] then self.back=background else $
        if background[1] lt background[0] then self.back = [background[1], background[0]] else $
        print, 'Wrong input background: ['+strtrim(background[0],2)+','+strtrim(background[1],2)+'], using default back=['+strtrim(self.back[0],2)+','+strtrim(self.back[1],2)+']'
    endif 
  endif
  
END

;******************************************************************************
;     IMPEAKFREAD - Reads the values from a file in disk
;******************************************************************************
FUNCTION runphotometry::ImPeakFRead, finfo
  
  ; We may even read the peaks from the fits products with the extension table
  ; To be done...
  
  defres = {xarr:[0.],yarr:[0.],nfound:0,nloop:0,flux:[0.],sharp:[0.],round:[0.]}
  
  if file_test(finfo) eq 0 then begin
    self->Message,'Peak file not found: '+finfo,priority='ERR',method='IMPEAKFREAD'
    return,defres
  endif
  
  readline = ''
  OPENR,inunit,finfo,/GET_LUN
  ; Read header
  readf,inunit,readline
  self->Message,readline,priority='DEBUG',method='IMPEAKFREAD'
  readf,inunit,readline
  self->Message,readline,priority='DEBUG',method='IMPEAKFREAD'
  readf,inunit,readline
  self->Message,readline,priority='DEBUG',method='IMPEAKFREAD'
  
  xarr = [-1]
  self->Message,'>>>>>>>>>>>>>>>>>>>>',priority='DEBUG',method='IMPEAKFREAD'
  self->Message,finfo,priority='DEBUG',method='IMPEAKFREAD'
  while not eof(inunit) do begin
    readf,inunit,readline
    self->Message,readline,priority='DEBUG',method='IMPEAKFREAD'
    readarr =strsplit(readline,STRING(9B),/extract)
    self->Message,readarr,priority='DEBUG',method='IMPEAKFREAD'
    fname = readarr[0]
    flag = readarr[1]
    x = float(readarr[2])
    y = float(readarr[3])
    f = float(readarr[4])
    sh = float(readarr[5])
    r = float(readarr[6])
    if xarr[0] eq -1 then begin
      xarr = x
      yarr = y
      farr = f
      sharp = sh
      rnd = r
    endif else begin
      xarr = [xarr,x]
      yarr = [yarr,y]
      farr = [farr,f]
      sharp = [sharp,sh]
      rnd = [rnd,r]
    endelse
    self->Message,'== ['+strjoin(xarr,',')+']',priority='DEBUG',method='IMPEAKFREAD'
  endwhile
  self->Message,'>>>>>>>>>>>>>>>>>>>>',priority='DEBUG',method='IMPEAKFREAD'
  
  CLOSE,inunit
  FREE_LUN,inunit
  
  return,{xarr:xarr,yarr:yarr, nfound:n_elements(xarr), nloop:0, flux:farr, sharp:sharp, round:rnd}

END

;******************************************************************************
;     IMPEAKFIND - Calculate background and noise and loop until finding peaks in image
;******************************************************************************
FUNCTION runphotometry::ImPeakFind, img_conv_abs, npeaks, thresh, tstep, sharplim, roundlim, allpeaks=allpeaks

  ; Now we calculate the first estimate of the level of counts that we will be using as a 
  ; cut level based on the noise of the image and the input threshold (thresh)
  ; For instance, sigma is the standard deviation of the image and bkgd is the background level
  bkgd         = median(img_conv_abs)
  res          = moment(img_conv_abs)
  sigma        = sqrt(res[1])
  cutlev       = thresh*sigma
  
  if keyword_set(allpeaks) then begin
    self->Message,'ALLPEAKS keyword is set',priority='DEBUG',method='IMPEAKFIND'
    find, img_conv_abs, xarr, yarr, f, sharp, rnd, cutlev, self.fwhm, roundlim, sharplim, /SILENT 
    return,{xarr:xarr,yarr:yarr, nfound:n_elements(xarr), nloop:1, flux:f, sharp:sharp, round:rnd}
  endif
  
  ; Now we start with the search. Since we will be changing the threshold while we don't 
  ; find the expected number of peaks we need to make the steps of the threshold variations
  ; lower as we approach the peak. If not we may get to the situation that we increase and decrease
  ; the threshold but we find always less or more peaks than expected.
  nfound = 0
  nloop  = 0
  maxloop = 1000
  OPENW,outunit,'/Users/mcharcos/Desktop/runphotometrytest.txt',/GET_LUN
  if self.interactive then maxloop = 10
  ; First we get as close as possible to the number of peaks we want (npeaks)
  self->Message,'Looking for peaks for static step '+strtrim(tstep,2),priority='DEBUG',method='IMPEAKFIND'
  while (nfound lt npeaks and nloop lt maxloop) do begin
    self->Message,'    threshold = '+strtrim(thresh,2),priority='DEBUG',method='IMPEAKFIND'
    time0=systime(1)
    find, img_conv_abs, xarr, yarr, f, sharp, rnd, cutlev, self.fwhm, roundlim, sharplim, /SILENT 
    dtime=systime(1)-time0
    printf,outunit,'Time='+strtrim(dtime,2)+', step='+strtrim(tstep,2)+', threshold='+strtrim(thresh,2)
    nfound = n_elements(xarr)
    thresh = thresh - tstep
    if thresh lt 0. then begin
      self->Message,'Negative threshold without peak success',priority='WARN',method='IMPEAKFIND'
      return,{xarr:replicate(-1,npeaks),yarr:replicate(-1,npeaks), nfound:-1, nloop:nloop, flux:-1, sharp:-1, round:-1}
    endif
    cutlev = thresh*sigma
    nloop  = nloop + 1
  end
  printf,outunit,'-------'
  
  if nloop eq maxloop then begin
    self->Message,'Maximum number of iterations has been reached without peak success',priority='WARN',method='IMPEAKFIND'
    return,{xarr:replicate(-1,npeaks),yarr:replicate(-1,npeaks), nfound:-1, nloop:nloop, flux:-1, sharp:-1, round:-1}
  endif

  ; Now we find tune the search until we either we iterate 1000 times or we find npeaks
  self->Message,'Looking for peaks for dynamic step',priority='DEBUG',method='IMPEAKFIND'
  threshhigh = 1
  currentstep = tstep
  while (nfound ne npeaks and nloop lt maxloop) do begin
    if self.debug eq 1 then begin
      atv,img_conv_abs
      atvplot,xarr,yarr,psym=4
    endif
    self->Message,'    threshold = '+strtrim(thresh,2),priority='DEBUG',method='IMPEAKFIND'
    self->Message,'    step = '+strtrim(tstep,2),priority='DEBUG',method='IMPEAKFIND'
    tstep = tstep/2.0
    time0=systime(1)
    find, img_conv_abs, xarr, yarr, f, sharp, rnd, cutlev, self.fwhm, roundlim, sharplim, /SILENT
    dtime=systime(1)-time0
    printf,outunit,'Time='+strtrim(dtime,2)+', step='+strtrim(currentstep,2)+', threshold='+strtrim(thresh,2)
    
    nfound = n_elements(xarr)
    if nfound lt npeaks then begin
      if threshhigh eq 0 then currentstep = currentstep/2.
      threshhigh = 1
      thresh = thresh - currentstep 
    endif else begin 
      if threshhigh eq 1 then currentstep = currentstep/2.
      threshhigh = 0
      thresh = thresh + currentstep
    endelse
    cutlev = thresh*sigma
    nloop  = nloop + 1
  end
  CLOSE,outunit
  FREE_LUN,outunit
  
  if nloop eq maxloop then begin
    self->Message,'Maximum number of iterations has been reached without peak success',priority='WARN',method='IMPEAKFIND'
    return,{xarr:replicate(-1,npeaks),yarr:replicate(-1,npeaks), nfound:-1, nloop:nloop, flux:-1, sharp:-1, round:-1}
  endif
  
  return,{xarr:xarr,yarr:yarr, nfound:nfound, nloop:nloop, flux:f, sharp:sharp, round:rnd}

END

;******************************************************************************
;     OPTPEAKFIND - Optimal peak finder
;******************************************************************************
FUNCTION runphotometry::OptPeakFind, img, npeaks, sharplim, roundlim
  
  img_conv_abs = img
  xmin=0
  xmax=n_elements(img_conv_abs[*,0])-1
  ymin=0
  ymax=n_elements(img_conv_abs[0,*])-1
  xcen=xmin
  ycen=ymin
  if self.trackpos gt 10 then begin
    if self.starprev ne ptr_new() then begin
      if n_elements(*(self.starprev)) gt 0 then begin
        ; At this stage we know that the user requested to track position and there is a previous position
        xcen = ((*self.starprev)[0]).starx
        ycen = ((*self.starprev)[0]).stary
        xmin=max([0,xcen-self.trackpos])
        xmax=min([xmax,xcen+self.trackpos])
        ymin=max([0,ycen-self.trackpos])
        ymax=min([ymax,ycen+self.trackpos])
        img_conv_abs = img[xmin:xmax,ymin:ymax]
      endif
    endif
  endif
  
  ; Now we calculate the first estimate of the level of counts that we will be using as a 
  ; cut level based on the noise of the image and the input threshold (thresh)
  ; For instance, sigma is the standard deviation of the image and bkgd is the background level
  bkgd         = median(img_conv_abs)
  res          = moment(img_conv_abs)
  sigma        = sqrt(res[1])
    
  ; Define initial values
  maxthresh = bkgd + 10.*sigma   ;(max(img_conv_abs,min=minthresh))
  minthresh = max([0.,bkgd - 10.*sigma])  ;minthresh])
  nfound = 0
  nloop  = 0
  maxloop = 1000
  changethresh=0
  maxchangethresh=5
  if self.interactive then maxloop = 10
  ; First we get as close as possible to the number of peaks we want (npeaks)
  while (nfound ne npeaks and nloop lt maxloop) do begin
    thresh=(minthresh+maxthresh)/2.0
    find, img_conv_abs, xarr, yarr, f, sharp, rnd, thresh, self.fwhm, roundlim, sharplim   ;, /SILENT 
    nfound = n_elements(xarr)
    if nfound eq 1 then if xarr[0] eq 0 and yarr[0] eq 0 then nfound = 0
    self->Message,'    fwhm = '+strtrim(self.fwhm,2)+', threshold = '+strtrim(thresh,2)+', minthresh = '+strtrim(minthresh,2)+', maxthresh = '+strtrim(maxthresh,2)+', nfound = '+strtrim(nfound,2)+', loop = '+strtrim(nloop+1,2)+'/'+strtrim(maxloop,2),priority='DEBUG',method='OPTPEAKFIND'
    if self.debug eq 1 and n_elements(xarr) gt 0 then begin
      atv_plus,img_conv_abs
      atvplot,xarr,yarr,psym=4
    endif
    if (abs(maxthresh - minthresh) lt 0.001 and nfound ne npeaks) then begin
      print,'Min/max interval is null, setting loop to end for iteration #'+strtrim(nloop,2)
      ;nloop = maxloop
      if nfound gt npeaks then maxthresh = 2.0*minthresh else minthresh = maxthresh/2.0
      changethresh = changethresh + 1
    endif else begin
      if nfound lt npeaks then maxthresh=thresh else minthresh=thresh
      nloop  = nloop + 1
    endelse
    if (changethresh ge maxchangethresh) then nloop = maxloop
  end
  
  if nloop ge maxloop then begin
    self->Message,'Maximum number of iterations has been reached without peak success',priority='WARN',method='OPTPEAKFIND'
    if self.trackpos gt 10 and self.starprev ne ptr_new() then begin
      if n_elements(*(self.starprev)) gt 0 and (xcen eq 0  or ycen eq 0) then begin
        xcen = ((*self.starprev)[0]).starx
        ycen = ((*self.starprev)[0]).stary
      endif
    endif
    if nfound lt npeaks then begin
      return,{xarr:replicate(xcen,npeaks),yarr:replicate(ycen,npeaks), nfound:-1, nloop:nloop, flux:-1, sharp:-1, round:-1}
    endif
    
    xarr = xarr[0:npeaks-1]
    yarr = yarr[0:npeaks-1]
    nfound = npeaks
  endif
    
    
  return,{xarr:xarr+xmin,yarr:yarr+ymin, nfound:nfound, nloop:nloop, flux:f, sharp:sharp, round:rnd}

END

;****************************************************************************
;     STARPEAKFIND - Calculate and fill starprop with the position of the stars. 
;****************************************************************************
PRO runphotometry::StarPeakFind, imfit, header=header, fitsname=fitsname, thresh=thresh, outfile=outfile, atvshow=atvshow, $
                                 useprevious=useprevious, noabsimg=noabsimg, fname=fname, _EXTRA=extrakeywords
  
  self->Message,'Starting',priority='DEBUG',method='StarPeakFind'
  
  aperts = self.aperts 
  apertidx = self.apertidx 
  back = self.back 
  npeaks = self.npeaks
  
  if keyword_set(fname) eq 0 then fname='default'
  
  ; We may want to include these values as attributes of the object
  if not(keyword_set(THRESH)) then thresh = 5.0
  if not(keyword_set(TSTEP))  then tstep  = 0.25
  if not(keyword_set(SHARPLIM)) then sharplim = [0.2,1.0]
  if not(keyword_set(ROUNDLIM)) then roundlim = [-0.75,0.75]
   
  ; Then it calculates the psf, filter the image with the psf and take the absolute value
  psf = psf_gaussian(npix=51,fwhm=self.fwhm,/normalize)
  if psf[0] eq -1 then begin
    self->Message,'Problem calculating psf_gaussian. Result is invalid',priority='WARN',method='STARPEAKFIND'
    return
  endif
  ;img_conv     = convolve(imfit, psf) 
  img_conv = imfit
  if not keyword_set(noabsimg) then img_conv_abs = abs(img_conv) else img_conv_abs = img_conv
  
  nfound = 0
  if keyword_set(outfile) and self.readoutf ne 0 then begin
    if file_test(outfile) then begin
      pospeakarr = self->ImPeakFRead(outfile) 
      nfound = pospeakarr.nfound
    endif 
  endif 
  
  if nfound eq 0 then begin
    ;pospeakarr = self->ImPeakFind(img_conv_abs,npeaks,thresh,tstep,sharplim,roundlim, _EXTRA=extrakeywords)
    pospeakarr = self->OptPeakFind(img_conv_abs,npeaks,sharplim,roundlim)
  endif
  
  xarr = pospeakarr.xarr
  yarr = pospeakarr.yarr
  nfound = pospeakarr.nfound
  nloop = pospeakarr.nloop 
  f = pospeakarr.flux
  sharp = pospeakarr.sharp
  rnd = pospeakarr.round  
  np = 10000.
  points = findgen(np)/np
  xcircle = cos(points*2.*!pi)
  ycircle = sin(points*2.*!pi)
  
  ; Show the image and the peaks in atv if requested
  if keyword_set(atvshow) or keyword_set(self.interactive) then begin  ; or self.debug eq 1
    atv_plus, imfit
    atvplot, xarr, yarr, psym=4
    atvxyouts,xarr+4,yarr+4,strtrim(indgen(n_elements(xarr))+1,2),charsize=2
    common atv_state, state
    ; Set default values of atv
    ; RA-DEC display in degrees
    state.display_base60=0
    state.aprad=self.aperts[self.apertidx]
    state.innersky=self.back[0]
    state.outersky=self.back[1]
    state.imagename = fitsname
    atv_apphot_toside, [xarr[0],yarr[0]]  
        
    if keyword_set(self.interactive) then begin
      ;correctstars = dialog_message('Are these star positions correct?',/question,/default_no)
      correctstars='No'
      if correctstars eq 'No' then begin
        nfound = 0
        print,'---------------------------------------------'
        print,'Press P over the star to define the position'
        print,'Press q when done'
        print,'---------------------------------------------'
        if keyword_set(header) then atv_plus, imfit, header=header $
        else atv_plus, imfit
        atvplot, xarr, yarr, psym=4
        atvxyouts,xarr+4,yarr+4,strtrim(indgen(n_elements(xarr))+1,2),charsize=2
        
	istar = 0
	while istar lt npeaks and correctstars ne 'q' do begin
          atv_apphot_toside, [xarr[istar],yarr[istar]]  
	  print,'Go over star '+strtrim(istar+1,2)+', press p to select star and press q when done'
          atvplot, [xarr[istar]], [yarr[istar]], psym=4,color=3
	  atv_activate
          atvplot, [xarr[istar]], [yarr[istar]], psym=4, color=4
          ;junk = dialog_message(['Before',strtrim(xarr[istar],2),strtrim(yarr[istar],2)])
	  xarr[istar] = state.centerpos[0]
	  yarr[istar] = state.centerpos[1]
          
          ;junk = dialog_message(['After',strtrim(xarr[istar],2),strtrim(yarr[istar],2)])
          atvplot, [xarr[istar]], [yarr[istar]], psym=4,color=3
	  atvxyouts,[xarr[istar]]+4,[yarr[istar]]+4,strtrim(istar+1,2),charsize=2, color=3
	  istar = istar + 1
	endwhile
        if state.aprad ne self.aperts[self.apertidx] or $
          state.innersky ne self.back[0] or $
          state.outersky ne self.back[1] then $
            self->ChangeAperture,state.aprad,background=[state.innersky,state.outersky]
	nfound = istar
	f = replicate('unknown',nfound)
	sharp = replicate('unknown',nfound)
	rnd = replicate('unknown',nfound)
      endif
    endif
  endif
  
  if nfound ne npeaks then begin
    self->Message,'The number of detected peaks ('+strtrim(nfound,2)+') is different from the number of expected peaks ('+strtrim(npeaks,2)+')',priority='WARN',method='StarPeakFind'
    if not keyword_set(self.interactive) and nfound lt npeaks then begin
      if self.starprop ne ptr_new() then ptr_free,self.starprop
      self->Message,'Star photometry is aborted for '+fname,priority='WARN',method='StarPeakFind'
      return
    endif else begin
      ; Read user star selection from standard input
      rightinput = 0
      usersel = ''
      
      ; I added these two lines because the read routine does not work in bash mode. So to skip this section
      selstar=indgen(nfound)
      rightinput=1
      while rightinput eq 0 do begin
	read,'Input the number of '+strtrim(npeaks,2)+' stars to keep between 1 and '+strtrim(nfound,2)+':',usersel
	selstar = fix(strsplit(usersel,' ,:',/extract))
	k = where(selstar ge 1 and selstar le nfound)
	if k[0] eq -1 then nsel = 0 else nsel = n_elements(k)
	if nsel ne npeaks then begin
	  self->Message,'Number of stars is '+strtrim(nsel,2)+' but it should be '+strtrim(npeaks,2),priority='WARN',method='StarPeakFind'
	  self->Message,'Try using spaces, commas or colons to separate the numbers?',priority='WARN',method='StarPeakFind'
	  self->Message,'The numbers should be between 1 and '+strtrim(nfound,2),priority='WARN',method='StarPeakFind'
	endif else begin
	  rightinput = 1
	endelse
      endwhile
      xarr = (pospeakarr.xarr)[selstar[k]-1]
      yarr = (pospeakarr.yarr)[selstar[k]-1]
      nfound = nsel
      nloop = pospeakarr.nloop 
      f = (pospeakarr.flux)[selstar[k]-1]
      sharp = (pospeakarr.sharp)[selstar[k]-1]
      rnd = (pospeakarr.round)[selstar[k]-1]
      atv, imfit
      atvplot, xarr, yarr, psym=4
      atvxyouts,xarr+4,yarr+4,strtrim(indgen(n_elements(xarr))+1,2),charsize=2
    endelse
  endif
  
  ; Now we print a summary of the process in the screen
  ; and we update the starprop array 
  strprintarr = ['# Number of loops run   = '+strtrim(nloop,2), $
                 '# Number of peaks found = '+strtrim(nfound,2), $
		 strjoin(['Filename','X','Y','F','Sharp','Round'],STRING(9B))]
  
  signarr = replicate(0,nfound)
  for j=0,nfound-1 do begin
    self->Message, strjoin(['xarr','yarr','f','sharp','rnd'],STRING(9B)),priority='INFO',method='StarPeakFind'
    self->Message, strjoin([strtrim(xarr[j],2),strtrim(yarr[j],2),strtrim(f[j],2),strtrim(sharp[j],2),strtrim(rnd[j],2)],STRING(9B)),priority='INFO',method='StarPeakFind'
    if img_conv[round(xarr[j]),round(yarr[j])] lt 0.0 then begin
      flag=' - ' 
      signaux = -1
    endif else begin
      flag=' + '
      signaux = 1
    endelse
    strprintarr = [strprintarr,strjoin([fname,strtrim(flag,2), strtrim(xarr[j],2), strtrim(yarr[j],2), strtrim(f[j],2), strtrim(sharp[j],2), strtrim(rnd[j],2)],STRING(9B))]
    
    ; update starprop list
    newstar = self->DefaultStar(starx=xarr[j],stary=yarr[j],valid=1,sign=signaux)
    if self.starprop eq ptr_new() then self.starprop = ptr_new([newstar]) else *self.starprop = [*self.starprop,newstar]
  endfor
  
  ; Print the result on screen and in outputfile if requested
  ; we can remove this in the future since we will be able to use save to create a file with the info
  strprint = transpose(strprintarr)  
  
  if keyword_set(OUTFILE) then begin
    self->Message,'Writing file '+outfile,priority='INFO',method='STARPEAKFIND'
    if FILE_TEST(outfile) then begin
      file_delete,outfile
      self->Message,'Deleted old file with same name',priority='DEBUG',method='STARPEAKFIND'
    endif
    openw, outunit, outfile, /APPEND, /GET_LUN
    printf,outunit,strprint
    close, outunit
    free_lun, outunit
  endif
  
  
END

;****************************************************************************
;     STARPROFILE - Fit a Gaussian in the image array imarr to the peak 
;                   defined by the star of index staridx. 
;                   Here we assume that in the input array the star has positive flux
;****************************************************************************
PRO runphotometry::StarProfile, imarr, staridx, viewfit=viewfit
  
  ; Define constants used in the calculations
  fact    = 2.0*sqrt(2.0*alog(2.0))
  ncut    = 30                          ; cutout region used for Gaussian fitting
  deg2rad = !pi/180.0                   ; Convertion factor from degrees to radians
  
  ; Check if the star index is correct
  if self->CheckStarIdx(staridx) eq 0 then return
  
  ; Read the center of the star
  xcen = ((*self.starprop)[staridx]).starx
  ycen = ((*self.starprop)[staridx]).stary
  sky = ((*self.starprop)[staridx]).sky
  
  sz = size(imarr)
  if (floor(xcen-ncut) lt 0) or (ceil(xcen+ncut) gt sz[1]) or $
       (floor(ycen-ncut) lt 0) or (ceil(ycen+ncut) gt sz[2]) then return
  
  ; Extract a ~ncut by ~ncut section in the array and read the size of the subarray
  isub  = imarr[floor(xcen-ncut):ceil(xcen+ncut),floor(ycen-ncut):ceil(ycen+ncut)]
  szc   = size(isub)
  nx    = szc[1]
  ny    = szc[2]

  ; Initialize the guessed values and do the fitting of the profile according to the profile type
  CASE strtrim(strupcase(self.profile),2) of
    'GAUSSIAN': begin
                ; This is actually the fit of the star with a Gaussian profile using mpfit2dpeak
                npars = 7
                gpars = fltarr(npars)
                gests = gpars
                gests = [sky,max(isub),self.fwhm/fact,self.fwhm/fact,nx/2,ny/2,45.0*deg2rad]
                gfit  = mpfit2dpeak(isub,gpars,ESTIMATES=gests,/GAUSSIAN,/TILT)
                (*self.starprop)[staridx].powerlaw = 0
              end
    'LORENTZIAN': begin
                  ; This is actually the fit of the star with a Lorentzian profile using mpfit2dpeak
                  npars = 7
                  gpars = fltarr(npars)
                  gests = gpars
                  gests = [sky,max(isub),self.fwhm,self.fwhm,nx/2,ny/2,45.0*deg2rad]
                  gfit  = mpfit2dpeak(isub,gpars,ESTIMATES=gests,/LORENTZIAN,/TILT)
                  (*self.starprop)[staridx].powerlaw = 0
                end
    else: begin  ; Including MOFFAT
            ; This is actually the fit of the star with a Moffat profile using mpfit2dpeak
            ; For other values than GAUSSIAN, LORENTZIAN or MOFFAT, MOFFAT is used as default.
            npars = 8
            gpars = fltarr(npars)
            gests = gpars
            gests = [sky,max(isub),self.fwhm,self.fwhm,nx/2,ny/2,45.0*deg2rad,1]
            gfit  = mpfit2dpeak(isub,gpars,ESTIMATES=gests,/MOFFAT,/TILT)
            (*self.starprop)[staridx].powerlaw = gpars[7]
         end
  ENDCASE
  
  ; Update star properties
  (*self.starprop)[staridx].baseline  = gpars[0]
  (*self.starprop)[staridx].peak  = gpars[1]
  if strtrim(strupcase(self.profile),2) eq 'GAUSSIAN' then begin
    (*self.starprop)[staridx].fwhmx  = fact*gpars[2]
    (*self.starprop)[staridx].fwhmy  = fact*gpars[3]
  endif else begin
    (*self.starprop)[staridx].fwhmx  = gpars[2]
    (*self.starprop)[staridx].fwhmy  = gpars[3]
  endelse
  (*self.starprop)[staridx].starx  = floor(xcen-ncut) + gpars[4]
  (*self.starprop)[staridx].stary  = floor(ycen-ncut) + gpars[5]
  gtot  = total(gfit - gpars[0])  ; Mean fwhm
  
  ; Do we want to update the center? Not sure?
  ; I would say yes but the forphot.pro keeps the initial peaks
  ; ((*self.starprop)[staridx]).starx = gpars[4]+xcen-float(ncut)
  ; ((*self.starprop)[staridx]).stary = gpars[5]+ycen-float(ncut)
  gang = gpars[6]/deg2rad      ; if gang gt 90.0 then gang = 180.0 - gang else gang = 90.0 - gang
  
  ; Some statistical analysis that could be used later to do a smart check on the result
  chi2  = total((isub-gfit)^2)
  chi2nu = chi2/(float(nx*ny - npars))
  (*self.starprop)[staridx].chi2nu = chi2nu
  
  ; Some variable renaiming to make the code clearer
  gfwx = ((*self.starprop)[staridx]).fwhmx
  gfwy = ((*self.starprop)[staridx]).fwhmy
  
  xmax1 = (gfwx/2.0)*cos(gpars[6])
  ymax1 = (-gfwx/2.0)*sin(gpars[6])
  xmax2 = (gfwy/2.0)*sin(gpars[6])
  ymax2 = (gfwy/2.0)*cos(gpars[6])
  if (gfwx ge gfwy) then begin
      if (ymax1 eq -ymax1) then begin
          gang = 0.0
      endif else begin
          gang = atan(ymax1,xmax1)/deg2rad
      endelse
      if gang lt 0.0 then gang = 180.0 + gang
      if keyword_set(VIEWFIT) then begin
         re = ' '
         atv, isub-gfit
         atvplot, [xmax1+gpars[4],-xmax1+gpars[4]], [ymax1+gpars[5],-ymax1+gpars[5]], color=2, thick=2
         atvplot, [xmax2+gpars[4],-xmax2+gpars[4]], [ymax2+gpars[5],-ymax2+gpars[5]], color=1, thick=2
         read, re
      endif
  endif else begin
      if (ymax2 eq -ymax2) then begin
          gang = 0.0
      endif else begin
          gang = atan(ymax2,xmax2)/deg2rad
      endelse
      if gang lt 0.0 then gang = 180.0 + gang
      if keyword_set(VIEWFIT) then begin
         re = ' '
         atv, isub-gfit
         atvplot, [xmax1+gpars[4],-xmax1+gpars[4]], [ymax1+gpars[5],-ymax1+gpars[5]], color=1, thick=2
         atvplot, [xmax2+gpars[4],-xmax2+gpars[4]], [ymax2+gpars[5],-ymax2+gpars[5]], color=2, thick=2
         read, re
      endif
  endelse
  
  (*self.starprop)[staridx].fwhmang = gang
  
  ; Update the photometry based on the profile
  CASE strtrim(strupcase(self.profile),2) of
    'GAUSSIAN':
    else:
  ENDCASE
    

END

;****************************************************************************
;     STARPHOT - Calculate and update the photometry in the input array 
;                of the star of index staridx (starting at 0)
;                The input image is the original fits image array
;****************************************************************************
PRO runphotometry::StarPhot, imarr, staridx, texp, aperts, apertidx, back, skipfits=skipfits
  
  self->Message,'Performing photometry for star #'+strtrim(staridx,2), priority='DEBUG',method='StarPhot'
  ; Check if the star index is correct
  if self->CheckStarIdx(staridx) eq 0 then begin
    self->Message,'Error: '+strtrim(staridx,2)+' star index is not valid', priority='ERR',method='StarPhot'
    return
  endif
  
   ; We change the image so the peak has positive flux
  imap = imarr*float((*self.starprop)[staridx].sign)
  
  self->Message,['Calling function: aper', $
                 'aper, imap, '+strtrim((*self.starprop)[staridx].starx,2)+', '+strtrim((*self.starprop)[staridx].stary,2)+', '+ $
		 'flux, eflux, sky, skyerr, '+strtrim(texp,2)+', '+ $
		 '['+strjoin(strtrim(aperts,2),',')+']'+', '+'['+strjoin(strtrim(back,2),',')+']'+', /NAN, /EXACT, /FLUX, /SILENT'], $
		 priority='DEBUG',method='STARPHOT'
  aper, imap, (*self.starprop)[staridx].starx, (*self.starprop)[staridx].stary, $
        flux, eflux, sky, skyerr, texp, aperts, back, /NAN, /FLUX, /EXACT , /SILENT
  self->Message,'Done with function: aper',priority='DEBUG',method='STARPHOT'
        
  ; Update star with result from aper function
  (*self.starprop)[staridx].starphot = [flux[apertidx]]
  (*self.starprop)[staridx].starephot = [eflux[apertidx]]
  
  (*self.starprop)[staridx].sky = sky
  (*self.starprop)[staridx].esky = skyerr
  
  if keyword_set(skipfits) eq 0 then begin
    self->StarProfile,imap, staridx
  endif 
  
  
END

;****************************************************************************
;     GETCONFIGURATION - Returns an array of structures with the attributes
;                        that configurable
;****************************************************************************
FUNCTION runphotometry::GetConfiguration
  
  self->Message,'Starting',priority='DEBUG',method='GETCONFIGURATION'
  
  resarr = {name:'dofwhm',value:strtrim(self.dofwhm,2),type:'int',error:0}
  resarr = [resarr,{name:'fwhm',value:strtrim(self.fwhm,2),type:'float',error:0}]
  resarr = [resarr,{name:'aperts',value:'['+strjoin(strtrim(self.aperts,2),',')+']',type:'array1D:int',error:0}]
  resarr = [resarr,{name:'apertidx',value:strtrim(self.apertidx,2),type:'int',error:0}]
  resarr = [resarr,{name:'back',value:'['+strjoin(strtrim(self.back,2),',')+']',type:'array1D:float',error:0}]
  resarr = [resarr,{name:'npeaks',value:strtrim(self.npeaks,2),type:'int',error:0}]
  resarr = [resarr,{name:'readoutf',value:strtrim(self.readoutf,2),type:'int',error:0}]
  resarr = [resarr,{name:'interactive',value:strtrim(self.interactive,2),type:'int',error:0}]
    
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
PRO runphotometry::run, exptimeKey=exptimeKey, gainScale=gainScale, _EXTRA=extraKeywords
  
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
  for i=1,Ndata-1 do *self.outdata = [*self.outdata,self->createFname(data[i], _EXTRA=extraKeywords)]
    
  for i=0,Ndata-1 do begin
    self->Message,strtrim(i+1,2)+'/'+strtrim(Ndata,2)+' File: '+(*self.indata)[i],priority='DEBUG',method='RUN'
    imread = readfits((*self.indata)[i],hin,/silent)
    texp = sxpar(hin,'EXPTIME')
    bckcounts = texp * self.bckpertime
    
    gain = 1.  ;sxpar(hin,'GAIN')
    if keyword_set(exptimeKey) then begin
      exptime_read = sxpar(hin,exptimeKey,count=Ncount)
      if Ncount gt 0 then gain = gain*float(exptime_read)
    endif
    if keyword_set(gainScale) then gain = gain * gainScale
    
    imin = imread  ;+ bckcounts  ; It does not really make a difference on the sky or flux error. Aper is probably not
                                 ; not using any poissonic model
    s = size(imin)
    if s[0] gt 2 then begin
      imin = imin[*,*,0]
    endif
    
    ; Clean star property array
    if self.starprop ne ptr_new() then begin
      if self.starprev ne ptr_new() then ptr_free,self.starprev
      self.starprev = self.starprop
      self.starprop = ptr_new()
    endif
    
    ; Find peaks for each star
    self->Message,'Calling StarPeakFind...',priority='DEBUG',method='RUN'
    self->StarPeakFind,imin, header=hin, fitsname=(*self.indata)[i], outfile=self->createFname((*self.indata)[i], /peaks, _EXTRA=extraKeywords), fname=(*self.indata)[i], _EXTRA=extraKeywords
    self->Message,'StarPeakFind is done',priority='DEBUG',method='RUN'
   
    ; Calculate the aperture photometry for each star if any found
    if self.starprop ne ptr_new() then begin
      if self.dofwhm eq 1 then begin		     
       self->Message,'Calling Starphot with dofwhm...',priority='DEBUG',method='RUN'
        for j=0,n_elements(*self.starprop)-1 do self->StarPhot,imin,j, gain, self.aperts, self.apertidx, self.back
      endif else begin
       self->Message,'Calling Starphot without dofwhm...',priority='DEBUG',method='RUN'
        for j=0,n_elements(*self.starprop)-1 do self->StarPhot,imin,j, gain, self.aperts, self.apertidx, self.back, /skipfits
      endelse
      self->Message,'Starphot calls is done',priority='DEBUG',method='RUN'
      
      hout = hin
      sxaddpar, hout, 'OBJCLASS',OBJ_CLASS(self), 'Runstep class creating this file'
      sxaddpar, hout, 'STARAp',(self.aperts)[self.apertidx], 'Aperture used for aperture photometry.'
      sxaddpar, hout, 'STARBCK1',(self.back)[0], 'Inside radius for background of aperture photometry'
      sxaddpar, hout, 'STARBCK2',(self.back)[1], 'Outside radius for background of aperture photometry'
      sxaddpar, hout, 'STPHPADU',gain
      sxaddpar, hout, 'BCKPT',self.bckpertime
      sxaddpar, hout, 'BCKADU',bckcounts
      if self.dofwhm eq 1 then sxaddpar, hout, 'PROFILE',self.profile else sxaddpar, hout, 'PROFILE','NONE','Profile type'
        
      ; Add the photometry information
      for j=0,n_elements(*self.starprop)-1 do begin
        sxaddpar,hout, 'ST'+strtrim(j,2)+'X',(*self.starprop)[j].starx, 'Star #'+strtrim(j,2)+' X position in pixels'
        sxaddpar,hout, 'ST'+strtrim(j,2)+'Y',(*self.starprop)[j].stary, 'Star #'+strtrim(j,2)+' Y position in pixels'
        sxaddpar,hout, 'ST'+strtrim(j,2)+'F',(*self.starprop)[j].starphot, 'Star #'+strtrim(j,2)+' photometry in same unit as image counts.'
        sxaddpar,hout, 'ST'+strtrim(j,2)+'eF',(*self.starprop)[j].starephot, 'Star #'+strtrim(j,2)+' photometry error in same unit as image.'
        sxaddpar,hout, 'ST'+strtrim(j,2)+'SK',(*self.starprop)[j].sky, 'Sky value around star #'+strtrim(j,2)+' in same unit as image.'
        sxaddpar,hout, 'ST'+strtrim(j,2)+'eSK',(*self.starprop)[j].esky, 'Sky error value around star #'+strtrim(j,2)+' in same unit as image.'
        
        ; Write the fwhm information if requested
        if self.dofwhm eq 1 then begin
          sxaddpar,hout, 'ST'+strtrim(j,2)+'BL',(*self.starprop)[j].baseline, 'Baseline of star #'+strtrim(j,2)+' in same unit as image.'
          sxaddpar,hout, 'ST'+strtrim(j,2)+'PK',(*self.starprop)[j].peak, 'Peak of star #'+strtrim(j,2)+' in same unit as image counts.'
          sxaddpar,hout, 'ST'+strtrim(j,2)+'WX',(*self.starprop)[j].fwhmx, 'FWHM along X of star #'+strtrim(j,2)+' in pixels.'
          ;sxaddpar,hout, 'ST'+strtrim(j,2)+'eWX',(*self.starprop)[j].efwhmx, 'Error of FWHM along X of star #'+strtrim(j,2)+' in pixels.'
          sxaddpar,hout, 'ST'+strtrim(j,2)+'WY',(*self.starprop)[j].fwhmy, 'FWHM along Y of star #'+strtrim(j,2)+' in pixels.'
          ;sxaddpar,hout, 'ST'+strtrim(j,2)+'eWY',(*self.starprop)[j].efwhmy, 'Error of FWHM along Y of star #'+strtrim(j,2)+' in pixels.'
          sxaddpar,hout, 'ST'+strtrim(j,2)+'WA',(*self.starprop)[j].fwhmang, 'Rotation of elongation of star #'+strtrim(j,2)+' in degrees.'
          ;sxaddpar,hout, 'ST'+strtrim(j,2)+'eWA',(*self.starprop)[j].efwhmang, 'Error of rotation of elongation of star #'+strtrim(j,2)+' in degrees.'
          if strtrim(strupcase(self.profile),2) eq 'MOFFAT' then sxaddpar,hout, 'ST'+strtrim(j,2)+'PL',(*self.starprop)[j].powerlaw, 'Power law index for star #'+strtrim(j,2)+' of moffat fitting.'
          sxaddpar,hout, 'ST'+strtrim(j,2)+'CHI2',(*self.starprop)[j].chi2nu, 'Xi square of profile fitting of star #'+strtrim(j,2)+'.'
        endif 
      endfor
      sxaddpar,hout,'PROCSTAT','LEVEL_3'
      writefits,(*self.outdata)[i],imin,hout
    endif
  endfor
    
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO runphotometry::plot
    
  if self.indata eq ptr_new() then begin
    self->Message,'Indata array is null',priority='DEBUG',method='PLOT'
    return
  endif
  if n_elements(*self.indata) eq 0 then begin
    self->Message,'Indata array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  for i = 0,n_elements(*self.previous)-1 do help,(*self.previous)[i]
  self->Message,'Displaying '+(*self.indata)[0],priority='INFO',method='PLOT'
  atv,(*self.indata)[0]
  
  if self.starprop eq ptr_new() then return
  if n_elements(*self.starprop) eq 0 then return
  
  ; Define three circles for aperture, inside and outside background
  np = 10000.
  points = findgen(np)/np
  xcircle = cos(points*2.*!pi)
  ycircle = sin(points*2.*!pi)
  xcircle_aperture = xcircle*self.aperts[self.apertidx]
  ycircle_aperture = ycircle*self.aperts[self.apertidx]
  xcircle_bckin = xcircle*self.back[0]
  ycircle_bckin = ycircle*self.back[0]
  xcircle_bckout = xcircle*self.back[1]
  ycircle_bckout = ycircle*self.back[1]
  
  for i=0,n_elements(*self.starprop)-1 do begin
    starprop = (*self.starprop)[i]
    atvplot,[starprop.starx],[starprop.stary],psym=4
    atvplot,starprop.starx+xcircle_aperture,starprop.stary+ycircle_aperture,psym=3,color=3
    atvplot,starprop.starx+xcircle_bckin,starprop.stary+ycircle_bckin,psym=3,color=4
    atvplot,starprop.starx+xcircle_bckout,starprop.stary+ycircle_bckout,psym=3,color=4
    atvxyouts,[starprop.starx],[starprop.stary],strtrim([i]+1,2),charsize=2
  endfor
  
END

;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runphotometry::cleanup
  
  self->runstep::cleanup
  
  if self.starprop ne ptr_new() then ptr_free,self.starprop
  
END

;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runphotometry::init, fwhm=fwhm, dofwhm=dofwhm, aperts=aperts,apertidx=apertidx, BACK=BACK, profile=profile, $
                              npeaks=npeaks, bckpertime=bckpertime, interactive=interactive, trackpos=trackpos, _Extra=extraKeyword
  
  if self->runstep::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (runstep)',priority='ERR',method='INIT'
    return,0
  endif
  
  if keyword_set(dofwhm) then self.dofwhm = 1 else self.dofwhm = 0
  if keyword_set(fwhm) then self.fwhm = fwhm else self.fwhm = 20.
  if keyword_set(profile) then self.profile = strtrim(strupcase(profile),2) else self.profile = 'MOFFAT'
  
  if not(keyword_set(aperts)) then aperts = [5,6,7,8,9,10,12,15,20,25,30,35]
  if not(keyword_set(apertidx)) then apertidx = 5
  if not(keyword_set(BACK))   then back   = [15.,20.]
  if keyword_set(npeaks) eq 0 then npeaks=1
  if keyword_set(bckpertime) eq 0 then bckpertime=0.
  self.aperts = aperts
  self.apertidx = apertidx
  self.back = back
  self.npeaks = npeaks
  self.bckpertime = bckpertime
  
  self.readoutf = 1
  self.interactive = keyword_set(interactive)
  
  if keyword_set(trackpos) then self.trackpos = trackpos else self.trackpos = 0
  
  return,1
  
END

;****************************************************************************
;     RUNPHOTOMETRY__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runphotometry__define
  
  struct={runphotometry, $
	  trackpos: 0,   $           ; Indicates if the routine keeps track of the position of previous images. The value indicates the distance to search for the star
          starprev: ptr_new(), $     ; Contains the information of the previous stars so we can use them as first estimate of the positions if trackpos is set
          starprop: ptr_new(), $     ; Array with the information of the stars as returned from self::DefaultStar
	  bckpertime: 0., $          ; Background counts per second
          dofwhm: 1, $
          profile:'MOFFAT', $        ; The type of profile fitting (GAUSSIAN, LORENTZIAN or MOFFAT). If different, moffat will be default
          fwhm:   4.5, $
	  aperts: [5,6,7,8,9,10,12,15,20,25,30,35], $   ; Apertures used for photometry
	  apertidx: 5, $                            ; Index of the aperture for final photometry
	  back:[0.,0.], $                           ; Radii for the background
	  npeaks:1, $                               ; Number of peaks to detect and measure photometry
	  readoutf:0, $                             ; Indicates if the process is going to read previous peak calculations or recalculate the photometry each time
	  inherits runstep}
END
