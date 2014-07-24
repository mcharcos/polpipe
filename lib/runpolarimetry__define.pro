; NAME:
;     RUNPOLARIMETRY - Version 1.0
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
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 19th 2013



;******************************************************************************
;     DUPLICATE -  Create a new object with some of the information of self
;                  I am sure we could make a more general duplicate function in classdef
;                  There is already a classdef:copy but we need to check it out first
;******************************************************************************
FUNCTION runpolarimetry::Duplicate
  
  newobj = self->runstep::Duplicate()
  
  if newobj eq obj_new() then begin
    self->Message,'Parent Duplicate function returned null object',priority='DEBUG',method='DUPLICATE'
    return,obj_new()
  endif
  
  newobj->SetProperty,Deltapos=self.Deltapos
  
  if self.photlist ne ptr_new() then begin
    if n_elements(*self.photlist) gt 0 then newobj->SetProperty,photlist=ptr_new(*self.photlist)
  endif
  if self.pollist ne ptr_new() then begin
    if n_elements(*self.pollist) gt 0 then newobj->SetProperty,pollist=ptr_new(*self.pollist)
  endif
  
  return,newobj
  
END

;****************************************************************************
;     HWPCONVERT - If the value is 1..8 it returns the angle that corresponds
;                  to that position. Else, it returns the same value
;****************************************************************************
FUNCTION runpolarimetry::HWPConvert, hwp
  
  self->Message,'Starting',priority='DEBUG',method='HWPCONVERT'
  
  hwpf = float(hwp)
  if size(hwp,/type) eq 7 then begin
    self->Message,'HWP type is string... verify your headers. Expected value type is float.',priority='WARN',method='HWPCONVERT'
  endif
  
  refhwppos = [1.,2.,3.,4.,5.,6.,7.,8.]
  hwpres = [0.,22.5,45.,67.5,90.,112.5,135.,157.5]
  
  k = where(refhwppos eq hwpf)
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='HWPCONVERT'
  
  if k[0] eq -1 then return, hwpf else return, hwpres[k[0]]
  
END


;****************************************************************************
;     DEFAULTRAYSTAR - Return a default structure storing the data of the 
;                        photometry of a star
;****************************************************************************
FUNCTION runpolarimetry::DefaultRayStar, _EXTRA=extraProperties
  
  defaultray = {name:'', $
                fname:'',  $
                path:'',  $
                ray:'', $
                flux:-1.,   $
                eflux:-1.,  $
                x:-1.,   $
                y:-1.,   $
                hwp:-1.,  $
                filter:'', $
                dataqual:'', $
                date:'' $
                }

  resray = fillstructure(defaultray,_EXTRA=extraProperties)
  
  return, resray
  
END

;****************************************************************************
;     ADDPHOTSTAR - Complete the input structure with the information of the
;                   star depending on the star and structure input
;****************************************************************************
FUNCTION runpolarimetry::Addphotstar, photstar, star
  
  ; We may want to check that star structure is as expected.
  ; I am going to assume here that it is (for now)
  
  if star.dataqual eq 'BAD' then photstar.bad = 1
  
  CASE strlowcase(star.ray) of
    'ordinary': begin
                if (photstar.fe ne -1) then begin
                  if (photstar.name ne star.name or $
                      photstar.fname ne star.fname or $
                      photstar.date ne star.date or $
                      photstar.filter ne star.filter or $
                      photstar.hwp ne star.hwp) then begin
                    self->Message,'Problem with name, fname or date mismatch',priority='WARN',method='Addphotstar'
                    return,photstar
                  endif
                endif
                photstar.fo = star.flux
                photstar.efo = star.eflux
                photstar.xo = star.x
                photstar.yo = star.y
                photstar.filter = star.filter
                photstar.hwp = star.hwp
                photstar.name = star.name
                photstar.fname = star.fname
                photstar.opath = star.path
                photstar.date = star.date
                
              end
    'extraordinary': begin
                      if (photstar.fo ne -1) then begin
                        if (photstar.name ne star.name or $
                            photstar.fname ne star.fname or $
                            photstar.date ne star.date or $
                            photstar.filter ne star.filter or $
                            photstar.hwp ne star.hwp) then begin
                          self->Message,'Problem with name, fname, date or hwp mismatch',priority='WARN',method='Addphotstar'
                          return,photstar
                        endif
                      endif
                      photstar.fe = star.flux
                      photstar.efe = star.eflux
                      photstar.xe = star.x
                      photstar.ye = star.y
                      photstar.filter = star.filter
                      photstar.hwp = star.hwp
                      photstar.name = star.name
                      photstar.fname = star.fname
                      photstar.epath = star.path
                      photstar.date = star.date
                  end
    'all': begin
            if photstar.fo eq -1 then begin
              photstar.fo = star.flux
              photstar.efo = star.eflux
              photstar.xo = star.x
              photstar.yo = star.y
              photstar.filter = star.filter
              photstar.hwp = star.hwp
              photstar.name = star.name
              photstar.fname = star.fname
              photstar.opath = star.path
              photstar.epath = star.path
              photstar.date = star.date
            endif else begin
              if (photstar.name eq star.name and $
                  photstar.fname eq star.fname and $
                  photstar.date eq star.date or $
                  photstar.filter ne star.filter or $
                  photstar.hwp ne star.hwp) then begin
                photstar.fe = star.flux
                photstar.efe = star.eflux
                photstar.xe = star.x
                photstar.ye = star.y
              endif else begin
                self->Message,'Problem with name, fname, date or hwp mismatch',priority='WARN',method='Addphotstar'
                return,photstar
              endelse
            endelse
         end
    else: begin
          self->Message,'Wrong star ray type ('+star.ray+')',priority='WARN', method='Addphotstar'
        end
  endcase
  
  return,photstar
  
END

;****************************************************************************
;     DEFAULTPHOTSTAR - Return a default structure storing the data of the 
;                        photometry of a star
;****************************************************************************
FUNCTION runpolarimetry::Defaultphotstar, star=star, _EXTRA=extraProperties
  
  defaultphot = {name:'', $
                 fname:'',  $
                 opath:'',  $
                 epath:'',  $
                 filter:'', $
                 fo:-1.,   $
                 efo:-1.,  $
                 fe:-1.,   $
                 efe:-1.,  $
                 fi:-1.,   $
                 efi:-1,   $
                 xo:-1.,   $
                 yo:-1.,   $
                 xe:-1.,   $
                 ye:-1.,   $
                 hwp:-1.,  $
                 qu:-1.,   $
                 equ:-1.,  $
                 bad:0,    $
                 date:'' $
                 }

  resphot = fillstructure(defaultphot,_EXTRA=extraProperties)
  
  if keyword_set(star) then begin
    resphot = self->Addphotstar(resphot, star)
  endif
  
  return, resphot
  
END

;****************************************************************************
;     COMPLETEPHOTSTAR - Fill the qu values of the input star
;****************************************************************************
FUNCTION runpolarimetry::CompletePhotStar, instar, _EXTRA=extraProperties
  
  resphot = fillstructure(instar,_EXTRA=extraProperties)
  
  if (resphot.fo eq -1 or resphot.fe eq -1) then begin
    self->Message,'Input star is not complete',priority='DEBUG',method='CompletePhotStar'
    return,resphot
  endif
    
  ; Calculate polarization
  resphot.fi = resphot.fo + resphot.fe
  if resphot.fi eq 0. then begin
    resphot.qu = 0.
  endif else begin
    resphot.qu = (resphot.fo - resphot.fe)/resphot.fi
  endelse
  
  ; Calculate the error polarization
  resphot.equ = 0.
  resphot.efi = sqrt(resphot.efo^2 + resphot.efe^2)
  if resphot.efi eq 0. then begin
    resphot.equ = 0.
  endif else begin
    resphot.equ = resphot.efi/resphot.fi*sqrt(1.+resphot.qu^2)
  endelse
  self->Message,['Input fo='+strtrim(resphot.fo,2),'Input fe='+strtrim(resphot.fe,2), $
                 'Calculated fi='+strtrim(resphot.fi,2),'Calculated qu='+strtrim(resphot.qu,2)],priority='DEBUG',method='CompletePhotStar'
  
  
  return, resphot
  
END


;****************************************************************************
;     ADDPRODUCT - Given a fits file, the object is initialized with the values
;                  from the headers. The fits file must be the result
;                  from a previous reducstion with runpolarimetry class
;****************************************************************************
PRO runpolarimetry::AddProduct, fname, _EXTRA=extraKeywords
  
  ; Free the photometry, starpol and polarimetry list
  if self.photlist ne ptr_new() then ptr_free, self.photlist
  self.photlist = ptr_new()
  if self.pollist ne ptr_new() then ptr_free, self.pollist
  self.pollist = ptr_new()
  if self.starpol ne ptr_new() then ptr_free, self.starpol
  self.starpol = ptr_new()
  
  ; Now, clear the array with the input names and ancillary data
  ; presumably the ancillary data is not used in this class, but
  ; I am going to leave it here so we remember in the future if
  ; we decide to use it for something (like instrumental polarization standards)
  if self.indata ne ptr_new() then ptr_free, self.indata
  self.indata = ptr_new()
  if self.ancillary ne ptr_new() then ptr_free, self.ancillary
  self.ancillary = ptr_new()
  
  ; Read the headers from fname and populate indata
  ; The values of the flux and polarimetry are read from the headers
  im = readfits(fname,h)
  
  countpl=0
  curname=sxpar(h,'ST'+strtrim(countpl,2)+'NAM',count=Nread)
  while Nread gt 0 do begin
    ; read ordinary and extraordinary paths     
    ;ipath=0
    ;readpath=sxpar(h,'ST'+strtrim(countpl,2)+'PTO'+strtrim(ipath,2),count=Nreadp)
    ;curopath=''
    ;while Nreadp gt 0 do begin
    ;  curopath=curopath+readpath
    ;  ipath = ipath + 1
    ;  readpath=sxpar(h,'ST'+strtrim(countpl,2)+'PTO'+strtrim(ipath,2),count=Nreadp)
    ;endwhile
    ;ipath=0
    ;readpath=sxpar(h,'ST'+strtrim(countpl,2)+'PTE'+strtrim(ipath,2),count=Nreadp)
    ;curepath=''
    ;while Nreadp gt 0 do begin
    ;  curepath=curepath+readpath
    ;  ipath = ipath + 1
    ;  readpath=sxpar(h,'ST'+strtrim(countpl,2)+'PTE'+strtrim(ipath,2),count=Nreadp)
    ;endwhile
    
    curopath = self->SxLongStr(h, 'ST'+strtrim(countpl,2)+'PTO')
    curepath = self->SxLongStr(h, 'ST'+strtrim(countpl,2)+'PTE')
    
    curfname=sxpar(h,'ST'+strtrim(countpl,2)+'FN')
    curopath=curopath+'/'
    curepath=curepath+'/'
    
    ; Add photometry star
    photstar = self->Defaultphotstar(name=curname, fname=curfname, $
                                      opath=curopath,epath=curepath,filter=sxpar(h,'FILTER'), $
                                      fo=sxpar(h,'ST'+strtrim(countpl,2)+'FO'),efo=sxpar(h,'ST'+strtrim(countpl,2)+'EFO'),  $
                                      fe=sxpar(h,'ST'+strtrim(countpl,2)+'FE'),efe=sxpar(h,'ST'+strtrim(countpl,2)+'EFE'),  $
                                      fi=sxpar(h,'ST'+strtrim(countpl,2)+'FI'), efi=sxpar(h,'ST'+strtrim(countpl,2)+'EFI'),   $
                                      xo=sxpar(h,'ST'+strtrim(countpl,2)+'XO'), yo=sxpar(h,'ST'+strtrim(countpl,2)+'YO'),   $
                                      xe=sxpar(h,'ST'+strtrim(countpl,2)+'XE'), ye=sxpar(h,'ST'+strtrim(countpl,2)+'YE'),   $
                                      hwp=sxpar(h,'ST'+strtrim(countpl,2)+'HWP'),  $
                                      qu=sxpar(h,'ST'+strtrim(countpl,2)+'QU'), equ=sxpar(h,'ST'+strtrim(countpl,2)+'EQU'),  $
                                      bad=sxpar(h,'ST'+strtrim(countpl,2)+'BAD'), date=sxpar(h,'ST'+strtrim(countpl,2)+'DAT') )
    if self.photlist eq ptr_new() then self.photlist = ptr_new([photstar]) else *self.photlist = [*self.photlist,photstar]
    
    ; Update input data
    if not file_test(curopath+curfname) then begin
      self->Message,['Problem adding header values from '+fname,curfname+' does not exist at '+curopath],priority='WARN',method='AddProduct'
      return
    endif
    if not file_test(curepath+curfname) then begin
      self->Message,['Problem adding header values from '+fname,curfname+' does not exist at '+curopath],priority='WARN',method='AddProduct'
      return
    endif
    
    if self.indata eq ptr_new() then self.indata = ptr_new([curopath+curfname,curepath+curfname]) $
    else *self.indata = [*self.indata,curopath+curfname,curepath+curfname]
    
    countpl=countpl+1
    curname=sxpar(h,'ST'+strtrim(countpl,2)+'NAM',count=Nread)
  endwhile
  
  
END

;****************************************************************************
;     ADDSTAR - Given a position and the photometry of a star for ordinary
;               and extraordinary, it checks if the star matches something
;               existing and add or complete the information
;****************************************************************************
PRO runpolarimetry::AddStar, star, _EXTRA=extraKeywords
  
  ; Check if the array of stars is empty and initialize if yes
  if self.photlist eq ptr_new() then begin
    self.photlist = ptr_new([self->Defaultphotstar(star=star)])
    return
  endif
  
  if n_elements(*self.photlist) eq 0 then begin
    ptr_free,self.photlist
    self.photlist = ptr_new([self->Defaultphotstar(star=star)])
    return
  endif
  
  ; Search for the current star
  names = (*self.photlist).name
  fnames = (*self.photlist).fname
  dates = (*self.photlist).date
  hwps = (*self.photlist).hwp
  k = where(names eq star.name and fnames eq star.fname and dates eq star.date and hwps eq star.hwp)
  
  ; Add the current star or complete an existing star
  if k[0] eq -1 then begin
    *self.photlist=[*self.photlist,self->Defaultphotstar(star=star)]
  endif else begin
    (*self.photlist)[k[0]] = self->CompletePhotStar(self->Addphotstar((*self.photlist)[k[0]], star))
  endelse
    
END


;****************************************************************************
;     READFITSSTAR - read the star properties of a fits file and
;                    adds a list of stars to the photlist structure list
;****************************************************************************
PRO runpolarimetry::readfitsstar, fname
  
  self->Message,'Starting',priority='DEBUG', method='READFITSSTAR
  
  imin = readfits(fname,hin,/silent)
    
  hwp = self->HWPConvert(sxpar(hin, 'HWP'))
  date = sxpar(hin, 'DATE-OBS')
  objname = strtrim(sxpar(hin, 'OBJECT', count=didobject),2)
  if didobject eq 0 then objname='unknown'
  currentstar = 0
  starx_read = sxpar(hin,'ST'+strtrim(currentstar,2)+'X', count=Nkey)
  currentray = strtrim(sxpar(hin,'RAY', count=didray),2)
  
  ; Add all the stars for the current file that are found in the headers
  while (Nkey ne 0.) do begin
    if didray eq 0 then currentray=strtrim(sxpar(hin,'ST'+strtrim(currentstar,2)+'RAY'),2)
    currentname=objname
    if didobject eq 0 then currentname=sxpar(hin,'ST'+strtrim(currentstar,2)+'NAME',count=didobject)
    if didobject eq 0 then currentname='unknown'
    
    star = self->DefaultRayStar(name=currentname, $
                                fname=file_basename(fname),  $
                                path=file_dirname(fname),  $
                                ray=currentray,   $
                                flux=sxpar(hin,'ST'+strtrim(currentstar,2)+'F'),   $
                                eflux=sxpar(hin,'ST'+strtrim(currentstar,2)+'EF'),  $
                                x=sxpar(hin,'ST'+strtrim(currentstar,2)+'X'),   $
                                y=sxpar(hin,'ST'+strtrim(currentstar,2)+'Y'),   $
                                hwp=hwp,  $
                                filter=sxpar(hin,'FILTER'), $
                                dataqual=strtrim(sxpar(hin,'DATAQUAL'),2), $
                                date=date)
    
    
    self->AddStar,star
    
    currentstar = currentstar + 1
    starx_read = sxpar(hin,'ST'+strtrim(currentstar,2)+'X', count=Nkey)
  endwhile
  
END

;****************************************************************************
;     DEFAULTPOLSTAR - Return a default structure storing the data of the 
;                        polarization of a star
;****************************************************************************
FUNCTION runpolarimetry::DefaultPolStar, _EXTRA=extraProperties
  
  defaultprop = {name:'', $
                 fname:'', $
                 opath:'', $
                 epath:'', $
                 photlist:ptr_new(),  $
                 fitstat:9999, $
                 fiterrmsg:'', $
                 P0: 0., $                ; This is the constant value B for P when fitting A*cos()+B
                 dP0: 0., $   
                 P: -1., $
                 dP: -1., $          
                 Xi: 0., $           
                 dXi: 0. $
                 }

  respol = fillstructure(defaultprop,_EXTRA=extraProperties)
  
  return, respol
  
END

;****************************************************************************
;     CALCULATEPOLARIZATION - Return the structure with the values of the
;                             polarization given a photometry list in the structure
;****************************************************************************
FUNCTION runpolarimetry::CalculatePolarization, polstar, fit=fit
  
  ; Check if the array of stars is empty 
  if polstar.photlist eq ptr_new() then begin
    return, polstar
  endif
  
  Nstars = n_elements(*polstar.photlist)
  if Nstars eq 0 then begin
    return, polstar
  endif
  photlistidx=*polstar.photlist
  photlist = (*self.photlist)[photlistidx]
  
  badflag = photlist.bad
  goodidx = where(badflag eq 0)
  Nstars = n_elements(goodidx)
  
  if goodidx[0] eq -1 then begin
    self->Message,'All data is flagged as bad and no polarization calculation is possible',priority='WARN',method='CALCULATEPOLARIZATION'
    return,polstar
  endif
  
  ; We cannot fit if there are less than three instances
  ; In theory, it should be less than three hwp positions since
  ; we may have same hwp in various instances
  ; But this is unlikely to happen, so let me simplify here
  if Nstars lt 4 then fit = 0
  
  if keyword_set(fit) then begin
    res = FitPolarimetry((photlist.hwp)[goodidx]*!pi/180.,(photlist.qu)[goodidx],dqu=(photlist.equ)[goodidx])
    Areturn = res.fit
    Aerror = res.error
    
    polstar.P0 = Areturn[2]
    polstar.dP0 = Aerror[2]
    polstar.P = Areturn[0]
    polstar.Xi = Areturn[1]*180./!pi
    polstar.dP = Aerror[0]
    polstar.dXi = Aerror[1]*180./!pi
    polstar.fitstat = res.status
    polstar.fiterrmsg = res.errmsg
        
    ; We need to check the status of the result res.status 
    kstat = where([1,2,3,4] eq res.status)
    if kstat[0] eq -1 then begin
      self->Message,'Failure - fitting status: '+strtrim(res.status,2),priority='DEBUG',method='CalculatePolarization'
      return, polstar
    endif else begin
      self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='CALCFITPOL'
    endelse
  endif else begin
    Qtotal = 0.
    Utotal = 0.
    for i=0, Nstars-1 do begin
      qterm = photlist.qu[goodidx[i]]*cos(photlist.hwp[goodidx[i]]*4.*!pi/180.)
      uterm = photlist.qu[goodidx[i]]*sin(photlist.hwp[goodidx[i]]*4.*!pi/180.)
      
      qtotal = qtotal + qterm
      utotal = utotal + uterm
    endfor
    qtotal = 2./Nstars*qtotal
    utotal = 2./Nstars*utotal
    
    ptotal = sqrt(qtotal^2+utotal^2)
    if qtotal eq 0. then begin
      xitotal = 90.
    endif else begin
      xitotal = 0.5*atan(utotal/qtotal)
      xitotal = xitotal*180./!pi
    endelse
    polstar.P0 = 0.
    polstar.dP0 = 0.
    polstar.P = Ptotal
    polstar.Xi = xitotal
    polstar.dP = 0.
    polstar.dXi = 0.
  endelse
  
  return,polstar
  
END

;****************************************************************************
;     COMPLETEPOLSTAR - Fill the information of the polarization considering
;                      the current photometry list
;****************************************************************************
PRO runpolarimetry::CompletePolStar, _EXTRA=extraKeywords
  
  ; Initialize polarization list
  if (self.pollist ne ptr_new()) then begin
    ptr_free,self.pollist
    self.pollist = ptr_new()
  endif
  ; Check if the array of stars is empty 
  if self.photlist eq ptr_new() then begin
    self->Message,'Photometric star array is null',priority='WARN',method='COMPLETEPOLSTAR'
    return
  endif
  
  if n_elements(*self.photlist) eq 0 then begin
    self->Message,'Photometric star array is empty',priority='WARN',method='COMPLETEPOLSTAR'
    return
  endif
  
  ; Fill the photometry list for each of the stars
  Nstars = n_elements(*self.photlist)
  names = (*self.photlist).name
  fnames = (*self.photlist).fname
  opaths = (*self.photlist).opath
  epaths = (*self.photlist).epath
  completeidx = replicate(0,Nstars)
  for i=0,Nstars-1 do begin
    if completeidx[i] eq 0 then begin
      k = where(names eq names[i])
      currentphotlist = k  ;(*self.photlist)[k]
      newpol=self->DefaultPolStar(photlist=currentphotlist, name=names[i], fname=fnames[i], opath=opaths[i], epath=epaths[i])
      if self.pollist eq ptr_new() then begin
        self.pollist = ptr_new([newpol])
      endif else begin
        *self.pollist = [*self.pollist, newpol]
      endelse
      completeidx[k] = 1
    endif
  endfor
  
  
  ; Check if we filled any list. There should be at list one
  if (self.pollist eq ptr_new()) then begin
    return
  endif
  
  ; Now calculate the photometry for each of the found stars
  for i=0,n_elements(*self.pollist)-1 do begin
    (*self.pollist)[i] = self->CalculatePolarization((*self.pollist)[i], /fit)
  endfor
  
END

;****************************************************************************
;     GETCONFIGURATION - Returns an array of structures with the attributes
;                        that configurable
;****************************************************************************
FUNCTION runpolarimetry::GetConfiguration
  
  self->Message,'Starting',priority='DEBUG',method='GETCONFIGURATION'
  
  resarr = {name:'deltapos',value:strtrim(self.deltapos,2),type:'int',error:0}
    
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
PRO runpolarimetry::run, skipphot=skipphot, _EXTRA=extraKeywords
  
  self->Message,['============ POLARIMETRY =============','Starting'],priority='DEBUG',method='RUN'
  
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
  
  ; Clean star property array and read information
  if not keyword_set(skipphot) then begin
    if self.photlist ne ptr_new() then begin
      ptr_free,self.photlist
      self.photlist = ptr_new()
    endif
    
    ; for each image we will loop in the stars inside the header
    for i=0,Ndata-1 do begin
      self->Message,['>>>>>>>>','Calculating polarimetry for file: '+(*self.indata)[i]],priority='INFO',method='RUN'
      
      self->readfitsstar, (*self.indata)[i]
    endfor
  endif
  if self.photlist eq ptr_new() then begin
    if keyword_set(skipphot) then begin
      self->Message,'There is not photometry information, try without skipping photometry reading from input data.',priority='WARN',method='RUN'
    endif else begin
      self->Message,'There is not photometry information. The input data is probably wrong.',priority='WARN',method='RUN'
    endelse
    self->Message,'Processing was aborted!',priority='ERR',method='RUN'
    return
  endif
  
  
  ; Clean polarization property array and calculate polarization from photometry
  if self.pollist ne ptr_new() then begin
    ptr_free,self.pollist
    self.pollist = ptr_new()
  endif
  self->CompletePolStar
  
  
  if self.interactive ne 0 then begin
    winnum=0
    self->Plot  ;,winnum
  endif
    
  ; Clean the output file names
  if self.outdata ne ptr_new() then begin
    ptr_free,self.outdata
    self.outdata = ptr_new()
  endif
  
  ; Create output files
  for i=0,n_elements(*self.pollist)-1 do begin
    starpol = (*self.pollist)[i]
    refname = starpol.fname
    if refname eq '' then refname = starpol.name
    refname=starpol.opath+'/'+refname
    
    if file_test(refname) eq 1 then begin
      ; we could make a cube with all the fits file images
      im = readfits(refname,hin)
      hout = hin
    endif else begin
      im = [[0.,0.],[0.,0.]]
      mkhdr,hout,0
    endelse
    
    ; Add polarization values to the header and images
    sxaddpar,hout,'PSTAT'+strtrim(i,2),starpol.fitstat
    sxaddpar,hout,'PEMSG'+strtrim(i,2),starpol.fiterrmsg
    sxaddpar,hout,'POLLEV'+strtrim(i,2),starpol.P
    sxaddpar,hout,'ePOLLEV'+strtrim(i,2),starpol.dP
    sxaddpar,hout,'POLANG'+strtrim(i,2),starpol.Xi
    sxaddpar,hout,'ePOLANG'+strtrim(i,2),starpol.dXi
    sxaddpar,hout, 'OBJCLASS',OBJ_CLASS(self)
    
    ; Remove keywords that do not make any sense any more
    sxdelpar,hout,'RAY'
    sxdelpar,hout,'HWP'
    countstidx=0
    junk=sxpar(hout,'ST'+strtrim(countstidx,2)+'X',count=istherestar)
    while(istherestar gt 0) do begin
      ;sxdelpar,hout,'ST'+strtrim(countstidx,2)+'X'
      ;sxdelpar,hout,'ST'+strtrim(countstidx,2)+'Y'
      sxdelpar,hout,'ST'+strtrim(countstidx,2)+'F'
      sxdelpar,hout,'ST'+strtrim(countstidx,2)+'EF'
      sxdelpar,hout,'ST'+strtrim(countstidx,2)+'SK'
      sxdelpar,hout,'ST'+strtrim(countstidx,2)+'ESK'
      
      countstidx = countstidx + 1
      junk=sxpar(hout,'ST'+strtrim(countstidx,2)+'X',count=istherestar)
    endwhile
    
    ; Write in the headers the information of all stars in the photlist array
    if self.photlist ne ptr_new() then begin
      for countpl=0,n_elements(*self.photlist)-1 do begin
        curphot=(*self.photlist)[countpl]
        sxdelpar,hout,'ST'+strtrim(countpl,2)+'BAD'
        if curphot.bad eq 1 then begin
          sxaddpar,hout,'ST'+strtrim(countpl,2)+'BAD','T'        
        endif 
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'NAM',curphot.name
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'FN',curphot.fname
        
        hout = self->SxAddLongStr(hout,'ST'+strtrim(countpl,2)+'PTO', curphot.opath)
        hout = self->SxAddLongStr(hout,'ST'+strtrim(countpl,2)+'PTE', curphot.epath)
        
        ;Npath=float(strlen(curphot.opath))/68.
        ;for ipath=0,fix(Npath)-1 do begin
        ;  sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTO'+strtrim(ipath,2),strmid(curphot.opath,68*ipath,68)
        ;endfor
        ;if Npath ne fix(Npath) then begin
        ;  sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTO'+strtrim(ipath,2),strmid(curphot.opath,68*ipath,68)
        ;endif
        ;Npath=float(strlen(curphot.epath))/68.
        ;for ipath=0,fix(Npath)-1 do begin
        ;    sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTE'+strtrim(ipath,2),strmid(curphot.epath,68*ipath,68)
        ;endfor
        ;if Npath ne fix(Npath) then begin
        ;  sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTE'+strtrim(ipath,2),strmid(curphot.epath,68*ipath,68)
        ;endif
        ;sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTO',curphot.opath
        ;sxaddpar,hout,'ST'+strtrim(countpl,2)+'PTE',curphot.epath
        
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'FO',curphot.fo
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'EFO',curphot.efo
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'FE',curphot.fe
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'EFE',curphot.efe
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'FI',curphot.fi
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'EFI',curphot.efi
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'XO',curphot.xo
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'YO',curphot.yo
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'XE',curphot.xe
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'YE',curphot.ye
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'HWP',curphot.hwp
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'QU',curphot.qu
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'EQU',curphot.equ
        sxaddpar,hout,'ST'+strtrim(countpl,2)+'DAT',curphot.date
      endfor
    endif
    
    ; Create file name for the current starpol
    outfname = self->createFname(refname, _EXTRA=extraKeywords)
    if self.outdata eq ptr_new() then self.outdata = ptr_new([outfname]) $
    else *self.outdata = [*self.outdata,outfname]
    
    ; Write fits file with images/plane and header
    self->Message,'Writing file '+outfname,priority='DEBUG',method='RUN'
    writefits,outfname,im,hout
    
    ; Make a plot with the fit
    pos = strpos(outfname, '.', /REVERSE_SEARCH)
    if pos[0] eq -1 then begin
      self->updatePlot, 0, badidx=(*self.photlist).bad, ps=outfname+'.ps'
    endif else begin
      self->updatePlot, 0, badidx=(*self.photlist).bad, ps=strmid(outfname,0,pos[0])+'.ps'
    endelse
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END

;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runpolarimetry::cleanup
  
  self->runstep::cleanup
  
  if self.photlist ne ptr_new() then ptr_free,self.photlist
  if self.pollist ne ptr_new() then begin
    for i=0,n_elements(*self.pollist)-1 do begin
      pollist = (*self.pollist)[i]
      if pollist.photlist ne ptr_new() then ptr_free,pollist.photlist
    endfor
    ptr_free,self.pollist
  endif
  
END
;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runpolarimetry::init, Deltapos=Deltapos, _Extra=extraKeyword
  
  if self->runstep::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (runstep)',priority='ERR',method='INIT'
    return,0
  endif
  
  if not keyword_set(deltapos) then deltapos = 40.
  if deltapos lt 0. then deltapos = 40.
  self.deltapos = deltapos
  
  return,1
  
END

;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;      GRAPHIC METHODS FOR INTERACTIVE MODE
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************
;********************************************************************************************************************************************************

;****************************************************************************
;     UPDATEPLOT - Update the data in the plotting area
;****************************************************************************
PRO runpolarimetry::updatePlot, winnum, specialidx=specialidx, badidx=badidx, selregion=selregion, zoom=zoom, ps=ps
  
  self->Message,'Starting',priority='DEBUG',method='UPDATEPLOT'
  if n_elements(winnum) eq 0 then begin
    self->Message,'No winnum',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
  
  if self.photlist eq ptr_new() then begin
    self->Message,'Null photlist',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
  
  photlist = *self.photlist
  if n_elements(photlist) eq 0 then begin
    self->Message,'No photlist',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
  
  if self.pollist eq ptr_new() then begin
    self->Message,'Null pollist',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
  
  pollist = (*self.pollist)[0]
  if n_elements(pollist) eq 0 then begin
    self->Message,'No pollist',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
    
  if keyword_set(ps) then begin
    if strtrim(ps,2) eq '1' or not file_test(file_dirname(strtrim(ps,2)),/directory) then begin
      sFile = DIALOG_PICKFILE(PATH=pollist.opath, TITLE='Select PS File to save the plot',  FILTER='*.ps')
    endif else begin
      sFile = ps
    endelse
    while file_test(sFile, /directory) do begin
      self->Message,['This file does not exists: '+sFile,'Select a real file.'],priority='WARN',method='UPDATEPLOT'
      sFile = DIALOG_PICKFILE(PATH=pollist.opath, TITLE='Select PS File to save the plot',  FILTER='*.ps')
    endwhile
    thisDevice = !D.Name
    SET_PLOT, 'PS'	; Select the PostScript driver.
    DEVICE, FILENAME = sFile, /COLOR, BITS=8
  endif else begin
    wset,winnum
    mkct,0
  endelse
  
  plotnum = 0
  
  scaleP=100.
  
  ; Check the requested zoom
  CASE n_elements(zoom) of
    0: begin  ; keep same zoom
        minhwp = !x.crange[0]
        maxhwp = !x.crange[1]
        minP = !y.crange[0]
        maxP = !y.crange[1]
        if minhwp eq maxhwp or minP eq maxP then begin
          minhwp = 0.
          maxhwp = 180.
          minP=min(photlist.qu,max=maxP)
          minP = minP*scaleP
          maxP = maxP*scaleP        
        endif
      end
    1: begin  ; unzoom to initial values
        minhwp = 0.
        maxhwp = 90.
        minP=min(photlist.qu,max=maxP)
        minP = minP*scaleP
        maxP = maxP*scaleP
      end
    4: begin  ; zoom to specified values [x1,y1,x2,y2]
        minhwp=min([zoom[0],zoom[2]],max=maxhwp)
        minP=min([zoom[1],zoom[3]],max=maxP)
      end
    else: begin
          if keyword_set(ps) then begin
            DEVICE, /CLOSE
            SET_PLOT, thisDevice
          endif
          self->Message,'Wrong zoom value',priority='WARN', method='UPDATEPLOT'
          return
        end
  ENDCASE
  
  ; absice for overploting cos fit
  absicedeg = findgen(1000)/1000.*(maxhwp-minhwp)+minhwp
  absicerad = absicedeg/180.*!pi
  
  plot,absicedeg,scaleP*pollist.P0+scaleP*pollist.P*cos(4.*absicerad-2*pollist.Xi/180.*!pi),linestyle=2,xtitle='HWP (degrees)', ytitle='P (%)',xrange=[minhwp,maxhwp],yrange=[minP,maxP]
  
  oplot,photlist.hwp,photlist.qu*scaleP, psym=2
  errplot,photlist.hwp,scaleP*photlist.qu-scaleP*photlist.equ,scaleP*photlist.qu+scaleP*photlist.equ
  
  if n_elements(specialidx) gt 0 then begin
    oplot,[(photlist.hwp)[specialidx]],[(photlist.qu)[specialidx]*scaleP], psym=2, color=2
    errplot,[(photlist.hwp)[specialidx]],[scaleP*(photlist.qu)[specialidx]]-scaleP*(photlist.equ)[specialidx],[scaleP*(photlist.qu)[specialidx]]+scaleP*(photlist.equ)[specialidx], color=2
  endif 
  
  if n_elements(badidx) eq n_elements(photlist) then begin
    colornonsel=3
    colorsel=4
    for i=0,n_elements(badidx)-1 do begin
      curcolor=colornonsel
      if badidx[i] eq 1 then begin
        if n_elements(specialidx) gt 0 then if (where(specialidx eq i))[0] ne -1 then curcolor = colorsel
        oplot,[(photlist.hwp)[i]],[(photlist.qu)[i]*scaleP], psym=2, color=curcolor
        errplot,[(photlist.hwp)[i]],[scaleP*(photlist.qu)[i]]-scaleP*(photlist.equ)[i],[scaleP*(photlist.qu)[i]]+scaleP*(photlist.equ)[i], color=colornonsel
    endif
    endfor
  endif
  
  if keyword_set(selregion) then plots, selregion,color=2, /DEVICE
  
  
  ; Write the information of the object and serkowski fit
  x0=minhwp+(maxhwp-minhwp)/30.
  dy=(maxP-minP)/30.
  y0=maxP-2.*dy
  charsz=1.5
  xystr=['Object: '+pollist.name, $
         'P0: '+STRING(scaleP*pollist.P0,FORMAT='(F6.2)')+'% +/- '+STRING(scaleP*pollist.dP0,FORMAT='(F6.2)'), $
         'P1: '+STRING(scaleP*pollist.P, FORMAT='(F6.2)')+'% +/- '+STRING(scaleP*pollist.dP,FORMAT='(F6.2)'), $
         'Phase: '+STRING(pollist.Xi, FORMAT='(F6.2)')+'deg +/- '+STRING(pollist.dXi,FORMAT='(F6.2)')]
  nxystr = n_elements(xystr)
  xyouts,replicate(x0,nxystr),y0-indgen(nxystr)*dy*charsz,xystr, charsize=charsz
  
  if keyword_set(ps) then begin
    DEVICE, /CLOSE
    SET_PLOT, thisDevice
  endif
  return
  
  
  ; We should consider a fit for each star but for now, we just stop here
  for i=0,n_elements(*self.starpol)-1 do begin
    starpol = (*self.pollist)[i]
    photlist = starpol.photlist
    if plotnum eq 0 then begin
      plot,photlist.hwp,photlist.qu,psym=2
      errplot,photlist.hwp,photlist.qu-photlist.dqu,photlist.qu+photlist.dqu
      
      ; Overplot the fit P*cos(4*hwp-Xi)
      oplot,absicedeg,starpol.P0+starpol.P*cos(4.*absicerad-2*starpol.Xi/180.*!pi),linestyle=2
    endif else begin
      oplot,photlist.hwp,photlist.qu,psym=2,color=2+plotnum-1
      errplot,photlist.hwp,photlist.qu-photlist.dqu,photlist.qu+starpol.dqu,color=2+plotnum-1
      
      ; Overplot the fit P*cos(4*hwp-Xi)
      oplot,absicedeg,starpol.P0+starpol.P*cos(4.*absicerad-2*starpol.Xi),linestyle=2,color=2+plotnum-1
    endelse
  endfor
  
  
  
  self->Message,'DONE',priority='DEBUG',method='UPDATEPLOT'
  
END

;****************************************************************************
;     PLOTDRAWAREA - Method when the event comes from the drawing area
;****************************************************************************
PRO runpolarimetry::plotdrawarea, event, parameters
  ;self->Message,'Called Plot_event with drawarea',priority='DEBUG',method='PLOT_EVENT'
  
  ; exit if a key was previously pressed
  if parameters.prevkey eq 1 then begin
    self->Message,'Previous key task pending',priority='DEBUG',method='PLOTDRAWAREA'
    return
  endif
  
  photlist = *self.photlist
  badidx=photlist.bad
  widget_control, event.id, get_value=winnum
  
  ; If the user pressed the button, we record the position
  if event.press eq 1 and event.ch eq 0 then begin
    parameters.x = event.x
    parameters.y = event.y
    parameters.xmove = event.x
    parameters.ymove = event.y
    widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
    self->Message,'    pressed button at '+strtrim(event.x,2)+', '+strtrim(event.y,2),priority='DEBUG',method='PLOT_EVENT'
    return
  endif
  
  ; move selection area if motion after clicking (paramters.xmove<>-1) and dragging
  if parameters.xmove ne -1 and event.type eq 2 then begin 
    self->updatePlot, winnum, selregion=transpose([[parameters.x,parameters.x,event.x,event.x,parameters.x],[parameters.y,event.y,event.y,parameters.y,parameters.y]]), badidx=badidx
    ;plots,[parameters.x,parameters.x,parameters.xmove,parameters.xmove,parameters.x],[parameters.y,parameters.ymove,parameters.ymove,parameters.y,parameters.y],color=0, /DEVICE
    ;plots, [parameters.x,parameters.x,event.x,event.x,parameters.x],[parameters.y,event.y,event.y,parameters.y,parameters.y],color=2, /DEVICE
    parameters.xmove = event.x
    parameters.ymove = event.y
    widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
  endif
  
  ; if release button with the 'a' key - select all data points
  if event.release eq 1 and event.ch eq 97 then begin
    parameters.selidx = indgen(n_elements(parameters.selidx))
    widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
    
    self->updatePlot, winnum, specialidx=parameters.selidx,badidx=badidx
    return
  endif
  
  ; if release button with mouse
  if event.release eq 1 and event.ch eq 0 then begin
    self->Message,'    released button at '+strtrim(event.x,2)+', '+strtrim(event.y,2),priority='DEBUG',method='PLOT_EVENT'
    
    ; Reset the selection square parameters
    parameters.xmove = -1
    parameters.ymove = -1
    widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
    
    x1=parameters.x
    y1=parameters.y
    x2=event.x
    y2=event.y
    
    data1 = convert_coord(x1,y1,/to_data,/device)
    data2 = convert_coord(x2,y2,/to_data,/device)
    
    ; We are going to select the data if we did not press any modifier key
    ; and if not, we will be zooming in the selected are
    ; In the first case when the released and pressed points are the same, we reset the zoom
    ; In the second case we zoom in the selected area
    
    ; when we clicked in the same position we do something with the closest data if it is close enougth to the mouse
    if (sqrt(total((data1-data2)^2))) lt 1 then begin
      
      if event.modifiers eq 0 then begin
        ; Find which is the closest value in photlist
        d=sqrt((photlist.hwp-data1[0])^2 + (photlist.qu*100.-data1[1])^2)
        dmin = min(d,fidx)
        
        if dmin lt 10 then begin
          farr = [(photlist.opath)[fidx],(photlist.epath)[fidx]]+'/'+(photlist.fname)[fidx]
        endif else begin
          self->updatePlot, winnum, badidx=badidx
          return
        endelse
      endif 
      if event.modifiers eq 2 then begin
        zoom = 1
      endif
    endif else begin
      if event.modifiers eq 0 then begin
        hwpmin = min([data1[0],data2[0]],max=hwpmax)
        pmin = min([data1[1],data2[1]],max=pmax)
        
        ; Find what data is within [hwpmin,hwpmax] and [pmin,pmax] and do something
        khwp = where(photlist.hwp ge hwpmin and photlist.hwp le hwpmax)
        if khwp[0] eq -1 then begin
          self->updatePlot, winnum, badidx=badidx
          return
        endif
        kp = where((photlist.qu)[khwp]*100. ge pmin and (photlist.qu)[khwp]*100. le pmax )
        if kp[0] eq -1 then begin
          self->updatePlot, winnum, badidx=badidx
          return
        endif
        ; If we get to this point it means that there are points in the selected area, let's build the array of file names
        fidx = khwp[kp]
      endif 
      if event.modifiers eq 2 then begin
        zoom = [data1[0:1],data2[0:1]]
      endif
    endelse
    
    if event.modifiers eq 0 then begin
      parameters.selidx = replicate(-1,n_elements(parameters.selidx))
      parameters.selidx[0:n_elements(fidx)-1] = fidx
      widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
    endif
    if event.modifiers eq 2 then begin
      fidx = where(parameters.selidx ne -1)
    endif
    
    if keyword_set(zoom) then begin
      self->updatePlot, winnum, zoom=zoom, specialidx=fidx, badidx=badidx
    endif else begin
      self->updatePlot, winnum, specialidx=fidx, badidx=badidx
    endelse
    
    return
  endif
  
  ; For now, we exit if we did not press any key.
  ; We could add some kind of status to overdraw a square on the display as we drag
  if event.press eq 0 and event.release eq 0 then return
  
  ; if we press ? we print the help
  if event.ch eq 63 and event.release then begin
    print,''
    print,''
    print,'============='
    print,'Keyword List'
    print,'============='
    print,''
    print,'Select images by clicking next to a data point or dragging the area containing the data points'
    print,'Press close to an empty area to deselect data points.'
    print,'Zoom around an area by pressing Ctrl, pressing the left click and dragging the mouse.'
    print,''
    print,'Press the following keys to operate on the selected images.'
    print,'d: open selected images with atv'
    print,'i: get information about selected images'
    print,'p: manually redo photometry of selected images using atv'
    print,'x: remove selected images from the list to calculate polarization'
    print,'r: restore selected images to the list to calcualte polarization'
    print,''
    return
  end
  
  ; Here we know that it is not a button click
  kidx = where(parameters.selidx ne -1)
  if kidx[0] eq -1 then begin
    self->updatePlot, winnum, badidx=badidx
    self->Message,'You must select at least one element',priority='INFO',method='PLOTDRAWAREA'
    print,'Press ? for help'
    return
  endif
  
  ; if we pressed a key let's put parameters.prevkey to 1 so we don't have multiple events from the same key stroke
  parameters.prevkey = 1
  widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
  
  fidx = (parameters.selidx)[kidx]
  
  ; Create an array with the file names corresponding to the selected data
  farr=''
  for countfidx=0,n_elements(fidx)-1 do begin
    curf=(photlist.opath)[fidx[countfidx]]+'/'+(photlist.fname)[fidx[countfidx]]
    if file_test(curf) then farr=[farr,curf]
    curf = (photlist.epath)[fidx[countfidx]]+'/'+(photlist.fname)[fidx[countfidx]]
    if file_test(curf) then farr=[farr,curf]
  endfor
  
  if n_elements(farr) eq 1 then begin
    self->Message,'No file was found for current photometry list',priority='WARN',method='PLOTDRAWAREA'
    return
  endif
  farr=farr[1:n_elements(farr)-1]
  
  ; Action according to the requested key
  CASE event.ch of
    ; if we pressed the d key we show the image with atvcal
    100: begin
          if (xregistered('atv', /noshow)) then begin
            common atv_state, state
            atv_shutdown
          endif
          atvcal,farr,/profile
        end
    ; if we pressed i we get information about the selected objects
    105: begin
        for i=0,n_elements(photlist)-1 do help,photlist[i]
        junk = dialog_message(farr)
      end
    ;if we press the p key we redo the photometry of these target for o and e rays
    112: begin          
          if (xregistered('atv', /noshow)) then begin
            common atv_state, state
            atv_shutdown
          endif
          dophotometry,farr, /interactive, profile='MOFFAT', /overwrite
          
          ; This updates the photometry list of the object
          for i=0,n_elements(farr)-1 do begin
            self->readfitsstar, farr[i]
          endfor
          
          ; This updates the polarization list of the object
          self->CompletePolStar
       end
    ; if we pressed r, we restore the data that was flagged as r in the selected points
    114: begin
        ; restore data
        for countfidx=0,n_elements(fidx)-1 do begin
          photlist[fidx[countfidx]].bad = 0
        endfor
        self->updatePlot, winnum, specialidx=fidx, badidx=photlist.bad
        
        ; Update the photometry list in the current object
        ;*(*self.pollist)[polstaridx].photlist = photlist
        *self.photlist = photlist
        
        ; This updates the polarization list of the object
        self->CompletePolStar
        
        ; Update headers of the photometric step
        for i=0,n_elements(farr)-1 do begin
          self->Message,'Updating DATAQUAL to GOOD for '+farr[i],priority='DEBUG',method='PLOT_EVENT'
          imcur = readfits(farr[i],hcur,/silent)
          sxaddpar,hcur,'DATAQUAL','GOOD'
          writefits,farr[i],imcur,hcur
        endfor
      end
    ; if we pressed x, we remove the specific data from the list
    120: begin
        ; remove data
        for countfidx=0,n_elements(fidx)-1 do begin
          photlist[fidx[countfidx]].bad = 1
        endfor
        self->updatePlot, winnum, specialidx=fidx, badidx=photlist.bad
        
        ; Update the photometry list in the current object
        ;*(*self.pollist)[polstaridx].photlist = photlist
        *self.photlist = photlist
        
        ; This updates the polarization list of the object
        self->CompletePolStar
        
        ; Update headers of the photometric step
        for i=0,n_elements(farr)-1 do begin
          self->Message,'Updating DATAQUAL to BAD for '+farr[i],priority='DEBUG',method='PLOT_EVENT'
          imcur = readfits(farr[i],hcur,/silent)
          sxaddpar,hcur,'DATAQUAL','BAD'
          writefits,farr[i],imcur,hcur
        endfor
      end
    else: print,'Press ? for help'
  ENDCASE
  
  ; Now we return the hand to any other event
  parameters.prevkey = 0
  widget_control,event.id, set_uvalue={method:'plot_event', object:self, parameters:parameters}
END

;****************************************************************************
;     PLOT_EVENT - Handle events of the plot method
;****************************************************************************
PRO runpolarimetry::plot_event, event, parameters
  
  if keyword_set(parameters) then begin
    index = Where(StrPos(Tag_Names(parameters), 'WNAME') EQ 0, count)
    if count gt 0 then wname = parameters.(index[0])
  endif
  
  if not keyword_set(wname) then begin
    self->Message,'Widget name is not set for widget '+strtrim(event.id),priority='INFO',method='PLOT_EVENT'
    return
  endif
  
  case strlowcase(wname) of
    'top': begin
           help,event ;drawpan
         end
    'continue': begin
                widget_control,event.top,/destroy
                if self.interactive eq 2 then self.interactive = 1
              end
    'save': begin
            self->updatePlot, 0, badidx=(*self.photlist).bad, /ps
          end
    'drawarea': begin
                self->plotdrawarea, event, parameters
              end
    else: begin
          self->Message,'Sorry, I do not recognize '+wname,priority='INFO',method='PLOT_EVENT'
        end
  endcase
  
END

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO runpolarimetry::plot, winnum

  if self.pollist eq ptr_new() then begin
    self->Message,'pollist array is null',priority='DEBUG',method='PLOT'
    return
  endif
  pollist = (*self.pollist)[0]
  
  photlist = *self.photlist
  
  if n_elements(pollist) eq 0 then begin
    self->Message,'pollist array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  if n_elements(winnum) eq 0 then begin
    ; base widget
    top=widget_base(title='Polarization Measurements for '+(photlist.name)[0]+' at '+(photlist.filter)[0],  /column, /TLB_SIZE_EVENTS, $
                    uvalue={method:'plot_event', object:self, parameters:{wname:'top',drawpan:0L}}, event_pro='class_eventhand') ;event_pro='top_event',
    
    ; Info area
    infobase = widget_base(top,/column)
      info = widget_label(infobase, value='Fit status: '+strtrim(pollist.fitstat,2))
      info = widget_label(infobase, value='Fit message: '+pollist.fiterrmsg)
    
    ; Plotting area
    plotpanel = widget_draw(top,xsize=1000., ysize=500., retain=2, $
                               /button_events, /MOTION_EVENTS, KEYBOARD_EVENTS=1 , $
                               uvalue={method:'plot_event', object:self, parameters:{wname:'drawarea',prevkey:0,x:-1.,y:-1.,xmove:-1,ymove:-1,selidx:replicate(-1,n_elements(photlist))}}, event_pro='class_eventhand')
    widget_control, plotpanel, get_value=winnum
    wset,winnum
    widget_control,top,set_uvalue={method:'plot_event', object:self, parameters:{wname:'top',drawpan:plotpanel}}
    
    ; Button panel
    butpan = widget_base(top,/row)
      butok  = widget_button(butpan, value='Continue', uvalue={method:'plot_event', object:self, parameters:{wname:'continue'}}, event_pro='class_eventhand')
      butps = widget_button(butpan, value='Save', uvalue={method:'plot_event',object:self,parameters:{wname:'save'}}, event_pro='class_eventhand')
      
    ; realize and start widgets
    widget_control, top, /realize
  endif else begin
    DEVICE, WINDOW_STATE=winstat
    if winstat[winnum] eq 0 then window,winnum,retain=2 else wset,winnum
  endelse
  
  self->updatePlot, winnum, badidx=photlist.bad
  
  if keyword_set(top) then begin
    xmanager, 'plotmanager', top, EVENT_HANDLER='dummy_eventhand', /no_block
  endif
  
  if (not(xregistered('plotmanager', /noshow))) then begin
    self->Message, 'No window currently exists.', priority='ERR',method='PLOT'
    return
  endif

  if self.interactive ne 0 then self.interactive = 2

  while (self.interactive EQ 2 && xregistered('plotmanager', /noshow)) do begin
    wait, 0.01
    void = widget_event(/nowait)
  endwhile
    
  widget_control, /hourglass
  
END


;****************************************************************************
;     RUNPOLARIMETRY__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runpolarimetry__define
  
  struct={runpolarimetry, $
          photlist: ptr_new(), $    ; Array with the list of photometry for all the stars
          pollist: ptr_new(), $     ; Array with the list of polarization for all the stars
	  starpol:ptr_new(), $      ; Array with the polarimetry of stars
          deltapos:20., $           ; Number of pixels considered as the possible variations between images of the position of the same star
          wpdist:[0.,0.], $         ; X and Y distance of the ordinary and extraordinary stars in the detector (in pixels)
          inherits runstep}
END
