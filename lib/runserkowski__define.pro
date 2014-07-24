; NAME:
;     RUNSERKOWSKI - Version 1.0
;
; PURPOSE: Fits the serkwoski law to a set of points with different filters
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
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 29th 2013


;****************************************************************************
;     GROUPARRAY - Creates an array of strings containing information about the object. 
;****************************************************************************
FUNCTION runserkowski::GroupArray,labels=labels, filebase=filebase, details=details, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='GROUPARRAY'
  
  finalarr = self->runstep::GroupArray(labels=keyword_set(labels),filebase=keyword_set(filebase),_EXTRA=extraKeywords)
  if self.starpol eq ptr_new() then begin
    self->Message,'No star polarization calculations, calling default Group Array from parent class',priority='INFO',method='GROUPARRAY'
    return,finalarr
  endif 
  if n_elements(*self.starpol) eq 0 then begin
    self->Message,'No star polarization calculations, calling default Group Array from parent class',priority='INFO',method='GROUPARRAY'
    return,finalarr
  endif
  
  if not keyword_set(details) then return,finalarr
  
  label_ref = ['starx','stary','P','dP','Xi','dXi','Pmax','dPmax','Wmax','dWmax']
  
  i = 0
    starpol = (*self.starpol)[i]
    label_arr = 'Star'+strtrim(i+1,2)+':'+label_ref
    
    resarr = [strtrim(starpol.starx,2),strtrim(starpol.stary,2),'['+strjoin(strtrim(starpol.P,2),',')+']','['+strjoin(strtrim(starpol.dP,2),',')+']', $
                 '['+strjoin(strtrim(starpol.Xi,2),',')+']','['+strjoin(strtrim(starpol.dXi,2),',')+']', $
                 strtrim(starpol.Pmax,2),strtrim(starpol.dPmax,2),strtrim(starpol.Wmax,2),strtrim(starpol.dWmax,2)]
  
  for i=1,n_elements(*self.starpol)-1 do begin
    starpol = (*self.starpol)[i]
    label_arr = [label_arr,'Star'+strtrim(i+1,2)+':'+label_ref]
    auxarr = [strtrim(starpol.starx,2),strtrim(starpol.stary,2),'['+strjoin(strtrim(starpol.P,2),',')+']','['+strjoin(strtrim(starpol.dP,2),',')+']', $
                 '['+strjoin(strtrim(starpol.Xi,2),',')+']','['+strjoin(strtrim(starpol.dXi,2),',')+']', $
                 strtrim(starpol.Pmax,2),strtrim(starpol.dPmax,2),strtrim(starpol.Wmax,2),strtrim(starpol.dWmax,2)]
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

;****************************************************************************
;     CREATEFNAME - Create a filename associated to a specific data
;****************************************************************************
FUNCTION runserkowski::createFname, fnames, name=name, _EXTRA=extraKeywords
  
  self->Message,'Starting...',priority='DEBUG',method='CREATEFNAME'
  
  ; We should extract some representative value of fnames
  fname_arr = strsplit(fnames[0],'|',/extract)
  refname = file_dirname(fname_arr[0])
  
  self->Message,'Reference name is '+refname,priority='DEBUG',method='CREATEFNAME'
  self->Message,'Switch to parent method (CREATEFNAME)',priority='DEBUG',method='CREATEFNAME'
  resname = self->runstep::CreateFname(refname+'/'+name+'.fits', _EXTRA=extraKeywords)
  
  self->Message,'Resulting name is '+resname,priority='DEBUG',method='CREATEFNAME'
  
  return,resname
END

;******************************************************************************
;     DUPLICATE -  Create a new object with some of the information of self
;                  I am sure we could make a more general duplicate function in classdef
;                  There is already a classdef:copy but we need to check it out first
;******************************************************************************
FUNCTION runserkowski::Duplicate
  
  newobj = self->runstep::Duplicate()
  
  if newobj eq obj_new() then begin
    self->Message,'Parent Duplicate function returned null object',priority='DEBUG',method='DUPLICATE'
    return,obj_new()
  endif
  
  newobj->SetProperty,Deltapos=self.Deltapos
  
  if self.starpol ne ptr_new() then begin
    if n_elements(*self.starpol) gt 0 then newobj->SetProperty,starpol=ptr_new(*self.starpol)
  endif
  
  return,newobj
  
END

;****************************************************************************
;     FILTERCONVERT - If the value is a number returns the value if not,
;                     check the wavelength
;****************************************************************************
FUNCTION runserkowski::FilterConvert, filter
  
  self->Message,'Starting',priority='DEBUG',method='FILTERCONVERT'
  
  if size(filter,/type) ne 7 then begin
    self->Message,'Filter type is not string. Returning value: '+strtrim(filter,2),priority='DEBUG',method='FILTERCONVERT'
    return,filter
  endif
  
  starpol = self->DefaultStar()
  reffilterpos = starpol.filtername  ; ['U','B','V','R','I']
  filterres = starpol.filter ; [0.36,0.44,0.55,0.64,0.7]
  
  k = where(reffilterpos eq strtrim(filter,2))
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='FILTERCONVERT'
  
  if k[0] eq -1 then return, 0.36 else return, filterres[k[0]]
  
END


;****************************************************************************
;     POLSTARSTATUS - Returns the star after completing the values q or u in the polarimetry
;               Status indicates how this new star relates to the star polarization object
;                   STATUS:
;                       * -1: Not valid
;                       * 0: Added to starpol
;                       * 1: Did not belong to starpol
;****************************************************************************
FUNCTION runserkowski::PolStarStatus, starpol, filter, starx=starx, stary=stary, name=name
  
  self->Message,'Starting',priority='DEBUG',method='POLSTARSTATUS'
  
  filterref = self->FilterConvert(filter)
  khwp = where(starpol.filter eq filterref)
  if khwp[0] eq -1 then begin
    self->Message,'This star does not have the right filter ('+strtrim(filter,2)+')',priority='WARN',method='POLSTARSTATUS'
    return,{starpol:starpol, distance:9999., status:-1, filtref:filterref}
  endif
  self->Message,'FILTER '+strtrim(filterref,2)+' was checked',priority='DEBUG',method='POLSTARSTATUS'
  
  newstar = starpol
  
  ; Check if the name of the new star matches the one in starpol
  if keyword_set(name) then begin
    self->Message,'Checking name '+name,priority='DEBUG',method='POLSTARSTATUS'
    if newstar.name eq '' then begin
      newstar.name = name 
    endif else begin
      if strlowcase(newstar.name) ne strlowcase(name) then begin
        self->Message,'This star does not belong to the group',priority='DEBUG',method='POLSTARSTATUS'
        return,{starpol:starpol, distance:9999., status:1, filtref:filterref}
      endif
    endelse
  endif else begin
    self->Message,'No name was input',priority='DEBUG',method='POLSTARSTATUS'
  endelse
  
  ; Check if it is within the expected position in the detector
  stardiff = 0.
  if starpol.starx eq -1 or starpol.stary eq -1 then begin
    self->Message,'Star did not exist in starpol',priority='DEBUG',method='POLSTARSTATUS'
    if keyword_set(starx) then begin
      self->Message,'Adding X star position '+starx[0],priority='DEBUG',method='POLSTARSTATUS'
      newstar.starx = starx[0]
    endif
    if keyword_set(stary) then begin
      self->Message,'Adding Y star position '+stary[0],priority='DEBUG',method='POLSTARSTATUS'
      newstar.stary = stary[0]
    endif
  endif else begin
    self->Message,'Star already existed in starpol',priority='DEBUG',method='POLSTARSTATUS'
    stardiff = sqrt((starx[0] - newstar.starx)^2 + (stary[0] - newstar.stary)^2)
    if stardiff gt self.deltapos then begin
      self->Message,'This star does not belong to the group',priority='DEBUG',method='POLSTARSTATUS'
      return,{starpol:starpol, distance:9999., status:1, filtref:filterref}
    endif
  endelse
  
  newstar.Nstars[khwp[0]] = newstar.Nstars[khwp[0]] + 1
  
  return,{starpol:newstar, distance:stardiff,status:0, filtref:filterref}
  
END

;****************************************************************************
;     POLSTAR - Returns the star after completing the values q or u in the polarimetry
;               Status indicates how this new star relates to the star polarization object
;                   STATUS:
;                       * -1: Not valid
;                       * 0: Added to starpol
;                       * 1: Did not belong to starpol
;****************************************************************************
FUNCTION runserkowski::PolStar, starpol, filter, p, dp=dp, fname=fname, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='POLSTAR'
  
  starstat = self->PolStarStatus(starpol,filter,_EXTRA=extraKeywords)
  if starstat.status ne 0 then begin
    self->Message,'Error returned by PolStarStatus',priority='WARN',method='POLSTAR'
    return,starstat
  endif
  
  newstar =starstat.starpol
    
  if newstar.p eq ptr_new() then begin
    self->Message,'No previous star',priority='DEBUG',method='POLSTAR'
    newstar.p = ptr_new([p[0]])
    newstar.infilter = ptr_new([starstat.filtref])
    if n_elements(dp) gt 0 then newstar.dp = ptr_new([dp[0]]) else newstar.dp = ptr_new([0.])
    if n_elements(xi) gt 0 then newstar.xi = ptr_new([xi[0]]) else newstar.xi = ptr_new([0.])
    if n_elements(dxi) gt 0 then newstar.dxi = ptr_new([dxi[0]]) else newstar.dxi = ptr_new([0.])
    newstar.bad = ptr_new([0])
    
    if keyword_set(fname) then begin
      if newstar.fnames eq ptr_new() then newstar.fnames = ptr_new([fname]) 
      if newstar.fname0 eq '' then newstar.fname0 = fname 
    endif else begin
      if newstar.fnames eq ptr_new() then newstar.fnames = ptr_new(['default']) 
      if newstar.fname0 eq '' then newstar.fname0 = 'default' 
    endelse
  endif else begin
    self->Message,'Previous star',priority='DEBUG',method='POLSTAR'
    *newstar.p = [*newstar.p,p[0]]
    *newstar.infilter = [*newstar.infilter, starstat.filtref]
    if n_elements(dp) gt 0 then *newstar.dp = [*newstar.dp,dp[0]] else *newstar.dp = [*newstar.dp,0.]
    if n_elements(xi) gt 0 then *newstar.xi = [*newstar.xi,xi[0]] else *newstar.xi = [*newstar.xi,0.]
    if n_elements(dxi) gt 0 then *newstar.dxi = [*newstar.dxi,dxi[0]] else *newstar.dxi = [*newstar.dxi,0.]
    *newstar.bad = [*newstar.bad,0]
    
    if keyword_set(fname) then begin
      *newstar.fnames = [*newstar.fnames,fname]
    endif else begin
      *newstar.fnames = [*newstar.fnames,'default']
    endelse
  endelse
    
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='POLSTAR'
  return, {starpol:newstar, distance:starstat.distance, status:0}
  
END

;****************************************************************************
;     POLSTAR - Returns the star after completing the values q or u in the polarimetry
;               Status indicates how this new star relates to the star polarization object
;                   STATUS:
;                       * -1: Not valid
;                       * 0: Added to starpol
;                       * 1: Did not belong to starpol
;****************************************************************************
FUNCTION runserkowski::PolStar_backup_justincase_oldstarstructure, starpol, filter, p, dp=dp, starx=starx, stary=stary, fname=fname, name=name
  
  self->Message,'Starting',priority='DEBUG',method='POLSTAR'
  
  filterref = self->FilterConvert(filter)
  khwp = where(starpol.filter eq filterref)
  if khwp[0] eq -1 then begin
    self->Message,'This star does not have the right filter ('+strtrim(filter,2)+')',priority='WARN',method='POLSTAR'
    return,{starpol:starpol, distance:9999., status:-1}
  endif
  self->Message,'FILTER '+strtrim(filterref,2)+' was checked',priority='DEBUG',method='POLSTAR'
  
  newstar = starpol
  
  ; Check if the name of the new star matches the one in starpol
  if keyword_set(name) then begin
    self->Message,'Checking name '+name,priority='DEBUG',method='POLSTAR'
    if newstar.name eq '' then begin
      newstar.name = name 
    endif else begin
      if strlowcase(newstar.name) ne strlowcase(name) then begin
        self->Message,'This star does not belong to the group',priority='DEBUG',method='POLSTAR'
        return,{starpol:starpol, distance:9999., status:1}
      endif
    endelse
  endif else begin
    self->Message,'No name was input',priority='DEBUG',method='POLSTAR'
  endelse
  
  ; Check if it is within the expected position in the detector
  stardiff = 0.
  if starpol.starx eq -1 or starpol.stary eq -1 then begin
    self->Message,'Star did not exist in starpol',priority='DEBUG',method='POLSTAR'
    if keyword_set(starx) then begin
      self->Message,'Adding X star position '+starx[0],priority='DEBUG',method='POLSTAR'
      newstar.starx = starx[0]
    endif
    if keyword_set(stary) then begin
      self->Message,'Adding Y star position '+stary[0],priority='DEBUG',method='POLSTAR'
      newstar.stary = stary[0]
    endif
  endif else begin
    self->Message,'Star already existed in starpol',priority='DEBUG',method='POLSTAR'
    stardiff = sqrt((starx[0] - newstar.starx)^2 + (stary[0] - newstar.stary)^2)
    if stardiff gt self.deltapos then begin
      self->Message,'This star does not belong to the group',priority='DEBUG',method='POLSTAR'
      return,{starpol:starpol, distance:9999., status:1}
    endif
  endelse
  
  if (newstar.Nstars)[khwp[0]] eq 0 then begin
    self->Message,'No previous star for FILTER '+strtrim(filterref,2),priority='DEBUG',method='POLSTAR'
    newstar.p[khwp[0]] = p[0]
    newstar.Nstars[khwp[0]] = 1
    if n_elements(dp) gt 0 then begin
      self->Message,'Delta P was set to '+strtrim(dp,2),priority='DEBUG',method='POLSTAR'
      newstar.dp[khwp[0]] = dp[0]
    endif else begin
      self->Message,'Delta P was not set',priority='DEBUG',method='POLSTAR'
      newstar.dp[khwp[0]] = 0.
    endelse
  endif else begin
    self->Message,strtrim((newstar.Nstars)[khwp[0]],2)+' previous star for Filter '+strtrim(filterref,2),priority='DEBUG',method='POLSTAR'
    newstar.p[khwp[0]] = (newstar.p[khwp[0]]*newstar.Nstars[khwp[0]] + p[0])/(newstar.Nstars[khwp[0]]+1)
    newstar.Nstars[khwp[0]] = newstar.Nstars[khwp[0]] + 1
    if n_elements(dp) gt 0 then begin
      self->Message,'Delta P was set to '+strtrim(dp,2),priority='DEBUG',method='POLSTAR'
      newstar.dp[khwp[0]] = sqrt((newstar.dp[khwp[0]])^2*newstar.Nstars[khwp[0]] + (dp[0])^2)/(newstar.Nstars[khwp[0]]+1)
    endif else begin
      self->Message,'Delta P was not set',priority='DEBUG',method='POLSTAR'
      ;newstar.dp[khwp[0]] = 0.
    endelse
  endelse
  
  if keyword_set(fname) then begin
    if newstar.fnames eq ptr_new() then newstar.fnames = ptr_new([fname]) else *newstar.fnames = [*newstar.fnames,fname]
    if newstar.fname0 eq '' then newstar.fname0 = fname 
  endif
  
  self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='POLSTAR'
  return, {starpol:newstar, distance:stardiff, status:0}
  
END

;****************************************************************************
;     DEFAULTSTAR - Return a default structure storing the data of the 
;                        peaks for a specific star
;****************************************************************************
; We need to implement the update of xi and dxi here and in Polstar as well.
FUNCTION runserkowski::DefaultStar, filter=filter, p=p, dp=dp, xi=xi, dxi=dxi, starx=starx, stary=stary, _EXTRA=extraProperties
  
  defaultprop = {name:'', $
                 fnames:ptr_new(), $
                 fname0:'', $
                 starx:-1., $
		 stary:-1., $
                 filter:[0.36,0.44,0.55,0.64,0.79], $
                 filtername:['U','B','V','R','I'], $
                 Nstars: [0,0,0,0,0], $
                 bad:ptr_new(), $
                 infilter:ptr_new(), $
                 P:ptr_new(), $
                 dP:ptr_new(), $
                 Xi:ptr_new(), $
                 dXi:ptr_new(), $
                 fitstatus: 0, $
                 fitmsg: '', $   
                 Pmax: 0., $              
                 dPmax: 0., $   
                 Wmax: 0., $
                 dWmax: 0., $
		 valid:0}
  
  if n_elements(filter) gt 0 then begin
    self->Message,'FILTER was set to '+strtrim(filter,2),priority='DEBUG',method='DEFAULTSTAR'
    if n_elements(p) gt 0 then begin
      self->Message,'P was set to '+strtrim(p,2),priority='DEBUG',method='DEFAULTSTAR'
      if n_elements(dp) gt 0 then begin
        self->Message,'Delta P was set to '+strtrim(dp,2),priority='DEBUG',method='DEFAULTSTAR'
        help,extraProperties,/struct
        respol = self->PolStar(defaultprop,filter,p, dp=dp, _EXTRA=extraProperties)
      endif else begin
        self->Message,'Delta P was not set',priority='DEBUG',method='DEFAULTSTAR'
        respol = self->PolStar(defaultprop,filter,p, _EXTRA=extraProperties)
      endelse
      ; We assume that it always exited with status = 0 which I think it is always
      ; true considering that we just created the default structure
      defaultprop = respol.starpol
    endif 
  endif
  
  ;if keyword_set(fname) then begin
  ;  if defaultprop.fnames eq ptr_new() then defaultprop.fnames = ptr_new([fname]) $
  ;  else *defaultprop.fnames = [*defaultprop.fnames ,fname]
  ;  if defaultprop.fname0 eq '' then defaultprop.fname0 = fname
  ;endif
  
  resprop = fillstructure(defaultprop,_EXTRA=extraProperties)
  
  if keyword_set(starx) then resprop.starx = starx[0]
  if keyword_set(stary) then resprop.stary = stary[0]
  
  return, resprop

END

;****************************************************************************
;     ADDSTAR - Given a position and the photometry of a star for ordinary
;               and extraordinary, it checks if the star matches something
;               existing and add or complete the information
;****************************************************************************
PRO runserkowski::AddStar, starx, stary, filter, p, _EXTRA=extraKeywords
  
  self->Message,'Starting',priority='DEBUG',method='ADDSTAR'
    
  if self.starpol eq ptr_new() then begin
    self->Message,'Star polarization array is null',priority='DEBUG'
    newpol = self->DefaultStar(filter=filter,p=p,starx=starx,stary=stary, _EXTRA=extraKeywords)
    self.starpol = ptr_new([newpol])
    return
  endif
  if n_elements(*self.starpol) eq 0 then begin
    self->Message,'Star polarization array is empty',priority='DEBUG'
    newpol = self->DefaultStar(filter=filter,p=p,starx=starx,stary=stary, _EXTRA=extraKeywords)
    *self.starpol = [newpol]
    return
  endif
  
  ; At this stage we have a new star to incorporate to the array *self.starpol which is not empty
  distance = 9990.
  idx = -1
  for i=0,n_elements(*self.starpol)-1 do begin
    starpol = (*self.starpol)[i]
    respol = self->PolStarStatus(starpol, filter, starx=starx, stary=stary, _EXTRA=extraKeywords)
    if respol.status eq 0 then begin
      if respol.distance lt distance then begin
        distance = respol.distance
        idx = i
      endif
    endif
  endfor
    
  ; Either we found a starpol that matched and we added to it (if idx ne -1)
  ; or we have to create a new one and add it to the list
  if idx ne -1 then begin
    respol = self->PolStar((*self.starpol)[idx], filter, p, starx=starx, stary=stary, _EXTRA=extraKeywords)
    (*self.starpol)[idx] = respol.starpol
  endif else begin
    *self.starpol = [*self.starpol,self->DefaultStar(filter=filter, p=p, starx=starx, stary=stary, _EXTRA=extraProperties)]
  endelse
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='ADDSTAR'
  
END

;****************************************************************************
;     ADDSTARLIST - Pair up stars of the input arrays and call Addstar to
;                   add polarization to the star pol array
;****************************************************************************
PRO runserkowski::AddStarList, starx, stary, parr, filter, eparr=eparr, xiarr=xiarr, exiarr=exiarr, _EXTRA=extraKeywords
    
  self->Message,'Starting',priority='DEBUG',method='ADDSTARLIST'
  
  ; for now we are going to take only the first to elements if they exist
  if n_elements(parr) lt 1 then begin
    self->Message,'Not enough stars',priority='ERROR',method='ADDSTARLIST'
    return
  endif
    
  ; We should order stars and pair them but I am going to assume that they are ordered
  ; eventhough it is not necessary the case
  
  self->Message,['Calling self->AddStar, '+strtrim(starx[0],2)+', '+strtrim(stary[0],2)+', '+strtrim(filter,2)+', '+strtrim(parr[0],2)], priority='DEBUG',method='ADDSTARLIST'
  
  self->AddStar, starx[0], stary[0], filter, parr[0], dp=eparr[0], xi=xiarr[0], dxi=exiarr[0], _EXTRA=extraKeywords
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='ADDSTARLIST'
  
END

;****************************************************************************
;     READFITSSTAR - read the star properties of a fits file and
;                    adds a list of stars to the photlist structure list
;****************************************************************************
PRO runserkowski::readfitsstar, fname
  
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
;     CALCFITPOL - returns the Serkowski polarimetry of the input star using fit
;****************************************************************************
FUNCTION runserkowski::CalcFitPol, starpol
  
  self->Message,'Starting',priority='DEBUG',method='CALCFITPOL'
  
  defaultres = {Pmax:-1.,dPmax:0.,Wmax:0.,dWmax:0., status:0., msg:''}
  
  if starpol.P eq ptr_new() then begin
    self->Message,'Polarimetry vector in starpol is null',priority='WARN',method='CALCFITPOL'
    return,defaultres
  endif
  if n_elements(*starpol.P) eq 0 then begin
    self->Message,'Polarimetry vector in starpol is empty',priority='WARN',method='CALCFITPOL'
    return,defaultres
  endif
  
  k = where(starpol.nstars ne 0)
  
  if k[0] eq -1 then begin
    self->Message,'No observation recorded for any Filter',priority='DEBUG',method='CALCFITPOL'
    return,defaultres
  endif
  
  if n_elements(k) lt 3 then begin
    self->Message,'Need more than 2 data points in order to fit curve',priority='WARN',method='CALCFITPOL'
    return,defaultres
  endif
  
  self->Message,'Calling FitSerkowski...',priority='DEBUG',method='CALCFITPOL'
  res = FitSerkowski(*starpol.infilter,*starpol.P,dp=*starpol.dP)
  self->Message,'   ... FitSerkowski DONE',priority='DEBUG',method='CALCFITPOL'
  Areturn = res.fit
  Aerror = res.error
  
  ; We need to check the status of the result res.status 
  kstat = where([1,2,3,4] eq res.status)
  if kstat[0] eq -1 then begin
    self->Message,'Failure - fitting status: '+strtrim(res.status,2),priority='DEBUG',method='CALCFITPOL'
  endif else begin
    self->Message,'DONE SUCCESSFULLY',priority='DEBUG',method='CALCFITPOL'
  endelse
  
  Pres = {Pmax:Areturn[0],dPmax:Aerror[0],Wmax:Areturn[1],dWmax:Aerror[1], status:res.status, msg:res.msg}
  
  return,Pres
  
END

;****************************************************************************
;     POLARIMETRY - Calculate the polarimetry of all stars in the object
;****************************************************************************
PRO runserkowski::Polarimetry, fit=fit
  
  if self.starpol eq ptr_new() then begin
    self->Message,'Star polarimetry array is null',priority='DEBUG',method='POLARIMETRY'
    return
  endif
  if n_elements(*self.starpol) eq 0 then begin
    self->Message,'Star polarimetry array is empty',priority='DEBUG',method='POLARIMETRY'
    return
  endif
  
  for i=0,n_elements(*self.starpol)-1 do begin
    if keyword_set(fit) then respol = self->CalcFitPol((*self.starpol)[i]) else respol = self->CalcPatatPol((*self.starpol)[i])
    (*self.starpol)[i].fitstatus = respol.status
    (*self.starpol)[i].fitmsg = respol.msg
    (*self.starpol)[i].Pmax = respol.Pmax
    (*self.starpol)[i].dPmax = respol.dPmax
    (*self.starpol)[i].Wmax = respol.Wmax
    (*self.starpol)[i].dWmax = respol.dWmax
  endfor
  
END


;****************************************************************************
;     GETFPHOTOMETRY - Returns an array of file names that were used to create
;                      fname. The information is extracted from the header
;                      based on outputs from runpolarimetry class
;****************************************************************************
FUNCTION runserkowski::GetFPhotometry, fname
  
  if not file_test(fname) then begin
    self->Message,'Input file does not exist: '+fname,priority='WARN',method='GETFPHOTOMETRY'
    return,{flist:[''],nfiles:0}
  endif
  
  im = readfits(fname,h)
  
  flist=''
  nfiles=0
  i=0
    fphot=sxpar(h,'ST'+strtrim(i,2)+'FN',count=Ncount)
    opath=self->SxLongStr(h,'ST'+strtrim(i,2)+'PTO')
    epath=self->SxLongStr(h,'ST'+strtrim(i,2)+'PTE')
  while (Ncount gt 0) do begin
    if file_test(opath+'/'+fphot) then begin
      if flist[0] eq '' then flist=[opath+'/'+fphot] else flist=[flist,opath+'/'+fphot]
      nfiles = nfiles + 1
    endif
    if file_test(epath+'/'+fphot) then begin
      if flist[0] eq '' then flist=[epath+'/'+fphot] else flist=[flist,epath+'/'+fphot]
      nfiles = nfiles + 1
    endif
    
    i = i + 1
    fphot=sxpar(h,'ST'+strtrim(i,2)+'FN',count=Ncount)
    opath=self->SxLongStr(h,'ST'+strtrim(i,2)+'PTO')
    epath=self->SxLongStr(h,'ST'+strtrim(i,2)+'PTE')
  endwhile
  
  return,{flist:flist,nfiles:nfiles}
  
END

;****************************************************************************
;     GETCONFIGURATION - Returns an array of structures with the attributes
;                        that configurable
;****************************************************************************
FUNCTION runserkowski::GetConfiguration
  
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
PRO runserkowski::run, _EXTRA=extraKeywords
  
  self->Message,['============ SERKOWSKI =============','Starting'],priority='DEBUG',method='RUN'
  
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
  
  ; Clean star property array
  if self.starpol ne ptr_new() then begin
    ptr_free,self.starpol
    self.starpol = ptr_new()
  endif
  
  ; for each image we will loop in the stars inside the header
  for i=0,Ndata-1 do begin
    self->Message,['>>>>>>>>','Calculating polarimetry for file: '+(*self.indata)[i]],priority='INFO',method='RUN'
    imin = readfits((*self.indata)[i],hin,/silent)
    
    filter = sxpar(hin, 'FILTER')
    currentstar = 0
    starx_read = sxpar(hin,'ST'+strtrim(currentstar,2)+'X')
        
    objname = strtrim(sxpar(hin, 'OBJECT'),2)
    
    while (starx_read ne 0.) do begin
      self->Message,'     - Adding star '+strtrim(currentstar,2),priority='DEBUG',method='RUN'
      
      ; Polarimetry calculated from the star
      newstarp = sxpar(hin,'POLLEV'+strtrim(currentstar,2))
      newstarep = sxpar(hin,'ePOLLEV'+strtrim(currentstar,2))
      newstarxi = sxpar(hin,'POLANG'+strtrim(currentstar,2))
      newstarexi = sxpar(hin,'ePOLANG'+strtrim(currentstar,2))
      
      ; Standard star polarimetry 
      newstdp = sxpar(hin,'UCPLEV'+strtrim(currentstar,2),count=Nhread)
      newstdep = sxpar(hin,'UCEPLEV'+strtrim(currentstar,2))
      newstdxi = sxpar(hin,'UCPANG'+strtrim(currentstar,2))
      newstdexi = sxpar(hin,'UCEPANG'+strtrim(currentstar,2))
      
      ; Correct instrumental polarization
      if Nhread eq 0 then begin
        q = newstarp*cos(2.*newstarxi/180.*!pi)
        u = newstarp*sin(2.*newstarxi/180.*!pi)
        qstd = newstdp * cos(2.*newstdxi/180*!pi)
        ustd = newstdp * sin(2.*newstdxi/180*!pi)
        qcorr = q - qstd
        ucorr = u - ustd
        newstarp = sqrt(qcorr^2 + ucorr^2)
        newstarxi = 0.5 * atan(ucorr/qcorr)
      endif
      
      if currentstar eq 0 then begin
        starx = sxpar(hin,'ST'+strtrim(currentstar,2)+'X')
        stary = sxpar(hin,'ST'+strtrim(currentstar,2)+'Y')
        
        ; Try to read first the polarization corrected values and if they do not exist, then the one derived from polarimetry
        starp = newstarp
        starep = newstarep
        starxi = newstarxi
        starexi = newstarexi
      endif else begin
        starx = [starx,sxpar(hin,'ST'+strtrim(currentstar,2)+'X')]
        stary = [stary,sxpar(hin,'ST'+strtrim(currentstar,2)+'Y')]
                
        starp = [starp,newstarp]
        starep = [starep,newstarep]
        starxi = [starxi,newstarxi]
        starexi = [starexi,newstarexi]
      endelse
      currentstar = currentstar + 1
      starx_read = sxpar(hin,'STAR'+strtrim(currentstar,2)+'X')
    endwhile
    
    self->Message,'    Adding star to list: '+(*self.indata)[i],priority='INFO',method='RUN'
    self->AddStarList, starx, stary, starp, filter,eparr=starep, xiarr=starxi, exiarr=starexi, name=objname,fname=(*self.indata)[i]
    self->Message,'DONE with polarimetry for file: '+(*self.indata)[i],priority='INFO',method='RUN'
  endfor
  
  if self.starpol eq ptr_new() then begin
    self->Message,'Star polarization list is null',priority='WARN',method='RUN'
    return
  endif
  
  if n_elements(*self.starpol) eq 0 then begin
    self->Message,'Star polarization list is empty',priority='WARN',method='RUN'
    return
  endif
  
  ; Calculate polarimetry for each starpol
  self->Polarimetry,/fit
  
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
  for i=0,n_elements(*self.starpol)-1 do begin
    starpol = (*self.starpol)[i]
    refname = starpol.fname0
    if refname eq '' then refname = (*self.indata)[i]
    
    if file_test(refname) eq 1 then begin
      im = readfits(refname,hin)
      hout=hin
    endif else begin
      im = [[0.,0.],[0.,0.]]
      mkhdr,hout,0
    endelse
    
    ; Add polarization values to the header and images
    sxaddpar,hout,'Pmax'+strtrim(i,2),starpol.Pmax
    sxaddpar,hout,'ePmax'+strtrim(i,2),starpol.dPmax
    sxaddpar,hout,'Wmax'+strtrim(i,2),starpol.Wmax
    sxaddpar,hout,'eWmax'+strtrim(i,2),starpol.dWmax
    sxaddpar,hout, 'OBJCLASS',OBJ_CLASS(self)
    
    ; Create file name for the current starpol
    outfname = self->createFname(starpol.fname0,name=starpol.name, _EXTRA=extraKeywords)
    if self.outdata eq ptr_new() then self.outdata = ptr_new([outfname]) $
    else *self.outdata = [*self.outdata,outfname]
    
    ; Write fits file with images/plane and header
    self->Message,'Writing file '+outfname,priority='DEBUG',method='RUN'
    writefits,outfname,im,hout
    
    ; Make a plot with the fit
    self->updatePlot, 0, badidx=(*self.starpol)[0].bad, ps=outfname+'.ps'
  endfor
  
  self->Message,'DONE SUCCESFULLY',priority='DEBUG',method='RUN'
  
END

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO runserkowski::plot_old, winnum
  
  if self.starpol eq ptr_new() then begin
    self->Message,'starpol array is null',priority='DEBUG',method='PLOT'
    return
  endif
  if n_elements(*self.starpol) eq 0 then begin
    self->Message,'starpol array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  starpol = (*self.starpol)[0]
  if starpol.P eq ptr_new() then begin
    self->Message,'polarimetry array in starpol is null',priority='DEBUG',method='PLOT'
    return
  endif
  if n_elements(*starpol.P) eq 0 then begin
    self->Message,'polarimetry array in starpol is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  if not keyword_set(winnum) then winnum = 0
  DEVICE, WINDOW_STATE=winstat
  if winstat[winnum] eq 0 then window,winnum,retain=2 else wset,winnum
  
  mkct,0
  plotnum = 0
  for i=0,n_elements(*self.starpol)-1 do begin
    starpol = (*self.starpol)[i]
    
    ; absice for overploting cos fit
    minfilter = min(starpol.filter)
    maxfilter = max(starpol.filter)
    absice = findgen(1000)/1000.*(maxfilter-minfilter)+minfilter
    
    k = where(starpol.Nstars ne 0)
    if k[0] ne -1 then begin
      if plotnum eq 0 then begin
        plot,*starpol.infilter,*starpol.p,psym=2
        errplot,*starpol.infilter,*starpol.p-*starpol.dp,*starpol.p+*starpol.dp
        
        ; Overplot the fit serkowski
        oplot,absice,serkowski(absice,[starpol.Pmax,starpol.Wmax]),linestyle=2
      endif else begin
        oplot,*starpol.filter,*starpol.p,psym=2,color=2+plotnum-1
        errplot,*starpol.filter,*starpol.p-*starpol.dp,*starpol.p+*starpol.dp,color=2+plotnum-1
        
        ; Overplot the fit P*cos(4*hwp-Xi)
        oplot,absice,serkowski(absice,[starpol.Pmax,starpol.Wmax]),linestyle=2,color=2+plotnum-1
      endelse
      
      
    endif
  endfor
  
END

;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO runserkowski::cleanup
  
  self->runstep::cleanup
  
  if self.starpol ne ptr_new() then ptr_free,self.starpol
  
END
;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION runserkowski::init, Deltapos=Deltapos, _Extra=extraKeyword
  
  if self->runstep::init(_Extra=extraKeyword) eq 0 then begin
    self->Message,'Init failed for parent class (runstep)',priority='ERR',method='INIT'
    return,0
  endif
  
  if not keyword_set(deltapos) then deltapos = 200.
  if deltapos lt 0. then deltapos = 200.
  self.deltapos = deltapos
  
  self.prevtype = 'multiple'
  
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
PRO runserkowski::updatePlot, winnum, specialidx=specialidx, badidx=badidx, selregion=selregion, zoom=zoom, ps=ps
  
  self->Message,'Starting',priority='DEBUG',method='UPDATEPLOT'
  if n_elements(winnum) eq 0 then begin
    self->Message,'No winnum',priority='DEBUG',method='UPDATEPLOT'
    return
  endif
  
  if self.starpol eq ptr_new() then begin
    self->Message,'starpol array is null',priority='DEBUG',method='PLOT'
    return
  endif
  
  starpol = *self.starpol
  
  if starpol.P eq ptr_new() then begin
    self->Message,'polarimetry array in starpol is null',priority='DEBUG',method='PLOT'
    return
  endif
  if n_elements(*starpol.P) eq 0 then begin
    self->Message,'polarimetry array in starpol is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  if n_elements(starpol) eq 0 then begin
    self->Message,'starpol array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  if keyword_set(ps) then begin
    if strtrim(ps,2) eq '1' or not file_test(file_dirname(strtrim(ps,2)),/directory) then begin
      sFile = DIALOG_PICKFILE(PATH=file_dirname(starpol.fname0), $
                              TITLE='Select PS File to save the plot',  FILTER='*.ps')
    endif else begin
      sFile = ps
    endelse
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
          maxhwp = 1.
          minP=min(*starpol.P,max=maxP)
          minP = max([0.,minP*scaleP])
          maxP = maxP*scaleP        
        endif
      end
    1: begin  ; unzoom to initial values
        minhwp = 0.
        maxhwp = 1.
        minP=min(*starpol.P,max=maxP)
        minP = max([0.,minP*scaleP])
        maxP = maxP*scaleP
      end
    4: begin  ; zoom to specified values [x1,y1,x2,y2]
        minhwp=min([zoom[0],zoom[2]],max=maxhwp)
        minP=min([zoom[1],zoom[3]],max=maxP)
      end
    else: begin
          self->Message,'Wrong zoom value',priority='WARN', method='UPDATEPLOT'
          if keyword_set(ps) then begin
            DEVICE, /CLOSE
            SET_PLOT, thisDevice
          endif
          return
        end
  ENDCASE
  
  ; absice for overploting cos fit
  absicefilt = findgen(1000)/1000.*(maxhwp-minhwp)+minhwp
  
  plot,absicefilt,scaleP*serkowski(absicefilt,[starpol.Pmax,starpol.Wmax]),linestyle=2,xtitle='Filter (microns)', ytitle='P (%)',xrange=[minhwp,maxhwp],yrange=[minP,maxP]
  
  oplot,*starpol.infilter,*starpol.P*scaleP, psym=2
  errplot,*starpol.infilter,*starpol.P*scaleP-*starpol.dP*scaleP,*starpol.P*scaleP+*starpol.dP*scaleP
  
  if n_elements(specialidx) gt 0 then begin
    oplot,[(*starpol.infilter)[specialidx]],[(*starpol.P)[specialidx]*scaleP], psym=2, color=2
    errplot,[(*starpol.infilter)[specialidx]],[scaleP*(*starpol.P)[specialidx]]-scaleP*(*starpol.dP)[specialidx],[scaleP*(*starpol.P)[specialidx]]+scaleP*(*starpol.dP)[specialidx], color=2
  endif 
    
  if n_elements(badidx) eq n_elements(*starpol.infilter) then begin
    colornonsel=3
    colorsel=4
    for i=0,n_elements(badidx)-1 do begin
      curcolor=colornonsel
      if badidx[i] eq 1 then begin
        if n_elements(specialidx) gt 0 then if (where(specialidx eq i))[0] ne -1 then curcolor = colorsel
        oplot,[(*starpol.infilter)[i]],[(*starpol.P)[i]*scaleP], psym=2, color=curcolor
        errplot,[(*starpol.infilter)[i]],[scaleP*(*starpol.P)[i]]-scaleP*(*starpol.dP)[i],[scaleP*(*starpol.P)[i]]+scaleP*(*starpol.dP)[i], color=colornonsel
    endif
    endfor
  endif
  
  if keyword_set(selregion) then plots, selregion,color=2, /DEVICE
  
  ; Write the information of the object and serkowski fit
  x0=minhwp+(maxhwp-minhwp)/30.
  dy=(maxP-minP)/30.
  y0=maxP-2.*dy
  xystr=['Object: '+starpol.name, $
         'Pmax: '+STRING(scaleP*starpol.Pmax,FORMAT='(F5.2)')+'% +/- '+STRING(scaleP*starpol.dPmax,FORMAT='(F5.2)'), $
         'Wmax: '+STRING(starpol.Wmax, FORMAT='(F5.2)')+'um +/- '+STRING(starpol.dWmax,FORMAT='(F5.2)')]
  nxystr = n_elements(xystr)
  xyouts,replicate(x0,nxystr),y0-indgen(nxystr)*dy,xystr, charsize=1
  
  if keyword_set(ps) then begin
    DEVICE, /CLOSE
    SET_PLOT, thisDevice
  endif
  
  self->Message,'DONE',priority='DEBUG',method='UPDATEPLOT'
  
END

;****************************************************************************
;     PLOTDRAWAREA - Method when the event comes from the drawing area
;****************************************************************************
PRO runserkowski::plotdrawarea, event, parameters
  ;self->Message,'Called Plot_event with drawarea',priority='DEBUG',method='PLOT_EVENT'
  
  ; exit if a key was previously pressed
  if parameters.prevkey eq 1 then begin
    self->Message,'Previous key task pending',priority='DEBUG',method='PLOTDRAWAREA'
    return
  endif
  
  starpol = *self.starpol
  badidx=starpol.bad
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
        ; Find which is the closest value in starpol
        d=sqrt((*starpol.infilter-data1[0])^2 + (*starpol.P*100.-data1[1])^2)
        dmin = min(d,fidx)
        
        if dmin lt 10 then begin
          farr = [(*starpol.fnames)[fidx]]
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
        khwp = where(*starpol.infilter ge hwpmin and *starpol.infilter le hwpmax)
        if khwp[0] eq -1 then begin
          self->updatePlot, winnum, badidx=badidx
          return
        endif
        kp = where((*starpol.p)[khwp]*100. ge pmin and (*starpol.p)[khwp]*100. le pmax )
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
  countfidx=0
  farr = [(*starpol.fnames)[fidx[countfidx]]]
  for countfidx=1,n_elements(fidx)-1 do begin
    farr = [farr,(*starpol.fnames)[fidx[countfidx]]]
  endfor
  
  ; Action according to the requested key
  CASE event.ch of
    ; if we pressed the d key we show the image with atvcal
    100: begin
          if (xregistered('atv', /noshow)) then begin
            common atv_state, state
            atv_shutdown
          endif
          
          fdisp = ''
          for ifarr = 0, n_elements(farr)-1 do begin
            flist = self->GetFPhotometry(farr[ifarr])
            if flist.nfiles gt 0 then begin
              redophot = dialog_message(['Do you want to redo the photometry of these files?','',flist.flist], /QUESTION)
              if redophot eq 'Yes' then if fdisp eq '' then fdisp = flist.flist else fdisp = [fdisp,flist.flist]
            endif else begin
              self->Message,'Cound not find the photometry files from '+farr[ifarr],priority='WARN',method='PLOTDRAWAREA'
            endelse
          endfor
          
          if fdisp[0] eq '' then begin
            self->Message,'No file was selected or existed to be displayed',priority='INFO',method='PLOTDRAWAREA'
          endif else begin
            atvcal,fdisp,/profile
          endelse
        end
    ; if we pressed i we get information about the selected objects
    105: begin
          msg='List of files [polarimetry/photometry]:'
          for ifarr = 0, n_elements(farr)-1 do begin
            msg=[msg,farr[ifarr]]
            flist = self->GetFPhotometry(farr[ifarr])
            if flist.nfiles gt 0 then begin
              msg=[msg,'     '+flist.flist]
            endif else begin
              self->Message,'Cound not find the photometry files from '+farr[ifarr],priority='WARN',method='PLOTDRAWAREA'
              msg=[msg,'     Not found']
            endelse
          endfor
        junk = dialog_message(msg)
      end
    ;if we press the p key we redo the polarization/photometry of these target for o and e rays
    112: begin          
          if (xregistered('atv', /noshow)) then begin
            common atv_state, state
            atv_shutdown
          endif
          
          dopolarimetry,farr, /interactive, /overwrite, /product
          
          if keyword_set(redoonlyphotometry) then begin
            for ifarr = 0, n_elements(farr)-1 do begin
              flist = self->GetFPhotometry(farr[ifarr])
              if flist.nfiles gt 0 then begin
                redophot = dialog_message(['Do you want to redo the photometry of these files?','',flist.flist], /QUESTION)
                if redophot eq 'Yes' then dophotometry,flist.flist, /interactive, profile='MOFFAT', /overwrite
              endif else begin
                self->Message,'Cound not find the photometry files from '+farr[ifarr],priority='WARN',method='PLOTDRAWAREA'
              endelse
            endfor
          endif 
          
          ; This updates the polarization list of the object
          junk = dialog_message(['WARNING:','Updating the photometry did not updated the information of the serkowski object','This is not implemented yet'])
          ; for ifarr = 0, n_elements(farr)-1 do self->readfits,farr[ifarr]
          ; self->Polarimetry,/fit
       end
    ; if we pressed r, we restore the data that was flagged as r in the selected points
    114: begin
        ; restore data
        for countfidx=0,n_elements(fidx)-1 do begin
          starpol[fidx[countfidx]].bad = 0
        endfor
        self->updatePlot, winnum, specialidx=fidx, badidx=starpol.bad
        
        ; Update the polarimetry list in the current object
        *self.starpol = starpol
        
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
          (*starpol.bad)[fidx[countfidx]] = 1
        endfor
        self->updatePlot, winnum, specialidx=fidx, badidx=starpol.bad
        
        ; Update the photometry list in the current object
        *self.starpol = starpol
        
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
PRO runserkowski::plot_event, event, parameters
  
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
            self->updatePlot, 0, badidx=(*self.starpol)[0].bad, /ps
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
PRO runserkowski::plot, winnum

  if self.starpol eq ptr_new() then begin
    self->Message,'starpol array is null',priority='DEBUG',method='PLOT'
    return
  endif
  
  starpol = *self.starpol
  
  if n_elements(starpol) eq 0 then begin
    self->Message,'starpol array is empty',priority='DEBUG',method='PLOT'
    return
  endif
  
  if n_elements(winnum) eq 0 then begin
    ; base widget
    top=widget_base(title='Polarization Measurements for '+(starpol.name)[0],  /column, /TLB_SIZE_EVENTS, $
                    uvalue={method:'plot_event', object:self, parameters:{wname:'top',drawpan:0L}}, event_pro='class_eventhand') ;event_pro='top_event',
    
    ; Info area
    infobase = widget_base(top,/column)
      info = widget_label(infobase, value='Fit status: '+strjoin(strtrim(starpol.fitstatus,2),','))
      info = widget_label(infobase, value='Fit message: ['+strjoin(starpol.fitmsg,',')+']')
    
    ; Plotting area
    plotpanel = widget_draw(top,xsize=500., ysize=500., retain=2, $
                               /button_events, /MOTION_EVENTS, KEYBOARD_EVENTS=1 , $
                               uvalue={method:'plot_event', object:self, parameters:{wname:'drawarea',prevkey:0,x:-1.,y:-1.,xmove:-1,ymove:-1,selidx:replicate(-1,n_elements(starpol.filter))}}, event_pro='class_eventhand')
    widget_control, plotpanel, get_value=winnum
    wset,winnum
    widget_control,top,set_uvalue={method:'plot_event', object:self, parameters:{wname:'top',drawpan:plotpanel}}
    
    ; Button panel
    butpan = widget_base(top,/row)
      butok = widget_button(butpan, value='Continue', uvalue={method:'plot_event', object:self, parameters:{wname:'continue'}}, event_pro='class_eventhand')
      butps = widget_button(butpan, value='Save', uvalue={method:'plot_event',object:self,parameters:{wname:'save'}}, event_pro='class_eventhand')
    ; realize and start widgets
    widget_control, top, /realize
  endif else begin
    DEVICE, WINDOW_STATE=winstat
    if winstat[winnum] eq 0 then window,winnum,retain=2 else wset,winnum
  endelse
  
  self->updatePlot, winnum, badidx=starpol.bad
  
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
;     RUNSERKOWSKI__DEFINE - Define the class structure for the class runstep
;****************************************************************************

PRO runserkowski__define
  
  struct={runserkowski, $
	  starpol:ptr_new(), $      ; Array with the polarimetry of stars
          deltapos:200., $           ; Number of pixels considered as the possible variations between images of the position of the same star
          inherits runstep}
END
