;******************************************************************************
;     FitSerkowski - Fit a serkowski law to the input parameters
;******************************************************************************
FUNCTION FitSerkowski, wave, P, dP=dP
  
  if n_elements(dP) eq 0 then dP = replicate(1.,n_elements(wave))
  
  parinfo = replicate({value:0.0d0, limited:[0,0], limits:[0.0,0]},2)
  Ainit = [0.1,0.55]
  if n_elements(dp) eq 0 then Aerror = [0.,0.] else Aerror = [max(dp),max(wave)-min(wave)]
  Areturn = mpfitfun('serkowski',wave,P,replicate(1.,n_elements(wave)), Ainit, $
		      weight=dP,PERROR=PERROR,DOF=auxdof,BESTNORM=auxbnorm, $
		      PARINFO=parinfo,YFIT=yfit,/QUIET,status=auxstatus,ERRMSG=auxerrmsg)
    
  print,'MPFITFUN status is '+strtrim(auxstatus,2)
  kstat = where([1,2,3,4] eq auxstatus)
  if kstat[0] eq -1 then begin
    print, 'No solution found fitting the current data points'
    print,auxerrmsg
    return,{fit:Areturn,error:Aerror,status:auxstatus, msg:auxerrmsg}
  endif 
  
  Aerror = perror * sqrt(auxbnorm/auxdof)
    
  ; check auxstat
  sign = '+'
  ;if Areturn[2] lt 0. then sign = '-'
  print,'FIT: Y = ( '+strtrim(Areturn[0],2)+' +/- '+strtrim(Aerror[0],2)+' ) x exp[-K x (alog( '+strtrim(abs(Areturn[1]),2)+' +/- '+strtrim(Aerror[1],2)+' )/W)^2'
  
  return, {fit:Areturn,error:Aerror,status:auxstatus, msg:auxerrmsg }
  
END  