;******************************************************************************
;     FitPolarimetry - Fit Linear Relation Y = A1x + A0. Returns A given an X and Y
;******************************************************************************
FUNCTION FitPolarimetry, hwp, qu, dqu=dqu
  
  default_res = [0.,0.,0.]
  if n_elements(dqu) eq 0 then dqu = replicate(1.,n_elements(hwp))
  
  parinfo = replicate({value:0.0d0, limited:[0,0], limits:[0.0,0]},3)
  Ainit = [0.,1.,0.]
  if n_elements(dqu) eq 0 then Aerror = [0.,0.,0.] else Aerror = [0.,max(dqu),max(hwp)-min(hwp)]
  Areturn = mpfitfun('cospol',hwp,qu,replicate(1.,n_elements(hwp)), Ainit, $
		      weight=dqu,PERROR=PERROR,DOF=auxdof,BESTNORM=auxbnorm, $
		      PARINFO=parinfo,YFIT=yfit,/QUIET,status=auxstatus,ERRMSG=auxerrmsg)

  ; Change sign of first element (=P) if negative
  if Areturn[0] lt 0. then begin
    Areturn[0] = -1.*Areturn[0]
    Areturn[1] = Areturn[1] + !pi/2.
  endif
  
  print,'MPFITFUN status is '+strtrim(auxstatus,2)
  kstat = where([1,2,3,4,5,6,7] eq auxstatus)
  if kstat[0] eq -1 then begin
    print, 'No solution found fitting the current data points'
    print,auxerrmsg
    return,{fit:Areturn,error:Aerror,status:auxstatus,errmsg:auxerrmsg}
  endif 
  
  Aerror = perror * sqrt(auxbnorm/auxdof)
    
  ; check auxstat
  sign = '+'
  if Areturn[2] lt 0. then sign = '-'
  print,'FIT: Y = ( '+strtrim(Areturn[0],2)+' +/- '+strtrim(Aerror[0],2)+' ) x cos[4.x( '+strtrim(abs(Areturn[1]),2)+' +/- '+strtrim(Aerror[1],2)+' )'+ $
                      sign+' X x ( '+strtrim(abs(Areturn[2]),2)+' +/- '+strtrim(Aerror[2],2)+' )'

  return, {fit:Areturn,error:Aerror,status:auxstatus,errmsg:auxerrmsg}
  
END  