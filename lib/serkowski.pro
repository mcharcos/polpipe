; Return the Serkowskii law
; A = [Pmax,Wmax] or A=[Pmax,Wmax,K]
FUNCTION serkowski, lambda, A
  
  ; A(0) = Pmax
  ; A(1) = lambda_max
  
  if n_elements(A) eq 3 then begin
    K=A[3] 
  endif else begin
    if A[1] gt 1000. then K=(1.66*A[1]/10000.+0.01) $
    else K=(1.66*A[1]+0.01)
  endelse
  
  P=A[0]*exp(-1.*K*(alog(A[1]/lambda))^2)
  
  return, P
  
END
