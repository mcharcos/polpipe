; Return the value A0*cos(4*X-A1) [+A2]
FUNCTION cosPol, hwp, A
  
  ; A(0) = P
  ; A(1) = Xi
  
  K = 0.
  if n_elements(A) eq 3 then begin
    K=A[2] 
  endif
  
  F=A[0]*cos(4.*hwp-2.*A[1]) + K
  
  return, F
  
END
