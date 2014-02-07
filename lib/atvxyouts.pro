pro atvxyouts, x, y, string, alignment=alignment, charsize=charsize, $
 charthick=charthick, color=color, font=font, orientation=orientation
common atv_pdata

; Routine to overplot text

if (N_params() LT 3) then begin
   print, 'Too few parameters for ATVXYOUTS'
   return
endif

if (ptext.nplot LT 999) then begin
   iplot = ptext.nplot

   ptext.x[iplot] = x
   ptext.y[iplot] = y
   ptext.string[iplot] = ptr_new(string)
   if (keyword_set(alignment)) then ptext.alignment[iplot] = alignment $
    else ptext.alignment[iplot] = 0.0
   if (keyword_set(charsize)) then ptext.charsize[iplot] = charsize $
    else ptext.charsize[iplot] = 1.0
   if (keyword_set(charthick)) then ptext.charthick[iplot] = charthick $
    else ptext.charthick[iplot] = 1.0
   ptext.color[iplot] = atv_icolor(color)
   if (keyword_set(font)) then ptext.font[iplot] = font $
    else ptext.font[iplot] = 1
   if (keyword_set(orientation)) then ptext.orientation[iplot] = orientation $
    else ptext.orientation[iplot] = 0.0

   ptext.nplot = ptext.nplot + 1
endif else begin
   print, 'Too many calls to ATVPLOT'
endelse

atv_refresh

end

