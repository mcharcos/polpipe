; NAME:
;     HDPOPULATE - Version 1.0
;
; PURPOSE: Populates headers of MLOF according to their file names
;     
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, April 16th 2013
PRO HDPOPULATE, path, go=go, flats=flats, darks=darks
  
  print,'-----------------------------------------'
  print,'-----------------------------------------'
  print,'---------  STARTING HDPOPULATE ----------'
  print,'-----------------------------------------'
  print,'-----------------------------------------'
  
  ; Search files in directory
  foundfiles = findfiles('*.fit*',root=path,recurse=keyword_set(recursive),count=Nfound)
  
  if Nfound eq 0 then begin
    print, 'No fits files found in directory: '+path, priority='ERR',method='ADDPATH'
    return
  endif
  
  filters = ['U','B','V','R','I']
  for i=0,Nfound-1 do begin
    print,strtrim(i+1,2)+'/'+strtrim(Nfound,2)+' Checking file '+foundfiles[i]
    fname = file_basename(foundfiles[i])
    fpath = file_dirname(foundfiles[i])
    ; Separate extension and rootname
    rootname = fname
    extension = 'fits'
    strpos = strpos(fname,'.',/reverse_search)
    if strpos ge 0 then begin
      rootname = strmid(fname,0,strpos)
      extension = strmid(fname,strpos+1,strlen(fname)-strpos)
    endif
    fcurrent = fpath+'/'+fname
     
    ; Check first if the file is in the list of flats or darks
    isflatdark = 0
    if n_elements(flats) gt 0 then begin
      ;change OBJECT keyword
      k = where(flats eq fcurrent)
      if k[0] ne -1 then begin
        im = readfits(foundfiles[i],h,/silent)
        print,fname+' is a flat file... changing OBJECT keyword'
        sxaddpar,h,'OBJECT','FLAT'
        if keyword_set(go) then begin
          print,'Updating '+fname+'...'
          writefits,foundfiles[i],im,h
        endif
        isflatdark = 1
      endif
    endif
    if n_elements(darks) gt 0 then begin
      ;change OBJECT keyword
      k = where(darks eq fcurrent)
      if k[0] ne -1 then begin
        im = readfits(foundfiles[i],h,/silent)
        print,fname+' is a dark file... changing OBJECT keyword'
        sxaddpar,h,'OBJECT','DARK'
        if keyword_set(go) then begin
          print,'Updating '+fname+'...'
          for ifilter = 0,n_elements(filters)-1 do begin
            print,'Creating copy file for '+filters[ifilter]+' filter'
            sxaddpar,h,'FILTER',filters[ifilter]
            writefits,fpath+'/'+rootname+'_'+filters[ifilter]+'.'+extension,im,h
          endfor
          sxaddpar,h,'FILTER',''
          writefits,foundfiles[i],im,h
        endif
        isflatdark = 1
      endif
    endif
    
    if isflatdark eq 0 then begin
        
      name_arr = strsplit(rootname,'_',/extract)
      if n_elements(name_arr) eq 2 or n_elements(name_arr) eq 3 or n_elements(name_arr) eq 4 or n_elements(name_arr) eq 5 then begin    
      
        im = readfits(foundfiles[i],h,/silent)
        
        obj = strupcase(strtrim(sxpar(h,'OBJECT'),2))
        
        if obj ne 'FLAT' and obj ne 'DARK' then begin
          ; Now check in the name for HWP and filter
          SWITCH n_elements(name_arr) of
            2:
            3: begin
                if n_elements(name_arr) eq 2 then begin
                  obj_arr = strsplit(name_arr[0],'-',/extract)
                  objname = obj_arr[0]
                  ffilter = strmid(name_arr[0],strlen(name_arr[0])-1,1)
                  idxhwp = 1
                endif else begin
                  objname = name_arr[0]
                  ffilter = strmid(name_arr[1],strlen(name_arr[1])-1,1)
                  idxhwp = 2
                endelse
                hwp = -1.
                CASE fix(name_arr[idxhwp]) of
                  0: hwp=0.0
                  225: hwp=22.5
                  45: hwp=45.0
                  675: hwp=67.5
                  90: hwp=90.
                  1125: hwp=112.5
                  135: hwp=135.0
                  1575: hwp=157.5
                  else: hwp = -1
                ENDCASE
                break
              end
            4:
            5: begin
               if n_elements(name_arr) eq 5 then addi = 1 else addi = 0
               ffilter = name_arr[1+addi]
               hwp = -1.
                CASE fix(name_arr[2+addi]) of
                  1: hwp=0.0
                  2: hwp=22.5
                  3: hwp=45.0
                  4: hwp=67.5
                  5: hwp=90.
                  6: hwp=112.5
                  7: hwp=135.0
                  8: hwp=157.5
                  else: hwp = -1
                ENDCASE
                objname = name_arr[0]
                if n_elements(name_arr) eq 5 then objname = objname+name_arr[1]
                break
              end
            else:
          ENDSWITCH
      
          ; First read filter and compare it to the filter from the name
          filtread = strtrim(sxpar(h,'FILTER'),2)
          if filtread ne '0' then begin
            if filtread ne ffilter then begin
              print,'Error with filter values'
              print,'     ... check file '+foundfiles[i]
            endif else begin
              print,'Writing filter '+ffilter+' in header'
              sxaddpar,h,'FILTER',ffilter
              
              ; Now we check the HWP
              print,'Writing HWP='+strtrim(hwp,2)+' in header'
              sxaddpar,h,'HWP',hwp
              
              ; Now we check the astronomical object
              print,'Writing OBJECT='+strtrim(objname,2)+' in header'
              sxaddpar,h,'OBJECT',objname
                    
              if keyword_set(go) then begin
                print,'Updating '+rootname+'...'
                writefits,foundfiles[i],im,h
              endif
            endelse
          endif
        endif else begin
          print,'Skipping '+obj+' file:'+rootname
        endelse
      endif else begin
        print,'Skipping file '+foundfiles[i]
        print,'    because does not follow naming convention OBJECT_FILTER_HWP_FILENUMBER'
      endelse
    endif
  endfor
  
  print,'-----------------------------------------'
  print,'-----------------------------------------'
  print,'---------  HDPOPULATE DONE --------------'
  print,'-----------------------------------------'
  print,'-----------------------------------------'
  
END
