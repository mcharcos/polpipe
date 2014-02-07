; This routine was made to group data that have the same filter/dichroic configuration

PRO gmlof, inpath, outpath, recursive=recursive, objclass=objclass, filter=filter, keyword=keyword, keypath=keypath, $
                            single=single, polarimetry=polarimetry, mkstage=mkstage, interactive=interactive
  
  
  if keyword_set(interactive) then begin
    doconf=dialog_message(['Interactive mode was requested', $
                           'This may result in asking to confirm for bad files a large number of times', $
                           'Do you really want to stay interactive?'],/question)
    if doconf eq 'No' then interactive = 0
  endif
  
  ; First check the inputs and massage the input and output paths
  if not keyword_set(inpath) then inpath = './'
  path = file_expand_path(inpath)
  if strmid(path,strlen(path)-1,1) ne '/' then path = path+'/'
  if not keyword_set(dataqual) then dataqual = 'thedataqualkeywordisnotchecked'
  dataqual = strlowcase(dataqual)
  if not keyword_set(outpath) then outpath='./'
  opath = file_expand_path(outpath)
  if strmid(opath,strlen(opath)-1,1) ne '/' then opath = opath+'/'
  if not keyword_set(objclass) then objclass='raw'
  
  ; This section is for non rotated images
  section0=[[12,920,404,24], $
            [576,920,968,24]]
  ; This section is for "Rotate 90 CW"
  section90=[[103,12,999,404], $
              [103,573,999,968]]
              
  ; Now check if the output paths exist. If not, abort and tell the user.
  if file_test(path, /directory) eq 0 then begin
    print,'Input path does not exist: '+path
    print,'Aborting gmlof.pro'
    return
  endif
  if file_test(opath, /directory) eq 0 then begin
    print,'Output path does not exist: '+opath
    print,'Aborting gmlof.pro'
    return
  endif
  
  
  ; Now create directory structure under output path
  if keyword_set(mkstage) then begin
    if file_test(opath+'stage', /directory) eq 0 then begin
      file_mkdir, opath+'stage'
    endif
    stage=opath+'stage/'
  endif else begin
    stage=opath
  endelse
  
  
  ; First find all the files in the input path and copy them to
  ; the temporary directory
  if not keyword_set(keyword) then keyword=0
  if not keyword_set(keypath) then keypath=0
  flist=findproducts(path, recursive=keyword_set(recursive))
  
  foundfiles = flist.files
  Nfound = flist.count
  if Nfound eq 0 then begin
    print,'No fits files were found under input directory'
    print,'      '+path
    print,'Aborting gmlof.pro'
    return
  endif
  
  ; Copy the files to temporary directory
  Lstage=strlen(stage)
  Linpath=strlen(path)
  for i=0, Nfound-1 do begin
    fname = foundfiles[i]
    rootname = (strsplit(file_basename(fname),'.',/extract))[0]
    extension = '.fits'
    
    ; If it is not a file already in stage directory then copy it
    ; to the staging area and modify its header
    if strmid(fname,0,Lstage-1) ne stage then begin
      ; From the file name we will read the object and filter
      ; We will organize the data by filter and object name
      objfilt = fmlof(fname)
      if objfilt.error eq 1 then begin
        print,'Skipping file '+file_basename(fname)+' due to name conventions'
        print,'      at '+file_dirname(fname)
        if keyword_set(interactive) then begin
          docont=dialog_message(['Problem with file name '+fname,'Would you like to continue interactive mode?', 'Press Cancel to Abort'],/QUESTION, /CANCEL)
          if docont eq 'Cancel' then begin
            print,'Aborting per user request'
            return
          endif
          if docont eq 'No' then interactive = 0
        endif
      endif else begin
        print,strtrim(i+1,2)+'/'+strtrim(Nfound,2)+' - File '+fname
        if objfilt.object eq 'DARK' then subdirarr='' $
        else subdirarr=strlowcase(objfilt.filter)+'/'
        if keyword_set(filter) then subdirarr=subdirarr+strlowcase(objfilt.object)+'/' $
        else subdirarr=strlowcase(objfilt.object)+'/'+subdirarr
        print,'    - OBJECT: '+objfilt.object
        print,'    - FILTER: '+objfilt.filter
        print,'    - HWP: '+strtrim(objfilt.hwp,2)
        
        ; Create a subdirectory structure to maintain
        ; the current directory (if we decide to stage different nights for example)
        ; and also to organize by objects
        subdir=strmid(file_dirname(fname),Linpath,Linpath)
        if subdir eq '.' then subdir=''
        subdirarr = subdirarr+objclass+'/'+subdir+'/'+strlowcase(strtrim(objfilt.hwp,2))+'/'
        
        ; Modify headers for each of the filters (1 filter if it is not a dark)
        ; Create a directory to stage this data and copy the fits file to the new directory
        im = readfits(fname,h,/silent)
        sxaddpar, h, 'OBJCLASS',strupcase(objclass)
        sxaddpar,h,'OBJECT',strupcase(objfilt.object)
        sxaddpar,h,'HWP',strupcase(objfilt.hwp)
        sxaddpar,h,'RAY','ALL'
        obsdate=strjoin(strsplit(strtrim(sxpar(h,'DATE-OBS'),2),':-',/extract))
        writename=rootname+'-'+obsdate+extension
        flip=strtrim(sxpar(h,'FLIPSTAT'),2)
        flipdir='rot0/'
        if flip eq 'Rotate 90 CW' then flipdir='rot90cw/'
        
          
        sxaddpar,h,'FILTER',strupcase(objfilt.filter)
        if keyword_set(single) then begin
          ray = strlowcase(strtrim(sxpar(h,'RAY'),2))
          sxaddpar,h,'FILE-ID',cgTimeStamp(8, /UTC, /VALID)
          if ray eq '0' or ray eq 'all' then begin
            file_mkdir,stage+flipdir+subdirarr
            writefits,stage+flipdir+subdirarr+writename,im,h
            print,'    - Copied at: '+subdirarr
          endif else begin
            if keyword_set(polarimetry) then begin
              file_mkdir,stage+flipdir+subdirarr+ray+'/'
              writefits,stage+flipdir+subdirarr+ray+'/'+file_basename(fname),im1,h
              print,'    - Copied at: '+subdirarr+ray+'/'
            endif else begin
              file_mkdir,stage+flipdir+ray+'/'+subdirarr
              writefits,stage+flipdir+ray+'/'+subdirarr+writename,im1,h
              print,'    - Copied at: '+ray+'/'+subdirarr
            endelse
          endelse
        endif else begin
          if flip eq 'Rotate 90 CW' then section=section90 else section=section0
          im1 = im[min([section[0,0],section[2,0]]):max([section[0,0],section[2,0]]),min([section[1,0],section[3,0]]):max([section[1,0],section[3,0]])]
          h1=h
          if flip eq 'Rotate 90 CW' then begin
            im1 = rotate(im1,3)
            ;sxaddpar,h1,'FLIPSTAT',''
          endif
          sxaddpar,h1,'RAY','ORDINARY'
          sxaddpar,h1,'FILE-ID',cgTimeStamp(8, /UTC, /VALID)
          if keyword_set(polarimetry) then begin
            file_mkdir,stage+flipdir+subdirarr+'ordinary/'
            writefits,stage+flipdir+subdirarr+'ordinary/'+file_basename(fname),im1,h1
            print,'    - Copied at: '+subdirarr+'ordinary/'
          endif else begin
            file_mkdir,stage+flipdir+'ordinary/'+subdirarr
            writefits,stage+flipdir+'ordinary/'+subdirarr+writename,im1,h1
            print,'    - Copied at: ordinary/'+subdirarr
          endelse
                    
          im2 = im[min([section[0,1],section[2,1]]):max([section[0,1],section[2,1]]),min([section[1,1],section[3,1]]):max([section[1,1],section[3,1]])]
          h2=h
          if flip eq 'Rotate 90 CW' then begin
            im2 = rotate(im2,3)
            ;sxaddpar,h2,'FLIPSTAT',''
          endif
          sxaddpar,h2,'RAY','EXTRAORDINARY'
          sxaddpar,h2,'FILE-ID',cgTimeStamp(8, /UTC, /VALID)
          if keyword_set(polarimetry) then begin
            file_mkdir,stage+flipdir+subdirarr+'extraordinary/'
            writefits,stage+flipdir+subdirarr+'extraordinary/'+file_basename(fname),im2,h2
            print,'    - Copied at: '+subdirarr+'extraordinary/'
          endif else begin
            file_mkdir,stage+flipdir+'extraordinary/'+subdirarr
            writefits,stage+flipdir+'extraordinary/'+subdirarr+writename,im2,h2
            print,'    - Copied at: extraordinary/'+subdirarr
          endelse
        endelse
      endelse
    endif
  endfor
  
END