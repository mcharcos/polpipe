PRO removefarposition, refpos, indir, outdir, recursive=recursive, plot=plot, ray=ray
  
  print,'Starting removefarposition...'
  
  if not keyword_set(ray) then ray='ordinary'
  if ray ne 'ordinary' and ray ne 'extraordinary' then ray = 'ordinary'
  
  if n_elements(refpos) ne 3 then begin
    print,'Reference position should be a three element array: [X,Y,R]'
    help,refpos
    print,'Aborting'
    return
  endif
  
  ;if not file_test(outdir, /directory) then begin
  ;  print, 'Creating output directory: '+outdir
  ;  file_mkdir,outdir
  ;endif
  outpath = file_expand_path(outdir)
  
  fprod = findproducts(indir, recursive=keyword_set(recursive))
  path = fprod.path
  foundfiles = fprod.files
  Ncount = fprod.count
  
  for i=0,Ncount-1 do begin
    if file_basename(file_dirname(foundfiles[i])) eq ray then begin
      relativepath=strmid(foundfiles[i],strlen(path),strlen(foundfiles[i]))
      curoutdir = outpath+file_dirname(relativepath)
      
      im = readfits(foundfiles[i],h,/silent)
      curx = sxpar(h,'ST0X')
      cury = sxpar(h,'ST0Y')
      curhwp = float(sxpar(h,'HWP'))
      curr=sqrt((curx-refpos[0])^2+(cury-refpos[1])^2)
      curpath=outpath+file_dirname(relativepath)+'/'
    endif else begin
      curx =-1
      cury =-1
      curr=99999
      curhwp=-1.
      relativepath='badray'
      curpath='badray'
    endelse
    if not keyword_set(xarr) then xarr = curx else xarr = [xarr,curx]
    if not keyword_set(yarr) then yarr = cury else yarr = [yarr,cury]
    if not keyword_set(rarr) then rarr = curr else rarr = [rarr,curr]
    if not keyword_set(hwparr) then hwparr = curr else hwparr = [hwparr,curhwp]
    if not keyword_set(opatharr) then opatharr = curpath else opatharr = [opatharr,curpath]    
    if not keyword_set(farr) then farr = relativepath else farr = [farr,relativepath]
  endfor
  
  
  idxbadray = where(farr ne 'badray')
  if idxbadray[0] eq -1 then begin
    print,'No requested ray '+ray+' in list'
    return
  endif
  
  for izoom=0,1 do begin
    window,izoom,retain=2
    mkct,0
    if izoom eq 0 then plot,xarr[idxbadray],yarr[idxbadray],psym=2, TITLE='HWP= 0 (green), 22.5 (blue), 45 (yellow) and 67.5 (purple) ' $
    else plot,xarr[idxbadray],yarr[idxbadray],psym=2, TITLE='HWP= 0 (green), 22.5 (blue), 45 (yellow) and 67.5 (purple) ',xrange=[refpos[0]-refpos[2]/2,refpos[0]+refpos[2]/2],yrange=[refpos[1]-refpos[2]/2,refpos[1]+refpos[2]/2]
    
    hwplist = [0.,22.5,45.,67.5]
    Nhwp = n_elements(hwplist)
    for ihwp=0,Nhwp-1 do begin
      khwp = where (hwparr eq hwplist[ihwp])
      
      if khwp[0] ne -1 then begin
        hxarr=xarr[khwp]
        hyarr=yarr[khwp]
        ;hrarr=rarr[khwp]
        ;hfarr=farr[khwp]
        ;hopatharr=opatharr[khwp]
        oplot,hxarr,hyarr,psym=2,color=3+ihwp
      endif
    endfor
    
    idxsel = where(rarr le refpos[2])
    if idxsel[0] eq -1 then begin
      print,'No star position was found within ['+strjoin(strtrim(refpos,2),',')+']'
      print,transpose([['FILE',farr],['HWP',strtrim(hwparr,2)],['STARX',strtrim(xarr,2)],['STARY',strtrim(yarr,2)],['DISTANCE',strtrim(rarr,2)]])
      junk = dialog_message('Exit')
      return
    endif
    
    oplot,xarr[idxsel],yarr[idxsel],psym=4, symsize=2,color=2
  endfor
  
  docp = dialog_message('Do you want to proceed copying these files?',/QUESTION)
  
  if docp eq 'No' then begin
    print,'Aborting copy step per user request'
    return
  endif
  
  if docp eq 'Yes' then begin
    for i=0,n_elements(idxsel)-1 do begin
      j = idxsel[i]
      pp = file_dirname(file_dirname(foundfiles[j]))
      ff = file_basename(foundfiles[j])
      if not file_test(file_dirname(opatharr[j])+'/ordinary',/directory) then begin
        print, 'Creating directory: '+file_dirname(opatharr[j])+'/ordinary'
        file_mkdir,file_dirname(opatharr[j])+'/ordinary'
      endif
      print,'Copying ordinary file '+farr[j]+'...'
      print,'    cp '+pp+'/ordinary/'+ff+' '+file_dirname(opatharr[j])+'/ordinary/'+ff
      file_copy,pp+'/ordinary/'+ff,file_dirname(opatharr[j])+'/ordinary/'+ff
      
      if not file_test(file_dirname(opatharr[j])+'/extraordinary',/directory) then begin
        print, 'Creating directory: '+file_dirname(opatharr[j])+'/extraordinary'
        file_mkdir,file_dirname(opatharr[j])+'/extraordinary'
      endif
      print,'Copying extraordinary file '+farr[j]+'...'
      print,'    cp '+pp+'/extraordinary/'+ff+' '+file_dirname(opatharr[j])+'/extraordinary/'+ff
      file_copy,pp+'/extraordinary/'+ff,file_dirname(opatharr[j])+'/extraordinary/'+ff
    endfor
  endif
  print,'Done with removefarposition!'
  
END
