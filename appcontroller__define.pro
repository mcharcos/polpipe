; NAME:
;     appcontroller - Version 1.0
;
; PURPOSE: Class allowing to visualize data in a table
;
; CLASS ATTRIBUTES:       
;
;     + calibrator_table: Object with the table widget
;     + flag_table:       Object with the table widget
;     + target_table:     Object with the table widget
;     + gui_load:         Object with the load widget
;     + gui_run:          Object with the run widget
;     + calplan:          Object with the calibration plan object
;     
;
; CLASS METHODS:
;     + INIT: Initialize object  
;     + CLEANUP: Clean heaps. 
;     + Refesh: Refresh the display
;     + AddPath: Update plan and table
;     
; INHEREITS METHODS:
;     + From CLASSDEF
;
; MODIFICATION HISTORY:
;     Written by:  Miguel Charcos (mcharcos@sofia.usra.edu), USRA, January 22nd 2013

;****************************************************************************
;     PLOT - Plot a specific element of the pipeline
;****************************************************************************
PRO appcontroller::plot, groupnum, type
  
  SWITCH strlowcase(type) of
    'name': begin
            plan = (self.pipeline->GetProperty(/plan)).key
            plan->Plot,groupnum
            resarr = plan->GroupArray(labels=1,filebase=1, _EXTRA=extraKeywords)
            ireset = 0
            maxireset = 4
            for i=0,n_elements(resarr[*,0])-1 do begin
              auxstr = resarr[i,0]+'='+resarr[i,groupnum+1]
              if ireset eq 0 then newmsg = auxstr else newmsg = [newmsg,auxstr]
              if ireset eq maxireset then begin
                strmsg = strjoin(newmsg,', ')
                if n_elements(msg) eq 0 then msg = strmsg else msg = [msg,strmsg]
                ireset = 0
              endif else begin
                ireset = ireset + 1
              endelse
            endfor
            if ireset ne maxireset then begin
              strmsg = strjoin(newmsg,', ')
              if n_elements(msg) eq 0 then msg = strmsg else msg = [msg,strmsg]
            endif
            junk = dialog_message(msg)
            break
          end
    'indata':
    'ancillary':
    'indata:1':
    'ancillary:1':
    'indata:1:2':
    'ancillary:1:2':
    'indata:1:2:3':
    'ancillary:1:3':
    'indata:1:2:3:4':
    'ancillary:1:3:4':
    'indata:1:2:3:4:5':
    'ancillary:1:3:4:5': begin
                          break
                        end
    else: begin
          ; Here we look for the first object with the same number and we call plot for that object
          refsteps = (self.pipeline->GetProperty(/refsteps)).key
          if refsteps eq ptr_new() then return
          if n_elements(*refsteps) eq 0 then return
          steps = (self.pipeline->GetProperty(/steps)).key
          if steps eq ptr_new() then return
          if n_elements(*steps) eq 0 then return
          
          ; Look for the runstep object that matches the type
          for i=0,n_elements(*refsteps)-1 do begin
            ;self->Message,'checking '+OBJ_CLASS((*refsteps)[i])+' vs '+type,priority='DEBUG',method='PLOT'
            if strlowcase(OBJ_CLASS((*refsteps)[i])) eq strlowcase(type) then begin
              ; We should check if groupnum matches step array
              ; let's assume it for now
              ptrobjlist = (*steps)[i]
              objlist = *ptrobjlist
              obj = objlist[groupnum]
              self->Message,'Plotting '+OBJ_CLASS(obj)+' object',priority='DEBUG',method='PLOT'
              obj->Plot
            endif
          endfor
          return
        end
  ENDSWITCH
  
END

;****************************************************************************
;     PRINTINFO - Print information of a specific element of the pipeline
;****************************************************************************
PRO appcontroller::printInfo, groupnum, type
  
  SWITCH strlowcase(type) of
    'name': 
    'indata':
    'ancillary':
    'indata:1':
    'ancillary:1':
    'indata:1:2':
    'ancillary:1:2':
    'indata:1:2:3':
    'ancillary:1:3':
    'indata:1:2:3:4':
    'ancillary:1:3:4':
    'indata:1:2:3:4:5':
    'ancillary:1:3:4:5': begin
                          break
                        end
    else: begin
          ; Here we look for the first object with the same number and we call plot for that object
          refsteps = (self.pipeline->GetProperty(/refsteps)).key
          if refsteps eq ptr_new() then return
          if n_elements(*refsteps) eq 0 then return
          steps = (self.pipeline->GetProperty(/steps)).key
          if steps eq ptr_new() then return
          if n_elements(*steps) eq 0 then return
          
          ; Look for the runstep object that matches the type
          for i=0,n_elements(*refsteps)-1 do begin
            if strlowcase(OBJ_CLASS((*refsteps)[i])) eq strlowcase(type) then begin
              ; We should check if groupnum matches step array
              ; let's assume it for now
              ptrobjlist = (*steps)[i]
              objlist = *ptrobjlist
              obj = objlist[groupnum]
              print,'OBJECT: '+OBJ_CLASS(obj)
              info = obj->GroupArray(/labels,/details)
              
              straux = strjoin(transpose(info),' = ')
              print,straux
              ok = dialog_message(straux)
            endif
          endfor
          return
        end
  ENDSWITCH
  
END

;****************************************************************************
;     PRINTDATA - Print information of input and output data
;****************************************************************************
PRO appcontroller::printData, groupnum, type
  
  SWITCH strlowcase(type) of
    'name': 
    'indata':
    'ancillary':
    'indata:1':
    'ancillary:1':
    'indata:1:2':
    'ancillary:1:2':
    'indata:1:2:3':
    'ancillary:1:3':
    'indata:1:2:3:4':
    'ancillary:1:3:4':
    'indata:1:2:3:4:5':
    'ancillary:1:3:4:5': begin
                          break
                        end
    else: begin
          ; Here we look for the first object with the same number and we call plot for that object
          refsteps = (self.pipeline->GetProperty(/refsteps)).key
          if refsteps eq ptr_new() then return
          if n_elements(*refsteps) eq 0 then return
          steps = (self.pipeline->GetProperty(/steps)).key
          if steps eq ptr_new() then return
          if n_elements(*steps) eq 0 then return
          
          ; Look for the runstep object that matches the type
          for i=0,n_elements(*refsteps)-1 do begin
            if strlowcase(OBJ_CLASS((*refsteps)[i])) eq strlowcase(type) then begin
              ; We should check if groupnum matches step array
              ; let's assume it for now
              ptrobjlist = (*steps)[i]
              objlist = *ptrobjlist
              obj = objlist[groupnum]
              print,'OBJECT: '+OBJ_CLASS(obj)
              indata = (obj->GetProperty(/indata)).key
              outdata = (obj->GetProperty(/outdata)).key
              ancillary = (obj->GetProperty(/ancillary)).key
              print,'INDATA:'
              if indata ne ptr_new() then print,*indata else print,'null'
              print,'OUTDATA:'
              if outdata ne ptr_new() then print,*outdata else print,'null'
              print,'ANCILLARY:'
              if ancillary ne ptr_new() then print,*ancillary else print,'null'
              ;straux = strjoin(transpose(info),' = ')
              ;ok = dialog_message(straux)
            endif
          endfor
          return
        end
  ENDSWITCH
  
END

;****************************************************************************
;     PRINTKEYS - Print information of input and output data
;****************************************************************************
PRO appcontroller::printKeys, groupnum, type
  
  SWITCH strlowcase(type) of
    'name': 
    'indata':
    'ancillary':
    'indata:1':
    'ancillary:1':
    'indata:1:2':
    'ancillary:1:2':
    'indata:1:2:3':
    'ancillary:1:3':
    'indata:1:2:3:4':
    'ancillary:1:3:4':
    'indata:1:2:3:4:5':
    'ancillary:1:3:4:5': begin
                          break
                        end
    else: begin
          ; Here we look for the first object with the same number and we call plot for that object
          refsteps = (self.pipeline->GetProperty(/refsteps)).key
          if refsteps eq ptr_new() then return
          if n_elements(*refsteps) eq 0 then return
          steps = (self.pipeline->GetProperty(/steps)).key
          if steps eq ptr_new() then return
          if n_elements(*steps) eq 0 then return
          
          ; Look for the runstep object that matches the type
          for i=0,n_elements(*refsteps)-1 do begin
            if strlowcase(OBJ_CLASS((*refsteps)[i])) eq strlowcase(type) then begin
              ; We should check if groupnum matches step array
              ; let's assume it for now
              print,'----------'
              print,'REF ===>'
              obj = (*refsteps)[i]
              print,'OBJECT: '+OBJ_CLASS(obj)
              keycommon = (obj->GetProperty(/keycommon)).key
              keylist = (obj->GetProperty(/keylist)).key
              keyancillary = (obj->GetProperty(/keyancillary)).key
              print,'KEY COMMON:'
              if keycommon ne ptr_new() then help,*keycommon
              if keycommon ne ptr_new() then print,*keycommon else print,'null'
              print,'KEY LIST:'
              if keylist ne ptr_new() then help,*keylist
              if keylist ne ptr_new() then print,*keylist else print,'null'
              print,'KEY ANCILLARY:'
              if keyancillary ne ptr_new() then help,*keyancillary
              if keyancillary ne ptr_new() then print,*keyancillary else print,'null'
              
              print,'----------'
              print,'GROUP ===>'
              ptrobjlist = (*steps)[i]
              objlist = *ptrobjlist
              obj = objlist[groupnum]
              print,'OBJECT: '+OBJ_CLASS(obj)
              keycommon = (obj->GetProperty(/keycommon)).key
              keylist = (obj->GetProperty(/keylist)).key
              keyancillary = (obj->GetProperty(/keyancillary)).key
              print,'KEY COMMON:'
              if keycommon ne ptr_new() then print,*keycommon else print,'null'
              print,'KEY LIST:'
              if keylist ne ptr_new() then print,*keylist else print,'null'
              print,'KEY ANCILLARY:'
              if keyancillary ne ptr_new() then print,*keyancillary else print,'null'
              ;straux = strjoin(transpose(info),' = ')
              ;ok = dialog_message(straux)
            endif
          endfor
          return
        end
  ENDSWITCH
  
END

;****************************************************************************
;     REFRESH - Refresh the displays (tables)
;****************************************************************************
PRO appcontroller::refresh
  
  ;--------------------------
  ; Update calibrator table
  if self.calibrator_table ne OBJ_NEW() then begin
    ;table_arr = plan->GroupArray(/labels,/mergearr,/filebase)
    table_arr = self.pipeline->toArray(/labels)
    s = size(table_arr)
    if s[0] ne 2 then begin
      self->Message,'Error creating calibration plan',priority='WARN',method='ADDPATH'
      self->Message,'     calibration table array of calibration plan is empty',priority='WARN',method='ADDPATH'
      self->Message,'Refresh calibration table failed',priority='WARN',method='ADDPATH'
    endif else begin
      index = strtrim(indgen(n_elements(table_arr[0,*])),2)
      table_arr = transpose([[index],[transpose(table_arr)]])

      ; Update table with information
      self.calibrator_table->Update,table_arr
      if self.calibrator_sel ne OBJ_NEW() then self.calibrator_sel->Refresh
    endelse 
  endif
  
END


;****************************************************************************
;     ADDPATH - Pop-up a browser window and after directory/file selection
;               it refresh the text widget and update the object if existing
;****************************************************************************
PRO appcontroller::addPath, path
  
  ; We are going to start from scratch when we add a new path
  ; another option would be to just add the files of the new path
  ; It would work but it will require to add an option for the
  ; user to reset the pipeline
  self.pipeline->reset
  
  ; Update the keywords of the data
  ; Unfortunately, I did not implement anything that allows skipping this step if it was already done
  updatehd = dialog_message('Do you want to update the headers of the files?',/question,/DEFAULT_NO)
  if updatehd eq 'Yes' then begin
    updateflatsdarks = dialog_message('Do you want to update flat and dark keywords?',/question,/DEFAULT_NO)
    if updateflatsdarks eq 'Yes' then begin
      flats=dialog_pickfile(/must_exist, /read, get_path=read_path, path=path, TITLE='Select Flat Fields', /multiple_files)
      darks=dialog_pickfile(/must_exist, /read, get_path=read_path, path=path, TITLE='Select Darks', /multiple_files)
      if flats[0] ne '' and darks[0] ne '' then begin
        hdpopulate, path, /go, flats=flats, darks=darks
      endif else begin
        if flats[0] ne '' then hdpopulate, path, /go, flats=flats else hdpopulate, path, /go, darks=darks
      endelse
    endif else begin
      hdpopulate, path, /go
    endelse
  endif
  
  ; Update pipeline path
  self.pipeline->addPath, path=path
  self.pipeline->addPath, path=path, /ancillary
    
  ; Update the information of the table based on the calibration plan
  self->refresh  
  
END


;****************************************************************************
;     CLEANUP - Call clean pointer heap variables. Requires implementation in child
;****************************************************************************
PRO appcontroller::cleanup
  
  ; We clean the objects when the controller is killed
  ;obj_destroy, self.calibrator_table
  ;obj_destroy, self.flag_table
  ;obj_destroy, self.target_table
  ;obj_destroy, self.gui_load
  ;obj_destroy, self.gui_run
  ;obj_destroy, self.calplan
  
  
END

;****************************************************************************
;     INIT - Initialize structure fields.
;****************************************************************************
FUNCTION appcontroller::init, pipeline, gui_load=gui_load, gui_run=gui_run, _Extra=extraKeyword, $
                         calibrator_table=calibrator_table, flag_table=flag_table, target_table=target_table, $
                         calibrator_sel=calibrator_sel, flag_sel=flag_sel, target_sel=target_sel
  
  if N_PARAMS() lt 1 then begin
    self->Message,'Wrong number of input parameters',priority='ERR',method='INIT'
    return,0
  endif
  
  if self->classdef::init(_Extra=extraKeyword) eq 0 then begin
    print,'appcontroller::init failed'
    return,0
  endif
  
  if keyword_set(calibrator_table) then self.calibrator_table = calibrator_table
  if keyword_set(flag_table) then self.flag_table = flag_table
  if keyword_set(target_table) then self.target_table = target_table
  if keyword_set(calibrator_sel) then self.calibrator_sel = calibrator_sel
  if keyword_set(flag_table) then self.flag_table = flag_table
  if keyword_set(target_sel) then self.target_sel = target_sel
  if keyword_set(gui_load) then self.gui_load = gui_load
  if keyword_set(gui_run) then self.gui_run = gui_run
  
  
  self.pipeline = pipeline
  
  return, 1
  
END

;****************************************************************************
;     APPCONTROLLER__DEFINE - Define the class structure for the class appcontroller
;****************************************************************************

PRO appcontroller__define

struct={appcontroller, $
        calibrator_table:OBJ_NEW(), $    ; Object with the table widget
        flag_table:OBJ_NEW(), $          ; Object with the table widget
        target_table:OBJ_NEW(), $        ; Object with the table widget
        calibrator_sel:OBJ_NEW(), $    ; Object with the selection table widget
        flag_sel:OBJ_NEW(), $          ; Object with the selection table widget
        target_sel:OBJ_NEW(), $        ; Object with the selection table widget
        display1:OBJ_NEW(), $            ; Object with display widget #1
        display2:OBJ_NEW(), $            ; Object with display widget #2
        gui_load:OBJ_NEW(), $            ; Object with the load widget
        gui_run:OBJ_NEW(), $             ; Object with the run widget
        pipeline:OBJ_NEW(), $           ; Array of objects forming the pipeline
	inherits classdef}

END
