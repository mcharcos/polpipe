;******************************************************************************
;     CHECK - Used for testing. Display information about current objects
;******************************************************************************
PRO check

  COMMON shared_controller, appcontroller
  
  pipeline = (appcontroller->GetProperty(/pipeline)).key
  plan = (pipeline->GetProperty(/plan)).key
  steps = (pipeline->GetProperty(/steps)).key
  refsteps = (pipeline->GetProperty(/refsteps)).key
  
  if steps ne ptr_new() then begin
    print,'OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
    print,'OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
    print,'OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO'
    help,plan
    print,'OOOOOOOOOOOOOO'
    for i=0,n_elements(*steps)-1 do begin
      currentstep = (*steps)[i]
      if currentstep ne ptr_new() then begin
        print,'REF:'
	help,(*refsteps)[i]
        help,*currentstep
	for j=0,n_elements(*currentstep)-1 do begin
	  print,'================='
	  print,'================='
	  help,(*currentstep)[j]
	  help,(*currentstep)[j],/struct
	  indata = ((*currentstep)[j]->Getproperty(/indata)).key
	  if indata ne ptr_new() then print,'INDATA: '+*indata else print,'INDATA: null'
	  outdata = ((*currentstep)[j]->Getproperty(/outdata)).key
	  if outdata ne ptr_new() then print,'OUTDATA: '+*outdata else print,'OUTDATA: null'
	  ancillary = ((*currentstep)[j]->Getproperty(/ancillary)).key
	  if ancillary ne ptr_new() then print,'ANCILLARY: '+*ancillary else print,'ANCILLARY: null'
	  previous = ((*currentstep)[j]->Getproperty(/previous)).key
	  if previous ne ptr_new() then begin
	    print,'PREVIOUS: '
	    for l=0,n_elements(*previous)-1 do help,(*previous)[l]
	  endif else begin
	    print,'PREVIOUS: null'
	  endelse
	  keycommon = ((*currentstep)[j]->Getproperty(/keycommon)).key
	  if keycommon ne ptr_new() then begin
	    print,'KEYCOMMON: '
	    for l=0,n_elements(*keycommon)-1 do help,(*keycommon)[l],/struct
	  endif else begin
	    print,'KEYCOMMON: null'
	  endelse
	  keylist = ((*currentstep)[j]->Getproperty(/keylist)).key
	  if keylist ne ptr_new() then begin
	    print,'KEYLIST: '
	    for l=0,n_elements(*keylist)-1 do help,(*keylist)[l],/struct
	  endif else begin
	    print,'KEYLIST: null'
	  endelse
	  keyancillary = ((*currentstep)[j]->Getproperty(/keyancillary)).key
	  if keyancillary ne ptr_new() then begin
	    print,'KEYANCILLARY: '
	    for l=0,n_elements(*keyancillary)-1 do help,(*keyancillary)[l],/struct
	  endif else begin
	    print,'KEYANCILLARY: null'
	  endelse
	endfor
      endif
    endfor
  endif
  
  print,'<<<<<<<<<< PAN >>>>>>>>>>>>>'
  print,'ROOTPATH = '+(plan->GetProperty(/rootpath)).key
  print,'RESOURCEPATH = '+(plan->GetProperty(/resourcepath)).key
  print,'FEXCLUSION = '+(plan->GetProperty(/fexclusion)).key
  ptrgroups = (plan->GetProperty(/groups)).key
  if ptrgroups ne ptr_new() then begin
    groups = *ptrgroups
    for i=0,n_elements(groups)-1 do begin
      currentptr = (groups[i].indata)
      if currentptr ne ptr_new() then print,'INDATA: '+(*currentptr) else print, 'INDATA: null'
      currentptr = (groups[i].ancillary)
      if currentptr ne ptr_new() then print,'ANCILLARY: '+(*currentptr) else print, 'ANCILLARY: null'
    endfor
  endif
  
  for i=0,2 do begin
    outs = plan->GiveOutputs(groupidx=i)
    if outs ne ptr_new() then begin
      print,'-_-_-_-_-_-_-_-_-_' + strtrim(i,2)
      print,*outs
    endif
  endfor
  
  a =*((*steps)[0])
  for i=0,n_elements(a)-1 do begin
    help,a[i]
    outputs = (a[i]->GetProperty(/keyancillary)).key
    help,(*outputs)[0],/struct
  endfor
  
  help,(*refsteps)[0],/struct
  
END
;******************************************************************************
;     CHECK2 - Used for testing. Display information about current objects
;******************************************************************************
PRO check2
  
  COMMON shared_controller, appcontroller
  
  pipeline = (appcontroller->GetProperty(/pipeline)).key
  plan = (pipeline->GetProperty(/plan)).key
  steps = (pipeline->GetProperty(/steps)).key
  refsteps = (pipeline->GetProperty(/refsteps)).key
  
  groups = *((plan->GetProperty(/groups)).key)
  
  ptrsteplist = (steps)[0]
  runsteplist = *ptrsteplist
  
  runstep = *(runsteplist[2])
  for i=0,n_elements(runstep)-1 do begin
    runobj = runstep[i]
    help,runobj,/struct
    a = *((runobj->GetProperty(/keycommon)).key)
    help,a,/struct
    b = *(groups[i].keycom)
    for j=0,n_elements(b)-1 do begin
      help,b[j],/struct
    endfor
  endfor
    
END

;******************************************************************************
;     MLOF_QUIT - Exits the gui
;******************************************************************************
PRO mlof_quit, event
  
  ; Problem when I push quit because it is trying to call itself
  ; after the window is destroy.
  
  COMMON share_widg, global_widg
    
  if event.id eq global_widg.top then begin 
    return
  endif else begin
    msg = ['Do you want to quit MLOF pipeline?', $
           '']

    doquit = dialog_message(msg,/QUESTION,/CENTER)
  endelse
  
  if doquit eq 'Yes' then begin
    ; Save latest path
    datapath = (global_widg.gui_load->GetProperty(/fname)).key
    
    save,datapath,filename=global_widg.DefaultConf
    
    ; Destroy gui
    widget_control,global_widg.top,/destroy
    
    ; Destroy objects and free heaps
          
  endif
END

;******************************************************************************
;     GUI_ABOUT - Message for user about software
;                     The message should be written in the about.txt file
;******************************************************************************
PRO mlof_about, event
  
  fname = 'about.txt'
  msg = 'CALPLAN'
  if FILE_TEST(fname) eq 1 then begin
    openr,inunit,fname,/get_lun
    readline = ''
    while not eof(inunit) do begin
      readf,inunit,readline
      msg = [msg,readline]
    endwhile
    close,inunit
    free_lun,inunit
  endif
  
  jnk = dialog_message(msg,/INFORMATION,/CENTER)
  
END

;******************************************************************************
;     MLOF_EVENT - Handle GUI Events
;******************************************************************************
PRO mlof_event, event
  
  COMMON share_widg, global_widg
  
END


;******************************************************************************
;     MLOF_GUI - Create GUI
;               XS, YS: Size unit of the window
;******************************************************************************

PRO mlof_gui, xsize=xs
  

  DEBUG=1
  if DEBUG eq 1 then setenv,'IDL_DEBUG=1' else setenv,'IDL_DEBUG=0'
  
  ; check size keywords
  if not keyword_set(xs) then xs=500
  if not keyword_set(ys) then ys=500
  
  ;---------------------------
  ; INITIALIZE COLOR MAP
  ;---------------------------  
  mkct,0
  ; colors are [1='white',2='red',3='green',4='blue',5='yellow',6='magenta',7='cyan',8='lightred',9='lightgreen',10='lightblue']
  
  ;---------------------------
  ; INITIALIZE OBJECTS
  ;---------------------------
  Nstars = 1
  processingplan = OBJ_NEW('RUNPLAN', $
            keycommon=[{keyname:'OBJECT',keyval:'',type:'diff'},{keyname:'FILTER',keyval:'',type:'same'},{keyname:'OBJCLASS',keyval:'',type:'none'}], $
            keylist=[{keyname:'OBJECT',keyval:'',type:'same'},{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'},{keyname:'HWP',keyval:'',type:'same'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'fixed'}])
  badpix_correction = OBJ_NEW('RUNBADPIX', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $   ;,{keyname:'OBJCLASS',keyval:'',type:'none'}], $
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DARK',type:'fixed'}])
  dark_correction = OBJ_NEW('RUNDARK', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $   ;,{keyname:'OBJCLASS',keyval:'',type:'none'}], $
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DARK',type:'fixed'}])
  flatfield_correction = OBJ_NEW('RUNFLATFIELD', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $   ;,{keyname:'OBJCLASS',keyval:'RUNDARK',type:'fixed'}], $
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'FLAT',type:'fixed'}])
  coadd_correction = OBJ_NEW('RUNCOADD', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $  
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}])
  photometry = OBJ_NEW('RUNPHOTOMETRY', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'}], $  
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}], $
	    Npeaks=2*Nstars)
  polarimetry = OBJ_NEW('RUNPOLARIMETRY', $
            keycommon=[{keyname:'FILTER',keyval:'',type:'same'},{keyname:'OBJECT',keyval:'',type:'same'}], $  
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}])
  serkowski = OBJ_NEW('RUNSERKOWSKI', $
            keycommon=[{keyname:'OBJECT',keyval:'',type:'same'}], $  
            keylist=[{keyname:'OBJECT',keyval:'DARK|FLAT',type:'diff'}], $
	    keyancillary=[{keyname:'OBJECT',keyval:'DONTNEEDIT',type:'fixed'}])
  
  pipeobjs = [badpix_correction,dark_correction,flatfield_correction,coadd_correction,photometry,polarimetry,serkowski]
  pipelines = OBJ_NEW('PIPELINE',processingplan,pipeobjs)
    
  ;---------------------------
  ; DEFINE STRUCTURE OF WIDGETS
  ;---------------------------
  COMMON share_widg, global_widg
  global_widg = {top:0L, $                               ; Top window widget id
		 defaultconf:'DefaultConf.sav', $
		 gui_load:obj_new(), $
		 xs:xs,ys:ys} 
    
  
  
  ;---------------------------
  ; CREATE LAYOUT
  ;---------------------------
  ; base widget
  global_widg.top=widget_base(title='MLOF PIPELINE', mbar=mbar,event_pro='calplan_event', /column) ;, $
                  ;kill_notify='gui_quit')
    
  ;--------------------
  ;--------------------
  ; Create Menu bar
  file_menu=widget_button(mbar, value='Files', /menu)
  quit=widget_button(file_menu, value='Quit', event_pro='mlof_quit',/separator)
  
  help_menu=widget_button(mbar, value='Help', /menu)
  about=widget_button(help_menu, value='About', event_pro='mlof_about')
  
  toppanel = widget_base(global_widg.top, /row)
  midpanel = widget_base(global_widg.top, /row)
  botpanel = widget_base(global_widg.top, /column)
  
  ;--------------------
  ;--------------------
  ; Define the controller of the application 
  COMMON shared_controller, appcontroller
  appcontroller = OBJ_NEW('APPCONTROLLER', pipelines, debug=DEBUG)
  
  ;==================
  ; TOP PANEL
  ;==================
  
  ; Button panel
  butsize = 150
  butpanel = widget_base(toppanel,/column, xsize=butsize,ysize=ys*0.6)
  
  ;--------------------
  ;--------------------
  ; Create Panel for directory selection - load files
  for i=0,n_elements(pipeobjs)-1 do begin
    gui_run = OBJ_NEW('gui_runstep',butpanel,pipelines,name=OBJ_CLASS(pipeobjs[i]),object=appcontroller, $
                                    procname='RUN', $
                                    procvar={name:'step'+strtrim(i,2),val:strtrim(i,0),type:'int'},debug=DEBUG)
  endfor
  
  ;--------------------
  ;--------------------
  ; Create Panel for plotting results
  display1 = OBJ_NEW('gui_plot',toppanel,debug=DEBUG, xsize=2.*xs)
  ;display2 = OBJ_NEW('gui_plot',toppanel,debug=DEBUG)
  appcontroller->SetProperty,display1=display1 ;,display2=display2
  
  
  ;==================
  ; MIDDLE PANEL
  ;==================
  
  ;--------------------
  ;--------------------
  ; Create Panel for directory selection - load files
  gui_load = OBJ_NEW('gui_load',midpanel,xsize=xs/4., /directory,object=appcontroller, debug=DEBUG)
  appcontroller->SetProperty,gui_load=gui_load
  global_widg.gui_load=gui_load
  
  
  ;==================
  ; BOTTOM PANEL
  ;==================
  
  ;--------------------
  ;--------------------
  ; Create Panel for directory selection - load files
  ytable_size = 200
  base_tab = widget_base(botpanel,/row)
  calibrator_table = OBJ_NEW('gui_table',base_tab,xsize=xs*2.5,ysize=ytable_size,object=appcontroller, debug=DEBUG)
  ;calibrator_sel = OBJ_NEW('gui_seltab',base_tab,calibrator_table,['FILTER','STARPHOT','STAREPHOT','SIGN','FWHMX','FWHMY','FWHMANG','EFWHMX','EFWHMY','EFWHMANG'],xsize=xs/2,ysize=ytable_size)
   
  ;appcontroller->SetProperty,calibrator_table=calibrator_table, flag_table=flag_table, target_table=target_table
  ;appcontroller->SetProperty,calibrator_sel=calibrator_sel, flag_sel=flag_sel, target_sel=target_sel
  appcontroller->SetProperty,calibrator_table=calibrator_table
  
  
  ;--------------------
  ;--------------------
  ; Add attributes to objects that require them
  ; Set of forcast data (entire mission)
  ;defaultfname='/Users/mcharcos/ext_disk/pipecal_test_Dec182012/'
  ; Set of forcast data (three files to test things in a small amount of time)
  ;defaultfname='/Users/mcharcos/ext_disk/pipecal_test_Nov282012/'
  ; Set of flitecam data (two files from Sachin)
  defaultfname='/Users/mcharcos/ext_disk/MLOF/MLOF_November_2012/test/'  ;20121114/'
  if file_test(global_widg.DefaultConf) then begin
    restore,global_widg.DefaultConf
    defaultfname = datapath
  endif
  
  if not file_test(defaultfname,/directory) then begin
    rootdir = GETENV('HOME')
    if file_test(rootdir,/directory) then defaultfname = rootdir+'/' else defaultfname = './'
  endif
  
  ; Check with the user if we stay in the current path
  pathstay = dialog_message(['The current path is '+defaultfname,'Would you like to stay in this path?'],/QUESTION)
  if pathstay eq 'No' then begin
    read_path= 'supercalifragilistico'
    while (file_test(read_path,/directory) eq 0) do begin
      readpath = dialog_pickfile(/must_exist, /read, get_path=read_path, path=defaultfname, TITLE='Select the folder containing the raw data', /directory)
      if readpath eq '' then begin
        print,'Quiting MLOF pipeline software...'
	return
      endif
    endwhile
    defaultfname = read_path
  endif
  gui_load->SetProperty,fname=defaultfname
  gui_load->Refresh
  appcontroller->addPath,(gui_load->GetProperty(/path)).key
  
  
  ;................................
  ;................................
  ; Back to the top panel
  ; This is because I need to first define the tables before creating the gui_plottab object
  ;gui_plottab1 = OBJ_NEW('gui_plottab',butpanel, display1, calibrator_table, ['','calfactor'], butname='Plot (NA)')
  ;................................
  ;................................
  
  
  ; realize and start widgets
  widget_control, global_widg.top, /realize
  xmanager, 'mlof', global_widg.top, /no_block
  
END
