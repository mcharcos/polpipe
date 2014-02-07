;******************************************************************************
;     CLASS_EVENTHAND - Event Handler
;******************************************************************************
pro class_eventhand, event
  Widget_Control, event.id, Get_UValue=cmd
  
  index = Where(StrPos(Tag_Names(cmd), 'PARAMETERS') EQ 0, count)
  if count eq 0 then begin
    Call_Method, cmd.method, cmd.object, event
  endif else begin
    Call_Method, cmd.method, cmd.object, event, cmd.parameters
  endelse
end