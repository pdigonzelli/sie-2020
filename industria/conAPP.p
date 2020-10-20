

DEFINE VARIABLE hfacu AS HANDLE     NO-UNDO.
DEFINE VARIABLE flag AS LOGICAL    NO-UNDO.
DEFINE VARIABLE vch AS char   NO-UNDO.
 
CREATE SERVER hfacu.
flag = hfacu:CONNECT("-AppService asbroker1 -H localhost").
MESSAGE flag
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*hfacu:export("pruebaAPP.p").*/
run pruebaAPP.p on server hfacu TRANSACTION DISTINCT (output vch).    


disp vch.
