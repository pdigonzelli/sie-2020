{windows.i}

define input parameter p_lista like listas_correo.id_lista.
define input parameter p_contrato as rowid.
define var okay as logical.
define var v_subject as char.
define var v_body as char.

find contratos where rowid(contratos) = p_contrato no-lock no-error.

if available contratos then
    do:
        for each usuarios_listas where usuarios_listas.id_lista = p_lista no-lock.
            v_body = "Contrato " + contratos.id_contrato.
            v_subject = "Contrato " + contratos.id_contrato.
            message "send mail " usuarios_lista.email v_subject v_body view-as alert-box.
            RUN SendMail (INPUT usuarios_listas.email,
                          INPUT v_subject, 
                          INPUT v_body, 
                          OUTPUT Okay). 
        
        end.
    end.
    
PROCEDURE SendMail : 

    DEFINE INPUT PARAMETER send-to-name AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER send-subject AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER send-text AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER Okay AS LOGICAL NO-UNDO INITIAL NO. 
    
    DEFINE VARIABLE pnames AS MEMPTR. 
    DEFINE VARIABLE psendto AS MEMPTR. 
    DEFINE VARIABLE psubj AS MEMPTR. 
    DEFINE VARIABLE ptext AS MEMPTR. 
    DEFINE VARIABLE pmessage AS MEMPTR. 
    DEFINE VARIABLE wans AS INT .
     
    SET-SIZE(pnames) = 24. 
    SET-SIZE(psendto) = length(send-to-name) + 10. 
    
    PUT-LONG(pnames,1) = 0. /* Reserved */ 
    PUT-LONG(pnames,5) = 1. /* Recip Class MAPI_TO */ 
    PUT-LONG(pnames,9) = GET-POINTER-VALUE(psendto). /* Names */ 
    PUT-LONG(pnames,17) = 0. /* EID Size */ 

    SET-SIZE(psubj) = 100. 
    SET-SIZE(ptext) = 8000. 
    SET-SIZE(pmessage) = 48.
    
    PUT-STRING(psubj,1) = send-subject. 
    PUT-STRING(ptext,1) = send-text. 
    PUT-STRING(psendto,1) = send-to-name. 
    
    PUT-LONG(pmessage,1) = 0. /* Reserved */ 
    PUT-LONG(pmessage,5) = GET-POINTER-VALUE(psubj). /* Subject */
    PUT-LONG(pmessage,9) = GET-POINTER-VALUE(ptext). /* Text */ 
    PUT-LONG(pmessage,25) = 0. /* Flags */ 
    PUT-LONG(pmessage,33) = 1. /* RecipCount */ 
    PUT-LONG(pmessage,37) = GET-POINTER-VALUE(pnames). 
    PUT-LONG(pmessage,41) = 0.    
    
    RUN MAPISendMail IN hpApi(INPUT 0, /* mapi session handle */ 
                              INPUT 0, /* parent window handle */ 
                              INPUT GET-POINTER-VALUE(pmessage), 
                              INPUT 0, /* flags */ 
                              INPUT 0, /* reserved, must be 0 */ 
                              OUTPUT Wans). /* error status */ 
                              
                              
    IF Wans<>0 THEN MESSAGE "Mail not sent, error code=" Wans VIEW-AS ALERT-BOX. ELSE Okay = YES. /* dealloc memory */ 
    
    SET-SIZE(pnames) = 0. 
    SET-SIZE(psendto) = 0. 
    SET-SIZE(psubj) = 0. 
    SET-SIZE(ptext) = 0. 
    SET-SIZE(pmessage) = 0. 
END PROCEDURE. 


