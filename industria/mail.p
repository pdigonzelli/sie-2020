{windows.i}

define var okay as logical.


RUN SendMail (INPUT "pdigonze@sa-sanmiguel.com", INPUT "Orden entreda 6584", INPUT "Mira tal OE", "c:\temp\adrian.txt" , OUTPUT Okay). 

PROCEDURE SendMail : 

    DEFINE INPUT PARAMETER send-to-name AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER send-subject AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER send-text AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER archivo   AS CHAR  NO-UNDO.
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
    PUT-LONG(pmessage,41) = if archivo = "" then 0 else 1. /* cero si no quiero attach */
    
    
    /****** PARA EL ARCHIVO ATACHADO ******************/
    
       DEFINE VARIABLE FilePathNamePtr AS MEMPTR.
       SET-SIZE(FilePathNamePtr) = LENGTH(archivo) + 1.  /* maximum = 255 */
       PUT-STRING(FilePathNamePtr,1) = archivo.  /* File pathname */
     
       DEFINE VARIABLE FILENAME AS CHARACTER NO-UNDO.
       FILENAME = SUBSTRING(archivo,R-INDEX(archivo,"\":U) + 1).
       /* extract filename (starting on last \) from filefullname */

       FILENAME = "     ":U + FILENAME.

       /* for some strange reason the first five chars disappear */
     
       DEFINE VARIABLE FileNamePtr AS MEMPTR.
       SET-SIZE(FileNamePtr) = LENGTH(FILENAME) + 1. /* maximum = 255 */ 
       PUT-STRING(FileNamePtr,1) = FILENAME. /* File name */
     
       DEFINE VARIABLE FileDescPtr AS MEMPTR.
       SET-SIZE(FileDescPtr) = 24.
       PUT-LONG(FileDescPtr,1) = 0. /* Reserved */ 
       PUT-LONG(FileDescPtr,5) = 0. /* Flags 0 = data file */
       PUT-LONG(FileDescPtr,9) =  LENGTH(send-text) + 2.  /* Position */
       PUT-LONG(FileDescPtr,13) = GET-POINTER-VALUE(FilePathNamePtr).  /* PathName */
       PUT-LONG(FileDescPtr,17) = GET-POINTER-VALUE(FileNamePtr). /* FileName */ 
       PUT-LONG(FileDescPtr,21) = 0. /* FileType */
    
    
    PUT-LONG(pmessage,45) = (IF archivo = "":U 
                                      THEN 0 
                                      ELSE GET-POINTER-VALUE(FileDescPtr)). /* Files */    
    
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
