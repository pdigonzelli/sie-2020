DEF INPUT PARAMETER cProfile   AS CHAR NO-UNDO.
DEF INPUT PARAMETER iPriority  AS INT  NO-UNDO.
  /* Low = 0; Normal = 1; High = 2 */

DEF INPUT PARAMETER cSubject   AS CHAR NO-UNDO.
DEF INPUT PARAMETER cText      AS CHAR NO-UNDO.
DEF INPUT PARAMETER cTo        AS CHAR NO-UNDO.
DEF INPUT PARAMETER cBCC       AS CHAR NO-UNDO.
DEF INPUT PARAMETER cFiles     AS CHAR NO-UNDO.


/* Variables */

DEF VAR chSession AS COM-HANDLE NO-UNDO.
DEF VAR chMessage AS COM-HANDLE NO-UNDO.
DEF VAR chRecip   AS COM-HANDLE NO-UNDO.
DEF VAR chFiles   AS COM-HANDLE NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.



DEFINE VARIABLE chOutApplication      AS COM-HANDLE.
DEFINE VARIABLE chOutMsg              AS COM-HANDLE.
DEFINE VARIABLE chOutRecip             AS COM-HANDLE.
DEFINE VAR s AS COM-HANDLE.

/* create a new Excel Application object */
CREATE "Outlook.Application" chOutApplication.

chMessage = chOutApplication:CreateItem(0).
chMessage:importance= iPriority.

chMessage:Subject = cSubject.
chMessage:Body = cText.

DO iLoop = 1 TO NUM-ENTRIES(cTo):    
    ASSIGN
        chOutRecip = chMessage:Recipients:ADD(ENTRY(iLoop,cTo)).
        chOutRecip:TYPE = 1.
        chOutRecip:Resolve.
END.

chMessage:BCC = cBCC.

DO iLoop = 1 TO NUM-ENTRIES(cFiles):
ASSIGN        
        chMessage:Text = chMessage:Text + CHR(10)
        chFiles        = chMessage:Attachments:ADD()
        chFiles:name   = ENTRY(iLoop, cFiles)
        chFiles:source = ENTRY(iLoop, cFiles).
        
END.


chMessage:SEND().
chOutApplication:QUIT().


RELEASE OBJECT chMessage.
RELEASE OBJECT chOutRecip.
RELEASE OBJECT chOutApplication.




/* Message Creation */

/*
ASSIGN
    chMessage            = chSession:OutBox:Messages:Add()
    chMessage:Subject    = cSubject    
    chMessage:Text       = cText
    chMessage:importance = iPriority.
    
/* Send To */
DO iLoop = 1 TO NUM-ENTRIES(cTo):    
    ASSIGN
        chRecip      = chMessage:Recipients:Add()
        chRecip:Name = ENTRY(iLoop, cTo)        
        chRecip:Type = 1.
    chRecip:Resolve.
END.

/* Attachments */

DO iLoop = 1 TO NUM-ENTRIES(cFiles):

    ASSIGN        
        chMessage:Text = chMessage:Text + CHR(10)
        chFiles        = chMessage:Attachments:ADD()
        chFiles:name   = ENTRY(iLoop, cFiles)
        chFiles:source = ENTRY(iLoop, cFiles).
        
END.

/* Send message */

chMessage:DeliveryReceipt = TRUE.

chMessage:Update(TRUE, TRUE).
chMessage:Send(TRUE,FALSE,TRUE).
chSession:Logoff.



/* Release Objects */
RUN ReleaseObjects. 

*/

RETURN.

/* ---------------------------------------------------------------- */
PROCEDURE ReleaseObjects :    

    if valid-handle(chRecip)    THEN RELEASE OBJECT chRecip.
    if valid-handle(chMessage)  THEN RELEASE OBJECT chMessage.    
    if valid-handle(chFiles)    THEN RELEASE OBJECT chFiles.
    if valid-handle(chSession)  THEN RELEASE OBJECT chSession.
    
END PROCEDURE.
