
     
     
     
DEF INPUT PARAMETER cProfile   AS CHAR NO-UNDO.
DEF INPUT PARAMETER iPriority  AS INT  NO-UNDO.
  /* Low = 0; Normal = 1; High = 2 */

DEF INPUT PARAMETER cSubject   AS CHAR NO-UNDO.
DEF INPUT PARAMETER cText      AS CHAR NO-UNDO.
DEF INPUT PARAMETER cTo        AS CHAR NO-UNDO.
DEF INPUT PARAMETER cFiles     AS CHAR NO-UNDO.

/* Variables */

DEF VAR chSession AS COM-HANDLE NO-UNDO.
DEF VAR chMessage AS COM-HANDLE NO-UNDO.
DEF VAR chRecip   AS COM-HANDLE NO-UNDO.
DEF VAR chFiles   AS COM-HANDLE NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
/* Connection */CREATE "MAPI.SESSION" chSession.
chSession:Logon(cProfile).
/* Message Creation */
ASSIGN
    chMessage            = chSession:OutBox:Messages:Add()
    chMessage:Subject    = cSubject    chMessage:Text       = cText
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

    ASSIGN        chMessage:Text = chMessage:Text + CHR(10)
        chFiles        = chMessage:Attachments:ADD()
        chFiles:name   = ENTRY(iLoop, cFiles)
        chFiles:source = ENTRY(iLoop, cFiles).
        
END.
        
        /* Send message */
chMessage:Update(TRUE, TRUE).
chMessage:Send(TRUE, FALSE).
chSession:Logoff.

/* Release Objects */RUN ReleaseObjects.
/* ---------------------------------------------------------------- */
PROCEDURE ReleaseObjects :    
    RELEASE OBJECT chRecip.
    RELEASE OBJECT chMessage.    
    RELEASE OBJECT chSession.
    if valid-handle(chFiles) then RELEASE OBJECT chFiles.
    
END PROCEDURE.
