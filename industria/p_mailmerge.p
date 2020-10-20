DEF INPUT PARAMETER ip_cDocument          AS CHAR.
DEF INPUT PARAMETER ip_cDataFile     AS CHAR.
DEF INPUT PARAMETER ip_cMailMergeDoc AS CHAR.
DEF INPUT PARAMETER iplPrint        AS LOGICAL.
DEFINE INPUT PARAMETER pv_chWord AS COM-HANDLE NO-UNDO.

DEFINE VAR lv_chDocument AS COM-HANDLE NO-UNDO.   

pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* Close all open documents */

ASSIGN  FILE-INFO:FILE-NAME = SEARCH(ip_cDocument)
        ip_cDocument              = FILE-INFO:FULL-PATHNAME
        FILE-INFO:FILE-NAME = SEARCH(ip_cDataFile)
        ip_cDataFile         = FILE-INFO:FULL-PATHNAME.

 ASSIGN lv_chDocument = pv_chWord:Documents:Open(ip_cDocument,NO,YES,YES,,,no,,) NO-ERROR.      /* Try & Open main Document */
 
 IF NOT VALID-HANDLE(lv_chDocument) THEN RETURN "ERROR".                                 /* Bummer */
 
 lv_chDocument:MailMerge:OpenDataSource(ip_cDataFile) NO-ERROR.                         /* Try & Open Data Source */
 
 IF ERROR-STATUS:ERROR THEN                                                         /* Bummer */
 DO: 
  RELEASE OBJECT lv_chDocument.
  RETURN "ERROR".
 END.

 ASSIGN lv_chDocument:MailMerge:Destination = 0.                                       /* Merge to New Document */
  
 lv_chDocument:MailMerge:Execute() NO-ERROR.                                           /* Perform the Merge */

 lv_chDocument:Close(NO).                                                              /* Close main document */
 
 RELEASE OBJECT lv_chDocument.                                                         /* Release document object */
 
 ASSIGN lv_chDocument = pv_chWord:Documents:Item(1) NO-ERROR.                             /* Get new mail merge document */
  
 IF NOT VALID-HANDLE(lv_chDocument) OR ERROR-STATUS:ERROR THEN RETURN "ERROR".              /* Bummer */
  
 lv_chDocument:SaveAs(ip_cMailMergeDoc).                                                /* Save new mail merge document */
 
 IF iplPrint THEN lv_chDocument:PrintOut(YES).                                         /* print new document if required */

 RELEASE OBJECT lv_chDocument NO-ERROR.                                                /* Release document object */
 
 /* pv_chWord:Documents:Close(NO) NO-ERROR.                                               /* close all documents */ */
