DEFINE INPUT  PARAMETER pcMailTo  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcSubject AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcBody    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcMailCC  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pcAttach  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cMailTo   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSubject  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBody     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMailCC   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAttach   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chOutlook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chmail    AS COM-HANDLE NO-UNDO.


ASSIGN cMailTo  = pcMailTo
       cSubject = pcSubject
       cBody    = pcBody
       cMailCC  = pcMailCC
       cAttach  = pcAttach.


CREATE "outlook.application.9" choutlook NO-ERROR.
ASSIGN chmail = chOutlook:createItem(0).
/** Assign data **/
ASSIGN chmail:Subject  = cSubject.
       chMail:Body     = cBody.
       chMail:TO       = cMailTo.
       chMail:CC       = cMailCC.
/** example for one attachment: **/
chMail:Attachments:Add(cAttach) NO-ERROR.
chMail:Attachments(1):DisplayName = cAttach.
/** view mail **/
/*chMail:Display(1).*/
chMail:SEND().


/*
RELEASE OBJECT chmail.
RELEASE OBJECT choutlook.
*/


