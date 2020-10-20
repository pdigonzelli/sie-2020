FUNCTION getFrom RETURNS CHARACTER /*devuelve la direccion de correo y el nombre completo del usuario logueado*/
  (pcUser AS CHARACTER):

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  FIND FIRST usuarios_listas WHERE usuarios_listas.usuario_sistema = pcUser
                             NO-LOCK NO-ERROR.
  IF AVAILABLE usuarios_listas THEN
    cRet = usuarios_listas.email + ";" + usuarios_listas.usuario.
  ELSE 
    cRet = "getfruit@sa-sanmiguel.com; SA San Miguel".

  RETURN cRet.

END FUNCTION.

FUNCTION getFileName RETURNS CHARACTER
  (pcFullPath AS CHARACTER):

  DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRet  AS CHARACTER  NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(pcFullPath, "\"):
    cRet = ENTRY(i, pcFullPath, "\").
  END.

  RETURN cRet.
END FUNCTION.



DEFINE INPUT PARAMETER cProfile   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER iPriority  AS INT  NO-UNDO. /* Low = 0; Normal = 1; High = 2 */
DEFINE INPUT PARAMETER cSubject   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cText      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cTo        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cFiles     AS CHAR NO-UNDO.


DEFINE VARIABLE lSucces AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cFrom   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMime   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFile   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cExt    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i       AS INTEGER    NO-UNDO.

cFrom = getFrom(USERID('userdb')).
/*cFrom = cFrom +  + CHR(1) + "yes,no".*/ /*opcional, solicitud de comprobacion de lectura*/


DO i = 1 TO NUM-ENTRIES(cFiles).
  cFile = ENTRY(i, cFiles).
  cFile = getFileName(cFile).
  cMime = cMime + 
          cFile + ":filetype=binary,".
END.
cMime = SUBSTRING(cMime, 1, LENGTH(cMime) - 1).


RUN smtpmail.p (cFrom, 
                cTo, 
                "",
                cMime, 
                cFiles, 
                cSubject, 
                cText + chr(10) + cMime + CHR(10) + cFiles, 
                "multipart/related",                 
                OUTPUT lSucces).

  



