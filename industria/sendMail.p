FUNCTION getFrom RETURNS CHARACTER /*devuelve la direccion de correo y el nombre completo del usuario logueado*/
  (pcUser AS CHARACTER):

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
  
  FIND FIRST usuarios_listas WHERE LENGTH(usuarios_listas.usuario_sistema) > 0
                               AND usuarios_listas.usuario_sistema         = pcUser
                             NO-LOCK NO-ERROR.
  IF AVAILABLE usuarios_listas THEN
    cRet = usuarios_listas.email /*+ ";" + usuarios_listas.usuario*/.
  ELSE 
    cRet = "facundoj@sa-sanmiguel.com; Ing Facundo Juarez".

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

FUNCTION getApplication RETURNS CHARACTER
  (pcExt AS CHARACTER):

  DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.

  CASE pcExt:
    WHEN "pdf" THEN cRet = ":type=application/pdf".
    WHEN "doc" THEN cRet = ":type=application/msword".
    OTHERWISE cRet = "".
  END CASE.

  RETURN cRet.

END FUNCTION.



DEFINE INPUT PARAMETER cProfile   AS CHAR NO-UNDO. /*para mantener compatibilidad con sendMail.p anterior*/
DEFINE INPUT PARAMETER iPriority  AS INT  NO-UNDO. /* Low = 0; Normal = 1; High = 2 */
DEFINE INPUT PARAMETER cSubject   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cText      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cTo        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER cFiles     AS CHAR NO-UNDO.


DEFINE VARIABLE lSuccess AS LOGICAL    NO-UNDO.
DEFINE VARIABLE cFrom    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMime    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFile    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cExt     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cApp     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cError   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCC      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i        AS INTEGER    NO-UNDO.
DEFINE VARIABLE j        AS INTEGER    NO-UNDO.


cFrom = getFrom(USERID('userdb')).
cTo   = cTo + "," + ENTRY(1, cFrom, ";").  /*para que envie una copia al usuario logueado*/

/*cFrom = cFrom +  CHR(1) + "yes,no".*/ /*opcional, solicitud de comprobacion de lectura*/

/*armo la cadena MIME para los attach*/
DO i = 1 TO NUM-ENTRIES(cFiles).
  cFile = ENTRY(i, cFiles).
  cFile = getFileName(cFile).
  cExt  = ENTRY(2, cFile, ".").
  cApp  = getApplication(cExt).
  
  cMime = cMime + 
          cFile + cApp + ":filetype=binary,".

END.
cMime = SUBSTRING(cMime, 1, LENGTH(cMime) - 1).


/*pongo 1 solo destinatario en pTo y el resto en pCC*/

DO i = 2 TO NUM-ENTRIES(cTo):
  cCC = ENTRY(i, cTo) + "," + cCC.
END.

cCC = SUBSTRING(cCC, 1, LENGTH(cCC) - 1).
cTo = ENTRY(1, cTo).



/*mail por socket*/

RUN ..\industria\smtpmail.p (cFrom, 
                cTo,
                cCC,
                cMime, 
                cFiles, 
                cSubject, 
                cText, 
                "",                 
                OUTPUT lSuccess).

/*delay*/
DO i = 1 TO 60000:
  j = j + 1.
END.  
  
/*  
IF NOT lSuccess THEN 
    MESSAGE "Ocurrio un Problema con el envio del correo a alguna de estas direcciones " + cTo
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
*/


/* comentado 23/08/2006, funciona bien, pero la performane es muy lenta, no soluciona
/*by facundo 22/08/2006, recorro la lista cTo y hago un envio por cada direccion */
DEFINE VARIABLE cAddress AS CHARACTER  NO-UNDO.

DO i = 1 TO NUM-ENTRIES(cTo):
  
  cAddress = TRIM(ENTRY(i, cTo)).
  /*cAddress = cAddress + CHR(1) + "yes,no".*/  /*pide confirmacion de lectura*/
  
  RUN ..\industria\smtpmail.p (cFrom, 
                               cAddress,
                               "",
                               cMime, 
                               cFiles, 
                               cSubject, 
                               cText, 
                               "",                 
                               OUTPUT lSuccess).

  IF NOT lSuccess THEN 
    MESSAGE "Ocurrio un Problema con el envio del correo a " + cAddress
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.


END.
/*end by facundo*/
*/

/*
RUN smtpmailv2.p (cTo, 
                  cFrom,
                  "", 
                  cMime, 
                  cFiles,
                  cSubject,
                  cText, 
                  "", 
                  "text", 
                  OUTPUT lSuccess, 
                  OUTPUT cError).
 */
/*"type=text/html:charset=usascii:multipart/related"*/

/*"type=text/html:charset=usascii:multipart/related"*/
  
