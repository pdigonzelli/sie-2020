DEFINE VARIABLE cExpression AS CHARACTER NO-UNDO.
DEFINE VARIABLE dResult AS DECIMAL NO-UNDO.
DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
DEFINE VARIABLE cResult AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt NO-UNDO /*dummy TT*/
FIELD f1 AS INTEGER.
DEF QUERY q FOR tt. /* dummy query, but required */






FUNCTION prepareExpression RETURNS CHARACTER
  (pcExp AS CHARACTER).

  DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
  DEFINE VARIABLE i    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cWrd AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cExp AS CHARACTER  NO-UNDO.

  RUN libProceduresReglas.p PERSISTENT SET hLib.

  cExp = pcExp.
  DO i = 1 TO NUM-ENTRIES(pcExp, " "): /*recorro la expresion separando las palabras por un espacion en blanco*/
    cWrd = TRIM(ENTRY(i, pcExp, " ")).

    /*evaluo que no sea un operador logico*/
    CASE cWrd:
      WHEN "AND" THEN.
      WHEN "OR" THEN.
      OTHERWISE DO: /*calculo el valor de verdad del operando y armo una expresion del tipo TRUE AND TRUE AND FALSE ...*/
        cExp = REPLACE(cExp, cWrd, STRING(DYNAMIC-FUNCTION(cWrd IN hLib, '11682'))).
      END.
    END CASE.
  END.

  RETURN cExp.

END FUNCTION.

FUNCTION GetLogical RETURNS LOGICAL
  (INPUT lValue AS LOGICAL).
  lResult = lValue.
  RETURN TRUE.
END FUNCTION.



/*Evaluate a 4GL logical expression */
cExpression = prepareExpression("es_ddp AND es_usa").
QUERY q:QUERY-PREPARE("FOR EACH tt WHERE DYNAMIC-FUNCTION( 'GetLogical' IN THIS-PROCEDURE, " + cExpression + ") = TRUE").
QUERY q:QUERY-OPEN().
QUERY q:QUERY-CLOSE.

MESSAGE lResult VIEW-AS ALERT-BOX INFO BUTTONS OK.

