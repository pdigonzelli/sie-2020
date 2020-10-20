DEFINE TEMP-TABLE ttReglas
    FIELD nro         AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Nro Regla"
    FIELD regla       AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Regla"
    FIELD valor       AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Valor"
    FIELD antecedente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Antecedente"
    FIELD consecuente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Consecuente"  .

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttReglas.
DEFINE INPUT PARAMETER pcArchivoReglas AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcArgs          AS CHARACTER NO-UNDO.


DEFINE VARIABLE cReg AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cAnt AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cCon AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cRun AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cNro AS CHARACTER FORMAT "X(3)"  NO-UNDO.
DEFINE VARIABLE cNom AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE cOp  AS CHARACTER                NO-UNDO.
DEFINE VARIABLE cTxt AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cOp1 AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cOp2 AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cRul AS CHARACTER FORMAT "X(70)" NO-UNDO.
DEFINE VARIABLE cAux AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.

DEFINE VARIABLE iPos AS INTEGER    NO-UNDO.
DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
DEFINE VARIABLE lFun AS LOGICAL    NO-UNDO.

DEFINE VARIABLE dCon AS DECIMAL    NO-UNDO.

RUN libProceduresReglas.p PERSISTENT SET hLib.
FOR EACH ttReglas. 
  DELETE ttReglas.
END.

INPUT FROM VALUE(pcArchivoReglas).
REPEAT :
  IMPORT UNFORMATTED cReg.
  cAux = cReg.
  IF LENGTH(cReg) = 0 THEN DO:  
    /*continua si encuentra una linea en blanco*/
    NEXT.
  END.
  iPos = INDEX(cReg, "#").
  IF iPos > 0 THEN DO:        
    /*continuo con la regla siguiente si esta el caracter de comentario #*/
    NEXT.
  END.
  

  /*extraigo nro*/
  cNro = ENTRY(1, cReg, "-").
  cReg = ENTRY(2, cReg, "-").
  /*extraigo nombre*/
  cNom = ENTRY(1, cReg, ";").
  cReg = ENTRY(2, cReg, ";").
  /*extraigo antecedente y consecuente*/
  cAnt = TRIM(ENTRY(1, cReg, ">")).
  cCon = TRIM(ENTRY(2, cReg, ">")).
  
  /*leo el antecedente y armo una lista entry separada por coma de operadores y operandos*/  
  cTxt = cAnt.
  DO WHILE TRUE:    
    /* Find the next operator character. If none found, find
    the end of string. */
    iPos = INDEX(cTxt, " AND ").
    IF iPos = 0 THEN DO:
      iPos = INDEX(cTxt, " OR  ").
      IF iPos = 0 THEN DO:
        iPos = INDEX(cTxt, " NOT ").
        IF iPos = 0 THEN 
          iPos = LENGTH(cTxt) + 1.
      END.
    END.
    /* If the string contains no (more) words, then read the 
    next line. */
    IF iPos = 1 THEN 
      LEAVE.
    
    /* Pull the first word off the string.
    Remove any punctuation characters around it. */
    cRun = SUBSTRING(cTxt, 1, iPos - 1).
    cRun = TRIM(cRun, ",.;:!? ~"~ ’[]").
    cOp  = TRIM(SUBSTRING(cTxt, iPos, 4)).
    cTxt = TRIM(SUBSTRING(cTxt, iPos + 5)).
    cRul = cRul + "," + cRun + "," + cOp.
  END.
  cRul = SUBSTRING(cRul, 2, LENGTH(cRul)).
  cRul = SUBSTRING(cRul, 1, LENGTH(cRul) - 1).
  
  
  /*opero el antecedente para calcular su valor*/
  i = 1.
  DO WHILE i <= NUM-ENTRIES(cRul):
    cOp2 = ENTRY(i, cRul).
    CASE cOp2:
      WHEN "AND" THEN DO:
        lFun = lFun AND DYNAMIC-FUNCTION(ENTRY(i + 1, cRul) IN hLib, pcArgs) NO-ERROR.      
        i = i + 1.
      END.
      WHEN "OR" THEN DO:
        lFun = lFun OR DYNAMIC-FUNCTION(ENTRY(i + 1, cRul) IN hLib, pcArgs) NO-ERROR.      
        i = i + 1.
      END.
      OTHERWISE DO:
        lFun = DYNAMIC-FUNCTION(cOp2 IN hLib, pcArgs) NO-ERROR.
      END.
    END CASE.
    i = i + 1.
  END.

  IF lFun AND LENGTH(cCon) > 0 THEN DO: /*ejecutrar consecuente si antecedente es verdadero*/
    RUN VALUE(cCon) IN hLib (pcArgs, OUTPUT dCon) NO-ERROR.
    /*DISP cNro cNom vdCon.*/
    CREATE ttReglas.
    ASSIGN ttReglas.nro         = cNro
           ttReglas.regla       = cNom
           ttReglas.valor       = STRING(dCon, ">>>>>>99.99")
           ttReglas.antecedente = cAnt
           ttReglas.consecuente = cCon.
  END.
  
END.
  
INPUT CLOSE.



   

