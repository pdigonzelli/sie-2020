DEFINE TEMP-TABLE ttReglas
    FIELD nro         AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Nro Regla"
    FIELD regla       AS CHARACTER FORMAT "X(30)" COLUMN-LABEL "Regla"
    FIELD valor       AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Valor"
    FIELD antecedente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Antecedente"
    FIELD consecuente AS CHARACTER FORMAT "X(50)" COLUMN-LABEL "Consecuente"  .

RUN pMotorInferencia.p (INPUT-OUTPUT TABLE ttReglas, 
                        "..\industria\reglas.rul",
                        "10166").

FOR EACH ttReglas.
  DISP  regla trim(valor) .
END.
