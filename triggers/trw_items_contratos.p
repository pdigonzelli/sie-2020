TRIGGER PROCEDURE FOR WRITE OF items_contratos.
DEFINE BUFFER b_distribucion FOR distribucion_contratos.
DEFINE VAR i AS INTEGER.

if available items_contratos Then
  do:
    i = 23. 
    DO WHILE i <= 41 :
    
    /* Crea distribucion_contratos */
     
    FIND FIRST b_distribucion OF items_contratos WHERE b_distribucion.semana = i NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b_distribucion THEN
     DO:
        CREATE b_distribucion.
        BUFFER-COPY items_contratos TO b_distribucion.
        ASSIGN b_distribucion.semana = i.
        i = i + 1.
     END.
    END.
     
    

  end.
