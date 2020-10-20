DEFINE VARIABLE i AS INTEGER    NO-UNDO.



DEFINE TEMP-TABLE ttConts
  RCODE-INFORMATION
  FIELD id_agencia AS INTEGER 
  FIELD agencias AS CHARACTER
  FIELD contenedores AS INTEGER.
  
FOR EACH orden_entrega
    WHERE orden_entrega.fecha >= DATE('01/04/2006')
      AND (id_agencia = 140 OR
           id_agencia = 145 OR
           id_agencia = 157 OR
           id_agencia = 161 OR
           id_agencia = 163 OR
           id_agencia = 181 OR
           id_agencia = 185 OR 
           id_agencia = 186 OR
           id_agencia = 192 OR
           id_agencia = 197 OR
           id_agencia = 205 OR
           id_agencia = 204 OR
           id_agencia = 203)
    NO-LOCK, 
    EACH items_orden_entrega OF orden_entrega 
    BREAK BY orden_entrega.id_agencia.
    i = i + items_orden_entrega.contenedores.
    IF LAST-OF(id_agencia) THEN DO:
      FIND FIRST agencias WHERE agencia.id_agencia = orden_entrega.id_agencia NO-LOCK NO-ERROR.
      CREATE ttConts.
      ASSIGN ttConts.id_agencia = orden_entrega.id_agencia
             ttConts.agencia = agencias.descripcion
             ttConts.contenedores = i.
      i = 0.
    END.

  

END.


RUN generateExcel.p (INPUT TABLE ttConts,
                        INPUT " Contenedores Maersk",
                        INPUT " Fecha Desde: 01/01/2004"  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).
