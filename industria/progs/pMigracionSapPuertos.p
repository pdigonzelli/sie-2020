DEFINE TEMP-TABLE ttPuertos
  RCODE-INFORMATION
  FIELD id_puerto  AS INTEGER COLUMN-LABEL "Cod Puerto"
  FIELD puerto     AS CHARACTER COLUMN-LABEL "Puerto"
  FIELD abrev     AS CHARACTER  COLUMN-LABEL "Abreviatura".


FOR EACH puertos.
  CREATE ttPuertos.
  ASSIGN ttPuertos.id_puerto = puertos.id_puerto
         ttPuertos.puerto    = puertos.nombre
         ttPuertos.abrev    = puertos.abreviatura.
END.



RUN generateExcel.p (INPUT TABLE ttPuertos,
                        INPUT " Puertos",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).


