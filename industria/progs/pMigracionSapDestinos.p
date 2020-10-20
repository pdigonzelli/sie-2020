DEFINE TEMP-TABLE ttDestinos
  RCODE-INFORMATION 
  FIELD grupo       AS CHARACTER COLUMN-LABEL "Grupo Destino"
  FIELD id_destino  AS INTEGER COLUMN-LABEL "Cod Destino"
  FIELD destino     AS CHARACTER COLUMN-LABEL "Destino"
  FIELD abrev       AS CHARACTER COLUMN-LABEL "Abreviatura".



FOR EACH destinos, 
    FIRST destinos_grupo OF destinos
    NO-LOCK.

  CREATE ttDestinos.
  ASSIGN ttDestinos.grupo       = destinos_grupo.descripcion
         ttDestinos.id_destino  = destinos.id_destino
         ttDestinos.destino     = destinos.descripcion
         ttDestinos.abrev       = destinos.abreviatura
         .

END.




RUN generateExcel.p (INPUT TABLE ttDestinos,
                        INPUT " Destinos",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).
