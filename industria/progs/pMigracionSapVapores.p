DEFINE TEMP-TABLE ttVapores
  RCODE-INFORMATION
  FIELD id_vapor  AS INTEGER COLUMN-LABEL "Cod Vapor"
  FIELD vapor     AS CHARACTER COLUMN-LABEL "Vapor"
  FIELD abrev     AS CHARACTER  COLUMN-LABEL "Abreviatura".


FOR EACH vapores.
  CREATE ttVapores.
  ASSIGN ttVapores.id_vapor = vapores.id_vapor
         ttVapores.vapor    = vapores.descripcion
         ttVapores.abrev    = vapores.abreviatura.
END.



RUN generateExcel.p (INPUT TABLE ttAgencias,
                        INPUT " Tarifas Agencias",
                        INPUT ""  ,
                        INPUT 7,
                        INPUT 8,
                        INPUT "Arial",
                        INPUT 8).


