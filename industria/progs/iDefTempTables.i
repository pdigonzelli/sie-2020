DEFINE TEMP-TABLE ttElabs
  RCODE-INFORMATION
  FIELD codigo_sap  AS CHARACTER COLUMN-LABEL "Codigo Sap"
  FIELD descripcion AS CHARACTER COLUMN-LABEL "Descripcion"
  FIELD flag        AS CHARACTER COLUMN-LABEL "Flag"
  FIELD codigo_prog AS CHARACTER COLUMN-LABEL "Codigo Progress"
  .

DEFINE TEMP-TABLE ttMat
  NO-UNDO
  LIKE BATCH_input_materiales_sap
  RCODE-INFORMATION
  .

DEFINE TEMP-TABLE ttCli
  NO-UNDO
  LIKE BATCH_input_clientes_sap
  RCODE-INFORMATION
  .
