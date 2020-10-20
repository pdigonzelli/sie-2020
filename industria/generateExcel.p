&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR tth-table AS HANDLE.
DEFINE INPUT PARAMETER TABLE-HANDLE FOR tth-table.
DEFINE INPUT  PARAMETER ptitle      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER psubtitle   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pfonttitles AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER pfontdata   AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER pfontname   AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER prowtitles  AS INTEGER    NO-UNDO.


DEFINE TEMP-TABLE tt-letras
    FIELD idx AS INTEGER
    FIELD letra AS CHAR.

{SetVarExcel.i}
{funcionesExcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


RUN exportToExcel.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-exportToExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportToExcel Procedure 
PROCEDURE exportToExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE vletra       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE btth AS HANDLE     NO-UNDO.
DEFINE VARIABLE qtth AS HANDLE     NO-UNDO.
DEFINE VARIABLE fh   AS HANDLE     NO-UNDO.
DEFINE VARIABLE i   AS INTEGER    NO-UNDO.

  {SetInitializeExcel.i}
  
  RUN setConfig.
  RUN SetHeaderExcel.p (INPUT chWorkSheet,
                        INPUT ptitle,
                        INPUT psubtitle).

  RUN SetTitleExcel.

  btth = tth-table:DEFAULT-BUFFER-HANDLE.

  
  CREATE QUERY qtth.
  qtth:SET-BUFFERS(btth).
  qtth:QUERY-PREPARE("for each "  + btth:TABLE).
  qtth:QUERY-OPEN.
 

 ifila = prowtitles + 2.

 REPEAT:
  qtth:GET-NEXT.
  IF qtth:QUERY-OFF-END THEN LEAVE. 

      REPEAT i = 1 TO btth:NUM-FIELDS:
          fh = btth:BUFFER-FIELD(i).
          cfila = STRING(ifila).
          crange = getColumnLetter(i) + cfila.
          /*chWorkSheet:Range(crange):value = "'" + fh:BUFFER-VALUE. /*by facundo 10/06/2005 - agregue el apostrofe para que el excel vea todo como texto y no formatee las celdas como se le cante el pingo*/*/
          chWorkSheet:Range(crange):VALUE = fh:BUFFER-VALUE. 
      END.

  ifila = ifila + 1.
END.

  {SetFooterExcel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setConfig) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setConfig Procedure 
PROCEDURE setConfig :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

chWorkSheet:Range("A" + STRING(prowtitles) + ":DZ" + STRING(prowtitles)):FONT:SIZE = pfonttitles.
chWorkSheet:Range("A" + STRING(prowtitles) + ":DZ" + STRING(prowtitles)):FONT:Bold = TRUE.
chWorkSheet:Range("A1:DZ10000"):FONT:NAME = pfontname.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTitleExcel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTitleExcel Procedure 
PROCEDURE setTitleExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE btth AS HANDLE     NO-UNDO.
DEFINE VARIABLE qtth AS HANDLE     NO-UNDO.
DEFINE VARIABLE fh   AS HANDLE     NO-UNDO.
DEFINE VARIABLE i   AS INTEGER    NO-UNDO.


btth = tth-table:DEFAULT-BUFFER-HANDLE.

/*
ASSIGN 
   ColumnRange = CHR(65) + ":" + CHR(65 + hBufferHandle:NUM-FIELDS - 
1).*/

/*
chWorkSheet:COLUMNS("A:AA"):SELECT.
chWorkSheet:COLUMNS:AUTOFIT().
  */


DO i = 1 TO btth:NUM-FIELDS:
    fh = btth:BUFFER-FIELD(i).
    
    chWorkSheet:Range(getColumnLetter(i) + STRING(prowtitles)):Value = fh:COLUMN-LABEL.
    /*chWorkSheet:COLUMNS(tt-letras.letra):AutoFit().*/
    chWorkSheet:Range(getColumnLetter(i) + STRING(prowtitles)):BorderAround(1,2,1,1).
    chWorkSheet:Range(getColumnLetter(i) + STRING(prowtitles)):HorizontalAlignment = 3.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

