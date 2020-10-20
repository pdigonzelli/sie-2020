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
DEFINE INPUT  PARAMETER pwsheet   AS COM-HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pposicion AS INTEGER    NO-UNDO.
DEFINE INPUT  PARAMETER ptitulos  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER panchos   AS CHARACTER  NO-UNDO.

DEFINE VARIABLE i AS INTEGER    NO-UNDO.

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
 DEFINE VARIABLE letra   AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE prefijo AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE idx AS INTEGER    NO-UNDO.

 prefijo = "".
 idx = 1.
 do i = 1 to num-entries(ptitulos):
   
   letra = CHR(97 + idx).
   pwsheet:Range(prefijo + letra + string(pposicion)):Value = entry(i,ptitulos,",").
   pwsheet:Range(prefijo + letra + string(pposicion)):BorderAround(1,2,1,1).
   IF panchos <> "" THEN
    pwsheet:Columns(prefijo + letra):ColumnWidth = integer(entry(i,panchos,",")).
   
   IF letra = "z" THEN DO:
        prefijo = (IF prefijo = "" THEN CHR(96) ELSE prefijo).
        prefijo = CHR(ASC(prefijo) + 1).
        idx = 0.
   END.
   ELSE
     idx = idx + 1.
 end.
 
  
 pwsheet:Range("A" + string(pposicion) + ":CA" + STRING(pposicion)):horizontalalignment = 3.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


