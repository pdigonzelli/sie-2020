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

DEFINE INPUT  PARAMETER pwsheet    AS COM-HANDLE  NO-UNDO.
DEFINE INPUT  PARAMETER ptitulo    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER psubtitulo AS CHARACTER  NO-UNDO.

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

 pwsheet:Pictures:INSERT("..\cons\logo.bmp").
 pwsheet:Pictures:TOP = 9.
 pwsheet:Pictures:LEFT = 62.
 pwsheet:Range("B2"):Value = "           CITRICOLA S.A. SAN MIGUEL".
 pwsheet:Range("B2"):Font:colorindex = 10.
 pwsheet:Range("B2"):Font:bold = TRUE.
 
 pwsheet:Range("H2"):Value = "Emision :" + STRING(TODAY,"99/99/99") + " - " + STRING(TIME,"HH:MM:SS").
 pwsheet:Range("H2"):Font:size        = 7.
 pwsheet:Range("H2"):Font:bold = TRUE.


 pwsheet:Range("B3"):Value = ptitulo.
 pwsheet:Range("B3"):Font:colorindex = 2.
 pwsheet:Range("A3:M3"):Font:size        = 13.
 pwsheet:Range("B3:I3"):BorderAround(1,2,1,1).
 pwsheet:Range("B3:I3"):interior:colorindex = 11.
 pwsheet:Range("B3"):Font:bold = TRUE.

 pwsheet:Range("B4"):Value = psubtitulo .
 pwsheet:Range("B4"):Font:colorindex = 11.
 pwsheet:Range("B4"):Font:bold = TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


