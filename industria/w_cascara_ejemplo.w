&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BUTTON-14 BUTTON-15 BUTTON-12 ~
BUTTON-13 BUTTON-16 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Normal" 
     SIZE 17 BY 1.43.

DEFINE BUTTON BUTTON-12 
     LABEL "Amontonado" 
     SIZE 17 BY 1.43.

DEFINE BUTTON BUTTON-13 
     LABEL "No Cabecera" 
     SIZE 17 BY 1.43.

DEFINE BUTTON BUTTON-14 
     LABEL "No cabecera arriba" 
     SIZE 20 BY 1.14.

DEFINE BUTTON BUTTON-15 
     LABEL "Bidones Pepsi" 
     SIZE 20 BY 1.43.

DEFINE BUTTON BUTTON-16 
     LABEL "Bidones Pepsi II" 
     SIZE 20 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.1 COL 1
     BUTTON-14 AT ROW 1.19 COL 21
     BUTTON-15 AT ROW 2.62 COL 21
     BUTTON-12 AT ROW 2.71 COL 1
     BUTTON-13 AT ROW 4.33 COL 1
     BUTTON-16 AT ROW 4.33 COL 21
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 42 BY 4.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 4.86
         WIDTH              = 41.2
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Normal */
DO:
  
        output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO720,20^GB3,1200,3^FS".                
        put control "^FO725,520^A0R,45,70^FD" "S.A. SAN MIGUEL" "^FS".
        put control "^FO680,20^GB3,1200,3^FS".
        put control "^FO688,296^A0R,25,30^FD" "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria,  y Financiera" "^FS".
        put control "^FO648,672^A0R,25,30^FD" "Administracion y Planta Industrial 1" "^FS".
        put control "^FO616,336^A0R,25,30^FD" "Lavalle 4001- C.C. 240(4000) S.M. de TUCUMAN - ARGENTINA." "^FS".
        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A.23032189 - R.N.E.23000573" "^FS".
        put control "^FO578,20^GB3,1200,3^FS".
        put control "^FO510,32^A0R,50,60^FD" "COD. PRODUCTO" "^FS".
        put control "^FO510,710^A0R,50,50^FD" "511" "^FS".
        
        put control "^FO410,32^A0R,50,60^FD" "DESCRIPCION" "^FS".
        put control "^FO410,510^A0R,50,50^FD" "CASCARA DESHIDRATADA" "^FS".
        
        put control "^FO310,32^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO310,710^A0R,50,50^FD" "10/09/2000" "^FS".
                
        put control "^FO210,32^A0R,50,60^FD" "NRO. PALLET" "^FS".
        put control "^FO210,735^A0R,50,50^FD" "19457" "^FS".
        
        put control "^FO120,32^A0R,50,60^FD" "NRO. PLANTA INDUSTRIAL" "^FS".
        put control "^FO120,710^A0R,50,50^FD" "1" "^FS".
  
        put control "^FO10,200^A0R,^BY3^B3R,N,100,N,N^FD" "07" + "1" + "096" + "02546" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close. 
  
  
  
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 W-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* Amontonado */
DO:
  output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO720,20^GB3,1200,3^FS".                
        put control "^FO725,520^A0R,45,70^FD" "S.A. SAN MIGUEL" "^FS".
        put control "^FO680,20^GB3,1200,3^FS".
        put control "^FO688,296^A0R,25,30^FD" "Agricola, Ganadera, Industrial, Comercial, Inmobiliaria,  y Financiera" "^FS".
        put control "^FO648,672^A0R,25,30^FD" "Administracion y Planta Industrial 1" "^FS".
        put control "^FO616,336^A0R,25,30^FD" "Lavalle 4001- C.C. 240(4000) S.M. de TUCUMAN - ARGENTINA." "^FS".
        put control "^FO584,184^A0R,25,30^FD" "Tel: (0381)4512600 - Fax: (0381)4512612 - R.N.P.A.23032189 - R.N.E.23000573" "^FS".
        put control "^FO578,20^GB3,1200,3^FS".
        put control "^FO510,32^A0R,50,60^FD" "COD. PRODUCTO" "^FS".
        put control "^FO510,710^A0R,50,50^FD" "511" "^FS".
        
        put control "^FO460,32^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO460,710^A0R,50,50^FD" "10/09/2000" "^FS".
        
        put control "^FO410,32^A0R,50,60^FD" "NRO. PALLET" "^FS".
        put control "^FO410,710^A0R,50,50^FD" "19457" "^FS".
                
        put control "^FO360,32^A0R,50,60^FD" "NRO. PLANTA INDUSTRIAL" "^FS".
        put control "^FO360,735^A0R,50,50^FD" "1" "^FS".
        
        put control "^FO310,32^A0R,50,60^FD" "NRO. LOTE" "^FS".
        put control "^FO310,710^A0R,50,50^FD" "2546" "^FS".
  
        put control "^FO210,200^A0R,^BY3^B3R,N,100,N,N^FD" "07" + "1" + "096" + "02546" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 W-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* No Cabecera */
DO:
  output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO510,200^A0R,50,60^FD" "COD. PRODUCTO" "^FS".
        put control "^FO510,900^A0R,50,50^FD" "511" "^FS".
        
        put control "^FO460,200^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO460,900^A0R,50,50^FD" "10/09/2000" "^FS".
        
        put control "^FO410,200^A0R,50,60^FD" "NRO. PALLET" "^FS".
        put control "^FO410,900^A0R,50,50^FD" "19457" "^FS".
                
        put control "^FO360,200^A0R,50,60^FD" "NRO. PLANTA INDUSTRIAL" "^FS".
        put control "^FO360,935^A0R,50,50^FD" "1" "^FS".
        
        put control "^FO310,200^A0R,50,60^FD" "NRO. LOTE" "^FS".
        put control "^FO310,900^A0R,50,50^FD" "2546" "^FS".
  
        put control "^FO210,200^A0R,^BY3^B3R,N,100,N,N^FD" "07" + "1" + "096" + "02546" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 W-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* No cabecera arriba */
DO:
  output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO710,200^A0R,50,60^FD" "COD. PRODUCTO" "^FS".
        put control "^FO710,900^A0R,50,50^FD" "511" "^FS".
        
        put control "^FO660,200^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO660,900^A0R,50,50^FD" "10/09/2000" "^FS".
        
        put control "^FO610,200^A0R,50,60^FD" "NRO. PALLET" "^FS".
        put control "^FO610,900^A0R,50,50^FD" "19457" "^FS".
                
        put control "^FO560,200^A0R,50,60^FD" "NRO. PLANTA INDUSTRIAL" "^FS".
        put control "^FO560,935^A0R,50,50^FD" "1" "^FS".
        
        put control "^FO510,200^A0R,50,60^FD" "NRO. LOTE" "^FS".
        put control "^FO510,900^A0R,50,50^FD" "2546" "^FS".
  
        put control "^FO410,200^A0R,^BY3^B3R,N,100,N,N^FD" "07" + "1" + "096" + "02546" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Bidones Pepsi */
DO:
  output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO710,200^A0R,50,60^FD" "NRO. LOTE" "^FS".
        put control "^FO710,900^A0R,50,50^FD" "010/00" "^FS".
        
        put control "^FO660,200^A0R,50,60^FD" "NRO. BIDON" "^FS".
        put control "^FO660,900^A0R,50,50^FD" "01/70" "^FS".
        
        put control "^FO610,200^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO610,900^A0R,50,50^FD" "10/09/2000" "^FS".
                
        put control "^FO510,200^A0R,^BY3^B3R,N,100,N,N^FD" "08" + "1" + "096" + "00010" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Bidones Pepsi II */
DO:
  output to c:\lpt1.  
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
        
        put control "^FO710,60^A0R,50,60^FD" "NRO. LOTE" "^FS".
        put control "^FO710,400^A0R,50,50^FD" "1010/00" "^FS".
        
        put control "^FO710,700^A0R,50,60^FD" "NRO. BIDON" "^FS".
        put control "^FO710,1050^A0R,50,50^FD" "01/70" "^FS".
        
        put control "^FO660,60^A0R,50,60^FD" "FECHA ELABORACION" "^FS".
        put control "^FO660,760^A0R,50,50^FD" "10/09/2000" "^FS".
                
        put control "^FO560,200^A0R,^BY3^B3R,N,100,N,N^FD" "08" + "1" + "096" + "00010" + "999" "^FS".
        
        
        
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BUTTON-1 BUTTON-14 BUTTON-15 BUTTON-12 BUTTON-13 BUTTON-16 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


