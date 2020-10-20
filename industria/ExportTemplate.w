&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

 /*-- VARIABLES POR DEFECTO --*/
    DEFINE VAR VDESDE   AS CHAR.
    DEFINE VAR VHASTA   AS CHAR.
    DEFINE VAR VFILTRO1 AS INTEGER.
    DEFINE VAR VFILTRO2 AS INTEGER.
    DEFINE VAR VFILTRO3 AS INTEGER.
 /*-- VARIABLES POR DEFECTO --*/

 /*-- VARIABLES DE EXCEL --*/

    define var chExcelAplication as com-handle.
    define var chWorkbook        as com-handle.
    define var chWorkSheet       as com-handle.
    define var chchart           as com-handle.
    define var chWorkSheetRange  as com-handle.
  
    define var ifila  as integer.
    define var cfila  as character.
    define var crange as character.

 /*-- FIN VARIABLES DE EXCEL --*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-3 FILL-fecha_desde Btn_Cancel ~
FILL-fecha_hasta TOGGLE-2 FILL-1 TOGGLE-3 FILL-2 RADIO-SET-1 
&Scoped-Define DISPLAYED-OBJECTS FILL-fecha_desde FILL-fecha_hasta TOGGLE-2 ~
FILL-1 fi-des1 TOGGLE-3 FILL-2 fi-des2 RADIO-SET-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.19
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-3 
     LABEL "Exportacion" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi-des1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .95
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-des2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FILL-1 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Filtro 1" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .95 NO-UNDO.

DEFINE VARIABLE FILL-2 AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Filtro 2" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-fecha_desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE FILL-fecha_hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .95 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Opcion 1", 1,
"Opcion 2", 2,
"Opcion", 3
     SIZE 31 BY 3.1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 4.05.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "Todos" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .71 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BUTTON-3 AT ROW 1.71 COL 62
     FILL-fecha_desde AT ROW 2.67 COL 19 COLON-ALIGNED
     Btn_Cancel AT ROW 3.14 COL 62
     FILL-fecha_hasta AT ROW 3.86 COL 19 COLON-ALIGNED
     TOGGLE-2 AT ROW 6.24 COL 17
     FILL-1 AT ROW 7.19 COL 15 COLON-ALIGNED
     fi-des1 AT ROW 7.19 COL 27 COLON-ALIGNED NO-LABEL
     TOGGLE-3 AT ROW 8.38 COL 17
     FILL-2 AT ROW 9.33 COL 15 COLON-ALIGNED
     fi-des2 AT ROW 9.33 COL 27 COLON-ALIGNED NO-LABEL
     RADIO-SET-1 AT ROW 11.24 COL 7 NO-LABEL
     RECT-1 AT ROW 10.76 COL 5
     "  Periodo" VIEW-AS TEXT
          SIZE 45 BY .71 AT ROW 1.71 COL 5
          BGCOLOR 1 FGCOLOR 15 
     "  Filtro" VIEW-AS TEXT
          SIZE 45 BY .71 AT ROW 5.29 COL 5
          BGCOLOR 1 FGCOLOR 15 
     SPACE(29.39) SKIP(9.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<Titulo>"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-des1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-des2 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* <Titulo> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 D-Dialog
ON CHOOSE OF BUTTON-3 IN FRAME D-Dialog /* Exportacion */
DO:
  RUN EXPORTACION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-1 D-Dialog
ON LEAVE OF FILL-1 IN FRAME D-Dialog /* Filtro 1 */
DO:
  find clientes where clientes.id_cliente = integer(self:screen-value in frame {&FRAME-NAME}) no-lock no-error.
  if available clientes then
    f-descripcion:screen-value in frame {&FRAME-NAME} = clientes.razon_social.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-1 D-Dialog
ON MOUSE-SELECT-DBLCLICK OF FILL-1 IN FRAME D-Dialog /* Filtro 1 */
DO:
  define var r as rowid.
  
  run ..\cons\c_clientes02.w (output r).
  if r <> ? then
   do:
     find clientes where rowid(clientes) = r no-lock no-error.
     if available clientes then
      do:
       fill-cliente:screen-value in frame {&FRAME-NAME} = string(clientes.id_cliente).
       f-descripcion:screen-value in frame {&FRAME-NAME} = clientes.razon_social.
      end. 
   end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-2 D-Dialog
ON LEAVE OF FILL-2 IN FRAME D-Dialog /* Filtro 2 */
DO:
  find tipocomp where tipocomp.id_tipocomp = integer(self:screen-value) no-lock no-error.
  if available tipocomp then 
    do:
      fi-banco:screen-value  = string(tipocomp.id_tipocomp).
      fi-des:screen-value    = tipocomp.descripcion.
    end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-2 D-Dialog
ON MOUSE-SELECT-DBLCLICK OF FILL-2 IN FRAME D-Dialog /* Filtro 2 */
DO:
  define var r as rowid no-undo.
  run ..\cons\c_j_medios_pago.w (output r).
  find tipocomp where rowid(tipocomp) = r no-lock no-error.
  if available tipocomp then 
    do:
      fi-banco:screen-value = string(tipocomp.id_tipocomp).
      fi-des:screen-value    = tipocomp.descripcion.
    end.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-2 D-Dialog
ON VALUE-CHANGED OF TOGGLE-2 IN FRAME D-Dialog /* Todos */
DO:
  if TOGGLE-2:screen-value = "yes" then
     FILL-cliente:sensitive = false.
  else
     FILL-cliente:sensitive = true.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-3 D-Dialog
ON VALUE-CHANGED OF TOGGLE-3 IN FRAME D-Dialog /* Todos */
DO:
   if TOGGLE-3:screen-value = "yes" then
     fi-banco:sensitive = false.
  else
     fi-banco:sensitive = true.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog _DEFAULT-ENABLE
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
  DISPLAY FILL-fecha_desde FILL-fecha_hasta TOGGLE-2 FILL-1 fi-des1 TOGGLE-3 
          FILL-2 fi-des2 RADIO-SET-1 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 BUTTON-3 FILL-fecha_desde Btn_Cancel FILL-fecha_hasta TOGGLE-2 
         FILL-1 TOGGLE-3 FILL-2 RADIO-SET-1 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EXPORTACION D-Dialog 
PROCEDURE EXPORTACION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /*-- CAPTURA DE DATOS DE PANTALLA --
 
    VDESDE = FILL-fecha_desde:screen-value in frame {&FRAME-NAME}.
    VHASTA = FILL-fecha_hasta:screen-value in frame {&FRAME-NAME}.
  
    VFILTRO1 = integer(FILL-1:screen-value in frame {&FRAME-NAME}).
    VFILTRO2 = integer(FILL-2:screen-value in frame {&FRAME-NAME}).
  
    CASE RADIO-SET-1:screen-value:
       WHEN "1" THEN
          VFILTRO3 = <VALOR>.
       WHEN "2" THEN
          VFILTRO3 = <VALOR>.
       WHEN "3" THEN
          VFILTRO3 = <VALOR>.
    END CASE.   

  -- CAPTURA DE DATOS DE PANTALLA --*/

  
 /*-- CONFIGURACION INICIAL --*/

    create "Excel.Application" chExcelAplication.
    chExcelAplication:visible = true.
    chWorkbook  = chExcelAplication:Workbooks:add().
    chWorkSheet = chExcelAplication:Sheets:Item(1).
    
    /* Formato del titulo general */
    chWorkSheet:Range("A1:AP6"):Font:Bold           = true.
    chWorkSheet:Range("A1:AP1900"):Font:size        = 8.
    chWorkSheet:Range("A6:AP6"):HorizontalAlignment = 3.
    
    /* Ancho de las columnas */
    chWorkSheet:Columns("A"):ColumnWidth = 25.
    chWorkSheet:Columns("B"):ColumnWidth = 25.

 /*-- FIN CONFIGURACION INICIAL --*/
 
 /*-- TITULOS GENERALES Y PARTICULARES --

  chWorkSheet:Range("B2"):Value = "          Fecha Desde : " + VDESDE + " - Fecha Hasta :" + VHASTA .
  chWorkSheet:Range("B3"):Font:colorindex = 3.
  
  chWorkSheet:Range("A6"):Value = <TITULO>.
  chWorkSheet:Range("A6"):BorderAround(1,2,1,1).
  
  chWorkSheet:Range("B6"):Value = <TITULO>.
  chWorkSheet:Range("B6"):BorderAround(1,2,1,1).
  
  chWorkSheet:Range("C6"):Value = <TITULO>.
  chWorkSheet:Range("C6"):BorderAround(1,2,1,1).
  
  chWorkSheet:Range("D6"):Value = <TITULO>.
  chWorkSheet:Range("D6"):BorderAround(1,2,1,1).
  
  -- FIN TITULOS GENERALES Y PARTICULARES --*/

  /*------------------------------------ PROCESO PRINCIPAL ---------------------------------------
  ifila = 9.
  FOR EACH <TABLA> WHERE <CRITERIO> NO-LOCK 
                   BREAK BY <BREAK CAMPO> :
            
        /*>>> Cabecera de Corte de Control */          
            IF FIRST-OF(<BREAK CAMPO>) THEN
              DO:
                ifila  = ifila + 1.
                cfila  = string(ifila).
                cRange = "A" + cfila.
                
                /*Titulo*/
                chWorkSheet:Range(crange):value = "Banco : (" + string(tipocomp.id_tipocomp) + ") " + tipocomp.descripcion.
                
                /*Formato Titulo*/
                chWorkSheet:range(crange):font:bold       = true.
                chWorkSheet:range(crange):font:size       = 10.
                chWorkSheet:range(crange):font:colorindex = 9.
    
                ifila = ifila + 2.
              END.
              
        /*>>> Variables Acumuladoras     */
            v_acum1 = t_acum2 + <CAMPO>.
            v_acum2 = t_acum2 + <CAMPO>.
            
            t_acum1 = t_acum2 + <CAMPO>.
            t_acum2 = t_acum2 + <CAMPO>.

        /*>>> Datos */
            cfila  = string(ifila).
            cRange = "B" + cfila.
            chWorkSheet:Range(crange):value = <CAMPO>.
            cRange = "C" + cfila.
            chWorkSheet:Range(crange):value = <CAMPO>.

        /*>>> Pie de Corte de Control          */
            IF LAST-OF(<BREAK CAMPO>) THEN
               DO:
                 ifila = ifila + 1.
                 cfila  = string(ifila).
               
                 /* Formato Titulo */
                 chWorkSheet:Range( "B" + cfila + ":V" + cfila):BorderAround(1,2,9,9).
                 chWorkSheet:range( "B" + cfila + ":V" + cfila):font:bold           = true.
                 chWorkSheet:range( "B" + cfila + ":V" + cfila):font:size           = 7.
                 chWorkSheet:Range( "B" + cfila + ":V" + cfila):interior:colorindex = 9.
                 chWorkSheet:Range( "B" + cfila + ":V" + cfila):font:colorindex     = 9.
               
                 /* Titulo */
                 cRange = "A" + cfila.   
                 chWorkSheet:Range(crange):value = "Sub.Total ".
               
                 cRange = "B" + cfila.   
                 chWorkSheet:Range(crange):value = v_acum1.
                 cRange = "C" + cfila.   
                 chWorkSheet:Range(crange):value = v_acum2.
               
                 v_acum1 = 0.
                 v_acum2 = 0.
               
              END.
  END.

                       
    /*-- TOTALES GENERALES --*/
      ifila = ifila + 1.
      cfila  = string(ifila).
      cRange = "A" + cfila.
      
      /* Formato Titulo */
      chWorkSheet:Range( "A" + cfila + ":Z" + cfila):BorderAround(1,2,11,11).
      chWorkSheet:Range(crange + ":Z" + cfila ):Font:Bold                = true.
      chWorkSheet:Range( "A" + cfila + ":Z" + cfila):interior:colorindex = 11.
      chWorkSheet:Range( "A" + cfila + ":Z" + cfila):font:colorindex     = 2.
      
      /* Titulo */     
      chWorkSheet:Range(crange):value = "Totales Generales".
 
      cRange = "B" + cfila.   
      chWorkSheet:Range(crange):formula = t_acum1.
      cRange = "C" + cfila.   
      chWorkSheet:Range(crange):formula = t_acum2.
                           
                           
  ----------------------------------- FIN PROCESO PRINCIPAL ----------------------------*/

  /*-- LIBERA VARIABLES DE EXCEL --*/
    if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
    if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
    if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.                                                  
     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
   
  fill-cliente:load-mouse-pointer  ('glove') in frame {&FRAME-NAME}.
  fi-banco:load-mouse-pointer  ('glove') in frame {&FRAME-NAME}.
  
  apply "entry" to  FILL-fecha_desde.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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


