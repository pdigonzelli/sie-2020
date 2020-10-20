&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
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
define var alta as logical no-undo initial false.
define var antes_id_contrato like contratos.id_contrato.
define var antes_id_tipo_contrato like contratos.id_tipo_contrato.
define var antes_anio like contratos.anio.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b_contratos_ro AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-misc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_cus-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_contratos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v_contratos_obs AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Items" 
     SIZE 22 BY 1.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 8  NO-FILL 
     SIZE-PIXELS 229 BY 50
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.71 COL 128
     RECT-2 AT Y 0 X 345
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 151 BY 25.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Contratos"
         HEIGHT             = 25.95
         WIDTH              = 151
         MAX-HEIGHT         = 26.43
         MAX-WIDTH          = 152.2
         VIRTUAL-HEIGHT     = 26.43
         VIRTUAL-WIDTH      = 152.2
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "visibles" W-Win _INLINE
/* Actions: ? custom/support/cusvis.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" W-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/ccontainr.i}
{custom/method/cabm.i}
{custom/method/contenedor.i}
{custom/method/l-create-obj.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Contratos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Contratos */
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
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Items */
DO:
  define var r as rowid.
  
  run get-rowid1 in h_b_contratos_ro (output r).
  
  find contratos where rowid(contratos) = r no-lock no-error.
  if available contratos then
    do:
        run w_items_contratos_ro.w (input contratos.id_contrato, 
                                    input contratos.id_tipo_contrato,
                                    input contratos.anio).
    end.
  else
    message "No se encontro el contrato." view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
{custom/method/winitialize.i}
{custom/method/ctitulo.i}
run deshabilita_viewer.
run habilitar_relacion_viewer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 0,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_cus-navico ).
       RUN set-position IN h_cus-navico ( 1.24 , 70.80 ) NO-ERROR.
       RUN set-size IN h_cus-navico ( 1.91 , 24.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'custom/objects/cus-misc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Misc-icon':U ,
             OUTPUT h_cus-misc ).
       RUN set-position IN h_cus-misc ( 1.24 , 93.80 ) NO-ERROR.
       RUN set-size IN h_cus-misc ( 1.91 , 21.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b_contratos_ro.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b_contratos_ro ).
       RUN set-position IN h_b_contratos_ro ( 3.62 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b_contratos_ro ( 6.71 , 146.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-LABELS = ':U + 'Principal|Observaciones' + ',
                     FOLDER-TAB-TYPE = 1':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 10.52 , 2.00 ) NO-ERROR.
       RUN set-size IN h_folder ( 16.43 , 149.00 ) NO-ERROR.

       /* Links to csmartbrowser h_b_contratos_ro. */
       RUN add-link IN adm-broker-hdl ( h_cus-navico , 'Navigation':U , h_b_contratos_ro ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_contratos.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_contratos ).
       RUN set-position IN h_v_contratos ( 12.43 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 11.95 , 145.00 ) */

       /* Links to SmartViewer h_v_contratos. */
       RUN add-link IN adm-broker-hdl ( h_b_contratos_ro , 'Record':U , h_v_contratos ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  '../industria/v_contratos_obs.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v_contratos_obs ).
       RUN set-position IN h_v_contratos_obs ( 11.95 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.71 , 139.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1') NO-ERROR.

       /* Links to SmartViewer h_v_contratos_obs. */
       RUN add-link IN adm-broker-hdl ( h_v_contratos , 'group-assign':U , h_v_contratos_obs ).
       RUN add-link IN adm-broker-hdl ( h_v_contratos , 'Record':U , h_v_contratos_obs ).

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros W-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE backup_impresion W-Win 
PROCEDURE backup_impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.
define var v_contrato as char.
define var v_filtro as character.
define var v_fecha as character.
define var v_lote as character.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

find contratos where rowid(contratos) = r no-lock no-error.
if available contratos then
    do:
        v_contrato = contratos.id_contrato.
        
    end.
    
lista_reportes = "Contratos,Contratos detallado,Contratos Martin,Orden de Fabricacion,Contratos por Semana,Exportacion Excell Contratos-Semana,Contratos - Daniel,Contratos detallado - Daniel,O.F. - Daniel".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        case cresult:
            when "Contratos" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
            when "Contratos detallado" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_items",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
            
            when "Contratos Martin" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_items_martin",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos para Martin Leyro",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
            
            when "Orden de Fabricacion" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "orden_fabricacion_nueva",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Orden de Fabricacion",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
           when "Contratos por Semana" then 
                do:
                   run contrato-semana.
                end.
           when "Exportacion Excell Contratos-Semana" then 
                do:
                   run exporta_excell_contratos_semana.w.
                end.
           when "Contratos - Daniel" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_ddip",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
            when "Contratos detallado - Daniel" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_items_ddip",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.
           when "O.F. - Daniel" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "orden_fabricacion_nueva_ddip",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Orden de Fabricacion",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end.          
       end case.
    end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE consulta W-Win 
PROCEDURE consulta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_consultas as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

if lista_consultas = "" then
    message "No hay consultas disponibles" view-as alert-box.
else
    run custom/support/cfun.w(input lista_consultas,output cresult).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE contrato-semana W-Win 
PROCEDURE contrato-semana :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
run p_contrato_semana.p.                
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita_viewer W-Win 
PROCEDURE deshabilita_viewer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE BUTTON-1 RECT-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-deshabilitados-paleta W-Win 
PROCEDURE get-deshabilitados-paleta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter hprograma as handle no-undo.
define output parameter estados as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-browser W-Win 
PROCEDURE get-rowid-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter p1 like contratos.id_contrato.
define output parameter p2 like contratos.id_tipo_contrato.
define output parameter p3 like contratos.anio.
define output parameter pr as rowid.
define var vr as rowid.

run get-rowid1 in h_b_contratos_ro (output vr).

p1 = antes_id_contrato.
p2 = antes_id_tipo_contrato.
p3 = antes_anio.
pr = vr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rowid-browser-2 W-Win 
PROCEDURE get-rowid-browser-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter pr as rowid.
define var vr as rowid.

run get-rowid1 in h_b_contratos_ro (output vr).

pr = vr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impresion W-Win 
PROCEDURE impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var lista_reportes as character no-undo.
define var cresult as character no-undo.
define var r as rowid no-undo.
define var v_contrato as char.
define var v_cantidad as integer.
define var v_lotes as character.
define var v_filtro as character.
define var v_fecha as character.
define var v_lote as character.
DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

run devuelve-rowid (output r).

if r = ? then
do: 
    message "No esta posicionado en ningun registro" view-as alert-box.
    return.
end.    

find contratos where rowid(contratos) = r no-lock no-error.
if available contratos then
    do:
        v_contrato = contratos.id_contrato.
    end.
    
/*lista_reportes = "Contratos,Contratos Martin,Orden de Fabricacion,Contratos por Semana,Exportacion Excell Contratos-Semana,Contratos - Daniel,Contratos detallado - Daniel,O.F. - Daniel,Nuevo reporte Contratos".*/
lista_reportes = "Contratos Martin,Contratos por Semana,Exportacion Excell Contratos-Semana,Contratos detallado - Daniel,Nuevo reporte Contratos".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        case cresult:
            /*
            when "Contratos" then 
                do:
                    run p_reportes.p (input "contratos",
                                      input "Reporte de Contratos",
                                      input "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio),
                                      input "").                        
                end.*/
            WHEN "Contratos detallado - Daniel" THEN DO:
                    run p_reporte_contrato_detallado.p (input r).                                  
            END.

            when "Contratos Martin" then 
                do:
                    run p_reportes_9.p (input "contratos_items_martin",
                                      input "Reporte de Contratos para Martin Leyro",
                                      input "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio),
                                      input "").                    
                end.

                      /*
            when "Orden de Fabricacion" then 
                do:
                    run p_reportes.p (input "orden_fabricacion_nueva",
                                      input "Reporte de Orden de Fabricacion",
                                      input "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio),
                                      input "").                    
                end.
                */

           WHEN "Contratos por Semana" THEN DO:
                   run contrato-semana.
           END.
           
           WHEN "Exportacion Excell Contratos-Semana" THEN DO:
                   run w_rep_excell_cont.w.
           END.

           WHEN "Nuevo reporte Contratos" THEN DO:
                  run p_reportes_9.p (input "reporte_contratos",
                                      input "Reporte de Contratos",
                                      input "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio),
                                      input "").                    
           END.

     /*      when "Contratos detallado - Daniel" then 
                do:
                    /************************************************************************************************************/
                    /********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
                    /************************************************************************************************************/
                    
                    v_filtro = "contratos.id_contrato = '" + v_contrato + "' and contratos.anio = " + string(contratos.anio).
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           "contratos_items",                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           v_filtro,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           "Reporte de Contratos",         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "" /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
                    /************************************************************************************************************/
                     
                end. */
       end case.
    end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-copy W-Win 
PROCEDURE post-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-create W-Win 
PROCEDURE post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-delete W-Win 
PROCEDURE post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-update W-Win 
PROCEDURE post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

assign contratos.c_usuario = userid("userdb")
       contratos.c_fecha   = today
       contratos.c_hora    = string(time,"HH:MM:SS").

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-copy W-Win 
PROCEDURE pre-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-create W-Win 
PROCEDURE pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle. /* handle del viewer */
alta = true.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-delete W-Win 
PROCEDURE pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */



find contratos where rowid(contratos) = r no-lock no-error.
if available contratos then do:
    for each items_contratos of contratos no-lock.
        find first orden_entrega where orden_entrega.id_contrato      = items_contratos.id_contrato
                                   and orden_entrega.id_tipo_contrato = items_contratos.id_tipo_contrato
                                   and orden_entrega.anio             = items_contratos.anio
                                   and orden_entrega.item             = items_contratos.item
                                   and orden_entrega.pendiente        = false 
                                   no-lock no-error.
        if available orden_entrega then
            do:
                message "No puede eliminar este contrato porque ya hay una OE cerrada asociada." view-as alert-box.
                return "ADM-ERROR".
            end.                     
    end.
end.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pre-update W-Win 
PROCEDURE pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter r as rowid. /* rowid de la tabla updateada */
define input parameter h as handle. /* handle del viewer */

find contratos where rowid(contratos) = r no-lock no-error.

/*
find orden_entrega where orden_entrega.id_contrato      = contratos.id_contrato
                     and orden_entrega.id_tipo_contrato = contratos.id_tipo_contrato
                     and orden_entrega.anio             = contratos.anio 
                     no-lock no-error.
if available orden_entrega then
    do:
        message "No puede modificar este contrato porque ya hay una OE asociada." view-as alert-box.
        return "ADM-ERROR".
    end.                     
*/

if available contratos then
    do:
        antes_id_contrato       = contratos.id_contrato.
        antes_id_tipo_contrato  = contratos.id_tipo_contrato.
        antes_anio              = contratos.anio.
    end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reportes W-Win 
PROCEDURE reportes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter p_nombre_rep as char.
define input parameter p_titulo_rep as char.
define input parameter p_filtro_rep as char.
define input parameter p_param_rep as char.

DEFINE VAR RB-MEMO-FILE AS CHARACTER INITIAL "".

/************************************************************************************************************/
/********************ENVIO DE DATOS AL REPORT BUILDER********************************************************/
/************************************************************************************************************/
                    
                    
                          RUN  aderb\_prntrb2(
                           "..\industria\reports.prl", /* RB-REPORT-LIBRARY */
                           p_nombre_rep,                    /* RB-REPORT-NAME */
                           "",                             /* RB-DB-CONNECTION */
                           "O",                             /* RB-INCLUDE-RECORDS */
                           p_filtro_rep,                              /* RB-FILTER */
                           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
                           "D",                             /* RB-PRINT-DESTINATION */
                           "?",                              /* RB-PRINTER-NAME */
                           "",                              /* RB-PRINTER-PORT */
                           "",                              /* RB-OUTPUT-FILE */
                            1,                              /* RB-NUMBER-COPIES  - zero */                  
                            0,                              /* RB-BEGIN-PAGE - zero */
                            0,                              /* RB-END-PAGE - zero */
                           no,                              /* RB-TEST-PATTERN */
                           p_titulo_rep,         /* RB-WINDOW-TITLE */
                           yes,                           /* RB-DISPLAY-ERRORS */
                           yes,                           /* RB-DISPLAY-STATUS */
                           no,                              /* RB-NO-WAIT */
                           "v_general = " + p_param_rep /* RB-OTHER-PARAMETERS */,
                           ""
                           ).   
                           
                    
                    
/************************************************************************************************************/
                     


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetea-registro W-Win 
PROCEDURE resetea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter h as handle no-undo.

alta = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

