&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          general          PROGRESS
          general          PROGRESS
          general         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

define var i as integer no-undo.
define var nombre-item as character no-undo.
define var empresa_origen as integer.
define var sucursal_origen as integer.
define var cantidad_disponible as integer.
define var origen as integer.
define var carga_produccion as logical initial false.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE csmartbrowser

&Scoped-define ADM-SUPPORTED-LINKS                                 Record-Target,TableIO-Target,Consulta-Target,Navigation-Target,record-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES lotes_jugo
&Scoped-define FIRST-EXTERNAL-TABLE lotes_jugo


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR lotes_jugo.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES envasado_lote tipos_origen unidad_medida ~
envases_prod

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table envasado_lote.id_tipo_origen ~
tipos_origen.descripcion envasado_lote.id_origen envasado_lote.cantidad 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ~
envasado_lote.id_tipo_origen envasado_lote.id_origen envasado_lote.cantidad 
&Scoped-define FIELD-PAIRS-IN-QUERY-br_table~
 ~{&FP1}id_tipo_origen ~{&FP2}id_tipo_origen ~{&FP3}~
 ~{&FP1}id_origen ~{&FP2}id_origen ~{&FP3}~
 ~{&FP1}cantidad ~{&FP2}cantidad ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table envasado_lote
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table envasado_lote
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH envasado_lote OF lotes_jugo WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH tipos_origen OF envasado_lote NO-LOCK, ~
      EACH unidad_medida WHERE unidad_medida.id_um = envasado_lote.unidad_medida NO-LOCK, ~
      EACH envases_prod OF envasado_lote NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table envasado_lote tipos_origen ~
unidad_medida envases_prod
&Scoped-define FIRST-TABLE-IN-QUERY-br_table envasado_lote


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
id_sucursal||y|general.envasado_lote.id_sucursal
id_tipo_origen||y|general.envasado_lote.id_tipo_origen
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "id_sucursal,id_tipo_origen"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Menues" B-table-Win _INLINE
/* Actions: ? custom/support/cusbmen.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "con parametros" B-table-Win _INLINE
/* Actions: ? custom/support/con-parametros.p ? ? ? */
/* SmartWindow,uib,49271
Destroy on next read */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Cabecera" B-table-Win _INLINE
/* Actions: ? custom/support/set-cabecera.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Detalle" B-table-Win _INLINE
/* Actions: ? custom/support/set-detalle.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Items" B-table-Win _INLINE
/* Actions: ? custom/support/set-items.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Modalidad de Insercion Browser" B-table-Win _INLINE
/* Actions: ? custom/support/bmodins.p ? ? ? */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Relaciones" B-table-Win _INLINE
/* Actions: ? custom/support/keyeditb.w ? ? ? */
/* campos relacionados con tablas externas 
general.envasado_lote.id_tipo_origen;wc_tipos_origen.w;tipos_origen.descripcion
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD valida B-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      envasado_lote, 
      tipos_origen, 
      unidad_medida, 
      envases_prod SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      envasado_lote.id_tipo_origen
      tipos_origen.descripcion
      envasado_lote.id_origen
      envasado_lote.cantidad COLUMN-LABEL "Litros"
  ENABLE
      envasado_lote.id_tipo_origen
      envasado_lote.id_origen
      envasado_lote.cantidad
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 52 BY 5.71
         BGCOLOR 17 FGCOLOR 0 FONT 0.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: csmartbrowser
   External Tables: general.lotes_jugo
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 5.71
         WIDTH              = 52.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "general.envasado_lote OF general.lotes_jugo,industria.tipos_origen OF general.envasado_lote,comercial.unidad_medida WHERE general.envasado_lote ...,general.envases_prod OF general.envasado_lote"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[3]      = "comercial.unidad_medida.id_um = general.envasado_lote.unidad_medida"
     _FldNameList[1]   > general.envasado_lote.id_tipo_origen
"envasado_lote.id_tipo_origen" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[2]   = general.tipos_origen.descripcion
     _FldNameList[3]   > general.envasado_lote.id_origen
"envasado_lote.id_origen" ? ? "integer" ? ? ? ? ? ? yes ?
     _FldNameList[4]   > general.envasado_lote.cantidad
"envasado_lote.cantidad" "Litros" ? "integer" ? ? ? ? ? ? yes ?
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{custom/method/cbrowser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON F3 OF br_table IN FRAME F-Main
DO:
  run buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-MOVE-UP OF br_table IN FRAME F-Main
DO:
  /*********** SIIIIII   FUNCIONA!!!!!!!!!  ******************************/
  /*     message "mouse up en el browse" envasado_lote.id_origen.*/
      
  /*empresa_origen = envasado_lote.id_empresa_origen.
  sucursal_origen = envasado_lote.id_sucursal_origen.
  origen = envasado_lote.id_origen.  */
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
  define var r as rowid no-undo.
  define var c as character no-undo.
  define var h as handle no-undo.
  run get-link-handle in adm-broker-hdl ( input this-procedure , input 'Consulta-SOURCE' , output c).
  h=widget-handle(c).
  if valid-handle(h) then
    run dispatch in h ('busca-rowid').  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
     if not carga_produccion then
    do:
        /*message "row entry" envasado_lote.id_origen.*/
        run cargo-variables-origen.   
    end.
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
     /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  
  if not carga_produccion then
    do:
        /*message "value change" envasado_lote.id_origen.*/
        run cargo-variables-origen.   
    end.  
  
  
   /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME envasado_lote.id_tipo_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL envasado_lote.id_tipo_origen br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF envasado_lote.id_tipo_origen IN BROWSE br_table /* Tipo Origen */
DO:
  define var r as rowid.
  run wc_tipos_origen.w(output r).
     find tipos_origen where rowid(tipos_origen) = r no-lock no-error.
     if available tipos_origen then 
      do: 
       envasado_lote.id_tipo_origen:screen-value in browse {&BROWSE-NAME} = string(tipos_origen.id_tipo_origen).
       /*envasado_lote.descripcion*/
      end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME envasado_lote.id_origen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL envasado_lote.id_origen br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DBLCLICK OF envasado_lote.id_origen IN BROWSE br_table /* Origen */
DO:
  define var r as rowid.
  define var p1 as integer.
  define var p2 as integer.
  define var p3 as integer.
  define var p4 as integer.
  define var num as integer.
  define var hcontainer as handle.
  define var p_fecha as date.
  
  RUN get-container (output hcontainer).
  RUN get-rowid-cabecera in hcontainer (output r).
  
  find lotes_jugo where rowid(lotes_jugo) = r no-lock no-error.
  if available lotes_jugo then p_fecha = lotes_jugo.fecha.


  if integer(envasado_lote.id_tipo_origen:screen-value in browse {&BROWSE-NAME}) = 1 then
    do: /* ES UNA PRODUCCION */
        run wc_produccion_jugo.w (output p1, output p2, output p3, output p4, input p_fecha).
        find produccion_jugo where produccion_jugo.id_empresa = p1 and
                                   produccion_jugo.id_sucursal = p2 and
                                   produccion_jugo.id_produccion = p3 no-lock no-error.
        if available produccion_jugo then
            do:
                envasado_lote.id_origen:screen-value in browse {&BROWSE-NAME} = string(produccion_jugo.id_produccion).
                empresa_origen = produccion_jugo.id_empresa.
                sucursal_origen = produccion_jugo.id_sucursal.
                cantidad_disponible = p4.
                carga_produccion = true.
            end.
    end.
  else
    do:
     if integer(envasado_lote.id_tipo_origen:screen-value in browse {&BROWSE-NAME}) = 2 then
        do:  /* ES UN SOBRANTE */
            run wc_sobrante.w (output p1, output p2, output p3, output p4, input p_fecha).
            find sobrante where sobrante.id_empresa = p1 and
                                sobrante.id_sucursal = p2 and
                                sobrante.id_lote = p3 no-lock no-error.
            if available sobrante then
                do:
                    envasado_lote.id_origen:screen-value in browse {&BROWSE-NAME} = string(sobrante.id_lote).
                    empresa_origen = sobrante.id_empresa.
                    sucursal_origen = sobrante.id_sucursal.
                    cantidad_disponible = p4.
                    carga_produccion = true.
                end.
        end.
     else message "No ha elegido un tipo de origen valido" view-as alert-box.
    
    end.
  
  /*num = 71.
     run wc_legajo.w (input num, output r).
     find legajo where rowid(legajo) = r no-lock no-error.
     if available legajo then 
     do:
        envasadores_fabrica.id_legajo:screen-value in browse {&BROWSE-NAME} = string(legajo.lega-12).
        envasadores_fabrica.id_categoria:screen-value in browse {&BROWSE-NAME} = string(legajo.p-codi).
        cat = legajo.p-codi.
        find perfil where perfil.p-codi = legajo.p-codi no-lock no-error.
        if available perfil then
            do: 
            envasadores_fabrica.rol:screen-value in browse {&BROWSE-NAME} = string(perfil.p-desc).
            rol1 = perfil.p-desc.
            end.
        
     end.*/
       

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME envasado_lote.cantidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL envasado_lote.cantidad br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF envasado_lote.cantidad IN BROWSE br_table /* Litros */
DO:
  define var origen as integer.
  define var tipo_origen as integer.
  if carga_produccion then
    do:
        /*message "carga produccion es true".*/
        if cantidad_disponible < integer(envasado_lote.cantidad:screen-value in browse {&BROWSE-NAME}) then
            do: 
                message "No puede ingresar un valor mayor a " cantidad_disponible view-as alert-box.  
                return "ADM-ERROR".
            end.
        
    end.
  
END.




  
  
  
  
  /*************CODIGO QUE NO SIRVE PERO LO MISMO LO GUARDO******************************/
  
  /*else
    do:
          message "carga produccion false".
        /*  message "veamos que emp suc or muestra " envasado_lote.id_empresa_origen envasado_lote.id_sucursal_origen envasado_lote.id_origen.*/
          if envasado_lote.id_tipo_origen = 1 then
            do: /* ES UNA PRODUCCION */
                
                find produccion_jugo where produccion_jugo.id_empresa = envasado_lote.id_empresa_origen and
                                        produccion_jugo.id_sucursal = envasado_lote.id_sucursal_origen and
                                        produccion_jugo.id_produccion = envasado_lote.id_origen no-lock no-error.
                if available produccion_jugo then
                    do:
                        run calcular-restante-produccion.
                        if cantidad_disponible < integer(envasado_lote.cantidad:screen-value in browse {&BROWSE-NAME}) then
                         do: 
                            message "No puede ingresar un valor mayor a " cantidad_disponible view-as alert-box.  
                            return no-apply.
                         end.                
                    end.
           end.
        else
            do:
                if envasado_lote.id_tipo_origen = 2 then
                    do:  /* ES UN SOBRANTE */
                        find sobrante where sobrante.id_empresa = envasado_lote.id_empresa_origen and
                                            sobrante.id_sucursal = envasado_lote.id_sucursal_origen and
                                            sobrante.id_lote = envasado_lote.id_origen no-lock no-error.
                        if available sobrante then
                            do:
                                run calcular-restante-sobrante.
                                if cantidad_disponible < integer(envasado_lote.cantidad:screen-value in browse {&BROWSE-NAME}) then
                                  do: 
                                     message "No puede ingresar un valor mayor a " cantidad_disponible view-as alert-box.  
                                     return no-apply.
                                  end.  
                            end.
                    end.
                else message "No ha elegido un tipo de origen valido" view-as alert-box.
    
            end.
    
    end.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL envasado_lote.cantidad br_table _BROWSE-COLUMN B-table-Win
ON MOUSE-SELECT-DOWN OF envasado_lote.cantidad IN BROWSE br_table /* Litros */
DO:
  /*message "row available mouse select down" envasado_lote.id_origen.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF




/******** EMPIEZA MENU ************/
define sub-menu Ordena 
  menu-item envasado_lote-id_tipo_origen
label 'id_tipo_origen( envasado_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envasado_lote.id_tipo_origen').
                  run set-attribute-list('titulo=Tipo Origen').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by envasado_lote.id_tipo_origen
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envasado_lote-id_origen
label 'id_origen( envasado_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envasado_lote.id_origen').
                  run set-attribute-list('titulo=Origen').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by envasado_lote.id_origen
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
 /* menu-item envasado_lote-unidad_medida
label 'unidad_medida( envasado_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envasado_lote.unidad_medida').
                  run set-attribute-list('titulo=Unidad Medida').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by envasado_lote.unidad_medida
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
  menu-item envasado_lote-id_envase
label 'id_envase( envasado_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envasado_lote.id_envase').
                  run set-attribute-list('titulo=Envase').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by envasado_lote.id_envase
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.*/
  menu-item envasado_lote-cantidad
label 'cantidad( envasado_lote ) '
          triggers: 
              on choose 
                       do:
                  run set-attribute-list('orden=envasado_lote.cantidad').
                  run set-attribute-list('titulo=Litros').
                  run set-attribute-list('busca= yes').
                  &SCOPE SORTBY-PHRASE  by envasado_lote.cantidad
                  {&OPEN-QUERY-{&BROWSE-NAME}} 
              end.
          end.
&SCOPE SORTBY-PHRASE by envasado_lote.id_tipo_origen
{&OPEN-QUERY-{&BROWSE-NAME}}
define menu m-accesorios sub-menu Ordena
   menu-item Busca 
        triggers:  
          on choose apply 'F3' TO {&BROWSE-NAME} in frame {&FRAME-NAME}.
        end.
menu m-accesorios:popup-only = true.
{&BROWSE-NAME}:popup-menu=menu m-accesorios:handle.
run set-attribute-list('orden=envasado_lote.id_tipo_origen').
run set-attribute-list('titulo=Tipo Origen').

/********** TERMINA MENU ***********/


/********** COMIENZAN TRIGGERS *************/


envasado_lote.id_tipo_origen:label-bgcolor in browse {&BROWSE-NAME} = 7.
envasado_lote.id_tipo_origen:label-fgcolor in browse {&BROWSE-NAME} = 15.
envasado_lote.id_origen:label-bgcolor in browse {&BROWSE-NAME} = 7.
envasado_lote.id_origen:label-fgcolor in browse {&BROWSE-NAME} = 15.
/*envasado_lote.unidad_medida:label-bgcolor in browse {&BROWSE-NAME} = 7.
envasado_lote.unidad_medida:label-fgcolor in browse {&BROWSE-NAME} = 15.
envasado_lote.id_envase:label-bgcolor in browse {&BROWSE-NAME} = 7.
envasado_lote.id_envase:label-fgcolor in browse {&BROWSE-NAME} = 15.*/
on MOUSE-SELECT-DBLCLICK,F10 of browse {&BROWSE-NAME} do:
   define var r as rowid no-undo.
   define var hcolumn as widget-handle no-undo.
   hcolumn = browse {&BROWSE-NAME}:current-column. 
   if hcolumn:name = 'id_tipo_origen' then do: 
     run wc_tipos_origen.w(output r).
     find tipos_origen where rowid(tipos_origen) = r no-lock no-error.
     if available tipos_origen then 
       envasado_lote.id_tipo_origen:screen-value in browse {&BROWSE-NAME} = string(tipos_origen.id_tipo_origen).
   end.
end.

/********** TERMINAN TRIGGERS **************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-create B-table-Win 
PROCEDURE adm-post-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var hcontainer as handle.
define var r as rowid.
/*message "post create".*/
run set-attribute-list('alta-automatica=no').

RUN get-container (output hcontainer).

RUN get-rowid-cabecera in hcontainer (output r).
for each lotes_jugo where rowid(lotes_jugo) = r.
    assign envasado_lote.id_empresa = lotes_jugo.id_empresa
           envasado_lote.id_sucursal = lotes_jugo.id_sucursal
           envasado_lote.id_lote = lotes_jugo.id_lote
           envasado_lote.id_empresa_origen = empresa_origen
           envasado_lote.id_sucursal_origen = sucursal_origen
           envasado_lote.unidad_medida = 4
           envasado_lote.id_envase = 12
           envasado_lote.c_usuario = userid("userdb")
           envasado_lote.c_fecha   = today
           envasado_lote.c_hora    = string(time,"HH:MM:SS").
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-delete B-table-Win 
PROCEDURE adm-post-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-post-update B-table-Win 
PROCEDURE adm-post-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
carga_produccion = false.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-create B-table-Win 
PROCEDURE adm-pre-create :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-delete B-table-Win 
PROCEDURE adm-pre-delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-pre-update B-table-Win 
PROCEDURE adm-pre-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
if not carga_produccion then
    do:
        /*message "antencion...esta entrando donde no le corresponde". */
        if envasado_lote.id_tipo_origen = 1 then
            do: /* ES UNA PRODUCCION */
                
                find produccion_jugo where produccion_jugo.id_empresa = empresa_origen and
                                        produccion_jugo.id_sucursal = sucursal_origen and
                                        produccion_jugo.id_produccion = origen no-lock no-error.
                if available produccion_jugo then
                    do:
                        run calcular-restante-produccion.
                        if cantidad_disponible < integer(envasado_lote.cantidad:screen-value in browse {&BROWSE-NAME}) then
                         do: 
                            message "No puede ingresar un valor mayor a " cantidad_disponible view-as alert-box.  
                            return no-apply.
                         end.                
                    end.
           end.
        else
            do:
                if envasado_lote.id_tipo_origen = 2 then
                    do:  /* ES UN SOBRANTE */
                        find sobrante where sobrante.id_empresa = empresa_origen and
                                            sobrante.id_sucursal = sucursal_origen and
                                            sobrante.id_lote = origen no-lock no-error.
                        if available sobrante then
                            do:
                                run calcular-restante-sobrante.
                                if cantidad_disponible < integer(envasado_lote.cantidad:screen-value in browse {&BROWSE-NAME}) then
                                  do: 
                                     message "No puede ingresar un valor mayor a " cantidad_disponible view-as alert-box.  
                                     return no-apply.
                                  end.  
                            end.
                    end.
                else message "No ha elegido un tipo de origen valido" view-as alert-box.
    
            end.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "lotes_jugo"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "lotes_jugo"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE asigna-parametros B-table-Win 
PROCEDURE asigna-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define input parameter lista-parametros as character no-undo.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca-registro B-table-Win 
PROCEDURE busca-registro :
define input parameter p as character no-undo.
define input parameter valor as character no-undo.
case p:
  WHEN 'envasado_lote.id_tipo_origen' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_tipo_origen = integer(valor)  no-lock no-error .
  WHEN 'envasado_lote.id_origen' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_origen = integer(valor)  no-lock no-error .
  WHEN 'envasado_lote.unidad_medida' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  unidad_medida = integer(valor)  no-lock no-error .
  WHEN 'envasado_lote.id_envase' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  id_envase = integer(valor)  no-lock no-error .
  WHEN 'envasado_lote.cantidad' then  
       find first {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} where  cantidad = integer(valor)  no-lock no-error .
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscar B-table-Win 
PROCEDURE buscar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
define var valor as character.
define var r as rowid no-undo.
define var p as character no-undo.
define var q as character no-undo.

run get-attribute('busca').
p= return-value.

if p <> "yes" Then
return.

run get-attribute('orden').
p= return-value.

run get-attribute('titulo').
q= return-value.

{&BROWSE-NAME}:set-repositioned-row(3,"always") in frame {&FRAME-NAME}.

run custom/support/fbusca.w  ( input p , input q, output valor) .

if valor <> "" then
do:
    run busca-registro (input p , input valor).
    if available {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} then
    do:
        r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
        reposition {&BROWSE-NAME} to rowid r.
        apply 'value-changed' to self.
    end.
end.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcular-restante-produccion B-table-Win 
PROCEDURE calcular-restante-produccion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var c as integer.
define buffer envasado for envasado_lote.

find general.produccion_jugo where produccion_jugo.id_empresa = empresa_origen and
                                     produccion_jugo.id_sucursal = sucursal_origen and
                                     produccion_jugo.id_produccion = origen NO-LOCK.

do:     
       c = 0.
      FOR EACH envasado WHERE envasado.id_empresa_origen = produccion_jugo.id_empresa
          AND envasado.id_sucursal_origen = produccion_jugo.id_sucursal
          AND envasado.id_origen = produccion_jugo.id_produccion
          AND envasado.id_tipo_origen = 1 NO-LOCK:
          
          if (envasado.id_empresa_origen <> empresa_origen or 
            envasado.id_sucursal_origen <> sucursal_origen or 
            envasado.id_origen <> origen) then
            c = c + envasado_lote.cantidad.
     end.
     
     cantidad_disponible = produccion_jugo.litros - c.
     /*message "cantidad disponible en produccion " cantidad_disponible.*/
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcular-restante-sobrante B-table-Win 
PROCEDURE calcular-restante-sobrante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define var c as integer.
define buffer envasado for envasado_lote.

find sobrante where sobrante.id_empresa = empresa_origen and
                    sobrante.id_sucursal = sucursal_origen and
                    sobrante.id_lote = origen NO-LOCK.

do:
       c = 0.
      FOR EACH envasado WHERE envasado.id_empresa_origen = sobrante.id_empresa
                          AND envasado.id_sucursal_origen = sobrante.id_sucursal
                          AND envasado.id_origen = sobrante.id_lote
                          AND envasado.id_tipo_origen = 2:
          
         if (envasado.id_empresa_origen <> empresa_origen or 
            envasado.id_sucursal_origen <> sucursal_origen or 
            envasado.id_origen <> origen) then
            c = c + envasado.cantidad.
     end.
     
     cantidad_disponible = sobrante.volumen - c.
     /*message "cantidad disponible en sobrante " cantidad_disponible.*/
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargo-variables-origen B-table-Win 
PROCEDURE cargo-variables-origen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        empresa_origen = envasado_lote.id_empresa_origen no-error.
        sucursal_origen = envasado_lote.id_sucursal_origen no-error.
        origen = envasado_lote.id_origen no-error. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE devuelve-rowid B-table-Win 
PROCEDURE devuelve-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
define output parameter r as rowid no-undo.
r=?.
&IF "{&FIELDS-IN-QUERY-{&BROWSE-NAME}}" <> "" &THEN
get current {&BROWSE-NAME}.
if available ({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) then
    r=rowid({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
else
    r=?.    
&ENDIF
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
run set-attribute-list('alta-automatica=no').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
/*message "row available 1" envasado_lote.id_origen.*/


  /* Code placed here will execute PRIOR to standard behavior. */
{custom/support/crow-available.i}

/*message "row available 2" envasado_lote.id_origen.*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

/*message "row available 3" envasado_lote.id_origen.*/

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "id_sucursal" "envasado_lote" "id_sucursal"}
  {src/adm/template/sndkycas.i "id_tipo_origen" "envasado_lote" "id_tipo_origen"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "lotes_jugo"}
  {src/adm/template/snd-list.i "envasado_lote"}
  {src/adm/template/snd-list.i "tipos_origen"}
  {src/adm/template/snd-list.i "unidad_medida"}
  {src/adm/template/snd-list.i "envases_prod"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION valida B-table-Win 
FUNCTION valida RETURNS LOGICAL
  (input nombre as character, input valor as character , output mensaje as character ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  case nombre:
    when "id_sucursal" then
        if integer(valor) = 0 then 
        do:
            mensaje = "error".
            return false.
         end.
    end case.
  RETURN true.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


