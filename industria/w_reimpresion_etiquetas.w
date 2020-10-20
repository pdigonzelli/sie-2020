&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS empresa sucursal lote_jugo anio ~
produccion_jugo tambor_produccion_jugo tambor_lote_jugo articulo producto ~
BUTTON-14 sucursal_impresion BUTTON-1 BUTTON-2 BUTTON-15 BUTTON-12 ~
BUTTON-37 BUTTON-13 BUTTON-36 boton_ar ar RECT-1 
&Scoped-Define DISPLAYED-OBJECTS empresa sucursal FILL-IN-1 FILL-IN-2 ~
lote_jugo anio produccion_jugo tambor_produccion_jugo tambor_lote_jugo ~
articulo producto sucursal_impresion ar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON boton_ar 
     LABEL "AR-" 
     SIZE 7 BY 1.05.

DEFINE BUTTON BUTTON-1 
     LABEL "Lote" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-12 
     LABEL "Sobrante" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-13 
     LABEL "Arrastre" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-14 
     LABEL "Imprimir" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-15 
     LABEL "Insp. 1 etiqueta" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-2 
     LABEL "Inspecciones" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-36 
     LABEL "Sin Nombre" 
     SIZE 15 BY 1.24.

DEFINE BUTTON BUTTON-37 
     LABEL "P y G" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE anio AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE ar AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE articulo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .95 NO-UNDO.

DEFINE VARIABLE empresa AS CHARACTER FORMAT "X(256)":U 
     LABEL "Empresa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "LOTES DE JUGO" 
     VIEW-AS FILL-IN 
     SIZE 50 BY .86
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "PRODUCCION DE JUGO" 
     VIEW-AS FILL-IN 
     SIZE 44 BY .86
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lote_jugo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .86 NO-UNDO.

DEFINE VARIABLE produccion_jugo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Producción" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .86 NO-UNDO.

DEFINE VARIABLE producto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .86 NO-UNDO.

DEFINE VARIABLE sucursal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE sucursal_impresion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sucursal donde se desea imprimir" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tambor_lote_jugo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tambor" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .86 NO-UNDO.

DEFINE VARIABLE tambor_produccion_jugo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tambor" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .86 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 110 BY .19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     empresa AT ROW 1.48 COL 31 COLON-ALIGNED
     sucursal AT ROW 1.48 COL 56 COLON-ALIGNED
     FILL-IN-1 AT ROW 3.24 COL 4 NO-LABEL
     FILL-IN-2 AT ROW 3.33 COL 62 NO-LABEL
     lote_jugo AT ROW 4.29 COL 17 COLON-ALIGNED
     anio AT ROW 4.29 COL 37 COLON-ALIGNED
     produccion_jugo AT ROW 4.29 COL 72 COLON-ALIGNED
     tambor_produccion_jugo AT ROW 4.29 COL 93.6 COLON-ALIGNED
     tambor_lote_jugo AT ROW 5.24 COL 17 COLON-ALIGNED
     articulo AT ROW 5.24 COL 37 COLON-ALIGNED
     producto AT ROW 5.24 COL 72 COLON-ALIGNED
     BUTTON-14 AT ROW 6.14 COL 91
     sucursal_impresion AT ROW 6.38 COL 35 COLON-ALIGNED
     BUTTON-1 AT ROW 7.81 COL 18
     BUTTON-2 AT ROW 7.81 COL 35
     BUTTON-15 AT ROW 7.91 COL 51
     BUTTON-12 AT ROW 9.24 COL 18
     BUTTON-37 AT ROW 9.33 COL 35
     BUTTON-13 AT ROW 10.67 COL 18
     BUTTON-36 AT ROW 12.19 COL 35
     boton_ar AT ROW 12.43 COL 16
     ar AT ROW 12.43 COL 21 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2.57 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 12.91.


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
         TITLE              = "Re-Impresion de Etiquetas"
         HEIGHT             = 12.91
         WIDTH              = 114
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 114
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Re-Impresion de Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Re-Impresion de Etiquetas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME boton_ar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL boton_ar W-Win
ON CHOOSE OF boton_ar IN FRAME F-Main /* AR- */
DO:
    if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
        output to \\192.168.1.151\zebra. 
    else
        output to \\192.168.2.11\zebra.  

    put control "^XA".
    put control "^LH0,0".
    put control "^PQ" 1 "^FS".
        
    put control "^FO15,50^GB760,1120,3^FS".        
    put control "^FO50,60^A0R,560,280^FD" "AR" "^FS".
    PUT control "^FO50,410^A0R,560,120^FD" "-" "^FS".
    put control "^FO50,570^A0R,560,280^FD" ar:screen-value in frame F-Main "^FS".
    put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
    put control "^XZ".
    output close.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Lote */
DO:
define var i as integer.
define var total as integer.
define buffer comp for tambores_industria.
        total = 0.

if sucursal_impresion:screen-value in frame F-Main <> "" then        
    do:                
        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra.
           
        
  /*     OUTPUT TO "\\gabriel\zebra". */
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".
                               
       FIND FIRST tambores_industria where tambores_industria.id_empresa = integer(empresa:screen-value in frame F-Main) and
                                     tambores_industria.id_sucursal = integer(sucursal:screen-value in frame F-Main) and
                                     tambores_industria.id_lote = integer(lote_jugo:screen-value in frame F-Main) and
                                     tambores_industria.id_tambor = integer(tambor_lote_jugo:screen-value in frame F-Main) AND 
                                     tambores_industria.id_tipotambor = 3 and
                                     tambores_industria.id_articulo = integer(articulo:screen-value in frame F-Main) and
                                     tambores_industria.anio = INTEGER(anio:screen-value in frame F-Main) no-lock no-error.
        
        if available tambores_industria then
         do:
            MESSAGE id_tambor VIEW-AS ALERT-BOX. 
            put control "^FO400,50^A0R,300,280^FD" string(tambores_industria.id_lote,">999")  + "/" + SUBSTRING(STRING(tambores_industria.fecha),7,2,"CHARACTER") "^FS".
            for each comp where comp.id_empresa = integer(empresa:screen-value in frame F-Main) and
                                comp.id_sucursal = integer(sucursal:screen-value in frame F-Main) and
                                comp.id_lote = integer(lote_jugo:screen-value in frame F-Main) and
                                comp.id_articulo = integer(articulo:screen-value in frame F-Main) and
                                comp.id_tipotambor = 3 and
                                comp.fecha >= date("01/01/" + anio:screen-value in frame F-Main) no-lock.
                total = total + 1.
            end.
        
            put control "^FO725,735^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
         end.
        else
         do:
            put control "^FO725,850^A0R,72,42^FD" "ERROR" "^FS".
         end.
            
            if total > 99 then
                put control "^FO24,100^A0R,300,250^FD" string(tambores_industria.id_tambor,"999") + "/" + string(total,"999") "^FS".
            else
                put control "^FO24,100^A0R,300,320^FD" string(tambores_industria.id_tambor,"99") + "/" + string(total,"99") "^FS".            

            put control "^FO150,970^A0N,^BY3^B3N,N,230,N,N^FD" "03" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
            put control "^FO10,965^GB780,0,3^FS".
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
    end.
    
else message "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 W-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* Sobrante */
DO:
    define var f as integer.
if sucursal_impresion:screen-value in frame F-Main <> "" then        
    do:                
    
find lotes_jugo where lotes_jugo.id_empresa = integer(empresa:screen-value in frame F-Main) and
                      lotes_jugo.id_sucursal = integer(sucursal:screen-value in frame F-Main) and
                      lotes_jugo.id_lote = integer(lote_jugo:screen-value in frame F-Main) and
                      lotes_jugo.id_articulo = integer(articulo:screen-value in frame F-Main) and 
                      lotes_jugo.anio >= integer(anio:screen-value in frame F-Main) 
                      no-lock no-error.
  if available lotes_jugo then
   do:                    
    for each sobrante of lotes_jugo.
       for each tambores_industria where tambores_industria.id_empresa       = sobrante.id_empresa
                                     and tambores_industria.id_sucursal      = sobrante.id_sucursal
                                     and tambores_industria.id_tipotambor    = sobrante.id_tipotambor_sobrante
                                     and tambores_industria.nromov           = sobrante.nromov_sobrante 
                                     and tambores_industria.id_tambor        = integer(tambor_lote_jugo:screen-value in frame F-Main) 
.
        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra.        
            
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".

        put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "04" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        put control "^FO580,288^A0R,150,140^FD" "SOBRANTE" "^FS".
        put control "^FO390,292^A0R,130,140^FD" "LOTE " + string(lotes_jugo.id_lote) + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
        
        put control "^FO24,393^A0R,130,140^FD" tambores_industria.fecha "^FS".
        put control "^FO725,835^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO180,293^A0R,130,140^FD" productos_terminados.abreviatura "^FS".                    
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(tambores_industria.id_articulo) "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                end.
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS". 
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        f = 1.
        do f = 1 to 30000.
        end.
        
       end. 
     end.
    end.
 end. 
 else message "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 W-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* Arrastre */
DO:
define var f as integer.
if sucursal_impresion:screen-value in frame F-Main <> "" then        
    do:
find lotes_jugo where lotes_jugo.id_empresa = integer(empresa:screen-value in frame F-Main) and
                      lotes_jugo.id_sucursal = integer(sucursal:screen-value in frame F-Main) and
                      lotes_jugo.id_lote = integer(lote_jugo:screen-value in frame F-Main) and
                      lotes_jugo.id_articulo = integer(articulo:screen-value in frame F-Main) and
                      lotes_jugo.anio >= integer(anio:screen-value in frame F-Main)  
                      no-lock no-error.
if available lotes_jugo then
do:
   find arrastre_lote where arrastre_lote.id_empresa = lotes_jugo.id_empresa
                        and arrastre_lote.id_sucursal = lotes_jugo.id_sucursal
                        and arrastre_lote.id_tipotambor = lotes_jugo.id_tipotambor
                        and arrastre_lote.nromov = lotes_jugo.nromov no-error.
   do:
                                   
    for each tambores_industria where tambores_industria.id_empresa = arrastre_lote.id_empresa
                                  and tambores_industria.id_sucursal = arrastre_lote.id_sucursal
                                  and tambores_industria.id_tipotambor = arrastre_lote.id_tipotambor_arrastre
                                  and tambores_industria.nromov = arrastre_lote.nromov_arrastre
                                  and tambores_industria.id_tambor = integer(tambor_lote_jugo:screen-value in frame F-Main)
                                   by id_tambor.
        
        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra. 
         
        put control "^XA".
        put control "^LH0,0".
        put control "^PQ" 1 "^FS".

        put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "05" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
        put control "^FO580,288^A0R,150,140^FD" "ARRASTRE" "^FS".
        put control "^FO390,292^A0R,130,140^FD" "LOTE " + string(lotes_jugo.id_lote) + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
        
        put control "^FO24,393^A0R,130,140^FD" tambores_industria.fecha "^FS".
        put control "^FO725,835^A0R,72,42^FD" substr(string(tambores_industria.fecha),7,2) + "-"  + 
                                              string(tambores_industria.id_etiqueta,"9999999") "^FS".

        
        put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
        put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".

        find productos_terminados where productos_terminados.id_articulo = tambores_industria.id_articulo no-lock no-error.
            if available productos_terminados then
                do:
                    put control "^FO180,293^A0R,130,140^FD" productos_terminados.abreviatura "^FS".                    
                    put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(tambores_industria.id_articulo) "^FS".
                    put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                end.
        put control "^FO10,275^GB780,0,3^FS".
        put control "^FO10,1072^GB780,0,3^FS".
        put control "^FO10,1200^GB780,0,3^FS". 
        put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
        put control "^XZ".
        output close.
        f = 1.
        do f = 1 to 30000.
    
        end.
     end.
    end.
end.
end.
else message "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 W-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Imprimir */
DO:
define var i as integer.
define var total as integer.
define buffer comp for tambores_industria.
        total = 0.        

if sucursal_impresion:screen-value in frame F-Main <> "" then        
    do:                
        
                                            
       find tambores_industria where tambores_industria.id_empresa = integer(empresa:screen-value in frame F-Main) and
                                     tambores_industria.id_sucursal = integer(sucursal:screen-value in frame F-Main) and
                                     tambores_industria.id_lote = integer(produccion_jugo:screen-value in frame F-Main) and
                                     tambores_industria.id_tambor = integer(tambor_produccion_jugo:screen-value in frame F-Main) and
                                     tambores_industria.id_tipotambor = 1 and
                                     tambores_industria.id_articulo = integer(producto:screen-value in frame F-Main) and
                                     tambores_industria.fecha >= date("01/01/" + string(year(today))) no-lock no-error.
        
        if available tambores_industria then
         do:
            MESSAGE "entro en el codigo" VIEW-AS ALERT-BOX.    
            
            if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
                output to \\192.168.1.151\zebra. 
            else
                output to \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            FIND produccion_jugo WHERE produccion_jugo.id_empresa = tambores_industria.id_empresa
                                   AND produccion_jugo.id_sucursal = tambores_industria.id_sucursal
                                   AND produccion_jugo.id_tipotambor = tambores_industria.id_tipotambor
                                   AND produccion_jugo.nromov = tambores_industria.nromov NO-LOCK NO-ERROR. 
            put control "^FO150,40^A0N,^BY3^B3N,N,230,N,N^FD" "01" + string(tambores_industria.id_etiqueta,"9999999") "^FS".
            put control "^FO24,393^A0R,130,140^FD" produccion_jugo.fecha "^FS".
            put control "^FO725,835^A0R,72,42^FD" substr(string(produccion_jugo.fecha),7,2) + "-"  + string(tambores_industria.id_etiqueta,"9999999") "^FS".
            
            put control "^FO24,1095^A0N,36,36,42^FDArticulo^FS".
            put control "^FO580,1095^A0N,36,42^FDNro.Tambor^FS".
    
            find productos_terminados where productos_terminados.id_articulo = produccion_jugo.id_articulo no-lock no-error.
                if available productos_terminados then
                    do:
                        if productos_terminados.id_articulo = 531 then 
                            do:
                                put control "^FO550,288^A0R,150,140^FD" productos_terminados.abreviatura "^FS".
                                put control "^FO190,292^A0R,300,320^FD" "C." + string(productos_terminados.id_articulo) "^FS".
                            end.
                        else 
                            do:
                                put control "^FO550,288^A0R,150,140^FD" "PRODUCCION" "^FS".
                                put control "^FO190,292^A0R,300,320^FD" string(produccion_jugo.id_produccion) "^FS".                
                            end.
                        put control "^FO24,1135^A0N,72,42^FD"  productos_terminados.descripcion + "  -  " + string(productos_terminados.id_articulo) "^FS".
                        put control "^FO630,1135^A0N,72,42^FD" tambores_industria.id_tambor "^FS".
                    end.
            put control "^FO10,275^GB780,0,3^FS".
            put control "^FO10,1072^GB780,0,3^FS".
            put control "^FO10,1200^GB780,0,3^FS".         
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
                output close.
        END.
    end.
    else message "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Insp. 1 etiqueta */
DO:
define buffer tam for tambores_industria.
define var tambores as integer.
define var peso_neto as integer.
define var f as integer.
define var gall as decimal.
define var peso_bruto as integer.
DEFINE VAR v_suc_impresion AS INTEGER.

tambores = 0.

v_suc_impresion = INTEGER(sucursal_impresion:screen-value in frame F-Main).

IF v_suc_impresion <> 0 THEN DO:                

    FOR EACH tam WHERE tam.id_empresa    = INTEGER(empresa:screen-value in frame F-Main) 
                   AND tam.id_sucursal   = INTEGER(sucursal:screen-value in frame F-Main)
                   AND tam.id_lote       = INTEGER(lote_jugo:screen-value in frame F-Main)
                   AND tam.id_articulo   = INTEGER(articulo:screen-value in frame F-Main) 
                   AND tam.id_tipotambor = 3 
                   AND tam.anio          = INTEGER(anio:screen-value in frame F-Main)
                   NO-LOCK
                   BY id_tambor.

        tambores = tambores + 1. 
    END.
    
    FOR EACH tambores_industria WHERE tambores_industria.id_empresa      = INTEGER(empresa:screen-value in frame F-Main) 
                                  AND tambores_industria.id_sucursal     = INTEGER(sucursal:screen-value in frame F-Main)
                                  AND tambores_industria.id_lote         = INTEGER(lote_jugo:screen-value in frame F-Main)
                                  AND tambores_industria.id_articulo     = INTEGER(articulo:screen-value in frame F-Main) 
                                  AND tambores_industria.id_tipotambor   = 3
                                  AND tambores_industria.anio            = INTEGER(anio:screen-value in frame F-Main) 
                                  NO-LOCK.
        
        IF v_suc_impresion = 96 THEN 
            OUTPUT TO \\192.168.1.151\zebra. 
        ELSE 
            OUTPUT TO \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            
            if tambores_industria.id_sucursal = 96 then
            do:  
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                           and r_productos_calidad.id_articulo = tambores_industria.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice AS LOGICAL.
                    
                    CASE choice:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
            end.
            else
            do:
            
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 2: Ruta Provincial 38 -Tel: (0381) 4512650" "^FS".            
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                           and r_productos_calidad.id_articulo = tambores_industria.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice95 AS LOGICAL.
                    
                    CASE choice95:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
                            
            END.


            put control "^FO25,200^GB785,0,3^FS".
            if tambores_industria.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if tambores_industria.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" tambores_industria.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(tambores_industria.id_lote,">999") + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
            
          find lotes_jugo where lotes_jugo.id_empresa = tambores_industria.id_empresa and  
                                lotes_jugo.id_sucursal = tambores_industria.id_sucursal and
                                lotes_jugo.id_tipotambor = tambores_industria.id_tipotambor and
                                lotes_jugo.nromov = tambores_industria.nromov .
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end.
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,">99") + "/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".
            put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".
            put control "^FO600,810^A0N,45,45^FD" "17" "^FS".
            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,938^A0N,45,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") + "%" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            end.
            
end.
ELSE MESSAGE "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Inspecciones */
DO:
define buffer tam for tambores_industria.
define var tambores as integer.
define var peso_neto as integer.
define var f as integer.
define var gall as decimal.
define var peso_bruto as integer.
tambores = 0.

if sucursal_impresion:screen-value in frame F-Main <> "" then        
    do:                

        for each tam where tam.id_empresa = integer(empresa:screen-value in frame F-Main) 
                       and tam.id_sucursal = integer(sucursal:screen-value in frame F-Main)
                       and tam.id_lote = integer(lote_jugo:screen-value in frame F-Main)
                       and tam.id_articulo = integer(articulo:screen-value in frame F-Main) 
                       and tam.id_tipotambor = 3 
                       and tam.fecha >= date("01/01/" + anio:screen-value in frame F-Main) by id_tambor.

             tambores = tambores + 1. 
        end.

if integer(tambor_lote_jugo:screen-value in frame F-Main) = 0 then
do:
/**********************************************************************************************************/
/*****************GENERO LA ETIQUETA NUMERO CERO***********************************************************/
            for each tambores_industria where tambores_industria.id_empresa = integer(empresa:screen-value in frame F-Main) 
                                      and tambores_industria.id_sucursal = integer(sucursal:screen-value in frame F-Main)
                                      and tambores_industria.id_lote = integer(lote_jugo:screen-value in frame F-Main)
                                      and tambores_industria.id_tambor = 1 
                                      and tambores_industria.id_articulo = integer(articulo:screen-value in frame F-Main) 
                                      and tambores_industria.id_tipotambor = 3
                                      and tambores_industria.fecha >= date("01/01/" + anio:screen-value in frame F-Main) no-lock.

        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            
            if tambores_industria.id_sucursal = 95 then
            do:  
              put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 2: Ruta Provincial 38 -Tel: (0381) 4512650" "^FS".            
              put control "^FO25,144^GB785,0,3^FS".
              put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973" "^FS".
            end.
            else
            do:  
              put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS". 
              put control "^FO25,144^GB785,0,3^FS".
              put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573" "^FS". 
            end.

            put control "^FO25,200^GB785,0,3^FS".
            if tambores_industria.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if tambores_industria.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" tambores_industria.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(tambores_industria.id_lote,">999") + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
            
          find lotes_jugo where lotes_jugo.id_empresa = tambores_industria.id_empresa and  
                                lotes_jugo.id_sucursal = tambores_industria.id_sucursal and
                                lotes_jugo.id_tipotambor = tambores_industria.id_tipotambor and
                                lotes_jugo.nromov = tambores_industria.nromov.
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end.
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            put control "^FO560,434^A0N,45,60^FD" string(0,">99") + "/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".
            put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".
            put control "^FO600,810^A0N,45,45^FD" "17" "^FS".
            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,938^A0N,45,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") + "%" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            end.
                        
/***************TERMINA LA ETIQUETA NUMERO CERO***********************************************************/            
/*********************************************************************************************************/            


end.

                
        for each tambores_industria where tambores_industria.id_empresa = integer(empresa:screen-value in frame F-Main) 
                                      and tambores_industria.id_sucursal = integer(sucursal:screen-value in frame F-Main)
                                      and tambores_industria.id_lote = integer(lote_jugo:screen-value in frame F-Main)
                                      and tambores_industria.id_tambor = integer(tambor_lote_jugo:screen-value in frame F-Main)
                                      and tambores_industria.id_articulo = integer(articulo:screen-value in frame F-Main) 
                                      and tambores_industria.id_tipotambor = 3
                                      and tambores_industria.fecha >= date("01/01/" + anio:screen-value in frame F-Main) no-lock.

        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra.
                
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            put control "^FO50,30^A0N,45,70^FD" "S.A. SAN MIGUEL" "^FS".
            put control "^FO380,70^A0N,30,30^FD" "A.G.I.C.I. y F." "^FS".
            put control "^FO25,96^GB580,0,3^FS".
            
            if tambores_industria.id_sucursal = 96 then
            do:  
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 1:Lavalle 4001-Tel: (0381) 4512600 - Fax: (0381) 4512612" "^FS".
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                           and r_productos_calidad.id_articulo = tambores_industria.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice AS LOGICAL.
                    
                    CASE choice:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "(4000) S.M. de Tucuman - Argentina.  RNE:23000573 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
            end.
            else
            do:
            
                put control "^FO30,106^A0N,36,23^FD" "Planta Industrial 2: Ruta Provincial 38 -Tel: (0381) 4512650" "^FS".            
                put control "^FO25,144^GB785,0,3^FS".
                find r_productos_calidad where r_productos_calidad.id_calidad = tambores_industria.id_calidad
                                           and r_productos_calidad.id_articulo = tambores_industria.id_articulo no-error.
                
                if available r_productos_calidad and r_productos_calidad.rnpa <> "" then
                  do:
                    put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:" + r_productos_calidad.rnpa + "^FS".
                  end.
                else 
                  do:
                    MESSAGE "Esta por imprimir una etiqueta que no tiene asociado un RNPA definido. Desea imprimir
                             con RNPA:23026750" VIEW-AS ALERT-BOX 
                        QUESTION BUTTONS YES-NO-CANCEL UPDATE choice95 AS LOGICAL.
                    
                    CASE choice95:
                        WHEN TRUE THEN /* Yes */
                          DO:
                            put control "^FO30,160^A0N,36,23^FD" "CP(4132) Padilla - Famailla - Argentina.  RNE:23001973 RNPA:23026750" "^FS".        
                          END.
                        WHEN FALSE THEN /* No */
                          DO:
                            return no-apply.                      
                          END.                
                    END CASE.    
                    
                  end.
                            
            END.


            put control "^FO25,200^GB785,0,3^FS".
            if tambores_industria.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if tambores_industria.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" tambores_industria.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(tambores_industria.id_lote,">999") + "/" + substr(string(tambores_industria.fecha),7,2) "^FS".
            
          find lotes_jugo where lotes_jugo.id_empresa = tambores_industria.id_empresa and  
                                lotes_jugo.id_sucursal = tambores_industria.id_sucursal and
                                lotes_jugo.id_tipotambor = tambores_industria.id_tipotambor and
                                lotes_jugo.nromov = tambores_industria.nromov .
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end.
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,">99") + "/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".
            put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".
            put control "^FO600,810^A0N,45,45^FD" "17" "^FS".
            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,938^A0N,45,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") + "%" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
            put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            end.
            
end.
else message "Por favor indique en que sucursal(Planta) desea hacer la impresion." view-as alert-box.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-36
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-36 W-Win
ON CHOOSE OF BUTTON-36 IN FRAME F-Main /* Sin Nombre */
DO:
  define buffer tam for tambores_industria.
define var tambores as integer.
define var peso_neto as integer.
DEFINE VAR peso_neto_balde AS INTEGER.
define var f as integer.
define var gall as decimal.
define var peso_bruto as integer.
DEFINE VAR peso_bruto_balde AS INTEGER.
tambores = 0.
        for each tam where tam.id_empresa = integer(empresa:screen-value in frame F-Main) 
                       and tam.id_sucursal = integer(sucursal:screen-value in frame F-Main)
                       and tam.id_articulo = integer(articulo:screen-value in frame F-Main) 
                       and tam.id_lote = integer(lote_jugo:screen-value in frame F-Main)
                       and tam.id_tipotambor = 3 by id_tambor.

             tambores = tambores + 1. 
        end.

if integer(tambor_lote_jugo:screen-value in frame F-Main) = 0 THEN do:
    /**********************************************************************************************************/
/*****************GENERO LA ETIQUETA NUMERO CERO***********************************************************/
    for each tambores_industria where tambores_industria.id_empresa = integer(empresa:screen-value in frame F-Main) 
                                  and tambores_industria.id_sucursal = integer(sucursal:screen-value in frame F-Main)
                                  and tambores_industria.id_lote = integer(lote_jugo:screen-value in frame F-Main)
                                  and tambores_industria.id_tambor = 1 
                                  and tambores_industria.id_articulo = integer(articulo:screen-value in frame F-Main) 
                                  and tambores_industria.id_tipotambor = 3
                                  and tambores_industria.fecha >= date("01/01/" + anio:screen-value in frame F-Main) no-lock.

            if tambores_industria.id_tambor = 1 then
             do:
                if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
                    output to c:\lpt1. 
                 else
                    output to \\192.168.2.11\zebra.
                   
            put control "^XA".
            put control "^LH0,0".
            put control "^PQ" 1 "^FS".
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".
            
            /* ACA ESTABA EL CODIGO QUE FALTA */
            
            put control "^FO25,200^GB785,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
                  put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if lotes_jugo.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.fecha),7,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
            /* if Inspecciones_lote.Acidez_w_v >= 500 then
                do:
                    peso_neto = 260.
                    peso_bruto = 277.
                end.
            else
                do:
                    peso_neto = 250.
                    peso_bruto = 267.
                end. */
                
            if lotes_jugo.id_envase = 527 then
                do:
                    peso_neto_balde = lotes_jugo.peso_neto.
                    peso_bruto_balde = lotes_jugo.peso_neto + 1.05.                    
                end.
            else
                do:
                    peso_neto = lotes_jugo.peso_neto.
                    peso_bruto = lotes_jugo.peso_neto + envases_prod.tara.    
                end.
               
                
            /* message "Peso Neto:" peso_neto "    Pero Bruto:" peso_bruto view-as alert-box.    */
                
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            if tambores > 99 then
                put control "^FO560,434^A0N,45,60^FD" "000/" + string(tambores,">99") "^FS".
            else 
                put control "^FO560,434^A0N,45,60^FD" "00/" + string(tambores,">99") "^FS".            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
                end.

            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,810^A0N,45,45^FD" "1.05" "^FS".
                end.
            else
                do:
                    put control "^FO600,810^A0N,45,45^FD" envases_prod.tara "^FS".
                end.

            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,938^A0N,45,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") + "%" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            
            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
                end.
            
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
       /*     put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".*/
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
                        
            end.
           
            
/***************TERMINA LA ETIQUETA NUMERO CERO***********************************************************/            
/*********************************************************************************************************/            
        if integer(sucursal_impresion:screen-value in frame F-Main) = 96 then
            output to \\192.168.1.151\zebra. 
        else
            output to \\192.168.2.11\zebra.

                 
            put control "^XA".
            put control "^LH0,0".
            
            if lotes_jugo.id_envase = 527 then    /* BIDONES PLASTICOS DE 22 LITROS */
                put control "^PQ" 1 "^FS".
            else 
                put control "^PQ" 2 "^FS".
            
            put control "^FO1,1^XGR:Logo.grf,1,1^FS".
            put control "^FO25,20^GB785,1200,3^FS".

            /* ACA ESTABA EL CODIGO QUE FALTA */

            put control "^FO25,200^GB785,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
                    put control "^FO30,220^A0N,40,40^FD" "PRODUCT: CONCENTRATED LEMON JUICE" "^FS".
                end.
            if lotes_jugo.id_articulo = 53 then
                do:
                    put control "^FO30,220^A0N,40,35^FD" "PRODUCT: CONCENTRATED LEMON JUICE CLEAR" "^FS".
                end.
            put control "^FO25,265^GB785,0,3^FS".
            put control "^FO25,280^GB355,720,3^FS".
            put control "^FO30,290^A0N,34,25^FD" "Production date" "^FS".
            put control "^FO100,330^A0N,45,60^FD" lotes_jugo.fecha "^FS".
            put control "^FO441,280^GB370,720,3^FS".
            put control "^FO446,290^A0N,34,25^FD" "Batch N." "^FS".
            put control "^FO560,330^A0N,45,60^FD" string(lotes_jugo.id_lote,">>99") + "/" + substr(string(lotes_jugo.fecha),7,2) "^FS".
            
          find last inspecciones_lote of lotes_jugo no-lock no-error.
          if available inspecciones_lote then
           do:                                     
                            
            if lotes_jugo.id_envase = 527 then
                do:
                    peso_neto_balde = lotes_jugo.peso_neto.
                    peso_bruto_balde = lotes_jugo.peso_neto + 1.05.                    
                end.
            else
                do:
                    peso_neto = lotes_jugo.peso_neto.
                    peso_bruto = lotes_jugo.peso_neto + envases_prod.tara.    
                end.
            
                       
            put control "^FO25,384^GB355,0,3^FS".
            put control "^FO30,394^A0N,34,25^FD" "Brix 20/20" "^FS".
            put control "^FO184,434^A0N,45,45^FD" string(Inspecciones_lote.Bx_20_20,">99.99") "^FS".
            put control "^FO441,384^GB370,0,3^FS".
            put control "^FO446,394^A0N,34,25^FD" "Drum N." "^FS".
            if tambores > 99 then
                put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,">99") + "/" + string(tambores,">99") "^FS".
            else
                put control "^FO560,434^A0N,45,60^FD" string(tambores_industria.id_tambor,"999") + "/" + string(tambores,">99") "^FS".
            
            put control "^FO486,434^A0N,34,25^FD"  "^FS".
            
            put control "^FO25,512^GB355,0,3^FS".
            put control "^FO30,522^A0N,34,25^FD" "Acidity %" "^FS".
            put control "^FO184,562^A0N,45,45^FD" string(inspecciones_lote.acidez_w_w,">99.99") "^FS".
            put control "^FO441,512^GB370,0,3^FS".
            
            find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
            if available brix then 
             do:
                gall = (peso_neto / brix.pe) / 3.785.
             end.
             
            put control "^FO446,522^A0N,34,25^FD" "Gall / Drum" "^FS".
            put control "^FO600,562^A0N,45,45^FD" string(round(gall,2),">99.99") "^FS".
            
            put control "^FO25,640^GB355,0,3^FS".
            put control "^FO30,650^A0N,34,25^FD" "Corrected Brix" "^FS".
            put control "^FO184,690^A0N,45,45^FD" inspecciones_lote.bx_correg "^FS".
            put control "^FO441,640^GB370,0,3^FS".
            put control "^FO446,650^A0N,34,25^FD" "Gross Weight" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,690^A0N,45,45^FD" peso_bruto "^FS".
                end.

            put control "^FO770,726^A0N,34,25^FD" "Kg." "^FS".
                        
            put control "^FO25,760^GB355,0,3^FS".
            put control "^FO30,770^A0N,34,25^FD" "Acidity (GPL)" "^FS".
            put control "^FO184,810^A0N,45,45^FD" string(inspecciones_lote.acidez_w_v,"999.99") "^FS".
            put control "^FO441,760^GB370,0,3^FS".
            put control "^FO446,770^A0N,34,25^FD" "Tare" "^FS".

            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,810^A0N,45,45^FD" "1.05" "^FS".
                end.
            else
                do:
                    put control "^FO600,810^A0N,45,45^FD" envases_prod.tara "^FS".
                end.

            put control "^FO770,854^A0N,34,25^FD" "Kg." "^FS".
            
            put control "^FO25,888^GB355,0,3^FS".
            if lotes_jugo.id_articulo = 52 then
                do:
            put control "^FO30,898^A0N,34,25^FD" "Pulp %" "^FS".
            put control "^FO184,938^A0N,45,45^FD" string(inspecciones_lote.porcentaje_pulpa,">9.99") + "%" "^FS".
                end.
            put control "^FO441,888^GB370,0,3^FS".
            put control "^FO446,898^A0N,34,25^FD" "Net Weight" "^FS".
            
            if lotes_jugo.id_envase = 527 then
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto_balde "^FS".
                end.
            else
                do:
                    put control "^FO600,938^A0N,45,45^FD" peso_neto "^FS".
                end.
            
            put control "^FO770,966^A0N,34,25^FD" "Kg." "^FS".
           end.
            put control "^FO25,1000^GB785,0,3^FS".
            put control "^FO358,1010^A0N,30,20^FD" "IMPORTANT" "^FS".
            put control "^FO86,1042^A0N,30,20^FD" "WE RECOMMEND TO KEEP THIS PRODUCT FROZEN, AT -18 / -20 C. WITHOUT" "^FS".
            put control "^FO142,1080^A0N,30,20^FD" "THAT PRECAUTION, WE ARE NOT RESPONSABLE OF THE PRODUCT" "^FS".
       /*     put control "^FO150,1120^A0N,38,25^FD" "THIS PRODUCT WAS MADE UNDER HACCP SYSTEM" "^FS".*/
            put control "^FO25,1180^GB785,0,3^FS".
            put control "^FO25,1185^GB785,0,3^FS".  
                        
            put control "^PR6,6,6^FS". /** velocidad de impresion 6 pulgadas por segundo**/
            put control "^XZ".
            output close.
            
            if lotes_jugo.id_sucursal = 96 then
                do: 
                    f = 1.
                    do f = 1 to 30000.
    
                    end.
                end.
            else
                do:
                    f = 1.
                    do f = 1 to 5000.
    
                    end.
                
                end.
    END.
end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-37
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-37 W-Win
ON CHOOSE OF BUTTON-37 IN FRAME F-Main /* P y G */
DO:

    RUN zebra_reimpresion_analisis_pyg.p (INPUT INTEGER(empresa:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(sucursal:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(lote_jugo:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(anio:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(tambor_lote_jugo:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(articulo:SCREEN-VALUE IN FRAME F-Main),
                                          INPUT INTEGER(sucursal_impresion:SCREEN-VALUE IN FRAME F-Main)).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

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
  DISPLAY empresa sucursal FILL-IN-1 FILL-IN-2 lote_jugo anio produccion_jugo 
          tambor_produccion_jugo tambor_lote_jugo articulo producto 
          sucursal_impresion ar 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE empresa sucursal lote_jugo anio produccion_jugo tambor_produccion_jugo 
         tambor_lote_jugo articulo producto BUTTON-14 sucursal_impresion 
         BUTTON-1 BUTTON-2 BUTTON-15 BUTTON-12 BUTTON-37 BUTTON-13 BUTTON-36 
         boton_ar ar RECT-1 
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

