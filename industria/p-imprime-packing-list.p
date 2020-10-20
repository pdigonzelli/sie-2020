&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

 define input parameter r as rowid no-undo.
 DEFINE INPUT PARAMETER p_seal AS INTEGER.
 DEFINE INPUT PARAMETER p_pe AS INTEGER.
 DEFINE INPUT PARAMETER p_oe AS INTEGER.
 DEFINE INPUT PARAMETER p_cv AS INTEGER.
 DEFINE INPUT PARAMETER p_des AS INTEGER.
 DEFINE INPUT PARAMETER p_age AS INTEGER.

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
         HEIGHT             = 7.19
         WIDTH              = 57.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  ***************************/

DEFINE VAR RB-MEMO-FILE AS CHARACTER.
DEFINE VAR v_cliente LIKE clientes.razon_social INITIAL " ".
DEFINE VAR v_vapor LIKE vapor.descripcion INITIAL " ".
DEFINE VAR v_origen AS CHARACTER INITIAL "".
DEFINE VAR v_cust_po AS CHARACTER INITIAL "".
DEFINE VAR v_cust_prod AS CHARACTER INITIAL "".
DEFINE VAR v_release AS CHARACTER INITIAL "".
DEFINE VAR v_datos AS INTEGER INITIAL 0.
DEFINE VAR v_obs1 AS CHARACTER FORMAT "x(100)".
DEFINE VAR v_obs2 AS CHARACTER FORMAT "x(100)".
DEFINE VAR v_obs3 AS CHARACTER FORMAT "x(100)".
DEFINE VAR v_tipo_envase AS CHARACTER FORMAT "x(50)".
DEFINE VAR v_permisos AS CHARACTER FORMAT "x(50)".
DEFINE VAR v_orden AS CHARACTER FORMAT "x(50)".
DEFINE VAR v_despachante AS CHARACTER FORMAT "x(50)".
DEFINE VAR v_agencia AS CHARACTER FORMAT "x(20)".
DEFINE VAR v_fda-1 AS CHARACTER FORMAT "x(30)".
DEFINE VAR v_fda-2 AS CHARACTER FORMAT "x(30)".
DEFINE VAR v_fda-1-a AS CHARACTER FORMAT "x(30)".
DEFINE VAR v_fda-2-a AS CHARACTER FORMAT "x(30)".
DEFINE VAR v_cant_lotes-1 AS INTEGER.
DEFINE VAR v_cant_lotes-2 AS INTEGER.
DEFINE VAR v_texto_prod AS CHARACTER FORMAT "x(30)".
DEFINE VAR v_cascara AS LOGICAL INITIAL NO.


RB-MEMO-FILE  = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000))  +  '.TXT'. 

FIND FIRST packing_list WHERE ROWID(packing_list) = r NO-LOCK NO-ERROR.
IF NOT AVAILABLE packing_list THEN
   RETURN.

FIND FIRST items_packing_list OF packing_list NO-LOCK NO-ERROR.
IF NOT AVAILABLE items_packing_list THEN
   RETURN.

FIND FIRST productos_terminados OF items_packing_list NO-LOCK NO-ERROR.
IF AVAILABLE productos_terminados THEN
  v_origen = "..\iconos\bmps\" + productos_terminados.nombre_bmp.
IF v_origen = "" THEN
   IF packing_list.id_tipo_pack_list = 1 THEN
       v_origen = "..\iconos\bmps\jugos.bmp".
     ELSE
       v_origen = "..\iconos\bmps\fruta entrada.bmp".

OS-COPY VALUE(v_origen) VALUE("..\iconos\bmps\producto.bmp").


IF productos_terminados.id_articulo = 54 THEN
    v_cascara = YES.

FIND FIRST clientes OF packing_list NO-LOCK NO-ERROR.
IF AVAILABLE clientes THEN
DO:
    v_cliente = clientes.razon_social.
    IF clientes.datos_contratos = YES THEN
        v_datos = 1.
      ELSE
        v_datos = 0.
END.

FIND FIRST vapor OF packing_list NO-LOCK NO-ERROR.
IF AVAILABLE vapor THEN
   v_vapor = vapor.descripcion.

   FIND FIRST items_contratos WHERE items_contratos.id_contrato = packing_list.id_contrato AND
        items_contratos.id_tipo_contrato = packing_list.id_tipo_contrato AND
        items_contratos.anio = packing_list.anio and
        items_contratos.ITEM = packing_list.ITEM_contrato
        NO-LOCK NO-ERROR.
    IF AVAILABLE items_contratos THEN
    DO:
       v_cust_po = items_contratos.id_po_cliente[1].
       v_cust_prod = items_contratos.id_articulo_cliente[1].
       v_release = items_contratos.numero_release[1].
    END.

    v_obs1 = entry(1,packing_list.observacion,CHR(10)) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        v_obs1 = "".

    v_obs2 = entry(2,packing_list.observacion,CHR(10)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        v_obs2 = "".

    v_obs3 = entry(3,packing_list.observacion,CHR(10)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        v_obs3 = "".


    /* Para cargar el tipo de envase */

      v_tipo_envase = "Drums".
        FIND FIRST items_packing_list OF packing_list NO-LOCK NO-ERROR.
        IF AVAILABLE items_packing_list THEN
        DO:
            FIND FIRST envases_prod WHERE envases_prod.id_envase =
                items_packing_list.id_envase NO-LOCK NO-ERROR.
            IF AVAILABLE envases_prod THEN
                v_tipo_envase = envases_prod.abreviatura_ingles.
          END.
   
   
   /* Permisos de embarque */
   IF p_pe = 1 THEN
   DO:
       v_permisos = " ".
       FOR EACH items_packing_list OF packing_list NO-LOCK BREAK BY anio_permiso
           BY id_aduana BY nro_permiso_embarque:
           IF LAST-OF(nro_permiso_embarque) THEN
              v_permisos = v_permisos + trim(items_packing_list.nro_permiso_embarque) + ".".
       END.
      IF trim(v_permisos) <> "" THEN v_permisos = SUBSTRING(v_permisos,1,LENGTH(v_permisos) - 1).
   END.


   /* Orden de embarque */
   IF p_oe = 1 THEN
   DO:
       v_orden = " ".
       FOR EACH items_packing_list OF packing_list NO-LOCK BREAK BY nro_orden_embarque:
           IF LAST-OF(nro_orden_embarque) THEN
              v_orden = v_orden + STRING(items_packing_list.nro_orden_embarque,">>>>9") + ".".
       END.
      IF trim(v_orden) <> "" THEN v_orden = SUBSTRING(v_orden,1,LENGTH(v_orden) - 1).
   END.


   /* Despachante */
   IF p_des = 1 THEN
   DO:
       v_despachante = " ".
       FIND FIRST despachantes OF packing_list NO-LOCK NO-ERROR.
       IF AVAILABLE despachantes THEN
           v_despachante = despachantes.descripcion.
   END.

   /* Agencia */
   IF p_age = 1 THEN
   DO:
       v_agencia = " ".
       FIND FIRST agencias WHERE agencias.id_agencia = packing_list.id_agencia NO-LOCK NO-ERROR.
       IF AVAILABLE agencias THEN
           v_agencia = agencias.descripcion.
   END.

   /* Datos de Planta y Nro de FDA para USA y Canada */

   v_fda-1 = " ".
   v_fda-2 = " ".
   v_fda-1-a = " ".
   v_fda-2-a = " ".
   v_cant_lotes-1 = 0.
   v_cant_lotes-2 = 0.
   FIND FIRST destinos WHERE destinos.id_destino = packing_list.id_destino_grupo AND
        (destinos.id_zona = 6 OR destinos.id_zona = 10) NO-LOCK NO-ERROR. 
   IF AVAILABLE destinos THEN
   DO:
       FOR EACH items_packing_list OF packing_list NO-LOCK BREAK BY nro_lote :
           IF LAST-OF(items_packing_list.nro_lote) THEN
           DO:
              FIND FIRST tambores_industria WHERE 
                   tambores_industria.id_lote = integer(ENTRY(1,items_packing_list.nro_lote,"/")) AND
                   tambores_industria.anio = integer(ENTRY(2,items_packing_list.nro_lote,"/")) + 2000 and
                  (tambores_industria.id_tipotambor = 3 OR tambores_industria.id_tipotambor = 6) AND
                   tambores_industria.id_articulo = items_packing_list.id_articulo NO-LOCK NO-ERROR.
              IF AVAILABLE tambores_industria THEN
              DO:
                  IF tambores_industria.id_sucursal = 96 THEN /*Lavalle*/
                    DO:
                     v_cant_lotes-1 = v_cant_lotes-1 + 1.
                     IF v_fda-1 = " " THEN
                         v_fda-1 = "Batch: " + items_packing_list.nro_lote.
                        ELSE
                         v_fda-1 = v_fda-1 + " - " + items_packing_list.nro_lote.
                    END.
                  IF tambores_industria.id_sucursal = 95 THEN  /* Famailla */
                  DO:
                     v_cant_lotes-2 = v_cant_lotes-2 + 1.
                     IF v_fda-2 = " " THEN
                         v_fda-2 = "Batch: " + items_packing_list.nro_lote.
                       ELSE
                         v_fda-2 = v_fda-2 + " - " + items_packing_list.nro_lote.
                  END.
              END.
           END.
       END.
   END.
   
   v_texto_prod = "".
   IF v_fda-1 <> " " Then
   DO:
      v_texto_prod = "produced in LAVALLE (PLANT I) - FDA # 17851058254".
      IF  v_cant_lotes-1 <= 3 THEN
          v_fda-1 =  v_fda-1 + " " + v_texto_prod.
        ELSE
          v_fda-1-a = v_texto_prod.
   END.

   IF v_fda-2 <> " " Then
   DO:
      v_texto_prod = "produced in FAMAILLA (PLANT II) - FDA # 18292109442".
      IF  v_cant_lotes-2 <= 3 THEN
          v_fda-2 = v_fda-2 + " " + v_texto_prod.
        ELSE
          v_fda-2-a = v_texto_prod.
   END.



 IF v_cascara = NO THEN
    RUN  aderb\_prntrb2(
       "..\ventas\ventas.prl", /* RB-REPORT-LIBRARY */
       "packing_list",                 /* RB-REPORT-NAME */
       "",                              /* RB-DB-CONNECTION */
       "O",                              /* RB-INCLUDE-RECORDS */
       "packing_list.id_sucursal = " + string(packing_list.id_sucursal) + 
       " and packing_list.id_packing_list = " + string(packing_list.id_packing_list)
        ,                              /* RB-FILTER */
       RB-MEMO-FILE,                              /* RB-MEMO-FILE */
       "",                             /* RB-PRINT-DESTINATION */
       "",                /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        0,                              /* RB-NUMBER-COPIES  - zero */                  
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "",                              /* RB-WINDOW-TITLE */
      yes,                              /* RB-DISPLAY-ERRORS */
      yes,                              /* RB-DISPLAY-STATUS */
      YES,                              /* RB-NO-WAIT */
       "v_general = " + v_cliente + "|" + v_vapor +
       "|" + v_cust_po + "|" + v_cust_prod + "|" + v_release + "|"
       + STRING(v_datos) + "|" + STRING(p_seal) + "|"  +
       v_obs1 + "|" + v_obs2 + "|" + v_obs3 + "|" + v_tipo_envase + "|" +
       v_permisos + "|" + v_orden + "|" + STRING(p_cv) + "|" +
       v_despachante + "|" + v_agencia + "|" + v_fda-1 + "|" + v_fda-2 + "|"
       + v_fda-1-a + "|" + v_fda-2-a + "|"
       ,
       ""
       ).                             /* RB-OTHER-PARAMETERS */
       
    ELSE
        RUN  aderb\_prntrb2(
           ".\ventas.prl", /* RB-REPORT-LIBRARY */
           "packing_list-cascara",                 /* RB-REPORT-NAME */
           "",                              /* RB-DB-CONNECTION */
           "O",                              /* RB-INCLUDE-RECORDS */
           "packing_list.id_sucursal = " + string(packing_list.id_sucursal) + 
           " and packing_list.id_packing_list = " + string(packing_list.id_packing_list)
            ,                              /* RB-FILTER */
           RB-MEMO-FILE,                              /* RB-MEMO-FILE */
           "D",                             /* RB-PRINT-DESTINATION */
           "?",                /* RB-PRINTER-NAME */
           "",                              /* RB-PRINTER-PORT */
           "",                              /* RB-OUTPUT-FILE */
            0,                              /* RB-NUMBER-COPIES  - zero */                  
            0,                              /* RB-BEGIN-PAGE - zero */
            0,                              /* RB-END-PAGE - zero */
           no,                              /* RB-TEST-PATTERN */
           "",                              /* RB-WINDOW-TITLE */
          yes,                              /* RB-DISPLAY-ERRORS */
          yes,                              /* RB-DISPLAY-STATUS */
           no,                              /* RB-NO-WAIT */
           "v_general = " + v_cliente + "|" + v_vapor +
           "|" + v_cust_po + "|" + v_cust_prod + "|" + v_release + "|"
           + STRING(v_datos) + "|" + STRING(p_seal) + "|"  +
           v_obs1 + "|" + v_obs2 + "|" + v_obs3 + "|" + v_tipo_envase + "|" +
           v_permisos + "|" + v_orden + "|" + STRING(p_cv) + "|" +
           v_despachante + "|" + v_agencia + "|" + v_fda-1 + "|" + v_fda-2 + "|"
           + v_fda-1-a + "|" + v_fda-2-a + "|"
           ,
           ""
           ).                             /* RB-OTHER-PARAMETERS */



   OS-DELETE VALUE(RB-MEMO-FILE).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


