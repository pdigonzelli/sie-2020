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
DEFINE OUTPUT PARAMETER pcTarget AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE tt-ctacte
    FIELD ITEM            AS INTEGER
    FIELD fecha_salida    AS DATE
    FIELD observacion     AS CHAR
    FIELD id_viaje        AS INTEGER
    FIELD id_articulo     AS INTEGER
    FIELD producto        AS CHAR
    FIELD id_variedad     AS INTEGER COLUMN-LABEL "Cod.Variedad"
    FIELD variedad        AS CHAR COLUMN-LABEL "a"
    FIELD id_vapor        AS INTEGER
    FIELD vapor           AS CHAR
    FIELD fecha_arribo    AS DATE
    FIELD id_cliente      AS INTEGER
    FIELD razon_social    AS CHAR
    FIELD pallets         AS INTEGER
    FIELD nro_comprobante AS CHAR
    FIELD id_condicion    AS INTEGER
    FIELD condicion       AS CHAR
    FIELD fecha_comp      AS DATE
    FIELD importe_origen  AS DECIMAL DECIMALS 2
    FIELD suma_fija_venta AS DECIMAL
    FIELD acarga_venta     AS DECIMAL
    FIELD contradoc_venta  AS DECIMAL
    FIELD arribo_venta     AS DECIMAL
    FIELD flete_venta      AS DECIMAL
    FIELD saldo_venta      AS DECIMAL
    FIELD TOTAL_venta      AS DECIMAL DECIMALS 2
    FIELD suma_fija AS DECIMAL
    FIELD acarga     AS DECIMAL
    FIELD contradoc  AS DECIMAL
    FIELD arribo     AS DECIMAL
    FIELD flete      AS DECIMAL
    FIELD saldo      AS DECIMAL
    FIELD TOTAL     AS DECIMAL
    FIELD suma_fija_cob AS DECIMAL
    FIELD acarga_cob     AS DECIMAL
    FIELD contradoc_cob  AS DECIMAL
    FIELD arribo_cob     AS DECIMAL
    FIELD flete_cob      AS DECIMAL
    FIELD saldo_cob      AS DECIMAL
    FIELD TOTAL_cob      AS DECIMAL DECIMALS 2
    FIELD monto_cobranza  AS DECIMAL
    FIELD suma_fija_exigible AS DECIMAL
    FIELD acarga_exigible     AS DECIMAL
    FIELD contradoc_exigible  AS DECIMAL
    FIELD arribo_exigible     AS DECIMAL
    FIELD flete_exigible      AS DECIMAL
    FIELD saldo_exigible      AS DECIMAL
    FIELD TOTAL_exigible      AS DECIMAL
    FIELD saldo_total_gestion AS DECIMAL
    FIELD saldo_total_contable AS DECIMAL
    FIELD origen AS CHAR
    FIELD moneda AS CHAR
    FIELD bultos AS DECIMAL
    FIELD fecha_contradoc AS DATE
    FIELD fecha_saldoarribo AS DATE
    FIELD pall_programa AS INTEGER
    INDEX clientes_item IS PRIMARY UNIQUE 
           id_cliente
           ITEM.

DEFINE VARIABLE vpallfact  AS INTEGER    NO-UNDO.
DEFINE VARIABLE vbultfact  AS INTEGER    NO-UNDO.
DEFINE VARIABLE vfobfact   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfletefact AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vtotfact   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vfob  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vpall AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vbult AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vfobventa    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vpallventa   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vbultventa   AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vacargaventa AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdocventa    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE varriboventa AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfijaventa   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vsaldoventa  AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vacarga AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdoc    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE varribo AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfija   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vsaldo  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfechasaldo AS DATE       NO-UNDO.

DEFINE VARIABLE vacargacob   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdoccob      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE varribocob   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfijacob     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vsaldocob    AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vacargaexigible AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vdocexigible    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE varriboexigible AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfijaexigible   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vsaldoexigible  AS DECIMAL    NO-UNDO.

DEFINE VARIABLE vfobtotal  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vtotalcob  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vfacttotal AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vimpocob   AS DECIMAL    NO-UNDO.


DEFINE VARIABLE vprecio       AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vobs          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i             AS INTEGER    NO-UNDO.
DEFINE VARIABLE vpallprograma AS INTEGER    NO-UNDO.
DEFINE VARIABLE vaux          AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vincumplido AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vcumplido   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE vorigen     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vnrocomp    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vtipofac    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vmoneda     AS CHARACTER  NO-UNDO.
DEFINE BUFFER b_subd FOR subd_vtas.
DEFINE BUFFER b_subd_aux FOR subd_vtas.
DEFINE BUFFER b_cond FOR cond_pagos_clientes.
DEFINE BUFFER b_itemscond FOR items_cond_pagos_clientes.
DEFINE BUFFER b_condaux FOR cond_pagos_clientes.
DEFINE BUFFER b_itemscondaux FOR items_cond_pagos_clientes.

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

RUN createTT.
RUN exportaTT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-createTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTT Procedure 
PROCEDURE createTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
vobs = "".
i = 1.

FOR EACH tt-ctacte:
    DELETE tt-general.
END.


FOR EACH ITEM_viajes WHERE ITEM_viajes.id_articulo = 1,
       FIRST viajes WHERE viajes.id_viaje   = ITEM_viajes.id_viaje AND
                          viajes.id_cliente =100350 AND
                          viajes.id_campana  = 2004 AND
                          viajes.id_compania = 1
       NO-LOCK BREAK BY viajes.id_cliente
                     BY ITEM_viajes.id_viaje
                     BY ITEM_viajes.id_articulo
                     BY ITEM_viajes.id_variedad
                     BY ITEM_viajes.id_envase
                     BY ITEM_viajes.id_tipo_pallet
                     BY ITEM_viajes.id_calidad
                     BY ITEM_viajes.id_marca
                     BY ITEM_viajes.calibre:
 
        IF FIRST-OF(viajes.id_cliente) THEN DO:
        
            /* Armo Cronograma 
              RUN createCronoTT (OUTPUT vincumplido,
                                 OUTPUT vcumplido).
              vaux = vincumplido.
             Armo Cronograma */
        END.

        /* Captura de la moneda del Cliente */  
          FIND cons_mone WHERE cons_mone.id_cliente = viajes.id_cliente NO-LOCK NO-ERROR.
          IF AVAILABLE cons_mone THEN DO:
            FIND tipo_moneda OF cons_mone NO-LOCK NO-ERROR.
            vmoneda = IF AVAILABLE tipo_moneda THEN tipo_moneda.descripcion ELSE "SinInfo".
          END.
        /* Captura de la moneda del Cliente */  
     
     /* Captura de Datos de Facturacion: Nro. Comprobantes 
        IF FIRST-OF(item_viajes.id_viaje) THEN DO:
            FOR EACH r_subd_ventas_viajes WHERE r_subd_ventas_viajes.id_viaje = viajes.id_viaje NO-LOCK:
               FIND b_subd_aux WHERE b_subd_aux.id_tipocomp  = r_subd_ventas_viajes.id_tipocomp  AND
                                     b_subd_aux.id_operacion = r_subd_ventas_viajes.id_operacion AND
                                     b_subd_aux.nromov       = r_subd_ventas_viajes.nromov  NO-LOCK NO-ERROR.
               IF AVAILABLE b_subd_aux THEN DO:
                             
                  RUN getTipoFactura.p (ROWID(b_subd_aux),
                                        OUTPUT vtipofac).

                  vnrocomp = vnrocomp + 
                          (IF b_subd_aux.nro_proforma = 0 THEN 
                             "(" + vtipofac + ")" + string(b_subd_aux.id_punto_venta) + "-" + string(b_subd_aux.nro_comp) 
                          ELSE 
                             "(" + vtipofac + ")" + string(b_subd_aux.id_punto_venta) + "-" + string(b_subd_aux.nro_proforma)) + "  ".
                             
               END.
            END.
        END.
       Datos de Facturacion: Nro. Comprobantes */        
      
     /* Facturacion */
         vpallfact  = vpallfact  + ITEM_viajes.tot_pall_fact.
         vbultfact  = vbultfact  + ITEM_viajes.tot_bultos_fact.
         vfobfact   = vfobfact   + ITEM_viajes.fob_mo.
         vfletefact = vfletefact + item_viajes.tot_mo_flete.
         vtotfact   = vtotfact   + item_viajes.tot_mo_fact.
     /* Facturacion */                            

     /* Venta de Gestion          */
         RUN getFobMO.p (BUFFER ITEM_viajes,
                         INPUT TODAY,
                         OUTPUT vfob,
                         OUTPUT vpall,
                         OUTPUT vbult).

         IF vfob <> 0  THEN
         DO:
             vorigen = "Liq". 
             vfobventa  =  vfobventa + ((vfob / vbult) * ITEM_viajes.tot_bultos_fact).
             vpallventa = vpallventa + vpall.
             vbultventa = vbultventa + vbult.
         END.


         IF vfob = 0  THEN DO:
            
            find last fobs where fobs.id_viaje    = viajes.id_viaje AND 
                                 fobs.id_articulo = item_viajes.id_articulo and
                                 fobs.id_variedad = item_viajes.id_variedad AND 
                                 FOBS.id_envase   = ITEM_viajes.id_envase use-index fecha no-lock no-error.
            IF AVAILABLE fobs THEN DO:
                vorigen = "Obj".
                vfobventa  = vfobventa  + (ITEM_viajes.tot_bultos_fact  * fobs.fob_objetivo).
                vpallventa = vpallventa + ITEM_viajes.tot_pall_fact .
                vbultventa = vbultventa + ITEM_viajes.tot_bultos_fact .
            END.
            ELSE DO:
                vorigen = "Fac".
                vprecio = ITEM_viajes.fob_mo / ITEM_viajes.tot_bultos_fact.
                vfobventa  = vfobventa  + (ITEM_viajes.tot_bultos_fact * vprecio) .
                vpallventa = vpallventa + ITEM_viajes.tot_pall_fact .
                vbultventa = vbultventa + ITEM_viajes.tot_bultos_fact .
            END. 
         END.
         /* Venta de Gestion */

     IF LAST-OF(ITEM_viajes.id_variedad) THEN DO:

         FIND clausula OF viajes NO-LOCK NO-ERROR.
         FIND clientes OF viajes NO-LOCK NO-ERROR.
         FIND FIRST PRODUCTOS_TERMINADOS WHERE PRODUCTOS_TERMINADOS.ID_ARTICULO = ITEM_viajes.id_articulo NO-LOCK NO-ERROR.
         FIND FIRST VARIEDADES WHERE VARIEDADES.ID_VARIEDAD = ITEM_viajes.id_variedad NO-LOCK NO-ERROR. 
         FIND FIRST VAPORES WHERE VAPORES.ID_VAPOR = viajes.id_vapor NO-LOCK NO-ERROR. 

         /* Programa total de Pallets para el Cliente 
         RUN getTotalPrograma (OUTPUT vpallprograma). */

         /* Condiciones y formas de Pago de Venta de Gestion  */
         FIND b_cond WHERE b_cond.id_cliente  = viajes.id_cliente  AND
                           b_cond.id_campana  = viajes.id_campana  AND
                           b_cond.id_articulo = ITEM_viajes.id_articulo NO-LOCK NO-ERROR.
         IF AVAILABLE b_cond THEN DO:
         
                 /* Montos Cobrandos desagregados */
                 RUN getDatosCobranzas (OUTPUT vfijacob,
                                        OUTPUT vacargacob,
                                        OUTPUT vdoccob,
                                        OUTPUT varribocob,
                                        OUTPUT vsaldocob).
                 /* Montos Cobrandos desagregados */

                 /*  
                 RUN getDatosVentaGestion (INPUT vpallprograma,
                                           INPUT vfobventa,
                                           INPUT vpallfact,
                                           INPUT vbultfact,
                                           INPUT vfijacob + vacargacob + vdoccob + varribocob + vsaldocob,
                                           OUTPUT vobs,
                                           OUTPUT vfijaventa,
                                           OUTPUT vacargaventa,
                                           OUTPUT vdocventa,
                                           OUTPUT varriboventa,
                                           OUTPUT vsaldoventa,
                                           OUTPUT vsaldoexigible,
                                           OUTPUT vfechasaldo).
                   
                 
                    
                IF vfijaventa < vaux THEN DO:
                    vfija = vfijaventa.
                    vaux = vaux - vfijaventa.    
                END.
                ELSE DO:
                    vfija = vaux.
                    vaux = 0.
                END.


                 RUN getDatosVentaExigible (INPUT vfobventa,
                                            INPUT vpallfact,
                                            INPUT vbultfact,
                                            INPUT vfija,
                                            OUTPUT vfija,
                                            OUTPUT vacarga,
                                            OUTPUT vdoc,
                                            OUTPUT varribo,
                                            OUTPUT vsaldo).
                                                             
                   */
         END.
         

        /* Saldos de Gestion Exigibles 
        vfijaexigible   = vfija - vfijacob.
        vacargaexigible = vacarga - vacargacob.
        vdocexigible    = vdoc - vdoccob.
        varriboexigible = varribo - varribocob.
        vsaldoexigible  = vsaldo - vsaldocob.
         Valores Exigibles */


        CREATE tt-general.
        ASSIGN
              tt-general.ITEM            = i
              tt-general.id_viaje        = viajes.id_viaje
              tt-general.id_articulo     = ITEM_viajes.id_articulo
              tt-general.producto        = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "Sininfo"
              tt-general.id_variedad     = ITEM_viajes.id_variedad
              tt-general.variedad        = IF AVAILABLE variedades THEN variedades.descripcion ELSE "SinInfo"
              tt-general.id_vapor        = viajes.id_vapor
              tt-general.vapor           = IF AVAILABLE vapores THEN vapores.descripcion ELSE "SinInfo"
              tt-general.fecha_salida    = viajes.fecha_salida
              tt-general.fecha_arribo    = viajes.fecha_arribo
              tt-general.id_cliente      = viajes.id_cliente
              tt-general.razon_social    = clientes.razon_social
              tt-general.pallets         = vpallfact
              tt-general.bultos          = vbultfact
              tt-general.nro_comprobante = vnrocomp
              tt-general.fecha_comp      = ?
              tt-general.id_condicion    = viajes.id_clausula
              tt-general.condicion       = IF AVAILABLE clausula THEN clausula.descripcion ELSE "SinInfo"
              tt-general.importe_origen  = vtotfact
              tt-general.suma_fija_venta = vfijaventa
              tt-general.acarga_venta    = vacargaventa
              tt-general.contradoc_venta = vdocventa
              tt-general.arribo_venta    = varriboventa
              tt-general.flete_venta     = vfletefact
              tt-general.saldo_venta     = vsaldoventa
              tt-general.TOTAL_venta     = vfobventa
              tt-general.suma_fija      = vfija
              tt-general.acarga         = vacarga
              tt-general.contradoc      = vdoc
              tt-general.arribo         = varribo
              tt-general.flete          = 0
              tt-general.saldo          = vsaldo
              tt-general.TOTAL          = vfija + vacarga + vdoc + varribo + vsaldo
              tt-general.suma_fija_cob   = vfijacob
              tt-general.acarga_cob      = vacargacob
              tt-general.contradoc_cob   = vdoccob
              tt-general.arribo_cob           = varribocob
              tt-general.saldo_cob            = vsaldocob
              tt-general.TOTAL_cob            = vfijacob + vacargacob + vdoccob + varribocob + vsaldocob
              tt-general.observacion          = vobs
              tt-general.suma_fija_exigible   = vfijaexigible
              tt-general.acarga_exigible      = vacargaexigible
              tt-general.contradoc_exigible   = vdocexigible
              tt-general.arribo_exigible      = varriboexigible
              tt-general.saldo_exigible       = vsaldoexigible
              tt-general.TOTAL_exigible       = vfijaexigible + vacargaexigible + vdocexigible + varriboexigible + vsaldoexigible
              tt-general.origen               = vorigen
              tt-general.moneda               = vmoneda
              tt-general.fecha_contradoc      = viajes.fecha_salida + 7
              tt-general.fecha_saldoarribo    = vfechasaldo
              tt-general.pall_programa        = vpallprograma.
              
              
              /*
              tt-general.saldo_total_gestion  = vfobventa - (vfijacob + vacargacob + vdoccob + varribocob + vsaldocob)
              tt-general.saldo_total_contable = vtotfact  - (vfijacob + vacargacob + vdoccob + varribocob + vsaldocob).   */
              
              i = i + 1.

              /* Totales por Cliente */
              vfobtotal  = vfobtotal + vfobventa.
              vtotalcob  = vtotalcob + (vfijacob + vacargacob + vdoccob + varribocob + vsaldocob).
              vfacttotal = vfacttotal + vtotfact.

              IF LAST-OF(viajes.id_cliente) THEN DO:
                  FIND FIRST parametros_cons NO-LOCK NO-ERROR.
                  vimpocob = 0.  
                  FOR EACH cobranzas_me WHERE cobranzas_me.id_cliente = viajes.id_cliente AND
                                              cobranzas_me.id_campana = 2004          AND
                                              cobranzas_me.fecha_comp <= TODAY          AND
                                              cobranzas_me.id_compania = 1      AND
                                              cobranzas_me.id_operacion = parametros_cons.id_operacion_fruta AND
                                              cobranzas_me.estado NO-LOCK:
                      vimpocob = vimpocob + cobranzas_me.saldo.
                  END.
              /*
                  CREATE tt-general.
                  ASSIGN
                    tt-general.ITEM = i 
                    tt-general.id_viaje        = viajes.id_viaje
                    tt-general.id_articulo     = ITEM_viajes.id_articulo
                    tt-general.producto        = IF AVAILABLE productos_terminados THEN productos_terminados.descripcion ELSE "Sininfo"
                    tt-general.id_variedad     = ITEM_viajes.id_variedad
                    tt-general.variedad        = IF AVAILABLE variedades THEN variedades.descripcion ELSE "SinInfo"
                    tt-general.id_vapor        = viajes.id_vapor
                    tt-general.vapor           = IF AVAILABLE vapores THEN vapores.descripcion ELSE "SinInfo"
                    tt-general.fecha_salida    = today
                    tt-general.fecha_arribo    = today
                    tt-general.id_cliente      = viajes.id_cliente
                    tt-general.razon_social    = clientes.razon_social
                    tt-general.observacion     = vobs
                    tt-general.vapor           = "SubTotal x Cliente".
                */

                  ASSIGN
                    tt-general.monto_cobranza       = vimpocob
                    tt-general.saldo_total_gestion  = /*(vimpocob + vtotalcob) * -1*/ vfobtotal - (vtotalcob + vimpocob)
                    tt-general.saldo_total_contable = /*(vimpocob + vtotalcob) * -1*/ vfacttotal - (vtotalcob + vimpocob).

                    vtotalcob  = 0.
                    vfobtotal  = 0.
                    vfacttotal = 0.

                    i = 1.

              END.
              /* Totales por Cliente */
                

       vpallfact    = 0.
       vbultfact    = 0.
       vfobfact     = 0.
       vfletefact   = 0.
       vtotfact     = 0.
       vfijaventa   = 0.
       vacargaventa = 0.
       vdocventa    = 0.
       varriboventa = 0.
       vsaldoventa  = 0.
       vfija   = 0.
       vacarga = 0.
       vdoc    = 0.
       varribo = 0.
       vsaldo  = 0.
       vfijaexigible   = 0.
       vacargaexigible = 0.
       vdocexigible    = 0.
       varriboexigible = 0.
       vsaldoexigible  = 0.
       vfijacob   = 0.
       vacargacob = 0.
       vdoccob    = 0.
       varribocob = 0.
       vsaldocob  = 0.
       vfobventa    = 0.
       vobs         = "".
       vpallventa   = 0.
       vbultventa   = 0.
       vnrocomp     = "".
       vmoneda      = "".
       vorigen      = "".
       vfechasaldo  = ?.
     END. /* IF LAST-OF(ITEM_viajes.id_variedad)*/

 END.
                                            
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exportaTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportaTT Procedure 
PROCEDURE exportaTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

OUTPUT TO pcTarget.
 FOR EACH tt-general.
     EXPORT DELIMITER ";"
           tt-general.id_vapor
           tt-general.vapor
           tt-general.id_cliente
           tt-general.razon_social
           tt-general.fecha_salida
           tt-general.fecha_arribo
           tt-general.importe_origen FORMAT "->>>,>>>,>>9.99"/*facturado*/
           tt-general.TOTAL_venta    FORMAT "->>>,>>>,>>9.99"/*venta gestion*/
           tt-general.TOTAL_cob      FORMAT "->>>,>>>,>>9.99"/*cob*/
           tt-general.TOTAL_venta - tt-general.TOTAL_cob FORMAT "->>>,>>>,>>9.99" /*exigible*/
           tt-general.id_articulo
           tt-general.producto
           tt-general.bultos
           tt-general.pallets
           1 /*id_moneda*/
           tt-general.moneda.
 END.
 OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

