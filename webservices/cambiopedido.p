
/*------------------------------------------------------------------------
    File        : cambiopedido.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 12 09:59:28 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER SUC AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PALLET AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PEDIDO AS INTEGER NO-UNDO.

DEFINE BUFFER AUX_PAL FOR PALLETS.
DEFINE BUFFER BPEDIDOS FOR pedidos_packing.
DEFINE BUFFER BITEMSPEDIDOS FOR items_pedidos_packing.
DEFINE BUFFER BIT FOR items_pedidos_packing.


DEFINE VAR XTOTPAL AS INTEGER NO-UNDO.
DEFINE VAR x_uenoue AS LOGICAL NO-UNDO.
DEFINE VAR x_noueue AS LOGICAL NO-UNDO.
DEFINE VAR operacion AS CHARACTER NO-UNDO.
DEFINE VAR CPARAMETROS AS CHARACTER NO-UNDO.
DEFINE VAR RES AS CHARACTER NO-UNDO.

DEFINE VAR VDOC1 AS CHARACTER NO-UNDO.
DEFINE VAR VDOC2 AS CHARACTER NO-UNDO.
DEFINE VAR VPOSDOC1 AS CHARACTER NO-UNDO.
DEFINE VAR VPOSDOC2 AS CHARACTER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = SUC AND PALLETS.ID_PALLET = PALLET.

FIND FIRST  pedidos_packing WHERE
            pedidos_packing.id_empresa          = 1 AND
            pedidos_packing.id_punto_emisor     = 1 AND
            pedidos_packing.id_orden            = PEDIDO NO-LOCK.


        /*****************************/
        /* CONTROLA TOTAL DE PALLETS */
        /*****************************/
FOR EACH aux_pal OF pedidos_packing NO-LOCK WHERE
    aux_pal.estado AND
    aux_pal.merma = FALSE:

    xTotPal = xTotPal + 1.
END.
RELEASE aux_pal.

IF xTotPal >= pedidos_packing.total_pallets THEN
    RETURN ERROR "PEDIDO " + STRING(pedidos_packing.id_orden) +  " COMPLETO - NO SE PUEDE MOVER EL PALLET".



        
DO ON ERROR UNDO, LEAVE.        
                
                
    FIND FIRST items_pedidos_packing OF pedidos_packing WHERE
        items_pedidos_packing.id_articulo       = pallets.id_articulo       AND
        items_pedidos_packing.id_variedad       = pallets.id_variedad       AND
        items_pedidos_packing.id_marca          = pallets.id_marca          AND
        items_pedidos_packing.id_caract         = pallets.id_caract         AND
/*        items_pedidos_packing.id_tipo_pallet    = pallets.id_tipo_pallet    and */
        items_pedidos_packing.calibre           = pallets.calibre           AND
        items_pedidos_packing.bultos            = pallets.bultos            NO-LOCK NO-ERROR.
        
    IF AVAILABLE items_pedidos_packing THEN DO:
        
        
        FIND FIRST bpedidos WHERE
            bpedidos.id_empresa          = 1 AND
            bpedidos.id_punto_emisor     = 1 AND
            bpedidos.id_orden            = PALLETS.ID_ORDEN NO-LOCK NO-ERROR.
        
        IF AVAILABLE bpedidos THEN
            FIND FIRST bitemspedidos OF bpedidos WHERE
                bitemspedidos.id_articulo       = pallets.id_articulo       AND
                bitemspedidos.id_variedad       = pallets.id_variedad       AND
                bitemspedidos.id_marca          = pallets.id_marca          AND
                bitemspedidos.id_caract         = pallets.id_caract         AND
/*                bitemspedidos.id_tipo_pallet    = pallets.id_tipo_pallet    and */
                bitemspedidos.calibre           = pallets.calibre           AND
                bitemspedidos.bultos            = pallets.bultos            NO-LOCK NO-ERROR.


            
        x_uenoue    = FALSE.
        x_noueue    = FALSE.

                
        /****************************/
        /* CAMBIA CABECERA DEL PALLET */
        /****************************/

                 
        ASSIGN
            PALLETS.id_orden                = pedidos_packing.id_orden
            PALLETS.item                    = items_pedidos_packing.item
            PALLETS.id_envase               = items_pedidos_packing.id_envase
            PALLETS.id_calidad              = items_pedidos_packing.id_calidad
            PALLETS.id_categoria            = items_pedidos_packing.id_categoria
            PALLETS.contramarca             = items_pedidos_packing.contramarca
            pallets.id_tipo_pallet          = items_pedidos_packing.id_tipo_pallet.
                
        IF pedidos_packing.union_europea <> pallets.union_europea THEN DO:
            IF pedidos_packing.union_europea = FALSE AND pallets.union_europea THEN DO:
                ASSIGN
                    PALLETS.union_europea           = FALSE
                    PALLETS.id_finca_senasa         = 0
                    PALLETS.id_lote_senasa          = 0.
                    
                x_uenoue    = TRUE.
            END.
            ELSE DO:
                ASSIGN
                    PALLETS.union_europea           = TRUE
                x_noueue    = TRUE.
            END.
        END.

  
        /*************************/
        /* MODIFICA ITEMS DEL PALLET */
        /*************************/

        FOR EACH items_pallets OF pallets:

            
            ASSIGN
                ITEMS_PALLETS.id_orden      = pedidos_packing.id_orden
                ITEMS_PALLETS.item          = items_pedidos_packing.item
                ITEMS_PALLETS.id_envase     = items_pedidos_packing.id_envase
                ITEMS_PALLETS.id_calidad    = items_pedidos_packing.id_calidad
                ITEMS_PALLETS.id_categoria  = items_pedidos_packing.id_categoria
                ITEMS_PALLETS.contramarca   = items_pedidos_packing.contramarca.
            
            IF x_uenoue THEN DO:  
                ASSIGN
                    ITEMS_PALLETS.id_finca_senasa   = 0
                    ITEMS_PALLETS.id_lote_senasa    = 0
                    ITEMS_PALLETS.certificado       = "".
            END.
                
        END.

        ASSIGN
            pallets.id_estado_pallet    = 2.     /* Cambio de Pedido */


        FOR EACH cajas OF pallets:
            ASSIGN
                cajas.id_orden  = pedidos_packing.id_orden
                cajas.item      = items_pedidos_packing.item.
        END.

        DEFINE VAR X_MERCADO AS CHARACTER NO-UNDO.
        
        IF pedidos_packing.union_europea THEN
            X_MERCADO = 'UE'.
        ELSE
            X_MERCADO = 'NOUE'.
             
        IF pedidos_packing.CHINA THEN
            X_MERCADO = 'CHINA'.

        OPERACION = 'pp378ing.py'.
/*        CPARAMETROS =  pallets.id_pallet_sap + ',' + STRING(BPEDIDOS.id_pedido_sap) + ',' + STRING(BPEDIDOS.posicion_pedido_sap) + 
                       ',' + STRING(pedidos_packing.ID_PEDIDO_SAP) + ',' + STRING(pedidos_packing.posicion_pedido_sap).*/
    
        CPARAMETROS =  pallets.id_pallet_sap + ',' + STRING(pedidos_packing.ID_PEDIDO_SAP) + ',' + STRING(pedidos_packing.posicion_pedido_sap) + 
                       ',' + STRING(BPEDIDOS.id_pedido_sap)  + ',' + STRING(BPEDIDOS.posicion_pedido_sap) + ',' + X_MERCADO .
        RUN OPERACION.P (INPUT operacion , INPUT CPARAMETROS, OUTPUT  RES) NO-ERROR.  
      /* MESSAGE RETURN-VALUE RES . */
        IF ERROR-STATUS:ERROR THEN
            UNDO, RETURN ERROR RETURN-VALUE.   
        
        RELEASE pallets.
        RELEASE items_pallets.
    END.
    ELSE 
       UNDO, RETURN ERROR "ERROR - NO EXISTE ITEM EN EL PEDIDO " + string(PEDIDO,"zzzzzzzz9") + " PARA MOVER EL PALLET". 
END.


CATCH E AS Progress.Lang.Error :
    UNDO , RETURN ERROR E:GETMESSAGE(1).		
END CATCH. 