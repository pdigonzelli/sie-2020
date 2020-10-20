
/*------------------------------------------------------------------------
    File        : VEOPALLETX.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Mar 26 19:45:11 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


DEFINE VAR I AS INTEGER NO-UNDO.

define var ipallet like pallets.id_pallet no-undo.


repeat:
    update ipallet with frame k.


    FOR LAST PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND /*PALLETS.ID_PALLET_sap >= '01000000000000002031' AND PALLETS.ID_PALLET > 16000000*/
        PALLETS.id_pallet = iPallet.
    

        disp pallets except pallets.id_orden pallets.id_calidad with frame x.
        update pallets.id_pallet_sap format 'x(30)' with frame x.
    
    /*
    disp pallets with frame x.
    
    for each items_pallets of pallets.
        disp items_pallets.bultos items_pallets.ubicacion with frame y.
        update items_pallets.ubicacion format 'x(20)' with frame y.
    end.    
    */
    /*
    assign id_pallet_sap = '01000000000000003439'.01000000000000005781
    FOR LAST CAJAS OF PALLETS NO-LOCK , EACH volcado_packing OF CAJAS NO-LOCK, 
            EACH items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                                items_stock.id_tipo_movsto = 70 AND
                                items_stock.nro_partida_serial = volcado_packing.nro_partida_serial BY 
            ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&').
        DISP ENTRY(3,ENTRY(2,ENTRY(1,items_stock.documento_sap,';'),'|'),'&') FORMAT 'X(30)'.
    END.
    */    
    END.
END.

/*
    DISP PALLETS.ID_PALLET PALLETS.ID_ORDEN FORMAT '999999999999999' pallets.id_pallet_sap FORMAT 'X(25)'.
assign id_pallet_sap = '1000000000000003439'.
FOR EACH cajas OF PALLETS WHERE cajas.id_testigo = 0 NO-LOCK:
    FIND volcado_packing OF CAJAS NO-LOCK.
    FIND FIRST items_stock WHERE  items_stock.nro_partida = volcado_packing.nro_partida AND
                            items_stock.id_tipo_movsto = 70 AND
                            items_stock.nro_partida_serial = volcado_packing.nro_partida_serial NO-LOCK NO-ERROR.
                            
   IF NOT AVAILABLE items_stock THEN 
        MESSAGE 'ERROR PARTIDA'
                volcado_packing.nro_volcado 
                volcado_packing.nro_partida 
                volcado_packing.nro_partida_serial VIEW-AS ALERT-BOX.  
    I = I + 1.
    DISP cajas.id_caja I. */