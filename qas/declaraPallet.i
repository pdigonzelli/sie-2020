&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  ****************************/

PROCEDURE declaraPalletSap.

    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR iEstado AS INTEGER NO-UNDO.
    DEFINE VAR cStatus AS CHARACTER NO-UNDO.
    DEFINE VAR cPallet AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.


    FIND bpallets WHERE
        bpallets.id_suc_trabajo = PISUC AND
        bpallets.id_pallet      = PIPALLET NO-ERROR.
  
    IF NOT AVAILABLE bpallets THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.


    RUN palletsToSap IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT cStatus) NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR 'ERROR EN PALLET TO SAP: ' + RETURN-VALUE. 

    RUN procesarPallet IN hLib (bpallets.id_pallet_sap , OUTPUT iEstado) NO-ERROR.     
    
    bpallets.status_sap = iEstado.
    
    RUN regrabaPalletProgress IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT iEstado , OUTPUT cPallet) NO-ERROR.
    

    IF bpallets.id_pallet_sap <> cPallet AND bpallets.id_pallet_sap = '' THEN
        bpallets.id_pallet_sap = cPallet.
    
    IF bpallets.status_sap <> iEstado AND bpallets.status_sap = 0 THEN
        bpallets.status_sap = iEstado.

 
    RETURN STRING(bpallets.status_sap).


END PROCEDURE.

PROCEDURE declaraPalletSapNuevo.

    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR iEstado AS INTEGER NO-UNDO.
    DEFINE VAR cStatus AS CHARACTER NO-UNDO.
    DEFINE VAR cPallet AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.
    DEFINE BUFFER bpallets1 FOR pallets.
    DEFINE VAR hAppSrv AS HANDLE NO-UNDO.
    DEFINE VAR RET AS LOGICAL NO-UNDO.
    
    FIND bpallets WHERE
        bpallets.id_suc_trabajo = PISUC AND
        bpallets.id_pallet      = PIPALLET NO-LOCK NO-ERROR.
  
    IF NOT AVAILABLE bpallets THEN RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.
                      
    CREATE SERVER hAppSrv.


    ret = hAppSrv:CONNECT("-H samiprogdesa -S 5162 -AppService sap3").

    
    FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = 16000008 NO-LOCK.

    RUN  PP159INGP1.P  ON hAppSrv  TRANSACTION DISTINCT  (pallets.id_suc_trabajo, pallets.id_pallet, OUTPUT CSTATUS)NO-ERROR.
                      
                                                        
    RUN pp159ingp1.p IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT cStatus) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR 'Error en la interface con SAP.' + chr(13) + 'El error que se produce es : '  + RETURN-VALUE + CHR(13) +
                     'Intente de nuevo. En caso de persistir el error Avise al  rea de Sistemas'.

    RETURN.


    CATCH E AS Progress.Lang.Error :
    	RETURN ERROR E:GETMESSAGE(1).	
    END CATCH.                     

    FINALLY.
        IF hAppSrv:CONNECTED () THEN
            ret = hAppSrv:DISCONNECT().
        DELETE OBJECT hAppSrv.
    END FINALLY.

END PROCEDURE.


PROCEDURE declaraPalletSapNuevoCalidad.

    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR iEstado AS INTEGER NO-UNDO.
    DEFINE VAR cStatus AS CHARACTER NO-UNDO.
    DEFINE VAR cPallet AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.
    DEFINE BUFFER bpallets1 FOR pallets.

    FIND bpallets WHERE
        bpallets.id_suc_trabajo = PISUC AND
        bpallets.id_pallet      = PIPALLET NO-LOCK NO-ERROR.
  
    IF NOT AVAILABLE bpallets THEN RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.
                                                        
    RUN palletsToSapNuevoCalidad IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT cStatus , OUTPUT cPallet ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR 'Error en la interface con SAP.' + chr(13) + 'El error que se produce es : '  + RETURN-VALUE + CHR(13) +
                     'Intente de nuevo. En caso de persistir el error Avise al  rea de Sistemas'.

    FIND FIRST bpallets1 WHERE bpallets1.id_pallet_sap = cPallet NO-LOCK NO-ERROR.
    IF AVAILABLE bpallets1 THEN
    RETURN ERROR 'No se genera Pallet.El lote SAP ' + cPallet + CHR(13) + ' ya existe en pallet ' +
                 STRING(bpallets.id_suc_trabajo) + '-' + STRING(bpallets.id_pallet) + CHR(13) +
                 'Intente de nuevo. En caso de persistir el error Avise al  rea de Sistemas'. 

    FIND CURRENT BPALLETS EXCLUSIVE-LOCK .
    bpallets.id_pallet_sap = cPallet.    
    FIND CURRENT BPALLETS NO-LOCK.

    RELEASE BPALLETS.
    IF cStatus <> "T" OR cPallet = "" THEN
        RETURN ERROR 'Error en parametros SAP.' + chr(13) +
                     'El error que se produce es : '  + cStatus + CHR(13) +
                     'o el lote es devuelto vac¡o ' + chr(13) + 
                     'Intente de nuevo. En caso de persistir el error Avise al  rea de Sistemas'. 
    
        
    
    RETURN.

END PROCEDURE.


PROCEDURE anulaPalletSap.
    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR cStatus AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.

    FIND bpallets WHERE
         bpallets.id_suc_trabajo = PISUC AND
         bpallets.id_pallet      = PIPALLET NO-LOCK NO-ERROR.

    IF NOT AVAILABLE bpallets THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A ANULAR'.

    find items_pedidos_packing of bpallets no-lock no-error.
    IF NOT AVAILABLE items_pedidos_packing  THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.

    RUN anulaPalletSap IN hlib ( items_pedidos_packing.material_sap + pallets.id_pallet_sap, TODAY , OUTPUT cStatus) NO-ERROR.
    IF ERROR-STATUS:ERROR OR CSTATUS <> 'OK' THEN 
        RETURN ERROR 'Error de Anulacion Pallet SAP: ' + RETURN-VALUE + " " + cstatus .
    
    RETURN.

END PROCEDURE.


PROCEDURE anulaPalletSapCalidad.
    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR cStatus AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.

    FIND bpallets WHERE
         bpallets.id_suc_trabajo = PISUC AND
         bpallets.id_pallet      = PIPALLET NO-LOCK NO-ERROR.

    IF NOT AVAILABLE bpallets THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A ANULAR'.

    find items_pedidos_packing of bpallets no-lock no-error.
    IF NOT AVAILABLE items_pedidos_packing  THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.

    RUN anulaPalletSap IN hlib ( items_pedidos_packing.material_sap + pallets.id_pallet_sap, TODAY , OUTPUT cStatus) NO-ERROR.
    IF ERROR-STATUS:ERROR OR CSTATUS <> 'OK' THEN 
        RETURN ERROR 'Error de Anulacion Pallet SAP: ' + RETURN-VALUE + " " + cstatus .
    
    RETURN.

END PROCEDURE.

PROCEDURE declaraPalletSapPrueba.

    DEFINE INPUT PARAMETER PISUC AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER PIPALLET AS INTEGER NO-UNDO.

    DEFINE VAR iEstado AS INTEGER NO-UNDO.
    DEFINE VAR cStatus AS CHARACTER NO-UNDO.
    DEFINE VAR cPallet AS CHARACTER NO-UNDO.

    DEFINE BUFFER bpallets FOR pallets.


    FIND bpallets WHERE
        bpallets.id_suc_trabajo = PISUC AND
        bpallets.id_pallet      = PIPALLET NO-ERROR.
  
    IF NOT AVAILABLE bpallets THEN
        RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.

    OUTPUT TO "z:\temp\intefacePalletPrueba.txt" APPEND.

    ETIME(TRUE).

    PUT UNFORMATTED "palletstosap|" STRING(bpallets.id_suc_trabajo, "9999") + "-" + STRING(bpallets.id_pallet,"99999999") + "|" + STRING(ETIME) + CHR(10).

    RUN palletsToSapPrueba IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT cStatus) NO-ERROR.    
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR 'ERROR EN PALLET TO SAP: ' + RETURN-VALUE. 

    PUT UNFORMATTED "palletstosap|" STRING(bpallets.id_suc_trabajo, "9999") + "-" + STRING(bpallets.id_pallet,"99999999") + "|" + STRING(ETIME)  + CHR(10).

    RUN procesarPalletPrueba IN hLib (bpallets.id_pallet_sap , OUTPUT iEstado) NO-ERROR. 
    
    PUT UNFORMATTED "procesarpallet|" STRING(bpallets.id_suc_trabajo, "9999") + "-" + STRING(bpallets.id_pallet,"99999999") + "|" + STRING(ETIME) + CHR(10).
    
    bpallets.status_sap = iEstado.
    
    RUN regrabaPalletProgressPrueba IN hLib (bpallets.id_suc_trabajo , bpallets.id_pallet , OUTPUT iEstado , OUTPUT cPallet) NO-ERROR.

    PUT UNFORMATTED "regrabaPallet|" STRING(bpallets.id_suc_trabajo, "9999") + "-" + STRING(bpallets.id_pallet,"99999999") + "|" + STRING(ETIME) + CHR(10).

    OUTPUT CLOSE.
    IF bpallets.id_pallet_sap <> cPallet AND bpallets.id_pallet_sap = '' THEN
        bpallets.id_pallet_sap = cPallet.
    
    IF bpallets.status_sap <> iEstado AND bpallets.status_sap = 0 THEN
        bpallets.status_sap = iEstado.

 
    RETURN STRING(bpallets.status_sap).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


