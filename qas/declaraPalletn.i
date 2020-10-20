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



PROCEDURE declaraPalletSapNuevo.

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
      
    IF bpallets.ID_PALLET_SAP <> '' THEN
        RETURN ERROR 'PALLET YA DECLARADO'.    

    IF bpallets.ESTADO = FALSE THEN
        RETURN ERROR 'PALLET ANULADO'.      
    
    IF NOT AVAILABLE bpallets THEN RETURN ERROR 'NO ENCONTRO EL PALLET A DECLARAR'.
                                                        
    RUN declaraPallet.p (bpallets.id_suc_trabajo , bpallets.id_pallet , 'QAS' , OUTPUT cStatus , OUTPUT CPALLET) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR 'Error en la interface con SAP.' + chr(13) + 'El error que se produce es : '  + RETURN-VALUE + CHR(13) +
                     'Intente de nuevo. En caso de persistir el error Avise al  rea de Sistemas'.

    FIND CURRENT BPALLETS EXCLUSIVE-LOCK .
    bpallets.id_pallet_sap = CPALLET.    
    FIND CURRENT BPALLETS NO-LOCK.

    RELEASE BPALLETS.
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

    RUN anulaPallet.P ( PALLETS.ID_PALLET_SAP , 'QAS' , OUTPUT cStatus) NO-ERROR.
    IF ERROR-STATUS:ERROR OR CSTATUS <> 'OK' THEN 
        RETURN ERROR 'Error de Anulacion Pallet SAP: ' + RETURN-VALUE + " " + cstatus .
    
    FIND CURRENT BPALLETS EXCLUSIVE-LOCK .
    bpallets.id_pallet_sap = ''.    
    FIND CURRENT BPALLETS NO-LOCK.  
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


