
/*------------------------------------------------------------------------
    File        : importalotes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 05 12:40:50 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE TTLOTES
    FIELD UMP AS CHARACTER
    FIELD HU AS CHARACTER 
    FIELD PALLET AS CHARACTER
    FIELD POSICION AS INTEGER
    FIELD C AS CHARACTER
    FIELD ENT AS CHARACTER
    FIELD POSENTREGA AS CHARACTER
    FIELD MATERIAL AS CHARACTER
    FIELD CE AS CHARACTER 
    FIELD CANTIDAD AS CHARACTER
    FIELD UM AS CHARACTER
    FIELD LOTE AS CHARACTER FORMAT 'X(20)'.
    
DEFINE TEMP-TABLE TBUENOS 
    FIELD SUC AS INTEGER
    FIELD PALLET AS INTEGER
    FIELD POSICION AS INTEGER
    FIELD ITEMPALLET AS INTEGER
    FIELD BULTOS AS INTEGER
    FIELD CANTIDAD AS INTEGER
    FIELD LOTE AS CHARACTER.

DEFINE TEMP-TABLE TMALOS
    FIELD SUC AS CHARACTER
    FIELD PALLET AS CHARACTER FORMAT 'X(30)'
    FIELD HU AS CHARACTER.

DEFINE VAR FLAG-ENCONTRO AS LOGICAL NO-UNDO.
    
INPUT FROM 'HU2.CSV'.

SESSION:NUMERIC-FORMAT = 'EUROPEAN'.

REPEAT:
    CREATE TTLOTES.
    IMPORT DELIMITER ';' TTLOTES.
/*    DISP TTLOTES.PALLET FORMAT 'X(12)'  integer(SUBSTRING(ttlotes.pallet,3)) TTLOTES.POSICION. */
    FLAG-ENCONTRO = FALSE.
    
    FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = integer(SUBSTRING(ttlotes.pallet,3)) NO-ERROR.
    IF NOT AVAILABLE PALLETS THEN 
    DO:
        CREATE TMALOS.
        ASSIGN TMALOS.PALLET = TTLOTES.PALLET.
        NEXT.         
    END.

    FIND items_pallets OF PALLETS WHERE items_pallets.item_pallet = TTLOTES.POSICION NO-ERROR.

    IF AVAILABLE items_pallets AND items_pallets.BULTOS = INTEGER(DECIMAL(TTLOTES.CANTIDAD)) THEN
    DO:
        FLAG-ENCONTRO = TRUE.
        CREATE TBUENOS.
        ASSIGN TBUENOS.SUC = items_pallets.id_suc_trabajo 
               TBUENOS.PALLET = items_pallets.id_pallet
               TBUENOS.ITEMPALLET = items_pallets.item_pallet
               TBUENOS.BULTOS = items_pallets.BULTOS
               TBUENOS.CANTIDAD = INTEGER(DECIMAL(TTLOTES.CANTIDAD))
               TBUENOS.POSICION = TTLOTES.POSICION
               TBUENOS.LOTE = TTLOTES.LOTE.
        NEXT.
    END.
    
    IF FLAG-ENCONTRO THEN NEXT.
    
    FOR EACH items_pallets OF PALLETS WHERE items_pallets.BULTOS = INTEGER(DECIMAL(TTLOTES.CANTIDAD)) NO-LOCK.
        FIND FIRST TBUENOS WHERE TBUENOS.SUC = items_pallets.id_suc_trabajo AND 
                                 TBUENOS.PALLET = items_pallets.id_pallet   AND 
                                 TBUENOS.ITEMPALLET = items_pallets.item_pallet NO-LOCK NO-ERROR.
        IF NOT AVAILABLE TBUENOS THEN
        DO:
            FLAG-ENCONTRO = TRUE.
            CREATE TBUENOS.
            ASSIGN TBUENOS.SUC = items_pallets.id_suc_trabajo 
                   TBUENOS.PALLET = items_pallets.id_pallet
                   TBUENOS.ITEMPALLET = items_pallets.item_pallet
                   TBUENOS.BULTOS = items_pallets.BULTOS
                   TBUENOS.CANTIDAD = INTEGER(DECIMAL(TTLOTES.CANTIDAD))
                   TBUENOS.POSICION = TTLOTES.POSICION
                   TBUENOS.LOTE = TTLOTES.LOTE.
            LEAVE.
        END.        
    END.
   
    IF NOT FLAG-ENCONTRO THEN
    DO:
        CREATE TMALOS.
        ASSIGN TMALOS.PALLET = TTLOTES.PALLET
               TMALOS.HU = TTLOTES.HU
               TMALOS.SUC = '98'.
        NEXT.         
    END.

END.     

OUTPUT CLOSE.
CURRENT-WINDOW:WIDTH = 130.
OUTPUT TO D:\TEMP\LOTEMALOS.TXT.
FOR EACH TMALOS BREAK BY(TMALOS.PALLET).
    IF LAST-OF(TMALOS.PALLET) THEN
        EXPORT DELIMITER ';' TMALOS.
END.
OUTPUT CLOSE.

OUTPUT TO D:\TEMP\LOTESBUENOS1.TXT.
FOR EACH TBUENOS.
    EXPORT DELIMITER ';' TBUENOS.
    FIND items_pallets WHERE items_pallets.id_suc_trabajo = TBUENOS.SUC AND
                             items_pallets.id_pallet = TBUENOS.PALLET AND
                             items_pallets.item_pallet = TBUENOS.ITEMPALLET NO-ERROR.
    items_pallets.ubicacion = TBUENOS.LOTE.  
END.
OUTPUT CLOSE.
                      
FOR EACH TBUENOS.
    DISP TBUENOS.LOTE FORMAT 'X(20)'.
END.