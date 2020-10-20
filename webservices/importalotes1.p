
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
/* HU;Pos.;Material;Ce.;Cantidad embalada;UM;Lote;ZCALIBRE */

DEFINE TEMP-TABLE TTLOTES
    FIELD HU AS CHARACTER
    FIELD POSICION AS INTEGER
    FIELD MATERIAL AS CHARACTER 
    FIELD CE AS CHARACTER
    FIELD CANTIDAD AS DECIMAL
    FIELD UM AS CHARACTER
    FIELD LOTE AS CHARACTER
    FIELD CALIBRE AS CHARACTER
    FIELD HU1 AS CHARACTER 
    FIELD PALLET AS INTEGER
    FIELD POSICION1 AS INTEGER
    INDEX P PALLET
    INDEX  H HU.
    
DEFINE TEMP-TABLE TTLOTES1
    FIELD HU AS CHARACTER
    FIELD MATERIAL AS CHARACTER 
    FIELD LOTE AS CHARACTER
    FIELD PALLET AS INTEGER
    FIELD CANTIDAD AS DECIMAL
    INDEX P PALLET
    INDEX  H AS UNIQUE HU LOTE.    
    
DEFINE TEMP-TABLE TBUENOS 
    FIELD SUC AS INTEGER
    FIELD PALLET AS INTEGER
    FIELD POSICION AS INTEGER
    FIELD ITEMPALLET AS INTEGER
    FIELD BULTOS AS INTEGER
    FIELD CANTIDAD AS INTEGER
    FIELD LOTE AS CHARACTER
    INDEX I SUC PALLET POSICION.

DEFINE TEMP-TABLE TMALOS
    FIELD SUC AS CHARACTER
    FIELD PALLET AS INTEGER
    FIELD HU AS CHARACTER.

DEFINE TEMP-TABLE TITMALOS
    FIELD SUC AS CHARACTER
    FIELD PALLET AS INTEGER
    FIELD HU AS CHARACTER
    FIELD LOTE AS CHARACTER
    FIELD CANTIDAD AS INTEGER.
    
DEFINE TEMP-TABLE TPALLETS
    FIELD ID_SUC_TRABAJO AS INTEGER
    FIELD ID_PALLET AS INTEGER 
    FIELD HU AS CHARACTER
    INDEX I AS UNIQUE HU.
        

DEFINE VAR FLAG-ENCONTRO AS LOGICAL NO-UNDO.
    
INPUT FROM 'LOTES21STK.CSV'.

SESSION:NUMERIC-FORMAT = 'EUROPEAN'.


FOR EACH   PALLETS WHERE    pallets.id_suc_trabajo = 98 AND
                            pallets.id_pallet >  16000000 AND
                            pallets.id_pallet_sap <> '' NO-LOCK.
    CREATE  TPALLETS.
    ASSIGN  TPALLETS.ID_SUC_TRABAJO = PALLETS.ID_SUC_TRABAJO
            TPALLETS.ID_PALLET = PALLETS.ID_PALLET
            TPALLETS.HU = TRIM(SUBSTRING(pallets.id_pallet_sap,2)).
END.


REPEAT:
    CREATE TTLOTES.
    IMPORT DELIMITER ';' TTLOTES.
    
    FIND FIRST TTLOTES1 WHERE TTLOTES1.HU = TTLOTES.HU AND TTLOTES1.LOTE = TTLOTES.LOTE NO-ERROR.
    IF NOT AVAILABLE TTLOTES1 THEN
    DO:
        CREATE  TTLOTES1.
        ASSIGN  TTLOTES1.HU = TTLOTES.HU 
                TTLOTES1.LOTE = TTLOTES.LOTE 
                TTLOTES1.MATERIAL = TTLOTES.MATERIAL
                TTLOTES1.PALLET = TTLOTES.PALLET.
    END.
    TTLOTES1.CANTIDAD = TTLOTES1.CANTIDAD + TTLOTES.CANTIDAD.
END.

MESSAGE 'CREANDO LOTES1' VIEW-AS ALERT-BOX.

FOR EACH TTLOTES1.

    FLAG-ENCONTRO = FALSE.
    
    FIND FIRST TPALLETS WHERE TPALLETS.HU = TRIM(TTLOTES1.HU) NO-ERROR.
    IF NOT AVAILABLE TPALLETS THEN
    DO:
       CREATE   TMALOS.
       ASSIGN   TMALOS.HU = TTLOTES1.HU.
       DISP TTLOTES1.HU FORMAT 'X(30)'.
       NEXT.
    END.
    
    
    FIND FIRST items_pallets WHERE 
            items_pallets.id_pallet = TPALLETS.ID_PALLET AND
            items_pallets.id_suc_trabajo = TPALLETS.ID_SUC_TRABAJO AND
            items_pallets.BULTOS = TTLOTES1.CANTIDAD NO-ERROR.

    IF AVAILABLE items_pallets THEN
    DO:
        FLAG-ENCONTRO = TRUE.
        CREATE TBUENOS.
        ASSIGN TBUENOS.SUC = items_pallets.id_suc_trabajo 
               TBUENOS.PALLET = items_pallets.id_pallet
               TBUENOS.ITEMPALLET = items_pallets.item_pallet
               TBUENOS.BULTOS = items_pallets.BULTOS
               TBUENOS.CANTIDAD = INTEGER(TTLOTES1.CANTIDAD)
               TBUENOS.LOTE = TTLOTES1.LOTE.
        NEXT.
    END.
    ELSE
    DO:
        CREATE TITMALOS.
        ASSIGN TITMALOS.HU = TTLOTES1.HU
               TITMALOS.LOTE = TTLOTES1.LOTE
               TITMALOS.CANTIDAD = TTLOTES1.CANTIDAD.
    END.
/*

/*    DISP TTLOTES.PALLET FORMAT 'X(12)'  integer(SUBSTRING(ttlotes.pallet,3)) TTLOTES.POSICION. */
    FLAG-ENCONTRO = FALSE.
    
    FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND PALLETS.ID_PALLET = integer(SUBSTRING(ttlotes.pallet,3)) NO-ERROR.
    IF NOT AVAILABLE PALLETS THEN 
    DO:
        CREATE TMALOS.
        ASSIGN TMALOS.PALLET = TTLOTES.PALLET.
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

*/
END.
MESSAGE 'PALLETS MALOS' VIEW-AS ALERT-BOX.

FOR EACH TMALOS.
    DISP TMALOS.
END.

MESSAGE 'ITEMS PALLETS MALOS' VIEW-AS ALERT-BOX.
FOR EACH TITMALOS.
    DISP TITMALOS.
END.
/*
CURRENT-WINDOW:WIDTH = 130.
MESSAGE 'ITEMS PALLETS BUENOS' VIEW-AS ALERT-BOX.
FOR EACH TBUENOS BY TBUENOS.PALLET BY TBUENOS.ITEMPALLET.
    DISP TBUENOS WITH WIDTH 120.
    PAUSE 0.
END.
*/
OUTPUT TO D:\TEMP\LOTESBUENOS2.TXT.
FOR EACH TBUENOS.
    EXPORT DELIMITER ';' TBUENOS.
    FIND items_pallets WHERE items_pallets.id_suc_trabajo = TBUENOS.SUC AND
                             items_pallets.id_pallet = TBUENOS.PALLET AND
                             items_pallets.item_pallet = TBUENOS.ITEMPALLET NO-ERROR.
    items_pallets.ubicacion = TBUENOS.LOTE.
END.
OUTPUT CLOSE.


MESSAGE 'TERMINO' VIEW-AS ALERT-BOX.
     
/*
FOR EACH TBUENOS.
    DISP TBUENOS.
END.
*/
/*
FOR EACH TTLOTES BREAK BY TTLOTES.HU BY TTLOTES.POSICION BY TTLOTES.LOTE.
    FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND pallets.id_pallet_sap = TTLOTES.HU  NO-ERROR.
    IF AVAILABLE PALLETS THEN 
        DISP PALLETS.ID_PALLET.
    ELSE
        DISP ('NO ' + TTLOTES.HU) FORMAT 'X(30)'.

/*    IF LAST(TTLOTES.LOTE) THEN DO:

    /*    DISP TTLOTES.PALLET FORMAT 'X(12)'  integer(SUBSTRING(ttlotes.pallet,3)) TTLOTES.POSICION. */
        FLAG-ENCONTRO = FALSE.
        
        FIND PALLETS WHERE PALLETS.ID_SUC_TRABAJO = 98 AND pallets.id_pallet_sap = TTLOTES.HU  NO-ERROR.
        IF AVAILABLE PALLETS THEN 
            DISP PALLETS.ID_PALLET.
        ELSE
            DISP ('NO ' + TTLOTES.HU) FORMAT 'X(30)'.
        /*
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
        END.*/
    END. */
END.
*/


/*
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
*/