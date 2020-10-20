
/*------------------------------------------------------------------------
    File        : PRUEBARES.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Apr 08 10:47:36 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VAR RES AS CHARACTER NO-UNDO.
DEFINE VAR CONTADOR AS INTEGER NO-UNDO.
DEFINE VAR ICANT AS INTEGER NO-UNDO.
DEFINE VAR ILOTE AS CHARACTER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RES = "09817002377 01000000000000084065 um:01000000000000084065|22.000 :0000578258|1.000 :0000578260|7.000 :0000578262|9.000 :0000578264|30.000 :0000578266|3.000 :0000578268|3.000 :0000578270|5.000 :0000578272".

FIND pallets WHERE pallets.id_suc_trabajo = 98 AND
                     pallets.id_pallet = 17002377 NO-ERROR.
                     
FOR EACH ITEMS_PALLETS OF PALLETS NO-LOCK.
    DISP ITEMS_PALLETS.BULTOS ITEMS_PALLETS.UBICACION.
END.                     


DO CONTADOR = 2 TO NUM-ENTRIES(RES,'|'):
    iCANT = INTEGER(ENTRY(1,ENTRY(CONTADOR,RES,'|'),':')).
    ILOTE = TRIM(ENTRY(2,ENTRY(CONTADOR,RES,'|'),':')) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.

    FIND FIRST items_pallets OF PALLETS WHERE items_pallets.bultos = ICANT AND
                                              items_pallets.ubicacion = ILOTE NO-ERROR.
    IF AVAILABLE items_pallets THEN
        DISP items_pallets.bultos.

    DISP ICANT ILOTE.
    PAUSE.    
END.
