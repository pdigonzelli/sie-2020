
/*------------------------------------------------------------------------
    File        : vermenuindustria.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Aug 10 15:28:40 ART 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FOR EACH PAR_MENU_GRUPOS  NO-LOCK WHERE par_menu_grupos.letra_inicial = "M" BY ITEM_MENU.
    
    DISPLAY SUBSTRING(par_menu_grupos.accion_seleccion,1,1)
            INDEX("0123456789", SUBSTRING(par_menu_grupos.accion_seleccion,1,1)).
END.