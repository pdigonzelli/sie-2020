DEFINE VARIABLE vcMnu AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcRet AS CHARACTER  NO-UNDO.

FUNCTION exploreMnu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER).


  DEFINE VARIABLE viItemPrimero   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcMenu          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcItem          AS CHARACTER  NO-UNDO.
  
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial  = pcLetraInicial
                               AND par_menu_grupos.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  IF AVAILABLE par_menu_grupos THEN DO:
    vcMenu        = pcMenu + "\" + par_menu_grupos.dato_menu.
    viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
    vcItem        = par_menu_grupos.ITEM_posterior.
    
    IF NOT ERROR-STATUS:ERROR AND viItemPrimero <> 0 THEN DO:  /*es una carpeta */
      viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion).
      vcItem        = STRING(viItemPrimero).
    END.      
    ELSE DO:  /*es un programa*/
      viItemPrimero = piItemPrimero.
      vcMenu        = pcMenu + "\" + par_menu_grupos.dato_menu + CHR(10).
    END.
      
      
  END.
  IF par_menu_grupos.ITEM_posterior = STRING(piItemPrimero) THEN DO:  /*recorrio todos los hijos*/
    pcMenu = vcMenu.
    RETURN vcMenu.  
  END.
  ELSE
    exploreMnu(pcLetraInicial, vcItem, viItemPrimero, INPUT-OUTPUT vcMenu).
END.


vcRet = exploreMnu("y", "026", 190, INPUT-OUTPUT vcMnu).

DISP vcMnu vcRet.
