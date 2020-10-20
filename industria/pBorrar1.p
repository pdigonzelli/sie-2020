DEFINE VARIABLE vcPath AS CHARACTER FORMAT "X(70)"  NO-UNDO.
DEFINE VARIABLE c      AS CHARACTER  NO-UNDO.


FUNCTION getPhater RETURN CHARACTER
        (INPUT pcLetra        AS CHARACTER, 
         INPUT pcItem         AS CHARACTER,
         INPUT-OUTPUT pcPath  AS CHARACTER).

  DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcItem AS CHARACTER  NO-UNDO.
  DEFINE BUFFER pm FOR par_menu_grupos.
  /*
  DEFINE VAR dbg AS LOGICAL.
  dbg = DEBUGGER:INITIATE().
  dbg = DEBUGGER:SET-BREAK().
  */
  /*busco el item*/
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial = pcLetra
                               AND par_menu_grupo.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  /*armo condicion de parada*/
  i = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 THEN  DO: /*es una carpeta */
    /*pcPath    = pcPath + " - " + par_menu_grupos.dato_menu.*/
  END.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 AND INTEGER(par_menu_grupos.ITEM_menu) <= 10 THEN DO:  
    RETURN pcPath.
  END.
  vcItem = par_menu_grupos.ITEM_anterior.
  IF AVAILABLE par_menu_grupos THEN DO:
    /*busco un item cuyo valor de accion_seleccion_gui = item_menu, es decir, la carpeta padre*/
    FIND FIRST pm WHERE pm.letra_inicial        = par_menu_grupos.letra_inicial
                    AND pm.accion_seleccion_gui = par_menu_grupos.ITEM_menu
                  NO-LOCK NO-ERROR.
    IF AVAILABLE pm THEN DO: /*es la carpeta padre*/
      pcPath    = pcPath + " - " + pm.dato_menu.
      vcItem    = pm.ITEM_anterior.
    END.
    c = getPhater(pcLetra, vcItem, pcPath).
  END.
  ELSE
    RETURN "Error".

END.


c = getPhater("y", "193", vcPath).


DISP vcPath.
