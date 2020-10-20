DEFINE INPUT PARAMETER v_orden_entrega as integer.
DEFINE INPUT PARAMETER v_item_oe AS INTEGER.
DEFINE INPUT PARAMETER v_cond_venta as integer.
DEFINE VAR hcon AS HANDLE.

/*by facundo 19/10/2005 - control de que los tambores tengan valores de analisis - pedido por graciela*/
DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE cAnl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lFlg AS LOGICAL    NO-UNDO.

RUN libReportes.p PERSISTENT SET hLib.

FOR EACH tambores_industria WHERE tambores_industria.id_orden_entrega = v_orden_entrega
                              AND tambores_industria.ITEM_oe          = v_item_oe
                              AND tambores_industria.id_tipotambor    = 3
                              AND tambores_industria.id_articulo      = 71
                              BREAK BY tambores_industria.nromov.
  IF LAST-OF(tambores_industria.nromov) THEN DO:
    cAnl = DYNAMIC-FUNCTION('getValoresAnalisis' IN hLib, tambores_industria.id_empresa,
                                                          tambores_industria.id_sucursal,
                                                          tambores_industria.id_tipotambor,
                                                          tambores_industria.nromov).
    IF DECIMAL(ENTRY(3, cAnl, CHR(1))) <= 0 THEN
      lFlg = TRUE.
  END.  
END.

IF lFlg THEN DO:
  MESSAGE "Imposible continuar ya que faltan cargar los valores de analisis para los lotes de la OE" VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RUN ..\industria\sendMail.p("",                               /* SIEMPRE TIENE QUE IR */
                              2,                                /* PRIORIDAD */
                              "cargar analisis lotes oe " + STRING(v_orden_entrega) + " parte " + STRING(v_item_oe),          /* SUBJECT */
                              "cargar analisis lotes oe " + STRING(v_orden_entrega) + " parte " + STRING(v_item_oe),             /* BODY     */
                              "facundoj@sa-sanmiguel.com,rvelez@sa-sanmiguel.com,ralvarez@sa-sanmiguel.com",     /* DEST. SEP COMAS */
                              "").                  

  /*RETURN.*/
END.



RUN p_calculo-gastos_item_oe.p (INPUT v_orden_entrega,
                                INPUT v_item_oe,
                                INPUT v_cond_venta,
                                INPUT DECIMAL(items_orden_entrega.contenedores:screen-value in frame F-Main)).

RUN get-container (OUTPUT hcon).
RUN refresca-browser-gastos IN hcon.
RUN carga-fob (INPUT v_orden_entrega, 
               INPUT v_item_oe). 
/* LLAMA AL PROGRAMA i_carga_fob_item_oe.i */