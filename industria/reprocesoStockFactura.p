FIND subd_vtas WHERE id_tipocomp = 24 AND id_punto_venta = 81 AND nro_comp = 1602  NO-LOCK NO-ERROR.
IF AVAILABLE subd_vtas  THEN
DO:
    DISPLAY subd_vtas.fecha.
    RUN ../ventas/y_gstkfacp.p (ROWID(subd_vtas) , 5 ).
    MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX.

END.
    