IF NOT CONNECTED("ventas") THEN
DO:
    CONNECT -db ventas -ld ventas -H progress2001 -S ventas9 -N tcp.
    IF CONNECTED("ventas") THEN
    DO:
        run w_reporte_suzzy.w.
    END.
END.
