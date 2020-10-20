/* CURRENT-WINDOW:WIDTH = 300. */

DEFINE VARI vsucursal AS INTEGER INITIAL 96.
    DEFINE BUFFER xs FOR stock_historico_tambores.
    
/*
FOR EACH xs WHERE  /* xs.id_ARTICULO = 521 AND id_lote = 1073 AND anio = 2002 AND */
        id_sucursal = vsucursal AND
        id_tipo_movim = 7
        BY id_serial.
    IF vsucursal = id_suc_origen and
       xs.signo = "+" THEN DO:
        DISPL xs.id_serial
               xs.fecha
               xs.id_suc_origen
               xs.id_suc_des
               xs.tambor_desde
               xs.tambor_hasta
               xs.id_tipo_movim
               Xs.signo
        WITH SCROLLABLE.       
        PAUSE 0.
         ASSIGN xs.signo = "-".  
    END.
    IF vsucursal = id_suc_des and
       xs.signo = "-" THEN DO:
        DISPL "kk" 
               xs.id_serial
               xs.fecha
               xs.id_suc_origen
               xs.id_suc_des
               xs.tambor_desde
               xs.tambor_hasta
               xs.id_tipo_movim
               Xs.signo
        WITH SCROLLABLE.      
        PAUSE 0.
        ASSIGN xs.signo = "+". 
    END.
END.



*/
  
    FOR EACH xs WHERE  xs.id_ARTICULO = 52 AND id_lote = 860 AND anio = 2002 AND 
        id_sucursal = vsucursal AND
        id_tipo_movim = 4
        BY id_serial.
    IF vsucursal = id_suc_origen and
       xs.signo = "-" THEN DO:
        DISPL xs.id_serial
               xs.fecha
               xs.id_suc_origen
               xs.id_suc_des
               xs.tambor_desde
               xs.tambor_hasta
               xs.id_tipo_movim
               Xs.signo
        WITH SCROLLABLE.       
        PAUSE 0.
         /* ASSIGN xs.signo = "+".   */
    END.
    IF vsucursal = id_suc_des and
       xs.signo = "+" THEN DO:
        DISPL "kk" 
               xs.id_serial
               xs.fecha
               xs.id_suc_origen
               xs.id_suc_des
               xs.tambor_desde
               xs.tambor_hasta
               xs.id_tipo_movim
               Xs.signo
        WITH SCROLLABLE.      
        PAUSE 0.
  /*      ASSIGN xs.signo = "-".  */
    END.
END.
