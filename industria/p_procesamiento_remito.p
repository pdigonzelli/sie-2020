DEFINE VAR v_suc AS INTEGER.
DEFINE VAR v_tip AS INTEGER.

/*
FIND remitos WHERE remitos.nro_comp = '004200003973' .
fecha_proceso = ?.  
 */

PROPATH = "..\ventas" + PROPATH.

RUN w_select_sucursal.w (OUTPUT v_suc,
                         OUTPUT v_tip).

RUN w_procesamiento_remito.w (INPUT v_suc,
                     INPUT v_tip).


