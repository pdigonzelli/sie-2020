DEFINE VAR v_suc AS INTEGER.
DEFINE VAR v_tip AS INTEGER.

PROPATH = "..\ventas" + PROPATH.

RUN w_select_sucursal.w (OUTPUT v_suc,
                         OUTPUT v_tip).

RUN w_new_remitos.w (INPUT v_suc,
                     INPUT v_tip).

