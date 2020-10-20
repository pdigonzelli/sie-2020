DEFINE VAR v_sucursal AS INTEGER.

RUN w_sel_sucursal.w (OUTPUT v_sucursal).

RUN w_tam_ind_prod_aceite.w (INPUT v_sucursal).
