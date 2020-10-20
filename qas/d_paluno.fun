on "choose" of b_imprim or "RETURN" of b_imprim anywhere do:
    x_pal_fin   = x0.

    if pallets.estado and
        pallets.merma = false then do:

        run d_etipal.p
            (input x1,
             input x0).
    end.
    else
        message "No Se Puede Emitir Etiqueta. Pallet Anulado o Merma"
            view-as alert-box.

    apply "entry" to b_buscar in frame botones.
end.


/******* Cambio de Pedido *******/
on "CHOOSE" of b_pedido do:

    update
        x2 label "Nro.Pedido"
        with frame pedido 1 columns row 5 
        centered overlay title " Pedido Produccion " three-d.


    run pp378ing.p (PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET, x2 , 'QAS') NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        UNDO, RETURN.
    END.
    
    run d_etipal.p
        (input pallets.id_suc_trabajo,
         input pallets.id_pallet).

    hide message. 
    apply "entry" to b_buscar in frame botones.
end.
