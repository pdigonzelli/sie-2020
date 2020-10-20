for each famgeneral.remitos no-lock where famgeneral.remitos.id_sucursal = 97 or
                                         famgeneral.remitos.id_sucursal = 95:
  find first general.remitos where
    famgeneral.remitos.id_sucursal    = general.remitos.id_sucursal    and
    famgeneral.remitos.id_tipo_movsto = general.remitos.id_tipo_movsto and
    famgeneral.remitos.nro            = general.remitos.nro
    no-error.
  if not available general.remitos then
    do:
      create general.remitos.
      buffer-copy famgeneral.remitos to general.remitos.
      for each famgeneral.items_factura no-lock of famgeneral.remitos:
        find first general.items_factura where
                famgeneral.items_factura.id_sucursal    = 
                   general.items_factura.id_sucursal    and
                famgeneral.items_factura.id_tipo_movsto = 
                   general.items_factura.id_tipo_movsto and
                famgeneral.items_factura.nro            = 
                   general.items_factura.nro            and
                famgeneral.items_factura.item           =
                   general.items_factura.item
                   no-error.
        if not available general.items_factura then
          do:
            create general.items_factura.
            buffer-copy famgeneral.items_factura to general.items_factura.
          end.
         else
          do:
            message "Remito sin items_factura. Avise a Sistemas"
                    view-as alert-box.
            disp famgeneral.remitos.nro_comprobante
                 famgeneral.remitos.bultos
                    general.remitos.bultos.
          end.
      end.
    end.
end.
      
return.
