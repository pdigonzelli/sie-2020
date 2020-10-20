define temp-table t-contratos 
   field orden_f as integer
   field dir_entresa as char 
   field envios_nombre as character 
   field envios_dir as character
   field otros_doc as character format "x(50)"
   field drum_marking as character
   field otros_prod as char.



input from "d:\temp\Campos_Contratos_faltantes.txt".
repeat:
   create t-contratos.
   import delimiter "," t-contratos  no-error.
   
end.   

for each t-contratos no-lock:
    find contratos where contratos.orden_fabricacion = t-contratos.orden_f no-error.
    if available contratos then
        do:
            assign contratos.direccion_envio = t-contratos.dir_entresa
                   contratos.nombre_envio_doc = t-contratos.envios_nombre
                   contratos.direccion_envio_doc = t-contratos.envios_dir
                   contratos.otros_doc = t-contratos.otros_doc
                   contratos.otros_productos = t-contratos.otros_prod.
        end.
    else message contratos.orden_fabricacion t-contratos.orden_f view-as alert-box.
end.
