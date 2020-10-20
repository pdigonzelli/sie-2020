define var v_archivo as character.


v_archivo = "z:\temp\rccostosfinca.txt".
output to value(v_archivo).

put "Cod.Prod;".
put "Cod Finca;".
put "Descripcion;".
put "CECO;".
PUT "Desc CECO;".
put skip.

for each liq_ccostosliq NO-LOCK: 

put liq_ccostosliq.id_proveedor.
put ";".
put liq_ccostosliq.id_origen.
put ";".

find first origenes OF liq_ccostosliq no-lock no-error.
if available origenes Then
   put origenes.descripcion.
put ";".
PUT liq_ccostosliq.id_centro_costo_liq FORMAT ">>>>>>>>>>>>>>>".
put ";".   
PUT liq_ccostosliq.descripcion FORMAT "x(50)".
put ";".   
put skip.         
end.             
output close.
message "Migracion realizada" view-as alert-box.

run p_texto_a_excel.p (INPUT "TEXT;" + v_archivo).
