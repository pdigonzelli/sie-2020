    define temp-table t-control
       field id_sucursal like control_tareas.id_sucursal 
       field legajo like items_control_tareas.legajo
       field nombre like items_control_tareas.nombre
       field id_tarea like items_control_tareas.id_tarea
       field id_unidad_liquidacion like items_control_tarea.id_unidad_liquidacion
       field cant_jornal like items_control_tareas.cantidad
       field cant_hs like items_control_tareas.cantidad
       field cant_otros like items_control_tareas.cantidad
       field nro_planilla like control_tareas.nro_planilla.

DEFINE INPUT PARAMETER TABLE FOR t-control.

{s_priini.i
 &id_formulario = "TARJA02"
 &heading       = "s_nada.hea"
 &footing       = "s_nada.foo" 
}


for each t-control where t-control.cant_jornal <> 0 or
  cant_hs <> 0 or cant_otros <> 0 no-lock break by t-control.nro_planilla by t-control.id_tarea by t-control.legajo:
   if first-of(t-control.id_tarea) Then
      do:
      find first tareas where tareas.id_tarea = t-control.id_tarea no-lock no-error.
      if available tareas Then
         display t-control.id_tarea format ">>9" column-label "" 
         tareas.abreviatura format "x(5)" column-label "Tarea".  
      end.
    display 
    t-control.legajo format ">>>>>9" column-label "Legajo" 
    t-control.cant_jornal format ">>>9.99" column-label "JS"
    t-control.cant_hs format ">>>9.99" column-label "HS"
    t-control.id_unidad_liquidacion format ">9" column-label "UL"
    t-control.cant_otros format ">>>>>9.99" column-label "CANT." 
    t-control.id_sucursal format ">9" column-label "Suc"
    t-control.nro_planilla format ">>>>>>>9" column-label "Planilla" with width 100.
end.   


{s_prifin.i}
