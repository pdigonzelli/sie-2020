define input parameter p_empresa like items_control_tareas.id_empresa.
define input parameter p_fecha_desde like items_control_tareas.fecha.
define input parameter p_fecha_hasta like items_control_tareas.fecha.

define buffer b_pers for personal_finca.

for each items_control_tareas where id_empresa = p_empresa and 
    items_control_tareas.legajo <> 0 and 
    items_control_tareas.fecha >= p_fecha_desde and
    items_control_tareas.fecha <= p_fecha_hasta no-lock 
    break by items_control_tareas.legajo:
    
    if last-of(items_control_tareas.legajo) Then
       do: 
        find first personal_finca where 
        personal_finca.id_empresa_cosechera = items_control_tareas.id_empresa and 
        personal_finca.legajo = items_control_tareas.legajo no-error.
        if not available personal_finca Then
         do:
            if length(items_control_tareas.dni_cuil) = 11 Then
               do:
                   find first b_pers where b_pers.id_empresa = items_control_tareas.id_empresa and
                              b_pers.dni_cuil = items_control_tareas.dni_cuil no-lock no-error.
                   if not available b_pers Then
                      do:             
                            create personal_finca.
                            assign personal_finca.id_empresa = items_control_tareas.id_empresa
                                   personal_finca.legajo = items_control_tareas.legajo
                                   personal_finca.nombre = items_control_tareas.nombre
                                   personal_finca.dni_cuil = items_control_tareas.dni_cuil
                                   personal_finca.tipo_liquidacion = "J".
                      end.             
               end.
         end.
         Else
           if personal_finca.tipo_liquidacion = "" Then
               assign personal_finca.tipo_liquidacion = "J".
      end.    
end.        

message "La actualizacion del archivo personal fue realizada" view-as alert-box.
