define var v_codigo as integer.
define var v_codigo-1 as integer.
define var v_codigo-2 as integer.
DEFINE VAR v_cargo AS INTEGER.
DEFINE VAR v_centro AS INTEGER.
    
  for each control_tareas no-lock where 
      control_tareas.id_empresa = 1 and 
      control_tareas.fecha >= DATE("01/02/2010") and control_tareas.fecha <= DATE("15/02/2010") and
      control_tareas.id_sucursal = 4 and
      control_tareas.id_sector = 2
      ,each items_control_tareas of control_tareas 
      no-lock by items_control_tareas.id_empresa by items_control_tareas.legajo
      :

      find first personal_finca where personal_finca.id_empresa_cosechera =
        items_control_tareas.id_empresa and personal_finca.legajo = items_control_tareas.legajo no-lock no-error.
        if available personal_finca Then
          do:
           v_cargo = personal_finca.id_cargo.
           v_centro = personal_finca.id_centro_abacus.
          end. 
          Else
          do:
           v_cargo = 0. 
           v_centro = 0.
          end. 


      find first tareas where tareas.id_tarea = items_control_tareas.id_tarea no-lock no-error.


            /***Jornal****/
       if items_control_tareas.cant_jornal <> 0  Then
        if v_cargo <> 32 Then /* Ayudante capataz*/
          v_codigo = tareas.id_concepto[14].
         Else
          v_codigo = tareas.id_concepto[11].
       Else
        v_codigo = 9999. 

      /*****Horas********/
      if items_control_tareas.cant_horas <> 0  Then
        if v_cargo <> 42 Then /* Tractorista */
         do:
          if items_control_tareas.nro_tractor <> 0 or items_control_tareas.nro_maquina <> 0 Then
             v_codigo-1 = tareas.id_concepto[15].
           Else  
             v_codigo-1 = tareas.id_concepto[2].
         end.
       Else
           v_codigo-1 = tareas.id_concepto[15].
      Else 
         v_codigo-1 = 9999.

     DISPLAY tareas.id_tarea cant_jornal cant_horas cantidad id_unidad_liquidacion v_codigo v_codigo-1.

END.
