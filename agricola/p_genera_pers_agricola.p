define input parameter v_empresa as integer.
define input parameter v_fecha_desde as date.
define input parameter v_fecha_hasta as date.
define input parameter v_sector as integer.
DEFINE INPUT PARAMETER v_carpeta AS CHARACTER.

define var v_nombre_empresa like proveedores.razon_social.
define var v_archivo as character.

find first proveedores where proveedores.id_proveedor = v_empresa no-lock no-error.
if available proveedores Then 
   v_nombre_empresa = proveedores.razon_social.  

v_archivo = v_carpeta + "E" + string(v_empresa,"999999")+ ".txt".

if v_sector = 0 Then
   do:

        output to value(v_archivo).
        
        put "Cod.Empresa;Empresa;Legajo;Apellido y Nombre;Tipo doc;DNI;CUIL;Calle;Localidad;CP;Provincia;Estado Civil;".
        put "Fecha Nac.;Nacionalidad;Sexo;Fecha Ing.;Cargo;Categorìa;Departamento;".
        put "Centro de costo/finca;Sindicato;Ob.Soc.;AFJP;Contrato".
        put skip.
        for each control_tareas no-lock where control_tareas.id_empresa = v_empresa and 
              control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta 
              ,each items_control_tareas of control_tareas where legajo <> 0 no-lock 
               break by items_control_tareas.id_empresa by legajo by items_control_tareas.fecha:
              
              if last-of(items_control_tareas.legajo) Then
               do:
               export delimiter ";" items_control_tareas.id_empresa
               v_nombre_empresa
               items_control_tareas.legajo items_control_tareas.nombre
               "CUIL"
               " " 
               if length(items_control_tareas.dni_cuil) = 11  Then
                 items_control_tareas.dni_cuil
                 Else
                 " "
               .
               end. 
               
        end.
        output close.
  end.
 Else
  do:
        output to value(v_archivo).
        
        put "Cod.Empresa;Empresa;Legajo;Apellido y Nombre;Tipo doc;DNI;CUIL;Calle;Localidad;CP;Provincia;Estado Civil;".
        put "Fecha Nac.;Nacionalidad;Sexo;Fecha Ing.;Cargo;Categorìa;Departamento;".
        put "Centro de costo/finca;Sindicato;Ob.Soc.;AFJP;Contrato".
        put skip.
        for each control_tareas no-lock where control_tareas.id_empresa = v_empresa and 
              control_tareas.fecha >= v_fecha_desde and control_tareas.fecha <= v_fecha_hasta and
              control_tareas.id_sector = v_sector 
              ,each items_control_tareas of control_tareas where legajo <> 0 no-lock 
               break by items_control_tareas.id_empresa by legajo by items_control_tareas.fecha:
              
              if last-of(items_control_tareas.legajo) Then
               do:
               export delimiter ";" items_control_tareas.id_empresa
               v_nombre_empresa
               items_control_tareas.legajo items_control_tareas.nombre
               "CUIL"
               " " 
               if length(items_control_tareas.dni_cuil) = 11  Then
                 items_control_tareas.dni_cuil
                 Else
                 " "
               .
               end. 
               
        end.
        output close.
  end. 
  
  
run p_texto_a_excel.p (input "TEXT;" + v_archivo).


