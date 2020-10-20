define input parameter v_reporte as integer.

  define var v_archivo as character.
  define var v_js_propias like items_control_tareas.cant_jornal.
  define var v_jstrac_propias like items_control_tareas.cant_jornal.
  define var v_jsayud_propias like items_control_tareas.cant_jornal.
  define var v_hs_propias like items_control_tareas.cant_horas.
  define var v_hstrac_propias like items_control_tareas.cant_horas.
  define var v_hspul_propias like items_control_tareas.cant_horas.
  define var v_hspult_propias like items_control_tareas.cant_horas.
  define var v_lic_propias like agricola.rb_resumen_agricola.lic.
  define var v_lluvia_propias like agricola.rb_resumen_agricola.lluvia.
  define var v_viaje_propias like agricola.rb_resumen_agricola.viaje.
  define var v_destajo_propias like agricola.rb_resumen_agricola.destajo.

  define var v_js_3eros like items_control_tareas.cant_jornal.
  define var v_jstrac_3eros like items_control_tareas.cant_jornal.
  define var v_jsayud_3eros like items_control_tareas.cant_jornal.
  define var v_hs_3eros like items_control_tareas.cant_horas.
  define var v_hstrac_3eros like items_control_tareas.cant_horas.
  define var v_hspul_3eros like items_control_tareas.cant_horas.
  define var v_hspult_3eros like items_control_tareas.cant_horas.
  define var v_lic_3eros like agricola.rb_resumen_agricola.lic.
  define var v_lluvia_3eros like agricola.rb_resumen_agricola.lluvia.
  define var v_viaje_3eros like agricola.rb_resumen_agricola.viaje.
  define var v_destajo_3eros like agricola.rb_resumen_agricola.destajo.

{s_varsis.i}

v_archivo = vlc_dir_temp + "fincasgral.txt".
output to value(v_archivo).
  put "Zona;". 
  put "Finca;".
  put "Sector;".
  put "Cod.Tarea;". 
  put "Tarea;".
  put "Propias-Js;".
  put "Propias-Js Trac;".
  put "Propias-Js Ayud;".
  put "Propias-Hs;".
  put "Propias-Hs Trac;".
  put "Propias-Hs Pulv;".
  put "Propias-Hs Pulv T;".
  put "Propias-Lic;".
  put "Propias-Lluv;".
  put "Propias-Viat;".
  put "Propias-Destajo;".
  put "De 3eros-Js;".
  put "De 3eros-Js Trac;".
  put "De 3eros-Js Ayud;".
  put "De 3eros-Hs;".
  put "De 3eros-Hs Trac;".
  put "De 3eros-Hs Pulv;".
  put "De 3eros-Hs Pulv T;".
  put "De 3eros-Lic;".
  put "De 3eros-Lluv;".
  put "De 3eros-Viat;".
  put "De 3eros-Destajo;".
  put skip.
for each agricola.rb_resumen_agricola where id_reporte = v_reporte no-lock 
   break by rb_resumen_agricola.id_zona by rb_resumen_agricola.id_proveedor by rb_resumen_agricola.id_origen
   by rb_resumen_agricola.id_tarea:
   if rb_resumen_agricola.id_empresa = 1 Then
        do:
           v_js_propias = v_js_propias + rb_resumen_agricola.jornal_peon.
           v_jstrac_propias = v_jstrac_propias + rb_resumen_agricola.jornal_trac.
           v_jsayud_propias = v_jsayud_propias + rb_resumen_agricola.jornal_ayud.
           v_hs_propias = v_hs_propias + rb_resumen_agricola.horas_peon.
           v_hstrac_propias = v_hstrac_propias + rb_resumen_agricola.horas_trac.
           v_hspul_propias = v_hspul_propias + rb_resumen_agricola.horas_pulv.
           v_hspult_propias = v_hspult_propias + rb_resumen_agricola.horas_pulv_trac.
           v_lic_propias = v_lic_propias + rb_resumen_agricola.lic.
           v_lluvia_propias = v_lluvia_propias + rb_resumen_agricola.lluvia.
           v_viaje_propias = v_viaje_propias + rb_resumen_agricola.viaje.
           v_destajo_propias = v_destajo_propias + rb_resumen_agricola.destajo.
        end.
      Else
        do:
           v_js_3eros = v_js_3eros + rb_resumen_agricola.jornal_peon.
           v_jstrac_3eros = v_jstrac_3eros + rb_resumen_agricola.jornal_trac.
           v_jsayud_3eros = v_jsayud_3eros + rb_resumen_agricola.jornal_ayud.
           v_hs_3eros = v_hs_3eros + rb_resumen_agricola.horas_peon.
           v_hstrac_3eros = v_hstrac_3eros + rb_resumen_agricola.horas_trac.
           v_hspul_3eros = v_hspul_3eros + rb_resumen_agricola.horas_pulv.
           v_hspult_3eros = v_hspult_3eros + rb_resumen_agricola.horas_pulv_trac.
           v_lic_3eros = v_lic_3eros + rb_resumen_agricola.lic.
           v_lluvia_3eros = v_lluvia_3eros + rb_resumen_agricola.lluvia.
           v_viaje_3eros = v_viaje_3eros + rb_resumen_agricola.viaje.
           v_destajo_3eros = v_destajo_3eros + rb_resumen_agricola.destajo.

        end.  
   if last-of(rb_resumen_agricola.id_tarea) Then
      do:   
            find first agricola.zonas of rb_resumen_agricola no-lock no-error.  
            find first origenes of rb_resumen_agricola no-lock no-error.
            find first sectores_agricolas of rb_resumen_agricola no-lock no-error.     
            find first tareas of rb_resumen_agricola no-lock no-error.
            export delimiter ";" 
                           zonas.descripcion when available zonas
                           origenes.descripcion when available origenes
                           sectores_agricolas.descripcion when available sectores_agricolas 
                           rb_resumen_agricola.id_tarea
                           tareas.descripcion when available tareas
                           v_js_propias
                           v_jstrac_propias
                           v_jsayud_propias
                           v_hs_propias 
                           v_hstrac_propias 
                           v_hspul_propias 
                           v_hspult_propias 
                           v_lic_propias
                           v_lluvia_propias 
                           v_viaje_propias 
                           v_destajo_propias 
                           v_js_3eros
                           v_jstrac_3eros
                           v_jsayud_3eros
                           v_hs_3eros 
                           v_hstrac_3eros 
                           v_hspul_3eros 
                           v_hspult_3eros 
                           v_lic_3eros
                           v_lluvia_3eros 
                           v_viaje_3eros 
                           v_destajo_3eros. 
   
            
            assign v_js_propias = 0
                   v_jstrac_propias = 0
                   v_jsayud_propias = 0
                           v_hs_propias = 0
                           v_hstrac_propias = 0 
                           v_hspul_propias = 0
                           v_hspult_propias = 0
                           v_lic_propias = 0
                           v_lluvia_propias = 0
                           v_viaje_propias = 0
                           v_destajo_propias = 0
                           v_js_3eros = 0
                           v_jstrac_3eros = 0
                           v_jsayud_3eros = 0
                           v_hs_3eros = 0
                           v_hstrac_3eros = 0 
                           v_hspul_3eros = 0
                           v_hspult_3eros = 0
                           v_lic_3eros = 0
                           v_lluvia_3eros = 0
                           v_viaje_3eros = 0
                           v_destajo_3eros = 0. 

                                  
      end.  
end.
output close.
message "Archivo generado " v_archivo view-as alert-box.

