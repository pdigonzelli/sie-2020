DEFINE TEMP-TABLE t-personal
       field legajo like liq_items_control_tareas.legajo
       field nombre like liq_items_control_tareas.nombre
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like liq_items_control_tareas.cantidad
       field id_centro_abacus like liq_legajos.id_centro_costo.

define temp-table t-resumen
       field id_centro_abacus like liq_legajos.id_centro_costo
       field id_concepto like r_tareas_unidades.id_concepto
       field cantidad like liq_items_control_tareas.cantidad.

DEFINE INPUT PARAMETER TABLE FOR t-personal.
DEFINE INPUT PARAMETER TABLE FOR t-resumen.

DEFINE INPUT PARAMETER v_opcion-1 AS LOGICAL.
DEFINE INPUT PARAMETER v_opcion-2 AS LOGICAL.
DEFINE INPUT PARAMETER v_opcion-3 AS LOGICAL.
DEFINE INPUT PARAMETER v_id_empresa AS INTEGER.


form
    skip(1)
    "Centro de Costo" t-personal.id_centro_abacus space(5)
    centros_costos_abacus.descripcion 
    skip
    with frame fcentro down no-box no-labels width 100 use-text stream-io.   
   


form 
    t-personal.legajo column-label "Legajo"
    liq_legajos.apellido_nombre format "x(30)" column-label "Nombre"
    t-personal.id_concepto column-label "Concepto"
    liq_conceptos.descripcion format "x(25)" column-label ""
    t-personal.cantidad column-label "Cantidad"
    with frame detalle down  width 100 use-text stream-io.   


form
    t-resumen.id_centro_abacus column-label "CCosto"
    centros_costos_abacus.descripcion format "x(20)"
    t-resumen.id_concepto column-label "Concepto"
    liq_conceptos.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresumen down  width 100 use-text stream-io.   


form
    t-resumen.id_concepto column-label "Concepto"
    liq_conceptos.descripcion
    t-resumen.cantidad column-label "Cantidad"
    with frame fresgral down  width 100 use-text stream-io.   


{s_priini.i
       &id_formulario = "TARJA01"
       &heading       = "s_nada.hea"
       &footing       = "s_nada.foo" }


if v_opcion-1 = yes Then
do:

for each t-personal no-lock  break by t-personal.id_centro_abacus by t-personal.legajo 
    by t-personal.nombre :
      
  if first-of(t-personal.id_centro_abacus) Then
     do:
     find first centros_costos_abacus of t-personal no-lock no-error.
     if available centros_costos_abacus Then
       do:
       display t-personal.id_centro_abacus centros_costos_abacus.descripcion
       with frame fcentro.
       down with frame fcentro.
       end.
     Else      
      do:
       display t-personal.id_centro_abacus with frame fcentro.
       down with frame fcentro.
      end. 
     end. 
  if first-of(t-personal.nombre) Then
     do:
   find first liq_legajos where liq_legajos.id_empresa = v_id_empresa and
          liq_legajos.legajo = t-personal.legajo no-lock no-error.
     if available liq_legajos Then
           display t-personal.legajo liq_legajos.apellido_nombre with frame detalle.      
         Else
           display t-personal.legajo 
                   t-personal.nombre @ liq_legajos.apellido_nombre with frame detalle.   
     end. 
   find first liq_conceptos of t-personal no-lock.       
   display t-personal.id_concepto 
           liq_conceptos.descripcion
           t-personal.cantidad with frame detalle.
   down with frame detalle.
end.   
end.

if v_opcion-2 = yes Then
do:


for each t-resumen no-lock break by t-resumen.id_centro_abacus:
  find first liq_conceptos of t-resumen no-lock.
  if first-of(t-resumen.id_centro_abacus) Then
  do: 
  find first centros_costos_abacus of t-resumen no-lock no-error.
  if available centros_costos_abacus Then
     display t-resumen.id_centro_abacus
          centros_costos_abacus.descripcion
          with frame fresumen.
   Else
     display t-resumen.id_centro_abacus
          with frame fresumen.
          
  end.
 
  display t-resumen.id_concepto
         (IF AVAILABLE liq_conceptos THEN liq_conceptos.descripcion ELSE "")
          t-resumen.cantidad
          with frame fresumen.

  down with frame fresumen.
   
end.
end.

if v_opcion-3 = YES Then
do:
 for each t-resumen no-lock break by t-resumen.id_concepto :
  accumulate t-resumen.cantidad (TOTAL by t-resumen.id_concepto).
  if last-of(t-resumen.id_concepto) Then
     do:  
       find first liq_conceptos of t-resumen no-lock.       
       
       display  t-resumen.id_concepto
          liq_conceptos.descripcion
          accum TOTAL by t-resumen.id_concepto t-resumen.cantidad @ t-resumen.cantidad
          with frame fresgral.
       down with frame fresgral.
     end.  
 end.
end.

{s_prifin.i}

