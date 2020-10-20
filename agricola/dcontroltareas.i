  FIELD c_fecha LIKE control_tareas.c_fecha VALIDATE ~
  FIELD c_hora LIKE control_tareas.c_hora VALIDATE ~
  FIELD c_usuario LIKE control_tareas.c_usuario VALIDATE ~
  FIELD fecha LIKE control_tareas.fecha VALIDATE ~
  FIELD id_empresa LIKE control_tareas.id_empresa VALIDATE  FORMAT ">>>>>9"~
  FIELD id_grupo LIKE control_tareas.id_grupo VALIDATE ~
  FIELD id_liquidacion LIKE control_tareas.id_liquidacion VALIDATE ~
  FIELD id_origen LIKE control_tareas.id_origen VALIDATE ~
  FIELD id_proveedor LIKE control_tareas.id_proveedor VALIDATE ~
  FIELD id_sector LIKE control_tareas.id_sector VALIDATE ~
  FIELD id_sucursal LIKE control_tareas.id_sucursal VALIDATE ~
  FIELD id_tipo_planilla LIKE control_tareas.id_tipo_planilla VALIDATE ~
  FIELD nro_planilla LIKE control_tareas.nro_planilla VALIDATE ~
  FIELD observaciones LIKE control_tareas.observaciones VALIDATE ~
  FIELD razon_social LIKE proveedores.razon_social VALIDATE ~
  FIELD descrip-sectores LIKE sectores_agricolas.descripcion VALIDATE ~
  FIELD nombre-suc LIKE sucursales.nombre VALIDATE 
