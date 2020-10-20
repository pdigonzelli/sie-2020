  FIELD id_muestra LIKE muestras.id_muestra VALIDATE ~
  FIELD anio_muestra LIKE muestras.anio_muestra VALIDATE ~
  FIELD c_fecha LIKE muestras.c_fecha VALIDATE ~
  FIELD c_hora LIKE muestras.c_hora VALIDATE ~
  FIELD c_usuario LIKE muestras.c_usuario VALIDATE ~
  FIELD directo_cliente LIKE muestras.directo_cliente VALIDATE ~
  FIELD fecha LIKE muestras.fecha VALIDATE ~
  FIELD id_articulo LIKE muestras.id_articulo VALIDATE ~
  FIELD id_destinatario LIKE muestras.id_destinatario VALIDATE ~
  FIELD id_estado LIKE muestras.id_estado VALIDATE ~
  FIELD id_solicitante LIKE muestras.id_solicitante VALIDATE ~
  FIELD id_contacto LIKE contactos_muestras.id_contacto VALIDATE ~
  FIELD nombre LIKE contactos_muestras.nombre VALIDATE ~
  FIELD razon_social LIKE contactos_muestras.razon_social VALIDATE ~
  FIELD descripcion LIKE prioridades.descripcion VALIDATE  LABEL "Prioridad"
