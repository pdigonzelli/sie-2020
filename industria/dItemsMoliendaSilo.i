  FIELD id_molienda LIKE items_molienda_silo.id_molienda VALIDATE ~
  FIELD id_linea LIKE items_molienda_silo.id_linea VALIDATE ~
  FIELD id_silo LIKE items_molienda_silo.id_silo VALIDATE ~
  FIELD hora_comienzo LIKE items_molienda_silo.hora_comienzo VALIDATE ~
  FIELD hora_termino LIKE items_molienda_silo.hora_termino VALIDATE ~
  FIELD kilos LIKE items_molienda_silo.kilos VALIDATE ~
  FIELD c_usuario LIKE items_molienda_silo.c_usuario VALIDATE ~
  FIELD c_fecha LIKE items_molienda_silo.c_fecha VALIDATE ~
  FIELD c_hora LIKE items_molienda_silo.c_hora VALIDATE ~
  FIELD Silo AS CHARACTER FORMAT "x(15)" LABEL "Silo"~
  FIELD id_turno LIKE items_molienda_silo.id_turno VALIDATE ~
  FIELD fecha LIKE items_molienda_silo.fecha VALIDATE ~
  FIELD kilos_aux LIKE items_molienda_silo.kilos_aux VALIDATE ~
  FIELD kilos_descarte_packing LIKE items_molienda_silo.kilos_descarte_packing VALIDATE ~
  FIELD id_pesada LIKE items_molienda_silo.id_pesada VALIDATE ~
  FIELD id_ticket LIKE items_molienda_silo.id_ticket VALIDATE 
