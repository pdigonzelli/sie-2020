  FIELD fecha LIKE molienda_silo.fecha VALIDATE ~
  FIELD id_linea LIKE molienda_silo.id_linea VALIDATE ~
  FIELD id_molienda LIKE molienda_silo.id_molienda VALIDATE ~
  FIELD id_turno LIKE molienda_silo.id_turno VALIDATE ~
  FIELD id_responsable LIKE molienda_silo.id_responsable VALIDATE ~
  FIELD responsable_turno LIKE molienda_silo.responsable_turno VALIDATE ~
  FIELD observaciones LIKE molienda_silo.observaciones VALIDATE ~
  FIELD c_usuario LIKE molienda_silo.c_usuario VALIDATE ~
  FIELD c_fecha LIKE molienda_silo.c_fecha VALIDATE ~
  FIELD c_hora LIKE molienda_silo.c_hora VALIDATE ~
  FIELD Linea AS CHARACTER FORMAT "x(15)" LABEL "Linea"~
  FIELD Molienda AS DECIMAL FORMAT ">>>,>>>,>>9.99" LABEL "Molienda"
