define input parameter p_empresa like liq_tarjas.id_empresa.
define input parameter p_fecha_desde like liq_tarjas.fecha.
define input parameter p_fecha_hasta like liq_tarjas.fecha.
define input parameter p_archivo as character.
 
DEF VAR vcant AS INTEGER.
DEF VAR vfecha AS DATE.
DEF VAR i AS INTEGER.
DEF VAR vempresa AS CHARACTER FORMAT "x(20)".
DEF VAR vvalor AS INTEGER.
DEF VAR vdiafecha AS DATE EXTENT 31.

DEF TEMP-TABLE tdias 
    FIELD id_empresa LIKE liq_tarjas.id_empresa
    FIELD legajo_rhpro LIKE liq_legajos.legajo_rhpro
    FIELD nombre LIKE liq_items_control_finca.nombre
    FIELD dia_trab AS INTEGER EXTENT 31
    INDEX idias IS UNIQUE
       id_empresa legajo_rhpro.



FOR EACH tdias:
    DELETE tdias.
END.


if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

vcant = (p_fecha_hasta - p_fecha_desde) + 1.

IF vcant > 31 THEN
DO:
    MESSAGE "El rango de fechas no es correcto" SKIP
            "El limite es de 31 d¡as" VIEW-AS ALERT-BOX INFORMATION.
    LEAVE.
END.

vfecha = p_fecha_desde.
DO i = 1 TO vcant:
    vdiafecha[i] = vfecha.
    vfecha = vfecha + 1.
END.


 
  /* Peones */

  FOR EACH liq_items_tarjas WHERE
      (IF p_empresa <> 0 THEN liq_items_tarjas.id_empresa = p_empresa ELSE TRUE)  AND
      liq_items_tarjas.id_empresa < 1000 AND
      liq_items_tarjas.fecha >= p_fecha_desde and
      liq_items_tarjas.fecha <= p_fecha_hasta AND
      liq_items_tarjas.id_tarea <> 0 no-lock,
      FIRST liq_tareas OF liq_items_tarjas WHERE liq_tareas.id_grupo_tarea <> 8 NO-LOCK,
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                              liq_legajos.legajo = liq_items_tarjas.legajo NO-LOCK 
      BREAK BY liq_legajos.id_empresa BY liq_legajos.legajo_rhpro BY liq_items_tarjas.fecha:


      FIND FIRST tdias WHERE 
          tdias.id_empresa = liq_items_tarjas.id_empresa AND
          tdias.legajo_rhpro = liq_legajos.legajo_rhpro NO-ERROR.
      IF NOT AVAILABLE tdias THEN
      DO:
          CREATE tdias.
          ASSIGN tdias.id_empresa = liq_items_tarjas.id_empresa
                 tdias.legajo_rhpro = liq_legajos.legajo_rhpro
                 tdias.nombre = liq_items_tarjas.nombre.

      END.

      IF liq_items_tarjas.fecha = vdiafecha[1] THEN tdias.dia_trab[1] = tdias.dia_trab[1]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[2] THEN tdias.dia_trab[2] = tdias.dia_trab[2]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[3] THEN tdias.dia_trab[3] = tdias.dia_trab[3]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[4] THEN tdias.dia_trab[4] = tdias.dia_trab[4]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[5] THEN tdias.dia_trab[5] = tdias.dia_trab[5]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[6] THEN tdias.dia_trab[6] = tdias.dia_trab[6]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[7] THEN tdias.dia_trab[7] = tdias.dia_trab[7]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[8] THEN tdias.dia_trab[8] = tdias.dia_trab[8]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[9] THEN tdias.dia_trab[9] = tdias.dia_trab[9]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[10] THEN tdias.dia_trab[10] = tdias.dia_trab[10]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[11] THEN tdias.dia_trab[11] = tdias.dia_trab[11]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[12] THEN tdias.dia_trab[12] = tdias.dia_trab[12]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[13] THEN tdias.dia_trab[13] = tdias.dia_trab[13]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[14] THEN tdias.dia_trab[14] = tdias.dia_trab[14]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[15] THEN tdias.dia_trab[15] = tdias.dia_trab[15]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[16] THEN tdias.dia_trab[16] = tdias.dia_trab[16]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[17] THEN tdias.dia_trab[17] = tdias.dia_trab[17]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[18] THEN tdias.dia_trab[18] = tdias.dia_trab[18]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[19] THEN tdias.dia_trab[19] = tdias.dia_trab[19]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[20] THEN tdias.dia_trab[20] = tdias.dia_trab[20]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[21] THEN tdias.dia_trab[21] = tdias.dia_trab[21]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[22] THEN tdias.dia_trab[22] = tdias.dia_trab[22]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[23] THEN tdias.dia_trab[23] = tdias.dia_trab[23]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[24] THEN tdias.dia_trab[24] = tdias.dia_trab[24]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[25] THEN tdias.dia_trab[25] = tdias.dia_trab[25]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[26] THEN tdias.dia_trab[26] = tdias.dia_trab[26]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[27] THEN tdias.dia_trab[27] = tdias.dia_trab[27]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[28] THEN tdias.dia_trab[28] = tdias.dia_trab[28]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[29] THEN tdias.dia_trab[29] = tdias.dia_trab[29]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[30] THEN tdias.dia_trab[30] = tdias.dia_trab[30]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[31] THEN tdias.dia_trab[31] = tdias.dia_trab[31]  + 1.
     
  end.

  /**************************************************************************/



  /* Capataces */
  for each liq_control_finca where 
      (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
            liq_control_finca.fecha >= p_fecha_desde  AND
            liq_control_finca.fecha <= p_fecha_hasta NO-LOCK
            BREAK  by liq_control_finca.id_empresa BY liq_control_finca.id_capataz: 
      
      FIND FIRST liq_capataces OF liq_control_finca NO-LOCK NO-ERROR.
      IF AVAILABLE liq_capataces THEN
      DO:
          IF p_empresa <> 101 THEN
                  vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
             ELSE vvalor = 0.


      FIND FIRST tdias WHERE 
          tdias.id_empresa = liq_control_finca.id_empresa AND
          tdias.legajo_rhpro = (vvalor + liq_capataces.legajo) NO-ERROR.
      IF NOT AVAILABLE tdias THEN
      DO:
          CREATE tdias.
          ASSIGN tdias.id_empresa = liq_control_finca.id_empresa
                 tdias.legajo = (vvalor + liq_capataces.legajo)
                 tdias.nombre = liq_capataces.nombre.

      END.
      IF liq_control_finca.fecha = vdiafecha[1] THEN tdias.dia_trab[1] = tdias.dia_trab[1]  + 1.
      IF liq_control_finca.fecha = vdiafecha[2] THEN tdias.dia_trab[2] = tdias.dia_trab[2]  + 1.
      IF liq_control_finca.fecha = vdiafecha[3] THEN tdias.dia_trab[3] = tdias.dia_trab[3]  + 1.
      IF liq_control_finca.fecha = vdiafecha[4] THEN tdias.dia_trab[4] = tdias.dia_trab[4]  + 1.
      IF liq_control_finca.fecha = vdiafecha[5] THEN tdias.dia_trab[5] = tdias.dia_trab[5]  + 1.
      IF liq_control_finca.fecha = vdiafecha[6] THEN tdias.dia_trab[6] = tdias.dia_trab[6]  + 1.
      IF liq_control_finca.fecha = vdiafecha[7] THEN tdias.dia_trab[7] = tdias.dia_trab[7]  + 1.
      IF liq_control_finca.fecha = vdiafecha[8] THEN tdias.dia_trab[8] = tdias.dia_trab[8]  + 1.
      IF liq_control_finca.fecha = vdiafecha[9] THEN tdias.dia_trab[9] = tdias.dia_trab[9]  + 1.
      IF liq_control_finca.fecha = vdiafecha[10] THEN tdias.dia_trab[10] = tdias.dia_trab[10]  + 1.
      IF liq_control_finca.fecha = vdiafecha[11] THEN tdias.dia_trab[11] = tdias.dia_trab[11]  + 1.
      IF liq_control_finca.fecha = vdiafecha[12] THEN tdias.dia_trab[12] = tdias.dia_trab[12]  + 1.
      IF liq_control_finca.fecha = vdiafecha[13] THEN tdias.dia_trab[13] = tdias.dia_trab[13]  + 1.
      IF liq_control_finca.fecha = vdiafecha[14] THEN tdias.dia_trab[14] = tdias.dia_trab[14]  + 1.
      IF liq_control_finca.fecha = vdiafecha[15] THEN tdias.dia_trab[15] = tdias.dia_trab[15]  + 1.
      IF liq_control_finca.fecha = vdiafecha[16] THEN tdias.dia_trab[16] = tdias.dia_trab[16]  + 1.
      IF liq_control_finca.fecha = vdiafecha[17] THEN tdias.dia_trab[17] = tdias.dia_trab[17]  + 1.
      IF liq_control_finca.fecha = vdiafecha[18] THEN tdias.dia_trab[18] = tdias.dia_trab[18]  + 1.
      IF liq_control_finca.fecha = vdiafecha[19] THEN tdias.dia_trab[19] = tdias.dia_trab[19]  + 1.
      IF liq_control_finca.fecha = vdiafecha[20] THEN tdias.dia_trab[20] = tdias.dia_trab[20]  + 1.
      IF liq_control_finca.fecha = vdiafecha[21] THEN tdias.dia_trab[21] = tdias.dia_trab[21]  + 1.
      IF liq_control_finca.fecha = vdiafecha[22] THEN tdias.dia_trab[22] = tdias.dia_trab[22]  + 1.
      IF liq_control_finca.fecha = vdiafecha[23] THEN tdias.dia_trab[23] = tdias.dia_trab[23]  + 1.
      IF liq_control_finca.fecha = vdiafecha[24] THEN tdias.dia_trab[24] = tdias.dia_trab[24]  + 1.
      IF liq_control_finca.fecha = vdiafecha[25] THEN tdias.dia_trab[25] = tdias.dia_trab[25]  + 1.
      IF liq_control_finca.fecha = vdiafecha[26] THEN tdias.dia_trab[26] = tdias.dia_trab[26]  + 1.
      IF liq_control_finca.fecha = vdiafecha[27] THEN tdias.dia_trab[27] = tdias.dia_trab[27]  + 1.
      IF liq_control_finca.fecha = vdiafecha[28] THEN tdias.dia_trab[28] = tdias.dia_trab[28]  + 1.
      IF liq_control_finca.fecha = vdiafecha[29] THEN tdias.dia_trab[29] = tdias.dia_trab[29]  + 1.
      IF liq_control_finca.fecha = vdiafecha[30] THEN tdias.dia_trab[30] = tdias.dia_trab[30]  + 1.
      IF liq_control_finca.fecha = vdiafecha[31] THEN tdias.dia_trab[31] = tdias.dia_trab[31]  + 1.
      END.
   END.
/***************************************************************/


  /* Ayudante Capataz */
  for each liq_control_finca where 
      (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
             liq_control_finca.fecha >= p_fecha_desde  AND
             liq_control_finca.fecha <= p_fecha_hasta  no-lock, 
              each liq_items_control_finca of liq_control_finca no-lock
              where id_causa_ausencia = 15 
              BREAK  by liq_items_control_finca.id_empresa BY liq_items_control_finca.legajo: 
   
      IF p_empresa <> 101 THEN
             vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
        ELSE vvalor = 0.
             FIND FIRST tdias WHERE 
                 tdias.id_empresa = liq_items_control_finca.id_empresa AND
                 tdias.legajo = (vvalor + liq_items_control_finca.legajo) NO-ERROR.
             IF NOT AVAILABLE tdias THEN
             DO:
                 CREATE tdias.
                 ASSIGN tdias.id_empresa = liq_items_control_finca.id_empresa
                        tdias.legajo = (vvalor + liq_items_control_finca.legajo)
                        tdias.nombre = liq_items_tarjas.nombre.

             END.
             IF liq_control_finca.fecha = vdiafecha[1] THEN tdias.dia_trab[1] = tdias.dia_trab[1]  + 1.
             IF liq_control_finca.fecha = vdiafecha[2] THEN tdias.dia_trab[2] = tdias.dia_trab[2]  + 1.
             IF liq_control_finca.fecha = vdiafecha[3] THEN tdias.dia_trab[3] = tdias.dia_trab[3]  + 1.
             IF liq_control_finca.fecha = vdiafecha[4] THEN tdias.dia_trab[4] = tdias.dia_trab[4]  + 1.
             IF liq_control_finca.fecha = vdiafecha[5] THEN tdias.dia_trab[5] = tdias.dia_trab[5]  + 1.
             IF liq_control_finca.fecha = vdiafecha[6] THEN tdias.dia_trab[6] = tdias.dia_trab[6]  + 1.
             IF liq_control_finca.fecha = vdiafecha[7] THEN tdias.dia_trab[7] = tdias.dia_trab[7]  + 1.
             IF liq_control_finca.fecha = vdiafecha[8] THEN tdias.dia_trab[8] = tdias.dia_trab[8]  + 1.
             IF liq_control_finca.fecha = vdiafecha[9] THEN tdias.dia_trab[9] = tdias.dia_trab[9]  + 1.
             IF liq_control_finca.fecha = vdiafecha[10] THEN tdias.dia_trab[10] = tdias.dia_trab[10]  + 1.
             IF liq_control_finca.fecha = vdiafecha[11] THEN tdias.dia_trab[11] = tdias.dia_trab[11]  + 1.
             IF liq_control_finca.fecha = vdiafecha[12] THEN tdias.dia_trab[12] = tdias.dia_trab[12]  + 1.
             IF liq_control_finca.fecha = vdiafecha[13] THEN tdias.dia_trab[13] = tdias.dia_trab[13]  + 1.
             IF liq_control_finca.fecha = vdiafecha[14] THEN tdias.dia_trab[14] = tdias.dia_trab[14]  + 1.
             IF liq_control_finca.fecha = vdiafecha[15] THEN tdias.dia_trab[15] = tdias.dia_trab[15]  + 1.
             IF liq_control_finca.fecha = vdiafecha[16] THEN tdias.dia_trab[16] = tdias.dia_trab[16]  + 1.
             IF liq_control_finca.fecha = vdiafecha[17] THEN tdias.dia_trab[17] = tdias.dia_trab[17]  + 1.
             IF liq_control_finca.fecha = vdiafecha[18] THEN tdias.dia_trab[18] = tdias.dia_trab[18]  + 1.
             IF liq_control_finca.fecha = vdiafecha[19] THEN tdias.dia_trab[19] = tdias.dia_trab[19]  + 1.
             IF liq_control_finca.fecha = vdiafecha[20] THEN tdias.dia_trab[20] = tdias.dia_trab[20]  + 1.
             IF liq_control_finca.fecha = vdiafecha[21] THEN tdias.dia_trab[21] = tdias.dia_trab[21]  + 1.
             IF liq_control_finca.fecha = vdiafecha[22] THEN tdias.dia_trab[22] = tdias.dia_trab[22]  + 1.
             IF liq_control_finca.fecha = vdiafecha[23] THEN tdias.dia_trab[23] = tdias.dia_trab[23]  + 1.
             IF liq_control_finca.fecha = vdiafecha[24] THEN tdias.dia_trab[24] = tdias.dia_trab[24]  + 1.
             IF liq_control_finca.fecha = vdiafecha[25] THEN tdias.dia_trab[25] = tdias.dia_trab[25]  + 1.
             IF liq_control_finca.fecha = vdiafecha[26] THEN tdias.dia_trab[26] = tdias.dia_trab[26]  + 1.
             IF liq_control_finca.fecha = vdiafecha[27] THEN tdias.dia_trab[27] = tdias.dia_trab[27]  + 1.
             IF liq_control_finca.fecha = vdiafecha[28] THEN tdias.dia_trab[28] = tdias.dia_trab[28]  + 1.
             IF liq_control_finca.fecha = vdiafecha[29] THEN tdias.dia_trab[29] = tdias.dia_trab[29]  + 1.
             IF liq_control_finca.fecha = vdiafecha[30] THEN tdias.dia_trab[30] = tdias.dia_trab[30]  + 1.
             IF liq_control_finca.fecha = vdiafecha[31] THEN tdias.dia_trab[31] = tdias.dia_trab[31]  + 1.
  END.

/***************************************************************/


  /* Peones Cosecha */
  for each liq_control_finca where 
      (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
             liq_control_finca.fecha >= p_fecha_desde  AND
             liq_control_finca.fecha <= p_fecha_hasta  no-lock, 
              each liq_items_control_finca of liq_control_finca no-lock
              where  (liq_items_control_finca.cantidad_tijera <> 0 or liq_items_control_finca.cantidad_mano <> 0) AND
                     liq_items_control_finca.id_causa_ausencia <> 15
              BREAK  by liq_items_control_finca.id_empresa BY liq_items_control_finca.legajo: 
   
      IF p_empresa <> 101 THEN
             vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
        ELSE vvalor = 0.
      
             FIND FIRST tdias WHERE 
                 tdias.id_empresa = liq_items_control_finca.id_empresa AND
                 tdias.legajo = (vvalor + liq_items_control_finca.legajo) NO-ERROR.
             IF NOT AVAILABLE tdias THEN
             DO:
                 CREATE tdias.
                 ASSIGN tdias.id_empresa = liq_items_control_finca.id_empresa
                        tdias.legajo = (vvalor + liq_items_control_finca.legajo)
                        tdias.nombre = liq_items_control_finca.nombre.

             END.
             IF liq_control_finca.fecha = vdiafecha[1] THEN tdias.dia_trab[1] = tdias.dia_trab[1]  + 1.
             IF liq_control_finca.fecha = vdiafecha[2] THEN tdias.dia_trab[2] = tdias.dia_trab[2]  + 1.
             IF liq_control_finca.fecha = vdiafecha[3] THEN tdias.dia_trab[3] = tdias.dia_trab[3]  + 1.
             IF liq_control_finca.fecha = vdiafecha[4] THEN tdias.dia_trab[4] = tdias.dia_trab[4]  + 1.
             IF liq_control_finca.fecha = vdiafecha[5] THEN tdias.dia_trab[5] = tdias.dia_trab[5]  + 1.
             IF liq_control_finca.fecha = vdiafecha[6] THEN tdias.dia_trab[6] = tdias.dia_trab[6]  + 1.
             IF liq_control_finca.fecha = vdiafecha[7] THEN tdias.dia_trab[7] = tdias.dia_trab[7]  + 1.
             IF liq_control_finca.fecha = vdiafecha[8] THEN tdias.dia_trab[8] = tdias.dia_trab[8]  + 1.
             IF liq_control_finca.fecha = vdiafecha[9] THEN tdias.dia_trab[9] = tdias.dia_trab[9]  + 1.
             IF liq_control_finca.fecha = vdiafecha[10] THEN tdias.dia_trab[10] = tdias.dia_trab[10]  + 1.
             IF liq_control_finca.fecha = vdiafecha[11] THEN tdias.dia_trab[11] = tdias.dia_trab[11]  + 1.
             IF liq_control_finca.fecha = vdiafecha[12] THEN tdias.dia_trab[12] = tdias.dia_trab[12]  + 1.
             IF liq_control_finca.fecha = vdiafecha[13] THEN tdias.dia_trab[13] = tdias.dia_trab[13]  + 1.
             IF liq_control_finca.fecha = vdiafecha[14] THEN tdias.dia_trab[14] = tdias.dia_trab[14]  + 1.
             IF liq_control_finca.fecha = vdiafecha[15] THEN tdias.dia_trab[15] = tdias.dia_trab[15]  + 1.
             IF liq_control_finca.fecha = vdiafecha[16] THEN tdias.dia_trab[16] = tdias.dia_trab[16]  + 1.
             IF liq_control_finca.fecha = vdiafecha[17] THEN tdias.dia_trab[17] = tdias.dia_trab[17]  + 1.
             IF liq_control_finca.fecha = vdiafecha[18] THEN tdias.dia_trab[18] = tdias.dia_trab[18]  + 1.
             IF liq_control_finca.fecha = vdiafecha[19] THEN tdias.dia_trab[19] = tdias.dia_trab[19]  + 1.
             IF liq_control_finca.fecha = vdiafecha[20] THEN tdias.dia_trab[20] = tdias.dia_trab[20]  + 1.
             IF liq_control_finca.fecha = vdiafecha[21] THEN tdias.dia_trab[21] = tdias.dia_trab[21]  + 1.
             IF liq_control_finca.fecha = vdiafecha[22] THEN tdias.dia_trab[22] = tdias.dia_trab[22]  + 1.
             IF liq_control_finca.fecha = vdiafecha[23] THEN tdias.dia_trab[23] = tdias.dia_trab[23]  + 1.
             IF liq_control_finca.fecha = vdiafecha[24] THEN tdias.dia_trab[24] = tdias.dia_trab[24]  + 1.
             IF liq_control_finca.fecha = vdiafecha[25] THEN tdias.dia_trab[25] = tdias.dia_trab[25]  + 1.
             IF liq_control_finca.fecha = vdiafecha[26] THEN tdias.dia_trab[26] = tdias.dia_trab[26]  + 1.
             IF liq_control_finca.fecha = vdiafecha[27] THEN tdias.dia_trab[27] = tdias.dia_trab[27]  + 1.
             IF liq_control_finca.fecha = vdiafecha[28] THEN tdias.dia_trab[28] = tdias.dia_trab[28]  + 1.
             IF liq_control_finca.fecha = vdiafecha[29] THEN tdias.dia_trab[29] = tdias.dia_trab[29]  + 1.
             IF liq_control_finca.fecha = vdiafecha[30] THEN tdias.dia_trab[30] = tdias.dia_trab[30]  + 1.
             IF liq_control_finca.fecha = vdiafecha[31] THEN tdias.dia_trab[31] = tdias.dia_trab[31]  + 1.
  
 END.


 FIND FIRST liq_empresas WHERE liq_empresas.id_empresa = p_empresa NO-LOCK NO-ERROR.
 IF AVAILABLE liq_empresas THEN vempresa = liq_empresas.descripcion.
                           ELSE vempresa = "Todas". 

output to value(p_archivo).
  PUT "Empresa ".
  PUT vempresa.
  PUT SKIP.
  put "Periodo ".
  put p_fecha_desde.
  put " - ".
  put p_fecha_hasta.
  put skip.
  PUT "Sector ".
  PUT "Todos".
  PUT SKIP.

  PUT "Empresa;".
  put "Legajo;". 
  put "Nombre;".
  vcant = (p_fecha_hasta - p_fecha_desde) + 1.
  vfecha = p_fecha_desde.
  DO i = 1 TO vcant:
      vdiafecha[i] = vfecha.
      PUT vfecha.
      PUT ";".
      vfecha = vfecha + 1.
  END.
  put skip.


  FOR EACH tdias NO-LOCK BY tdias.id_empresa BY tdias.legajo_rhpro:
          export delimiter ";" 
              tdias.id_empresa
              tdias.legajo_rhpro
              tdias.nombre
              (IF tdias.dia_trab[1] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[2] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[3] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[4] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[5] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[6] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[7] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[8] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[9] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[10] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[11] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[12] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[13] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[14] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[15] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[16] <> 0 THEN "1" ELSE "") 
              (IF tdias.dia_trab[17] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[18] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[19] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[20] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[21] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[22] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[23] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[24] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[25] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[26] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[27] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[28] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[29] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[30] <> 0 THEN "1" ELSE "")
              (IF tdias.dia_trab[31] <> 0 THEN "1" ELSE "").
  END.

output close. 

message "Archivo generado " p_archivo view-as alert-box.



run p_texto_a_excel.p (input "TEXT;" + p_archivo).

