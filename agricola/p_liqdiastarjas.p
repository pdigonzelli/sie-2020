define input parameter p_empresa like liq_tarjas.id_empresa.
define input parameter p_fecha_desde like liq_tarjas.fecha.
define input parameter p_fecha_hasta like liq_tarjas.fecha.
define input parameter p_archivo as character.
 
DEF VAR vcant AS INTEGER.
DEF VAR vfecha AS DATE.
DEF VAR i AS INTEGER.
DEF VAR vempresa AS CHARACTER FORMAT "x(20)".
DEF VAR vvalor AS DECIMAL.
DEF VAR vdiafecha AS DATE EXTENT 31.
DEF VAR vdiatrab AS INTEGER EXTENT 31.


FIND FIRST liq_empresas WHERE liq_empresas.id_empresa = p_empresa NO-LOCK NO-ERROR.
IF AVAILABLE liq_empresas THEN vempresa = liq_empresas.descripcion.
                          ELSE vempresa = "Todas". 

if p_archivo = "" Then p_archivo = "z:\temp\reporte01.txt".

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

  PUT "Tipo;".
  PUT "Empresa;".
  put "Legajo;". 
  put "Nombre;".
  
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
      PUT vfecha.
      PUT ";".
      vfecha = vfecha + 1.
  END.
  put skip.
  
  /* Peones */

  FOR EACH liq_items_tarjas WHERE
      (IF p_empresa <> 0 THEN liq_items_tarjas.id_empresa = p_empresa ELSE TRUE)  AND
      liq_items_tarjas.id_empresa < 1000 AND
      liq_items_tarjas.fecha >= p_fecha_desde and
      liq_items_tarjas.fecha <= p_fecha_hasta AND
      liq_items_tarjas.id_tarea <> 0 no-lock,
      FIRST liq_tareas OF liq_items_tarjas WHERE liq_tareas.id_grupo_tarea <> 8 NO-LOCK,
      FIRST liq_legajos WHERE liq_legajos.id_empresa_liq = liq_items_tarjas.id_empresa and
                              liq_legajos.legajo = liq_items_tarjas.legajo 
      NO-LOCK BREAK BY liq_legajos.id_empresa BY liq_legajos.legajo_rhpro BY liq_items_tarjas.fecha:

      /*FIND FIRST liq_tareas OF liq_items_tarjas NO-LOCK NO-ERROR.
      IF liq_tareas.id_grupo_tarea = 8 THEN  NEXT. */

      IF liq_items_tarjas.fecha = vdiafecha[1] THEN  vdiatrab[1] = vdiatrab[1] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[2] THEN  vdiatrab[2] = vdiatrab[2] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[3] THEN  vdiatrab[3] = vdiatrab[3] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[4] THEN  vdiatrab[4] = vdiatrab[4] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[5] THEN  vdiatrab[5] = vdiatrab[5] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[6] THEN  vdiatrab[6] = vdiatrab[6] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[7] THEN  vdiatrab[7] = vdiatrab[7] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[8] THEN  vdiatrab[8] = vdiatrab[8] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[9] THEN  vdiatrab[9] = vdiatrab[9] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[10] THEN  vdiatrab[10] = vdiatrab[10] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[11] THEN  vdiatrab[11] = vdiatrab[11] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[12] THEN  vdiatrab[12] = vdiatrab[12] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[13] THEN  vdiatrab[13] = vdiatrab[13] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[14] THEN  vdiatrab[14] = vdiatrab[14] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[15] THEN  vdiatrab[15] = vdiatrab[15] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[16] THEN  vdiatrab[16] = vdiatrab[16] + 1.
      IF liq_items_tarjas.fecha = vdiafecha[17] THEN vdiatrab[17] = vdiatrab[17]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[18] THEN vdiatrab[18] = vdiatrab[18]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[19] THEN vdiatrab[19] = vdiatrab[19]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[20] THEN vdiatrab[20] = vdiatrab[20]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[21] THEN vdiatrab[21] = vdiatrab[21]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[22] THEN vdiatrab[22] = vdiatrab[22]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[23] THEN vdiatrab[23] = vdiatrab[23]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[24] THEN vdiatrab[24] = vdiatrab[24]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[25] THEN vdiatrab[25] = vdiatrab[25]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[26] THEN vdiatrab[26] = vdiatrab[26]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[27] THEN vdiatrab[27] = vdiatrab[27]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[28] THEN vdiatrab[28] = vdiatrab[28]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[29] THEN vdiatrab[29] = vdiatrab[29]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[30] THEN vdiatrab[30] = vdiatrab[30]  + 1.
      IF liq_items_tarjas.fecha = vdiafecha[31] THEN vdiatrab[31] = vdiatrab[31]  + 1.
     
     IF LAST-OF(liq_legajos.legajo_rhpro) THEN
     DO:
         export delimiter ";" 
             "agricola"
                liq_legajos.id_empresa
                liq_legajos.legajo_rhpro
                liq_items_tarjas.nombre
             (IF vdiatrab[1] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[2] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[3] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[4] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[5] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[6] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[7] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[8] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[9] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[10] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[11] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[12] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[13] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[14] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[15] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[16] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[17] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[18] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[19] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[20] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[21] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[22] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[23] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[24] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[25] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[26] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[27] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[28] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[29] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[30] <> 0 THEN "1" ELSE "")
             (IF vdiatrab[31] <> 0 THEN "1" ELSE "").


         DO i = 1 TO vcant:
             vdiatrab[i] = 0.
         END.
     END.                   .
  end.

  /**************************************************************************/

  DO i = 1 TO vcant:
    vdiatrab[i] = 0.
  END.

  /* Capataces */
  for each liq_control_finca where 
            (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
            liq_control_finca.fecha >= p_fecha_desde  AND
            liq_control_finca.fecha <= p_fecha_hasta NO-LOCK
            BREAK  by liq_control_finca.id_empresa BY liq_control_finca.id_capataz: 

      IF liq_control_finca.fecha = vdiafecha[1] THEN  vdiatrab[1] = vdiatrab[1] + 1.
      IF liq_control_finca.fecha = vdiafecha[2] THEN  vdiatrab[2] = vdiatrab[2] + 1.
      IF liq_control_finca.fecha = vdiafecha[3] THEN  vdiatrab[3] = vdiatrab[3] + 1.
      IF liq_control_finca.fecha = vdiafecha[4] THEN  vdiatrab[4] = vdiatrab[4] + 1.
      IF liq_control_finca.fecha = vdiafecha[5] THEN  vdiatrab[5] = vdiatrab[5] + 1.
      IF liq_control_finca.fecha = vdiafecha[6] THEN  vdiatrab[6] = vdiatrab[6] + 1.
      IF liq_control_finca.fecha = vdiafecha[7] THEN  vdiatrab[7] = vdiatrab[7] + 1.
      IF liq_control_finca.fecha = vdiafecha[8] THEN  vdiatrab[8] = vdiatrab[8] + 1.
      IF liq_control_finca.fecha = vdiafecha[9] THEN  vdiatrab[9] = vdiatrab[9] + 1.
      IF liq_control_finca.fecha = vdiafecha[10] THEN  vdiatrab[10] = vdiatrab[10] + 1.
      IF liq_control_finca.fecha = vdiafecha[11] THEN  vdiatrab[11] = vdiatrab[11] + 1.
      IF liq_control_finca.fecha = vdiafecha[12] THEN  vdiatrab[12] = vdiatrab[12] + 1.
      IF liq_control_finca.fecha = vdiafecha[13] THEN  vdiatrab[13] = vdiatrab[13] + 1.
      IF liq_control_finca.fecha = vdiafecha[14] THEN  vdiatrab[14] = vdiatrab[14] + 1.
      IF liq_control_finca.fecha = vdiafecha[15] THEN  vdiatrab[15] = vdiatrab[15] + 1.
      IF liq_control_finca.fecha = vdiafecha[16] THEN  vdiatrab[16] = vdiatrab[16] + 1.
      IF liq_control_finca.fecha = vdiafecha[17] THEN vdiatrab[17] = vdiatrab[17]  + 1.
      IF liq_control_finca.fecha = vdiafecha[18] THEN vdiatrab[18] = vdiatrab[18]  + 1.
      IF liq_control_finca.fecha = vdiafecha[19] THEN vdiatrab[19] = vdiatrab[19]  + 1.
      IF liq_control_finca.fecha = vdiafecha[20] THEN vdiatrab[20] = vdiatrab[20]  + 1.
      IF liq_control_finca.fecha = vdiafecha[21] THEN vdiatrab[21] = vdiatrab[21]  + 1.
      IF liq_control_finca.fecha = vdiafecha[22] THEN vdiatrab[22] = vdiatrab[22]  + 1.
      IF liq_control_finca.fecha = vdiafecha[23] THEN vdiatrab[23] = vdiatrab[23]  + 1.
      IF liq_control_finca.fecha = vdiafecha[24] THEN vdiatrab[24] = vdiatrab[24]  + 1.
      IF liq_control_finca.fecha = vdiafecha[25] THEN vdiatrab[25] = vdiatrab[25]  + 1.
      IF liq_control_finca.fecha = vdiafecha[26] THEN vdiatrab[26] = vdiatrab[26]  + 1.
      IF liq_control_finca.fecha = vdiafecha[27] THEN vdiatrab[27] = vdiatrab[27]  + 1.
      IF liq_control_finca.fecha = vdiafecha[28] THEN vdiatrab[28] = vdiatrab[28]  + 1.
      IF liq_control_finca.fecha = vdiafecha[29] THEN vdiatrab[29] = vdiatrab[29]  + 1.
      IF liq_control_finca.fecha = vdiafecha[30] THEN vdiatrab[30] = vdiatrab[30]  + 1.
      IF liq_control_finca.fecha = vdiafecha[31] THEN vdiatrab[31] = vdiatrab[31]  + 1.
      

    IF LAST-OF(liq_control_finca.id_capataz) THEN
     DO:

        FIND FIRST liq_capataces OF liq_control_finca NO-LOCK NO-ERROR.
        IF AVAILABLE liq_capataces THEN
        DO:
            IF p_empresa <> 101 THEN
                    vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
               ELSE vvalor = 0.

             export delimiter ";" 
                  "cosecha-capataz"  
                   liq_capataces.id_empresa
                 (vvalor + liq_capataces.legajo)
                    liq_capataces.nombre
                     (IF vdiatrab[1] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[2] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[3] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[4] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[5] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[6] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[7] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[8] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[9] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[10] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[11] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[12] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[13] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[14] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[15] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[16] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[17] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[18] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[19] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[20] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[21] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[22] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[23] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[24] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[25] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[26] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[27] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[28] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[29] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[30] <> 0 THEN "1" ELSE "")
                     (IF vdiatrab[31] <> 0 THEN "1" ELSE "").


             DO i = 1 TO vcant:
                 vdiatrab[i] = 0.
             END.

         END.
     END.                   .

   END.
/***************************************************************/

  DO i = 1 TO vcant:
    vdiatrab[i] = 0.
  END.

  /* Ayudante Capataz */
  for each liq_control_finca where 
             (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
             liq_control_finca.fecha >= p_fecha_desde  AND
             liq_control_finca.fecha <= p_fecha_hasta  no-lock, 
              each liq_items_control_finca of liq_control_finca no-lock
              where id_causa_ausencia = 15 
              BREAK  by liq_items_control_finca.id_empresa BY liq_items_control_finca.legajo: 
   
      IF liq_control_finca.fecha = vdiafecha[1] THEN  vdiatrab[1] = vdiatrab[1] + 1.
      IF liq_control_finca.fecha = vdiafecha[2] THEN  vdiatrab[2] = vdiatrab[2] + 1.
      IF liq_control_finca.fecha = vdiafecha[3] THEN  vdiatrab[3] = vdiatrab[3] + 1.
      IF liq_control_finca.fecha = vdiafecha[4] THEN  vdiatrab[4] = vdiatrab[4] + 1.
      IF liq_control_finca.fecha = vdiafecha[5] THEN  vdiatrab[5] = vdiatrab[5] + 1.
      IF liq_control_finca.fecha = vdiafecha[6] THEN  vdiatrab[6] = vdiatrab[6] + 1.
      IF liq_control_finca.fecha = vdiafecha[7] THEN  vdiatrab[7] = vdiatrab[7] + 1.
      IF liq_control_finca.fecha = vdiafecha[8] THEN  vdiatrab[8] = vdiatrab[8] + 1.
      IF liq_control_finca.fecha = vdiafecha[9] THEN  vdiatrab[9] = vdiatrab[9] + 1.
      IF liq_control_finca.fecha = vdiafecha[10] THEN  vdiatrab[10] = vdiatrab[10] + 1.
      IF liq_control_finca.fecha = vdiafecha[11] THEN  vdiatrab[11] = vdiatrab[11] + 1.
      IF liq_control_finca.fecha = vdiafecha[12] THEN  vdiatrab[12] = vdiatrab[12] + 1.
      IF liq_control_finca.fecha = vdiafecha[13] THEN  vdiatrab[13] = vdiatrab[13] + 1.
      IF liq_control_finca.fecha = vdiafecha[14] THEN  vdiatrab[14] = vdiatrab[14] + 1.
      IF liq_control_finca.fecha = vdiafecha[15] THEN  vdiatrab[15] = vdiatrab[15] + 1.
      IF liq_control_finca.fecha = vdiafecha[16] THEN  vdiatrab[16] = vdiatrab[16] + 1.
      IF liq_control_finca.fecha = vdiafecha[17] THEN vdiatrab[17] = vdiatrab[17]  + 1.
      IF liq_control_finca.fecha = vdiafecha[18] THEN vdiatrab[18] = vdiatrab[18]  + 1.
      IF liq_control_finca.fecha = vdiafecha[19] THEN vdiatrab[19] = vdiatrab[19]  + 1.
      IF liq_control_finca.fecha = vdiafecha[20] THEN vdiatrab[20] = vdiatrab[20]  + 1.
      IF liq_control_finca.fecha = vdiafecha[21] THEN vdiatrab[21] = vdiatrab[21]  + 1.
      IF liq_control_finca.fecha = vdiafecha[22] THEN vdiatrab[22] = vdiatrab[22]  + 1.
      IF liq_control_finca.fecha = vdiafecha[23] THEN vdiatrab[23] = vdiatrab[23]  + 1.
      IF liq_control_finca.fecha = vdiafecha[24] THEN vdiatrab[24] = vdiatrab[24]  + 1.
      IF liq_control_finca.fecha = vdiafecha[25] THEN vdiatrab[25] = vdiatrab[25]  + 1.
      IF liq_control_finca.fecha = vdiafecha[26] THEN vdiatrab[26] = vdiatrab[26]  + 1.
      IF liq_control_finca.fecha = vdiafecha[27] THEN vdiatrab[27] = vdiatrab[27]  + 1.
      IF liq_control_finca.fecha = vdiafecha[28] THEN vdiatrab[28] = vdiatrab[28]  + 1.
      IF liq_control_finca.fecha = vdiafecha[29] THEN vdiatrab[29] = vdiatrab[29]  + 1.
      IF liq_control_finca.fecha = vdiafecha[30] THEN vdiatrab[30] = vdiatrab[30]  + 1.
      IF liq_control_finca.fecha = vdiafecha[31] THEN vdiatrab[31] = vdiatrab[31]  + 1.
      
      
      
      
  IF LAST-OF(liq_items_control_finca.legajo) THEN
      DO:
      IF p_empresa <> 101 THEN
             vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
        ELSE vvalor = 0.

          export delimiter ";" 
              "cosecha ayudante"
                 liq_items_control_finca.id_empresa
                 (vvalor + liq_items_control_finca.legajo)
                 liq_items_control_finca.nombre
              (IF vdiatrab[1] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[2] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[3] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[4] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[5] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[6] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[7] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[8] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[9] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[10] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[11] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[12] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[13] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[14] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[15] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[16] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[17] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[18] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[19] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[20] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[21] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[22] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[23] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[24] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[25] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[26] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[27] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[28] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[29] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[30] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[31] <> 0 THEN "1" ELSE "").


             DO i = 1 TO vcant:
                 vdiatrab[i] = 0.
             END.

      END.                   .

  END.

/***************************************************************/
  DO i = 1 TO vcant:
    vdiatrab[i] = 0.
  END.


  /* Peones Cosecha */
  for each liq_control_finca where 
             (IF p_empresa <> 0 THEN liq_control_finca.id_empresa = p_empresa ELSE TRUE)  AND
             liq_control_finca.fecha >= p_fecha_desde  AND
             liq_control_finca.fecha <= p_fecha_hasta  no-lock, 
              each liq_items_control_finca of liq_control_finca no-lock
              where  (liq_items_control_finca.cantidad_tijera <> 0 or liq_items_control_finca.cantidad_mano <> 0) AND
                     liq_items_control_finca.id_causa_ausencia <> 15
              BREAK BY liq_items_control_finca.id_empresa  by liq_items_control_finca.legajo: 
   
      IF liq_control_finca.fecha = vdiafecha[1] THEN  vdiatrab[1] = vdiatrab[1] + 1.
      IF liq_control_finca.fecha = vdiafecha[2] THEN  vdiatrab[2] = vdiatrab[2] + 1.
      IF liq_control_finca.fecha = vdiafecha[3] THEN  vdiatrab[3] = vdiatrab[3] + 1.
      IF liq_control_finca.fecha = vdiafecha[4] THEN  vdiatrab[4] = vdiatrab[4] + 1.
      IF liq_control_finca.fecha = vdiafecha[5] THEN  vdiatrab[5] = vdiatrab[5] + 1.
      IF liq_control_finca.fecha = vdiafecha[6] THEN  vdiatrab[6] = vdiatrab[6] + 1.
      IF liq_control_finca.fecha = vdiafecha[7] THEN  vdiatrab[7] = vdiatrab[7] + 1.
      IF liq_control_finca.fecha = vdiafecha[8] THEN  vdiatrab[8] = vdiatrab[8] + 1.
      IF liq_control_finca.fecha = vdiafecha[9] THEN  vdiatrab[9] = vdiatrab[9] + 1.
      IF liq_control_finca.fecha = vdiafecha[10] THEN  vdiatrab[10] = vdiatrab[10] + 1.
      IF liq_control_finca.fecha = vdiafecha[11] THEN  vdiatrab[11] = vdiatrab[11] + 1.
      IF liq_control_finca.fecha = vdiafecha[12] THEN  vdiatrab[12] = vdiatrab[12] + 1.
      IF liq_control_finca.fecha = vdiafecha[13] THEN  vdiatrab[13] = vdiatrab[13] + 1.
      IF liq_control_finca.fecha = vdiafecha[14] THEN  vdiatrab[14] = vdiatrab[14] + 1.
      IF liq_control_finca.fecha = vdiafecha[15] THEN  vdiatrab[15] = vdiatrab[15] + 1.
      IF liq_control_finca.fecha = vdiafecha[16] THEN  vdiatrab[16] = vdiatrab[16] + 1.
      IF liq_control_finca.fecha = vdiafecha[17] THEN vdiatrab[17] = vdiatrab[17]  + 1.
      IF liq_control_finca.fecha = vdiafecha[18] THEN vdiatrab[18] = vdiatrab[18]  + 1.
      IF liq_control_finca.fecha = vdiafecha[19] THEN vdiatrab[19] = vdiatrab[19]  + 1.
      IF liq_control_finca.fecha = vdiafecha[20] THEN vdiatrab[20] = vdiatrab[20]  + 1.
      IF liq_control_finca.fecha = vdiafecha[21] THEN vdiatrab[21] = vdiatrab[21]  + 1.
      IF liq_control_finca.fecha = vdiafecha[22] THEN vdiatrab[22] = vdiatrab[22]  + 1.
      IF liq_control_finca.fecha = vdiafecha[23] THEN vdiatrab[23] = vdiatrab[23]  + 1.
      IF liq_control_finca.fecha = vdiafecha[24] THEN vdiatrab[24] = vdiatrab[24]  + 1.
      IF liq_control_finca.fecha = vdiafecha[25] THEN vdiatrab[25] = vdiatrab[25]  + 1.
      IF liq_control_finca.fecha = vdiafecha[26] THEN vdiatrab[26] = vdiatrab[26]  + 1.
      IF liq_control_finca.fecha = vdiafecha[27] THEN vdiatrab[27] = vdiatrab[27]  + 1.
      IF liq_control_finca.fecha = vdiafecha[28] THEN vdiatrab[28] = vdiatrab[28]  + 1.
      IF liq_control_finca.fecha = vdiafecha[29] THEN vdiatrab[29] = vdiatrab[29]  + 1.
      IF liq_control_finca.fecha = vdiafecha[30] THEN vdiatrab[30] = vdiatrab[30]  + 1.
      IF liq_control_finca.fecha = vdiafecha[31] THEN vdiatrab[31] = vdiatrab[31]  + 1.
      
      
      
  IF LAST-OF(liq_items_control_finca.legajo) THEN
      DO:
          IF p_empresa <> 101 THEN
             vvalor = integer(SUBSTRING(STRING(liq_control_finca.id_empresa),1,1)) * 10000000.
           ELSE vvalor = 0.

          export delimiter ";" 
              "cosecha peones"
                 liq_items_control_finca.id_empresa
                 (vvalor + liq_items_control_finca.legajo)
                 liq_items_control_finca.nombre
              (IF vdiatrab[1] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[2] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[3] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[4] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[5] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[6] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[7] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[8] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[9] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[10] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[11] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[12] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[13] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[14] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[15] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[16] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[17] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[18] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[19] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[20] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[21] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[22] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[23] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[24] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[25] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[26] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[27] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[28] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[29] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[30] <> 0 THEN "1" ELSE "")
              (IF vdiatrab[31] <> 0 THEN "1" ELSE "").

             DO i = 1 TO vcant:
                 vdiatrab[i] = 0.
             END.

      END.                   .

  END.





output close.

message "Archivo generado " p_archivo view-as alert-box.


run p_texto_a_excel.p (input "TEXT;" + p_archivo).
