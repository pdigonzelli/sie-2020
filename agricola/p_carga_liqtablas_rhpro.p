def temp-table t-temp1 
FIELD descripcion AS CHARACTER
FIELD id_empresa AS INTEGER.

def temp-table t-temp9 
FIELD id_empresa AS INTEGER
FIELD legajo AS INTEGER
FIELD id_division AS INTEGER
FIELD id_centro_costo AS INTEGER
FIELD apellido AS CHARACTER
FIELD nombre AS CHARACTER
FIELD domicilio AS CHARACTER
FIELD localidad AS CHARACTER
FIELD codigo_postal AS INTEGER
FIELD fecha_nac AS DATE FORMAT "99/99/9999"
FIELD sexo AS CHARACTER
FIELD fecha_ult_alta AS DATE
FIELD fecha_alta AS DATE
FIELD id_causa AS INTEGER
FIELD id_convenio AS INTEGER
FIELD tipo_liq AS CHARACTER
FIELD cuit AS CHARACTER FORMAT "x(15)"
FIELD estado AS CHARACTER
FIELD fecha_baja AS DATE
FIELD id_categoria AS INTEGER
FIELD id_contrato AS INTEGER
FIELD id_cargo AS INTEGER.




def temp-table t-temp2 LIKE liq_divisionliq. 
def temp-table t-temp3 LIKE liq_ccargosliq.
def temp-table t-temp4 LIKE liq_ccontratosliq.
def temp-table t-temp5 LIKE liq_cconveniosliq.
def temp-table t-temp6 LIKE liq_ccostosliq.
def temp-table t-temp7 LIKE liq_ccategoriasliq.
def temp-table t-temp8 LIKE liq_motivo_baja.
DEF TEMP-TABLE t-temp10 LIKE liq_licencias.

DEF VAR i AS INTEGER.
DEF VAR v-titulo AS CHARACTER. 
DEF VAR v-path AS CHARACTER.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_legajo AS INTEGER.

v-path = "\\tucuman3\rhpro\".

input from VALUE(v-path + "empresas.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp1.
      import DELIMITER ";" t-temp1.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp1 WHERE t-temp1.id_empresa <> 0 NO-LOCK:
    FIND FIRST liq_empresa WHERE liq_empresa.id_empresa =  t-temp1.id_empresa NO-ERROR.
    IF NOT AVAILABLE liq_empresa THEN
    DO:
        CREATE liq_empresa.
        ASSIGN liq_empresa.id_empresa = t-temp1.id_empresa.
    END.  
    ASSIGN liq_empresa.descripcion = t-temp1.descripcion.
END.

input from VALUE(v-path + "legajos.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp9.
      import DELIMITER ";" t-temp9.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp9 WHERE t-temp9.legajo <> 0 NO-LOCK:
    IF t-temp9.id_empresa <> 101 THEN
                         DO:
                          v_inicio = integer(SUBSTRING(STRING(t-temp9.id_empresa),1,1)).
                          v_legajo = t-temp9.legajo - (v_inicio * 10000000).
                         END.
                      ELSE v_legajo = t-temp9.legajo.
    FIND FIRST liq_legajos WHERE liq_legajos.id_empresa = t-temp9.id_empresa AND
        liq_legajos.legajo = v_legajo NO-ERROR.
    IF NOT AVAILABLE liq_legajos THEN
        DO:
            CREATE liq_legajos.
            ASSIGN liq_legajos.id_empresa = t-temp9.id_empresa
                   liq_legajos.legajo = v_legajo. 
                   
           ASSIGN 
               liq_legajos.legajo_rhpro = t-temp9.legajo
               liq_legajos.apellido_nombre = t-temp9.apellido + ", " + t-temp9.nombre
               liq_legajos.domicilio = t-temp9.domicilio
               liq_legajos.codigo_postal = string(t-temp9.codigo_postal)
               liq_legajos.localidad = t-temp9.localidad
               liq_legajos.fecha_nacimiento = t-temp9.fecha_nac
               liq_legajos.cuil = REPLACE(t-temp9.cuit,"-",""). 
               
            if t-temp9.sexo begins "Masculino" Then
               liq_legajos.sexo = "M". 
             Else
               liq_legajo.sexo = "F".
               
            if t-temp9.estado begins "Activo" Then
               liq_legajo.liquida = yes.
             Else 
               liq_legajo.liquida = no. 
               
            assign liq_legajos.fecha_ingreso = t-temp9.fecha_alta
                   liq_legajos.fecha_inicio_temp = t-temp9.fecha_ult_alta
                   liq_legajos.fecha_egreso = t-temp9.fecha_baja
                   liq_legajos.tipo_liq = t-temp9.tipo_liq.           
               
            CASE t-temp9.id_division:
                WHEN 4686 THEN liq_legajos.id_sector = 5.
                WHEN 4687 THEN liq_legajos.id_sector = 6.
            END CASE.
               
       END.

       /* Datos */

      ASSIGN  liq_legajos.apellido_nombre = t-temp9.apellido + ", " + t-temp9.nombre
              liq_legajos.domicilio = t-temp9.domicilio
              liq_legajos.codigo_postal = string(t-temp9.codigo_postal)
              liq_legajos.localidad = t-temp9.localidad
              liq_legajos.fecha_nacimiento = t-temp9.fecha_nac
              liq_legajos.cuil = REPLACE(t-temp9.cuit,"-",""). 

        if t-temp9.sexo begins "Masculino" Then
           liq_legajos.sexo = "M". 
         Else
           liq_legajo.sexo = "F".
    
        if t-temp9.estado begins "Activo" Then
           liq_legajo.liquida = yes.
         Else 
           liq_legajo.liquida = no. 
    
        assign liq_legajos.fecha_ingreso = t-temp9.fecha_alta
               liq_legajos.fecha_inicio_temp = t-temp9.fecha_ult_alta
               liq_legajos.fecha_egreso = t-temp9.fecha_baja
               liq_legajos.tipo_liq = t-temp9.tipo_liq
               liq_legajos.id_motivo_egreso = t-temp9.id_causa.           
    
        CASE t-temp9.id_division:
            WHEN 4686 THEN liq_legajos.id_sector = 5.
            WHEN 4687 THEN liq_legajos.id_sector = 6.
        END CASE.


       /* Datos rhpro */

       ASSIGN  liq_legajos.legajo_rhpro = t-temp9.legajo
              liq_legajos.id_ccontrato_liq = t-temp9.id_contrato
              liq_legajos.id_cconvenio_liq = t-temp9.id_convenio
              liq_legajos.id_ccargo_liq = t-temp9.id_cargo
              liq_legajos.id_ccategoria_liq = t-temp9.id_categoria
              liq_legajos.id_division = t-temp9.id_division
              liq_legajos.id_ccostos_liq = t-temp9.id_centro_costo.

       IF liq_legajos.id_capataz = ? THEN
           ASSIGN liq_legajos.id_capataz = 0.

       IF liq_legajos.grupo = ? THEN
           ASSIGN liq_legajos.grupo = 0.


END.





input from VALUE(v-path + "Division de Liquidacion.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp2.
      import DELIMITER ";" t-temp2.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp2 WHERE t-temp2.id_division <> 0 NO-LOCK:
    FIND FIRST liq_divisionliq OF t-temp2 NO-ERROR.
    IF NOT AVAILABLE liq_divisionliq THEN
        CREATE liq_divisionliq.
    BUFFER-COPY t-temp2 TO liq_divisionliq.
END.

input from VALUE(v-path + "Cargo.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp3.
      import DELIMITER ";" t-temp3.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp3 WHERE t-temp3.id_cargo <> 0 NO-LOCK:
    FIND FIRST liq_ccargosliq OF t-temp3 NO-ERROR.
    IF NOT AVAILABLE liq_ccargosliq THEN
        CREATE liq_ccargosliq.
    BUFFER-COPY t-temp3 TO liq_ccargosliq.
END.


input from VALUE(v-path + "Contrato.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp4.
      import DELIMITER ";" t-temp4.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp4 WHERE t-temp4.id_contrato <> 0 NO-LOCK:
    FIND FIRST liq_ccontratosliq OF t-temp4 NO-ERROR.
    IF NOT AVAILABLE liq_ccontratosliq THEN
        CREATE liq_ccontratosliq.
    BUFFER-COPY t-temp4 TO liq_ccontratosliq.
END.

input from VALUE(v-path + "Convenio.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp5.
      import DELIMITER ";" t-temp5.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp5 WHERE t-temp5.id_convenio <> 0 NO-LOCK:
    FIND FIRST liq_cconveniosliq OF t-temp5 NO-ERROR.
    IF NOT AVAILABLE liq_cconveniosliq THEN
        CREATE liq_cconveniosliq.
    BUFFER-COPY t-temp5 TO liq_cconveniosliq.
END.


input from VALUE(v-path + "Costos.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp6.
      import DELIMITER ";" t-temp6.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp6 WHERE t-temp6.id_centro_costo_liq <> 0 NO-LOCK:
    FIND FIRST liq_ccostosliq OF t-temp6 NO-ERROR.
    IF NOT AVAILABLE liq_ccostosliq THEN
        CREATE liq_ccostosliq.
    BUFFER-COPY t-temp6 EXCEPT id_proveedor id_origen TO liq_ccostosliq.
END.

input from VALUE(v-path + "Categor¡as.csv").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp7.
      import DELIMITER ";" t-temp7.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp7 WHERE t-temp7.id_categoria <> 0 NO-LOCK:
    FIND FIRST liq_ccategoriasliq OF t-temp7 NO-ERROR.
    IF NOT AVAILABLE liq_ccategoriasliq THEN
        CREATE liq_ccategoriasliq.
    BUFFER-COPY t-temp7 TO liq_ccategoriasliq.
END.

/*input from VALUE(v-path + "motibaja.txt").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp8.
      import DELIMITER ";" t-temp8.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp8 WHERE t-temp8.id_motivo_baja <> 0 NO-LOCK:
    FIND FIRST liq_motivo_baja OF t-temp8 NO-ERROR.
    IF NOT AVAILABLE liq_categorias THEN
        CREATE liq_motivo_baja.
    BUFFER-COPY t-temp8 TO liq_motivo_baja.
END.




input from VALUE(v-path + "licencias.txt").
i = 0.
repeat:
  i = i + 1.
  IF i > 1 THEN
  DO:
      create t-temp10.
      import DELIMITER ";" t-temp10.
  END.
  ELSE
      IMPORT DELIMITER ";" v-titulo.
end.
input close.

FOR EACH t-temp10 WHERE t-temp10.id_empresa_liq <> 0 NO-LOCK:
    FIND FIRST liq_licencias OF t-temp10 NO-ERROR.
    IF NOT AVAILABLE liq_licencias THEN
        CREATE liq_licencias.
    BUFFER-COPY t-temp10 TO liq_licencias.
END.
*/


MESSAGE "La importacion desde RHPRO fue realizada" VIEW-AS ALERT-BOX WARNING.
