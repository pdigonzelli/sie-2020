def temp-table t-temp1 LIKE liq_empresa. 
def temp-table t-temp2 LIKE liq_sectores. 
def temp-table t-temp3 LIKE liq_cargo.
def temp-table t-temp4 LIKE liq_contratos.
def temp-table t-temp5 LIKE liq_convenio.
def temp-table t-temp6 LIKE liq_centros_costos.
def temp-table t-temp7 LIKE liq_categorias.
def temp-table t-temp8 LIKE liq_motivo_baja.
def temp-table t-temp9 LIKE liq_legajos.
DEF TEMP-TABLE t-temp10 LIKE liq_licencias.

DEF VAR i AS INTEGER.
DEF VAR v-titulo AS CHARACTER. 


input from \\Srvdatasul\Gessi\empresas.txt.
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
    FIND FIRST liq_empresa OF t-temp1 NO-ERROR.
    IF NOT AVAILABLE liq_empresa THEN
        CREATE liq_empresa.
    BUFFER-COPY t-temp1 TO liq_empresa.
END.


input from \\Srvdatasul\Gessi\sectores.txt.
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

FOR EACH t-temp2 WHERE t-temp2.id_sector <> 0 NO-LOCK:
    FIND FIRST liq_sectores OF t-temp2 NO-ERROR.
    IF NOT AVAILABLE liq_sectores THEN
        CREATE liq_sectores.
    BUFFER-COPY t-temp2 TO liq_sectores.
END.

input from \\Srvdatasul\Gessi\cargo.txt.
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
    FIND FIRST liq_cargo OF t-temp3 NO-ERROR.
    IF NOT AVAILABLE liq_cargo THEN
        CREATE liq_cargo.
    BUFFER-COPY t-temp3 TO liq_cargo.
END.


input from \\Srvdatasul\Gessi\contrato.txt.
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
    FIND FIRST liq_contratos OF t-temp4 NO-ERROR.
    IF NOT AVAILABLE liq_contratos THEN
        CREATE liq_contratos.
    BUFFER-COPY t-temp4 TO liq_contratos.
END.

input from \\Srvdatasul\Gessi\convenio.txt.
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
    FIND FIRST liq_convenio OF t-temp5 NO-ERROR.
    IF NOT AVAILABLE liq_convenio THEN
        CREATE liq_convenio.
    BUFFER-COPY t-temp5 TO liq_convenio.
END.


input from \\Srvdatasul\Gessi\costos.txt.
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

FOR EACH t-temp6 WHERE t-temp6.id_centro_costo <> 0 NO-LOCK:
    FIND FIRST liq_centros_costos OF t-temp6 NO-ERROR.
    IF NOT AVAILABLE liq_centros_costos THEN
        CREATE liq_centros_costos.
    BUFFER-COPY t-temp6 TO liq_centros_costos.
END.

input from \\Srvdatasul\Gessi\categorias.txt.
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
    FIND FIRST liq_categorias OF t-temp7 NO-ERROR.
    IF NOT AVAILABLE liq_categorias THEN
        CREATE liq_categorias.
    BUFFER-COPY t-temp7 TO liq_categorias.
END.

input from \\Srvdatasul\Gessi\motibaja.txt.
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


input from \\Srvdatasul\Gessi\legajos.txt.
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
    FIND FIRST liq_legajos OF t-temp9 NO-ERROR.
    IF NOT AVAILABLE liq_legajos THEN
        CREATE liq_legajos.
    BUFFER-COPY t-temp9 TO liq_legajos.
END.


input from \\Srvdatasul\Gessi\licencias.txt.
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



MESSAGE "La importacion fue realizada" VIEW-AS ALERT-BOX WARNING.
