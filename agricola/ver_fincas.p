DEF VAR v_tipo AS CHARACTER FORMAT "x(15)".
OUTPUT TO z:\temp\fincas.txt.
FOR EACH origenes WHERE finca = YES AND estado = YES NO-LOCK,
    FIRST proveedores OF origenes NO-LOCK:
  FIND FIRST tipo_origen OF origenes NO-LOCK NO-ERROR.
  IF AVAILABLE tipo_origen THEN   v_tipo = tipo_origen.descripcion.
                           ELSE   v_tipo = "".
  EXPORT DELIMITER ";" 
      v_tipo
      origenes.id_proveedor
      proveedores.razon_social
      origenes.id_origen
      origenes.descripcion.


END.
OUTPUT CLOSE.
