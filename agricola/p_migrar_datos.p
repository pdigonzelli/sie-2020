def temp-table t-control 
    FIELD id_empresa     AS INTEGER   FORMAT ">>>,>>9" LABEL "Empresa" COLUMN-LABEL "Empresa"
    FIELD id_sucursal    AS INTEGER   FORMAT ">>9" LABEL "Sucursal" COLUMN-LABEL "Suc."
    FIELD id_sector      AS INTEGER   LABEL "Sector" COLUMN-LABEL "Sector"
    FIELD fecha          AS DATE      FORMAT "99/99/9999"
    FIELD id_proveedor   AS INTEGER   FORMAT ">>,>>9" LABEL "Cod. Proveedor" COLUMN-LABEL "Cod. Proveedor"
    FIELD id_origen      AS INTEGER   FORMAT ">,>>9" LABEL "Origen" COLUMN-LABEL "Origen"
    FIELD id_grupo       AS INTEGER   FORMAT ">>9" LABEL "Grupo" COLUMN-LABEL "Grupo"
    FIELD nro_planilla   AS INTEGER   LABEL "Nro Planilla" COLUMN-LABEL "Nro Planilla"
    FIELD observaciones  AS CHARACTER FORMAT "X(100)"
    FIELD id_liquidacion AS INTEGER
    FIELD c_fecha        AS DATE      LABEL "Fecha" COLUMN-LABEL "Fecha"
    FIELD c_hora         AS CHARACTER FORMAT "X(8)" LABEL "Hora" COLUMN-LABEL "Hora"
    FIELD c_usuario      AS CHARACTER FORMAT "X(8)" LABEL "Usuario" COLUMN-LABEL "Usrio.".

DEF TEMP-TABLE t-items
    FIELD id_empresa            AS INTEGER   FORMAT ">>>,>>9" LABEL "Empresa" COLUMN-LABEL "Empresa"
    FIELD id_sucursal           AS INTEGER   FORMAT ">>9" LABEL "Sucursal" COLUMN-LABEL "Suc."
    FIELD id_sector             AS INTEGER   LABEL "Sector" COLUMN-LABEL "Sector"
    FIELD fecha                 AS DATE      FORMAT "99/99/9999"
    FIELD id_proveedor          AS INTEGER   FORMAT ">>,>>9" LABEL "Cod. Proveedor" COLUMN-LABEL "Cod. Proveedor"
    FIELD id_origen             AS INTEGER   FORMAT ">,>>9" LABEL "Origen" COLUMN-LABEL "Origen"
    FIELD id_grupo              AS INTEGER   FORMAT ">>9" LABEL "Grupo" COLUMN-LABEL "Grupo"
    FIELD legajo                AS INTEGER   FORMAT ">>>,>>9" LABEL "Legajo" COLUMN-LABEL "Legajo"
    FIELD nombre                AS CHARACTER FORMAT "x(30)" LABEL "Apellido y Nombres" COLUMN-LABEL "Apellido y Nombres"
    FIELD id_tarea              AS INTEGER   FORMAT ">>>,>>9" LABEL "Tarea" COLUMN-LABEL "Tarea"
    FIELD id_lote               AS INTEGER   FORMAT ">>>9" LABEL "Lote" COLUMN-LABEL "Lote"
    FIELD cant_jornal           AS DECIMAL   DECIMALS 2 FORMAT ">>9.99" LABEL "Jornal" COLUMN-LABEL "Jornal"
    FIELD cant_horas            AS DECIMAL   DECIMALS 2 FORMAT ">>9.99" LABEL "Horas" COLUMN-LABEL "Horas"
    FIELD id_unidad_liquidacion AS INTEGER   FORMAT ">>>,>>9" LABEL "Unid. Liquidacion" COLUMN-LABEL "Unid. Liq."
    FIELD cantidad              AS DECIMAL   DECIMALS 2
    FIELD id_centro_costo       AS CHARACTER FORMAT "99.99.99.999.999" LABEL "Centro de Costo" COLUMN-LABEL "Cen.Cos."
    FIELD nro_tractor           AS INTEGER   FORMAT ">>>,>>9" LABEL "Nro Tractor" COLUMN-LABEL "Nro Tractor"
    FIELD nro_maquina           AS INTEGER   FORMAT ">,>>>,>>9" LABEL "Nro Maquina" COLUMN-LABEL "Nro Maquina"
    FIELD dni_cuil              AS CHARACTER FORMAT "X(12)" LABEL "CUIL" COLUMN-LABEL "CUIL"
    FIELD c_usuario             AS CHARACTER FORMAT "X(8)" LABEL "Usuario" COLUMN-LABEL "Usrio."
    FIELD c_fecha               AS DATE      LABEL "Fecha" COLUMN-LABEL "Fecha"
    FIELD c_hora                AS CHARACTER FORMAT "X(8)" LABEL "Hora" COLUMN-LABEL "Hora".


/*input from z:\sistemas\sami\util\ctrl_tar.d.
repeat:
  create t-control.
  import t-control.
end.
input close.

for each t-control WHERE fecha >= DATE("01/01/2009") AND fecha <= DATE("31/12/2009") NO-LOCK:
    CREATE CONTROL_tareas.
    BUFFER-COPY t-control TO CONTROL_tareas.
end.*/


input from z:\sistemas\sami\util\itm_tar.d.
repeat:
  create t-items.
  import t-items.
end.
input close.

for each t-items WHERE fecha >= DATE("01/01/2009") AND fecha <= DATE("31/12/2009") NO-LOCK:
    CREATE items_control_tareas.
    BUFFER-COPY t-items TO items_control_tareas.
end.
