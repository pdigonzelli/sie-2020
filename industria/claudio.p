DEFINE VARIABLE v_suc AS INTEGER.
DEFINE VARIABLE v_tip AS INTEGER.

DEFINE TEMP-TABLE t_rem
    FIELD nro   AS CHARACTER FORMAT "x(11)".

INPUT FROM g:\remiindustria.csv.
REPEAT :
    CREATE t_rem.
    IMPORT t_rem.
    ASSIGN t_rem.nro = REPLACE(t_rem.nro, "-", "").
END.
INPUT CLOSE.

FOR EACH t_rem WHERE t_rem.nro <> "":

    ASSIGN v_suc = 0
           v_tip = 0.
    IF SUBSTRING(t_rem.nro,1,4) = "0070" THEN
       ASSIGN v_suc = 96
              v_tip = 123.
    IF SUBSTRING(t_rem.nro,1,4) = "0069" THEN
       ASSIGN v_suc = 95
              v_tip = 123.
    IF SUBSTRING(t_rem.nro,1,4) = "0032" THEN
       ASSIGN v_suc = 95
              v_tip = 122.
    IF SUBSTRING(t_rem.nro,1,4) = "0042" THEN
       ASSIGN v_suc = 96
              v_tip = 122.


    FIND FIRST remitos WHERE
        remitos.id_sucursal     = v_suc AND
        remitos.id_tipo_movsto  = v_tip AND
        remitos.nro_comprobante = t_rem.nro
        NO-ERROR.
    IF NOT AVAILABLE remitos THEN NEXT.

    ASSIGN remitos.impresion = 0.

    /*DISPLAY remitos.nro_comprobante WITH 11 DOWN.*/

    DISPLAY t_rem.nro FORMAT "x(12)" WITH FRAME f_ver 11 DOWN.
    DOWN WITH FRAME f_ver.
    PAUSE 0.
    
END.

