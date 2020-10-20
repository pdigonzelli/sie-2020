DEF VAR v_empresa AS INTEGER.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_legajo AS INTEGER.
DEF VAR v_num AS INTEGER.
v_empresa = 303.
v_num = 101.
v_inicio = integer(SUBSTRING(STRING(v_empresa),1,1)).
v_legajo = (v_inicio * 10000000) + v_num.


MESSAGE v_inicio v_legajo VIEW-AS ALERT-BOX.
