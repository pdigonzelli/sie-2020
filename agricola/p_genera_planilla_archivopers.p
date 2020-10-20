  DEFINE INPUT PARAMETER v_tipo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_empresa AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_sucursal AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_sector AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_fecha AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER v_proveedor AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_origen AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_grupo AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER v_fecha_desde AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER v_fecha_hasta AS DATE NO-UNDO.


  DEF OUTPUT PARAMETER v_nro AS INTEGER NO-UNDO.

  DEFINE BUFFER bpers FOR personal_finca.
  DEFINE BUFFER bcontrol-1 FOR CONTROL_tareas.
  DEF VAR v_quin AS INTEGER NO-UNDO.
  DEF VAR v_sem AS INTEGER NO-UNDO.
  DEF VAR v_total_hs AS DECIMAL.

  find last bpers where bpers.id_empresa = v_empresa and
       bpers.id_proveedor = v_proveedor and
       bpers.id_origen =  v_origen no-lock no-error.
  if not available bpers Then
     do: 
        message "No existen legajos para esa empresa en archivo personal" view-as ALERT-BOX WARNING.    
        return.
     end.
    Else
    do:
        find last bcontrol-1 where bcontrol-1.id_empresa = v_empresa and
             bcontrol-1.id_sucursal = v_sucursal and 
             bcontrol-1.id_sector = v_sector and 
             bcontrol-1.fecha = v_fecha and 
             bcontrol-1.id_proveedor = v_proveedor and
             bcontrol-1.id_origen =  v_origen and
             bcontrol-1.id_grupo = v_grupo and
             bcontrol-1.id_tipo_planilla = v_tipo no-lock no-error.
        IF AVAILABLE bcontrol-1 THEN
        DO:
            message "Ya fue creada esta planilla en esta fecha" SKIP
                "Genera otra planilla?"
                view-as ALERT-BOX WARNING
                BUTTONS YES-NO
                UPDATE respuesta AS LOGICAL.    
           IF NOT respuesta  THEN RETURN.
        END.


         CREATE CONTROL_tareas.
         ASSIGN CONTROL_tareas.id_empresa = v_empresa 
                CONTROL_tareas.id_sucursal = v_sucursal 
                CONTROL_tareas.id_sector = v_sector
                CONTROL_tareas.id_proveedor = v_proveedor 
                CONTROL_tareas.id_origen =  v_origen 
                CONTROL_tareas.id_grupo = v_grupo.
         ASSIGN CONTROL_tareas.fecha = v_fecha
                CONTROL_tareas.id_tipo_planilla = v_tipo
                CONTROL_tareas.c_usuario = userid("userdb")
                CONTROL_tareas.c_fecha = TODAY
                CONTROL_tareas.c_hora = STRING(TIME,"HH:MM:SS").

         RUN devuelve-nro (INPUT v_sucursal, INPUT v_sector, INPUT v_tipo, OUTPUT v_nro).
         ASSIGN CONTROL_tareas.nro_planilla = v_nro.
         

         MESSAGE "Planilla generada tipo " v_tipo " Nro " v_nro VIEW-AS ALERT-BOX.


         FOR EACH bpers WHERE bpers.id_empresa = v_empresa and
                  bpers.id_proveedor = v_proveedor and
                  bpers.id_origen =  v_origen NO-LOCK:
            
             RUN devuelve-total-horas (INPUT v_tipo, INPUT v_nro, INPUT bpers.id_empresa, INPUT bpers.legajo, OUTPUT v_total_hs).

            CREATE items_control_tareas.
            BUFFER-COPY CONTROL_tareas TO items_control_tareas. 
            ASSIGN  items_control_tareas.id_tipo_planilla = CONTROL_tareas.id_tipo_planilla
                    items_control_tareas.legajo = bpers.legajo
                    items_control_tareas.nombre = bpers.nombre
                    items_control_tareas.dni_cuil = bpers.dni_cuil
                    items_control_tareas.total_horas = v_total_hs.

            ASSIGN items_control_tareas.c_usuario = userid("userdb")
                   items_control_tareas.c_fecha = TODAY
                   items_control_tareas.c_hora = STRING(TIME,"HH:MM:SS").


         END.
    
         
END.




PROCEDURE devuelve-nro.
DEFINE INPUT PARAMETER p-suc AS INTEGER.
DEFINE INPUT PARAMETER p-sec AS INTEGER.
DEFINE INPUT PARAMETER p-tipo AS INTEGER.
DEFINE OUTPUT PARAMETER p-nro AS INTEGER.

DEFINE BUFFER b_control FOR CONTROL_tareas.

find last b_control use-index suc_sector_tipo_nro where  
          b_control.id_sucursal = p-suc and 
          b_control.id_sector = p-sec AND
          b_control.id_tipo = p-tipo no-lock no-error.
if available b_control Then
          p-nro = b_control.nro_planilla + 1.
       Else  p-nro = 1.      

END PROCEDURE.



PROCEDURE devuelve-total-horas.
DEF INPUT PARAMETER p_tipo AS INTEGER.
DEF INPUT PARAMETER p_nro AS INTEGER.
DEF INPUT PARAMETER p_empresa AS INTEGER.
DEF INPUT PARAMETER p_legajo AS INTEGER.
DEFINE OUTPUT PARAMETER p_total AS INTEGER.

DEFINE BUFFER b_control FOR CONTROL_tareas.
DEFINE BUFFER b_items FOR items_CONTROL_tareas.
DEFINE BUFFER b_uniliq FOR unidades_liquidacion.

DEF VAR v_otros AS INTEGER.

IF v_fecha = v_fecha_desde THEN p_total = 0.

FOR EACH b_items WHERE b_items.fecha >= v_fecha_desde AND
    b_items.fecha <= v_fecha AND b_items.id_empresa = p_empresa AND b_items.legajo = p_legajo NO-LOCK, 
    FIRST b_control OF b_items NO-LOCK:

    IF b_control.id_tipo_planilla = p_tipo AND b_control.nro_planilla = p_nro THEN  NEXT.

    v_otros = 0.
    FIND FIRST b_uniliq OF b_items NO-LOCK NO-ERROR.
    IF AVAILABLE b_uniliq THEN
    DO:
        IF b_uniliq.es_hora THEN
           v_otros = b_items.cantidad.
    END.


    p_total = p_total + (b_items.cant_jornal_norm * 8 +  b_items.cant_hs_norm + b_items.cant_hs_comp) + v_otros.


END.

END PROCEDURE.
