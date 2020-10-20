
/*------------------------------------------------------------------------
    File        : pp159ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sat Feb 20 20:20:51 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER v_fecha_desde AS DATE NO-UNDO.
DEFINE INPUT PARAMETER v_fecha_hasta AS DATE NO-UNDO.
 

DEFINE TEMP-TABLE T_Entrada NO-UNDO
    NAMESPACE-URI "" 
    FIELD Finca AS CHARACTER 
    FIELD Cod_Tarea AS CHARACTER 
    FIELD Cantidad AS CHARACTER 
    FIELD Periodo AS CHARACTER 
    FIELD Proveedor AS CHARACTER .

DEFINE DATASET MT_Request NAMESPACE-URI "http://sanmiguel.com/PM001ING" 
    FOR T_Entrada.

DEFINE TEMP-TABLE T_Respuesta NO-UNDO
    NAMESPACE-URI "" 
    FIELD Orden AS CHARACTER 
    FIELD Resultado AS CHARACTER 
    FIELD Observaciones AS CHARACTER 
    FIELD Fecha AS CHARACTER 
    FIELD Finca AS CHARACTER 
    FIELD Cod_Tarea AS CHARACTER 
    FIELD Cantidad AS CHARACTER 
    FIELD Periodo AS CHARACTER 
    FIELD Proveedor AS CHARACTER 
    FIELD Num_Notif AS CHARACTER 
    FIELD Pos_Num_Notif AS CHARACTER 
    FIELD Hoja_Servicio AS CHARACTER .

DEFINE DATASET MT_Response NAMESPACE-URI "http://sanmiguel.com/PM001ING" 
    FOR T_Respuesta.


DEFINE TEMP-TABLE t-finca
   FIELD id_proveedor LIKE liq_tarjas.id_proveedor
   FIELD id_origen LIKE liq_tarjas.id_origen
   FIELD id_tarea LIKE liq_items_tarjas.id_tarea
   FIELD cantidad LIKE liq_items_tarjas.cantidad FORMAT ">>>>9.99"
   FIELD FECHA AS DATE
   FIELD id_empresa LIKE liq_tarjas.id_empresa.



DEFINE VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE VARIABLE hSI_OS_PM001ING AS HANDLE NO-UNDO.

DEFINE VAR EX AS Progress.Lang.AppError.
DEFINE VAR FERROR AS LOGICAL NO-UNDO.
DEFINE VAR CERROR AS CHARACTER NO-UNDO.


DEF VAR v_finca AS INTEGER.
DEF VAR v_cuenta AS INTEGER.

DEF VAR v_cuenta_jornal AS INTEGER.
DEF VAR v_cuenta_horas AS INTEGER.
DEF VAR v_cuenta_otros AS INTEGER.
DEF VAR v_jornal AS DECIMAL.

DEF VAR v_codigo AS INTEGER.
DEF VAR v_codigo-1 AS INTEGER.
DEF VAR v_codigo-2 AS INTEGER.

DEF VAR v_entero AS DECIMAL.
DEF VAR v_decimal AS DECIMAL.
DEFINE VAR v_cargo LIKE liq_legajos.id_cargo.
DEFINE VAR v_horas AS DECIMAL.
DEF VAR v_cantidad AS CHARACTER.
DEF VAR v_legajo LIKE liq_items_tarjas.legajo.
DEF VAR v_inicio AS INTEGER.
DEF VAR v_desde AS CHARACTER.
DEF VAR v_hasta AS CHARACTER.
DEF VAR v_finca_sap AS CHAR.
DEF VAR v_tarea_sap AS CHAR.
DEF VAR v_empresa_sap AS CHAR.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


RUN ARMA-TABLA(v_fecha_desde, v_fecha_hasta, OUTPUT table t-finca).

FOR EACH T-FINCA.
    V_EMPRESA_SAP = "".
    FIND FIRST liq_empresas WHERE liq_empresas.id_empresa = t-finca.id_empresa  NO-LOCK NO-ERROR.
    IF AVAILABLE liq_empresas THEN v_empresa_sap = liq_empresas.codigo_sap.
    v_finca_sap = "".
    FIND FIRST origenes OF t-finca NO-LOCK NO-ERROR.
    IF AVAILABLE origenes THEN v_finca_sap = origenes.codigo_sap.
    v_tarea_sap = "".
    FIND FIRST liq_tareas OF t-finca NO-LOCK NO-ERROR.
    IF AVAILABLE liq_tareas THEN v_tarea_sap = liq_tareas.codigo_sap.
    v_cantidad = TRIM(STRING(t-finca.cantidad, ">>>>9.99")).
    CREATE T_ENTRADA.
    ASSIGN  T_ENTRADA.FINCA         = V_FINCA_SAP
            T_ENTRADA.COD-TAREA     = V_TAREA_SAP
            T_ENTRADA.FINCA         = V_FINCA_SAP
            T_ENTRADA.CANTIDAD      = V_CANTIDAD
            T_ENTRADA.Periodo       = STRING(YEAR(T-FINCA.FECHA,'9999')) + STRING(MONTH(T-FINCA.FECHA),'99') + STRING(DAY(T-FINCA.FECHA), '99')
            T_ENTRADA.PROVEEDOR     = V_EMPRESA_SAP.
END.
        
CREATE SERVER hWebService.
hWebService:CONNECT("-WSDL 'http://smazapppiqas.sanmiguel.local:50200/dir/wsdl?p=sa/4482bdd506883b5cb96f559d63849cc2'
                     -WSDLUserid PIAPPLPID
                     -WSDLPassword SanMigu3l2015
                     -SOAPEndpointUserid PIAPPLPID 
                     -SOAPEndpointPassword SanMigu3l2015
                     -Service SI_OS_PM001INGService
                     -Port HTTP_Port").



RUN SI_OS_PM001ING SET hSI_OS_PM001ING ON hWebService.
RUN SI_OS_PM001ING IN hSI_OS_PM001ING(INPUT DATASET MT_Request, OUTPUT DATASET MT_Response).

FERROR = FALSE.
FOR FIRST T_RESPUESTA WHERE RESULTADO <> 'OK'.
    FERROR = TRUE.
END.

IF FERROR THEN RETURN ERROR NEW Progress.Lang.AppError(CSTATUS).

CSTATUS = 'OK'.

CATCH E AS Progress.Lang.Error:
    CSTATUS = E:GETMESSAGE(1).
    MESSAGE "ERROR RUTINA: " CSTATUS.
    UNDO, RETURN ERROR E.
END CATCH.


FINALLY:

END FINALLY.
    


/* **********************  Internal Procedures  *********************** */

PROCEDURE ARMA-TABLA:
    DEFINE INPUT PARAMETER P-FECHA-DESDE AS DATE.
    DEFINE INPUT PARAMETER P-FECHA-HASTA AS DATE.
    DEFINE OUPUT PARAMETER TABLE FOR T-FINCA.
    
    
    FOR EACH liq_tarjas NO-LOCK WHERE
          liq_tarjas.fecha >= P-FECHA-DESDE AND liq_tarjas.fecha <= P-FECHA-HASTA 
          ,EACH liq_items_tarjas OF liq_tarjas 
           WHERE liq_items_tarjas.id_tarea <> 0 NO-LOCK
           BY liq_items_tarjas.id_empresa:
    
         FIND FIRST t-finca WHERE t-finca.id_empresa = liq_tarjas.id_empresa AND
           t-finca.id_proveedor = liq_tarjas.id_proveedor AND
           t-finca.id_origen = liq_tarjas.id_origen AND
           t-finca.id_tarea = liq_items_tarjas.id_tarea AND
           T-FINCA.FECHA = LIQ_ITEMS_TARJAS.FECHA NO-LOCK NO-ERROR.
           IF NOT AVAILABLE t-finca THEN
              DO:
                 CREATE t-finca.
                 ASSIGN t-finca.id_empresa = liq_tarjas.id_empresa
                        t-finca.id_proveedor = liq_tarjas.id_proveedor
                        t-finca.id_origen = liq_tarjas.id_origen
                        t-finca.id_tarea = liq_items_tarjas.id_tarea
                        t-finca.fecha = liq_items_tarjas.fecha.
              END. 
    
              IF liq_items_tarjas.cant_hs_norm <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.cant_hs_norm.
              
    
              IF liq_items_tarjas.hs_acond_finca <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.hs_acond_finca.
    
              IF liq_items_tarjas.hs_plus_tareas_automatico <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.hs_plus_tareas_automatico.
    
              IF liq_items_tarjas.hs_plus_tareas_trabajadas <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.hs_plus_tareas_trabajadas.
              
              IF liq_items_tarjas.hs_adicionales_tareas_trabajadas <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.hs_adicionales_tareas_trabajadas.
              
              IF liq_items_tarjas.cantidad <> 0 THEN
                    t-finca.cantidad = t-finca.cantidad + liq_items_tarjas.cantidad.
    
    END.   

END PROCEDURE.
        

