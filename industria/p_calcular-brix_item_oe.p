define input parameter p_id_orden_entrega like orden_entrega.id_orden_entrega.
DEFINE INPUT PARAMETER p_item_oe AS INTEGER.
define output parameter p_kilos as decimal.
define output parameter p_kilos_brutos as decimal.
define output parameter p_cantidad_tambores as decimal.
define output parameter p_gall as decimal.
define output parameter p_gall_brix as decimal.

define var p_peso as decimal.
define var v_kilos_temp as decimal initial 0.
define var v_gall_tambor_temp as decimal initial 0.
define var v_gall_brix_temp as decimal initial 0.
define var v_total_tambores_lote as integer initial 0.
define var i as integer initial 0.

DEFINE VARIABLE hLib AS HANDLE     NO-UNDO.
DEFINE VARIABLE dLit AS DECIMAL    NO-UNDO.

DEFINE VARIABLE hLibCom AS HANDLE.
RUN libCommonFunctions.p PERSISTENT SET hLibCom.
hLib = DYNAMIC-FUNCTION('getPersistentLib' IN hLibCom, 'libTamboresIndustria.p').
DELETE OBJECT hLibCom.


DEFINE BUFFER bb_item_oe1 FOR items_orden_entrega.

FIND FIRST bb_item_oe1 WHERE bb_item_oe1.id_orden_entrega = p_id_orden_entrega
                         AND bb_item_oe1.ITEM_oe          = p_item_oe
                               NO-LOCK NO-ERROR.
IF bb_item_oe1.id_articulo <> 54  THEN DO:
  for each tambores_industria where tambores_industria.id_orden_entrega = p_id_orden_entrega
                                AND tambores_industria.ITEM_oe          = p_item_oe
                                break by tambores_industria.nromov.               /*by facundo reemplaze break by id_lote por break by nromov*/
    v_kilos_temp = v_kilos_temp + tambores_industria.kilos_tambor.
    /*p_kilos_brutos = p_kilos_brutos + tambores_industria.kilos_tambor + tambores_industria.tara.*/
    p_peso = tambores_industria.kilos_tambor.
    p_cantidad_tambores = p_cantidad_tambores + 1.
    v_total_tambores_lote = v_total_tambores_lote + 1.

    /*by facundo 29/04/05*/
    FIND FIRST envases_prod OF tambores_industria NO-LOCK NO-ERROR.
    IF tambores_industria.tara = 0 THEN 
      p_kilos_brutos = p_kilos_brutos + tambores_industria.kilos_tambor + envases_prod.tara.
    ELSE
      p_kilos_brutos = p_kilos_brutos + tambores_industria.kilos_tambor + tambores_industria.tara.

    
    if LAST-OF(tambores_industria.nromov) THEN do: /*by facundo reemplaze nromov por id_lote*/
      
      p_kilos = p_kilos + v_kilos_temp.                            
      find lotes_jugo of tambores_industria no-lock no-error.
      if available lotes_jugo then do:
        find last inspecciones_lote of lotes_jugo no-lock no-error.
        if available inspecciones_lote THEN do:
          find last brix where brix.brix <= round(inspecciones_lote.bx_correg,1) no-lock no-error.
          if available brix then do:
            /*by facundo 06/06/2006 - considero la suma de los kilos de los tbs y el coef p. espeficico para el calculo de galones*/
            dLit = v_kilos_temp / DYNAMIC-FUNCTION('getCoefPesoEspecifico' IN hLib, inspecciones_lote.bx_correg).
            p_gall = dLit / 3.785.
            
            v_gall_tambor_temp   = round((p_peso / brix.pe) / 3.785 , 2).
            /*p_gall               = p_gall + (v_total_tambores_lote * v_gall_tambor_temp).*/            
            v_gall_brix_temp     = v_gall_brix_temp + inspecciones_lote.bx_correg.
            i = i + 1.                                       
          end.
        end.        
      end.
      v_kilos_temp = 0.
      v_total_tambores_lote = 0.
    end.                  
  end.
  
  if i > 0 and v_gall_tambor_temp > 0 then do:
    p_gall_brix     = v_gall_brix_temp / i.
  end.
END.

