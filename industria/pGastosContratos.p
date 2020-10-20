DEFINE TEMP-TABLE ttGastosContratos
  FIELD cliente AS CHARACTER
  FIELD contrato AS CHARACTER
  FIELD parte   AS INTEGER
  FIELD oe AS INTEGER
  FIELD fecha AS DATE
  FIELD condicion AS CHARACTER
  FIELD destino AS CHARACTER
  
  FIELD seguro_flete AS DECIMAL
  FIELD delivery_ddp AS DECIMAL
  FIELD dutty_ddp AS DECIMAL
  FIELD ddu AS DECIMAL
  FIELD flete_internacional AS DECIMAL
  FIELD delivery_destino AS DECIMAL
  FIELD reproceso AS DECIMAL
  FIELD duties AS DECIMAL 
  FIELD thc_origen AS DECIMAL
  FIELD comision_agente AS DECIMAL
  FIELD seguro AS DECIMAL
  FIELD varios AS DECIMAL
  FIELD iks AS DECIMAL
  FIELD toll AS DECIMAL
  FIELD gate_out AS DECIMAL
  FIELD chasis_fee AS DECIMAL
  FIELD bunker AS DECIMAL
  FIELD gastos_destino AS DECIMAL
  FIELD gastos_bl AS DECIMAL
  FIELD thc_destino AS DECIMAL
  FIELD dumping AS DECIMAL
  FIELD fca_costs AS DECIMAL
  FIELD financing_storage AS DECIMAL
  FIELD palletizing AS DECIMAL
  FIELD freight AS DECIMAL
  FIELD inbalance AS DECIMAL
  FIELD dcr AS DECIMAL
  FIELD warehouse AS DECIMAL
  FIELD agp AS DECIMAL
  FIELD t7 AS DECIMAL
  FIELD ebaf AS DECIMAL
  FIELD advanced_manifest_cargo AS DECIMAL
  FIELD customs_broker_fee AS DECIMAL
  FIELD duties_usa AS DECIMAL
  FIELD service_fee AS DECIMAL
  FIELD percepcion_ing_bto AS DECIMAL
  FIELD security_charge AS DECIMAL
  FIELD destination_security AS DECIMAL
  FIELD panama_toll AS DECIMAL
  FIELD bl_destino AS DECIMAL
  FIELD wharfarge AS DECIMAL
  
  FIELD kilos AS DECIMAL.


CURRENT-WINDOW:WIDTH = 150.

FOR EACH ttGastosContratos.
  DELETE ttGastosContratos.
END.

FOR EACH contratos 
    WHERE contratos.fecha >= DATE('01/01/2003')
      AND contratos.id_tipo_contrato < 99
    NO-LOCK, 
    FIRST clientes
       OF contratos
    NO-LOCK, 
    EACH items_contrato 
      OF contratos
    NO-LOCK, 
    FIRST items_orden_entrega
      OF items_contratos
    NO-LOCK, 
    FIRST orden_entrega
       OF items_orden_entrega
    NO-LOCK,
    FIRST destinos 
       OF orden_entrega 
    WHERE destinos.id_destino_grupo = 25 /*USA*/
    NO-LOCK, 
    FIRST clausula
    WHERE clausula.id_clausula = items_orden_entrega.id_condicion_venta
    NO-LOCK.

  

  CREATE ttGastosContratos.
  ASSIGN ttGastosContratos.cliente = clientes.razon_social
         ttGastosContratos.contrato = contratos.id_contrato
         ttGastosContratos.parte   = items_contratos.ITEM
         ttGastosContratos.oe = orden_entrega.id_orden_entrega
         ttGastosContratos.fecha = orden_entrega.fecha
         ttGastosContratos.condicion = clausulas.descripcion
         ttGastosContratos.destino = destinos.descripcion
         ttGastosContratos.kilos = items_orden_entrega.kgs_netos_tambores.


  FOR EACH gastos_orden_entrega
      OF orden_entrega
    NO-LOCK, 
    FIRST gastos_venta
       OF gastos_orden_entrega
    NO-LOCK.

    CASE gastos_orden_entrega.id_gasto:
      WHEN 1 THEN ttGastosContratos.seguro_flete  = gastos_orden_entrega.importe.
      WHEN 2 THEN ttGastosContratos.delivery_ddp  = gastos_orden_entrega.importe.
      WHEN 3 THEN ttGastosContratos.dutty_ddp  = gastos_orden_entrega.importe.
      WHEN 4 THEN ttGastosContratos.ddu  = gastos_orden_entrega.importe.
      WHEN 5 THEN ttGastosContratos.flete_internacional  = gastos_orden_entrega.importe.
      WHEN 6 THEN ttGastosContratos.delivery_destino  = gastos_orden_entrega.importe.
      WHEN 7 THEN ttGastosContratos.reproceso  = gastos_orden_entrega.importe.
      WHEN 8 THEN ttGastosContratos.duties  = gastos_orden_entrega.importe.
      WHEN 9 THEN ttGastosContratos.thc_origen  = gastos_orden_entrega.importe.
      WHEN 10 THEN ttGastosContratos.comision_agente  = gastos_orden_entrega.importe.
  
      WHEN 11 THEN ttGastosContratos.seguro  = gastos_orden_entrega.importe.
      WHEN 12 THEN ttGastosContratos.varios  = gastos_orden_entrega.importe.
      WHEN 13 THEN ttGastosContratos.iks  = gastos_orden_entrega.importe.
      WHEN 14 THEN ttGastosContratos.toll  = gastos_orden_entrega.importe.
      WHEN 15 THEN ttGastosContratos.gate_out  = gastos_orden_entrega.importe.
      WHEN 16 THEN ttGastosContratos.chasis_fee  = gastos_orden_entrega.importe.
      WHEN 17 THEN ttGastosContratos.bunker  = gastos_orden_entrega.importe.
      WHEN 18 THEN ttGastosContratos.gastos_destino  = gastos_orden_entrega.importe.
      WHEN 19 THEN ttGastosContratos.gastos_bl  = gastos_orden_entrega.importe.
      WHEN 20 THEN ttGastosContratos.thc_destino  = gastos_orden_entrega.importe.
  
      WHEN 21 THEN ttGastosContratos.dumping  = gastos_orden_entrega.importe.
      WHEN 22 THEN ttGastosContratos.fca_costs  = gastos_orden_entrega.importe.
      WHEN 23 THEN ttGastosContratos.financing_storage  = gastos_orden_entrega.importe.
      WHEN 24 THEN ttGastosContratos.palletizing  = gastos_orden_entrega.importe.
      WHEN 25 THEN ttGastosContratos.freight  = gastos_orden_entrega.importe.
      WHEN 26 THEN ttGastosContratos.inbalance  = gastos_orden_entrega.importe.
      WHEN 27 THEN ttGastosContratos.dcr  = gastos_orden_entrega.importe.
      WHEN 28 THEN ttGastosContratos.warehouse  = gastos_orden_entrega.importe.
      WHEN 29 THEN ttGastosContratos.agp  = gastos_orden_entrega.importe.
      WHEN 30 THEN ttGastosContratos.t7  = gastos_orden_entrega.importe.
  
      WHEN 31 THEN ttGastosContratos.ebaf  = gastos_orden_entrega.importe.
      WHEN 32 THEN ttGastosContratos.advanced_manifest_cargo  = gastos_orden_entrega.importe.
      WHEN 33 THEN ttGastosContratos.customs_broker_fee  = gastos_orden_entrega.importe.
      WHEN 34 THEN ttGastosContratos.duties_usa  = gastos_orden_entrega.importe.
      WHEN 35 THEN ttGastosContratos.service_fee  = gastos_orden_entrega.importe.
      WHEN 36 THEN ttGastosContratos.percepcion_ing_bto  = gastos_orden_entrega.importe.
      WHEN 37 THEN ttGastosContratos.security_charge  = gastos_orden_entrega.importe.
      WHEN 38 THEN ttGastosContratos.destination_security  = gastos_orden_entrega.importe.
      WHEN 39 THEN ttGastosContratos.panama_toll  = gastos_orden_entrega.importe.
      WHEN 40 THEN ttGastosContratos.bl_destino  = gastos_orden_entrega.importe.
  
      WHEN 41 THEN ttGastosContratos.wharfarge  = gastos_orden_entrega.importe.
    
  
    END CASE.

  END.


    
END.


RUN generateExcel.p (INPUT TABLE ttGastosContratos,
                       INPUT " Gastos Contratos ",
                       INPUT " ",
                       INPUT 7,
                       INPUT 8,
                       INPUT "Arial",
                       INPUT 8).
