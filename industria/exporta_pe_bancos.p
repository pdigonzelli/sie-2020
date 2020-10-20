 DEFINE INPUT  PARAMETER pdesde AS DATE       NO-UNDO.
 DEFINE INPUT  PARAMETER phasta AS DATE       NO-UNDO.
 
 define var lst-titulos as char.
 define var lst-width   as char.
 define var lst-valores as char.
 
 DEFINE VARIABLE vgastos AS DECIMAL    NO-UNDO.
 define var i as integer.  
 DEFINE VAR vfact AS CHAR.
 DEFINE VAR vanulada AS CHAR.
 define var ving as decimal.
 DEFINE VAR vok AS LOGICAL INITIAL FALSE.
 DEFINE VARIABLE vestado AS LOGICAL    NO-UNDO.
 DEFINE VARIABLE vtipo AS CHARACTER  NO-UNDO.

 DEFINE VARIABLE vsigno AS INTEGER    NO-UNDO.
 DEFINE BUFFER b_bancos1 FOR subd_bancos.
 DEFINE BUFFER b_bancos2 FOR subd_bancos.
 DEFINE VARIABLE flag AS LOGICAL    NO-UNDO.
 /********* Variables de Excel **************/

 define var chExcelAplication as com-handle.
 define var chWorkbook        as com-handle.
 define var chWorkSheet       as com-handle.
 define var chchart           as com-handle.
 define var chWorkSheetRange  as com-handle.

 define var ifila  as integer.
 define var cfila  as character.
 define var crange as character.


 /********** Arma Planilla ***********/

 create "Excel.Application" chExcelAplication.
 chExcelAplication:visible = true.
 chWorkbook  = chExcelAplication:Workbooks:add().
 chWorkSheet = chExcelAplication:Sheets:Item(1).

 chWorkSheet:Range("A1:AM12"):Font:Bold = true.
 chWorkSheet:Range("A1:AM800"):Font:size = 8.
 chWorkSheet:Range("B2"):Font:size = 12.

 chWorkSheet:Range("B2:K2"):BorderAround(1,2,1,1).
 chWorkSheet:Range("B2:K2"):interior:colorindex = 11.
 chWorkSheet:Range("B2:K2"):font:colorindex = 2.
 chWorkSheet:Range("B2"):Value = " LISTADO DE PERMISOS DE EMBARQUES". 

 chWorkSheet:Range("B4"):Value = " REFERENCIAS:". 
 chWorkSheet:Range("C5"):Value = "TipoComprobante / PuntoVenta - Nro (Tipo) (Estado)".
 chWorkSheet:Range("C7"):Value = "TipoComprobante :".
 chWorkSheet:Range("C8"):Value = "PuntoVenta      :".
 chWorkSheet:Range("C9"):Value = "(Tipo)          :".
 chWorkSheet:Range("C10"):Value = "(Estado)        :".
 
 chWorkSheet:Range("D7"):Value = " 24 Factura - 26 Nota de Credito".
 chWorkSheet:Range("D8"):Value = " 19 San Miguel ".
 chWorkSheet:Range("D9"):Value = " P Proforma - D Definitiva - N Normal - PD Proforma con Definitiva".
 chWorkSheet:Range("D10"):Value = " A Anulada - V Vigente".

 lst-titulos = "Anio,PE,Aduana,OE,Fecha Of.,Importe,Gastos,Ingresado,Saldo,Fecha Vto.,Facturas,Banco,Cuenta,Nro.Cuenta,Fecha,Importe MO,T/C,Importe".
 lst-width   = "8,15,20,10,8,10,10,10,10,8,40,15,20,12,8,10,6,10".
 
 do i = 1 to num-entries(lst-titulos):
   chWorkSheet:Range(chr(97 + i) + "12"):Value = entry(i,lst-titulos,",").
   chWorkSheet:Range(chr(97 + i) + "12"):BorderAround(1,2,1,1).
   chWorkSheet:Columns(chr(97 + i)):ColumnWidth = integer(entry(i,lst-width,",")).
 end.
 
  
 chWorkSheet:Range("A7:R7"):horizontalalignment = 3.
 chWorkSheet:Columns("F"):NumberFormat = "dd/mm/yy".
 chWorkSheet:Columns("K"):NumberFormat = "dd/mm/yy".
 chWorkSheet:Columns("P"):NumberFormat = "dd/mm/yy".


/*
 if p-simbolo = 1 then
   do:
        chWorkSheet:Columns("F"):NumberFormat = "###.###.##0,00".
        chWorkSheet:Columns("H"):NumberFormat = "###.###.##0,00".
        chWorkSheet:Columns("J"):NumberFormat = "###.###.##0,00".
        chWorkSheet:Columns("K"):NumberFormat = "###.###.##0".
        chWorkSheet:Columns("L"):NumberFormat = "$* ###.###.##0,00".
        chWorkSheet:Columns("M"):NumberFormat = "$* ###.###.##0,00".
        chWorkSheet:Columns("N"):NumberFormat = "$* ###.###.###.##0,00".
        chWorkSheet:Columns("O"):NumberFormat = "$* ###.###.###.##0,00".
        chWorkSheet:Columns("P"):NumberFormat = "$* ###.###.###.##0,00".
        chWorkSheet:Columns("Q"):NumberFormat = "$* ###.###.###.##0,00".
        chWorkSheet:Columns("R"):NumberFormat = "$* ###.###.###.##0,00".

   end.
 else
   do:
        chWorkSheet:Columns("F"):NumberFormat = "###,###,##0.00".
        chWorkSheet:Columns("H"):NumberFormat = "###,###,##0.00".
        chWorkSheet:Columns("J"):NumberFormat = "###,###,##0.00".
        chWorkSheet:Columns("K"):NumberFormat = "###.###,##0".
        chWorkSheet:Columns("L"):NumberFormat = "###,###,##0.00".
        chWorkSheet:Columns("M"):NumberFormat = "###,###,##0.00".
        chWorkSheet:Columns("N"):NumberFormat = "###,###,###,##0.00".
        chWorkSheet:Columns("O"):NumberFormat = "###,###,###,##0.00".
        chWorkSheet:Columns("P"):NumberFormat = "###,###,###,##0.00".
        chWorkSheet:Columns("Q"):NumberFormat = "###,###,###,##0.00".
        chWorkSheet:Columns("R"):NumberFormat = "###,###,###,##0.00".

   end.
  */
   
 chExcelAplication:Selection:Style:IncludeNumber = true.

 ifila = 14.
 for each permisos_embarque 
     where length(permisos_embarque.id_permiso_embarque) = 11 AND 
           permisos_embarque.fecha_oficializacion >= pdesde AND
           permisos_embarque.fecha_oficializacion <= phasta 
           ,first aduanas where aduanas.id_aduana = permisos_embarque.id_aduana NO-LOCK BY permisos_embarque.fecha_oficializacion .

            find orden_entrega where orden_entrega.id_orden_entrega = permisos_embarque.id_orden_entrega no-lock no-error.
            
             
          /*>> CALCULO DEL MONTO INGRESADO <<*/
            ving = 0.
            for each r_ingresos_pe where  r_ingresos_pe.anio = permisos_embarque.anio and
                                          r_ingresos_pe.id_aduana = permisos_embarque.id_aduana and
                                          r_ingresos_pe.id_permiso_embarque = permisos_embarque.id_permiso_embarque no-lock:
                ving = ving + r_ingresos_pe.importe.
            END.                               
          /*>> FIN CALCULO DEL MONTO INGRESADO <<*/
          
          /* FACTURAS VINCULADAS CON EL PERMISO DE EMBARQUE*/
           vfact = "".
           vok = FALSE.
           for each r_subd_ventas_embarque 
                        where r_subd_ventas_embarque.anio_permiso = permisos_embarque.anio and
                              r_subd_ventas_embarque.id_aduana    = permisos_embarque.id_aduana and
                              r_subd_ventas_embarque.nro_embarque = permisos_embarque.id_permiso_embarque NO-LOCK:                        

               FIND subd_vtas WHERE subd_vtas.id_punto_venta = r_subd_ventas_embarque.id_punto_venta AND
                                    subd_vtas.nromov         = r_subd_ventas_embarque.nromov NO-LOCK NO-ERROR.
               IF AVAILABLE subd_vtas THEN DO:

                   vanulada = "".
                   RUN getEstadofactura.p (INPUT ROWID(subd_vtas),
                                           OUTPUT vestado).

                   IF NOT vestado THEN
                       vanulada = "(A)".
                   ELSE 
                       vanulada = "(V)".
                   
                   RUN getTipofactura.p (INPUT ROWID(subd_vtas),
                                         OUTPUT vtipo ).

                   IF subd_vtas.id_punto_venta <> 9999 THEN 
                      vfact = vfact + STRING(subd_vtas.id_tipocomp) + "/" + string(subd_vtas.id_punto_venta) + "-" + (IF subd_vtas.nro_proforma <> 0 THEN string(subd_vtas.nro_proforma,"99999999") + "(" + vtipo + ")" + vanulada ELSE string(subd_vtas.nro_comp,"99999999") + "(" + vtipo + ")" + vanulada) + " ~ ".
               END.
           END.
           /* FIN FACTURAS VINCULADAS CON EL PERMISO DE EMBARQUE*/


           /* GASTOS VINCULADOS CON EL PERMISO DE EMBARQUE*/
              vgastos = 0.
              FOR EACH r_gastos_permiso_embarque 
                             WHERE r_gastos_permiso_embarque.anio = permisos_embarque.anio AND
                                   r_gastos_permiso_embarque.id_aduana = permisos_embarque.id_aduana AND
                                   r_gastos_permiso_embarque.id_permiso_embarque = permisos_embarque.id_permiso_embarque NO-LOCK:
                   vgastos = vgastos + r_gastos_permiso_embarque.importe.
              END.
           /* FIN GASTOS VINCULADOS CON EL PERMISO DE EMBARQUE*/
            
           IF vfact <> ""  THEN DO:
           
            lst-valores = string(permisos_embarque.anio) + "," + 
                          permisos_embarque.id_permiso_embarque + "," + 
                          aduanas.descripcion + "," + 
                          string(permisos_embarque.id_orden_entrega) + "," + 
                          (if permisos_embarque.fecha_oficializacion = ? then "SinInfo" else string(permisos_embarque.fecha_oficializacion,"99/99/99")) + "," + 
                          string(permisos_embarque.importe) + "," +
                          STRING(round(vgastos,2)) + "," +
                          string(ving) + "," + 
                          string(permisos_embarque.importe - ving) + "," + 
                          (if (permisos_embarque.fecha_oficializacion + 120) = ? then "SinInfo" else string(permisos_embarque.fecha_oficializacion + 120,"99/99/99")) + "," + 
                          vfact.
                          
                          
            cfila  = string(ifila).
            cRange = "C" + cfila.
            if trim(permisos_embarque.observaciones) <> "" then
                  chWorkSheet:Range(crange):AddComment(permisos_embarque.observaciones).
            do i=1 to num-entries(lst-valores):
              cRange = chr(97 + i) + cfila.
              IF CHR(97 + i) = "f" OR CHR(97 + i) = "k" THEN
                  chWorkSheet:Range(crange):value = DATE(entry(i,lst-valores,",")).
              ELSE 
                  chWorkSheet:Range(crange):value = TRIM(entry(i,lst-valores,",")).
            end.                                            
            flag = FALSE.
            for each r_ingresos_pe where  r_ingresos_pe.anio      = permisos_embarque.anio and
                                          r_ingresos_pe.id_aduana = permisos_embarque.id_aduana and
                                          r_ingresos_pe.id_permiso_embarque = permisos_embarque.id_permiso_embarque no-lock:
                
                  FIND bancos WHERE bancos.id_banco =  r_ingresos_pe.id_banco NO-LOCK NO-ERROR.
                  FIND tcuenta WHERE tcuenta.id_tcuenta =  r_ingresos_pe.id_tcuenta NO-LOCK NO-ERROR.
                  FIND tipocomp WHERE tipocomp.id_tipocomp =  r_ingresos_pe.id_tipocomp NO-LOCK NO-ERROR.
                  
                  /*vsigno = IF tipocomp.signo THEN 1 ELSE -1.*/
                  
                  cfila = STRING(ifila).
                  cRange = "M" + cfila.
                  chWorkSheet:Range(crange):value = if available bancos then bancos.nombre else "Sin Info".
                  cRange = "N" + cfila.
                  chWorkSheet:Range(crange):value = IF AVAILABLE tcuenta THEN tcuenta.descripcion ELSE "Sin Info".
                  cRange = "O" + cfila.
                  chWorkSheet:Range(crange):value = r_ingresos_pe.nro_cuenta.
                  cRange = "P" + cfila.
                  chWorkSheet:Range(crange):value = r_ingresos_pe.fecha_comp.
                  cRange = "Q" + cfila.
                  chWorkSheet:Range(crange):value = ving /** vsigno*/ .

                  cRange = "R" + cfila.
                  chWorkSheet:Range(crange):value = r_ingresos_pe.tipo_cambio.
                  cRange = "S" + cfila.
                  chWorkSheet:Range(crange):value = ving * r_ingresos_pe.tipo_cambio.
                  cRange = "T" + cfila.
                  chWorkSheet:Range(crange):value =  r_ingresos_pe.nro_minuta.
                
                   ifila = ifila + 1.
                   flag = TRUE.
                  
            END.
            IF NOT flag THEN
               ifila = ifila + 1.

           END. 

 end. /*FOR PERMISOS EMBARQUE*/
  
 if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
 if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
 if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  

