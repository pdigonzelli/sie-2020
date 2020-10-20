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

 chWorkSheet:Range("A1:AM7"):Font:Bold = true.
 chWorkSheet:Range("A1:AM800"):Font:size = 8.
 chWorkSheet:Range("B2"):Font:size = 12.

 chWorkSheet:Range("B2:K2"):BorderAround(1,2,1,1).
 chWorkSheet:Range("B2:K2"):interior:colorindex = 11.
 chWorkSheet:Range("B2:K2"):font:colorindex = 2.
 chWorkSheet:Range("B2"):Value = " LISTADO DE PERMISOS DE EMBARQUES". 

 
 lst-titulos = "Anio,PE,Aduana,OE,Fecha Of.,Importe,Gastos,Ingresado,Saldo,Fecha Vto.,Facturas".
 lst-width   = "8,15,20,10,8,10,10,10,10,8,40".
 
 do i = 1 to num-entries(lst-titulos):
   chWorkSheet:Range(chr(97 + i) + "7"):Value = entry(i,lst-titulos,",").
   chWorkSheet:Range(chr(97 + i) + "7"):BorderAround(1,2,1,1).
   chWorkSheet:Columns(chr(97 + i)):ColumnWidth = integer(entry(i,lst-width,",")).
 end.
 
  
 chWorkSheet:Range("A7:R7"):horizontalalignment = 3.
 chWorkSheet:Columns("F"):NumberFormat = "dd/mm/yy".
 chWorkSheet:Columns("K"):NumberFormat = "dd/mm/yy".


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

 ifila = 9.
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
            end.                               
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
                   FIND FIRST r_fact_ventas WHERE r_fact_ventas.id_punto_venta_ventas =  subd_vtas.id_punto_venta AND
                                                  r_fact_ventas.nromov_ventas         = subd_vtas.nromov NO-LOCK NO-ERROR.
                   IF AVAILABLE r_fact_ventas THEN
                       vanulada = "(A)".
                   ELSE DO:
                      IF NOT subd_vtas.estado THEN
                          vanulada = "(A)".
                   END.

                   IF subd_vtas.id_punto_venta <> 9999 THEN 
                      vfact = vfact + string(subd_vtas.id_punto_venta) + "-" + (IF subd_vtas.nro_proforma <> 0 THEN string(subd_vtas.nro_proforma,"99999999") + vanulada ELSE string(subd_vtas.nro_comp,"99999999") + vanulada) + " ~ ".
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
            ifila = ifila + 1.

           END. 

         
 end. /*FOR PERMISOS EMBARQUE*/
  
 if valid-handle(chExcelAplication) then RELEASE OBJECT chExcelAplication.
 if valid-handle(chWorkBook)        then RELEASE OBJECT chWorkBook.
 if valid-handle(chWorkSheet)       then RELEASE OBJECT chWorkSheet.  

