DEFINE VARIABLE cNombreRep AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAttach    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBody      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSubject   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cUsuarios  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMailCC    AS CHARACTER  NO-UNDO.

DEFINE VARIABLE lista_reportes AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cResult        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v_o_f          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMes           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cCalidad       AS CHARACTER  NO-UNDO.

lista_reportes = "Protocolo Jugo,Protocolo Jugo sin Lote Asoc.,Protocolo Aceite,Protocolo Aceite Muestra".

IF lista_reportes = "" THEN
  MESSAGE "No hay Reportes Disponibles" VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:
  RUN custom/support/cfun.w(input lista_reportes,output cresult).
  /*generar archivo pdf segun la opcion elegida*/
  CASE cResult:
    
    /*protocolo de jugo con lote asociado*/
    WHEN "Protocolo Jugo" THEN DO:
      FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_jugo THEN DO:
        FOR EACH tambores_industria OF lotes_jugo
                                    WHERE tambores_industria.id_tambor = protocolos.desde_tambor
                                    NO-LOCK.
          FIND FIRST contratos WHERE contratos.id_contrato      = tambores_industria.id_contrato_of 
                                 AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                                 AND contratos.anio             = tambores_industria.anio_of
                               NO-LOCK NO-ERROR.
          IF AVAILABLE contratos THEN DO:
              v_o_f = STRING(contratos.orden_fabricacion) + "/" + SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
          END.
        END.
      END.
      cNombreRep = "protocolo_jugo".
    END.
    
    /*protocolo de jugo sin lotea asociado*/
    WHEN "Protocolo Jugo sin Lote Asoc." THEN DO:
      FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_jugo THEN DO:
        FOR EACH tambores_industria OF lotes_jugo
                                    WHERE tambores_industria.id_tambor = protocolos.desde_tambor
                                    NO-LOCK.
          FIND FIRST contratos WHERE contratos.id_contrato      = tambores_industria.id_contrato_of 
                                 AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                                 AND contratos.anio             = tambores_industria.anio_of
                               NO-LOCK NO-ERROR.
          IF AVAILABLE contratos THEN DO:
              v_o_f = STRING(contratos.orden_fabricacion) + "/" + SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
          END.
        END.
      END.
      cNombreRep = "protocolo_jugo_sin_lote_asoc".
    END.

    /*protocolo aceite*/
    WHEN "Protocolo Aceite" THEN DO:
      FIND FIRST lotes_aceite OF protocolos NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_aceite THEN DO:
        FOR EACH tambores_industria OF lotes_aceite WHERE tambores_industria.id_tambor = protocolos.desde_tambor
                                                    NO-LOCK.
          FIND FIRST contratos WHERE contratos.id_contrato = tambores_industria.id_contrato_of 
                                 AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                                 AND contratos.anio = tambores_industria.anio_of
                               NO-LOCK NO-ERROR.
          IF AVAILABLE contratos THEN DO:
            v_o_f = STRING(contratos.orden_fabricacion) + "/" + SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
          END.
        END.
      END.
      cNombreRep = "protocolo_aceite".
    END.

    /*protocolo de muestra de aceite*/
    WHEN "Protocolo Aceite Muestra" THEN DO:
      FIND FIRST lotes_aceite OF protocolos NO-LOCK NO-ERROR.
      IF AVAILABLE lotes_aceite THEN DO:
        FOR EACH tambores_industria OF lotes_aceite WHERE tambores_industria.id_tambor = protocolos.desde_tambor
                                                    NO-LOCK.
          FIND FIRST contratos WHERE contratos.id_contrato = tambores_industria.id_contrato_of 
                                 AND contratos.id_tipo_contrato = tambores_industria.id_tipocontrato_of
                                 AND contratos.anio = tambores_industria.anio_of
                               NO-LOCK NO-ERROR.
          IF AVAILABLE contratos THEN DO:
            v_o_f = STRING(contratos.orden_fabricacion) + "/" + SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
          END.
        END.
      END.
      cNombreRep = "protocolo_aceite_muestras".
    END.



  END CASE.
  /*armo el body*/
  FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
  FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.
  FIND FIRST envases_muestras OF items_muestras NO-LOCK NO-ERROR.
  FIND FIRST muestras OF items_muestras NO-LOCK NO-ERROR.
  FIND FIRST contactos_muestras WHERE contactos_muestras.id_contacto = muestras.id_destinatario NO-LOCK NO-ERROR.
  FIND FIRST couriers WHERE items_muestras.id_courier_tuc = couriers.id_courier NO-LOCK NO-ERROR.
  FIND FIRST tambores_industria WHERE tambores_industria.id_empresa    = protocolos.id_empresa
                                  AND tambores_industria.id_sucursal   = protocolos.id_sucursal
                                  AND tambores_industria.id_tipotambor = protocolos.id_tipotambor
                                  AND tambores_industria.nromov        = protocolos.nromov
                                NO-LOCK NO-ERROR.
  FIND FIRST calidades OF tambores_industria NO-LOCK NO-ERROR.
  cCalidad = IF AVAILABLE calidades THEN calidades.descripcion ELSE "".

  RUN mes_largo.p (MONTH(general.items_muestras.fecha_enviado_tuc),
                   "ing",
                   OUTPUT cMes).



  cSubject = "Protocol " + STRING(protocolos.id_protocolo,">>999") + "-" + 
                           STRING(protocolos.anio,"99").
  
  cBody = "Dear Madam/Sir" + CHR(10) + 
          " On " + cMes + " " + STRING(DAY(items_muestra.fecha_enviado_tuc))  + " we sent the following " + CAPS(productos_terminados.descripcion_ingles) + " samples to your attention:" + CHR(10) + CHR(10) + 
          "Quality: " + cCalidad + CHR(10) +
          "Packaging: " + envases_muestras.descripcion_ingles + CHR(10) + 
          "Quantity: " + STRING(items_muestras.cantidad) + CHR(10) + 
          "Courier: " + couriers.descripcion + CHR(10) + 
          "AWB: " + items_muestras.nro_guia_tuc + CHR(10) + CHR(10) +
          "Please find attached the certificate of analysis" + CHR(10) + 
          "Best Regards".
         

  /*generar protocolo en pdf*/
  RUN ..\web\pImpProtocoloPDF.p (cNombreRep,
                                 "pdfIndustria",
                                 "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                 "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                 "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                 v_o_f + ";").    
  cAttach  = "..\industria\pdfs\" + cNombreRep + ".pdf".
   
  FIND FIRST usuarios_listas WHERE usuarios_listas.usuario_sistema = USERID("userdb")
                             NO-LOCK NO-ERROR.
  IF AVAILABLE usuarios_listas THEN DO: 
    cUsuarios = usuarios_listas.email.
  END.
  
  
  IF TRIM(cUsuarios) = "" THEN DO:
    cUsuarios = "facundoj@sa-sanmiguel.com".
    cSubject  = cSubject + " (NO SE ENCONTRO LA DIR DE MAIL DEL DESTINATARIO)".
    cMailCC   = "".
  END.

  RUN sendMail.p("",
                 1,
                 cSubject, 
                 cBody,
                 cUsuarios, 
                 cAttach).

  OS-DELETE VALUE(cAttach).
  

END.

