lista_reportes = "Protocolo Jugo,Protocolo Jugo sin Lote Asoc.,Protocolo Aceite,Protocolo Aceite Muestra".

if lista_reportes = "" then
    message "No hay reportes disponibles" view-as alert-box.
else
    do:
        run custom/support/cfun.w(input lista_reportes,output cresult).
        
        case cresult:
            when "Protocolo Jugo" then 
                do:

                    FIND FIRST lotes_jugo OF protocolos NO-LOCK NO-ERROR.
                    IF AVAILABLE lotes_jugo THEN DO:
                        FOR EACH tambores_industria OF lotes_jugo
                                                    WHERE tambores_industria.id_tambor = 
                                                            protocolos.desde_tambor
                                                    NO-LOCK.
                            FIND FIRST contratos WHERE contratos.id_contrato = 
                                                       tambores_industria.id_contrato_of 
                                                   AND contratos.id_tipo_contrato = 
                                                       tambores_industria.id_tipocontrato_of
                                                   AND contratos.anio = 
                                                       tambores_industria.anio_of
                                                 NO-LOCK NO-ERROR.
                            IF AVAILABLE contratos THEN DO:
                                v_o_f = STRING(contratos.orden_fabricacion) + "/" + 
                                        SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
                            END.
                        END.
                        /*by facundo para el desglose por envases para lotes de pepsi 17/07/2006*/
                        DEFINE BUFFER buTam FOR tambores_industria.
                        DEFINE VARIABLE j     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE i     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE d     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE h     AS INTEGER    NO-UNDO.
                        DEFINE VARIABLE cDes  AS CHARACTER  NO-UNDO.

                        FOR EACH buTam 
                            OF lotes_jugo
                            BREAK BY buTam.id_envase
                            BY buTam.id_tambor.
                          j = j + 1.
                          i = buTam.id_tambor.
                          IF LAST-OF(buTam.id_envase) THEN DO:
                            FIND FIRST envases_prod OF buTam NO-LOCK NO-ERROR.
                            d = i - j + 1.
                            h = i.
                            cDes = cDes + "From " + envases_prod.abreviatura_ingles + " " + STRING(d) + " To " + STRING(h) + ", " + STRING(j) + " Units" + CHR(10).
                            i = 0.
                            j = 0.                            
                          END.
                        END.

                        ASSIGN protocolos.desglose_envases = cDes.

                    END.
                    run p_reportes_9.p (input "protocolo_jugo",
                                        input "Protocolos",
                                        input "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                        input v_o_f + ";").                    
                end.
            when "Protocolo Jugo sin Lote Asoc." then 
                do:
                    run p_reportes_9.p (input "protocolo_jugo_sin_lote_asoc",
                                        input "Protocolos",
                                        input "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                        input "").                    
                end.
            when "Protocolo Aceite" then 
                do:
                    FIND FIRST lotes_aceite OF protocolos NO-LOCK NO-ERROR.
                    IF AVAILABLE lotes_aceite THEN DO:
                        FOR EACH tambores_industria OF lotes_aceite
                                                    WHERE tambores_industria.id_tambor = 
                                                            protocolos.desde_tambor
                                                    NO-LOCK.
                            FIND FIRST contratos WHERE contratos.id_contrato = 
                                                       tambores_industria.id_contrato_of 
                                                   AND contratos.id_tipo_contrato = 
                                                       tambores_industria.id_tipocontrato_of
                                                   AND contratos.anio = 
                                                       tambores_industria.anio_of
                                                 NO-LOCK NO-ERROR.
                            IF AVAILABLE contratos THEN DO:
                                v_o_f = STRING(contratos.orden_fabricacion) + "/" + 
                                        SUBSTRING(STRING(YEAR(contratos.fecha)),3,2).
                            END.
                        END.
                    END.
                    run p_reportes_9.p (input "protocolo_aceite",
                                        input "Protocolos",
                                        input "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                        input v_o_f + ";").                    
                end.
            when "Protocolo Aceite Muestra" then 
                do: 
                    run p_reportes_9.p (input "protocolo_aceite_muestras",
                                        input "Protocolos",
                                        input "protocolos.id_protocolo = " + STRING(protocolos.id_protocolo) + " and " +
                                              "protocolos.anio = " + STRING(protocolos.anio) + " and " +
                                              "protocolos.id_articulo = " + STRING(protocolos.id_articulo) ,
                                        input "").                    
                end.
        end case.
    end.
