DEFINE VAR r as rowid.
define var v_subject as char.
define var v_body as char.
define var v_usuarios as char.
DEFINE VAR v_usuarios_com AS CHAR.
DEFINE VAR v_protocolos AS CHAR.
DEFINE VAR v_carac AS CHAR.
DEFINE VAR v_mes AS CHAR.

FIND FIRST contramarcas OF protocolos NO-LOCK NO-ERROR.
FIND FIRST productos_terminados OF protocolos NO-LOCK NO-ERROR.

v_subject = "Protocol " + STRING(protocolos.id_protocolo,">>999") + "/" + 
                          STRING(protocolos.anio,"99").
    
v_body = v_body +
             "GENERAL INFORMATION FOR THE CUSTOMER:" + CHR(10) + 
             "DATE: " + STRING(MONTH(tambores_industria.fecha)) + "/" +
                        STRING(DAY(tambores_industria.fecha)) + "/" +
                        STRING(YEAR(tambores_industria.fecha)) + CHR(10) +
             "COUNTERMARK: " + CAPS(contramarcas.descripcion) + CHR(10) +
             "LOT NUMBER: " + STRING(tambores_industria.id_lote,">999") + "/"
                            + STRING(tambores_industria.anio) + 
             CHR(10) + CHR(10) +
             "SAMPLE DESCRIPTION: " + CHR(10) +
             "IDENTIFICATION LABEL: " + CAPS(productos_terminados.descripcion_ingles) + CHR(10) +
             "BATCH DESCRIPTION: " + CHR(10) +
             "NUMBER OF DRUMS: " + STRING(industria.protocolos.cantidad_tambores) + "/" + STRING(industria.protocolos.cantidad_tambores) + CHR(10) +
             "NET WEIGHT PER DRUM: " + STRING(industria.protocolos.peso_neto) + "KG" + CHR(10) +
             "DRUMS PER BATCH: " + STRING(industria.protocolos.desde_tambor) + "/" + STRING(industria.protocolos.cantidad_tambores) + " TO " + 
                                   STRING(industria.protocolos.hasta_tambor) + "/" + STRING(industria.protocolos.cantidad_tambores) + CHR(10) +
             CHR(10).
 
    IF protocolos.con_galones THEN DO: 
        v_body = v_body + "GALONS PER DRUM: " + STRING(protocolos.galones_tambores,"99.99") +
                 CHR(10).
    END.
    
    v_body = v_body + CHR(10) +
             "PHYSICO-CHEMICAL PROPERTIES: " + CHR(10).

    FOR EACH items_protocolo OF protocolos NO-LOCK.
        FIND FIRST caracteristicas_quimicas WHERE caracteristicas_quimicas.id_caracteristica = items_protocolo.id_caracteristica NO-LOCK NO-ERROR.
        IF AVAILABLE caracteristicas_quimicas THEN DO:
            v_carac = v_carac + caracteristicas_quimicas.descripcion + "      " + items_protocolos.valor_caracter + CHR(10).
        END.
    END.
    
    v_body = v_body + v_carac.
   
    FIND FIRST usuarios_listas WHERE usuarios_listas.usuario_sistema = USERID("userdb")
                               NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios_listas THEN DO: 
        v_usuarios = usuarios_listas.email.
        
        RUN SendMail.p(INPUT "",                                      /* SIEMPRE TIENE QUE IR */
                       INPUT 2,                                       /* PRIORIDAD */
                       INPUT v_subject,                               /* SUBJECT */
                       INPUT v_body,                                  /* BODY     */
                       INPUT v_usuarios,               /* DEST. SEP COMAS */
                       INPUT ""                                       /* ARCHIVOS ATTACHED SEP POR COMAS */
                       ).
    END.
    ELSE MESSAGE "No tiene cargado el mail, por favor avise a sistemas." VIEW-AS ALERT-BOX.
