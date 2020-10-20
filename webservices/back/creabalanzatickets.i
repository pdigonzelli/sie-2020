
/*------------------------------------------------------------------------
    File        : creabalanzatickets.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Wed Jan 13 15:07:16 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
        CREATE balanza_tickets.
        ASSIGN 
            balanza_tickets.union_europea           = vunioneuropea
            balanza_tickets.renspa                  = vrenspa
            balanza_tickets.orden_compra_sap        = POSICIONES.PSNROPESADA
            balanza_tickets.pos_orden_compra_sap    = POSICIONES.PSNROPOSICIONPESADA
            balanza_tickets.peso_neto_ticket        = vpesonetoticket
            balanza_tickets.peso_envases_entrada    = vpesoenvasesentrada
            balanza_tickets.peso_descarte           = vpesodescarte
            balanza_tickets.periodo_cosecha         = periodo_cosecha.periodo_cosecha
            balanza_tickets.nro_ticket              = I
            balanza_tickets.nro_remito              = vnroremito
            balanza_tickets.nro_partida_serial      = 1
            balanza_tickets.nro_partida             = NEXT-VALUE(nro_partida, produccion)
            balanza_tickets.nro_partida_general     = balanza_tickets.nro_partida
            balanza_tickets.id_variedad             = vvariedad
            balanza_tickets.id_variedad_sap         = vvariedadsap
            balanza_tickets.id_tipo_servicio        = vtiposervicio
            balanza_tickets.id_tipo_cosecha         = vtipocosecha
            balanza_tickets.id_sucursal_packing     = vsucursalpacking
            balanza_tickets.id_sucursal_etiqueta    = vsucursaletiqueta
            balanza_tickets.id_sucursal             = vsucursal
            balanza_tickets.id_proveedor_origen     = vproveedororigen
            balanza_tickets.id_proveedor            = vproveedor
            balanza_tickets.id_pesada               = vpesada
            balanza_tickets.id_origen_origen        = vorigenorigen
            balanza_tickets.id_origen               = vorigen
            balanza_tickets.id_materia_prima        = iarticulo
            balanza_tickets.id_lote_senasa          = IF vunioneuropea  THEN vlotesenasa ELSE ''
            balanza_tickets.id_lote                 = vlote
            balanza_tickets.id_finca_senasa         = IF vunioneuropea THEN  vfincasenasa ELSE ''
            balanza_tickets.zona_up                 = vzona_up
            balanza_tickets.id_etiqueta             = vetiqueta
            balanza_tickets.id_envase               = venvase
            balanza_tickets.id_detalle_transporte   = vdetalletransporte
            balanza_tickets.id_destino_packing      = vdestinopacking
            balanza_tickets.id_descarte             = vdescarte
            balanza_tickets.id_color                = vcolor
            balanza_tickets.id_calidad_balanza      = vcalidadbalanza
            balanza_tickets.id_balanza              = vbalanza
            balanza_tickets.hora_salida             = balanza_pesadas.hora_entrada
            balanza_tickets.hora_entrada            = balanza_pesadas.hora_entrada
            balanza_tickets.finca                   = vfinca
            balanza_tickets.fecha_salida            = vfechaentrada
            balanza_tickets.fecha_remito            = vfecharemito
            balanza_tickets.fecha_operativa         = vfechaoperativa
            balanza_tickets.fecha_entrada           = vfechaentrada
            balanza_tickets.fecha_cosecha           = vfechacosecha
            balanza_tickets.cod_barra_sap           = TRIM(cmateriaprima) + PSNROLOTE
            balanza_tickets.codigo_trazabilidad     = ccodigotrazabilidad
            balanza_tickets.china                   = vchina
            balanza_tickets.cert_china              = vcertchina
            balanza_tickets.certificado             = vcertificado
            balanza_tickets.cant_env_entrada        = vcantenvasesentrada
            balanza_tickets.loteagricolaSap         = PSLOTEAGRICOLA
            balanza_tickets.especieSap              = productos_terminados.id_articulo_sap
            balanza_tickets.variedadSap             = PSVARIEDAD 
            balanza_tickets.fincasap                = PSFINCA
            balanza_tickets.usa                     = vusa.
