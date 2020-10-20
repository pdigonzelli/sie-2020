
/*------------------------------------------------------------------------
    File        : mm050ing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sun Dec 20 19:25:32 ACT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* CABECERA 
TRANSPORTISTA VERIFICAR
TRANSPORTE VERIFICAR

  POSICION
  
NROREMITO  CONFIRMAR FORMATO 9999R99999999
MERCADO  VERIFICAR TABLA DE MERCADOS - QUE PASA CON CHINA
MATERIAL USAR LO CONVENIDO
ESPECIE  VERIFICAR TABLA CONTRA MATERIAL SAP - FALTAN VARIEDADES COMO PALTA TANGELO KUNKUAT 
VARIEDAD VERIFICAR TABLA CONTRA MATERIAL SAP
FINCA    VERIFICAR TABLA DE FINCAS
TIPOENVASE VERIFICAR TABLA
TIPOCORTE  VERIFICAR TABLA CONTRA MATERIAL SAP
COLOR VERIFICAR TABLA

FALTA EL TIPO DE SERVICIO DESDE SAP
*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

FUNCTION devuelvefechaoperativa RETURNS DATE
    ( INPUT pfecha AS DATE, INPUT phora AS CHARACTER)  FORWARD.

FUNCTION nroPesada RETURNS INTEGER
    ( INPUT IBALANZA AS INTEGER, INPUT IPESASA AS INTEGER)  FORWARD.

FUNCTION WLOG RETURNS LOGICAL
    ( INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER)  FORWARD.


DEFINE TEMP-TABLE POSICIONES
    FIELD PSNROREMITO            AS CHARACTER
    FIELD PSFECHAREMITO          AS CHARACTER
    FIELD PSNROLOTE              AS CHARACTER
    FIELD PSFECHACOSECHA         AS CHARACTER
    FIELD PSMERCADO              AS CHARACTER
    FIELD PSMATERIAL             AS CHARACTER
    FIELD PSLOTEAGRICOLA         AS CHARACTER
    FIELD PSNRODOCUMENTOMATERIAL AS CHARACTER
    FIELD PSESPECIE              AS CHARACTER
    FIELD PSVARIEDAD             AS CHARACTER
    FIELD PSFINCA                AS CHARACTER
    FIELD PSTRAZABILIDAD         AS CHARACTER
    FIELD PSTIPOENVASE           AS CHARACTER
    FIELD PSCANTENVASES          AS CHARACTER
    FIELD PSCANTPES              AS CHARACTER
    FIELD PSUNIDAD               AS CHARACTER
    FIELD PSTIPOCORTE            AS CHARACTER
    FIELD PSCOLOR                AS CHARACTER
    FIELD PSNROPESADA            AS CHARACTER
    FIELD PSNROPOSICIONPESADA    AS CHARACTER.

DEFINE TEMP-TABLE RESPUESTAS NO-UNDO
    FIELD POSTATUS  AS CHARACTER
    FIELD POMENSAJE AS CHARACTER.



DEFINE INPUT PARAMETER SSUCURSAL AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER STRANSPORTISTA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER STRANSPORTE AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SPESOENTRADA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SFECHAENTRADA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SHORAENTRADA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SPESOSALIDA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SFECHASALIDA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SHORASALIDA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SPESODESCARTE AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SPESONETO AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER SNROPESADA AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR POSICIONES.
DEFINE OUTPUT PARAMETER TABLE FOR RESPUESTAS.




DEFINE VARIABLE VIBALANZA           AS INTEGER   NO-UNDO.
DEFINE VARIABLE nm                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE hProd               AS HANDLE    NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vposicion           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v1                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v2                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vtipocosecha        AS INTEGER   NO-UNDO.
DEFINE VARIABLE vunioneuropea       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vchina              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vusa                AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vsucursaletiqueta   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vorigenorigen       AS INTEGER   NO-UNDO.
DEFINE VARIABLE vfechacosecha       AS DATE      NO-UNDO.
DEFINE VARIABLE vfincasenasa        AS INTEGER   NO-UNDO.
DEFINE VARIABLE vlotesenasa         AS INTEGER   NO-UNDO.
DEFINE VARIABLE vcertificado        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vpesonetoticket     AS DECIMAL   NO-UNDO DECIMALS 3.
DEFINE VARIABLE laceptado           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ctipocosecha        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcertunion          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcertchina          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ctiposervicio       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cenvase             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cvariedad           AS CHARACTER NO-UNDO.
DEFINE VARIABLE ccolor              AS CHARACTER NO-UNDO.
DEFINE VARIABLE clote               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cproveedor          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cmateriaprima       AS CHARACTER NO-UNDO.
DEFINE VARIABLE corigensap          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ccodigotrazabilidad AS CHARACTER NO-UNDO.
DEFINE VARIABLE vtiposervicio       AS INTEGER   NO-UNDO.
DEFINE VARIABLE venvase             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cmercado            AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcolor              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iarticulo           AS INTEGER   NO-UNDO.
DEFINE VARIABLE vpesadasap          AS INTEGER   NO-UNDO.
DEFINE VARIABLE vzona_up            AS CHARACTER NO-UNDO.
DEFINE VARIABLE vrenspa             AS CHARACTER NO-UNDO.
DEFINE VARIABLE vordencomprasap     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vposordencomprasap  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vpesoenvasesentrada AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vvariedad           AS INTEGER   NO-UNDO.
DEFINE VARIABLE vvariedadsap        AS INTEGER   NO-UNDO.
DEFINE VARIABLE vbalanza            AS INTEGER   NO-UNDO.
DEFINE VARIABLE vpesada             AS INTEGER   NO-UNDO.
DEFINE VARIABLE vsucursal           AS INTEGER   NO-UNDO.
DEFINE VARIABLE vsucplaya           AS INTEGER   NO-UNDO.
DEFINE VARIABLE vtipomov            AS INTEGER   NO-UNDO.
DEFINE VARIABLE vsucorigen          AS INTEGER   NO-UNDO.
DEFINE VARIABLE vproveedor          AS INTEGER   NO-UNDO.
DEFINE VARIABLE vpesodescarte       AS DECIMAL   NO-UNDO DECIMALS 3.
DEFINE VARIABLE vsucursalpacking    AS INTEGER   NO-UNDO.
DEFINE VARIABLE vproveedororigen    AS INTEGER   NO-UNDO.
DEFINE VARIABLE vorigen             AS INTEGER   NO-UNDO.
DEFINE VARIABLE vmateriaprima       AS INTEGER   NO-UNDO.
DEFINE VARIABLE vlote               AS INTEGER   NO-UNDO.
DEFINE VARIABLE vetiqueta           AS INTEGER   NO-UNDO.
DEFINE VARIABLE vdetalletransporte  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vdestinopacking     AS INTEGER   NO-UNDO.
DEFINE VARIABLE vdescarte           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE vcalidadbalanza     AS INTEGER   NO-UNDO.
DEFINE VARIABLE vhorasalida         AS CHARACTER NO-UNDO.
DEFINE VARIABLE vhoraentrada        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vfinca              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vfechasalida        AS DATE      NO-UNDO.
DEFINE VARIABLE vfecharemito        AS DATE      NO-UNDO.
DEFINE VARIABLE vfechaoperativa     AS DATE      NO-UNDO.
DEFINE VARIABLE vfechaentrada       AS DATE      NO-UNDO.
DEFINE VARIABLE vcodbarrasap        AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcantenvasesentrada AS DECIMAL   NO-UNDO.
DEFINE VARIABLE pcode               AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE porden              AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE pposic              AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE perror              AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE ptexto              AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE ptipoe              AS CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE cerror              AS CHARACTER NO-UNDO INITIAL 'Error de interface de ingreso de fruta'.
DEFINE VARIABLE VINDEX              AS INTEGER   NO-UNDO.
DEFINE VARIABLE VNROREMITO          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus             AS CHARACTER NO-UNDO.
DEFINE VARIABLE IPESADA             AS INTEGER   NO-UNDO.

DEFINE VARIABLE ORESPUESTA          AS CHARACTER NO-UNDO.
DEFINE VARIABLE OMENSAJERESPUESTA   AS CHARACTER NO-UNDO.

DEFINE BUFFER aux_items FOR items_stock.

DEFINE VARIABLE ARCHIVO AS CHARACTER NO-UNDO.
DEFINE VARIABLE VTEXTO  AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

nm = SESSION:NUMERIC-FORMAT.
ARCHIVO = SESSION:TEMP-DIRECTORY + SNROPESADA.
ORESPUESTA = 'OK'.
OMENSAJERESPUESTA = 'Ingreso correcto'.  



CASE SSUCURSAL:
    WHEN '1101' THEN 
        VBALANZA = 2.
    WHEN '1200' THEN 
        VBALANZA = 4.
    OTHERWISE
    DO:
        ORESPUESTA = 'NOK'.
        OMENSAJERESPUESTA = 'Balanza inexistente ' + SSUCURSAL.  
        RETURN.
    END.
END CASE.

VTEXTO = "CABECERA: " + SSUCURSAL + "|" + 
    STRANSPORTISTA + "|" + STRANSPORTE + "|" + 
    SPESOENTRADA + "|" + SFECHAENTRADA + "|" + 
    SHORAENTRADA + "|" + SPESOSALIDA + "|" + 
    SFECHASALIDA + "|" +  SHORASALIDA + "|" + SPESODESCARTE + "|" + SPESONETO + "|" + SNROPESADA.

WLOG(ARCHIVO,VTEXTO).

FOR EACH POSICIONES.
    VTEXTO = "POSICION: " + 
        PSNROREMITO + "|" +
        PSFECHAREMITO + "|" +
        PSNROLOTE + "|" +
        PSFECHACOSECHA + "|" +
        PSMERCADO + "|" +
        PSMATERIAL + "|" +
        PSLOTEAGRICOLA + "|" +
        PSNRODOCUMENTOMATERIAL + "|" +
        PSESPECIE + "|" +
        PSVARIEDAD + "|" +
        PSFINCA + "|" +
        PSTRAZABILIDAD + "|" +
        PSTIPOENVASE + "|" +
        PSCANTENVASES + "|" +
        PSCANTPES + "|" +
        PSUNIDAD + "|" +
        PSTIPOCORTE + "|" +
        PSCOLOR + "|" +
        PSNROPESADA + "|" +
        PSNROPOSICIONPESADA.
    
    WLOG(ARCHIVO,VTEXTO).

END.

FIND FIRST balanzas WHERE balanzas.id_balanza = VBALANZA NO-LOCK.
/*    IF NOT AVAILABLE balanzas  THEN
        UNDO , RETURN ERROR 'Balanza inexistente'. */
  
ipesada  =   INTEGER(SNROPESADA).
ipesada  = nroPesada (balanzas.id_balanza , ipesada).  

VTEXTO = "PESADA: " + STRING(IPESADA).
WLOG(ARCHIVO,VTEXTO)  .
FIND FIRST balanza_pesadas WHERE balanza_pesadas.id_balanza = balanzas.ID_BALANZA AND 
    balanza_pesadas.id_pesada = IPESADA NO-LOCK NO-ERROR.
                                     
 
IF AVAILABLE balanza_pesadas THEN 
DO ON ERROR UNDO, THROW:
    VTEXTO = "ACTUALIZAPESADA: INICIO".
    WLOG(ARCHIVO,VTEXTO).      
    RUN ACTUALIZADOCUMENTOMATERIALPESADA(BUFFER BALANZA_PESADAS).
    VTEXTO = "ACTUALIZAPESADA: OK".
    CATCH ERRA AS Progress.Lang.Error :
        ORESPUESTA = 'NOK'.
        OMENSAJERESPUESTA = ERRA:GetMessage(1). 
        VTEXTO = "ACTUALIZAPESADA : " + ERRA:GetMessage(1).
        UNDO , THROW ERRA.
    END CATCH.  
    FINALLY.
        WLOG(ARCHIVO,VTEXTO).
    END.      
END.
ELSE
DO TRANSACTION ON ERROR UNDO, THROW:
    
    VTEXTO = "INICIA SAPTOBALANZAPESADA".
    WLOG(ARCHIVO,VTEXTO)  .
    
    RUN sapToBalanzaPesada (balanzas.ID_BALANZA, OUTPUT vbalanza , OUTPUT vpesada ).

    VTEXTO = "FINALIZA SAPTOBALANZAPESADA".
    WLOG(ARCHIVO,VTEXTO)  .
    
    FIND balanza_pesadas WHERE balanza_pesadas.id_balanza = VBALANZA AND balanza_pesadas.ID_PESADA = VPESADA NO-LOCK.
    
    
    
    RUN valoresfijos (    INPUT  vbalanza,     
        OUTPUT vsucursal,
        OUTPUT vsucplaya,
        OUTPUT vtipomov,
        OUTPUT vsucorigen).
                          
    DEFINE VARIABLE I-C AS INTEGER NO-UNDO.
    I-C = 0.
    FOR EACH POSICIONES.
        I-C = I-C + 1.
    END.                          

    VTEXTO = "INICIA POSCIONES".
    WLOG(ARCHIVO,VTEXTO)  .

    
    I = 0.
    FOR EACH POSICIONES BY POSICIONES.PSNROLOTE:
        i = i + 1.

        VTEXTO = "POSICION " + STRING(I).
        WLOG(ARCHIVO,VTEXTO)  .
                
        RUN devuelvevaloresMovflete ( i, vsucursal ,
            OUTPUT cmateriaprima , 
            OUTPUT vcantenvasesentrada , 
            OUTPUT vpesoenvasesentrada , 
            OUTPUT ctipocosecha , 
            OUTPUT ctiposervicio , 
            OUTPUT cenvase , 
            OUTPUT corigensap , 
            OUTPUT ccodigotrazabilidad , 
            OUTPUT vcolor,
            OUTPUT cmercado ,
            OUTPUT vpesonetoticket , 
            OUTPUT vunioneuropea ,
            OUTPUT vchina ,
            OUTPUT vusa,
            OUTPUT vfinca ,
            OUTPUT vsucursaletiqueta,
            OUTPUT vsucursalpacking,
            OUTPUT vproveedor,
            OUTPUT vproveedororigen,
            OUTPUT vorigen,
            OUTPUT vorigenorigen,
            OUTPUT vtipocosecha,
            OUTPUT vcalidadbalanza,
            OUTPUT vtiposervicio,
            OUTPUT venvase,
            OUTPUT vlotesenasa,
            OUTPUT vlote,
            OUTPUT vfincasenasa,
            OUTPUT vcertunion,
            OUTPUT vcertchina,
            OUTPUT vcertificado,
            OUTPUT v1,
            OUTPUT v2,
            OUTPUT iarticulo,
            OUTPUT vvariedad,
            OUTPUT vzona_up,
            OUTPUT vrenspa).

        vcantenvasesentrada = INTEGER(POSICIONES.PSCANTENVASES).            
        vvariedadsap = vvariedad.
    
        FIND productos_terminados WHERE productos_terminados.id_articulo = iarticulo NO-LOCK.
        FIND FIRST periodo_cosecha NO-LOCK.


        vfechacosecha   = DATE( INTEGER(SUBSTRING(PSFECHACOSECHA,4,2)) , INTEGER(SUBSTRING(PSFECHACOSECHA,1,2)) , INTEGER(SUBSTRING(PSFECHACOSECHA,7,4)) ).
        vfecharemito    = DATE( INTEGER(SUBSTRING(PSFECHAREMITO,4,2)) , INTEGER(SUBSTRING(PSFECHAREMITO,1,2)) , INTEGER(SUBSTRING(PSFECHAREMITO,7,4)) ).


        vfechaoperativa = balanza_pesadas.fecha_operativa.
        vfechaentrada   = balanza_pesadas.fecha_entrada.
        vfechasalida    = balanza_pesadas.fecha_salida.



    
        vnroremito = PSNROREMITO.
    
        vindex      = INDEX(VNROREMITO, "-").
        IF vindex = 0 THEN
            vindex      = INDEX(VNROREMITO, "R").
    

        IF VINDEX > 0 THEN
        DO:
            vnroremito = FILL ('0', 4 - (LENGTH(SUBSTRING(vnroremito,1,vindex - 1)))) + SUBSTRING(vnroremito,1,vindex - 1) + FILL ('0', 8 - (LENGTH(SUBSTRING(vnroremito , vindex + 1)))) + SUBSTRING(vnroremito , vindex + 1).
        END.
        
    
        /* Asigno etiqueta */
        FIND LAST aux_items USE-INDEX etiqueta WHERE
            aux_items.id_sucursal_etiqueta = vsucursaletiqueta NO-LOCK NO-ERROR.
        IF AVAILABLE aux_items THEN
            vetiqueta = aux_items.id_etiqueta + 1.
        ELSE
            vetiqueta = 1.
    
        VTEXTO = "CREA BALANZATICKETS".
        WLOG(ARCHIVO,VTEXTO)  .

        {creabalanzatickets.i}
            

        VTEXTO = "PROCESA BALANZATICKETS".
        WLOG(ARCHIVO,VTEXTO)  .

        RUN procesarBalanzaTicket (BUFFER balanza_tickets).
        RELEASE balanza_tickets.
        SESSION:NUMERIC-FORMAT = nm.



    END. 
    VTEXTO = "CIERRA PESADA".
    WLOG(ARCHIVO,VTEXTO)  .
    
    RUN pcierrapesada.p (balanza_pesadas.id_balanza , balanza_pesadas.id_pesada , FALSE , OUTPUT cStatus).
    IF cStatus <> 'OK' THEN
        RETURN ERROR cStatus.

/*    CATCH ErrB AS Progress.Lang.ProError .
        OUTPUT TO D:\TEMP\WS2.TXT APPEND.
        EXPORT DELIMITER ';' "160" "ERROR" errC:GetMEssage(1).
        OUTPUT CLOSE.    
        ORESPUESTA = 'NOK'.
        OMENSAJERESPUESTA = 'Ingreso incorrecto'.  
        UNDO , THROW ErrB.
    END CATCH. */
END.

CATCH ErrC AS Progress.Lang.ProError .
    VTEXTO =  "161 " + errC:GetMEssage(1).
    WLOG(ARCHIVO,VTEXTO).
    ORESPUESTA = 'NOK'.
    OMENSAJERESPUESTA = 'Ingreso incorrecto'.  
    UNDO .
END CATCH.

FINALLY.
    CREATE RESPUESTAS.
    ASSIGN 
        RESPUESTAS.POSTATUS  = ORESPUESTA
        RESPUESTAS.POMENSAJE = OMENSAJERESPUESTA.
    VTEXTO =  "FINALIZO".
    WLOG(ARCHIVO,VTEXTO).
END FINALLY.


PROCEDURE ACTUALIZADOCUMENTOMATERIALPESADA.
    DEFINE PARAMETER BUFFER bp FOR balanza_pesadas.
    
    DEFINE VARIABLE I AS INTEGER NO-UNDO.
    I = 0.
    
    FOR EACH POSICIONES BY POSICIONES.PSNROLOTE.
        I = I + 1.
        FIND FIRST balanza_tickets OF bp WHERE balanza_tickets.nro_ticket = I.
        ASSIGN 
            balanza_tickets.orden_compra_sap     = POSICIONES.PSNROPESADA
            balanza_tickets.pos_orden_compra_sap = POSICIONES.PSNROPOSICIONPESADA. 
    END.
END PROCEDURE.

PROCEDURE SAPTOBALANZAPESADA.

    DEFINE INPUT  PARAMETER PIBALANZA                   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER pobalanza                   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER popesada                    AS INTEGER NO-UNDO.

    DEFINE VARIABLE X_numero        AS INTEGER.
    DEFINE VARIABLE hProd           AS HANDLE    NO-UNDO.
    DEFINE VARIABLE vfecha          AS DATE      NO-UNDO.
    DEFINE VARIABLE vfechaOperativa AS DATE      NO-UNDO.
    DEFINE VARIABLE vhora           AS DATETIME  NO-UNDO.
    DEFINE VARIABLE vfechaEntrada   AS DATE      NO-UNDO.
    DEFINE VARIABLE vfechaSalida    AS DATE      NO-UNDO.
    DEFINE VARIABLE vHoraEntrada    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vHoraSalida     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vPesoEntrada    AS DECIMAL   NO-UNDO DECIMALS 3.
    DEFINE VARIABLE vTransporte     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vProveedor      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vtipotransporte AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vmarca          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vmodelo         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vnromov         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vpesoneto       AS DECIMAL   NO-UNDO DECIMALS 3.
    DEFINE VARIABLE vpesodescarte   AS DECIMAL   NO-UNDO DECIMALS 3.
    DEFINE VARIABLE vpesosalida     AS DECIMAL   NO-UNDO DECIMALS 3.
    DEFINE VARIABLE vtara           AS DECIMAL   NO-UNDO DECIMALS 3.
    DEFINE VARIABLE vsucursal       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vsucplaya       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vtipomov        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vsucorigen      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vpatente        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE xenvase         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vpesadasap      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE VOCARGA         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError          AS CHARACTER NO-UNDO.
        /* DEFINE VAR vocarga          AS CHARACTER NO-UNDO. */ /* NO LO USO */
    DEFINE VARIABLE vbalanza        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vpesada         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vpref           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vTransportista  AS CHARACTER NO-UNDO.

    DO TRANSACTION ON ERROR UNDO, THROW.
    
        /* {persistentprocedure.i libProduccion.p hProd}. */

    
        /* vocarga = hTableMovFlete:VALUE(1,2). */ /* NO LO USO */

     
        vfechaEntrada = DATE( INTEGER(SUBSTRING(SFECHAENTRADA,5,2)) , INTEGER(SUBSTRING(SFECHAENTRADA,7,2)) , INTEGER(SUBSTRING(SFECHAENTRADA,1,4)) ).
        vfecha = VFECHAENTRADA.
        vhoraEntrada  = SHORAENTRADA.
        vfechaOperativa =  DYNAMIC-FUNCTION("devuelvefechaoperativa" IN THIS-PROCEDURE, vfecha , vhoraEntrada).

    
    

        vpesadasap = SNROPESADA.    

        vbalanza = pibalanza.
        vpref    = vbalanza.
        vpesada  = vpref * 1000000 + INTEGER(VPESADASAP).
    
        RUN valoresfijos   (    INPUT vbalanza,     
            OUTPUT vsucursal,
            OUTPUT vsucplaya,
            OUTPUT vtipomov,
            OUTPUT vsucorigen).
                            
    
        vfechaSalida  = DATE(INTEGER(SUBSTRING(SFECHASALIDA,5,2)) , INTEGER(SUBSTRING(SFECHASALIDA,7,2)), INTEGER(SUBSTRING(SFECHASALIDA,1,4)) ).
        vhoraSalida   = SHORASALIDA.
        vPesoEntrada  = DECIMAL(SPESOENTRADA). 
        vPesoSalida   = DECIMAL(SPESOSALIDA). 
    
        IF vfechasalida = DATE('30/12/1899') THEN
            vfechasalida = ?.
        vTransporte   = STRANSPORTE.
        vtransportista = STRANSPORTISTA.
        /*
        FIND FIRST proveedores WHERE proveedores.id_proveedor_sap = vtransportista NO-LOCK NO-ERROR.
        IF NOT AVAILABLE proveedores  THEN
            RETURN ERROR 'Trasnportista inexistente - Comunicarse con Transporte'.
        vproveedor = proveedores.id_proveedor.
        IF vTransporte <> "" THEN
        DO:
         
            FIND FIRST transportes_proveedor WHERE
               transportes_proveedor.id_proveedor = proveedores.id_proveedor AND 
               transportes_proveedor.id_transporte_sap = vTransporte NO-LOCK NO-ERROR.
            IF NOT AVAILABLE transportes_proveedor THEN RETURN ERROR "Error en numero de interno . Comunicarse con Transporte ".
        
            ASSIGN      vtipotransporte = transportes_proveedor.id_tipo_transporte
                        vmarca          = transportes_proveedor.marca
                        vmodelo         = transportes_proveedor.modelo
                        vpatente        = transportes_proveedor.patente
                        vproveedor      = transportes_proveedor.id_proveedor.
            
            FIND LAST mov_transp_cabecera NO-LOCK NO-ERROR.
            IF AVAILABLE mov_transp_cabecera THEN
                ASSIGN vnromov = mov_transp_cabecera.nromov + 1.
            ELSE
                ASSIGN vnromov = 1.
        END.
        ELSE
        DO:
           FIND FIRST r_prov_activ WHERE r_prov_activ.id_proveedor = vproveedor AND
               r_prov_activ.id_actividad = 3 NO-LOCK NO-ERROR.
           IF NOT AVAILABLE r_prov_activ THEN RETURN ERROR "Debe ingresar un transportista valido. Comunicarse con transporte".
        END.
        */



        ASSIGN 
            vpesoneto = DECIMAL(SPESONETO).
        ASSIGN 
            vpesodescarte = DECIMAL(SPESODESCARTE).


    {creabalanzapesada.i}
    
        /* RUN crearmovtransportedebalanza (  INPUT vbalanza, INPUT vpesada) NO-ERROR. */ 


        FIND FIRST tipo_numero WHERE tipo_numero.id_sucursal = vsucplaya AND
            tipo_numero.id_tipo_movsto = vtipomov NO-ERROR.
        x_numero = tipo_numero.nro + 1.

   
        ASSIGN 
            tipo_numero.nro = x_numero.
        RELEASE tipo_numero.

        ASSIGN
            balanza_pesadas.id_sucursal    = vsucplaya
            balanza_pesadas.id_tipo_movsto = vtipomov
            balanza_pesadas.nro            = x_numero.

        VTEXTO =  "CREAMOVSUCU".
        WLOG(ARCHIVO,VTEXTO).

    {creamovsucu.i}
    
        VTEXTO =  "TERMINA CREAMOVSUCU".
        WLOG(ARCHIVO,VTEXTO).

    END.
    FINALLY.
        pobalanza = vbalanza.
        popesada  = vpesada.
    END FINALLY.


END PROCEDURE.       


PROCEDURE procesarBalanzaTicket :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER balanza_tickets FOR balanza_tickets.

    
    DEFINE VARIABLE hProd AS HANDLE NO-UNDO.
    DEFINE BUFFER aux_pesada  FOR balanza_pesadas.
    DEFINE BUFFER aux_tickets FOR balanza_tickets.
    DEFINE BUFFER aux_salida  FOR balanza_salidas.
    DEFINE VARIABLE X_peso_envases AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v1             AS INTEGER.
    DEFINE VARIABLE v2             AS INTEGER.

    DEFINE VARIABLE cerror         AS CHARACTER NO-UNDO INITIAL 'Error en la rutina de proceso de la balanza'.



    DO ON ERROR UNDO , THROW:
    

        FIND FIRST      lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = balanza_tickets.codigo_trazabilidad NO-LOCK.
        /*    IF NOT AVAILABLE lotes_plantacion THEN RETURN ERROR 'Error de lotes de plantacion'. */

        FIND FIRST   lote        OF lotes_plantacion  NO-LOCK.
        /*    IF NOT AVAILABLE lotes_plantacion THEN RETURN ERROR 'Error de lotes'. */

        IF balanza_tickets.UNION_europea THEN
            ASSIGN  balanza_tickets.certificado = lote.certificado.
        ELSE
            ASSIGN  balanza_tickets.certificado = ""
                balanza_tickets.cert_china  = "".

        IF balanza_tickets.china THEN
            ASSIGN  balanza_tickets.certificado = lote.cert_china
                balanza_tickets.cert_china  = lote.cert_china.
    
    
        CASE balanza_tickets.id_balanza:
            WHEN  2 OR  
            WHEN 4 THEN
                DO:
    
                    RUN crearpartidas (INPUT balanza_tickets.id_sucursal,
                        INPUT balanza_tickets.nro_partida ).
                    RUN crearitemsstockdebalanza (INPUT balanza_tickets.id_balanza, 
                        INPUT balanza_tickets.id_pesada, balanza_tickets.nro_ticket).
                END.
            WHEN 7 THEN
                DO:
                    FIND FIRST aux_pesada WHERE
                        aux_pesada.id_balanza = balanza_tickets.id_balanza AND
                        aux_pesada.id_pesada  = balanza_tickets.id_pesada NO-LOCK.
    
    
                    FIND FIRST aux_tickets WHERE   
                        aux_tickets.id_balanza = balanza_tickets.id_balanza AND
                        aux_tickets.id_pesada  = balanza_tickets.id_pesada NO-LOCK.
                    IF AVAILABLE aux_tickets THEN 
                    DO:
                        FOR EACH aux_salida OF aux_pesada:
                            DELETE aux_salida.
                        END.
                        FOR EACH aux_tickets OF aux_pesada:
                            CREATE aux_salida.
                            ASSIGN
                                aux_salida.id_balanza          = aux_tickets.id_balanza
                                aux_salida.id_pesada           = aux_tickets.id_pesada
                                aux_salida.id_envase           = aux_tickets.id_envase
                                aux_salida.cant_env_salida     = aux_tickets.cant_env_entrada
                                aux_salida.peso_envases_salida = aux_tickets.peso_envases_entrada.
    
                        END.
                        FIND aux_pesada WHERE
                            aux_pesada.id_balanza = balanza_tickets.id_balanza AND
                            aux_pesada.id_pesada  = balanza_tickets.id_pesada.
    
                        IF AVAILABLE aux_pesada THEN 
                        DO:
                            FOR EACH aux_tickets WHERE
                                aux_tickets.id_balanza = balanza_tickets.id_balanza AND
                                aux_tickets.id_pesada  = balanza_tickets.id_pesada:
                                x_peso_envases  = x_peso_envases  + aux_tickets.peso_envases_entrada.
                            END.
                            ASSIGN 
                                aux_pesada.peso_envases_entrada = x_peso_envases.
                        END.
                    END.
                END.
        END CASE.
    
        /* Creo tabla items_transporte */

        /*
        RUN crearitemstranspdebalanza IN hprod (    INPUT balanza_tickets.id_balanza,
                                                    INPUT balanza_tickets.id_pesada). */
    
    
        /* Actualizo fecha y hora salida de balanza_pesada */
        FIND FIRST aux_pesada WHERE aux_pesada.id_balanza = balanza_tickets.id_balanza AND
            aux_pesada.id_pesada = balanza_tickets.id_pesada.
        IF AVAILABLE aux_pesada THEN
        DO:
            ASSIGN 
                aux_pesada.fecha_salida = balanza_tickets.fecha_salida
                aux_pesada.hora_salida  = balanza_tickets.hora_salida.
        END.
    

        IF balanza_tickets.id_balanza = 2 OR balanza_tickets.id_balanza = 4 THEN
        DO:
            /* Actualizo pesos */
            RUN pesosbalanzapesada (INPUT balanza_tickets.id_balanza , balanza_tickets.ID_PESADA ). 
    
            /* Actualizo saldos de partida */
            RUN actsaldospartidadebalanza (INPUT balanza_tickets.id_sucursal, INPUT balanza_tickets.nro_partida , INPUT balanza_tickets.nro_partida_serial). 
    
        /*
            /* Imprimo etiqueta */
            RUN dd_etibal.p (input balanza_tickets.nro_partida).
        */
        END.
    END.
/*
CATCH err AS Progress.Lang.Error:
    OUTPUT TO D:\TEMP\WS2.TXT APPEND.
    EXPORT DELIMITER ';' "21" err:GetMEssage(1).
    OUTPUT CLOSE.    
    UNDO, THROW ERR .    
END CATCH.
*/
END PROCEDURE.


PROCEDURE valoresfijos :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* REVISAR:  */
    DEFINE INPUT PARAMETER X_balanza AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER x_sucursal  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER x_suc_playa AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER x_tipo_mov  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER x_suc_origen AS INTEGER NO-UNDO.

    
    FIND FIRST balanzas WHERE
        balanzas.id_balanza   = X_balanza AND
        balanzas.id_suc_envio <> 0 NO-LOCK.
    IF AVAILABLE balanzas THEN
    DO:
        FIND FIRST tipo_movsto WHERE tipo_movsto.abreviatura = "ingreso"
            NO-LOCK NO-ERROR.
    
        x_sucursal  = balanzas.id_sucursal.
        x_suc_playa = balanzas.id_suc_envio.
        x_tipo_mov  = tipo_movsto.id_tipo_movsto.
        X_suc_origen = balanzas.id_suc_origen.
    END.
END PROCEDURE.

PROCEDURE devuelvevaloresmovflete :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER i AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vsucursal AS INTEGER NO-UNDO.

    DEFINE OUTPUT PARAMETER cmateriaprima AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcantenvasesentrada AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vpesoenvasesentrada AS DECIMAL NO-UNDO DECIMALS 3.
    DEFINE OUTPUT PARAMETER ctipocosecha        AS  CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ctiposervicio       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cenvase             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER corigensap          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ccodigotrazabilidad AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcolor              AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER cmercado            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vpesonetoticket     AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vunioneuropea       AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER vchina              AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER vusa                AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER vfinca              AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER vsucursaletiqueta   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vsucursalpacking    AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vproveedor          AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vproveedororigen    AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vorigen             AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vorigenorigen       AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vtipocosecha        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcalidadbalanza     AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vtiposervicio       AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER venvase             AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vlotesenasa         AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vlote               AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vfincasenasa        AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcertunion          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcertchina          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vcertificado        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER v1                  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER v2                  AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER iarticulo           AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER ivariedad           AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER vzona_up            AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER vrenspa             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cantpes  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE vcantpes AS CHARACTER NO-UNDO.


    cmateriaprima       = POSICIONES.PSMATERIAL. /* REVISAR */   
    ctipocosecha        = SUBSTRING(POSICIONES.PSMATERIAL,4,1).
    /*  ctiposervicio       = PSTIPOSERVICIO. NO LO TENGO */
    cenvase             = PSTIPOENVASE.
        /*     corigensap          = STRING(htableMovFleteD:VALUE(i,15),"x(4)") + STRING(htableMovFleteD:VALUE(i,16),"9999"). */ /* NO SE USA */
    ccodigoTrazabilidad = PSTRAZABILIDAD.
    cmercado            = PSMERCADO. /**** REVISAR MERCADO SEGUN TABLA ***/
    
    
    CASE SUBSTRING(cmateriaprima,6,2):
        WHEN '01' THEN 
            iarticulo = 1. /* LIMON */
        WHEN '04' THEN 
            iarticulo = 2. /* POMELO */
        WHEN '02' THEN 
            iarticulo = 3. /* NARANJA */
        WHEN '03' THEN 
            iarticulo = 4. /* MANDARINA */
        
        /*        WHEN 'PALTA001' THEN iarticulo = 5. FALTAN VARIEDADES
                WHEN 'TANGELO001' THEN iarticulo = 6.
                WHEN 'KUNKUA001' THEN iarticulo = 7. */
        OTHERWISE 
        iarticulo = 0.
    END CASE.
    
    
    /* PROCESAR EL MATERIAL SAP PARA DETERMINAR EL ARTICULO */

    FIND FIRST      tipo_cosecha WHERE id_tipo_cosecha_sap      =  ('0' + ctipocosecha) NO-LOCK NO-ERROR.
    IF AVAILABLE    tipo_cosecha THEN ASSIGN vtipocosecha    = tipo_cosecha.id_tipo_cosecha
            vcalidadbalanza = tipo_cosecha.id_calidad_balanza. 
    ELSE ASSIGN vtipocosecha    = 0 vcalidadbalanza = 0.
    FIND FIRST      tipos_servicios WHERE id_tipo_servicio_sap =  ctiposervicio NO-LOCK NO-ERROR.
    IF AVAILABLE    tipos_servicios THEN  vtiposervicio = tipos_servicios.id_tipo_servicio. 
    ELSE vtiposervicio = 0.
    
     
    vcantpes = PSCANTPES.                                  
    IF INDEX(SESSION:NUMERIC-DECIMAL-POINT , vcantpes ) <> 0 THEN
        cantpes = DECIMAL(vcantpes).
    ELSE
    DO:
        IF SESSION:NUMERIC-DECIMAL-POINT = '.' THEN
            cantpes = DECIMAL (REPLACE(vcantpes , ',' , '.')  ).
        ELSE
            cantpes = DECIMAL (REPLACE(vcantpes , '.' , ',') ).
    END.

    IF cenvase <> "" THEN 
    DO:
        FIND FIRST      envases_prod WHERE id_envase_sap =  cenvase NO-LOCK NO-ERROR.
        IF AVAILABLE    envases_prod THEN venvase = envases_prod.id_envase . 
        ELSE venvase = 0.

        FIND r_envases_prod WHERE r_envases_prod.id_envase = envases_prod.id_envase AND
            r_envases_prod.id_articulo = iArticulo NO-LOCK NO-ERROR.

/*        vcantenvasesentrada = IF TRIM(STRING( PSUNIDAD )) = 'KG' THEN (IF AVAILABLE r_envases_prod THEN cantpes / r_envases_prod.Kilos ELSE 0) ELSE INTEGER(PSCANTPES). */
        vpesoenvasesentrada = vcantenvasesentrada * ( IF AVAILABLE envases_prod THEN envases_prod.tara ELSE 0  ).
        vpesonetoticket     = IF TRIM(STRING( PSUNIDAD )) = 'KG' THEN ( CANTPES - VPESOENVASESENTRADA ) ELSE vcantenvasesentrada * ( IF AVAILABLE r_envases_prod THEN r_envases_prod.kilos ELSE 0  ).
    END.
    ELSE
    DO:
        venvase = 0.
        vcantenvasesentrada = 0.
        vpesoenvasesentrada = 0.
        vpesonetoticket     = cantpes.
    END.
    /*
         FIND FIRST      colores  WHERE colores.id_color_sap = PSCOLOR NO-LOCK NO-ERROR.
         IF AVAILABLE    colores  THEN vcolor = colores.id_color . ELSE vcolor = 0. */


    FIND FIRST      colores  WHERE colores.id_color_sap = INTEGER(PSCOLOR) NO-LOCK NO-ERROR.
    IF AVAILABLE    colores  THEN vcolor = colores.id_color . 
    ELSE vcolor = 0. 


    FIND FIRST      lotes_plantacion WHERE lotes_plantacion.codigo_trazabilidad = ccodigotrazabilidad NO-LOCK.
    /* IF NOT AVAILABLE lotes_plantacion THEN RETURN ERROR 'Error de lotes de plantacion'. */

    FIND FIRST   lote        OF lotes_plantacion  NO-LOCK NO-ERROR.
    IF AVAILABLE lote  THEN ASSIGN vlotesenasa = lote.id_lote_senasa
            vlote       = lote.id_lote. 
    ELSE ASSIGN vlotesenasa = 0 vlote       = 0.

    FIND FIRST      origenes    OF lote NO-LOCK NO-ERROR.       
    IF AVAILABLE    origenes  THEN
        ASSIGN
            vfincasenasa = origenes.id_finca_senasa 
            vorigen      = origenes.id_origen 
            vzona_up     = origenes.zona_up 
            vrenspa      = origenes.renspa.
    ELSE
        ASSIGN
            vfincasenasa = 0
            vorigen      = 0
            vzona_up     = "" 
            vrenspa      = "".
    FIND FIRST      proveedores OF origenes  NO-LOCK NO-ERROR.
    IF AVAILABLE    proveedores THEN vproveedor = proveedores.id_proveedor. 
    ELSE vproveedor = 0.


    FIND variedades WHERE variedades.id_articulo = iarticulo AND
        variedades.id_variedad = lote.id_variedad NO-LOCK NO-ERROR.
    IF NOT AVAILABLE variedades THEN ivariedad = 0. 
    ELSE ivariedad = variedades.id_variedad.
    
    IF iarticulo = 1 THEN
        ivariedad = 1.  /***** FUERZO VARIEDAD 1 EN LIMON ***/

    CASE cmercado: /*** CONFIRMAR LO QUE VUELVE DE MERCADO **/
        WHEN 'UE' THEN
            DO:
                vunioneuropea = TRUE.
                vchina        = FALSE.
                vusa          = FALSE.
            END.
        WHEN 'CHINA' THEN
            DO:
                vunioneuropea = FALSE.
                vchina = TRUE.
                vusa          = FALSE.
            END.
        WHEN 'USA' THEN
            DO:
                vunioneuropea   = FALSE.
                vchina          = FALSE.
                vusa            = TRUE.
            END.
        OTHERWISE 
        DO:
            vunioneuropea = FALSE.
            vchina        = FALSE.
        END.
    END CASE.
    /*** DETERMINAR SI LA FRUTA ES DE FINCA O ES PROCESADA **/
    /*
    IF hTableMovfleteD:VALUE(1,15) = 'A300' THEN
        vfinca = TRUE.
    ELSE
        vfinca = FALSE.
    */
    
    IF SUBSTRING(CMATERIAPRIMA,1,3) = '303' THEN
        VFINCA = FALSE.
    ELSE 
        VFINCA = TRUE.
        
    IF vfinca THEN 
    DO:

        IF vsucursal = 98 THEN
            vsucursaletiqueta = 101.
        ELSE
            vsucursaletiqueta = 111.

        ASSIGN 
            vsucursalpacking = 0
            vproveedororigen = vproveedor
            vorigenorigen    = vorigen.
    END.

    IF NOT vfinca OR vtipocosecha = 4 THEN 
    DO:

        IF vsucursal = 98 THEN
            vsucursaletiqueta = 110.
        ELSE
            vsucursaletiqueta = 130.

        ASSIGN
            vsucursalpacking = vsucursal
            vproveedororigen = 1
            vorigenorigen    = (IF vsucursal = 98 THEN 97 ELSE 98).
    END.
    
    vcertunion  = lote.certificado.
    vcertchina  = lote.cert_china.


    vcertificado = "".
    IF cmercado = 'UE' THEN
    DO:
        IF vcertunion = "" THEN UNDO , THROW NEW Progress.Lang.AppError("El lote no posee certificado UE", 550).
        vchina = NO.
        vunioneuropea = TRUE.
        vcertificado = vcertunion.
    END.


    IF cmercado  = 'CHINA' THEN
    DO:
        IF vcertchina = "" THEN UNDO , THROW NEW Progress.Lang.AppError("El lote no posee certificado CH", 550).
        vunioneuropea = NO.
        vchina = TRUE.
        vcertificado = vcertchina. 
    END.
    IF vsucursal = 98 THEN  ASSIGN v1 = 110
            v2 = 97.
    ELSE  ASSIGN v1 = 130
            v2 = 98. 
/*
    CATCH ERRX AS Progress.Lang.Error :
        UNDO, THROW ERRX.
    END CATCH.
*/    

END PROCEDURE.


PROCEDURE crearpartidas :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    /** Crea tabla partidas a partir de balanza_tickets */
    DEFINE INPUT PARAMETER vsuctrabajo AS INTEGER.
    DEFINE INPUT PARAMETER vpartida AS INTEGER.


    FIND FIRST produccion.partidas WHERE produccion.partidas.id_suc_trabajo = vsuctrabajo AND
        partidas.nro_partida = vpartida NO-ERROR.
    IF NOT AVAILABLE partidas THEN
    DO:
        CREATE partidas.
        ASSIGN 
            partidas.id_suc_trabajo = vsuctrabajo
            partidas.nro_partida    = vpartida.
     
        FIND FIRST balanza_tickets WHERE balanza_tickets.id_sucursal = partidas.id_suc_trabajo AND
            balanza_tickets.nro_partida = partidas.nro_partida AND 
            balanza_tickets.nro_partida_serial = 1 NO-LOCK.
        IF AVAILABLE balanza_tickets THEN
        DO:
            BUFFER-COPY balanza_tickets TO partidas.
        END.
    END.   
/*  
CATCH ERRX AS Progress.Lang.Error :
    UNDO, THROW ERRX.
END CATCH. */
END PROCEDURE.  
  
PROCEDURE crearitemsstockdebalanza :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER x_balanza    AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER x_pesada     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER X_nro_ticket AS INTEGER NO-UNDO.

    DEFINE VARIABLE X_partida        AS INTEGER NO-UNDO.
    DEFINE VARIABLE X_partida_serial AS INTEGER NO-UNDO.
    DEFINE VARIABLE X_suc_etiq       AS INTEGER NO-UNDO.
    DEFINE VARIABLE X_etiqueta       AS INTEGER NO-UNDO.
    DEFINE VARIABLE v1               AS INTEGER NO-UNDO.
    DEFINE VARIABLE v2               AS INTEGER NO-UNDO.
    DEFINE VARIABLE K                AS INTEGER NO-UNDO.

    FIND FIRST balanza_pesadas WHERE balanza_pesadas.id_balanza = x_balanza AND
        balanza_pesadas.id_pesada = x_pesada NO-LOCK.
    /*    IF NOT AVAILABLE balanza_pesadas THEN RETURN ERROR 'PESADA INEXISTENTE'. */

    FIND FIRST balanza_tickets WHERE balanza_tickets.id_balanza = X_balanza AND
        balanza_tickets.id_pesada = X_pesada AND
        balanza_tickets.nro_ticket = X_nro_ticket NO-LOCK.
    /*    IF NOT AVAILABLE balanza_tickets THEN RETURN ERROR 'TICKET INEXISTENTE'. */

    X_partida = balanza_tickets.nro_partida.
    x_partida_serial = balanza_tickets.nro_partida_serial.
    X_suc_etiq = balanza_tickets.id_sucursal_etiqueta.
    X_etiqueta = balanza_tickets.id_etiqueta.

    FIND FIRST  movsucu OF balanza_pesadas.
    IF AVAILABLE  movsucu THEN 
    DO:
        IF  movsucu.fecha_proceso = ? THEN 
        DO:
            ASSIGN
                movsucu.fecha     = balanza_pesadas.fecha_salida
                movsucu.hora      = SUBSTRING(balanza_pesadas.hora_salida,1,2) + "00"
                movsucu.peso      = balanza_pesadas.peso_neto
                movsucu.c_usuario = USERID("userdb")
                movsucu.c_fecha   = TODAY
                movsucu.c_hora    = STRING(TIME, "hh:mm:ss").

            ASSIGN  
                movsucu.fecha_operativa = balanza_pesadas.fecha_operativa.
            
            WLOG(ARCHIVO, 'CREAITEMSTOCK').
            FIND FIRST  items_stock WHERE
                items_stock.id_sucursal =  movsucu.id_sucursal AND
                items_stock.id_tipo_movsto =  movsucu.id_tipo_movsto AND
                items_stock.nro =  movsucu.nro AND
                items_stock.item = x_nro_ticket NO-ERROR.
            IF NOT AVAILABLE  items_stock THEN 
            DO:
                CREATE  items_stock.
                ASSIGN
                    items_stock.id_sucursal     = movsucu.id_sucursal
                    items_stock.id_tipo_movsto  = movsucu.id_tipo_movsto
                    items_stock.id_suc_envio    = movsucu.id_suc_envio
                    items_stock.nro             = movsucu.nro
                    items_stock.fecha_operativa = movsucu.fecha_operativa.
                RUN s_stock.p (OUTPUT  items_stock.id_serial).
            END.
            
            
            ASSIGN
                items_stock.nro_partida          = x_partida
                items_stock.nro_partida_origen   = x_partida
                items_stock.nro_partida_serial   = x_partida_serial     
                items_stock.id_sucursal_etiqueta = x_suc_etiq
                items_stock.id_etiqueta          = x_etiqueta
                items_stock.item                 = balanza_tickets.nro_ticket
                items_stock.fecha                = balanza_tickets.fecha_salida
                items_stock.hora                 = SUBSTRING(balanza_tickets.hora_salida,1,2) +
                                                      substring(balanza_tickets.hora_salida,4,2)
                items_stock.dia                  = WEEKDAY(balanza_tickets.fecha_salida)
                items_stock.nro_comprobante      = balanza_tickets.nro_remito
                items_stock.id_articulo          = balanza_tickets.id_materia_prima
                items_stock.peso                 = balanza_tickets.peso_neto_ticket
                items_stock.codigo_stock         = "I"
                items_stock.cantidad             = balanza_tickets.cant_env_entrada
                items_stock.cantidad1            = balanza_tickets.peso_neto_ticket / 20 /* cantidad de bandejas */
                items_stock.id_calidad           = balanza_tickets.id_calidad_balanza
                items_stock.id_variedad          = balanza_tickets.id_variedad
                items_stock.id_envase            = balanza_tickets.id_envase
                items_stock.id_color             = 0
                items_stock.fecha_cosecha        = balanza_tickets.fecha_cosecha
                items_stock.union_europea        = balanza_tickets.union_europea
                items_stock.china                = balanza_tickets.china
                items_stock.id_proveedor         = balanza_tickets.id_proveedor
                items_stock.id_origen            = balanza_tickets.id_origen
                items_stock.id_lote              = balanza_tickets.id_lote
                items_stock.id_finca_senasa      = balanza_tickets.id_finca_senasa
                items_stock.id_lote_senasa       = balanza_tickets.id_lote_senasa
                items_stock.codigo_trazabilidad  = balanza_tickets.codigo_trazabilidad
                items_stock.id_tipo_cosecha      = balanza_tickets.id_tipo_cosecha
                items_stock.periodo_cosecha      = balanza_tickets.periodo_cosecha
                items_stock.certificado          = balanza_tickets.certificado
                items_stock.cert_china           = balanza_tickets.cert_china
                items_stock.c_usuario            = USERID("userdb")
                items_stock.c_fecha              = TODAY
                items_stock.c_hora               = STRING(TIME, "hh:mm:ss").

            WLOG(ARCHIVO, 'TERMINACREAITEMSTOCK').
        
            IF  items_stock.peso <> 0 THEN
                ASSIGN  items_stock.cantidad1 = items_stock.peso / 20.
        
            CASE balanza_tickets.id_tipo_cosecha:
                WHEN 1 THEN
                    items_stock.estado_fruta = FALSE.
                WHEN 4 THEN
                    items_stock.estado_fruta = TRUE.
                OTHERWISE
                items_stock.estado_fruta = FALSE.
            END CASE.

            /******* Archivo de Bines *******/
        
            FIND FIRST bines USE-INDEX proveedor_remito WHERE
                bines.id_proveedor  = balanza_tickets.id_proveedor AND
                bines.nro_remito    = balanza_tickets.nro_remito NO-LOCK NO-ERROR.
            IF AVAILABLE bines THEN 
            DO:
                FOR EACH bines WHERE
                    bines.id_proveedor  = balanza_tickets.id_proveedor AND
                    bines.nro_remito    = balanza_tickets.nro_remito AND 
                    bines.nro_ticket    = 0 :
            
                    ASSIGN
                        bines.id_balanza         = x_balanza
                        bines.id_pesada          = x_pesada
                        bines.nro_ticket         = x_nro_ticket
                        bines.nro_partida        = x_partida
                        bines.nro_partida_serial = x_partida_serial
                        bines.id_sucursal        = items_stock.id_sucursal
                        bines.id_tipo_movsto     = items_stock.id_tipo_movsto
                        bines.nro                = items_stock.nro
                        bines.item               = items_stock.item
                        bines.id_suc_envio       = items_stock.id_suc_envio
                        bines.fecha_operativa    = items_stock.fecha_operativa
                        bines.fecha              = items_stock.fecha
                        bines.hora               = items_stock.hora.
                END.
            END.
            ELSE 
            DO:    
                DO k = 1 TO balanza_tickets.cant_env_entrada:
                    CREATE bines.
                    ASSIGN
                        bines.id_balanza         = x_balanza
                        bines.id_pesada          = x_pesada
                        bines.nro_ticket         = x_nro_ticket
                        bines.nro_partida        = x_partida
                        bines.nro_partida_serial = x_partida_serial
                        bines.id_sucursal        = items_stock.id_sucursal
                        bines.id_tipo_movsto     = items_stock.id_tipo_movsto
                        bines.nro                = items_stock.nro
                        bines.item               = items_stock.item
                        bines.id_suc_envio       = items_stock.id_suc_envio
                        bines.fecha_operativa    = items_stock.fecha_operativa
                        bines.fecha              = items_stock.fecha
                        bines.hora               = items_stock.hora
                        bines.id_proveedor       = items_stock.id_proveedor
                        bines.id_origen          = items_stock.id_origen
                        bines.id_lote            = items_stock.id_lote.
                END.
            END.
    
            RELEASE bines.
    
    
            /******* Archivo Relacion Bines/Items Stock *******/
            FOR EACH bines NO-LOCK WHERE
                bines.nro_partida           = x_partida AND
                bines.nro_partida_serial    = x_partida_serial:
            
                CREATE r_bines_items_stock.
                ASSIGN
                    r_bines_items_stock.nro_bin            = bines.nro_bin
                    r_bines_items_stock.nro_partida        = bines.nro_partida
                    r_bines_items_stock.nro_partida_serial = bines.nro_partida_serial
                    r_bines_items_stock.id_sucursal        = bines.id_sucursal
                    r_bines_items_stock.id_tipo_movsto     = bines.id_tipo_movsto
                    r_bines_items_stock.item               = bines.item
                    r_bines_items_stock.nro                = bines.nro.
            END.
           
            RELEASE r_bines_items_stock.




            /* Revisar --ver parametrizar */
            IF balanza_tickets.id_sucursal = 98 THEN  ASSIGN v1 = 110
                    v2 = 97.
            ELSE ASSIGN v1 = 130
                    v2 = 98. 
            
            
            

            /******* Llama al programa para estimacion cosecha *******/
            IF balanza_tickets.id_tipo_cosecha = 1 AND
                (balanza_tickets.union_europea OR 
                balanza_tickets.china) AND
                balanza_tickets.id_sucursal_etiqueta <> v1 AND
                balanza_tickets.id_origen_origen <> v2 THEN 
            DO:

                RUN dd_sdoest.p
                    (INPUT balanza_tickets.fecha_cosecha,
                    INPUT balanza_tickets.id_proveedor,
                    INPUT balanza_tickets.id_finca_senasa,
                    INPUT balanza_tickets.id_lote_senasa,
                    INPUT balanza_tickets.certificado,
                    INPUT balanza_tickets.codigo_trazabilidad,
                    INPUT balanza_tickets.peso_neto_ticket,
                    INPUT "alta").
            END.

        END.
    END.
/*
    CATCH ERRX AS Progress.Lang.Error :
        UNDO, THROW ERRX.
    END CATCH. */
END PROCEDURE.

PROCEDURE pesosbalanzapesada :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER X_BALANZA AS INTEGER NO-UNDO.    
    DEFINE INPUT PARAMETER x_pesada AS INTEGER NO-UNDO.

    DEFINE BUFFER auxbalticket FOR balanza_tickets.
    DEFINE BUFFER auxbalpes    FOR balanza_tickets.
    DEFINE BUFFER AUXBAL       FOR balanza_PESADAS.
    
    DEFINE VARIABLE vpesoneto     AS DECIMAL.
    DEFINE VARIABLE vpesodescarte AS DECIMAL.

    FIND FIRST AUXBAL WHERE AUXBAL.id_balanza = X_PESADA AND AUXBAL.ID_PESADA = X_PESADA  NO-LOCK NO-ERROR.

    

    IF AVAILABLE AUXBAL THEN
    DO:
        FIND FIRST balanzas WHERE balanzas.id_balanza = AUXBAL.ID_BALANZA NO-LOCK.
        vpesoneto = 0.
        FOR EACH auxbalticket WHERE auxbalticket.id_balanza = AUXBAL.id_balanza AND
            auxbalticket.id_pesada = AUXBAL.id_pesada NO-LOCK:
            IF auxbalticket.id_tipo_cosecha = 1 OR
                auxbalticket.id_tipo_cosecha = 4 OR
                auxbalticket.id_tipo_cosecha = 5 THEN
                vpesoneto = vpesoneto + auxbalticket.peso_neto.
        END.
        
        vpesodescarte = 0.
        IF balanzas.id_suc_envio <> 0 THEN
        DO:
            FOR EACH auxbalticket WHERE auxbalticket.id_balanza = AUXBAL.id_balanza AND
                auxbalticket.id_pesada = AUXBAL.id_pesada NO-LOCK:
                IF auxbalticket.id_tipo_cosecha = 2 OR
                    auxbalticket.id_tipo_cosecha = 3 THEN
                    vpesodescarte = vpesodescarte + auxbalticket.peso_neto.
            END.
        END.
        ELSE
        DO:
            FOR EACH auxbalticket WHERE auxbalticket.id_balanza = AUXBAL.id_balanza AND
                auxbalticket.id_pesada = AUXBAL.id_pesada NO-LOCK:
                vpesodescarte = vpesodescarte + auxbalticket.peso_descarte.
            END.
        END.
        ASSIGN 
            AUXBAL.peso_neto     = vpesoneto
            AUXBAL.peso_descarte = vpesodescarte.
    END.
END PROCEDURE.  

PROCEDURE actsaldospartidadebalanza :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER vsuctrabajo AS INTEGER.
    DEFINE INPUT PARAMETER vpartida AS INTEGER.
    DEFINE INPUT PARAMETER vpartidaserial AS INTEGER.

    FIND FIRST balanza_tickets WHERE balanza_tickets.nro_partida = vpartida AND 
        balanza_tickets.nro_partida_serial = vpartidaserial NO-LOCK NO-ERROR.
    IF AVAILABLE balanza_tickets THEN
    DO:
        FIND FIRST balanza_pesadas OF balanza_tickets NO-LOCK NO-ERROR.
        FIND FIRST saldos_partidas WHERE saldos_partidas.id_suc_trabajo = vsuctrabajo  AND
            saldos_partidas.nro_partida = balanza_tickets.nro_partida AND 
            saldos_partidas.id_sucursal = balanza_pesada.id_sucursal NO-ERROR.
        IF NOT AVAILABLE saldos_partidas THEN
        DO:
            CREATE saldos_partidas.
            ASSIGN 
                saldos_partidas.id_suc_trabajo = vsuctrabajo 
                saldos_partidas.nro_partida    = balanza_tickets.nro_partida
                saldos_partidas.id_sucursal    = balanza_pesada.id_sucursal.
            
        END.
        ASSIGN 
            saldos_partida.ingreso = saldos_partidas.ingreso + balanza_tickets.peso_neto_ticket
            saldos_partida.saldo   = saldos_partidas.ingreso - saldos_partidas.egreso.
    END.

    /* Actualiza saldos_packing */
    FIND FIRST saldos_packing WHERE
        saldos_packing.nro_partida  = vpartida AND
        saldos_packing.nro_partida_serial = vpartidaserial NO-ERROR.
    IF NOT AVAILABLE saldos_packing THEN
    DO:
        CREATE saldos_packing.
        ASSIGN
            saldos_packing.nro_partida          = vpartida
            saldos_packing.nro_partida_serial   = vpartidaserial
            saldos_packing.id_sucursal_etiqueta = balanza_tickets.id_sucursal_etiqueta
            saldos_packing.id_etiqueta          = balanza_tickets.id_etiqueta
            saldos_packing.id_sucursal          = balanza_pesadas.id_sucursal
            saldos_packing.cantidad_total       = balanza_tickets.peso_neto_ticket / 20
            saldos_packing.saldo                = balanza_tickets.peso_neto_ticket / 20
            saldos_packing.saldo_camara         = balanza_tickets.peso_neto_ticket / 20.

        RELEASE saldos_packing.
    END.


END PROCEDURE.
FUNCTION devuelvefechaoperativa RETURNS DATE
    ( INPUT pfecha AS DATE, INPUT phora AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:   
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE vfechaop AS DATE.
  
    FIND FIRST fechas_operativas WHERE
        (fechas_operativas.fecha_inicio = pfecha AND phora >= fechas_operativas.hora_inicio) OR
        (fechas_operativas.fecha_fin = pfecha AND phora <= fechas_operativas.hora_fin)  NO-LOCK NO-ERROR.
    IF  AVAILABLE fechas_operativas THEN 
        vfechaop = fechas_operativas.fecha_inicio.



    RETURN vfechaop.
END FUNCTION.     

FUNCTION nroPesada RETURNS INTEGER (INPUT IBALANZA AS INTEGER, INPUT IPESASA AS INTEGER):
    DEFINE VARIABLE VINROPESADA AS INTEGER NO-UNDO.
    
    VINROPESADA = IBALANZA * 1000000 + IPESADA.
    OUTPUT TO D:\TEMP\WS2.TXT APPEND.
    RETURN VINROPESADA.
END FUNCTION.
 
FUNCTION WLOG RETURNS LOGICAL (INPUT ARCHIVO AS CHARACTER, INPUT TEXTO AS CHARACTER):
    OUTPUT TO VALUE(ARCHIVO) APPEND.
    EXPORT DELIMITER "|" STRING(NOW)  TEXTO.
    OUTPUT CLOSE.
    RETURN TRUE.
END FUNCTION.