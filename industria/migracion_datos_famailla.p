&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

define var sucursal_local as integer.
    define var i as integer initial 0.
    sucursal_local = 95.


/* LOTES DE JUGO */
for each famindust.lotes_jugo no-lock where famindust.lotes_jugo.id_sucursal = sucursal_local:
    
    find first general.lotes_jugo where 
       general.lotes_jugo.id_empresa      = famindust.lotes_jugo.id_empresa and
       general.lotes_jugo.id_sucursal     = famindust.lotes_jugo.id_sucursal and
       general.lotes_jugo.id_tipotambor   = famindust.lotes_jugo.id_tipotambor and
       general.lotes_jugo.nromov          = famindust.lotes_jugo.nromov no-error.
    
    if available general.lotes_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
        do:
        
        end.
    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
        do:
           create general.lotes_jugo.
           buffer-copy famindust.lotes_jugo to general.lotes_jugo.
           i = i + 1. 
           registros:screen-value in frame F-Main = string(i).
           
           /* AHORA CREO LOS TAMBORES DE ESE LOTE */
           for each famindust.tambores_industria of famindust.lotes_jugo no-lock.
                create general.tambores_industria.
                buffer-copy famindust.tambores_industria to general.tambores_industria.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES */
           for each famindust.composicion_lote where 
              famindust.composicion_lote.id_empresa = famindust.lotes_jugo.id_empresa and
              famindust.composicion_lote.id_sucursal = famindust.lotes_jugo.id_sucursal and
              famindust.composicion_lote.id_tipotambor = famindust.lotes_jugo.id_tipotambor and
              famindust.composicion_lote.nromov = famindust.lotes_jugo.nromov  no-lock.
                create general.composicion_lote.
                buffer-copy famindust.composicion_lote to general.composicion_lote.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
           /* VOY A GRABAR LAS INSPECCIONES DE LOS LOTES */
           for each famindust.inspecciones_lote of famindust.lotes_jugo no-lock.
                create general.inspecciones_lote.
                buffer-copy famindust.inspecciones_lote to general.inspecciones_lote.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
           for each famindust.sobrante of famindust.lotes_jugo no-lock.
                create general.sobrante.
                buffer-copy famindust.sobrante to general.sobrante.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                for each famindust.tambores_industria where 
                    famindust.tambores_industria.id_empresa = famindust.sobrante.id_empresa and
                    famindust.tambores_industria.id_sucursal = famindust.sobrante.id_sucursal and
                    famindust.tambores_industria.id_tipotambor = famindust.sobrante.id_tipotambor_sobrante and
                    famindust.tambores_industria.nromov = famindust.sobrante.nromov_sobrante no-lock.
                
                     create general.tambores_industria.
                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                     i = i + 1. 
                     registros:screen-value in frame F-Main = string(i).
                end.
                    
           /* PERO NO NOS OLVIDEMOS DE LOS ARRASTRE DE LOTE */
           for each famindust.arrastre_lote of famindust.lotes_jugo no-lock.
                create general.arrastre_lote.
                buffer-copy famindust.arrastre_lote to general.arrastre_lote.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
                /* TAMBIEN LOS TAMBORES DEL ARRASTRE */
                for each famindust.tambores_industria where 
                    famindust.tambores_industria.id_empresa = famindust.arrastre_lote.id_empresa and
                    famindust.tambores_industria.id_sucursal = famindust.arrastre_lote.id_sucursal and
                    famindust.tambores_industria.id_tipotambor = famindust.arrastre_lote.id_tipotambor_arrastre and
                    famindust.tambores_industria.nromov = famindust.arrastre_lote.nromov_arrastre no-lock.
                
                     create general.tambores_industria.
                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                     i = i + 1. 
                     registros:screen-value in frame F-Main = string(i).
                
                end.
        end.
end.

lista:INSERT("Lotes de Jugo - " + string(i),1).

i = 0.

/* LOTES DE ACEITE */
for each famindust.lotes_aceite no-lock where famindust.lotes_aceite.id_sucursal = sucursal_local:
    
    find first general.lotes_aceite where 
       general.lotes_aceite.id_empresa      = famindust.lotes_aceite.id_empresa and
       general.lotes_aceite.id_sucursal     = famindust.lotes_aceite.id_sucursal and
       general.lotes_aceite.id_tipotambor   = famindust.lotes_aceite.id_tipotambor and
       general.lotes_aceite.nromov          = famindust.lotes_aceite.nromov no-error.
    
    if available general.lotes_aceite then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
        do:
        
        end.
    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
        do:
           create general.lotes_aceite.
           buffer-copy famindust.lotes_aceite to general.lotes_aceite.
           i = i + 1. 
           registros:screen-value in frame F-Main = string(i). 
           
           /* AHORA CREO LOS TAMBORES DE ESE LOTE DE ACEITE*/
           for each famindust.tambores_industria of famindust.lotes_aceite no-lock.
                create general.tambores_industria.
                buffer-copy famindust.tambores_industria to general.tambores_industria.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
           /* ESTOY CREANDO LAS COMPOSICIONES DE LOS LOTES DE ACEITE */
           for each famindust.composicion_lote_aceite where 
              famindust.composicion_lote_aceite.id_empresa      = famindust.lotes_aceite.id_empresa and
              famindust.composicion_lote_aceite.id_sucursal     = famindust.lotes_aceite.id_sucursal and
              famindust.composicion_lote_aceite.id_tipotambor   = famindust.lotes_aceite.id_tipotambor and
              famindust.composicion_lote_aceite.nromov          = famindust.lotes_aceite.nromov  no-lock.
                create general.composicion_lote_aceite.
                buffer-copy famindust.composicion_lote_aceite to general.composicion_lote_aceite.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
           /* AHORA LE TOCA TRAER LOS SOBRANTE DE LOTE */
           for each famindust.sobrante_lotes_aceite of famindust.lotes_aceite no-lock.
                create general.sobrante_lotes_aceite.
                buffer-copy famindust.sobrante_lotes_aceite to general.sobrante_lotes_aceite.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
                /* TAMBIEN LOS TAMBORES DEL SOBRANTE */
                for each famindust.tambores_industria where 
                    famindust.tambores_industria.id_empresa     = famindust.sobrante_lotes_aceite.id_empresa and
                    famindust.tambores_industria.id_sucursal    = famindust.sobrante_lotes_aceite.id_sucursal and
                    famindust.tambores_industria.id_tipotambor  = famindust.sobrante_lotes_aceite.id_tipotambor_sobrante and
                    famindust.tambores_industria.nromov         = famindust.sobrante_lotes_aceite.nromov_sobrante no-lock.
                
                     create general.tambores_industria.
                     buffer-copy famindust.tambores_industria to general.tambores_industria.
                     i = i + 1. 
                     registros:screen-value in frame F-Main = string(i).
                
                end.
           
        end.
end.

lista:INSERT("Lotes de Aceite - " + string(i),2).

i = 0.
/* PRODUCCION DE JUGOS */
for each famindust.produccion_jugo no-lock where famindust.produccion_jugo.id_sucursal = sucursal_local:
    
    find first general.produccion_jugo where 
       general.produccion_jugo.id_empresa      = famindust.produccion_jugo.id_empresa and
       general.produccion_jugo.id_sucursal     = famindust.produccion_jugo.id_sucursal and
       general.produccion_jugo.id_tipotambor   = famindust.produccion_jugo.id_tipotambor and
       general.produccion_jugo.nromov          = famindust.produccion_jugo.nromov no-error.
    
    if available general.produccion_jugo then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
        do:
        
        end.
    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
        do:
           create general.produccion_jugo.
           buffer-copy famindust.produccion_jugo to general.produccion_jugo.
           i = i + 1. 
           registros:screen-value in frame F-Main = string(i). 
           
           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
           for each famindust.tambores_industria of famindust.produccion_jugo no-lock.
                create general.tambores_industria.
                buffer-copy famindust.tambores_industria to general.tambores_industria.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
        end.
end.

lista:INSERT("Producción de Jugo - " + string(i),3).

i = 0.

/* PRODUCCION DE ACEITE */
for each famindust.tambores_industria where famindust.tambores_industria.id_tipotambor = 2 and
         famindust.tambores_industria where famindust.tambores_industria.id_sucursal = 95 no-lock.
    find first general.tambores_industria where 
        general.tambores_industria.id_empresa      = famindust.tambores_industria.id_empresa and
        general.tambores_industria.id_sucursal     = famindust.tambores_industria.id_sucursal and
        general.tambores_industria.id_tipotambor   = famindust.tambores_industria.id_tipotambor and
        general.tambores_industria.nromov          = famindust.tambores_industria.nromov and
        general.tambores_industria.id_tambor       = famindust.tambores_industria.id_tambor no-error.    
    
    if available general.tambores_industria then
        do:
        
        end.
    else
        do: /* NO EXISTE ENTONCES LO MIGRO */
            create general.tambores_industria.
            buffer-copy famindust.tambores_industria to general.tambores_industria.
            i = i + 1. 
            registros:screen-value in frame F-Main = string(i).
        end.
end.

lista:INSERT("Producción de Aceite - " + string(i),4).

i = 0.

/* PRODUCTOS DE TERCEROS */
for each famindust.productos_terceros no-lock where famindust.productos_terceros.id_sucursal = sucursal_local:
    
    find first general.productos_terceros where 
       general.productos_terceros.id_empresa      = famindust.productos_terceros.id_empresa and
       general.productos_terceros.id_sucursal     = famindust.productos_terceros.id_sucursal and
       general.productos_terceros.id_proveedor    = famindust.productos_terceros.id_proveedor and
       general.productos_terceros.id_articulo     = famindust.productos_terceros.id_articulo and
       general.productos_terceros.id_tipotambor   = famindust.productos_terceros.id_tipotambor and
       general.productos_terceros.nromov          = famindust.productos_terceros.nromov no-error.
    
    if available general.productos_terceros then /* SI LO ENCONTRO POR LO TANTO REBIZO SI NO LO MODIFICO */
        do:
        
        end.
    else    /* NO LO ENCONTRO POR LO TANTO LO CREO */
        do:
           create general.productos_terceros.
           buffer-copy famindust.productos_terceros to general.productos_terceros.
           i = i + 1. 
           registros:screen-value in frame F-Main = string(i). 
           
           /* AHORA CREO LOS TAMBORES DE ESA PRODUCCION DE JUGO*/
           for each famindust.tambores_industria of famindust.productos_terceros no-lock.
                create general.tambores_industria.
                buffer-copy famindust.tambores_industria to general.tambores_industria.
                i = i + 1. 
                registros:screen-value in frame F-Main = string(i).
           end.
           
        end.
end.

lista:INSERT("Productos de Terceros - " + string(i),5).
lista:insert("                     ",6).
lista:INSERT("Se termino el proceso de ",7).
lista:INSERT("    Importacion!!!!!",8).
lista:bgcolor = 8.
disable lista.

message "Se terminó la importación de datos de Famailla" view-as alert-box.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


