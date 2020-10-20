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

define input parameter etiqueta as char. /* PARAMETRO DE ENTRADA  */
define output parameter v_etiqueta as char. /* PARAMETRO DE SALIDA */

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


    define var emp as integer.
    define var suc as integer.
    define var lot as integer.
    define var tam as integer.
    define var tip as integer.
    define var art as integer.
    define var ind as integer.
    v_etiqueta = "".
    
    if LENGTH(etiqueta, "CHARACTER") <= 7 then  
       do:  /************************************************/
            /*ACA ENTRA CUANDO SE INGRESA A MANO LA ETIQUETA*/
            /************************************************/
            v_etiqueta = etiqueta.
            /* message v_etiqueta. */
       end.
    else 
        do:
            if LENGTH(etiqueta, "CHARACTER") < 11 then 
                do:
                    /***************************************/
                    /*ACA ENTRA CUANDO SE LEE CON LA LECTORA LAS ETIQUETAS NUEVAS CON SOLO EL ID_ETIQUETA */
                    /***************************************/
                    v_etiqueta = substr(etiqueta,4,7).
                    /* message v_etiqueta. */
                end.
            else 
                do:
                    /**************************************/
                    /*ACA SE ENTRA CUANDO SE LEE UNA ETIQUETA VIEJA CON EMP SUC LOT TAM ETC ETC*/
                    /**************************************/
                    tip = integer(substr(etiqueta,2,2)).
                    if tip = 2 then
                        do:                                     /*ETIQUETAS DE PRODUCCION DE ACEITE*/
                            emp = integer(substr(etiqueta,4,1)).
                            suc = integer(substr(etiqueta,5,3)).
                            art = integer(substr(etiqueta,8,3)).
                            tam = integer(substr(etiqueta,11,5)).
                            /* message emp suc art tam tip view-as alert-box. */
                    
                            find tambores_industria where id_empresa = emp 
                                                      and id_sucursal = suc
                                                      and id_articulo = art
                                                      and id_tambor = tam
                                                      and id_tipotambor = tip no-lock no-error.

                            if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                            /* message v_etiqueta. */
                        end.
                    else
                        do:
                            if tip <> 9 then
                              do:                        /*ETIQUETAS DE PRODUCTOS DE TERCEROS*/
                                emp = integer(substr(etiqueta,4,1)).
                                suc = integer(substr(etiqueta,5,3)).
                                lot = integer(substr(etiqueta,8,5)).
                                tam = integer(substr(etiqueta,13,3)).
                                /* message emp suc lot tam tip view-as alert-box. */
                        
                                find tambores_industria where id_empresa = emp 
                                                          and id_sucursal = suc
                                                          and id_lote = lot
                                                          and id_tambor = tam
                                                          and id_tipotambor = tip no-lock no-error.

                                if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                                /* message v_etiqueta.     */
                              end.
                            else
                              do:                                    /*TODAS LAS DEMAS ETIQUETAS*/
                                emp = integer(substr(etiqueta,4,1)).
                                suc = integer(substr(etiqueta,5,3)).
                                ind = integer(substr(etiqueta,8,7)).
                                /* message emp suc ind tip view-as alert-box. */
                        
                                find tambores_industria where id_empresa = emp 
                                                          and id_sucursal = suc
                                                          and indice_tambor = ind
                                                          and id_tipotambor = tip no-lock no-error.

                                if available tambores_industria then v_etiqueta = string(tambores_industria.id_etiqueta).
                                /* message v_etiqueta. */
                              end.
                              
                        end.
                            
                    
                end.
       end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


