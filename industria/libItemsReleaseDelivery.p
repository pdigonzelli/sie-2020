&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getLoteDeposito) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLoteDeposito Procedure 
PROCEDURE getLoteDeposito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piIdEmpresa    AS INTEGER. 
DEFINE INPUT  PARAMETER piIdSucursal   AS INTEGER. 
DEFINE INPUT  PARAMETER piIdTipoTambor AS INTEGER. 
DEFINE INPUT  PARAMETER piNroMov       AS INTEGER.
DEFINE OUTPUT PARAMETER pcReturn       AS CHARACTER INITIAL "".

FIND FIRST item_ingreso_lote_ubicacion WHERE ITEM_ingreso_lote_ubicacion.id_empresa    = piIdEmpresa
                                         AND ITEM_ingreso_lote_ubicacion.id_sucursal   = piIdSucursal
                                         AND ITEM_ingreso_lote_ubicacion.id_tipotambor = piIdTipoTambor
                                         AND ITEM_ingreso_lote_ubicacion.nromov        = piNroMov
                                       NO-LOCK NO-ERROR.
IF AVAILABLE ITEM_ingreso_lote_ubicacion THEN DO:
  pcReturn = ITEM_ingreso_lote_ubicacion.id_lote_deposito.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPackingList Procedure 
PROCEDURE getPackingList :
/*------------------------------------------------------------------------------
  Purpose:  devuelve el id_vapor a partir la clave de lote
    Notes:  devuelve una lista con id_packing_list, 
                                   item_packing_list, 
                                   id_vapor,
                                   nro_contenedor, 
                                   nro_pack_list delimitada por ","
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER piIdEmpresa    AS INTEGER. 
DEFINE INPUT  PARAMETER piIdSucursal   AS INTEGER. 
DEFINE INPUT  PARAMETER piIdTipoTambor AS INTEGER. 
DEFINE INPUT  PARAMETER piNroMov       AS INTEGER.
DEFINE OUTPUT PARAMETER pcReturn       AS CHARACTER INITIAL ",,,,".

DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO INITIAL ",,,,".

FOR FIRST tambores_industria WHERE tambores_industria.id_empresa    = piIdEmpresa
                               AND tambores_industria.id_sucursal   = piIdSucursal 
                               AND tambores_industria.id_tipotambor = piIdTipoTambor
                               AND tambores_industria.nromov        = piNroMov
                             NO-LOCK.
  FOR FIRST items_packing_list WHERE items_packing_list.id_sucursal_remito = tambores_industria.id_sucursal_remito
                                 AND items_packing_list.id_tipo_movsto     = tambores_industria.id_tipo_movsto
                                 AND items_packing_list.nro                = tambores_industria.nro_remito
                                 AND items_packing_list.ITEM_remito        = tambores_industria.ITEM_factura
                               NO-LOCK.  
    FOR FIRST packing_list OF items_packing_list NO-LOCK.     
      ENTRY(1, cRet) = STRING(packing_list.id_packing_list).
      ENTRY(2, cRet) = STRING(items_packing_list.ITEM).
      ENTRY(3, cRet) = STRING(packing_list.id_vapor).
      ENTRY(4, cRet) = items_packing_list.nro_contenedor.
      ENTRY(5, cRet) = packing_list.nro_pack_list.
      /*DISP packing_list.nro_pack_list nro_contenedor id_vapor.*/
    END.
  END.
END.
 
pcReturn = cRet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

