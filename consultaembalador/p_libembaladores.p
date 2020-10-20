&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Method-Library 
/*------------------------------------------------------------------------
    Library     : 
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



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Method-Library
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Method-Library ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Method-Library 
/* ************************* Included-Libraries *********************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Method-Library 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-cajasPorEmbalador) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cajasPorEmbalador Method-Library 
PROCEDURE cajasPorEmbalador :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER PSUC AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PPAC AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PEMB AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER PDT1 AS DATETIME NO-UNDO.
DEFINE INPUT PARAMETER PDT2 AS DATETIME NO-UNDO.

DEFINE OUTPUT PARAMETER PCAJMEN10E  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJMEN10   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJMEN20E  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJMEN20   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJMAY20E  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJMAY20   AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER PCAJTOT     AS INTEGER NO-UNDO.


DEFINE VAR ICAJMEN10E AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20E AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20E AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN10 AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20 AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20 AS INTEGER NO-UNDO.
DEFINE VAR ICAJTOT   AS INTEGER NO-UNDO.

ASSIGN 
    ICAJMEN10E = 0
    ICAJMEN10  = 0
    ICAJMEN20E = 0
    ICAJMEN20  = 0
    ICAJMAY20E = 0
    ICAJMAY20  = 0
    ICAJTOT    = 0.



FOR EACH cajas WHERE    cajas.id_suc_trabajo = PSUC AND
                        cajas.creacion  >= Pdt1 AND cajas.creacion <= Pdt2 AND 
                        ( lectura  <> ? OR hora_lectura_pallet <> ? OR lectura_stock_piso <> ?) AND 
                        cajas.id_embalador      = PEMB NO-LOCK .

   IF cajas.id_packing <> ppac AND ppac <> 0 THEN NEXT.

    IF cajas.peso_nominal < 10 AND cajas.peso_nominal <> 0 THEN
    DO:
        IF cajas.empapelado THEN
            ASSIGN ICAJMEN10E = ICAJMEN10E + 1.
        ELSE
            ASSIGN ICAJMEN10 = ICAJMEN10 + 1.
    END.
    ELSE                 
        IF cajas.peso_nominal >= 10 AND cajas.peso_nominal < 20 THEN
        DO:
            IF cajas.empapelado THEN
              ASSIGN ICAJMEN20E = ICAJMEN20E + 1.
            ELSE
              ASSIGN ICAJMEN20 = ICAJMEN20 + 1.
        END.
        ELSE
        DO:
            IF cajas.peso_nominal >= 20 THEN
              ASSIGN ICAJMAY20E = ICAJMAY20E + 1.
            ELSE
              ASSIGN ICAJMAY20 = ICAJMAY20 + 1.
        END.

        ICAJTOT = ICAJTOT + 1.
END.


    ASSIGN  PCAJMEN10E      = ICAJMEN10E
            PCAJMEN10       = ICAJMEN10
            PCAJMEN20E      = ICAJMEN20E
            PCAJMEN20       = ICAJMEN20
            PCAJMAY20E      = ICAJMAY20E
            PCAJMAY20       = ICAJMAY20
            PCAJTOT         = ICAJTOT.


RETURN.

CATCH MYERR AS Progress.Lang.AppError.
    RETURN ERROR MYERR:RETURNVALUE.
END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cajasPorEmbaladorFecha) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cajasPorEmbaladorFecha Method-Library 
PROCEDURE cajasPorEmbaladorFecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT    PARAMETER PSUC          AS INTEGER NO-UNDO.
DEFINE INPUT    PARAMETER PPAC          AS INTEGER NO-UNDO.
DEFINE INPUT    PARAMETER PEMB          AS INTEGER NO-UNDO.
DEFINE INPUT    PARAMETER PFEC          AS DATE NO-UNDO.


DEFINE OUTPUT   PARAMETER PCAJMEN10E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN10     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN20E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN20     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMAY20E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMAY20     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJTOT       AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PDT1          AS DATETIME NO-UNDO.
DEFINE OUTPUT   PARAMETER PDT2          AS DATETIME NO-UNDO.



DEFINE VAR ICAJMEN10E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN10     AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20     AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20     AS INTEGER NO-UNDO.
DEFINE VAR ICAJTOT       AS INTEGER NO-UNDO.


DEFINE VAR DT1 AS DATETIME NO-UNDO.
DEFINE VAR DT2 AS DATETIME NO-UNDO.

DEFINE VAR DT4 AS DATETIME NO-UNDO.






DEFINE VARIABLE hour     AS INTEGER NO-UNDO.
DEFINE VARIABLE minute   AS INTEGER NO-UNDO.
DEFINE VARIABLE sec      AS INTEGER NO-UNDO.
DEFINE VARIABLE timeleft AS INTEGER NO-UNDO. 

DEFINE VAR DDESDE        AS DATE NO-UNDO.


/*
timeleft = (24 * 60 * 60) - TIME.   /* seconds till next midnight */
sec = timeleft MOD 60.
timeleft = (timeleft - sec) / 60.   /* minutes till next midnight */
minute = timeleft MOD 60. /* hours till next midnight */
hour = (timeleft - minute) / 60.
*/
HOUR = MTIME / 3600000 .

DDESDE = PFEC.   
    

DT1 = DATETIME( MONTH(ddesde) , DAY(ddesde) ,  YEAR (ddesde)  , 6  , 00).
DT2 = DATETIME( MONTH(ddesde + 1) , DAY(ddesde + 1) ,  YEAR (ddesde + 1)  , 5  , 59 , 59).


ASSIGN 
    ICAJMEN10E = 0
    ICAJMEN10  = 0
    ICAJMEN20E = 0
    ICAJMEN20  = 0
    ICAJMAY20E = 0
    ICAJMAY20  = 0
    ICAJTOT    = 0.

RUN cajasPorEmbalador 
(   psuc , 
    ppac, 
    pemb  , 
    DT1 , 
    DT2  , 
    OUTPUT icajmen10e , 
    OUTPUT icajmen10, 
    OUTPUT icajmen20e , 
    OUTPUT icajmen20 , 
    OUTPUT icajmay20e ,
    OUTPUT icajmay20 ,
    OUTPUT icajtot ).    

    ASSIGN  PCAJMEN10E      = ICAJMEN10E
            PCAJMEN10       = ICAJMEN10
            PCAJMEN20E      = ICAJMEN20E
            PCAJMEN20       = ICAJMEN20
            PCAJMAY20E      = ICAJMAY20E
            PCAJMAY20       = ICAJMAY20
            PCAJTOT         = ICAJTOT
            PDT1            = DT1
            PDT2            = DT2.


 RETURN.

CATCH MYERR AS Progress.Lang.AppError.
    RETURN MYERR:RETURNVALUE.
END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cajasPorEmbaladorHoy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cajasPorEmbaladorHoy Method-Library 
PROCEDURE cajasPorEmbaladorHoy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT    PARAMETER PSUC          AS INTEGER NO-UNDO.
DEFINE INPUT    PARAMETER PPAC          AS INTEGER NO-UNDO.
DEFINE INPUT    PARAMETER PEMB          AS INTEGER NO-UNDO.

DEFINE OUTPUT   PARAMETER PCAJMEN10E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN10     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN20E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMEN20     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMAY20E    AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJMAY20     AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PCAJTOT       AS INTEGER NO-UNDO.
DEFINE OUTPUT   PARAMETER PDT1          AS DATETIME NO-UNDO.
DEFINE OUTPUT   PARAMETER PDT2          AS DATETIME NO-UNDO.



DEFINE VAR ICAJMEN10E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN10     AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMEN20     AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20E    AS INTEGER NO-UNDO.
DEFINE VAR ICAJMAY20     AS INTEGER NO-UNDO.
DEFINE VAR ICAJTOT       AS INTEGER NO-UNDO.


DEFINE VAR DT1 AS DATETIME NO-UNDO.
DEFINE VAR DT2 AS DATETIME NO-UNDO.

DEFINE VAR DT4 AS DATETIME NO-UNDO.






DEFINE VARIABLE hour     AS INTEGER NO-UNDO.
DEFINE VARIABLE minute   AS INTEGER NO-UNDO.
DEFINE VARIABLE sec      AS INTEGER NO-UNDO.
DEFINE VARIABLE timeleft AS INTEGER NO-UNDO. 

DEFINE VAR DDESDE        AS DATE NO-UNDO.


/*
timeleft = (24 * 60 * 60) - TIME.   /* seconds till next midnight */
sec = timeleft MOD 60.
timeleft = (timeleft - sec) / 60.   /* minutes till next midnight */
minute = timeleft MOD 60. /* hours till next midnight */
hour = (timeleft - minute) / 60.
*/
HOUR = MTIME / 3600000 .

IF HOUR < 6 THEN
   DDESDE = DATE(MONTH (TODAY - 1) , DAY(TODAY - 1) , YEAR(TODAY - 1 )).
ELSE
    DDESDE = TODAY.   
    

DT1 = DATETIME( MONTH(ddesde) , DAY(ddesde) ,  YEAR (ddesde)  , 6  , 00).
DT2 = DATETIME( MONTH(ddesde + 1) , DAY(ddesde + 1) ,  YEAR (ddesde + 1)  , 5  , 59 , 59).


ASSIGN 
    ICAJMEN10E = 0
    ICAJMEN10  = 0
    ICAJMEN20E = 0
    ICAJMEN20  = 0
    ICAJMAY20E = 0
    ICAJMAY20  = 0
    ICAJTOT    = 0.

RUN cajasPorEmbalador 
(   psuc , 
    ppac, 
    pemb  , 
    DT1 , 
    DT2  , 
    OUTPUT icajmen10e , 
    OUTPUT icajmen10, 
    OUTPUT icajmen20e , 
    OUTPUT icajmen20 , 
    OUTPUT icajmay20e ,
    OUTPUT icajmay20 ,
    OUTPUT icajtot ).    

    ASSIGN  PCAJMEN10E      = ICAJMEN10E
            PCAJMEN10       = ICAJMEN10
            PCAJMEN20E      = ICAJMEN20E
            PCAJMEN20       = ICAJMEN20
            PCAJMAY20E      = ICAJMAY20E
            PCAJMAY20       = ICAJMAY20
            PCAJTOT         = ICAJTOT
            PDT1            = DT1
            PDT2            = DT2.


 RETURN.

CATCH MYERR AS Progress.Lang.AppError.
    RETURN MYERR:RETURNVALUE.
END CATCH.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

