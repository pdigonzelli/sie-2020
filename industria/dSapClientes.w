&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          prog2sap         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
DEFINE VARIABLE h_asindustria              AS HANDLE          NO-UNDO.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{adm2/support/customColors.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF

&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BATCH_INPUT_CLIENTES_SAP

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  ABLAD AKONT ALTKN ANRED AUFSD BANKL BANKN BANKS BKONT BKREF BRSCH BUKRS~
 BVTYP BZIRK COUNC FDGRV FITYP GRICD GRIDT INCO1 INCO2 KALKS KDGRP KKBER~
 KNA1-STKZN KNB1-XZVER KNB1-ZWELS KNFAK KNVI-TAXKD KOINH KONDA KTGRD KTOKD~
 KUKLA KUNNR KZAZU LAND1 LIFNR LPRIO LZONE NAME1 NAME2 NAME3 NAME4 ORT01~
 PLTYP PSTLZ REGIO SORT1 SORT2 SPART STCD1 STCD2 STCDT STRAS TELF1 TELFX~
 VERSG VKBUR VKGRP VKORG VSBED VTWEG VWERK WAERS XEZER ZTERM
&Scoped-define ENABLED-FIELDS-IN-BATCH_INPUT_CLIENTES_SAP ABLAD AKONT ALTKN ~
ANRED AUFSD BANKL BANKN BANKS BKONT BKREF BRSCH BUKRS BVTYP BZIRK COUNC ~
FDGRV FITYP GRICD GRIDT INCO1 INCO2 KALKS KDGRP KKBER KNA1-STKZN KNB1-XZVER ~
KNB1-ZWELS KNFAK KNVI-TAXKD KOINH KONDA KTGRD KTOKD KUKLA KUNNR KZAZU LAND1 ~
LIFNR LPRIO LZONE NAME1 NAME2 NAME3 NAME4 ORT01 PLTYP PSTLZ REGIO SORT1 ~
SORT2 SPART STCD1 STCD2 STCDT STRAS TELF1 TELFX VERSG VKBUR VKGRP VKORG ~
VSBED VTWEG VWERK WAERS XEZER ZTERM 
&Scoped-Define DATA-FIELDS  ABLAD AKONT ALTKN ANRED AUFSD BANKL BANKN BANKS BKONT BKREF BRSCH BUKRS~
 BVTYP BZIRK COUNC FDGRV FITYP GRICD GRIDT INCO1 INCO2 KALKS KDGRP KKBER~
 KNA1-STKZN KNB1-XZVER KNB1-ZWELS KNFAK KNVI-TAXKD KOINH KONDA KTGRD KTOKD~
 KUKLA KUNNR KZAZU LAND1 LIFNR LPRIO LZONE NAME1 NAME2 NAME3 NAME4 ORT01~
 PLTYP PSTLZ REGIO SORT1 SORT2 SPART STCD1 STCD2 STCDT STRAS TELF1 TELFX~
 VERSG VKBUR VKGRP VKORG VSBED VTWEG VWERK WAERS XEZER ZTERM
&Scoped-define DATA-FIELDS-IN-BATCH_INPUT_CLIENTES_SAP ABLAD AKONT ALTKN ~
ANRED AUFSD BANKL BANKN BANKS BKONT BKREF BRSCH BUKRS BVTYP BZIRK COUNC ~
FDGRV FITYP GRICD GRIDT INCO1 INCO2 KALKS KDGRP KKBER KNA1-STKZN KNB1-XZVER ~
KNB1-ZWELS KNFAK KNVI-TAXKD KOINH KONDA KTGRD KTOKD KUKLA KUNNR KZAZU LAND1 ~
LIFNR LPRIO LZONE NAME1 NAME2 NAME3 NAME4 ORT01 PLTYP PSTLZ REGIO SORT1 ~
SORT2 SPART STCD1 STCD2 STCDT STRAS TELF1 TELFX VERSG VKBUR VKGRP VKORG ~
VSBED VTWEG VWERK WAERS XEZER ZTERM 
&Scoped-Define MANDATORY-FIELDS 
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST 
&Scoped-Define DATA-FIELD-DEFS "dSapClientes.i"
&Scoped-define QUERY-STRING-Query-Main FOR EACH BATCH_INPUT_CLIENTES_SAP NO-LOCK ~
    BY BATCH_INPUT_CLIENTES_SAP.KUNNR INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH BATCH_INPUT_CLIENTES_SAP NO-LOCK ~
    BY BATCH_INPUT_CLIENTES_SAP.KUNNR INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main BATCH_INPUT_CLIENTES_SAP
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main BATCH_INPUT_CLIENTES_SAP


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      BATCH_INPUT_CLIENTES_SAP SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "prog2sap.BATCH_INPUT_CLIENTES_SAP"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "prog2sap.BATCH_INPUT_CLIENTES_SAP.KUNNR|yes"
     _FldNameList[1]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.ABLAD
"ABLAD" "ABLAD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[2]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.AKONT
"AKONT" "AKONT" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[3]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.ALTKN
"ALTKN" "ALTKN" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[4]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.ANRED
"ANRED" "ANRED" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[5]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.AUFSD
"AUFSD" "AUFSD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[6]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BANKL
"BANKL" "BANKL" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[7]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BANKN
"BANKN" "BANKN" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[8]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BANKS
"BANKS" "BANKS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[9]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BKONT
"BKONT" "BKONT" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[10]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BKREF
"BKREF" "BKREF" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[11]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BRSCH
"BRSCH" "BRSCH" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[12]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BUKRS
"BUKRS" "BUKRS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[13]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BVTYP
"BVTYP" "BVTYP" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[14]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.BZIRK
"BZIRK" "BZIRK" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[15]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.COUNC
"COUNC" "COUNC" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[16]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.FDGRV
"FDGRV" "FDGRV" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[17]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.FITYP
"FITYP" "FITYP" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[18]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.GRICD
"GRICD" "GRICD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[19]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.GRIDT
"GRIDT" "GRIDT" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[20]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.INCO1
"INCO1" "INCO1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[21]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.INCO2
"INCO2" "INCO2" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[22]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KALKS
"KALKS" "KALKS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[23]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KDGRP
"KDGRP" "KDGRP" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[24]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KKBER
"KKBER" "KKBER" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[25]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KNA1-STKZN
"KNA1-STKZN" "KNA1-STKZN" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[26]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KNB1-XZVER
"KNB1-XZVER" "KNB1-XZVER" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[27]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KNB1-ZWELS
"KNB1-ZWELS" "KNB1-ZWELS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[28]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KNFAK
"KNFAK" "KNFAK" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[29]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KNVI-TAXKD
"KNVI-TAXKD" "KNVI-TAXKD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[30]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KOINH
"KOINH" "KOINH" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[31]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KONDA
"KONDA" "KONDA" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[32]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KTGRD
"KTGRD" "KTGRD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[33]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KTOKD
"KTOKD" "KTOKD" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[34]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KUKLA
"KUKLA" "KUKLA" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[35]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KUNNR
"KUNNR" "KUNNR" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[36]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.KZAZU
"KZAZU" "KZAZU" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[37]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.LAND1
"LAND1" "LAND1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[38]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.LIFNR
"LIFNR" "LIFNR" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[39]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.LPRIO
"LPRIO" "LPRIO" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[40]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.LZONE
"LZONE" "LZONE" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[41]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.NAME1
"NAME1" "NAME1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[42]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.NAME2
"NAME2" "NAME2" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[43]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.NAME3
"NAME3" "NAME3" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[44]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.NAME4
"NAME4" "NAME4" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[45]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.ORT01
"ORT01" "ORT01" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[46]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.PLTYP
"PLTYP" "PLTYP" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[47]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.PSTLZ
"PSTLZ" "PSTLZ" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[48]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.REGIO
"REGIO" "REGIO" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[49]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.SORT1
"SORT1" "SORT1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[50]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.SORT2
"SORT2" "SORT2" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[51]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.SPART
"SPART" "SPART" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[52]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.STCD1
"STCD1" "STCD1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[53]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.STCD2
"STCD2" "STCD2" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[54]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.STCDT
"STCDT" "STCDT" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[55]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.STRAS
"STRAS" "STRAS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[56]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.TELF1
"TELF1" "TELF1" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[57]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.TELFX
"TELFX" "TELFX" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[58]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VERSG
"VERSG" "VERSG" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[59]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VKBUR
"VKBUR" "VKBUR" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[60]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VKGRP
"VKGRP" "VKGRP" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[61]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VKORG
"VKORG" "VKORG" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[62]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VSBED
"VSBED" "VSBED" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[63]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VTWEG
"VTWEG" "VTWEG" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[64]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.VWERK
"VWERK" "VWERK" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[65]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.WAERS
"WAERS" "WAERS" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[66]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.XEZER
"XEZER" "XEZER" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _FldNameList[67]   > prog2sap.BATCH_INPUT_CLIENTES_SAP.ZTERM
"ZTERM" "ZTERM" ? ? "character" ? ? ? ? ? ? yes ? no 100 yes
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beginTransactionValidate dTables  _DB-REQUIRED
PROCEDURE beginTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endTransactionValidate dTables  _DB-REQUIRED
PROCEDURE endTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPosUpdate dTables  _DB-REQUIRED
PROCEDURE especialPosUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER  xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreCreate dTables  _DB-REQUIRED
PROCEDURE especialPreCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xCase AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE especialPreUpdate dTables  _DB-REQUIRED
PROCEDURE especialPreUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER xCase   AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fieldsWithProblems dTables  _DB-REQUIRED
PROCEDURE fieldsWithProblems :
/*------------------------------------------------------------------------------
  Purpose: In this procedure we check all conditions of BUssines Logic that the
  RowObject can give us.Returns a comma separated list in wich each element is a
  pair of field + chr(3)+  color we want to mark as irregular ones
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cList AS CHARACTER NO-UNDO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getField dTables  _DB-REQUIRED
PROCEDURE getField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER xFieldName AS CHARACTER NO-UNDO.

DEFINE VAR hBuffer AS HANDLE NO-UNDO.
DEFINE VAR hField  AS HANDLE NO-UNDO.

hBuffer = BUFFER RowObject:HANDLE.
IF VALID-HANDLE(hBuffer) THEN
DO:
    hField = hBuffer:BUFFER-FIELD(xFieldName).
    IF VALID-HANDLE(hField) THEN
    DO:
        RETURN hField:BUFFER-VALUE().
    END.
END.

RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/



  /* Code placed here will execute PRIOR to standard behavior. */
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  /*
  DEFINE VAR xDataSource AS CHARACTER NO-UNDO.
  {get DataSource xDataSource}.
  IF xDataSource <> ? THEN
  DO:
      {set AutoCommit NO}.
      {set CommitSource xDataSource}.
  END.
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postTransactionValidate dTables  _DB-REQUIRED
PROCEDURE postTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FIND LAST RowObjUpd NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postUpdate dTables  _DB-REQUIRED
PROCEDURE postUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer   AS HANDLE NO-UNDO.
DEFINE VAR hField                AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preCreate dTables  _DB-REQUIRED
PROCEDURE preCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preTransactionValidate dTables  _DB-REQUIRED
PROCEDURE preTransactionValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

{&DB-REQUIRED-START}

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE preUpdate dTables  _DB-REQUIRED
PROCEDURE preUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER hBuffer AS HANDLE NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

{&DB-REQUIRED-END}

