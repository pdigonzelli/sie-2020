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



&SCOPED-DEFINE WSADESCRIPTION_LEN       256
&SCOPED-DEFINE WSASYS_STATUS_LEN        128
 
&SCOPED-DEFINE WSADATA_VERSION_LOW        1    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_VERSION_HIGH       3    /* WORD(2)  */
&SCOPED-DEFINE WSADATA_DESCRIPTION        5    /* CHAR(WSADESCRIPTION_LEN + 1) */ 
&SCOPED-DEFINE WSADATA_SYSTEM_STATUS    262    /* CHAR(WSASYS_STATUS_LEN + 1)  */ 
&SCOPED-DEFINE WSADATA_MAX_SOCKETS      391    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_MAX_UDP          395    /* SHORT(4) */ 
&SCOPED-DEFINE WSADATA_VENDOR_INFO      399    /* CHAR*(4) */ 
&SCOPED-DEFINE WSADATA_LENGTH           403   
 
&SCOPED-DEFINE HOSTENT_NAME               1    /* CHAR*(4)  */
&SCOPED-DEFINE HOSTENT_ALIASES            5    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_ADDR_TYPE          9    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LENGTH       11    /* SHORT(2)  */ 
&SCOPED-DEFINE HOSTENT_ADDR_LIST         13    /* CHAR**(4) */ 
&SCOPED-DEFINE HOSTENT_LENGTH            16

/*by facundo 20/11/2006*/

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
  DEFINE OUTPUT       PARAMETER p-Hostname      AS CHARACTER.
  DEFINE INPUT        PARAMETER p-Length        AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE gethostbyname EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-Name          AS CHARACTER.
  DEFINE RETURN       PARAMETER p-Hostent       AS LONG.
END PROCEDURE.
 
PROCEDURE inet_ntoa EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-AddrStruct    AS LONG.
  DEFINE RETURN       PARAMETER p-AddrString    AS MEMPTR.
END PROCEDURE.
 
PROCEDURE WSAStartup EXTERNAL "wsock32.dll" :
  DEFINE INPUT        PARAMETER p-VersionReq    AS SHORT.
  DEFINE INPUT        PARAMETER ptr-WsaData     AS LONG.
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.
 
PROCEDURE WSACleanup EXTERNAL "wsock32":
  DEFINE RETURN       PARAMETER p-Return        AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getHost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHost Procedure 
FUNCTION getHost RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getIp Procedure 
FUNCTION getIp RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoggedUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLoggedUser Procedure 
FUNCTION getLoggedUser RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{prowinapi/windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-i-GetTcpInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE i-GetTcpInfo Procedure 
PROCEDURE i-GetTcpInfo PRIVATE :
/*------------------------------------------------------------------------
  Procedure   : i-GetTcpInfo
 
  Description : Return the windows TCP host name and address of this PC.
 
  Parms       : - Host name. (OUTPUT, CHARACTER)
                - Host address. (OUTPUT, CHARACTER):
 
  Sample usage: RUN i-GetTcpInfo (OUTPUT w-TcpName,
                                  OUTPUT w-TcpAddr).
 
  Notes       : -
------------------------------------------------------------------------*/

  DEFINE OUTPUT PARAMETER p-TcpName      AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER p-TcpAddr      AS CHARACTER NO-UNDO.
 
  DEFINE VARIABLE         w-TcpName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE         w-Length       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         w-Return       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         ptr-WsaData    AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         w-Hostent      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE         ptr-Hostent    AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-AddrString AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-AddrList   AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         ptr-ListEntry  AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE         w-TcpLong      AS INTEGER   NO-UNDO.
 
  /* Initialize return values */
  ASSIGN p-TcpName = ?
         p-TcpAddr = ?
         .
 
  /* Allocate work structure for WSADATA */
  SET-SIZE(ptr-WsaData) = {&WSADATA_LENGTH}.
 
  /* Ask Win32 for winsock usage */
  RUN WSAStartup (INPUT  257,        /* requested version 1.1 */
                  INPUT  GET-POINTER-VALUE(ptr-WsaData),
                  OUTPUT w-Return).
 
  /* Release allocated memory */
  SET-SIZE(ptr-WsaData) = 0.
 
  /* Check for errors */
  IF w-Return NE 0 THEN DO:
    MESSAGE "Error accessing WINSOCK support." VIEW-AS ALERT-BOX.
    RETURN.
  END.
 
  /* Set up variables */
  ASSIGN w-Length  = 100
         w-TcpName = FILL(" ", w-Length)
         .
 
  /* Call Win32 routine to get host name */
  RUN gethostname (OUTPUT w-TcpName,
                   INPUT  w-Length,
                   OUTPUT w-Return).
 
  /* Check for errors */
  IF w-Return NE 0 THEN DO:
    MESSAGE "Error getting tcp name." VIEW-AS ALERT-BOX.
    RUN WSACleanup (OUTPUT w-Return).
    RETURN.
  END.
 
  /* Pass back gathered info */
  /* remember: the string is null-terminated so there is a CHR(0)
               inside w-TcpName. We have to trim it:  */
  p-TcpName = ENTRY(1,w-TcpName,CHR(0)).
 
  /* Call Win32 routine to get host address */
  RUN gethostbyname (INPUT  w-TcpName,
                     OUTPUT w-Hostent).
 
  /* Check for errors */
  IF w-Hostent EQ 0 THEN DO:
    MESSAGE "Error resolving host name." VIEW-AS ALERT-BOX.
    RUN WSACleanup (OUTPUT w-Return).
    RETURN.
  END.
 
  /* Set pointer to HostEnt data structure */
  SET-POINTER-VALUE(ptr-Hostent) = w-Hostent.
 
  /* "Chase" pointers to get to first address list entry */
  SET-POINTER-VALUE(ptr-AddrList)  = GET-LONG(ptr-Hostent, 
                                              {&HOSTENT_ADDR_LIST}).
  SET-POINTER-VALUE(ptr-ListEntry) = GET-LONG(ptr-AddrList, 1).
  w-TcpLong                        = GET-LONG(ptr-ListEntry, 1).
 
  RUN inet_ntoa (INPUT  w-TcpLong,
                 OUTPUT ptr-AddrString).
 
  /* Pass back gathered info */
  p-TcpAddr = GET-STRING(ptr-AddrString, 1).
 
  /* Terminate winsock usage */
  RUN WSACleanup (OUTPUT w-Return).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-winUserName) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winUserName Procedure 
PROCEDURE winUserName :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER NAME AS CHARACTER.
 
  DEFINE VARIABLE nr AS INTEGER NO-UNDO INITIAL 100.
  DEFINE VARIABLE ReturnValue AS INTEGER NO-UNDO.
  NAME = FILL(" ", nr).
  RUN GetUserName{&A} IN hpApi (INPUT-OUTPUT NAME,
                                 INPUT-OUTPUT nr,
                                 OUTPUT ReturnValue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getHost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHost Procedure 
FUNCTION getHost RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cHost AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAddr AS CHARACTER  NO-UNDO.

  RUN i-GetTcpInfo (OUTPUT cHost, OUTPUT cAddr).

  RETURN cHost.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getIp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getIp Procedure 
FUNCTION getIp RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cHost AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAddr AS CHARACTER  NO-UNDO.

  RUN i-GetTcpInfo (OUTPUT cHost, OUTPUT cAddr).

  RETURN cAddr.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getLoggedUser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLoggedUser Procedure 
FUNCTION getLoggedUser RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cUserName AS CHARACTER  NO-UNDO.

  RUN WinUserName(OUTPUT cUserName).

  RETURN cUserName.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

