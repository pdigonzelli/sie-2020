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

DEFINE VARIABLE hWindow AS WIDGET-HANDLE     NO-UNDO.

DEFINE TEMP-TABLE ttNodos
  FIELD anterior  AS CHARACTER
  FIELD siguiente AS CHARACTER
  FIELD nro       AS CHARACTER
  FIELD caption   AS CHARACTER
  FIELD prog      AS CHARACTER
  FIELD visitado  AS LOGICAL.


DEFINE VARIABLE vhWin AS HANDLE     NO-UNDO.

/* ***************************  Definitions  ************************** */

&GLOB  MAX_PATH 260
&GLOB  FIND_DATA-SIZE 318

/*------------------------------ SHFILEOPSTRUC -------------------------

  Public Type SHFILEOPSTRUC
     hWnd        As Long
     wFunc       As Long
     pFrom       As String
     pTo         As String
     fFlags      As Integer
     fAborted    As Boolean
     hNameMaps   As Long
     sProgress   As String
   End Type
-------------------------------------------------------------------------*/
/* Window function, move, to perform */
&GLOB FO_MOVE               1
/* Window function, copy, to perform */
&GLOB FO_COPY               2
/* Window function, delete, to perform */
&GLOB FO_DELETE             3
/* Window function, rename, to perform */
&GLOB FO_RENAME             4

/* Do not display progress dialog box */
&GLOB FOF_SILENT            4
/* Respond with yes to all for any dialog box that is displayed. */
&GLOB FOF_NOCONFIRMATION   16
/* Perform the operation of files only if a wildcard file name(*.*) is specified */
&GLOB FOF_FILESONLY       128
/* Displays a progress dialog box but does not show the file names. */
&GLOB FOF_SIMPLEPROGRESS  256
/* Does not confirm the creatio of a new directory if the operation requires one to be created */
&GLOB FOF_NOCONFIRMMKDIR  512

&GLOB SHARD_PATH            2 
/* Window desktop virtual folder at the root of the name sapce */
&GLOB CSIDL_DESKTOP         0
/* Directory which contains the user's program groups */
&GLOB CSIDL_PROGRAMS        2
/* Control Panel - virtual folder containing icons for the conrtol panel applications */
&GLOB CSIDL_CONTROL         3
/* Printers folder - virtual folder containing installed printers */
&GLOB CSIDL_PRINTERS        4
/* File system directory which servers as a common repository for documents, (Document folder) */
&GLOB CSIDL_PERSONAL        5
/* File system directory which contains the user's IE favorite URLs */
&GLOB CSIDL_FAVORITES       6
/* File system directory that corresponds to the user's Startup program group */
&GLOB CSIDL_STARTUP         7
/* File system directory that contains the most recently used documents */
&GLOB CSIDL_RECENT          8
/* File system directory that contains the 'Send To' menu items */
&GLOB CSIDL_SENDTO          9
/* Recycle bin file system directory */
&GLOB CSIDL_BITBUCKET      10
/* File system directory containing 'Start' menu items */
&GLOB CSIDL_STARTMENU      11
/* File system directory used to physically store file objects on the desktop, */
/* this should not be confused with the desktop folder itself                  */
&GLOB CSIDL_DESKTOPDIRECTORY 16
/* My Computer - virtual folder containing everything on the local computer (storage devices,printers,...) */
&GLOB CSIDL_DRIVES          17
/* Network Neighborhood - virtual folder representing the top level of the network hierarchy */
&GLOB CSIDL_NETWORK         18
/* File system directory containing objects that appear in the network neighborhood */
&GLOB CSIDL_NETHOOD         19
/* Virtual folder containing fonts */
&GLOB CSIDL_FONTS           20
/* File system directory that servers as a common respository for document tmeplates */
&GLOB CSIDL_TEMPLATES       21

/*-------------------------- BrowseInfo.ulFlags --------------------------*/

/* Only returns file system directories */
&GLOB BIF_RETURNONLYFSDIRS   1
/* Do not include network folders below the domain level */
&GLOB BIF_DONTGOBELOWDOMAIN  2
/* Include a status area in the dialog box */
&GLOB BIF_STATUSTEXT         4
/* Only return file system ancestors.  If the user selects anything other than */
/* a file system ancestor, the OK button is grayed.                            */
&GLOB BIF_RETURNANCESTORS    8
/* Only return computers.  If the users selects anything other than a computer */
/* the OK button is grayed.                                                    */
&GLOB BIF_BROWSEFORCOMPUTER 256
/* Only return (network) printers. If the user selects anything other than a   */
/* printer the OK button is grayed.                                            */
&GLOB BIF_BROWSEFORPRINTER  512

&GLOBAL-DEFINE GWL_STYLE -16 
&GLOBAL-DEFINE BM_SETIMAGE 247 
&GLOBAL-DEFINE IMAGE_ICON 1 
&GLOBAL-DEFINE SHGFI_ICON 256 
&GLOBAL-DEFINE SHGFI_SYSICONINDEX 16384 
&GLOBAL-DEFINE SHGFI_USEFILEATTRIBUTES 16 
&GLOBAL-DEFINE FILE_ATTRIBUTE_NORMAL 128 
&GLOBAL-DEFINE BS_ICON 64 


/*------------------------- Shell Interface API's ----------------------------*/



PROCEDURE SHAddToRecentDocs EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER dwFlags        AS  long.
  DEFINE INPUT PARAMETER dwData         AS  long.
  DEFINE RETURN PARAMETER iResult       AS  long.
END.
    
PROCEDURE SHFileOperationA EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER lpFileOp       AS  long.  /* pointer to SHFILEOPSTRUCT */
  DEFINE RETURN PARAMETER iResult       AS  long.
END.

PROCEDURE SHGetPathFromIDListA EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER pidl           AS  long.
  DEFINE OUTPUT PARAMETER pszPath        AS  CHAR.
  DEFINE RETURN PARAMETER iResult       AS  long.
END.
       
PROCEDURE SHGetSpecialFolderLocation EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER hwndOwner      AS  long.
  DEFINE INPUT PARAMETER nFolder        AS  long.
  DEFINE OUTPUT PARAMETER pidl           AS  long.
  DEFINE RETURN PARAMETER iResult       AS  long.
END.

PROCEDURE SHChangeNotify            EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER wEventId       AS  long.
  DEFINE INPUT PARAMETER uFlags         AS  long.
  DEFINE INPUT PARAMETER dwItem1        AS  long.
  DEFINE INPUT PARAMETER dwItem2        AS  long.
  DEFINE RETURN PARAMETER iResult       AS  long.
END.

PROCEDURE CoTaskMemFree             EXTERNAL "ole32.dll":
  DEFINE INPUT PARAMETER lpPidl         AS  long.
END.

/* miscellaneous API(s) */

PROCEDURE GetTempPathA EXTERNAL "kernel32":u:
  DEFINE INPUT PARAMETER nBufferLength   AS  long.
  DEFINE OUTPUT PARAMETER lpBuffer       AS  CHAR.
  DEFINE RETURN PARAMETER nTempPathSz    AS  long.
END.

PROCEDURE Sleep EXTERNAL 'kernel32':U:
  DEFINE INPUT  PARAMETER dwMilliseconds AS LONG.
END PROCEDURE.

PROCEDURE FindClose EXTERNAL 'kernel32' :
    DEFINE INPUT PARAMETER hSearch AS long.
END PROCEDURE.

PROCEDURE FindFirstFileA EXTERNAL 'kernel32' :
    DEFINE INPUT PARAMETER  lpFileName AS CHAR.
    DEFINE INPUT PARAMETER  lpFindFileData AS long.
    DEFINE RETURN PARAMETER hSearch AS long.
END PROCEDURE.    


PROCEDURE SHGetFileInfo EXTERNAL 'shell32': 
    DEFINE INPUT  PARAMETER pszPath AS CHAR. 
    DEFINE INPUT  PARAMETER dwFileAttributes AS LONG. 
    DEFINE OUTPUT PARAMETER SBFileInfo AS MEMPTR. 
    DEFINE INPUT  PARAMETER cbSizeFileInfo AS LONG. 
    DEFINE INPUT  PARAMETER uFlags AS LONG. 
    DEFINE RETURN PARAMETER ReturnValue AS LONG. 
END PROCEDURE. 




/*-------------------------- SHChangeNotify EventIds --------------------------*/

/* Event Id which can occure and be based via SHChangeNotify */
/* All events hav occurred */
&GLOB SHCNE_ALLEVENTS        2147483647
/* The name of a nonfolder item has changed. */
&GLOB SHCNE_RENAMEITEM                1
/* A nonfolder item has been created. */
&GLOB SHCNE_CREATE                    2
/* A nonfolder item has been deleted. */
&GLOB SHCNE_DELETE                    4
/* A folder has been created. */
&GLOB SHCNE_MKDIR                     8
/* A folder has been removed. */
&GLOB SHCNE_RMDIR                    16
/* Storage media has been inserted into a drive. */
&GLOB SHCNE_MEDIAINSERTED            32
/* Storage media has ben removed from a drive. */
&GLOB SHCNE_MEDIAREMOVED             64
/* A drive has been removed. */
&GLOB SHCNE_DRIVEREMOVED            128
/* A drive has been added. */
&GLOB SHCNE_DRIVEADD                256
/* A folder on the local computer is being shared via the network. */
&GLOB SHCNE_NETSHARE                512
/* A folder on the local computer is no longer being shared via the network. */
&GLOB SHCNE_NETUNSHARE             1024
/* The attributes of an item or folder have changed. */
&GLOB SHCNE_ATTRIBUTES             2048
/* The contents of an existing foler have changed, but the folder still exist */
/* and has not been renamed. */
&GLOB SHCNE_UPDATEDIR              4096
/* An existing nonfolder item has changed, but the item still exist and has */
/* not been renamed. */
&GLOB SHCNE_UPDATEITEM             8192
/* The computer has disconnectd from a server. */
&GLOB SHCNE_SERVERDISCONNECT      16384
/* An image in the system image list has changed. */
&GLOB SHCNE_UPDATEIMAGE           32768
/* A drive has been added and the shell should create a new window for the drive. */
&GLOB SHCNE_DRIVEADDGUI           65536
/* The name of a folder has changed */
&GLOB SHCNE_RENAMEFOLDER         131072
/* The amount of free space on a drive has changed. */
&GLOB SHCNE_FREESPACE            262144
/* A file type association has changed. */
&GLOB SHCNE_ASSOCCHANGED      134217728

/*-------------------------- SHChangeNotify uFlags --------------------------*/
/* Flags for use with SHChangeNotify */
/* uFlags & SHCNF_TYPE is an ID which indicates what dwItem1 and dwItem2 mean */

/* dwItem1 and dwItem2 are the address of ITEMIDLIST structures that represent */
/* the item(s) affected by the change.   */
&GLOB SHCNF_IDLIST         0        
/* dwItem1 and dwItem2 are the addresses of NULL terminated strings that */
/* contain the full path names of the item(s) affected by the change.    */
&GLOB SHCNF_PATH           1      
/* dwItem1 and dwItem2 are the addresses of NULL terminated strings that */
/* represent the friendly names of the printer(s) affected by the change.*/
&GLOB SHCNF_PRINTER        2        
/* the dwItem1 and dwItem2 parameters are DWORD values. */
&GLOB SHCNF_DWORD          3
&GLOB SHCNF_TYPE         255
/* The function should not return until the notification has been delivered */
/* to all components. */
&GLOB SHCNF_FLUSH       4096
/* The function should begin delivering notifications to all affected components */
/* but should return as soon as the notification process has begun. */
&GLOB SHCNF_FLUSHNOWAIT 8192

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-addMenuItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD addMenuItem Procedure 
FUNCTION addMenuItem RETURNS CHARACTER
  (pcLetra   AS CHARACTER,  /*letra del sistema*/
   pcItem    AS CHARACTER,  /*nro de nodo*/
   pcPrimero AS CHARACTER,  /*primero de la rama (sirve para evaluar la condicion de parada)*/
   phParent  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-canFindFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canFindFile Procedure 
FUNCTION canFindFile RETURNS LOGICAL
(INPUT  pcFileName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-canRun) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD canRun Procedure 
FUNCTION canRun RETURNS LOGICAL
  (pcProgram AS CHARACTER, 
   pcUser    AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cleanUp Procedure 
FUNCTION cleanUp RETURNS LOGICAL
  ( INPUT pcRemoveFileSpec  AS  CHAR, 
    INPUT phWindowHandle    AS  HANDLE  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createMenuItem Procedure 
FUNCTION createMenuItem RETURNS HANDLE
  (phParent   AS HANDLE, 
   pcName     AS CHARACTER, 
   pcLabel    AS CHARACTER, 
   pcAccel    AS CHARACTER, 
   pcOnChoose AS CHARACTER, 
   plState    AS LOGICAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuSub) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createMenuSub Procedure 
FUNCTION createMenuSub RETURNS HANDLE
  (phParent AS HANDLE, 
   pcName   AS CHARACTER, 
   pcLabel  AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuTop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD createMenuTop Procedure 
FUNCTION createMenuTop RETURNS HANDLE
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD displayMenu Procedure 
FUNCTION displayMenu RETURNS CHARACTER
  (phWindow AS HANDLE, 
   phMenu   AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exploreMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD exploreMenu Procedure 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER, 
   INPUT phWindowHandle  AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentsFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentsFolder Procedure 
FUNCTION getParentsFolder RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentWidgetHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentWidgetHandle Procedure 
FUNCTION getParentWidgetHandle RETURNS WIDGET-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentWindowHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentWindowHandle Procedure 
FUNCTION getParentWindowHandle RETURNS WIDGET-HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcedureProgressVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getProcedureProgressVersion Procedure 
FUNCTION getProcedureProgressVersion RETURNS INTEGER
  (phProc AS WIDGET-HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSpecialFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSpecialFolder Procedure 
FUNCTION getSpecialFolder RETURNS CHARACTER
  ( INPUT iCSIDL  AS  INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getToolbarProperties Procedure 
FUNCTION getToolbarProperties RETURNS CHARACTER
  (pcConfigFile AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  (pcXml AS CHARACTER, 
   pcTag AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-populateMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD populateMenu Procedure 
FUNCTION populateMenu RETURNS HANDLE
  ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shellMoveFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shellMoveFile Procedure 
FUNCTION shellMoveFile RETURNS LOGICAL
  ( INPUT pcFromFiles    AS  CHAR,
    INPUT pcDestination  AS  CHAR,
    INPUT phWindowHandle AS  HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shellRenameFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shellRenameFile Procedure 
FUNCTION shellRenameFile RETURNS LOGICAL
  ( INPUT pcOldName      AS  CHAR,
    INPUT pcDestination  AS  CHAR,
    INPUT phWindowHandle AS  HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shortCut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shortCut Procedure 
FUNCTION shortCut RETURNS LOGICAL
  (INPUT pcShortcut          AS CHARACTER,
   INPUT pcDescription       AS CHARACTER,
   INPUT pcTargetPath        AS CHARACTER,
   INPUT pcArguments         AS CHARACTER,
   INPUT pcWorkingDirectory  AS CHARACTER,
   INPUT piWindowStyle       AS INTEGER,
   INPUT pcHotkey            AS CHARACTER,
   INPUT pcIconLocation      AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD updateStatus Procedure 
FUNCTION updateStatus RETURNS CHARACTER
  ( INPUT pcText AS CHAR )  FORWARD.

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
         WIDTH              = 59.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-addttNodos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addttNodos Procedure 
PROCEDURE addttNodos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcNro AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcAnt AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcPos AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcCap AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER pcPrg AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER plSta AS LOGICAL    NO-UNDO.

  
  CREATE ttNodos.
  ASSIGN ttNodos.nro        = pcNro
         ttNodos.anterior   = pcAnt
         ttNodos.siguiente  = pcPos
         ttNodos.caption    = pcCap
         ttNodos.prog       = pcPrg
         ttNodos.visitado   = plSta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cleanUpInfoInParent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cleanUpInfoInParent Procedure 
PROCEDURE cleanUpInfoInParent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMnu AS WIDGET-HANDLE     NO-UNDO.
  
  hMnu = getParentWindowHandle().

  RUN setRunningProc IN hMnu ("", "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-closeAllChilds) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeAllChilds Procedure 
PROCEDURE closeAllChilds :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phParent AS WIDGET-HANDLE     NO-UNDO.

  DEFINE VARIABLE hLib AS WIDGET-HANDLE     NO-UNDO.
  
  hLib = SESSION:FIRST-PROCEDURE.


  DO WHILE VALID-HANDLE(hLib):

    IF DYNAMIC-FUNCTION('getContainerType' IN hLib) = "WINDOW" THEN DO:
      APPLY "CLOSE" TO hLib.
      RETURN NO-APPLY.
    END.
    hLib = hLib:NEXT-SIBLING.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createProgressShortCut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createProgressShortCut Procedure 
PROCEDURE createProgressShortCut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER phWindowHandle AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER pcMenuFolder   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcMenuName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcFileName     AS CHARACTER NO-UNDO.

DEFINE VARIABLE iResult        AS  INTEGER      NO-UNDO.
DEFINE VARIABLE iTempSz        AS  INTEGER      NO-UNDO.
DEFINE VARIABLE cTempPath      AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cFolderPath    AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cStartMenuPath AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cDesktopPath   AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cFavoritesPath AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cFileNameOld   AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cFileNameNew   AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE vcDesde        AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE vcHasta        AS  CHARACTER    NO-UNDO.
DEFINE VARIABLE cTarget        AS  CHARACTER  NO-UNDO.
DEFINE VARIABLE lpFileName     AS  MEMPTR       NO-UNDO.


  cTempPath = FILL(' ', {&MAX_PATH}).
  
  RUN GetTempPathA(INPUT  {&MAX_PATH},
                   OUTPUT cTempPath,
                   OUTPUT iTempSz).

  cTempPath = TRIM(cTempPath).
  
  UpdateStatus('Obtaining SpecialFolder paths').

  cStartMenuPath    = GetSpecialFolder({&CSIDL_STARTMENU}).
  cFolderPath       = GetSpecialFolder({&CSIDL_RECENT}).
  cDesktopPath      = GetSpecialFolder({&CSIDL_DESKTOPDIRECTORY}).
  cFavoritesPath    = GetSpecialFolder({&CSIDL_FAVORITES}).
  
  IF cStartMenuPath = '' OR
     cFolderPath    = '' OR
     cDesktopPath   = '' OR
     cFavoritesPath = '' THEN DO:
    MESSAGE 'Error obtain special folder information.  We can not continue.' VIEW-AS ALERT-BOX.
    RETURN "".
  END.
  cTarget = cStartMenuPath + "Menu Sistema" + pcMenuFolder + pcFileName + ".lnk".
  vcDesde = cFolderPath + pcMenuName + ".lnk".
  vcHasta = cStartMenuPath + "Menu Sistema" + pcMenuFolder + pcMenuName + ".lnk".
  
  /*remuevo caracteres invalidos como / o ' */
  vcDesde = REPLACE(vcDesde, "/", "").
  vcHasta = REPLACE(vcHasta, "/", "").

  /*creo shortcut en documentos recientes*/
  IF shortCut(vcDesde, 
              pcMenuName, 
              'd:\Progress\dlc91d\bin\prowin32.exe', 
              ' -p ..\launchProgram.p -pf u:\cr_indust_launcher.pf -param ' + pcFileName + ' -d dmy -s 120 -Wa -wpp', 
              '..\industria', 
              1,
              "",
              'D:\PROGRESS\dlc91d\gui\adeicon\progress.ico') THEN DO:
    /*borro si existe*/
    cleanUp(vcHasta, 
            phWindowHandle).
        
    /*muevo shortcut de documentos recientes a carpeta del menu, uso esta funcion porque si el directorio no existe lo crea*/
    shellMoveFile(vcDesde,
                  vcHasta, 
                  phWindowHandle).
  END.
  ELSE
    MESSAGE "Failed to Created shortcut." VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.



/*
  /*borro documentos recientes*/
  RUN SHAddToRecentDocs(INPUT {&SHARD_PATH}, 
                        INPUT 0,
                        OUTPUT iResult).

  /*agrego link a documentos recientes para despues moverlo a carpeta de menu*/
  DEFINE VARIABLE path AS CHARACTER  NO-UNDO.
  path = "d:\prowin32.exe".
  path = cTempPath + pcFileName.
  /*path = "d:\progress\dlc91d\bin\prowin32.exe -pf u:\cr_indust_desarrollo_run.pf -d dmy -s 120 -Wa -wpp".*/
  ASSIGN  SET-SIZE(lpFileName)     = 0
          SET-SIZE(lpFileName)     = 260
          PUT-STRING(lpFileName,1) = path.
          
  /*PUT-STRING(lpFileName,1) = cTempPath + pcFileName . /*aqui va el archivo que tiene que levantar el shortcut*/*/
  RUN SHAddToRecentDocs(INPUT {&SHARD_PATH}, 
                        INPUT GET-POINTER-VALUE(lpFilename),
                        OUTPUT iResult).
  
  /*notifico al sistema de los cambios*/
  ASSIGN SET-SIZE(lpFileName)     = 0
         SET-SIZE(lpFileName)     = 260
         PUT-STRING(lpFileName,1) = cFolderPath.

  RUN SHChangeNotify(INPUT  {&SHCNE_CREATE},
                     INPUT  {&SHCNF_PATH},
                     INPUT  GET-POINTER-VALUE(lpFilename),
                     INPUT  0,
                     OUTPUT iResult).
                     
  /* give the shell a chance to reflect changes */ 
  RUN sleep(100).

  /*muevo de documentos recientes a carpeta de menu*/
  vcDesde = cFolderPath + pcFileName + ".lnk".
  vcHasta = cStartMenuPath + "Menu Sistema" + pcMenuFolder + pcFileName + ".lnk".
  
  /*borro si existe*/
  cleanUp(cStartMenuPath + "Menu Sistema" + pcMenuFolder + pcMenuName + ".lnk", 
          phWindowHandle).
  
  ShellMoveFile(vcDesde,
                vcHasta, 
                phWindowHandle) NO-ERROR.
  
  /*renombro el link con el parametro pcMenuName*/
  ShellRenameFile(vcHasta,
                  cStartMenuPath + "Menu Sistema" + pcMenuFolder + pcMenuName + ".lnk", 
                  phWindowHandle).

  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createShortCut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createShortCut Procedure 
PROCEDURE createShortCut :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  We need to perform some up front prepatory work prior to doing
            anything.  We will need to obtain 3 thing for this example.
            
            First:  The path to the user's start menu folder.  This is
                    needed since different user may use different paths for
                    the current menu (i.e. Win95 System, c:win; Win NT4 uses
                    profiles so each user has their own.)
            Second: The file system directory that contains the most recently
                    used documents for a user, for the same reasons. 
            Third:  The file system directory used to physically store
                    file objects on the desktop.
------------------------------------------------------------------------------*/
  
  /*DEFINE INPUT PARAMETER pcMenuFolderName AS CHARACTER NO-UNDO.*/
  DEFINE INPUT PARAMETER phWindowHandle AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER pcMenuPath     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcMenuName     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcFileName     AS CHARACTER NO-UNDO.


  DEFINE VAR iResult        AS  INT     NO-UNDO.
  DEFINE VAR cTempPath      AS  CHAR    NO-UNDO.
  DEFINE VAR iTempSz        AS  INT     NO-UNDO.
  DEFINE VAR cFolderPath    AS  CHAR    NO-UNDO.
  DEFINE VAR cStartMenuPath AS  CHAR    NO-UNDO.
  DEFINE VAR cDesktopPath   AS  CHAR    NO-UNDO.
  DEFINE VAR cFavoritesPath AS  CHAR    NO-UNDO.
  DEFINE VAR cFileNameOld   AS  CHAR    NO-UNDO.
  DEFINE VAR cFileNameNew   AS  CHAR    NO-UNDO.
  DEFINE VAR lpFileName     AS  MEMPTR  NO-UNDO.

  cTempPath = FILL(' ', {&MAX_PATH}).
  
  RUN GetTempPathA( INPUT  {&MAX_PATH},
                    OUTPUT cTempPath,
                    OUTPUT iTempSz).

  cTempPath = TRIM(cTempPath).
  
  UpdateStatus('Obtaining SpecialFolder paths').

  cStartMenuPath    = GetSpecialFolder({&CSIDL_STARTMENU}).
  cFolderPath       = GetSpecialFolder({&CSIDL_RECENT}).
  cDesktopPath      = GetSpecialFolder({&CSIDL_DESKTOPDIRECTORY}).
  cFavoritesPath    = GetSpecialFolder({&CSIDL_FAVORITES}).
  
  IF cStartMenuPath = '' OR
     cFolderPath    = '' OR
     cDesktopPath   = '' OR
     cFavoritesPath = '' THEN
  DO:
    MESSAGE 'Error obtain special folder information.  We can not continue.'
      VIEW-AS ALERT-BOX.
    RETURN "":u.
  END.
  /* set up the StartMenuPath and DesktopPath to reflect the folder */
  /* where we want to install our shortcuts                         */
  
  ASSIGN cStartMenuPath         = cStartMenuPath + 'MenuSistema\'
         cDesktopPath           = cDesktopPath   + 'MenuSistema\'
         set-size(lpFileName)   = 260.
  
  /* now cleanup any files that are out there */
  cleanUp(cTempPath + pcFileName, phWindowHandle).
  cleanUp(cDesktopPath + pcMenuName + '.lnk', phWindowHandle).

    /*cleanUp(cTempPath + 'readme.txt', phWindowHandle).
    cleanUp(cDesktopPath + 'Todds Readme.lnk', phWindowHandle).
    */

  
  /*----------------------------------------------------------------------------
    The example is dependent on three fictional files to create shortcuts for.
    These may represent files which you might provide with your application.
    Note:  SHAddToRecentDocs dosen't care if the file actually exists at this
           point; its not resolved until the shortcut is accessed.
           
           If the files exist in the specified directory, the process will fail.
           If this is the second time this is run, you must ensure that the
           shortcuts are removed and the recent documents are cleared
           (press the 'Clear Recent Documents' button).
  -----------------------------------------------------------------------------*/

  /*put-string(lpFileName,1) = cTempPath + 'application.exe'.*/
  put-string(lpFileName,1) = cTempPath + 'application.exe'.
  
  /*UpdateStatus(substitute('     &1application.exe', cTempPath)).*/

  RUN SHAddToRecentDocs(INPUT {&SHARD_PATH}, 
                        INPUT GET-POINTER-VALUE(lpFilename),
                        OUTPUT iResult).
  /*                      
  assign
  set-size(lpFileName)     = 0
  set-size(lpFileName)     = 260
  put-string(lpFileName,1) = cTempPath + 'readme.txt'.
  */
  ASSIGN
  set-size(lpFileName)     = 0
  set-size(lpFileName)     = 260
  put-string(lpFileName,1) = cTempPath + pcFileName.
  
  RUN SHAddToRecentDocs(INPUT {&SHARD_PATH}, 
                        INPUT GET-POINTER-VALUE(lpFilename),
                        OUTPUT iResult).

  /*----------------------------------------------------------------------------- 
    Now we will notifiy the system of an event that our applicantion has 
    performed so that it is reflected by the system.  An application should
    invoke the SHChangeNotify function if it performs an action that may
    affect the shell.
  ------------------------------------------------------------------------------*/
 
  ASSIGN
  set-size(lpFileName) = 0
  set-size(lpFileName) = 260
  put-string(lpFileName,1) = cFolderPath.

  RUN SHChangeNotify(INPUT  {&SHCNE_CREATE},
                     INPUT  {&SHCNF_PATH},
                     INPUT  GET-POINTER-VALUE(lpFilename),
                     INPUT  0,
                     OUTPUT iResult).
                     
  /* give the shell a chance to reflect changes */ 
  RUN sleep(1000).

                  
   /*----------------------------------------------------------------------------- 
    Since the shortcuts now exist in the documents folder, and the path to that
    that folder and the users start menu, desktop folders are valid; we can move 
    the shortcuts from the recent folder into our new application folder using
    the SHFileOperation API.
    
    The shortcuts created reside in the Recent folder, so we need to use this
    path pluse the file name.  Additionally, the extenstion now has .lnk
    appended to the original file name.
  ------------------------------------------------------------------------------*/
  /* invoke call to actually move shortcuts */
  
  /*modificar aqui el item del menu*/
/*  
DEFINE VAR dbg AS LOGICAL.
dbg = DEBUGGER:INITIATE().
dbg = DEBUGGER:SET-BREAK().
*/


  cStartMenuPath = cStartMenuPath + pcFileName.
  /*MESSAGE cstartmenupath VIEW-AS ALERT-BOX.*/

  /*
  ShellMoveFile(cFolderPath + 'Readme.txt' + '.lnk',
                cStartMenuPath + 'Readme.txt' + '.lnk', 
                phWindowHandle).
  */              
MESSAGE cTempPath + pcFileName + '.lnk' + "==>" + cStartMenuPath + '.lnk' VIEW-AS ALERT-BOX.
  ShellMoveFile(cTempPath + pcFileName + '.lnk',
                cStartMenuPath + '.lnk', 
                phWindowHandle).
               
  /* now rename the file to something more common */
  /*
  ShellRenameFile(cStartMenuPath + 'Readme.txt' + '.lnk',
                  cStartMenuPath + 'Todds Readme.lnk', 
                  phWindowHandle).
  */

  ShellRenameFile(cStartMenuPath + pcFileName + '.lnk',
                  cStartMenuPath + pcMenuName + '.lnk', 
                  phWindowHandle).

  set-size(lpFilename) = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-mover) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mover Procedure 
PROCEDURE mover :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER phWindowHandle AS HANDLE.

  DEFINE VARIABLE vcDesde AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcHasta AS CHARACTER  NO-UNDO.

  vcDesde = "C:\Documents and Settings\facundoj\Escritorio\archivo.txt.lnk".
  vcHasta = "C:\Documents and Settings\facundoj\Menú Inicio\prueba\archivo.txt.lnk".

  
  
  ShellMoveFile(vcDesde,
                vcHasta, 
                phWindowHandle).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-onMenuChoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE onMenuChoice Procedure 
PROCEDURE onMenuChoice :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER pcProgram AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE hWin AS WIDGET-HANDLE     NO-UNDO.
  DEFINE VARIABLE hMnu AS WIDGET-HANDLE     NO-UNDO.

  hMnu = getParentWindowHandle().
  
  RUN VALUE(pcProgram) PERSISTENT SET hWin.

  IF CAN-QUERY(hWin, 'CURRENT-WINDOW') THEN  DO:  

    IF hWin:CURRENT-WINDOW:TYPE <> "WINDOW" THEN RETURN. 

    IF getProcedureProgressVersion(hWin) = 8 THEN
      RUN adm-initialize IN hWin NO-ERROR.
    ELSE
      RUN initializeObject IN hWin NO-ERROR.
    
    IF VALID-HANDLE(hMnu) THEN DO:
      hWin:CURRENT-WINDOW:PARENT = hMnu:CURRENT-WINDOW .
      RUN setActiveWindowData (hWin).
    END.

  END.
  ELSE
    RUN VALUE(pcProgram) .

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setActiveWindowData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setActiveWindowData Procedure 
PROCEDURE setActiveWindowData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phCurrWin AS WIDGET-HANDLE     NO-UNDO.

  RUN setRunningProc IN getParentWindowHandle() (phCurrWin:CURRENT-WINDOW:TITLE, phCurrWin:FILE-NAME) NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setMenuWindowHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMenuWindowHandle Procedure 
PROCEDURE setMenuWindowHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER phWindow AS HANDLE     NO-UNDO.

  hWindow = phWindow.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-addMenuItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION addMenuItem Procedure 
FUNCTION addMenuItem RETURNS CHARACTER
  (pcLetra   AS CHARACTER,  /*letra del sistema*/
   pcItem    AS CHARACTER,  /*nro de nodo*/
   pcPrimero AS CHARACTER,  /*primero de la rama (sirve para evaluar la condicion de parada)*/
   phParent  AS HANDLE) :   /*handle del parent del menu*/
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iNodo      AS INTEGER    NO-UNDO. 
  DEFINE VARIABLE cProg      AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAux       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cPrimero   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cSiguiente AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cUsuario   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE hMenu      AS HANDLE     NO-UNDO.
  DEFINE VARIABLE lCanRun    AS LOGICAL    NO-UNDO.

  cUsuario = SUBSTRING(USERID("userdb"), 3).

  FIND FIRST ttNodos WHERE ttNodos.nro      = pcItem
                       AND ttNodos.visitado = FALSE
                     NO-LOCK NO-ERROR.
  IF AVAILABLE ttNodos THEN DO:
    iNodo            = INTEGER(ttNodos.prog) NO-ERROR.
    cSiguiente       = ttNodos.siguiente.
    ttNodos.visitado = TRUE. /*marco el nodo como visitado*/
    
    IF NOT ERROR-STATUS:ERROR AND iNodo <> 0 THEN DO:  /*es una carpeta - una rama*/      
      hMenu = createMenuSub(phParent, ttNodos.prog, ttNodos.caption).  /*crea la opcion de menu como submenu*/
      addMenuItem(pcLetra, ttNodos.prog, ttNodos.prog, hMenu). /*continua explorando con el proximo nodo*/
    END.      
    ELSE DO:  /*es un programa - nodo terminal*/
      lCanRun = canRun(ttNodos.prog, cUsuario). /*evalua permisos*/
      hMenu = createMenuItem(phParent, ttNodos.prog, ttNodos.caption, "", ttNodos.prog, lCanRun). /*crea la opcion de menu*/
    END.
  END.


  IF cSiguiente = pcPrimero THEN DO:  /*recorrio todos los nodosterminales de la rama*/
    RETURN "Termina".   
  END.
  ELSE DO:  /*sigue expolorando con el siguiente nodo no visitado*/    
    FIND FIRST ttNodos WHERE ttNodos.anterior = pcPrimero
                         AND ttNodos.visitado = FALSE
                       NO-LOCK NO-ERROR.
    IF AVAILABLE ttNodos THEN
      addMenuItem(pcLetra, ttNodos.nro, ttNodos.nro, phParent).

  END.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-canFindFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canFindFile Procedure 
FUNCTION canFindFile RETURNS LOGICAL
(INPUT  pcFileName AS CHAR):
/*------------------------------------------------------------------------

  Function:    CanFindFile

  Description: Input: pcFilename - path\filename
               Returns TRUE if successful else false
               
  History: 
          
------------------------------------------------------------------------*/

   DEF VAR hSearch    AS INTEGER.
   DEF VAR lpFindData AS MEMPTR.
   
   set-size(lpFindData) = {&FIND_DATA-SIZE}.

   RUN FindFirstFileA(pcFileName, 
                      GET-POINTER-VALUE(lpFindData), 
                      OUTPUT hSearch).

  IF hSearch <> -1 /* INVALID_HANDLE_VALUE */ THEN
      RUN FindClose(hSearch).

  set-size(lpFindData) = 0.
  
  RETURN hSearch <> -1. /* INVALID_HANDLE_VALUE */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-canRun) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION canRun Procedure 
FUNCTION canRun RETURNS LOGICAL
  (pcProgram AS CHARACTER, 
   pcUser    AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST par_permisos WHERE par_permisos.nombre_programa = pcProgram
                            AND par_permisos.puede_ejecutar  = "*"
                          NO-LOCK NO-ERROR.
  IF AVAILABLE par_permisos THEN
    RETURN TRUE.
 


  FIND FIRST par_permisos WHERE par_permisos.nombre_programa = pcProgram
                            AND par_permisos.puede_ejecutar  MATCHES "*" + pcUser + "*"
                          NO-LOCK NO-ERROR.
  IF AVAILABLE par_permisos THEN
    RETURN TRUE.
  ELSE 
    RETURN FALSE.


  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-cleanUp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cleanUp Procedure 
FUNCTION cleanUp RETURNS LOGICAL
  ( INPUT pcRemoveFileSpec  AS  CHAR, 
    INPUT phWindowHandle    AS  HANDLE  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  This is just here to clean up the existing links and files so 
            the program can continue and is for demo purposes only.
------------------------------------------------------------------------------*/
  DEFINE VAR cTempDir   AS  CHAR    NO-UNDO.
  DEFINE VAR iTempSz    AS  INT     NO-UNDO.

  DEF VAR iResult           AS  INT     NO-UNDO.
  DEF VAR lpFileSpec        AS  MEMPTR  NO-UNDO.
  DEF VAR lpDest            AS  MEMPTR  NO-UNDO.
  DEF VAR lpSHFileOPStruct  AS  MEMPTR  NO-UNDO.
  DEF VAR cNullStr          AS  CHAR    NO-UNDO.
  
  IF NOT CanFindFile(pcRemoveFileSpec) THEN 
    RETURN FALSE.
  
  /* allocate memory */
  ASSIGN
  set-size(lpSHFileOPStruct)  =  32
  set-size(lpFileSpec)        =  LENGTH(pcRemoveFileSpec) + 3.
  
  /* set list of files to be renamed.  The list must be double null-terminated */
  ASSIGN
  put-string(lpFileSpec,1)  = pcRemoveFileSpec
  put-byte(lpFileSpec,LENGTH(pcRemoveFileSpec) + 1) = 0
  put-byte(lpFileSpec,LENGTH(pcRemoveFileSpec) + 2) = 0.
    
  
  /* setup structure */
  ASSIGN
  put-long(lpSHFileOPStruct,1)    =   phWindowHandle:HWND
  put-long(lpSHFileOPStruct,5)    =   {&FO_DELETE}          /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   GET-POINTER-VALUE(lpFileSpec)
  put-long(lpSHFileOPStruct,13)   =   0
  put-long(lpSHFileOPStruct,17)   =   {&FOF_SILENT} +       /* Flags */
                                      {&FOF_NOCONFIRMATION} 
  put-long(lpSHFileOPStruct,21)   =   0
  put-long(lpSHFileOPStruct,25)   =   0
  put-long(lpSHFileOPStruct,29)   =   0.
   

 /*--------------------------------------------------------------------------------
   now delete the file.
 ----------------------------------------------------------------------------------*/
  RUN SHFileOperationA(INPUT  GET-POINTER-VALUE(lpSHFileOPStruct),
                       OUTPUT iResult) NO-ERROR.

  /* de-allocate memory */
  ASSIGN
  set-size(lpFileSpec)        = 0
  set-size(lpSHFileOPStruct)  = 0.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuItem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createMenuItem Procedure 
FUNCTION createMenuItem RETURNS HANDLE
  (phParent   AS HANDLE, 
   pcName     AS CHARACTER, 
   pcLabel    AS CHARACTER, 
   pcAccel    AS CHARACTER, 
   pcOnChoose AS CHARACTER, 
   plState    AS LOGICAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMenuItem AS HANDLE     NO-UNDO.

  CREATE MENU-ITEM hMenuItem

  ASSIGN PARENT      = phParent
         NAME        = pcName
         LABEL       = pcLabel       
         ACCELERATOR = pcAccel /* This IS WHERE the ACCELERATOR KEYS are DEFINED */      
  
  TRIGGERS:
    ON CHOOSE PERSISTENT RUN onMenuChoice IN TARGET-PROCEDURE (pcOnChoose).
  END TRIGGERS.
  
  ASSIGN hMenuItem:SENSITIVE = plState.


  RETURN hMenuItem.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuSub) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createMenuSub Procedure 
FUNCTION createMenuSub RETURNS HANDLE
  (phParent AS HANDLE, 
   pcName   AS CHARACTER, 
   pcLabel  AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hSubMenu AS HANDLE     NO-UNDO.

  CREATE SUB-MENU hSubMenu  
  ASSIGN PARENT = phParent
         NAME   = pcName
         LABEL  = pcLabel.

  RETURN hSubMenu.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-createMenuTop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION createMenuTop Procedure 
FUNCTION createMenuTop RETURNS HANDLE
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMenu     AS HANDLE     NO-UNDO.
  
  CREATE MENU hMenu.

  RETURN hMenu.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-displayMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION displayMenu Procedure 
FUNCTION displayMenu RETURNS CHARACTER
  (phWindow AS HANDLE, 
   phMenu   AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN phWindow:MENUBAR = phMenu:HANDLE.   

  RETURN "OK".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-exploreMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION exploreMenu Procedure 
FUNCTION exploreMenu RETURNS CHARACTER
  (INPUT pcLetraInicial  AS CHARACTER, 
   INPUT pcItem          AS CHARACTER, 
   INPUT piItemPrimero   AS INTEGER,
   INPUT-OUTPUT pcMenu   AS CHARACTER, 
   INPUT phWindowHandle  AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  recorre la tabla recursivamente para crear el menu
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE viItemPrimero   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE c               AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMenu          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcItem          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vcMenuPath      AS CHARACTER  NO-UNDO.

  vcMenu = pcMenu.
  FIND FIRST par_menu_grupos WHERE par_menu_grupos.letra_inicial  = pcLetraInicial
                               AND par_menu_grupos.ITEM_menu      = pcItem
                             NO-LOCK NO-ERROR.
  IF AVAILABLE par_menu_grupos THEN DO:
    viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion) NO-ERROR.
    vcItem        = par_menu_grupos.ITEM_posterior.
    
    IF NOT ERROR-STATUS:ERROR AND viItemPrimero <> 0 THEN DO:  /*es una carpeta */
      vcMenu        = pcMenu + "\" + par_menu_grupos.dato_menu.
      viItemPrimero = INTEGER(par_menu_grupos.accion_seleccion).
      vcItem        = STRING(viItemPrimero, "999").
    END.      
    ELSE DO:  /*es un programa*/
      viItemPrimero = piItemPrimero.
      /*obtengo la ruta de la que cuelga el programa*/
      c = getParentsFolder(pcLetraInicial, par_menu_grupos.ITEM_menu, vcMenuPath).
      
      /*agrego una carpeta correspondiente al sistema del parametro pcLetraInicial*/
      CASE pcLetraInicial:
        WHEN "y" THEN
          vcMenuPath = "\Industria\" + vcMenuPath.
        WHEN "r" THEN
          vcMenuPath = "\Ventas\" + vcMenuPath.
      END CASE.
      
      RUN createProgressShortCut (phWindowHandle, 
                                  vcMenuPath, 
                                  par_menu_grupos.dato_menu,
                                  par_menu_grupos.accion_seleccion_gui).
    END.
      
      
  END.
  IF par_menu_grupos.ITEM_posterior = STRING(piItemPrimero, "999") THEN DO:  /*recorrio todos los hijos*/
    RETURN pcMenu.  
  END.
  ELSE DO:
    c = exploreMenu(pcLetraInicial, vcItem, viItemPrimero, INPUT-OUTPUT vcMenu, phWindowHandle).
  END.

END FUNCTION.

    /*
    DEFINE VAR dbg AS LOGICAL.
    dbg = DEBUGGER:INITIATE().
    dbg = DEBUGGER:SET-BREAK().
    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentsFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentsFolder Procedure 
FUNCTION getParentsFolder RETURNS CHARACTER
  (INPUT pcLetra        AS CHARACTER, 
   INPUT pcItem         AS CHARACTER,
   INPUT-OUTPUT pcPath  AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  dado un item del menu devuelve las carpetas de las que cuelga
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE vcItem AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE c      AS CHARACTER  NO-UNDO.
  DEFINE BUFFER pm  FOR par_menu_grupos.
  DEFINE BUFFER pmg FOR par_menu_grupos.
  
    /*busco el item*/
  FIND FIRST pmg WHERE pmg.letra_inicial = pcLetra
                   AND pmg.ITEM_menu     = pcItem
                 NO-LOCK NO-ERROR.
  /*armo condicion de parada*/
  i = INTEGER(pmg.accion_seleccion) NO-ERROR.
  IF NOT ERROR-STATUS:ERROR AND i <> 0 AND INTEGER(pmg.ITEM_menu) <= 10 THEN DO:  
    RETURN pcPath.
  END.
  vcItem = pmg.ITEM_anterior.
  IF AVAILABLE pmg THEN DO:
    /*busco un item cuyo valor de accion_seleccion_gui = item_menu, es decir, la carpeta padre*/
    FIND FIRST pm WHERE pm.letra_inicial        = pmg.letra_inicial
                    AND pm.accion_seleccion_gui = pmg.ITEM_menu
                  NO-LOCK NO-ERROR.
    IF AVAILABLE pm THEN DO: /*es la carpeta padre*/
      pcPath    = pm.dato_menu + "\" + pcPath.
      vcItem    = pm.ITEM_anterior.
    END.
    c = getParentsFolder(pcLetra, vcItem, pcPath).
  END.
  ELSE
    RETURN "Error".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentWidgetHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentWidgetHandle Procedure 
FUNCTION getParentWidgetHandle RETURNS WIDGET-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getParentWindowHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentWindowHandle Procedure 
FUNCTION getParentWindowHandle RETURNS WIDGET-HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hRet AS WIDGET-HANDLE     NO-UNDO.
  DEFINE VARIABLE hLib AS WIDGET-HANDLE     NO-UNDO.
  
  hLib = SESSION:FIRST-PROCEDURE.

  DO WHILE VALID-HANDLE(hLib):
    IF hLib:FILE-NAME MATCHES "*wInicioIndustria.w" THEN DO:
      hRet = hLib.
      LEAVE.
    END.
    
    hLib = hLib:NEXT-SIBLING.
  END.

  RETURN hRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getProcedureProgressVersion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getProcedureProgressVersion Procedure 
FUNCTION getProcedureProgressVersion RETURNS INTEGER
  (phProc AS WIDGET-HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFileName AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iVersion  AS INTEGER    NO-UNDO.

  cFileName = phProc:FILE-NAME.

  IF SUBSTRIN(cFileName, 1, 2) = "w_" THEN
    iVersion = 8.
  ELSE 
    iVersion = 9.

  

  RETURN iVersion.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getSpecialFolder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSpecialFolder Procedure 
FUNCTION getSpecialFolder RETURNS CHARACTER
  ( INPUT iCSIDL  AS  INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR iResult   AS  INT  NO-UNDO.
  DEF VAR cPath     AS  CHAR NO-UNDO.
  DEF VAR pidl      AS  INT  NO-UNDO.
  
  &SCOPE NO_ERROR     0
  &SCOPE MAX_LENGTH 260
  
  /* set the pidl with the specified folder item */
  RUN SHGetSpecialFolderLocation( INPUT 1,
                                  INPUT iCSIDL,
                                  OUTPUT pidl,
                                  OUTPUT iResult).
                                  
  cPath = FILL(' ', {&MAX_LENGTH}).

  IF iResult = {&NO_ERROR} THEN
  DO:
    RUN SHGetPathFromIDListA(INPUT pidl,
                             OUTPUT cPath,
                             OUTPUT iResult).
                             
    /* free memory back to os */
    RUN CoTaskMemFree(INPUT pidl).
    
    /* if successful, iResult will be greater then 0 */
    /* return path with a trailing slash appended */
    IF iResult > {&NO_ERROR} THEN
      RETURN TRIM(cPath) + '\':U.
  END.

  /* if we get here we encounterd an error so return nothing for path */
  
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getToolbarProperties) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getToolbarProperties Procedure 
FUNCTION getToolbarProperties RETURNS CHARACTER
  (pcConfigFile AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLine  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cValue AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER  NO-UNDO.

  cFile = "y_default.toolbar".
  IF SEARCH("..\industria\" + pcConfigFile) <> ?  THEN
    cFile = "..\industria\" + pcConfigFile.
  
  INPUT FROM VALUE(cFile).
  REPEAT:
    IMPORT UNFORMATTED cLine.

    IF cLine MATCHES "*</NAME>" THEN DO:
      cRet = cRet + getValue(cLine, "NAME") + CHR(1).
    END.

    IF cLine MATCHES "*</LABEL>" THEN DO:
      cRet = cRet + getValue(cLine, "LABEL") + CHR(1).
    END.

    IF cLine MATCHES "*</PICTURE>" THEN DO:
      cRet = cRet + getValue(cLine, "PICTURE") + CHR(1).
    END.

    IF cLine MATCHES "*</TOOLTIP>" THEN DO:
      cRet = cRet + getValue(cLine, "TOOLTIP") + CHR(1).
    END.

    IF cLine MATCHES "*</PROGRAM>" THEN DO:
      cRet = cRet + getValue(cLine, "PROGRAM").
    END.


    IF cLine = "</BUTTON>" THEN
      cRet = cRet + CHR(10).
    
  END.

  INPUT CLOSE.


  RETURN cRet. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getValue) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getValue Procedure 
FUNCTION getValue RETURNS CHARACTER
  (pcXml AS CHARACTER, 
   pcTag AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLine AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cOTag AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cCTag AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cRet  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iPos1 AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iPos2 AS INTEGER    NO-UNDO.


  ASSIGN cLine = pcXml
         cOTag = "<" + pcTag + ">"
         cCTag = "</" + pcTag + ">".

  iPos1 = INDEX(cLine, cOTag) + LENGTH(cOTag).
  iPos2 = INDEX(cLine, cCTag) .

  cRet = SUBSTRING(pcXml, iPos1, iPos2 - iPos1).

  RETURN cRet.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-populateMenu) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION populateMenu Procedure 
FUNCTION populateMenu RETURNS HANDLE
  () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE hMenuTop AS HANDLE     NO-UNDO.

  FOR EACH ttNodos.
    DELETE ttNodos.
  END.

  FOR EACH par_menu_grupos WHERE letra_inicial = "y".
    RUN addttNodos(par_menu_grupos.ITEM_menu,
                   par_menu_grupos.ITEM_anterior, 
                   par_menu_grupos.ITEM_posterior, 
                   par_menu_grupos.dato_menu, 
                   par_menu_grupos.accion_seleccion_gui, 
                   FALSE).    
  END.
  
  
  
  CREATE MENU hMenuTop.  

  FIND FIRST ttNodos WHERE ttNodos.nro = "001" NO-ERROR. /*actividades*/
  addMenuItem("y", ttNodos.nro, "", hMenuTop).

  FIND FIRST ttNodos WHERE ttNodos.nro = "003" NO-ERROR. /*mantenimiento*/
  addMenuItem("y", ttNodos.nro, "", hMenuTop).

  FIND FIRST ttNodos WHERE ttNodos.nro = "005" NO-ERROR. /*reportes*/
  addMenuItem("y", ttNodos.nro, "", hMenuTop).

  FIND FIRST ttNodos WHERE ttNodos.nro = "006" NO-ERROR. /*consultas*/
  addMenuItem("y", ttNodos.nro, "", hMenuTop).

  FIND FIRST ttNodos WHERE ttNodos.nro = "007" NO-ERROR. /*ayuda*/
  addMenuItem("y", ttNodos.nro, "", hMenuTop).
  
  RETURN hMenuTop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shellMoveFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shellMoveFile Procedure 
FUNCTION shellMoveFile RETURNS LOGICAL
  ( INPUT pcFromFiles    AS  CHAR,
    INPUT pcDestination  AS  CHAR,
    INPUT phWindowHandle AS  HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iResult           AS  INT     NO-UNDO.
  DEF VAR lpFromFiles       AS  MEMPTR  NO-UNDO.
  DEF VAR lpDest            AS  MEMPTR  NO-UNDO.
  DEF VAR lpSHFileOPStruct  AS  MEMPTR  NO-UNDO.
  DEF VAR cNullStr          AS  CHAR    NO-UNDO.


  /* allocate memory */
  ASSIGN
  set-size(lpSHFileOPStruct) =  32
  set-size(lpFromFiles)      =  LENGTH(pcFromFiles) + 3
  set-size(lpDest)           =  LENGTH(pcDestination) + 3.
  
  /* set list of files to be copied.  The list must be double null-terminated */
  ASSIGN
  put-string(lpFromFiles,1)  = pcFromFiles
  put-byte(lpFromFiles,LENGTH(pcFromFiles) + 1) = 0
  put-byte(lpFromFiles,LENGTH(pcFromFiles) + 2) = 0
  put-string(lpDest,1) = pcDestination
  put-byte(lpDest,LENGTH(pcDestination) + 1) = 0
  put-byte(lpDest,LENGTH(pcDestination) + 2) = 0. 
  
  
  /* setup structure */
  ASSIGN
  put-long(lpSHFileOPStruct,1)    =   phWindowHandle:HWND 
  put-long(lpSHFileOPStruct,5)    =   {&FO_MOVE}  /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   GET-POINTER-VALUE(lpFromFiles)
  put-long(lpSHFileOPStruct,13)   =   GET-POINTER-VALUE(lpDest)
  put-long(lpSHFileOPStruct,17)   =   {&FOF_SILENT} +
                                      {&FOF_NOCONFIRMATION} +
                                      {&FOF_NOCONFIRMMKDIR}
  put-long(lpSHFileOPStruct,21)   =   0
  put-long(lpSHFileOPStruct,25)   =   0
  put-long(lpSHFileOPStruct,29)   =   0.
                                      
 /*--------------------------------------------------------------------------------
   now perform the move.  If the folder specified does not exist, SHFileOperation
   will create it.  Since the FOF_SILENT flag is passed the API is instructed not
   to display the 'flying folders' dialog during the move.  The FOF_NOCONFIRMATION
   suppresses prompting to move the files, 'Are you sure you want to move etc...'
   dialog.  Finally, FOF_NOCONFIRMMKDIR instructs it to create the folder without
   prompting if its OK.
 ----------------------------------------------------------------------------------*/
 
  RUN SHFileOperationA(INPUT  GET-POINTER-VALUE(lpSHFileOPStruct),
                       OUTPUT iResult).
  IF iResult > 0 THEN DO:
    DEFINE VAR dbg AS LOGICAL.
    dbg = DEBUGGER:INITIATE().
    dbg = DEBUGGER:SET-BREAK().
  END.
  /* de-allocate memory */
  ASSIGN
  set-size(lpFromFiles)      = 0
  set-size(lpDest)           = 0
  set-size(lpSHFileOPStruct) = 0.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shellRenameFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shellRenameFile Procedure 
FUNCTION shellRenameFile RETURNS LOGICAL
  ( INPUT pcOldName      AS  CHAR,
    INPUT pcDestination  AS  CHAR,
    INPUT phWindowHandle AS  HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iResult           AS  INT     NO-UNDO.
  DEF VAR lpOldName         AS  MEMPTR  NO-UNDO.
  DEF VAR lpDest            AS  MEMPTR  NO-UNDO.
  DEF VAR lpSHFileOPStruct  AS  MEMPTR  NO-UNDO.
  DEF VAR cNullStr          AS  CHAR    NO-UNDO.
  
  /* allocate memory */
  ASSIGN
  set-size(lpSHFileOPStruct) =  32
  set-size(lpOldName)        =  LENGTH(pcOldName) + 3
  set-size(lpDest)           =  LENGTH(pcDestination) + 3.
  
  /* set list of files to be renamed.  The list must be double null-terminated */
  ASSIGN
  put-string(lpOldName,1)  = pcOldName
  put-byte(lpOldName,LENGTH(pcOldName) + 1) = 0
  put-byte(lpOldName,LENGTH(pcOldName) + 2) = 0
  put-string(lpDest,1) = pcDestination 
  put-byte(lpDest,LENGTH(pcDestination) + 1) = 0
  put-byte(lpDest,LENGTH(pcDestination) + 2) = 0.
  
  
  /* setup structure */
  ASSIGN
  put-long(lpSHFileOPStruct,1)    =   phWindowHandle:HWND 
  put-long(lpSHFileOPStruct,5)    =   {&FO_RENAME}          /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   GET-POINTER-VALUE(lpOldName)
  put-long(lpSHFileOPStruct,13)   =   GET-POINTER-VALUE(lpDest)
  put-long(lpSHFileOPStruct,17)   =   {&FOF_SILENT} +       /* Flags */
                                      {&FOF_NOCONFIRMATION} +
                                      {&FOF_NOCONFIRMMKDIR}
  put-long(lpSHFileOPStruct,21)   =   0
  put-long(lpSHFileOPStruct,25)   =   0
  put-long(lpSHFileOPStruct,29)   =   0.
                                      
 /*--------------------------------------------------------------------------------
   now rename the file.
 ----------------------------------------------------------------------------------*/
  RUN SHFileOperationA(INPUT  GET-POINTER-VALUE(lpSHFileOPStruct),
                       OUTPUT iResult).

  /* de-allocate memory */
  ASSIGN
  set-size(lpOldName)        = 0
  set-size(lpDest)           = 0
  set-size(lpSHFileOPStruct) = 0.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-shortCut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shortCut Procedure 
FUNCTION shortCut RETURNS LOGICAL
  (INPUT pcShortcut          AS CHARACTER,
   INPUT pcDescription       AS CHARACTER,
   INPUT pcTargetPath        AS CHARACTER,
   INPUT pcArguments         AS CHARACTER,
   INPUT pcWorkingDirectory  AS CHARACTER,
   INPUT piWindowStyle       AS INTEGER,
   INPUT pcHotkey            AS CHARACTER,
   INPUT pcIconLocation      AS CHARACTER ) :
 
  /*------------------------------------------------------------------------------
  Purpose: creates a windows shortcut/snelkoppeling
  Notes: rdb 7-10-2000
  ------------------------------------------------------------------------------*/
  DEF VARIABLE WSHShell AS COM-HANDLE     NO-UNDO.
  DEF VARIABLE ShortCut AS COM-HANDLE     NO-UNDO.
  DEF VARIABLE lok      AS LOG INIT FALSE NO-UNDO.
  
  IF "{&OPSYS}" = "WIN32" THEN DO:
    CREATE "WScript.Shell" WSHShell NO-ERROR.
    IF VALID-HANDLE(WSHShell) THEN DO:
      ASSIGN ShortCut = WSHShell:CreateShortcut(pcShortcut)
             Shortcut:Description      = pcDescription
             ShortCut:TargetPath       = pcTargetPath
             ShortCut:Arguments        = pcArguments
             ShortCut:WorkingDirectory = pcWorkingdirectory
             ShortCut:WindowStyle      = piWindowstyle
             ShortCut:IconLocation     = pcIconLocation.
      ShortCut:SAVE NO-ERROR.
      RELEASE OBJECT WSHShell NO-ERROR.
      RELEASE OBJECT ShortCut NO-ERROR.
      ASSIGN lok = TRUE.
    END. /* IF */
    ELSE 
      MESSAGE "Please install Windows Scripting Host." VIEW-AS ALERT-BOX.
    END. /* IF */
  ELSE 
    MESSAGE "Shortcut not supported" VIEW-AS ALERT-BOX.
 
  RETURN lok.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-updateStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION updateStatus Procedure 
FUNCTION updateStatus RETURNS CHARACTER
  ( INPUT pcText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  /*cStatus:insert-string(pcText + chr(10)) in frame {&frame-name}.*/
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

