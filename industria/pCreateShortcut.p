&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Todd G. Nist

  Created: 10/5/98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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

/* {winapi/winapi.i} */

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

/*------------------------- Shell Interface API's ----------------------------*/

PROCEDURE SHAddToRecentDocs EXTERNAL "shell32.dll":
  define input parameter dwFlags        as  long.
  define input parameter dwData         as  long.
  define return parameter iResult       as  long.
END.
    
PROCEDURE SHFileOperationA EXTERNAL "shell32.dll":
  define input parameter lpFileOp       as  long.  /* pointer to SHFILEOPSTRUCT */
  define return parameter iResult       as  long.
END.

PROCEDURE SHGetPathFromIDListA EXTERNAL "shell32.dll":
  define input parameter pidl           as  long.
  define output parameter pszPath        as  char.
  define return parameter iResult       as  long.
END.
       
PROCEDURE SHGetSpecialFolderLocation EXTERNAL "shell32.dll":
  define input parameter hwndOwner      as  long.
  define input parameter nFolder        as  long.
  define output parameter pidl           as  long.
  define return parameter iResult       as  long.
END.

PROCEDURE SHChangeNotify            EXTERNAL "shell32.dll":
  define input parameter wEventId       as  long.
  define input parameter uFlags         as  long.
  define input parameter dwItem1        as  long.
  define input parameter dwItem2        as  long.
  define return parameter iResult       as  long.
END.

PROCEDURE CoTaskMemFree             EXTERNAL "ole32.dll":
  define input parameter lpPidl         as  long.
END.

/* miscellaneous API(s) */

Procedure GetTempPathA External "kernel32":u:
  define input parameter nBufferLength   as  long.
  define output parameter lpBuffer       as  char.
  define return parameter nTempPathSz    as  long.
END.

PROCEDURE Sleep EXTERNAL 'kernel32':U:
  define input  parameter dwMilliseconds AS LONG.
END PROCEDURE.

PROCEDURE FindClose EXTERNAL 'kernel32' :
    define input parameter hSearch as long.
END PROCEDURE.

PROCEDURE FindFirstFileA EXTERNAL 'kernel32' :
    define input parameter  lpFileName as char.
    define input parameter  lpFindFileData as long.
    define return parameter hSearch as long.
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

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cStatus BUTTON-1 BUTTON-2 BUTTON-3 
&Scoped-Define DISPLAYED-OBJECTS cStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CanFindFile C-Win 
FUNCTION CanFindFile RETURNS LOGICAL
(INPUT  pcFileName as CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CleanUp C-Win 
FUNCTION CleanUp RETURNS LOGICAL
  ( input pcRemoveFileSpec  as  char  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSpecialFolder C-Win 
FUNCTION GetSpecialFolder RETURNS CHARACTER
  ( input iCSIDL  as  integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShellMoveFile C-Win 
FUNCTION ShellMoveFile RETURNS LOGICAL
  ( input pcFromFiles    as  char,
    input pcDestination  as  char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ShellRenameFile C-Win 
FUNCTION ShellRenameFile RETURNS LOGICAL
  ( input pcOldName      as  char,
    input pcDestination  as  char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD UpdateStatus C-Win 
FUNCTION UpdateStatus RETURNS CHARACTER
  ( input pcText as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Create Shortcut" 
     SIZE 28.8 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Clear Recent Documents" 
     SIZE 28.8 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Add To Recent Documents" 
     SIZE 28.8 BY 1.14.

DEFINE VARIABLE cStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 86.4 BY 9 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cStatus AT ROW 1.95 COL 1.6 NO-LABEL
     BUTTON-1 AT ROW 11.29 COL 1.6
     BUTTON-2 AT ROW 11.29 COL 30.4
     BUTTON-3 AT ROW 11.29 COL 59.2
     "Status:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 2
          FGCOLOR 9 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 87.2 BY 11.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Create Shortcut Example"
         HEIGHT             = 11.48
         WIDTH              = 87.2
         MAX-HEIGHT         = 17.38
         MAX-WIDTH          = 114.6
         VIRTUAL-HEIGHT     = 17.38
         VIRTUAL-WIDTH      = 114.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       cStatus:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Create Shortcut Example */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Create Shortcut Example */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Create Shortcut */
DO:
  run CreateShortCut.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Clear Recent Documents */
DO:
    def var iResult as  int no-undo.
    
    /* clear the Recent Doucments Folder */

    run SHAddToRecentDocs(input {&SHARD_PATH}, 
                        input 0,
                        output iResult).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Add To Recent Documents */
DO:
    def var iResult as  int no-undo.
    def var lpFilename as memptr no-undo.
    
    assign
    set-size(lpFilename) = 260
    put-string(lpFilename,1) = 'c:\temp\readme.txt'.
    
    /* clear the Recent Doucments Folder */

    run SHAddToRecentDocs(input {&SHARD_PATH}, 
                        input get-pointer-value(lpFilename),
                        output iResult).

    set-size(lpFilename) = 0.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateShortCut C-Win 
PROCEDURE CreateShortCut :
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
  define var iResult        as  int     no-undo.
  define var cTempPath      as  char    no-undo.
  define var iTempSz        as  int     no-undo.
  define var cFolderPath    as  char    no-undo.
  define var cStartMenuPath as  char    no-undo.
  define var cDesktopPath   as  char    no-undo.
  define var cFavoritesPath as  char    no-undo.
  define var cFileNameOld   as  char    no-undo.
  define var cFileNameNew   as  char    no-undo.
  define var lpFileName     as  memptr  no-undo.

  cTempPath = fill(' ', {&MAX_PATH}).
  
  RUN GetTempPathA( input  {&MAX_PATH},
                    output cTempPath,
                    output iTempSz).

  cTempPath = trim(cTempPath).
  
  UpdateStatus('Obtaining SpecialFolder paths').

  cStartMenuPath    = GetSpecialFolder({&CSIDL_STARTMENU}).
  cFolderPath       = GetSpecialFolder({&CSIDL_RECENT}).
  cDesktopPath      = GetSpecialFolder({&CSIDL_DESKTOPDIRECTORY}).
  cFavoritesPath    = GetSpecialFolder({&CSIDL_FAVORITES}).
  
  if cStartMenuPath = '' or
     cFolderPath    = '' or
     cDesktopPath   = '' or
     cFavoritesPath = '' then
  do:
    message 'Error obtain special folder information.  We can not continue.'
      view-as alert-box.
    return "":u.
  end.

  UpdateStatus('     Start Menu Path - ' + cStartMenuPath).
  UpdateStatus('     Recent Doc Path - ' + cFolderPath).
  UpdateStatus('     Desktop Path - ' + cDesktopPath).
  UpdateStatus('     Favorites Path - ' + cFavoritesPath + chr(10)).

  /* set up the StartMenuPath and DesktopPath to reflect the folder */
  /* where we want to install our shortcuts                         */
  
  assign cStartMenuPath         = cStartMenuPath + 'SomeApplication\':U
         cDesktopPath           = cDesktopPath   + 'SomeApplication\':U
         set-size(lpFileName)   =   260.
  
  /* now cleanup any files that are out there */
    cleanUp(cTempPath + 'readme.txt':U).
    cleanUp(cDesktopPath + 'Todds Readme.lnk':U).

DEFINE VAR dbg AS LOGICAL.
dbg = DEBUGGER:INITIATE().
dbg = DEBUGGER:SET-BREAK().
  
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
  UpdateStatus('Add to Recent Documents:').

  put-string(lpFileName,1) = cTempPath + 'application.exe':U.
  
  UpdateStatus(substitute('     &1application.exe', cTempPath)).

  run SHAddToRecentDocs(input {&SHARD_PATH}, 
                        input get-pointer-value(lpFilename),
                        output iResult).
                        
  assign
  set-size(lpFileName) = 0
  set-size(lpFileName) = 260
  put-string(lpFileName,1) = cTempPath + 'readme.txt':U.
  
  UpdateStatus(substitute('     &1readme.txt', cTempPath) + chr(10)).

  run SHAddToRecentDocs(input {&SHARD_PATH}, 
                        input get-pointer-value(lpFilename),
                        output iResult).

  /*----------------------------------------------------------------------------- 
    Now we will notifiy the system of an event that our applicantion has 
    performed so that it is reflected by the system.  An application should
    invoke the SHChangeNotify function if it performs an action that may
    affect the shell.
  ------------------------------------------------------------------------------*/
 
  assign
  set-size(lpFileName) = 0
  set-size(lpFileName) = 260
  put-string(lpFileName,1) = cFolderPath.

  run SHChangeNotify(input  {&SHCNE_CREATE},
                     input  {&SHCNF_PATH},
                     input  get-pointer-value(lpFilename),
                     input  0,
                     output iResult).
                     
  /* give the shell a chance to reflect changes */ 
  run sleep(1000).

                  
   /*----------------------------------------------------------------------------- 
    Since the shortcuts now exist in the documents folder, and the path to that
    that folder and the users start menu, desktop folders are valid; we can move 
    the shortcuts from the recent folder into our new application folder using
    the SHFileOperation API.
    
    The shortcuts created reside in the Recent folder, so we need to use this
    path pluse the file name.  Additionally, the extenstion now has .lnk
    appended to the original file name.
  ------------------------------------------------------------------------------*/

  UpdateStatus('ShellMoveFile:').
  UpdateStatus('     ' + cFolderPath + 'Readme.txt':U + '.lnk' + chr(10)).
  
  /* invoke call to actually move shortcuts */
  ShellMoveFile(cFolderPath + 'Readme.txt':U + '.lnk':U,
                cDesktopPath + 'Readme.txt':U + '.lnk':U).
               
  /* now rename the file to something more common */
  ShellRenameFile(cDesktopPath + 'Readme.txt':U + '.lnk':U,
                  cDesktopPath + 'Todds Readme.lnk':U).

  UpdateStatus('Create Internet Favorite Link').
  UpdateStatus('     ' + cFavoritesPath + 'Jurjen API Site.url':u).
  
  /* now actually create the files which correspond to the shortcuts. */
  
  output to value(cFavoritesPath + 'Jurjen API Site.url':u + chr(10)).
  put unformatted
    '[InternetShortcut]' skip
    'URL=http://www.global-shared.com/' skip
    'Modified=A0FE3BB5BA78BD017E'.
  output close.

  output to value(cTempPath + 'readme.txt':u).
  put unformatted 'This is a simple example for creating shortcuts from' skip
                  'within Progress.' skip(1)
                  'It has been brought to you by Todd G. Nist.' skip(2)
                  'Hope this helps.' skip(2)
                  'Sincerely,' skip(3)
                  'Todd' skip
                  'tnist@netcarrier.com'.
  output close.
             
  UpdateStatus('Now simply click on the folder or if you desire,':u).
  UpdateStatus('go to the start menu and launch the shortcut.':U + chr(10)).
  UpdateStatus('You can now use the SHGetFileInfo to obtain the attributes').
  UpdateStatus('of the shortcut created and modify them as necessary.' + chr(10)).
  UpdateStatus('Hope this helps,' + chr(10)).
  UpdateStatus('Todd' + chr(10)).

  set-size(lpFilename) = 0.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cStatus 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cStatus BUTTON-1 BUTTON-2 BUTTON-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CanFindFile C-Win 
FUNCTION CanFindFile RETURNS LOGICAL
(INPUT  pcFileName as CHAR):
/*------------------------------------------------------------------------

  Function:    CanFindFile

  Description: Input: pcFilename - path\filename
               Returns TRUE if successful else false
               
  History: 
          
------------------------------------------------------------------------*/

   def var hSearch    as integer.
   def var lpFindData as memptr.
   
   set-size(lpFindData) = {&FIND_DATA-SIZE}.

   run FindFirstFileA(pcFileName, 
                      get-pointer-value(lpFindData), 
                      output hSearch).

  if hSearch <> -1 /* INVALID_HANDLE_VALUE */ then
      run FindClose(hSearch).

  set-size(lpFindData) = 0.
  
  RETURN hSearch <> -1. /* INVALID_HANDLE_VALUE */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CleanUp C-Win 
FUNCTION CleanUp RETURNS LOGICAL
  ( input pcRemoveFileSpec  as  char  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  This is just here to clean up the existing links and files so 
            the program can continue and is for demo purposes only.
------------------------------------------------------------------------------*/
  define var cTempDir   as  char    no-undo.
  define var iTempSz    as  int     no-undo.

  def var iResult           as  int     no-undo.
  def var lpFileSpec        as  memptr  no-undo.
  def var lpDest            as  memptr  no-undo.
  def var lpSHFileOPStruct  as  memptr  no-undo.
  def var cNullStr          as  char    no-undo.
  
  if not CanFindFile(pcRemoveFileSpec) then 
    return false.
  
  /* allocate memory */
  assign
  set-size(lpSHFileOPStruct)  =  32
  set-size(lpFileSpec)        =  length(pcRemoveFileSpec) + 3.
  
  /* set list of files to be renamed.  The list must be double null-terminated */
  assign
  put-string(lpFileSpec,1)  = pcRemoveFileSpec
  put-byte(lpFileSpec,length(pcRemoveFileSpec) + 1) = 0
  put-byte(lpFileSpec,length(pcRemoveFileSpec) + 2) = 0.
    
  /* setup structure */
  assign
  put-long(lpSHFileOPStruct,1)    =   {&window-name}:hwnd 
  put-long(lpSHFileOPStruct,5)    =   {&FO_DELETE}          /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   get-pointer-value(lpFileSpec)
  put-long(lpSHFileOPStruct,13)   =   0
  put-long(lpSHFileOPStruct,17)   =   {&FOF_SILENT} +       /* Flags */
                                      {&FOF_NOCONFIRMATION} 
  put-long(lpSHFileOPStruct,21)   =   0
  put-long(lpSHFileOPStruct,25)   =   0
  put-long(lpSHFileOPStruct,29)   =   0.
                                      
 /*--------------------------------------------------------------------------------
   now delete the file.
 ----------------------------------------------------------------------------------*/
  run SHFileOperationA(input  get-pointer-value(lpSHFileOPStruct),
                       output iResult) no-error.

  /* de-allocate memory */
  assign
  set-size(lpFileSpec)        = 0
  set-size(lpSHFileOPStruct)  = 0.
  
  RETURN FALSE.   /* Function return value. */

/*
  cTempDir = fill(' ', {&MAX_PATH}).
  
  RUN GetTempPath{&A} in hWinApi( input  {&MAX_PATH},
                                  output cTempDir,
                                  output iTempSz).
                                  
  cTempDir = trim(cTempDir).                                  

  os-delete value(cTempDir + 'readme.txt':U).

  RETURN FALSE.   /* Function return value. */
*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSpecialFolder C-Win 
FUNCTION GetSpecialFolder RETURNS CHARACTER
  ( input iCSIDL  as  integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  def var iResult   as  int  no-undo.
  def var cPath     as  char no-undo.
  def var pidl      as  int  no-undo.
  
  &SCOPE NO_ERROR     0
  &SCOPE MAX_LENGTH 260
  
  /* set the pidl with the specified folder item */
  run SHGetSpecialFolderLocation( input {&window-name}:hwnd,
                                  input iCSIDL,
                                  output pidl,
                                  output iResult).
                                  
  cPath = fill(' ', {&MAX_LENGTH}).

  if iResult = {&NO_ERROR} then
  do:
    run SHGetPathFromIDListA(input pidl,
                             output cPath,
                             output iResult).
                             
    /* free memory back to os */
    run CoTaskMemFree(input pidl).
    
    /* if successful, iResult will be greater then 0 */
    /* return path with a trailing slash appended */
    if iResult > {&NO_ERROR} then
      return trim(cPath) + '\':U.
  end.

  /* if we get here we encounterd an error so return nothing for path */
  
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShellMoveFile C-Win 
FUNCTION ShellMoveFile RETURNS LOGICAL
  ( input pcFromFiles    as  char,
    input pcDestination  as  char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var iResult           as  int     no-undo.
  def var lpFromFiles       as  memptr  no-undo.
  def var lpDest            as  memptr  no-undo.
  def var lpSHFileOPStruct  as  memptr  no-undo.
  def var cNullStr          as  char    no-undo.
  
  /* allocate memory */
  assign
  set-size(lpSHFileOPStruct) =  32
  set-size(lpFromFiles)      =  length(pcFromFiles) + 3
  set-size(lpDest)           =  length(pcDestination) + 3.
  
  /* set list of files to be copied.  The list must be double null-terminated */
  assign
  put-string(lpFromFiles,1)  = pcFromFiles
  put-byte(lpFromFiles,length(pcFromFiles) + 1) = 0
  put-byte(lpFromFiles,length(pcFromFiles) + 2) = 0
  put-string(lpDest,1) = pcDestination
  put-byte(lpDest,length(pcDestination) + 1) = 0
  put-byte(lpDest,length(pcDestination) + 2) = 0. 
  
  /* setup structure */
  assign
  put-long(lpSHFileOPStruct,1)    =   {&window-name}:hwnd 
  put-long(lpSHFileOPStruct,5)    =   {&FO_MOVE}  /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   get-pointer-value(lpFromFiles)
  put-long(lpSHFileOPStruct,13)   =   get-pointer-value(lpDest)
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
  
  run SHFileOperationA(input  get-pointer-value(lpSHFileOPStruct),
                       output iResult).

  /* de-allocate memory */
  assign
  set-size(lpFromFiles)      = 0
  set-size(lpDest)           = 0
  set-size(lpSHFileOPStruct) = 0.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ShellRenameFile C-Win 
FUNCTION ShellRenameFile RETURNS LOGICAL
  ( input pcOldName      as  char,
    input pcDestination  as  char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var iResult           as  int     no-undo.
  def var lpOldName         as  memptr  no-undo.
  def var lpDest            as  memptr  no-undo.
  def var lpSHFileOPStruct  as  memptr  no-undo.
  def var cNullStr          as  char    no-undo.
  
  /* allocate memory */
  assign
  set-size(lpSHFileOPStruct) =  32
  set-size(lpOldName)        =  length(pcOldName) + 3
  set-size(lpDest)           =  length(pcDestination) + 3.
  
  /* set list of files to be renamed.  The list must be double null-terminated */
  assign
  put-string(lpOldName,1)  = pcOldName
  put-byte(lpOldName,length(pcOldName) + 1) = 0
  put-byte(lpOldName,length(pcOldName) + 2) = 0
  put-string(lpDest,1) = pcDestination 
  put-byte(lpDest,length(pcDestination) + 1) = 0
  put-byte(lpDest,length(pcDestination) + 2) = 0.
  
  /* setup structure */
  assign
  put-long(lpSHFileOPStruct,1)    =   {&window-name}:hwnd 
  put-long(lpSHFileOPStruct,5)    =   {&FO_RENAME}          /* function to be performed */
  put-long(lpSHFileOPStruct,9)    =   get-pointer-value(lpOldName)
  put-long(lpSHFileOPStruct,13)   =   get-pointer-value(lpDest)
  put-long(lpSHFileOPStruct,17)   =   {&FOF_SILENT} +       /* Flags */
                                      {&FOF_NOCONFIRMATION} +
                                      {&FOF_NOCONFIRMMKDIR}
  put-long(lpSHFileOPStruct,21)   =   0
  put-long(lpSHFileOPStruct,25)   =   0
  put-long(lpSHFileOPStruct,29)   =   0.
                                      
 /*--------------------------------------------------------------------------------
   now rename the file.
 ----------------------------------------------------------------------------------*/
  run SHFileOperationA(input  get-pointer-value(lpSHFileOPStruct),
                       output iResult).

  /* de-allocate memory */
  assign
  set-size(lpOldName)        = 0
  set-size(lpDest)           = 0
  set-size(lpSHFileOPStruct) = 0.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION UpdateStatus C-Win 
FUNCTION UpdateStatus RETURNS CHARACTER
  ( input pcText as char ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cStatus:insert-string(pcText + chr(10)) in frame {&frame-name}.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

