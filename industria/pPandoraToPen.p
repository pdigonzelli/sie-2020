/*
** Name : ReadDir.p
** Desc : Read all files in a dir and store the names in a tt.
** If needed, proc can be run recursively.
** Author : Patrick Tingen
** Date : dec 2003
*/

DEFINE temp-table tFile no-undo
  FIELD cFolderName AS CHARACTER  LABEL 'Folder' format 'x(50)'
  FIELD cFileName   AS CHARACTER  LABEL 'Filename' format 'x(24)'
  FIELD cTokens     AS CHARACTER  LABEL 'Tokens' format 'x(6)'
  FIELD iByteSize   AS INTEGER    LABEL 'Size'
  FIELD tCreated    AS DATE       LABEL 'Date Created' format '99-99-9999'
  FIELD iCreated    AS INTEGER    LABEL 'Time Created'
  FIELD cCreated    AS CHARACTER  LABEL 'Created' format 'x(19)'
  FIELD tModified   AS DATE       LABEL 'Date Modified' format '99-99-9999'
  FIELD iModified   AS INTEGER    LABEL 'Time Modified'
  FIELD cModified   AS CHARACTER  LABEL 'Last Modified' format 'x(19)'
  FIELD cType       AS CHARACTER  LABEL 'Type' FORMAT 'x(25)'
  FIELD cFullPath   AS CHARACTER  LABEL 'FullPath' FORMAT 'x(100)'
  
  INDEX tFile-prim  AS PRIMARY UNIQUE cFolderName cFileName
  .

DEFINE VARIABLE cPandoraDir  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cToPenDir    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSource      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDestination AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iOrden       AS INTEGER    NO-UNDO.

CURRENT-WINDOW:WIDTH = 150.

/* -> test */
EMPTY TEMP-TABLE tFile.

cPandoraDir = "D:\musica\pandora\mp3\latinpop\".
cToPenDir   = "d:\musica\topen\latinos\".

RUN ReadDir(cPandoraDir, '', YES, YES).

DEFINE VARIABLE err-status AS INTEGER    NO-UNDO.

iOrden = 1.
FOR EACH pandora_play_list
    NO-LOCK.
  FIND FIRST tFile
       WHERE pandora_play_list.cancion = tFile.cFileName
       NO-LOCK NO-ERROR.
  IF AVAILABLE tFile THEN DO:
    cSource = tFile.cFullPath.
    cDestination = cToPenDir + STRING(iOrden) + "_" + tFile.cFileName.
  
    OS-COPY VALUE(cSource) VALUE(cDestination).
    iOrden = iOrden + 1.
  END.
END.


MESSAGE 'listo'
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
/* <- test */


PROCEDURE ReadDir:
  DEFINE INPUT PARAMETER cDir           AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cFilter        AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER lRecursive     AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER lDetailedInfo  AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE cFile         AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE iFilter       AS INTEGER NO-UNDO.
  DEFINE VARIABLE lPassedFilter AS LOGICAL NO-UNDO.
  
  /* treat a dot like a literal dot and not like 'any char' */
  IF INDEX(cFilter,'~~') EQ 0 THEN
    cFilter = REPLACE(cFilter,'.','~~.').
  
  /* read contents of dir */
  INPUT FROM OS-DIR( cDir ).
  REPEAT:
    IMPORT cFile.
  
    /* Skip current and parent dir */
    IF cFile[1] EQ '.' OR cFile[1] EQ '..' THEN NEXT.
  
    /* We only want to store files inside the TT, not dirs */
    IF cFile[3] BEGINS 'F' THEN DO:
      IF cFilter NE '' AND cFilter NE '*' THEN DO:
        /* Skip files that do not match filter */
        ASSIGN lPassedFilter = FALSE.
  
        filter:
        DO iFilter = 1 TO NUM-ENTRIES(cFilter):
          IF cFile[1] MATCHES ENTRY(iFilter,cFilter) THEN DO:
            ASSIGN lPassedFilter = TRUE.
            LEAVE filter.
          END.
        END.
  
        IF NOT lPassedFilter THEN NEXT.
      END. /* need to filter ? */
  
      CREATE tFile.
      ASSIGN  tFile.cFolderName = replace(cFile[2],cFile[1],'')
              tFile.cFileName   = cFile[1]
              tFile.cTokens     = cFile[3]
              .
  
      /* Gathering detailed info is slower, so may be turned off */
      IF lDetailedInfo THEN DO:
        FILE-INFO:FILE-NAME = cFile[2].
        ASSIGN  tFile.cFullPath = FILE-INFO:FULL-PATHNAME
                tFile.cType     = FILE-INFO:TYPE
                tFile.iByteSize = FILE-INFO:FILE-SIZE
                tFile.tModified = FILE-INFO:FILE-MOD-DATE
                tFile.iModified = FILE-INFO:FILE-MOD-TIME
                tFile.cModified = SUBSTITUTE('&1 &2', STRING(FILE-INFO:FILE-MOD-DATE,'99-99-9999'), STRING(FILE-INFO:FILE-MOD-TIME,'hh:mm:ss'))
                tFile.tCreated  = FILE-INFO:FILE-CREATE-DATE
                tFile.iCreated  = FILE-INFO:FILE-CREATE-TIME
                tFile.cCreated  = SUBSTITUTE('&1 &2', STRING(FILE-INFO:FILE-CREATE-DATE,'99-99-9999'), STRING(FILE-INFO:FILE-CREATE-TIME,'hh:mm:ss'))
                .
      END. /* detailed info */
    END. /* file, not dir */
  
    /* Recursive read */
    IF cFile[3] BEGINS 'D' AND lRecursive THEN
      RUN ReadDir(cFile[2], cFilter, lRecursive, lDetailedInfo).

  END. /* read a file */
  
  INPUT close.
END. /* ReadDir */
