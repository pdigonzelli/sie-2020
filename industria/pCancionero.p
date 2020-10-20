
DEFINE VARIABLE cDir              AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFile             AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFileStream       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFileMaster       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE chWordApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chDoc             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER    NO-UNDO.


/*cDir = 'E:\home\Facundo\personal\musica\Folklore Argentino Cancionero acordes para guitarra\'.*/
cDir = "D:\home\facundoj\personal\musica\canciones\ricardo arjona\".
cFileMaster = cDir + "arjona.doc".

CREATE "Word.application" chWordApplication.
chWordApplication:VISIBLE=TRUE.
chDoc = chWordApplication:Documents:ADD().

/*
FIND LAST cancionero NO-LOCK NO-ERROR.
IF AVAILABLE cancionero THEN
  i = cancionero.id_cancion + 1.
ELSE
  i = 1.
*/
INPUT FROM OS-DIR (cDir) ECHO.
CURRENT-WINDOW:WIDTH = 150.

REPEAT:
    IMPORT cFileStream.
    cFile = cDir + cFileStream.

    IF LENGTH(cFileStream) > 2 THEN DO:    /*considera archivos los 2 archivos de dos . y ..*/
      chWordApplication:SELECTION:InsertFile(cFile).
      chWordApplication:SELECTION:InsertBreak().
      
      /*RELEASE OBJECT chWordApplication.*/
      /*
      FIND FIRST cancionero WHERE cancionero.archivo = cFile NO-ERROR.
      IF AVAILABLE cancionero THEN DO:
        ASSIGN cancionero.impresiones = cancionero.impresiones + 1.
      END.
      ELSE DO:
        CREATE cancionero.
        ASSIGN cancionero.id_cancion  = i
               cancionero.titulo      = ENTRY(1, cFileStream, ".")
               cancionero.archivo     = cFileStream
               cancionero.impresiones = 1.
        i = i + 1.
      END.
      */
    END.
    
    /*DISP cFile FORMAT "X(140)" WITH WIDTH 150.*/
END.


    /*
    DISPLAY cFileStream FORMAT "X(30)" LABEL 'name of the file'
            FILE-INFO:FULL-PATHNAME FORMAT "X(30)" LABEL 'FULL-PATHNAME'
            FILE-INFO:PATHNAME FORMAT "X(20)" LABEL 'PATHNAME'
            FILE-INFO:FILE-TYPE FORMAT "X(5)" LABEL 'FILE-TYPE' WITH WIDTH 150.*/
