DEFINE VARIABLE cPandoraLog AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSong       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cEstacion   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iPos        AS INTEGER    NO-UNDO.
DEFINE VARIABLE i           AS INTEGER    NO-UNDO.
DEFINE VARIABLE j           AS INTEGER    NO-UNDO.

DEFINE TEMP-TABLE ttPlayList
  FIELD nro   AS INTEGER
  FIELD song  AS CHARACTER
  FIELD fecha AS DATE.

CURRENT-WINDOW:WIDTH = 200.
cPandoraLog = "D:\downloads\pandora\pandora.beta.7.4.1\pandorasJar.log".
cEstacion   = "Estacion Mana".
i = 1.

INPUT FROM VALUE(cPandoraLog).
REPEAT :
  IMPORT UNFORMATTED cLine.

  IF LENGTH(cLine) = 0 THEN DO:  
    /*continua si encuentra una linea en blanco*/
    NEXT.
  END.
  
  iPos = INDEX(cLine, "ripped file to:").
  IF iPos > 0 THEN DO:        

   CREATE ttPlayList.
   ASSIGN ttPlayList.nro   = i
          ttPlayList.song  = REPLACE(cLine, "ripped to file: ", "")
          ttPlayList.fecha = TODAY.
   i = i + 1.
  
  END.
END.

INPUT CLOSE.

j = 1.
FOR LAST pandora_play_list.
  j = pandora_play_list.orden + 1.
END.

FOR EACH ttPlayList.
  DO i = 1 TO NUM-ENTRIES(song, "\"):
    cSong = ENTRY(i, song, "\").
  END.
  
  FIND FIRST pandora_play_list
       WHERE pandora_play_list.cancion = cSong
       NO-LOCK NO-ERROR.
  IF NOT AVAILABLE pandora_play_list THEN DO:
    CREATE pandora_play_list.
    ASSIGN pandora_play_list.id_playlist  = 6
           pandora_play_list.orden        = j
           pandora_play_list.cancion      = cSong
           pandora_play_list.fecha        = TODAY
           pandora_play_list.estacion     = cEstacion
           .
    j = j + 1.
  END.


END.

FOR EACH pandora_play_list.
  DISP orden cancion FORMAT "x(100)" pandora_play_list.fecha estacion WITH WIDTH 200.
END.

/*
FOR EACH ttPlayList.
  DISP ttPlayList.song FORMAT "x(100)" WITH WIDTH 150.
END.
*/


