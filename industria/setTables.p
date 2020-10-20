 DEFINE TEMP-TABLE tt-tablas
    FIELD dbase  AS CHARACTER FORMAT "x(10)" COLUMN-LABEL "Base" 
    FIELD nombre AS CHARACTER FORMAT "x(25)" COLUMN-LABEL "Tablas". 


DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-tablas.

FOR EACH {1}._file WHERE NOT _hidden NO-LOCK:
    CREATE tt-tablas.
    ASSIGN
         tt-tablas.nombre = trim({1}._file._file-name)
         tt-tablas.dbase  = "{1}".
END.



