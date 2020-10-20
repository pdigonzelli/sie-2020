
FOR EACH {1}._file WHERE NOT _hidden NO-LOCK:
    CREATE tt-tablas.
    ASSIGN
         tt-tablas.nombre = trim({1}._file._file-name)
         tt-tablas.dbase  = vdb.
END.



