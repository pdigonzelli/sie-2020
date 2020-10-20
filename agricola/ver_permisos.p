DEF BUFFER bpar FOR par_permisos.

FOR EACH par_permisos WHERE nombre_programa MATCHES "wliqoperativo.w":
    DISPLAY nombre_programa.
    UPDATE puede_ejecutar VIEW-AS EDITOR INNER-LINES 5 INNER-CHARS 60.
    

  /*  CREATE bpar.
    BUFFER-COPY par_permisos EXCEPT par_permisos.nombre_programa TO bpar.
    ASSIGN bpar.nombre_programa = "wliqconceptos.w". */
END.

/*
FOR EACH par_permisos WHERE puede_ejecutar MATCHES "*terribile*":
    DISPLAY nombre_programa.
    UPDATE puede_ejecutar VIEW-AS EDITOR INNER-LINES 5 INNER-CHARS 60.
END.
  */   
