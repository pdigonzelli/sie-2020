FOR EACH par_permisos WHERE nombre_programa = "x_sdogec.w" :
   UPDATE nombre_programa FORMAT "x(60)"
       puede_ejecutar VIEW-AS EDITOR INNER-LINES 10 INNER-CHARS 70.
END.
