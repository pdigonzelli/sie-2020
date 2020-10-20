DEF VAR i AS INTEGER.

i = 6.
FOR EACH tareas WHERE 
    id_concepto[i] = 12  AND id_concepto_liq[i] <> 0   
     BREAK BY id_concepto[i]:
    DISPLAY id_tarea descripcion
         id_concepto[i] id_concepto_liq[i].
     UPDATE id_concepto_liq[i]. 
END.


/*FOR EACH tareas WHERE id_concepto[i] = 562:
    /*ASSIGN id_concepto_liq[i] = 1441. */
    DISPLAY id_concepto[i] id_concepto_liq[i].
END.*/ 

