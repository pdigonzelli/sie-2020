DEF INPUT PARAMETER v-desde AS DATE.
DEF INPUT PARAMETER  v-hasta AS DATE.
DEF OUTPUT PARAMETER v-desde-g1 AS DATE.
DEF OUTPUT PARAMETER v-hasta-g1 AS DATE.
DEF OUTPUT PARAMETER v-g1 AS INTEGER.
DEF OUTPUT PARAMETER v-desde-g2 AS DATE.
DEF OUTPUT PARAMETER v-hasta-g2 AS DATE.
DEF OUTPUT PARAMETER v-g2 AS INTEGER.
DEF OUTPUT PARAMETER v-desde-g3 AS DATE.
DEF OUTPUT PARAMETER v-hasta-g3 AS DATE.
DEF OUTPUT PARAMETER v-g3 AS INTEGER.



DEF VAR i AS INTEGER.
DEF VAR v-primer-dia AS INTEGER.


v-primer-dia = WEEKDAY(v-desde).
CASE v-primer-dia:
    WHEN 1 THEN /* Domingo */
    DO:
        v-desde-g1 = v-desde + 1.
        v-hasta-g1 = v-desde + 6.
    
    END.
    WHEN 2 THEN /* Lunes */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde + 5.
    END.
    WHEN 3 THEN /* Martes */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde + 4.
    END.
    WHEN 4 THEN /* Miercoles */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde + 3.
    END.
    
    WHEN 5 THEN /* Jueves */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde + 2.
    END.
    WHEN 6 THEN /* Viernes */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde + 1.
    END.

    WHEN 7 THEN /* Sabado */
    DO:
        v-desde-g1 = v-desde.
        v-hasta-g1 = v-desde.
    END.
END CASE.

    v-g1 = (v-hasta-g1 - v-desde-g1) + 1. 

    v-desde-g2 = v-hasta-g1 + 2.
    v-hasta-g2 = v-desde-g2 + 5.
    v-g2 = (v-hasta-g2 - v-desde-g2) + 1. 

    IF WEEKDAY(v-hasta) = 1 THEN v-hasta = v-hasta - 1.

    IF v-hasta-g2 < v-hasta THEN
    DO:
        v-desde-g3 = v-hasta-g2 + 2.
        v-hasta-g3 = v-hasta.
        v-g3 = (v-hasta-g3 - v-desde-g3) + 1. 
    END.


