DEFINE INPUT PARAMETER p_mes AS INTEGER.
DEFINE INPUT PARAMETER p_idioma AS CHAR.
DEFINE OUTPUT PARAMETER p_nombre_mes AS CHAR.

CASE p_idioma:
    WHEN "ing" THEN DO:
        CASE p_mes:
            WHEN 1 THEN p_nombre_mes = "January".
            WHEN 2 THEN p_nombre_mes = "February".
            WHEN 3 THEN p_nombre_mes = "March".
            WHEN 4 THEN p_nombre_mes = "April".
            WHEN 5 THEN p_nombre_mes = "May".
            WHEN 6 THEN p_nombre_mes = "June".
            WHEN 7 THEN p_nombre_mes = "July".
            WHEN 8 THEN p_nombre_mes = "August".
            WHEN 9 THEN p_nombre_mes = "September".
            WHEN 10 THEN p_nombre_mes = "October".
            WHEN 11 THEN p_nombre_mes = "November".
            WHEN 12 THEN p_nombre_mes = "December".
        END CASE.
    END.
    WHEN "esp" THEN DO:
        CASE p_mes:
            WHEN 1 THEN p_nombre_mes = "Enero".
            WHEN 2 THEN p_nombre_mes = "Febrero".
            WHEN 3 THEN p_nombre_mes = "Marzo".
            WHEN 4 THEN p_nombre_mes = "Abril".
            WHEN 5 THEN p_nombre_mes = "Mayo".
            WHEN 6 THEN p_nombre_mes = "Junio".
            WHEN 7 THEN p_nombre_mes = "Julio".
            WHEN 8 THEN p_nombre_mes = "Agosto".
            WHEN 9 THEN p_nombre_mes = "Septiembre".
            WHEN 10 THEN p_nombre_mes = "Octubre".
            WHEN 11 THEN p_nombre_mes = "Noviembre".
            WHEN 12 THEN p_nombre_mes = "Diciembre".
        END CASE.
    END.
END CASE
