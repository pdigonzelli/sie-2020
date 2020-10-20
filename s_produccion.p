/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_produccion.p                                     */
/****************************************************************************/
/*  Programa para actividades globales de los sistemas.                     */
/****************************************************************************/



/*--- PARAMETROS ---*/
DEFINE OUTPUT PARAMETER sal AS LOGICAL INITIAL NO.

/*--- VARIABLES GLOBALES DEL SISTEMA DE produccion-*/
/*---RUN ../s_core.p (OUTPUT sal). */
{packvars.i "NEW GLOBAL"}.


FIND FIRST usuarios_produccion WHERE usuarios_produccion.id_usuario = USERID("userdb") NO-LOCK NO-ERROR.
IF NOT AVAILABLE usuarios_produccion THEN RETURN.
ASSIGN vlc_suc_trabajo = usuarios_produccion.id_suc_trabajo.



sal = YES.
RETURN.
