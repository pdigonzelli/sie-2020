/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_userdb.p                                     */
/****************************************************************************/
/*  Subprograma que devuelve el n£mero de entradas que contiene el archivo  */
/*  _user de la base de datos l¢gica que mapea el alias userdb.             */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os                               */
/*  FECHA CREACION.......:   19/01/95                                       */
/*  REVISION.............:   2.00                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

DEFINE OUTPUT PARAMETER a AS INTEGER INITIAL 0.

/*--- CUENTO USUARIOS POR BASE DE DATOS PARA DETERMINAR CUAL ES LA PRINCIPAL ---*/
FOR EACH userdb._user FIELDS (_userid) NO-LOCK:
  a = a + 1.
END.

/*--- EN VERSIàN OPENEDGE 10 LOS USUARIOS SE POPULAN/PROPAGAN EN TODAS LAS BASES DE DATOS ---*/
IF a > 0 AND CAN-FIND(FIRST userdb._file WHERE userdb._file._file-name = "par_bases") THEN
  a = 999999999.

RETURN.
