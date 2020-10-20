/****************************************************************************/
/*  NOMBRE PROGRAMA......:   a_muser2.p -similar a h_paspop.p-              */
/****************************************************************************/
/*  Propaga la tabla de seguridad y usuarios a todas las bases de datos     */
/*  conectadas                                                              */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  FECHA CREACION.......:   18/01/2005 -desde Alpa Corral-                 */
/*  REVISION.............:   1.10                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:   20/03/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   AgreguÇ controles                              */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- VARIABLES ----*/
define variable respuesta as logical no-undo.

/*--- CONTROL: QUE NO SEA LA MISMA BASE DE DATOS ---*/
if ldbname("passworddb") = ldbname("userdb") then
  return.

/*--- CONTROL: QUE EL USUARIO CONECTADO TENGA PERMISOS PARA ADMINISTRAR LA SEGURIDAD ---*/
run "prodict/_dctadmn.p" (input userid("passworddb"), output respuesta).
if not respuesta then
  do:
    bell.
    message color white/red
            "Usted No tiene Privilegios de Administrador de Seguridad para esta Base de Datos !!!" skip
            "Referencia para Sistemas - Base de Datos:" ldbname("passworddb")
            view-as alert-box error.
    return.
  end.

/*--- CONTROL: QUE LA BASE DE DATOS NO ESTE CONECTADA COMO SOLO LECTURA ---*/
if can-do("READ-ONLY",dbrestrictions("passworddb")) then
  do:
    bell.
    message color white/red
            "La Base de Datos est† Conectada como S¢lo Lectura !!!" skip
            "Referencia para Sistemas - Base de Datos:" ldbname("passworddb")
            view-as alert-box error.
    return.
  end.

/*--- CONTROL: QUE NO SEA UN USUARIO EN BLANCO QUIEN EJECUTE ESTE PROGRAMA ---*/
if userid("passworddb") = "" and can-find(first passworddb._user) then
  do:
    bell.
    message color white/red
            "Este Programa No Puede ser Ejecutado por un Usuario en Blanco en esta Base de Datos !!!"
            "Referencia para Sistemas - Base de Datos:" ldbname("passworddb")
            view-as alert-box error.
    return.
  end.

/*--- CONTROL: LA BASE DE DATOS PRINCIPAL DE SEGURIDAD DEBE SER PROGRESS ---*/
if dbtype("passworddb") <> "PROGRESS" then
  do:
    bell.
    message color white/red
            "La Base de Datos Principal de Seguridad debe ser Progress !!!" skip
            "Referencia para Sistemas - Base de Datos:" ldbname("passworddb")
            view-as alert-box error.
    return.
  end.

/*--- PROCESO PRINCIPAL: ELIMINACI‡N DE USUARIOS ---*/
for each passworddb._user where passworddb._user._userid <> "computos":
  
  /* BORRO TODOS, PUESTO QUE HOY POR HOY TODOS SON ADMINISTRADORES DE SEGURIDAD
     Y COMO YO SOY ADMINISTRACI‡N DE SEGURIDAD, PISO TODOS, TODAS LAS PASSWORDS.
     AS÷ FUNCIONA MEJOR LA COSA. JUAN CARLOS 20/03/2005
  run "prodict/_dctadmn.p" (input passworddb._user._userid, output respuesta).
  if not respuesta then
  */

    delete passworddb._user no-error.
end.

/*--- SI NO EXISTE EL USUARIO computos LO AGREGO ---*/
if not can-find(first passworddb._user where passworddb._user._userid = "computos") then
  do:
    find userdb._user where userdb._user._userid = "computos" no-lock no-error.
    if available userdb._user then
      do transaction:
        create passworddb._user.
        assign passworddb._user._userid    = userdb._user._userid
               passworddb._user._user-name = userdb._user._user-name
               passworddb._user._password  = userdb._user._password
               passworddb._user._user-misc = userdb._user._user-misc.
        release passworddb._user.
      end.
  end.

/*--- PROCESO PRINCIPAL: AGREGADOS Y CAMBIOS ---*/
for each userdb._user where userdb._user._userid <> "computos" and
                            not can-find(first passworddb._user where passworddb._user._userid = userdb._user._userid) no-lock:
  create passworddb._user.
  assign passworddb._user._userid    = userdb._user._userid
         passworddb._user._user-name = userdb._user._user-name
         passworddb._user._password  = userdb._user._password
         passworddb._user._user-misc = userdb._user._user-misc.
  release passworddb._user.
end.
release passworddb._user.

return.
