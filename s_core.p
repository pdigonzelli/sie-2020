/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_core.p                                       */
/****************************************************************************/
/*  Agrega al PROPATH los directorios de Core en caso de que el sistema No  */
/*  sea Core y se est‚ ejecutando un m¢dulo de Core (se entiende?)          */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R¡os - Grupo Sauken S.A.           */
/*  FECHA CREACION.......:   03/06/2006                                     */
/*  REVISION.............:   1.00                                           */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- PARAMETROS ---*/
define output parameter sal as logical initial no format "Si/No".

/*--- SISTEMAS: (contacto, gessi, posmaster, nudelman, core) ---*/
{../sistema.i}

/*--- MODALIDAD: YES=desarrollo; NO=sistemas ---*/
{../modalidad.i}

/*--- VARIABLES DEL SISTEMA ---*/
{s_varsis.i}

/* DEFINO LA BARRA SEPARADORA SEGUN EL SISTEMA OPERATIVO */
define variable c-sep as char no-undo.
if opsys = "UNIX" then
    assign c-sep = "/".
else
    assign c-sep = "~\".

&IF "{&MODALIDAD}" = "yes" AND "{&SISTEMA}" <> "core" &THEN
   if lookup(vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "custom",     propath) = 0 and
      lookup(vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "gui",        propath) = 0 and
      lookup(vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "src",        propath) = 0 and
      lookup(vlc_dir_fuentes + "supervisor" + c-sep + "core",                        propath) = 0 and
      lookup(vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "supervisor", propath) = 0 then
         propath = vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "custom," +
                   vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "gui," +
                   vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "src," +
                   vlc_dir_fuentes + "supervisor" + c-sep + "core," +
                   vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                   propath.
&ENDIF

&IF "{&MODALIDAD}" = "no" AND "{&SISTEMA}" <> "core" &THEN
   if lookup(vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "custom",     propath) = 0 and
      lookup(vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "gui",        propath) = 0 and
      lookup(vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "src",        propath) = 0 and
      lookup(vlc_dir_objetos + "supervisor" + c-sep + "core",                        propath) = 0 and
      lookup(vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "supervisor", propath) = 0 then
         propath = vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "custom," +
                   vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "gui," +
                   vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "src," +
                   vlc_dir_objetos + "supervisor" + c-sep + "core," +
                   vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                   propath.
&ENDIF

sal = yes.
return.
