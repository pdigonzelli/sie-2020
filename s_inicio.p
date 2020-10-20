/****************************************************************************/
/*  NOMBRE PROGRAMA......:   s_inicio.p                                     */
/****************************************************************************/
/*  Programa de arranque. Crea el alias 'userdb' para la base de datos que  */
/*  que contenga la definici¢n de usuarios, carga las variable globales y   */
/*  ejecuta el programa s_login.p o z_login.w seg£n corresponda.            */
/****************************************************************************/
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  FECHA CREACION.......:   19/01/95                                       */
/*  REVISION.............:   2.99.3                                         */
/****************************************************************************/
/*  FECHA MODIFICACION...:   21/04/98                                       */
/*  PROGRAMADOR..........:   Claudio Sandri                                 */
/*  MODIFICACION.........:   Soporte para plataforma WIN32.                 */
/****************************************************************************/
/*  FECHA MODIFICACION...:   18/02/2002                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Mejor selecci¢n de Host Principal de Aplicaci¢n*/
/*                           Mayores controles antes de iniciar sesi¢n      */
/****************************************************************************/
/*  FECHA MODIFICACION...:   30/07/2002                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Variables con PATH de E-Mail y HTTP            */
/****************************************************************************/
/*  FECHA MODIFICACION...:   28/09/2002                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Soporte a Blat y Getmail                       */
/****************************************************************************/
/*  FECHA MODIFICACION...:   05/10/2004                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Si MODALIDAD=NO (sistemas) no se validan algu- */
/*                           nos directorios                                */
/****************************************************************************/
/*  FECHA MODIFICACION...:   05/10/2004                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Se detecta ahora Mozilla Firefox y Thunderbird */
/****************************************************************************/
/*  FECHA MODIFICACION...:   08/11/2004 -en memoria de Francisco Siri-      */
/*  PROGRAMADOR..........:   Juan Carlos R°os             Grupo SÉuken      */
/*  MODIFICACION.........:   Auto Instalaci¢n                               */
/****************************************************************************/
/*  FECHA MODIFICACION...:   20/12/2004                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os             Grupo SÉuken      */
/*  MODIFICACION.........:   Soporte a Open Office 2                        */
/****************************************************************************/
/*  FECHA MODIFICACION...:   05/01/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os             Grupo SÉuken      */
/*  MODIFICACION.........:   Soporte simult†neo Contacto, Gessi, POS-Master */
/*                           Nudelman                                       */
/****************************************************************************/
/*  FECHA MODIFICACION...:   17/01/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Soporte MISCELANEOS                            */
/****************************************************************************/
/*  FECHA MODIFICACION...:   18/01/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   Valida la existencia del usuario "computos"    */
/*                           Esta funci¢n se traslad¢ a 's_login.p' y       */
/*                           'z_login.w' por el alias 'userbd'              */
/****************************************************************************/
/*  FECHA MODIFICACION...:   31/01/2005 -nev¢ en el Champaqu°-              */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   AgreguÇ {modalidad.i} y {sistema.i}            */
/****************************************************************************/
/*  FECHA MODIFICACION...:   20/03/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   AgreguÇ autoxonexi¢n de BD al inicio del sis-  */
/*  tema. Ahora se puede conectar a una base de datos (la que contiene los  */
/*  usuarios, la principal) y a partir de Çsta conectar las restantes (las  */
/*  BD que son necesarias tener conectadas siempre). Estas restantes BD pue-*/
/*  den tener desactivado el acceso de usuario en blanco, para dar mayor se-*/
/*  guridad al sistema (esta auxtoconexi¢n se hizo por este motivo). Del    */
/*  mismo modo, la BD de inicio puede conectarse a un usuario desde el .PF  */
/*  poniendo "-U usuario -P contrase§a" y de esta manera tener todas las BD */
/*  en modo "desactiva el acceso con usuario en blanco"                     */
/****************************************************************************/
/*  FECHA MODIFICACION...:   27/05/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   Omisi¢n: Faltaba agregar "smartgraph"          */
/****************************************************************************/
/*  FECHA MODIFICACION...:   26/10/2005                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   Soporte a SIE-Core                             */
/****************************************************************************/
/*  FECHA MODIFICACION...:   05/02/2006                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Grupo SÉuken S.A.           */
/*  MODIFICACION.........:   Optimizaci¢n en validaci¢n de directorios      */
/****************************************************************************/
/*  FECHA MODIFICACION...:   08/03/2006                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Se agreg¢ la detecci¢n de Editor HTML y progra-*/
/*                           ma de Env°o Masivo de E-Mails                  */
/****************************************************************************/
/*  FECHA MODIFICACION...:   24/06/2006                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os                               */
/*  MODIFICACION.........:   Se modific¢ la forma de asignar "vlc_dir_temp" */
/****************************************************************************/
/*  FECHA MODIFICACION...:   17/01/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Soporte OpenEdge 10, simplicaci¢n en variables */
/*                           de entorno                                     */
/****************************************************************************/
/*  FECHA MODIFICACION...:   24/01/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Las variables globales est†n en "s_inicio.i"   */
/****************************************************************************/
/*  FECHA MODIFICACION...:   24/01/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Se agrega soporte a SMTP Sender en Blat.exe    */
/****************************************************************************/
/*  FECHA MODIFICACION...:   15/02/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Detecci¢n de Acrobat o Acrobat Reader          */
/****************************************************************************/
/*  FECHA MODIFICACION...:   19/02/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Instalaci¢n de nuevo ftp.exe para Windows      */
/****************************************************************************/
/*  FECHA MODIFICACION...:   19/02/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Soporte de par†mtros de entrada (compilar)     */
/****************************************************************************/
/*  FECHA MODIFICACION...:   17/03/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Copio ejecutables de Windows a "usuarios"      */
/****************************************************************************/
/*  FECHA MODIFICACION...:   26/03/2007                                     */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Correcciones menores y mensaje de inicio       */
/****************************************************************************/
/*  FECHA MODIFICACION...:   16/04/2007 -cumplea§os de Adrianita-           */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   M†s detecciones en Linux. Uso de OE10 /etc/    */
/****************************************************************************/
/*  FECHA MODIFICACION...:   23/05/2007 -cumplea§os de Juancito-            */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Agrega barra al final de los directorios       */
/****************************************************************************/
/*  FECHA MODIFICACION...:   04/05/2008 -que fin de semana-                 */
/*  PROGRAMADOR..........:   Juan Carlos R°os - Software Solutions           */
/*  MODIFICACION.........:   Se validan las variable de entorno             */
/****************************************************************************/
/*  FECHA MODIFICACION...:                                                  */
/*  PROGRAMADOR..........:                                                  */
/*  MODIFICACION.........:                                                  */
/*                                                                          */
/****************************************************************************/

/*--- VARIABLES GLOBALES DE TODOS LOS SISTEMAS ---*/
{../s_inicio.i "new global"}

/*--- SISTEMAS: (contacto, gessi, posmaster, nudelman, core) ---*/
{../sistema.i}

/*--- MODALIDAD: YES=desarrollo; NO=sistemas ---*/
{../modalidad.i}
 
{../s_ambiente.i "new global shared"}



DEFINE NEW SHARED VAR X_path  AS CHARACTER .

/*--- INICIO SESION ---*/
PAUSE 0 BEFORE-HIDE.
SESSION:DATA-ENTRY-RETURN = YES.
SESSION:TIME-SOURCE = "general".
input clear.

/*--- DEFINO TITULO ---*/
DEFINE VARIABLE c-tit AS CHARACTER FORMAT "x(50)" NO-UNDO.
&IF "{&SISTEMA}" = "gessi" &THEN
  c-tit = "SIE-ERP GESSI".
&ENDIF
&IF "{&SISTEMA}" = "posmaster" &THEN
  c-tit = "SIE-ERP POS-MASTER".
&ENDIF
&IF "{&SISTEMA}" = "nudelman" &THEN
  c-tit = "SAUKEN-LAW".
&ENDIF
&IF "{&SISTEMA}" = "contacto" &THEN
  c-tit = "SIE-CRM".
&ENDIF
&IF "{&SISTEMA}" = "core" &THEN
  c-tit = "SIE-CORE".
&ENDIF
c-tit = "Iniciando la Aplicaci¢n " + c-tit + " !!!".
&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  c-tit = FILL(" ", INTEGER((50 - length(c-tit)) / 2)) + c-tit.
&ELSE
  c-tit = "  " + c-tit.
&ENDIF

/*--- PARµMETROS DE ENTRADA DE LA SESI‡N ---*/

DEFINE VARIABLE x_parametros AS CHARACTER NO-UNDO.
x_parametros = SESSION:PARAMETER.


IF NUM-ENTRIES(x_parametros) > 2 THEN
DO:
    x_ambiente = ENTRY(3,x_parametros).
    IF ENTRY(1,x_parametros) = "" THEN
        x_parametros = "".
END.
/*--- MENSAJE DE INICIO ---*/
IF x_parametros = "" THEN
  DISPLAY c-tit WITH FRAME inicio OVERLAY NO-LABELS
    &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      ROW 18 CENTERED COLOR white/red.
    &ELSE
      no-box row 1 column 1 width 54 font 9 fgcolor 4 view-as dialog-box.
    &ENDIF

/*--- VARIABLES LOCALES ---*/
DEFINE VARIABLE a AS INTEGER INITIAL 0.
DEFINE VARIABLE b AS INTEGER INITIAL 0.
DEFINE VARIABLE c AS INTEGER INITIAL 0.
DEFINE VARIABLE d AS INTEGER INITIAL 0.

DEFINE VARIABLE x_path_aplicacion    AS CHARACTER.
DEFINE VARIABLE x_version_aplicacion AS INTEGER.
DEFINE VARIABLE x_nombre_comercial   AS CHARACTER.
DEFINE VARIABLE x_proversion         AS INTEGER.

DEFINE VARIABLE a_smtp_server LIKE par_estado.smtp_server.
DEFINE VARIABLE a_smtp_sender LIKE par_estado.smtp_sender_email.
DEFINE VARIABLE a_pop3_server LIKE par_estado.pop3_server.
DEFINE VARIABLE unixusr AS CHARACTER.

DEFINE VARIABLE salida AS LOGICAL.
DEFINE VARIABLE aux    AS CHARACTER.
DEFINE VARIABLE tipo   AS CHARACTER.

DEFINE VARIABLE dirwin AS CHARACTER.
DEFINE VARIABLE dirsvs AS CHARACTER.
DEFINE VARIABLE dirusr AS CHARACTER.

DEFINE VARIABLE v1 AS CHARACTER.
DEFINE VARIABLE v2 AS CHARACTER.

/*--- VARIABLES DE CONEXION DE BASES DE DATOS INICIALES ---*/
DEFINE VARIABLE respuesta AS LOGICAL   NO-UNDO.
DEFINE VARIABLE archivo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE conexion  AS CHARACTER NO-UNDO.
DEFINE VARIABLE esdemo    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE contador  AS INTEGER   NO-UNDO.

/*--- CONTROLO QUE EXISTE AL MENOS UNA BASE DE DATOS CONECTADA ---*/
IF NUM-DBS = 0 THEN
 DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE COLOR white/red "No se ha conectado a ninguna Base de Datos !!!" view-as alert-box error.
    QUIT.
 END.

/*--- EXPLORO EN QUE BASE DE DATOS SE HAN DEFINIDO LOS USUARIOS ---*/
DO a = 1 TO NUM-DBS:
  CREATE ALIAS userdb FOR DATABASE VALUE(LDBNAME(a)).
  RUN ../s_userdb.p  (OUTPUT b).
  IF b >= c THEN ASSIGN c = b d = a.
  DELETE ALIAS userdb.
END.

/*--- CONTROLO QUE EXISTAN USUARIOS DEFINIDOS ---*/
IF c = 0 THEN
 DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE COLOR white/red "No se han definido usuarios de sistemas !!!" view-as alert-box error.
    QUIT.
 END.

/*--- CREO EL ALIAS USERDB PARA LA BASE DE DATOS QUE CONTIENE USUARIOS ---*/
CREATE ALIAS userdb FOR DATABASE VALUE(LDBNAME(d)). 

/*--- VERSION DE PROGRESS EJECUTµNDOSE ---*/
x_proversion = INTEGER(SUBSTRING(PROVERSION,1, INDEX(PROVERSION,".") - 1)).

/*--- CARGO VARIABLE DLC DESDE ENTORNO, REGISTRO DE WINDOWS O /etc/progress EN UNIX  ---*/

x_dlc = OS-GETENV("DLC").
IF x_dlc <> "" AND x_dlc <> ? AND search(x_dlc + "/version") = ? THEN
  x_dlc = ?.
IF (x_dlc = "" OR x_dlc = ?) AND x_proversion >= 10 THEN
  DO:
    IF OPSYS = "WIN32" THEN
      DO:
        LOAD "Software/PSC/OpenEdge/" + PROVERSION BASE-KEY "HKEY_LOCAL_MACHINE" NO-ERROR.
        USE "Software/PSC/OpenEdge/" + PROVERSION NO-ERROR.
        GET-KEY-VALUE SECTION "" KEY "DLC" VALUE x_dlc.
        UNLOAD "Software/PSC/OpenEdge/" + PROVERSION NO-ERROR.
        IF x_dlc = ? THEN
         DO:
           LOAD "Software/PSC/PROGRESS/" + PROVERSION + "/Startup" BASE-KEY "HKEY_LOCAL_MACHINE" NO-ERROR.
           USE "Software/PSC/PROGRESS/" + PROVERSION + "/Startup" NO-ERROR.
           GET-KEY-VALUE SECTION "" KEY "DLC" VALUE x_dlc.
           UNLOAD "Software/PSC/PROGRESS/" + PROVERSION + "/Startup" NO-ERROR.
         END.
      END.
    ELSE
      DO:
        IF SEARCH("/etc/progress") <> ? THEN
          DO:
            INPUT through "cat /etc/progress" no-echo.
            SET x_dlc FORMAT "x(78)" WITH FRAME x_dlc NO-BOX NO-LABELS.
            INPUT close.
          END.
      END.
  END.
  
  
/*--- CARGO VALORES A VARIABLE GLOBAL VLC_HOST ---*/
IF OPSYS = "UNIX" THEN
 DO:

    /*--- TOMO NOMBRE DEL HOST ---*/
    INPUT through "uname -n" no-echo.
    SET vlc_host FORMAT "x(20)" WITH FRAME host NO-BOX NO-LABELS.
    INPUT close.

    /*--- TOMO NOMBRE DEL USUARIO (se usa en procedimiento de instalaci¢n) ---*/
    IF OS-GETENV("DISPLAY") = "" OR os-getenv("DISPLAY") = ? THEN
      DO:
        INPUT through "who -m -s" no-echo.
        SET unixusr FORMAT "x(20)" WITH FRAME unixusr NO-BOX NO-LABELS.
        INPUT close.
        IF unixusr <> "" AND unixusr <> ? THEN
          unixusr = TRIM(SUBSTRING(unixusr, 1 , INDEX(unixusr," ") - 1)).
      END.
 END.
ELSE
    vlc_host = "WIN32". /*--- SE CAMBIA MAS ADELANTE ---*/

/*--- CARGO HOSTS DE APLICACIONES Y OTROS ---*/
FIND FIRST par_estado NO-LOCK NO-ERROR.
IF NOT AVAILABLE par_estado THEN
  DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE "No Existe el Registro de Estado del Sistema (par_estado) !!!" SKIP
            "No se puede continuar la ejecuci¢n del mismo !!!" VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END.

IF par_estado.fecha_caducidad <> ? AND
   TODAY  >= par_estado.fecha_caducidad AND   
   TODAY  <= par_estado.fecha_caducidad + 20  THEN
   DO:
        BELL.
        MESSAGE 'Se acerca el fin  de licenciamiento del software. Por favor regularice su situacion' VIEW-AS ALERT-BOX INFORMATION.
   END.

IF par_estado.fecha_caducidad <> ? AND
   TODAY  >= par_estado.fecha_caducidad AND   
   TODAY  > par_estado.fecha_caducidad + 20  THEN
   DO:
        BELL.
        MESSAGE 'Se llego al fin  de licenciamiento del software. Por favor regularice su situacion' VIEW-AS ALERT-BOX ERROR.
        QUIT.
   END.
ASSIGN vlc_host_app  = par_estado.host_aplicacion
       a_smtp_server = par_estado.smtp_server
       a_smtp_sender = par_estado.smtp_sender_email
       a_pop3_server = par_estado.pop3_server.
IF a_smtp_sender = "" OR a_smtp_sender = ? THEN
  ASSIGN a_smtp_sender = "default_user".
IF par_estado.accesos = ? THEN esdemo = NO. ELSE esdemo = YES.

&IF "{&SISTEMA}" = "gessi" OR "{&SISTEMA}" = "posmaster" OR "{&SISTEMA}" = "contacto" &THEN
  ASSIGN v_dia_de_pago = par_estado.dia_de_pago.
&ENDIF

IF x_parametros = '' THEN
DO:
/*--- CARGO VALORES A VARIABLES GLOBALES RELACIONADAS CON FILE SERVER ---*/
    IF OPSYS = "UNIX" THEN
    DO:
     
    /*--- AL SER UNIX EL NOMBRE DEL HOST DEBE COINCIDIR ---*/
        FIND par_file_server WHERE par_file_server.id_opsys = "UNIX" AND par_file_server.file_server = vlc_host NO-LOCK NO-ERROR.
        IF NOT AVAILABLE par_file_server THEN
        DO:
            BELL.
            HIDE FRAME inicio NO-PAUSE.
            MESSAGE COLOR white/red "Host UNIX no definido en el sistema !!!" view-as alert-box error.
            QUIT.
        END.
    END.
    
    ELSE
        DO:
        
            /*--- VARIABLE DE CONTROL ---*/
            vlc_file_server = ?.
        
            /*--- INTENTO AUTENTICAR PRIMERO EL HOST DE LA APLICACION ---*/
            FOR EACH par_file_server WHERE par_file_server.id_opsys = "WIN32" AND par_file_server.file_server = vlc_host_app AND par_file_server.autenticacion <> "" NO-LOCK:
              IF SEARCH(par_file_server.autenticacion) <> ? THEN
               DO:
                  vlc_file_server = par_file_server.file_server.
                  LEAVE.
               END.
            END.
        
            /*--- SI NO SE AUTENTICO HOST DE APLICACION, INTENTO CON OTROS ---*/
            IF vlc_file_server = ? THEN
              FOR EACH par_file_server WHERE par_file_server.id_opsys = "WIN32" AND par_file_server.autenticacion <> "" NO-LOCK:
                IF SEARCH(par_file_server.autenticacion) <> ? THEN
                  DO:
                    vlc_file_server = par_file_server.file_server.
                    LEAVE.
                  END.
              END.
        
            /*--- SI NO SE AUTENTICO, CARGO DEFAULT ---*/
            IF vlc_file_server = ? THEN
              DO:
                FIND FIRST par_file_server WHERE par_file_server.id_opsys = "WIN32" AND par_file_server.autenticacion = "" NO-LOCK NO-ERROR.
                IF NOT AVAILABLE par_file_server THEN
                  DO:
                    BELL.
                    HIDE FRAME inicio NO-PAUSE.
                    MESSAGE COLOR white/red "Equipo WIN32 no definido en el sistema !!!" view-as alert-box error.
                    QUIT.
                  END.
                vlc_file_server = par_file_server.file_server.
              END.
        
            /*--- LEO FILE SERVER ---*/
            FIND par_file_server WHERE par_file_server.id_opsys = "WIN32" AND par_file_server.file_server = vlc_file_server NO-LOCK.
           
        END.
    
    /*--- ASIGNO VALORES DEL FILE SERVER ---*/
        ASSIGN vlc_file_server = par_file_server.file_server
           vlc_dir_fuentes = par_file_server.dir_fuentes
           vlc_dir_objetos = par_file_server.dir_objetos
           vlc_dir_spool   = par_file_server.dir_spool
           vlc_dir_temp    = par_file_server.dir_temp
           vlc_dir_i-o     = par_file_server.dir_i-o
           vlc_dir_bkp     = par_file_server.dir_bkp.
           
      END.
ELSE
    IF ENTRY(1,x_parametros) = 'normal' THEN
       ASSIGN        vlc_dir_fuentes = ENTRY(2,x_parametros) + '/desarrollo/'
                     vlc_dir_objetos = ENTRY(2,x_parametros) + '/objetos/'
                     vlc_dir_spool   = ENTRY(2,x_parametros) + '/spool/'
                     vlc_dir_temp    = ENTRY(2,x_parametros) + '/temp/'
                     vlc_dir_i-o     = ENTRY(2,x_parametros) + '/transfer/'
                     vlc_dir_bkp     = ENTRY(2,x_parametros) + '/temp/'.
    ELSE
        IF ENTRY(1,x_parametros) = 'normal&fuentes' THEN
           ASSIGN        vlc_dir_fuentes = ENTRY(2,x_parametros) + '/' + entry(3,x_parametros) + '/' 
                     vlc_dir_objetos = ENTRY(2,x_parametros) + '/objetos/'
                     vlc_dir_spool   = ENTRY(2,x_parametros) + '/spool/'
                     vlc_dir_temp    = ENTRY(2,x_parametros) + '/temp/'
                     vlc_dir_i-o     = ENTRY(2,x_parametros) + '/transfer/'
                     vlc_dir_bkp     = ENTRY(2,x_parametros) + '/temp/'.
                     
                     
       
/*--- VERIFICO DIRECTORIO DE TEMPORARIOS ---*/
IF OPSYS = "UNIX" AND (vlc_dir_temp = ? OR vlc_dir_temp = "") THEN
  vlc_dir_temp = "/tmp/".
IF OPSYS = "WIN32" AND vlc_dir_temp = "%TEMP%" AND os-getenv("TEMP") <> ? THEN
  vlc_dir_temp = OS-GETENV("TEMP").
IF OPSYS = "WIN32" AND vlc_dir_temp = "%TMP%" AND os-getenv("TMP") <> ? THEN
  vlc_dir_temp = OS-GETENV("TMP").
IF OPSYS = "WIN32" AND (vlc_dir_temp = ? OR vlc_dir_temp = "") THEN
  vlc_dir_temp = OS-GETENV("TEMP").
IF vlc_dir_temp = ? OR vlc_dir_temp = "" THEN
  vlc_dir_temp = SESSION:TEMP-DIRECTORY.
/*--- CARGO VALORES A VARIABLES GLOBALES RELACIONADAS CON PRINT SERVER ---*/
IF OPSYS = "UNIX" THEN
 DO:

    /*--- AL SER UNIX EL NOMBRE DEL HOST DEBE COINCIDIR ---*/
    FIND FIRST par_print_server WHERE par_print_server.id_opsys = "UNIX" NO-LOCK NO-ERROR.
    IF NOT AVAILABLE par_print_server THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Impresi¢n sobre UNIX no definida en el sistema !!!" view-as alert-box error.
        QUIT.
     END.

 END.

ELSE

 DO:

    /*--- VARIABLE DE CONTROL ---*/
    vlc_print_server = ?.

    /*--- INTENTO AUTENTICAR PRIMERO EL HOST DE LA APLICACION ---*/
    FOR EACH par_print_server WHERE par_print_server.id_opsys = "WIN32" AND par_print_server.print_server = vlc_host_app AND par_print_server.autenticacion <> "" NO-LOCK:
      IF SEARCH(par_print_server.autenticacion) <> ? THEN
       DO:
          vlc_print_server = par_print_server.print_server.
          LEAVE.
       END.
    END.

    /*--- SI NO SE AUTENTICO HOST DE APLICACION, INTENTO CON OTROS ---*/
    IF vlc_print_server = ? THEN
      FOR EACH par_print_server WHERE par_print_server.id_opsys = "WIN32" AND par_print_server.autenticacion <> "" NO-LOCK:
        IF SEARCH(par_print_server.autenticacion) <> ? THEN
         DO:
            vlc_print_server = par_print_server.print_server.
            LEAVE.
         END.
      END.

    /*--- SI NO SE AUTENTICO, CARGO DEFAULT ---*/
    IF vlc_print_server = ? THEN
     DO:
        FIND FIRST par_print_server WHERE par_print_server.id_opsys = "WIN32" AND par_print_server.autenticacion = "" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE par_print_server THEN
         DO:
            BELL.
            HIDE FRAME inicio NO-PAUSE.
            MESSAGE COLOR white/red "Impresi¢n sobre WIN32 no definida en el sistema !!!" view-as alert-box error.
            QUIT.
         END.
        vlc_print_server = par_print_server.print_server.
     END.

    /*--- LEO PRINT SERVER ---*/
    FIND par_print_server WHERE par_print_server.id_opsys = "WIN32" AND par_print_server.print_server = vlc_print_server NO-LOCK.

 END.

/*--- ASIGNO VALORES DEL PRINT SERVER ---*/
ASSIGN vlc_print_server = par_print_server.print_server
       vlc_comando      = par_print_server.comando.

/*--- WINDOWS ---*/
IF vlc_host = "WIN32" AND vlc_file_server <> "WIN32" THEN
  vlc_host = vlc_file_server.

/*--- VERIFICO QUE TODOS LOS DIRECTORIOS TERMINEN CON UNA BARRA ---*/
IF NOT (SUBSTRING(vlc_dir_fuentes,LENGTH(vlc_dir_fuentes),1) = "/" OR substring(vlc_dir_fuentes,LENGTH(vlc_dir_fuentes),1) = "~\") THEN
  ASSIGN vlc_dir_fuentes = vlc_dir_fuentes + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").
IF NOT (SUBSTRING(vlc_dir_objetos,LENGTH(vlc_dir_objetos),1) = "/" OR substring(vlc_dir_objetos,LENGTH(vlc_dir_objetos),1) = "~\") THEN
  ASSIGN vlc_dir_objetos = vlc_dir_objetos + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").
IF NOT (SUBSTRING(vlc_dir_spool,LENGTH(vlc_dir_spool),1) = "/" OR substring(vlc_dir_spool,LENGTH(vlc_dir_spool),1) = "~\") THEN
  ASSIGN vlc_dir_spool = vlc_dir_spool + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").
IF NOT (SUBSTRING(vlc_dir_i-o,LENGTH(vlc_dir_i-o),1) = "/" OR substring(vlc_dir_i-o,LENGTH(vlc_dir_i-o),1) = "~\") THEN
  ASSIGN vlc_dir_i-o = vlc_dir_i-o + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").

/*--- VERIFICO QUE TODOS LOS DIRECTORIOS TENGAN LA BARRA QUE CORRESPONDE AL SISTEMA OPERATIVO ---*/
IF OPSYS = "UNIX" THEN
  ASSIGN vlc_file_server = REPLACE(vlc_file_server, "~\", "/")
         vlc_dir_fuentes = REPLACE(vlc_dir_fuentes, "~\", "/")
         vlc_dir_objetos = REPLACE(vlc_dir_objetos, "~\", "/")
         vlc_dir_spool   = REPLACE(vlc_dir_spool,   "~\", "/")
         vlc_dir_temp    = REPLACE(vlc_dir_temp,    "~\", "/")
         vlc_dir_i-o     = REPLACE(vlc_dir_i-o,     "~\", "/")
         vlc_dir_bkp     = REPLACE(vlc_dir_bkp,     "~\", "/").
ELSE
  ASSIGN vlc_file_server = REPLACE(vlc_file_server, "/", "~\")
         vlc_dir_fuentes = REPLACE(vlc_dir_fuentes, "/", "~\")
         vlc_dir_objetos = REPLACE(vlc_dir_objetos, "/", "~\")
         vlc_dir_spool   = REPLACE(vlc_dir_spool,   "/", "~\")
         vlc_dir_temp    = REPLACE(vlc_dir_temp,    "/", "~\")
         vlc_dir_i-o     = REPLACE(vlc_dir_i-o,     "/", "~\")
         vlc_dir_bkp     = REPLACE(vlc_dir_bkp,     "/", "~\").

/*--- VERIFICO QUE EXISTAN LOS DIRECTORIOS ---*/
/*
OUTPUT to "Test".
PUT UNFORMATTED "Test~n".
OUTPUT close.
*/
/* SPOOL */

RUN valido_directorio (INPUT vlc_dir_spool, OUTPUT salida).
IF NOT salida THEN
  DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE COLOR white/red "El Directorio:" vlc_dir_spool "No Existe o No es Accesible !!!" view-as alert-box error.
    RUN muestro_valores.
    RUN cambio_tablas.
    OS-DELETE "Test".
    QUIT.
  END.

/* FUENTES */
&IF "{&MODALIDAD}" = "yes" &THEN
  IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_fuentes,1,2)) OR OPSYS = "UNIX" THEN
    DO:
      RUN valido_directorio (INPUT vlc_dir_fuentes, OUTPUT salida).
      IF NOT salida THEN
        DO:
          BELL.
          HIDE FRAME inicio NO-PAUSE.
          MESSAGE COLOR white/red "El Directorio:" vlc_dir_fuentes "No Existe o No es Accesible !!!" view-as alert-box error.
          RUN muestro_valores.
          RUN cambio_tablas.
          OS-DELETE "Test".
          QUIT.
        END.
    END.
&ENDIF

/* OBJETOS */
IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_objetos,1,2)) OR OPSYS = "UNIX" THEN
  DO:
    RUN valido_directorio (INPUT vlc_dir_objetos, OUTPUT salida).
    IF NOT salida THEN
      DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "El Directorio:" vlc_dir_objetos "No Existe o No es Accesible !!!" view-as alert-box error.
        RUN muestro_valores.
        RUN cambio_tablas.
        OS-DELETE "Test".
        QUIT.
      END.
  END.

/* TEMP (1) */
IF NOT (SUBSTRING(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "/" OR substring(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "~\") AND
   index(vlc_dir_temp,"%") = 0 AND index(vlc_dir_temp,"$") = 0 THEN
  ASSIGN vlc_dir_temp = vlc_dir_temp + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").

/* TEMP (2) */
IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_temp,1,2)) OR OPSYS = "UNIX" THEN
  DO:
    RUN valido_directorio (INPUT vlc_dir_temp, OUTPUT salida).
    IF NOT salida THEN
      DO:
        IF OPSYS = "UNIX" THEN
          vlc_dir_temp = "/tmp/".
        IF OPSYS = "WIN32" THEN
          DO:
            IF OS-GETENV("TEMP") <> ? THEN
              vlc_dir_temp = OS-GETENV("TEMP").
            ELSE
              IF OS-GETENV("TMP") <> ? THEN
                vlc_dir_temp = OS-GETENV("TMP").
              ELSE
                vlc_dir_temp = SESSION:TEMP-DIRECTORY.
          END.
      END.
  END.

/* TEMP (3) */
IF NOT (SUBSTRING(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "/" OR substring(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "~\") THEN
  ASSIGN vlc_dir_temp = vlc_dir_temp + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").

/* TEMP (4) */
IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_temp,1,2)) OR OPSYS = "UNIX" THEN
  DO:
    RUN valido_directorio (INPUT vlc_dir_temp, OUTPUT salida).
    IF NOT salida THEN
      DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red 
                "El Directorio:" vlc_dir_temp "No Existe o No es Accesible !!!" skip
                "Por Favor, verifique este problema !!!" skip(1)
                "Se asume autom†ticamente como Directorio de Temporarios:" session:temp-directory
                view-as alert-box warning.
        vlc_dir_temp = SESSION:TEMP-DIRECTORY.
      END.
  END.

/* TEMP (5) */
IF NOT (SUBSTRING(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "/" OR substring(vlc_dir_temp,LENGTH(vlc_dir_temp),1) = "~\") THEN
  ASSIGN vlc_dir_temp = vlc_dir_temp + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").

/* BACKUP (1) */
IF vlc_dir_bkp = "" OR vlc_dir_bkp = ? OR vlc_dir_bkp = "%TEMP%" OR vlc_dir_bkp = "%TMP%" OR vlc_dir_bkp = "$TMP" THEN
  vlc_dir_bkp = vlc_dir_temp.

/* BACKUP (2) */
IF NOT (SUBSTRING(vlc_dir_bkp,LENGTH(vlc_dir_bkp),1) = "/" OR substring(vlc_dir_bkp,LENGTH(vlc_dir_bkp),1) = "~\") THEN
  ASSIGN vlc_dir_bkp = vlc_dir_bkp + (IF OPSYS = "UNIX" THEN "/" ELSE "~\").

/* BACKUP (3) */
IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_bkp,1,2)) OR OPSYS = "UNIX" THEN
  DO:
    RUN valido_directorio (INPUT vlc_dir_bkp, OUTPUT salida).
    IF NOT salida THEN
      vlc_dir_bkp = vlc_dir_temp.
  END.

/* TRANSFERENCIAS */
IF (OPSYS = "WIN32" AND substring(vlc_dir_spool,1,2) <> substring(vlc_dir_i-o,1,2)) OR OPSYS = "UNIX" THEN
  DO:
    RUN valido_directorio (INPUT vlc_dir_i-o, OUTPUT salida).
    IF NOT salida THEN
      DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "El Directorio:" vlc_dir_i-o "No Existe o No es Accesible !!!" view-as alert-box error.
        RUN muestro_valores.
        RUN cambio_tablas.
        OS-DELETE "Test".
        QUIT.
      END.
  END.

/* TEMPORARIOS DE SESSION */
IF OPSYS <> "UNIX" AND x_proversion >= 10 AND x_parametros = "" THEN
  IF SESSION:TEMP-DIRECTORY = "c:~\winnt~\temp" OR SESSION:TEMP-DIRECTORY = "c:~\windows~\temp" THEN
    DO:
      MESSAGE "Si el Usuario con que inici¢ la Sesi¢n Windows No es Administrador, es probable que Usted No Tenga Acceso Total al"
              "directorio de temporarios" SESSION:TEMP-DIRECTORY "con que se inici¢ esta Aplicaci¢n." SKIP(1)
              "En caso de que algunas opciones muestren un Error en la ejecuci¢n de las mismas, por favor, de aviso de esto a la Gerencia de Sistemas." SKIP(1)
              "Para solucionar este inconveniente se debe cambiar el directorio de temporarios de la sesi¢n Progress OpenEdge que se inicia"
              "con los par†metros del archivo *.pf" SKIP(1)
              "En este archivo hay que cambiar o agregar la l°nea -T, o bien en la l°nea de comandos del °cono, agregar al final, por ejemplo, un -T %TEMP%" SKIP(1)
              "Se puede tambiÇn crear un directorio nuevo para los temporarios, por ejemplo C:~\temp, y asignar a Çste los temporarios de sesi¢n anteriores." SKIP(1)
              "Para m†s informaci¢n, por favor, vea la ayuda de instalaci¢n disponible en" vlc_dir_objetos + "install~\instalar-help.txt"
              VIEW-AS ALERT-BOX WARNING.
    END.

/* TEMPORARIOS DE SESSION o DE LA APLICACI‡N EN CARPETA DE RED */ /* PABLO */

/******* PABLO ******
if opsys <> "UNIX" and x_parametros = "" then
  do:
    if caps(substring(session:temp-directory,1,1)) > "E" then
      do:
        message "El Directorio de Temporarios de la Sesi¢n Progress" session:temp-directory "est† en una Carpeta de Red !!!" skip(1)
                "Esto ocasiona que la Aplicaci¢n se ejecute lentamente y genere tr†fico de red innecesario !!!" skip(1)
                "Por favor avise a Gerencia de Sistemas. Muchas Gracias."
                view-as alert-box warning.
      end.
    if caps(substring(vlc_dir_temp,1,1)) > "E" then
      do:
        message "El Directorio de Temporarios de la Aplicaci¢n" vlc_dir_temp "est† en una Carpeta de Red !!!" skip(1)
                "Esto ocasiona que la Aplicaci¢n se ejecute lentamente y genere tr†fico de red innecesario !!!" skip(1)
                "Por favor avise a Gerencia de Sistemas. Muchas Gracias."
                view-as alert-box warning.
      end.
  end.
***************************/

/*--- BASES DE DATOS QUE SE CONECTAN AL INICIO DE LA APLICACI‡N Y PERMANECEN CONECTADAS HASTA QUE FINALICE ---*/
FOR EACH par_bases WHERE par_bases.estado       = YES AND 
                         par_bases.autoconexion = YES AND
                         NOT connected(par_bases.id_base) NO-LOCK:

  /*--- PREFIJO ---*/
  IF vlc_host <> par_bases.id_host THEN 
    archivo = "pr".
  ELSE
    archivo = "pd".

  /*--- NOMBRE DE ARCHIVO ---*/
  archivo = archivo + "_" + par_bases.id_base + ".pf".

  /*--- PATH DEL ARCHIVO ---*/
  IF SEARCH(vlc_dir_objetos + "supervisor/" + archivo) <> ? THEN
    archivo = vlc_dir_objetos + "supervisor/" + archivo.
  ELSE
    FOR EACH par_grupos WHERE par_grupos.letra_inicial <> " " AND index("0123456789", par_grupos.letra_inicial) = 0 NO-LOCK:
      IF SEARCH(vlc_dir_objetos + lc(par_grupos.nombre_grupo) + "/" + archivo) <> ? THEN
        DO:
          archivo = SEARCH(vlc_dir_objetos + lc(par_grupos.nombre_grupo) + "/" + archivo).
          LEAVE.
        END.
    END.

  /*--- CONEXION DE LA BASE DE DATOS ---*/
  CONNECT value("-pf " + archivo + " -U computos -P wifaro") NO-ERROR.
  IF NOT CONNECTED(par_bases.id_base) THEN
    CONNECT value("-pf " + archivo) NO-ERROR.
  IF NOT CONNECTED(par_bases.id_base) AND esdemo THEN
    DO:
      IF INDEX(archivo,"pd_") > 0 THEN
        archivo = REPLACE(archivo,"pd_","su_").
      IF INDEX(archivo,"pr_") > 0 THEN
        archivo = REPLACE(archivo,"pr_","su_").
    END.
  CONNECT value("-pf " + archivo + " -U computos -P wifaro") NO-ERROR.
  IF NOT CONNECTED(par_bases.id_base) THEN
    CONNECT value("-pf " + archivo) NO-ERROR.
  IF NOT CONNECTED(par_bases.id_base) THEN
    DO:
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE COLOR white/green
              "La Base de Datos de Inicio [" + par_bases.id_base + "] No se ha Podido Conectar !!!" skip(1)
              "Referencia para Sistemas:" archivo view-as alert-box warning.
    END.
END.

/*--- VERIFICO CONEXION DE BASES DE DATOS ---*/
&IF "{&SISTEMA}" = "gessi" OR "{&SISTEMA}" = "core-old" &THEN
   salida = YES.
   IF NOT CONNECTED("parametros") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'PARAMETROS' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF NOT CONNECTED("stock") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'STOCK' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF NOT CONNECTED("comercial") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'COMERCIAL' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF NOT CONNECTED("estadis") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'ESTADIS' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF salida = NO THEN
     DO:
       OS-DELETE "Test".
       QUIT.
     END.
&ENDIF

&IF "{&SISTEMA}" = "posmaster" &THEN
   salida = YES.
   IF NOT CONNECTED("minorista") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'MINORISTA' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF NOT CONNECTED("stock") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'STOCK' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF NOT CONNECTED("estadis") THEN
     DO:
        BELL.
        HIDE FRAME inicio NO-PAUSE.
        MESSAGE COLOR white/red "Base de Datos 'ESTADIS' No Conectada !!!" view-as alert-box error.
        salida = NO.
     END.
   IF salida = NO THEN
     DO:
       OS-DELETE "Test".
       QUIT.
     END.
&ENDIF

&IF "{&SISTEMA}" = "contacto" &THEN
  IF NOT CONNECTED("contacto") THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE COLOR white/red "Base de Datos 'CONTACTO' No Conectada !!!" view-as alert-box error.
      OS-DELETE "Test".
      QUIT.
    END.
&ENDIF

&IF "{&SISTEMA}" = "nudelman" &THEN
  IF NOT CONNECTED("nudelman") THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE COLOR white/red "Base de Datos 'NUDELMAN' No Conectada !!!" view-as alert-box error.
      OS-DELETE "Test".
      QUIT.
    END.
&ENDIF

/*--- VERIFICO VARIABLES DE ENTORNO ---*/ /**** PABLO : No es necesario  ****/
/******

if x_dlc = ? and esdemo = no and x_parametros = "" then
  do:
    bell.
    hide frame inicio no-pause.
    message color white/red
            " No est† definida la variable de entorno <DLC> !!! " skip
            " Esta variable es requerida por PROGRESS !!! " skip(1)
            " Verifique tambiÇn que el Directorio de Binarios " skip
            " de la Aplicaci¢n y el '<DLC>~\BIN' estÇn en <PATH>. "
            view-as alert-box warning.
  end.

******/
/*--- SI EXISTE DLC VERIFICO QUE TENGA UN VALOR COHERENTE ---*/
/*************************************************************
if os-getenv("DLC") <> ? and os-getenv("DLC") <> "" then
  if os-getenv("DLC") <> x_dlc and esdemo = no and x_parametros = "" then
    do:
      bell.
      hide frame inicio no-pause.
      message color white/red
              " El valor de la variable de entorno <DLC> difiere con la versi¢n de PROGRESS que ejecuta actualmente !!! " skip(1)
              " Esto puede causar problemas en la ejecuci¢n de las aplicaciones PROGRESS !!! " skip(1)
              " Cambie el valor de la variable de entorno <DLC> y vuelva a ejecutar la aplicaci¢n."
              view-as alert-box warning.
    end.

salida = no.
if opsys = "UNIX" then
  do:
    if index(os-getenv("PATH"),x_dlc + "/bin") = 0 then
      salida = yes.  
  end.
else
  do:
    if index(os-getenv("PATH"),x_dlc + "~\BIN") = 0 then
      salida = yes.  
  end.
if salida = yes and x_proversion < 10 and vlc_host = vlc_host_app and esdemo = no and x_parametros = "" then
  do:
    bell.

    &IF "{&SISTEMA}" <> "contacto" &THEN
      hide frame inicio no-pause.
      message color white/red
              " El directorio de ejecutables de PROGRESS indicado por la " skip
              " variable de entorno '<DLC>~\BIN' no est† en el <PATH> !!! " skip
              " Esta definici¢n es requerida por la aplicaci¢n !!!" skip(1)
              " Verifique tambiÇn que el Directorio de Binarios " skip
              " de la Aplicaci¢n tambiÇn estÇ en el <PATH>. "
              view-as alert-box warning.
    &ENDIF
    
  end.

/* A ESTO NO TENGO FORMA DE VALIDARLO - QUEDA A MODO DE REFERENCIA
   DESCOMENTARLO EN CASO DE UTILIZAR EN "TODOS" LOS EQUIPOS EL U:\
   COMO DIRECCION DE LOS BINARIOS DE LA APLICACION
if index(os-getenv("PATH"),"U:~\") = 0 then
  do:
    bell.
    hide frame inicio no-pause.
    message color white/red
            " El directorio de binarios de la aplicaci¢n " skip
            " no est† en el <PATH> del sistema !!! " skip
            " Esta definici¢n es requerida por la aplicaci¢n !!!"
            view-as alert-box warning.
  end.
* FIN DE COMENTARIO */
*************************************************************/

&IF "{&SISTEMA}" <> "core" &THEN
  IF vlc_host = vlc_host_app THEN
    DO:
      IF OS-GETENV("DIRDB") = ? AND esdemo = NO AND x_parametros = "" THEN
        DO:
          BELL.
          HIDE FRAME inicio NO-PAUSE.
          MESSAGE COLOR white/red
                  " No est† definida la variable de entorno <DIRDB> !!! " skip
                  " Algunas opciones no van a funcionar !!! "
                  view-as alert-box warning.
        END.

      &IF "{&SISTEMA}" = "posmaster" &THEN
        IF OS-GETENV("DIRP") = ? AND esdemo = NO AND x_parametros = "" THEN
          DO:
            BELL.
            HIDE FRAME inicio NO-PAUSE.
            MESSAGE COLOR white/red
                    " No est† definida la variable de entorno <DIRDP> !!! " skip
                    " La variable <DIRP> deber°a contener un valor " skip
                    " similar a: <D:~\U~\SUCURSAL>, por ejemplo. " skip
                    " Algunas opciones no van a funcionar !!! "
                    view-as alert-box warning.
          END.
      &ENDIF

      &IF "{&SISTEMA}" = "gessi" &THEN
         IF OPSYS = "UNIX" THEN
           aux = "/u/modem/".
         ELSE
           aux = "D:~\u~\modem~\".
         RUN valido_directorio (INPUT aux, OUTPUT salida).
         IF salida = NO AND esdemo = NO AND x_parametros = "" THEN
           DO:
             BELL.
             HIDE FRAME inicio NO-PAUSE.
             MESSAGE COLOR white/red
                "El Directorio:" aux "No Existe o No es Accesible !!!" skip
                " Algunas opciones no van a funcionar !!! "
                view-as alert-box warning.
           END.
      &ENDIF

    END.
&ENDIF

/*--- EN POS-MASTER VALIDO SISTEMAS EXTERNOS: POS, BALANZAS E IMPRESORAS ---*/
&IF "{&SISTEMA}" = "posmaster" &THEN

IF par_estado.id_tipo_comercio = 1 AND x_parametros = "" THEN /* TIPO DE COMERCIO */
DO:

  /*--- PUNTO DE VENTA ---*/
  IF par_estado.id_modelo_registradora = 1 THEN  /* INVEL CHILE */
    DO:
      FIND FIRST modelos_registradoras WHERE modelos_registradoras.id_modelo_registradora = par_estado.id_modelo_registradora NO-LOCK.
      IF modelos_registradoras.directorio_instalacion = "" OR modelos_registradoras.directorio_instalacion = ?  THEN
        DO:
          BELL.
          HIDE FRAME inicio NO-PAUSE.
          MESSAGE COLOR white/red
                  "La versi¢n de Modelo de Punto de Venta parametrizada"  skip
                  "actualmente en POS-Master, requiere tener configurado" skip
                  "un Directorio de Instalaci¢n del Software del POS !!!" skip
                  "Las funciones relacionadas con los POS no funcionar†n"
                  view-as alert-box warning.
        END.
      ELSE
        DO:
          RUN valido_directorio (INPUT modelos_registradoras.directorio_instalacion, OUTPUT salida).
          IF NOT salida THEN
            DO:
              BELL.
              HIDE FRAME inicio NO-PAUSE.
              MESSAGE COLOR white/red
                      "No se puede acceder al directorio: '" +
                      modelos_registradoras.directorio_instalacion + "' !!!" skip
                      "Las funciones relacionadas con los POS no funcionar†n !!!"
                      view-as alert-box warning.
            END.
        END.
      RELEASE modelos_registradoras.
    END.
  ELSE
    DO:
       BELL.
       HIDE FRAME inicio NO-PAUSE.
       MESSAGE COLOR white/red "Verifique el Modelo de Punto de Venta !!!" view-as alert-box warning.
    END.
  
  /*--- BALANZAS ELECTRONICAS ---*/
  IF par_estado.id_modelo_balanza = 0 THEN  /* METTLER TOLEDO CHILE */
    DO:
      RUN valido_directorio (INPUT vlc_dir_i-o + "balanzas~\scalevis~\", OUTPUT salida).
      IF NOT salida THEN
        DO:
          BELL.
          HIDE FRAME inicio NO-PAUSE.
          MESSAGE COLOR white/red
                  "No se puede acceder al directorio: '" + vlc_dir_i-o + "balanzas~\scalevis' !!!" skip
                  "Las funciones relacionadas con las Balanzas Electr¢nicas no funcionar†n !!!"
                  view-as alert-box warning.
        END.
    END.
  ELSE
    DO:
       BELL.
       HIDE FRAME inicio NO-PAUSE.
       MESSAGE COLOR white/red "Verifique el Modelo de Balanza Electr¢nica !!!" view-as alert-box warning.
    END.
  
  /*--- IMPRESORA PARA CARTELERIA ---*/
  
  /* se est† utilizando por defecto para todo Chile la ZEBRA y no se valida
     actualmente el tipo de impresora, por eso se comenta este p†rrafo
  if par_estado.tipo_impresora <> 3 then /* FUSION TERMICA PARA CHILE */
    do:
       bell.
       hide frame inicio no-pause.
       message color white/red "Verifique el Tipo de Impresora !!!" view-as alert-box warning.
    end.
  */

END. /* TIPO DE COMERCIO */

&ENDIF

OS-DELETE "Test".

/*--- SI LA LICENCIA DEL SOFTWARE CADUCA, PASO A MODO DEMO ---*/
/*
&IF "{&SISTEMA}" = "core" OR "{&SISTEMA}" = "contacto" &THEN
  if par_estado.fecha_caducidad <> ? then
    if today > par_estado.fecha_caducidad + 2 then
      do transaction:
        find current par_estado exclusive-lock.
        assign par_estado.accesos = (if par_estado.accesos = ? then 100 else 300).
        release par_estado.
      end.
&ENDIF
*/
/*--- LIBERO REGISTROS ---*/
RELEASE par_estado.
RELEASE par_file_server.
RELEASE par_print_server.

/*--- VALIDO VARIABLES DE ENTORNO DEFINIDAS ---*/
IF OS-GETENV("RBLNG") <> "" AND os-getenv("RBLNG") <> ? THEN
  IF SEARCH(OS-GETENV("RBLNG")) = ? THEN
    MESSAGE "La Variable de Entorno %RBLNG% est† Definida, pero su Valor es Incorrecto !!!" SKIP(1)
            "Verifique AUTOEXEC.BAT, Variables de Entorno dentro de Propiedades de Mi PC, /etc/profile o lo que corresponda a su Sistema Operativo." SKIP(1)
            "Si contin£a no se podr†n Generar, Visualizar ni Imprimir reportes que se realizan desde Report Builder" SKIP(1)
            "Avise a Gerencia de Sistema sobre esta anormalidad."
            VIEW-AS ALERT-BOX WARNING.
IF OS-GETENV("PROMSGS") <> "" AND os-getenv("PROMSGS") <> ? THEN
  IF SEARCH(OS-GETENV("PROMSGS")) = ? THEN
    MESSAGE "La Variable de Entorno %PROMSGS% est† Definida, pero su Valor es Incorrecto !!!" SKIP(1)
            "Verifique AUTOEXEC.BAT, Variables de Entorno dentro de Propiedades de Mi PC, /etc/profile o lo que corresponda a su Sistema Operativo." SKIP(1)
            "Si contin£a no se podr†n Visualizar correctamente los Mensajes de la Aplicaci¢n Inform†tica" SKIP(1)
            "Avise a Gerencia de Sistema sobre esta anormalidad."
            VIEW-AS ALERT-BOX WARNING.

/*--- ASIGNO PATH A PROGRAMA DE CORREO ELECTRONICO Y NAVEGADOR DE WEB ---*/
ASSIGN vlc_email = SEARCH(OS-GETENV("PRG_EMAIL"))
       vlc_web   = SEARCH(OS-GETENV("PRG_WEB")).

/*--- ESPECIFICO PARA SUSE-LINUX ---*/
IF OPSYS = "UNIX" AND vlc_email = "/usr/X11R6/bin/mozilla" THEN
  ASSIGN vln_email = "Mozilla Suite 1"
         vlx_email = 1.

IF OPSYS = "UNIX" AND vlc_web = "/usr/X11R6/bin/mozilla" THEN
  ASSIGN vln_web = "Mozilla Suite 1"
         vlx_web = 1.

/*--- ESPECIFICO PARA OPEN-SUSE ---*/
IF OPSYS = "UNIX" THEN
  DO:
  
    /* EMAIL */
    IF vlc_email = "" OR vlc_email = ? THEN
      vlc_email = SEARCH("/usr/bin/seamonkey").
    IF vlc_email = "" OR vlc_email = ? THEN
      vlc_email = SEARCH("/usr/bin/thunderbird").
    IF vlc_email = "" OR vlc_email = ? THEN
      vlc_email = SEARCH("/usr/bin/mozilla").
    CASE vlc_email:
      WHEN "/usr/bin/seamonkey" THEN
        ASSIGN vln_email = "Mozilla SeaMonkey 1"
               vlx_email = 1.
      WHEN "/usr/bin/mozilla" THEN
        ASSIGN vln_email = "Mozilla Suite 1"
               vlx_email = 1.
      WHEN "/usr/bin/thunderbird" THEN
        ASSIGN vln_email = "Mozilla ThunderBird 2"
               vlx_email = 2.
    END CASE.

    /* WEB */
    IF vlc_web = "" OR vlc_web = ? THEN
      vlc_web = SEARCH("/usr/bin/seamonkey").
    IF vlc_web = "" OR vlc_web = ? THEN
      vlc_web = SEARCH("/usr/bin/firefox").
    IF vlc_web = "" OR vlc_web = ? THEN
      vlc_web = SEARCH("/usr/bin/mozilla").
    CASE vlc_web:
      WHEN "/usr/bin/seamonkey" THEN
        ASSIGN vln_web = "Mozilla SeaMonkey 1"
               vlx_web = 1.
      WHEN "/usr/bin/mozilla" THEN
        ASSIGN vln_web = "Mozilla Suite 1"
               vlx_web = 1.
      WHEN "/usr/bin/firefox" THEN
        ASSIGN vln_web = "Mozilla FireFox 2"
               vlx_web = 2.
    END CASE.

    /* NVU */
    IF vlc_html = "" OR vlc_html = ? THEN
      vlc_html = SEARCH("/usr/bin/nvu").
    IF vlc_html <> "" AND vlc_html <> ? THEN
      ASSIGN vln_html = "Nvu 1"
             vlx_html = 1.

    /* OPENOFFICE CALC */
    IF vlc_calc = "" OR vlc_calc = ? THEN
      vlc_calc = SEARCH("/usr/bin/oocalc").
    IF vlc_calc <> "" AND vlc_calc <> ? THEN
      ASSIGN vln_calc = "OpenOffice Calc 2"
             vlx_calc = 2.

    /* OPENOFFICE WRITER */
    IF vlc_doc = "" OR vlc_doc = ? THEN
      vlc_doc = SEARCH("/usr/bin/oowriter").
    IF vlc_doc <> "" AND vlc_doc <> ? THEN
      ASSIGN vln_doc = "OpenOffice Writer 2"
             vlx_doc = 2.

    /* ACROBAT READER */
    IF vlc_pdf = "" OR vlc_pdf = ? THEN
      vlc_pdf = SEARCH("/usr/X11R6/bin/acroread").
    IF vlc_pdf <> "" AND vlc_pdf <> ? THEN
      ASSIGN vlc_pdf = vlc_pdf + " &1".

  END.

/*--- ESPECIFICO PARA WINDOWS ---*/
IF OPSYS <> "UNIX" THEN
  DO:

    /*--- ESTµ INSTALADO ACROBAT y/o READER ? (no hay forma de determinar versi¢n y nombre completo) ---*/
    vlc_pdf = ?.

    /* Acrobat Reader 8 XP - 2003 - VISTA */
    LOAD "Applications/AcroRd32.exe/shell/Read/command/" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
    USE "Applications/AcroRd32.exe/shell/Read/command/" NO-ERROR.
    GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE vlc_pdf.
    UNLOAD "Applications/AcroRd32.exe/shell/Read/command/" NO-ERROR.
    IF vlc_pdf <> ? THEN
      ASSIGN vlc_pdf = REPLACE(vlc_pdf,'" "%1','')
             vlc_pdf = REPLACE(vlc_pdf,'"','').
    vlc_pdf = SEARCH(vlc_pdf).

    /* Acrobat Reader 7 XP - 2000 */
    IF vlc_pdf = ? THEN
      DO:
        LOAD "Applications/AcroRd32.exe/shell/open/command/" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
        USE "Applications/AcroRd32.exe/shell/open/command/" NO-ERROR.
        GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE vlc_pdf.
        UNLOAD "Applications/AcroRd32.exe/shell/open/command/" NO-ERROR.
        IF vlc_pdf <> ? THEN
          ASSIGN vlc_pdf = REPLACE(vlc_pdf,'" "%1','')
                 vlc_pdf = REPLACE(vlc_pdf,'"','').
        vlc_pdf = SEARCH(vlc_pdf).
      END.

    /* Acrobat Reader 6 98 - ME - 2000 */
    IF vlc_pdf = ? THEN
      DO:
        LOAD "AcroExch.Document/shell/Open/command/" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
        USE "AcroExch.Document/shell/Open/command/" NO-ERROR.
        GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE vlc_pdf.
        UNLOAD "AcroExch.Document/shell/Open/command/" NO-ERROR.
        IF vlc_pdf <> ? THEN
          ASSIGN vlc_pdf = REPLACE(vlc_pdf,'" "%1','')
                 vlc_pdf = REPLACE(vlc_pdf,'"','').
        vlc_pdf = SEARCH(vlc_pdf).
      END.

    /* Adobe Acrobat 6 */
    IF vlc_pdf = ? THEN
      DO:
        LOAD "Applications/Acrobat.exe/shell/open/command/" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
        USE "Applications/Acrobat.exe/shell/open/command/" NO-ERROR.
        GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE vlc_pdf.
        UNLOAD "Applications/Acrobat.exe/shell/open/command/" NO-ERROR.
        IF vlc_pdf <> ? THEN
          ASSIGN vlc_pdf = REPLACE(vlc_pdf,'" "%1','')
                 vlc_pdf = REPLACE(vlc_pdf,'"','').
        vlc_pdf = SEARCH(vlc_pdf).
      END.

    IF vlc_pdf <> ? THEN
      vlc_pdf = vlc_pdf + " &1".

    /*--- ESTµ INSTALADO WORLDCAST ? ---*/
    IF vlc_send_email = ? OR vlc_send_email = "" THEN
      DO:
        ASSIGN vlc_send_email = SEARCH("C:~\ARCHIV~~1~\worldcast~\worldcst.exe").
        IF vlc_send_email <> ? THEN
          ASSIGN vlx_send_email = 3.1
                 vln_send_email = "WorldCast 3.1".
      END.
    IF vlc_send_email = ? THEN
      DO:
        ASSIGN vlc_send_email = SEARCH("D:~\ARCHIV~~1~\worldcast~\worldcst.exe").
        IF vlc_send_email <> ? THEN
          ASSIGN vlx_send_email = 3.1
                 vln_send_email = "WorldCast 3.1".
      END.
    IF vlc_send_email = ? THEN
      DO:
        ASSIGN vlc_send_email = SEARCH("C:~\PROGRA~~1~\worldcast~\worldcst.exe").
        IF vlc_send_email <> ? THEN
          ASSIGN vlx_send_email = 3.1
                 vln_send_email = "WorldCast 3.1".
      END.
    IF vlc_send_email = ? THEN
      DO:
        ASSIGN vlc_send_email = SEARCH("D:~\PROGRA~~1~\worldcast~\worldcst.exe").
        IF vlc_send_email <> ? THEN
          ASSIGN vlx_send_email = 3.1
                 vln_send_email = "WorldCast 3.1".
      END.

    /*--- AVERIGUO DATOS DE STAR-CALC DE STAROFFICE / OPENOFFICE 1 ---*/
    RUN s_staroffice (INPUT "soffice.StarCalcDocument", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
    ASSIGN vlc_calc = x_path_aplicacion
           vlx_calc = x_version_aplicacion
           vln_calc = x_nombre_comercial.
    IF INDEX(vlc_calc,"soffice") <> 0 THEN
      ASSIGN vlc_calc = REPLACE(vlc_calc,"soffice","scalc")
             vlc_calc = REPLACE(vlc_calc,'"',' ')
             vlc_calc = TRIM(REPLACE(vlc_calc,"%1"," ")).
    
    /*--- AVERIGUO DATOS DE STAR-WRITER DE STAROFFICE / OPENOFFICE 1 ---*/
    RUN s_staroffice (INPUT "soffice.StarWriterDocument", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
    ASSIGN vlc_doc = x_path_aplicacion
           vlx_doc = x_version_aplicacion
           vln_doc = x_nombre_comercial.
    IF INDEX(vlc_doc,"soffice") <> 0 THEN
      ASSIGN vlc_doc = REPLACE(vlc_doc,"soffice","swriter")
             vlc_doc = REPLACE(vlc_doc,'"',' ')
             vlc_doc = TRIM(REPLACE(vlc_doc,"%1"," ")).

    /*--- AVERIGUO DATOS DE OPENOFFICE CALC 2 ---*/
    RUN s_staroffice (INPUT "opendocument.CalcDocument", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
    ASSIGN vlc_calc = x_path_aplicacion
           vlx_calc = x_version_aplicacion
           vln_calc = x_nombre_comercial.
    IF INDEX(vlc_calc,"soffice") <> 0 THEN
      ASSIGN vlc_calc = REPLACE(vlc_calc,"soffice","scalc")
             vlc_calc = REPLACE(vlc_calc,'"',' ')
             vlc_calc = TRIM(REPLACE(vlc_calc,"%1"," ")).
    
    /*--- AVERIGUO DATOS DE OPENOFFICE WRITER 2 ---*/
    RUN s_staroffice (INPUT "opendocument.WriterDocument", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
    ASSIGN vlc_doc = x_path_aplicacion
           vlx_doc = x_version_aplicacion
           vln_doc = x_nombre_comercial.
    IF INDEX(vlc_doc,"soffice") <> 0 THEN
      ASSIGN vlc_doc = REPLACE(vlc_doc,"soffice","swriter")
             vlc_doc = REPLACE(vlc_doc,'"',' ')
             vlc_doc = TRIM(REPLACE(vlc_doc,"%1"," ")).

    /*--- AVERIGUO DATOS DE EXCEL ---*/
    RUN s_appwin (INPUT "Excel.Application", 
                  OUTPUT x_path_aplicacion,
                  OUTPUT x_version_aplicacion,
                  OUTPUT x_nombre_comercial).
    ASSIGN vlc_calc = x_path_aplicacion
           vlx_calc = x_version_aplicacion
           vln_calc = x_nombre_comercial.
    
    /*--- AVERIGUO DATOS DE WORD ---*/
    RUN s_appwin (INPUT "Word.Application", 
                  OUTPUT x_path_aplicacion,
                  OUTPUT x_version_aplicacion,
                  OUTPUT x_nombre_comercial).
    ASSIGN vlc_doc = x_path_aplicacion
           vlx_doc = x_version_aplicacion
           vln_doc = x_nombre_comercial.

    /*--- AVERIGUO DATOS DE FRONTPAGE ---*/
    RUN s_appwin (INPUT "FrontPage.Application", 
                  OUTPUT x_path_aplicacion,
                  OUTPUT x_version_aplicacion,
                  OUTPUT x_nombre_comercial).
    ASSIGN vlc_html = x_path_aplicacion
           vlx_html = x_version_aplicacion
           vln_html = x_nombre_comercial.

    /*--- ESTµ INSTALADO NVU ? ---*/
    IF vlc_html = ? OR vlc_html = "" THEN
      DO:
        ASSIGN vlc_html = SEARCH("C:~\ARCHIV~~1~\nvu~\nvu.exe").
        IF vlc_html <> ? THEN
          ASSIGN vlx_html = 1
                 vln_html = "Nvu 1".
      END.
    IF vlc_html = ? THEN
      DO:
        ASSIGN vlc_html = SEARCH("D:~\ARCHIV~~1~\nvu~\nvu.exe").
        IF vlc_html <> ? THEN
          ASSIGN vlx_html = 1
                 vln_html = "Nvu 1".
      END.
    IF vlc_html = ? THEN
      DO:
        ASSIGN vlc_html = SEARCH("C:~\PROGRA~~1~\nvu~\nvu.exe").
        IF vlc_html <> ? THEN
          ASSIGN vlx_html = 1
                 vln_html = "Nvu 1".
      END.
    IF vlc_html = ? THEN
      DO:
        ASSIGN vlc_html = SEARCH("D:~\PROGRA~~1~\nvu~\nvu.exe").
        IF vlc_html <> ? THEN
          ASSIGN vlx_html = 1
                 vln_html = "Nvu 1".
      END.

    /*--- INTENTO BUSCAR MAS DATOS ---*/
    IF vlc_email = ? THEN      /* SEAMONKEY 1 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\ARCHIV~~1~\mozilla.org~\SEAMON~~1~\SEAMONKEY.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla SeaMonkey 1"
                 vlx_email = 1
                 vln_email = "Mozilla SeaMonkey 1".
      END.

    /*--- INTENTO BUSCAR MAS DATOS ---*/
    IF vlc_email = ? THEN      /* SEAMONKEY 1 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\ARCHIV~~1~\mozilla.org~\SEAMON~~1~\SEAMONKEY.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla SeaMonkey 1"
                 vlx_email = 1
                 vln_email = "Mozilla SeaMonkey 1".
      END.

    /*--- INTENTO BUSCAR MAS DATOS ---*/
    IF vlc_email = ? THEN      /* SEAMONKEY 1 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\PROGRA~~1~\mozilla.org~\SEAMON~~1~\SEAMONKEY.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla SeaMonkey 1"
                 vlx_email = 1
                 vln_email = "Mozilla SeaMonkey 1".
      END.

    /*--- INTENTO BUSCAR MAS DATOS ---*/
    IF vlc_email = ? THEN      /* SEAMONKEY 1 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\PROGRA~~1~\mozilla.org~\SEAMON~~1~\SEAMONKEY.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla SeaMonkey 1"
                 vlx_email = 1
                 vln_email = "Mozilla SeaMonkey 1".
      END.

    /*--- INTENTO BUSCAR MAS DATOS ---*/
    IF vlc_email = ? THEN      /* NETSCAPE 7 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\ARCHIV~~1~\NETSCAPE~\NETSCAPE~\NETSCP.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 7
                 vln_web   = "Netscape 7"
                 vlx_email = 7
                 vln_email = "Netscape 7".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 7 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\ARCHIV~~1~\NETSCAPE~\NETSCAPE~\NETSCP.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 7
                 vln_web   = "Netscape 7"
                 vlx_email = 7
                 vln_email = "Netscape 7".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 7 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\PROGRA~~1~\NETSCAPE~\NETSCAPE~\NETSCP.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 7
                 vln_web   = "Netscape 7"
                 vlx_email = 7
                 vln_email = "Netscape 7".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 7 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\PROGRA~~1~\NETSCAPE~\NETSCAPE~\NETSCP.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 7
                 vln_web   = "Netscape 7"
                 vlx_email = 7
                 vln_email = "Netscape 7".
      END.

    IF vlc_email = ? THEN      /* MOZILLA SUITE 1 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\ARCHIV~~1~\MOZILLA.ORG~\MOZILLA~\MOZILLA.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla Suite 1"
                 vlx_email = 1
                 vln_email = "Mozilla Suite 1".
      END.

    IF vlc_email = ? THEN      /* MOZILLA SUITE 1 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\ARCHIV~~1~\MOZILLA.ORG~\MOZILLA~\MOZILLA.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla Suite 1"
                 vlx_email = 1
                 vln_email = "Mozilla Suite 1".
      END.

    IF vlc_email = ? THEN      /* MOZILLA SUITE 1 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\PROGRA~~1~\MOZILLA.ORG~\MOZILLA~\MOZILLA.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla Suite 1"
                 vlx_email = 1
                 vln_email = "Mozilla Suite 1".
      END.

    IF vlc_email = ? THEN      /* MOZILLA SUITE 1 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\PROGRA~~1~\MOZILLA.ORG~\MOZILLA~\MOZILLA.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Mozilla Suite 1"
                 vlx_email = 1
                 vln_email = "Mozilla Suite 1".
      END.

    IF vlc_email = ? THEN      /* MOZILLA THUNDERBIRD 2 */
      DO:
        ASSIGN vlc_email = SEARCH("c:~\archivos de programa~\mozilla thunderbird~\thunderbird.exe").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 2
                 vln_email = "Mozilla Thunderbird 2".
      END.

    IF vlc_email = ? THEN      /* MOZILLA THUNDERBIRD 2 */
      DO:
        ASSIGN vlc_email = SEARCH("d:~\archivos de programa~\mozilla thunderbird~\thunderbird.exe").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 2
                 vln_email = "Mozilla Thunderbird 2".
      END.

    IF vlc_email = ? THEN      /* MOZILLA THUNDERBIRD 2 */
      DO:
        ASSIGN vlc_email = SEARCH("c:~\program files~\mozilla thunderbird~\thunderbird.exe").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 2
                 vln_email = "Mozilla Thunderbird 2".
      END.

    IF vlc_email = ? THEN      /* MOZILLA THUNDERBIRD 2 */
      DO:
        ASSIGN vlc_email = SEARCH("d:~\program files~\mozilla thunderbird~\thunderbird.exe").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 2
                 vln_email = "Mozilla Thunderbird 2".
      END.

    IF vlc_web = ? THEN        /* MOZILLA FIREFOX 2 */
      DO:
        ASSIGN vlc_web = SEARCH("c:~\archivos de programa~\mozilla firefox~\firefox.exe").
        IF vlc_web <> ? THEN
          ASSIGN vlx_web = 2
                 vln_web = "Mozilla Firefox 2".
      END.

    IF vlc_web = ? THEN        /* MOZILLA FIREFOX 2 */
      DO:
        ASSIGN vlc_web = SEARCH("d:~\archivos de programa~\mozilla firefox~\firefox.exe").
        IF vlc_web <> ? THEN
          ASSIGN vlx_web = 2
                 vln_web = "Mozilla Firefox 2".
      END.

    IF vlc_web = ? THEN        /* MOZILLA FIREFOX 2 */
      DO:
        ASSIGN vlc_web = SEARCH("c:~\program files~\mozilla firefox~\firefox.exe").
        IF vlc_web <> ? THEN
          ASSIGN vlx_web = 2
                 vln_web = "Mozilla Firefox 2".
      END.

    IF vlc_web = ? THEN        /* MOZILLA FIREFOX 2 */
      DO:
        ASSIGN vlc_web = SEARCH("d:~\program files~\mozilla firefox~\firefox.exe").
        IF vlc_web <> ? THEN
          ASSIGN vlx_web = 2
                 vln_web = "Mozilla Firefox 2".
      END.

    IF vlc_email = ? THEN      /* OUTLOOK EXPRESS 6 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\ARCHIV~~1~\OUTLOO~~1~\MSIMN.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 6
                 vln_email = "Outlook Express 6".
      END.

    IF vlc_email = ? THEN      /* OUTLOOK EXPRESS 6 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\ARCHIV~~1~\OUTLOO~~1~\MSIMN.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 6
                 vln_email = "Outlook Express 6".
      END.

    IF vlc_email = ? THEN      /* OUTLOOK EXPRESS 6 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\PROGRA~~1~\OUTLOO~~1~\MSIMN.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 6
                 vln_email = "Outlook Express 6".
      END.

    IF vlc_email = ? THEN      /* OUTLOOK EXPRESS 6 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\PROGRA~~1~\OUTLOO~~1~\MSIMN.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlx_email = 6
                 vln_email = "Outlook Express 6".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 4 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\ARCHIV~~1~\NETSCAPE~\COMMUN~~1~\NETSCAPE.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Netscape 4"
                 vlx_email = 1
                 vln_email = "Netscape 4".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 4 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\ARCHIV~~1~\NETSCAPE~\COMMUN~~1~\NETSCAPE.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Netscape 4"
                 vlx_email = 1
                 vln_email = "Netscape 4".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 4 */
      DO:
        ASSIGN vlc_email = SEARCH("C:~\PROGRA~~1~\NETSCAPE~\COMMUN~~1~\NETSCAPE.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Netscape 4"
                 vlx_email = 1
                 vln_email = "Netscape 4".
      END.

    IF vlc_email = ? THEN      /* NETSCAPE 4 */
      DO:
        ASSIGN vlc_email = SEARCH("D:~\PROGRA~~1~\NETSCAPE~\COMMUN~~1~\NETSCAPE.EXE").
        IF vlc_email <> ? THEN
          ASSIGN vlc_web   = vlc_email
                 vlx_web   = 1
                 vln_web   = "Netscape 4"
                 vlx_email = 1
                 vln_email = "Netscape 4".
      END.

    /*--- AVERIGUO DATOS DE INTERNET EXPLORER ---*/
    IF vlc_web = ? THEN
      DO:
        RUN s_appwin (INPUT "InternetExplorer.Application", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
        ASSIGN vlc_web = x_path_aplicacion
               vlx_web = x_version_aplicacion
               vln_web = x_nombre_comercial.

      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA WINDOWS NT */
      DO:
        ASSIGN vlc_web = SEARCH("C:~\ARCHIV~~1~\PLUS!~\MICROS~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA WINDOWS NT */
      DO:
        ASSIGN vlc_web = SEARCH("D:~\ARCHIV~~1~\PLUS!~\MICROS~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA WINDOWS NT */
      DO:
        ASSIGN vlc_web = SEARCH("C:~\PROGRA~~1~\PLUS!~\MICROS~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA WINDOWS NT */
      DO:
        ASSIGN vlc_web = SEARCH("D:~\PROGRA~~1~\PLUS!~\MICROS~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA RESTO DE WINDOWS */
      DO:
        ASSIGN vlc_web = SEARCH("C:~\ARCHIV~~1~\INTERN~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA RESTO DE WINDOWS */
      DO:
        ASSIGN vlc_web = SEARCH("D:~\ARCHIV~~1~\INTERN~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA RESTO DE WINDOWS */
      DO:
        ASSIGN vlc_web = SEARCH("C:~\PROGRA~~1~\INTERN~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    IF vlc_web = ? THEN   /* INTERNET EXPLORER 6 PARA RESTO DE WINDOWS */
      DO:
        ASSIGN vlc_web = SEARCH("D:~\PROGRA~~1~\INTERN~~1~\IEXPLORE.EXE").
        IF vlc_web = ? THEN
          ASSIGN vlx_web = 6
                 vln_web = "Internet Explorer 6".
      END.

    /*--- AVERIGUO DATOS DE OUTLOOK (NO OUTLOOK EXPRESS) ---*/
    IF vlc_email = ? THEN
      DO:
        RUN s_appwin (INPUT "Outlook.Application", 
                      OUTPUT x_path_aplicacion,
                      OUTPUT x_version_aplicacion,
                      OUTPUT x_nombre_comercial).
        ASSIGN vlc_email = x_path_aplicacion
               vlx_email = x_version_aplicacion
               vln_email = x_nombre_comercial.
      END.

  END.

/*--- ESPECIFICO PARA SUSE LINUX: OPENOFFICE 1 CALC ---*/
IF OPSYS = "UNIX" AND (vlc_calc = ? OR vlc_calc = "") THEN
  IF SEARCH("/opt/OpenOffice.org/program/scalc") <> ? THEN
    ASSIGN vlc_calc = "/opt/OpenOffice.org/program/scalc"
           vlx_calc = 1.1
           vln_calc = "OpenOffice Calc 1".

/*--- ESPECIFICO PARA SUSE LINUX: OPENOFFICE 1 WRITE ---*/
IF OPSYS = "UNIX" AND (vlc_doc = ? OR vlc_doc = "") THEN
  IF SEARCH("/opt/OpenOffice.org/program/swriter") <> ? THEN
    ASSIGN vlc_doc = "/opt/OpenOffice.org/program/swriter"
           vlx_doc = 1.1
           vln_doc = "OpenOffice Writer 1".

/*--- STAR-CALC DE STAROFFICE / OPENOFFICE 1 & 2 ---*/
IF vlc_calc <> ? AND vlc_calc <> "" AND (INDEX(vlc_calc,"scalc") <> 0 OR index(vlc_calc,"oocalc") <> 0) THEN
  vlc_calc = vlc_calc + " &1".

/*--- STAR-WRITER DE STAROFFICE / OPENOFFICE 1 & 2 ---*/
IF vlc_doc <> ? AND vlc_doc <> "" AND (INDEX(vlc_doc,"swriter") <> 0 OR index(vlc_doc,"oowriter") <> 0) THEN
  vlc_doc = vlc_doc + " &1".

/*--- EXCEL ---*/
IF vlc_calc <> ? AND vlc_calc <> "" AND index(vlc_calc,"EXCEL") <> 0 THEN
  vlc_calc = vlc_calc + " /e /p &1 &2".

/*--- WORD ---*/
IF vlc_doc <> ? AND vlc_doc <> "" AND index(vlc_doc,"WINWORD") <> 0 THEN
  vlc_doc = vlc_doc + " &1".

/*--- FRONTPAGE ---*/
IF vlc_html <> ? AND vlc_html <> "" AND index(vlc_html,"FRONTPG") <> 0 THEN
  vlc_html = vlc_html + " &1".

/*--- WEB: INTERNET EXPLORER ---*/
IF vln_web BEGINS "Internet Explorer" THEN
  vlc_web = vlc_web + " -nohome &1".

/*--- WEB: NETSCAPE/MOZILLA ---*/
IF vln_web = "Netscape 7" OR vln_web = "Mozilla Suite 1" OR vln_web = "Mozilla Firefox 2" OR vln_web = "Mozilla SeaMonkey 1" THEN
  vlc_web = vlc_web + " -quiet -url &1".

/*--- EMAIL: NETSCAPE ---*/
IF vln_email = "Netscape 7" THEN
  vlc_email = vlc_email + " -mail &1".

/*--- EMAIL: MOZILLA ---*/
IF vln_email = "Mozilla Suite 1" OR vln_email = "Mozilla Thunderbird 2" OR vln_email = "Mozilla SeaMonkey 1" THEN
  /* vlc_email = vlc_email + " -mail &1". HUBO CAMBIOS EN 1.7 ver http://www.mozilla.org/docs/command-line-args.html */
  vlc_email = vlc_email + ' -quiet -compose "to=''&1''"'.

/*--- EMAIL: OUTLOOK EXPRESS ---*/
IF vln_email BEGINS "Outlook Express" THEN
  vlc_email = vlc_email + " /mailurl:&1".

/*--- EMAIL: OUTLOOK ---*/
IF INDEX(vln_email,"Outlook") <> 0 AND index(vln_email,"Express") = 0 THEN
  vlc_email = vlc_email + " -c IPM.Note /m &1".

/*--- EDITOR HTML: NVU ---*/
IF INDEX(vln_html,"Nvu") <> 0 THEN
  vlc_html = vlc_html + " &1".

/*--- ALERTAS ---*/
&IF "{&SISTEMA}" <> "core" &THEN
  IF vlc_email = ? AND x_parametros = "" THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE " No hay seleccionado un programa de correo electr¢nico !!! " SKIP
              " Es necesario definir la variable de entorno 'PRG_EMAIL' y " SKIP
              " en ella cargar el PATH al programa ejecutable de correo. "  SKIP(1)
              " La funci¢n de Env°o de Correo Electr¢nico estar† inactiva."
              VIEW-AS ALERT-BOX INFORMATION.
    END.

  IF vlc_web = ? AND x_parametros = "" THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE " No hay seleccionado un programa para la navegaci¢n en Internet !!! " SKIP
              " Es necesario definir la variable de entorno 'PRG_WEB' y en ella " SKIP
              " el PATH al programa ejecutable que permita navegar en Internet. " SKIP(1)
              " La funci¢n de Navegaci¢n de Web estar† inactiva."
              VIEW-AS ALERT-BOX INFORMATION.
    END.

  IF (a_smtp_server = ? OR a_smtp_server = "") AND x_parametros = "" THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE " No est† configurado un servidor para el env°o autom†ticos de e-mails !!! " SKIP
              " Recomendamos cargar los datos del Servidor SMTP en los par†metros " SKIP
              " generales del sistema (estado) desde el m¢dulo 'Supervisor' !!! " SKIP(1)
              " La funci¢n de Env°o Autom†tico de E-Mails estar† inactiva."
              VIEW-AS ALERT-BOX INFORMATION.
    END.

  IF (a_pop3_server = ? OR a_pop3_server = "") AND x_parametros = "" THEN
    DO:
      BELL.
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE " No est† configurado un servidor para la recepci¢n de e-mails !!! " SKIP
              " Recomendamos cargar los datos del Servidor POP3 en los par†metros " SKIP
              " generales del sistema (estado) desde el m¢dulo 'Supervisor' !!! " SKIP(1)
              " La funci¢n de Recepci¢n Autom†tica de E-Mails estar† inactiva."
              VIEW-AS ALERT-BOX INFORMATION.
    END.
&ENDIF

/*--- VALIDO QUE EXISTAN LAS LIBRERIAS/COMPILADOS ---*/
&IF "{&MODALIDAD}" = "no" &THEN
  ASSIGN aux  = PROPATH
         tipo = "".
  IF SESSION:DISPLAY-TYPE = "GUI" THEN
  DO:
    tipo = "gui/".
  END.
  PROPATH = vlc_dir_objetos + tipo + "supervisor/supervisor.pl," +
            vlc_dir_objetos + tipo + "supervisor".
  IF SEARCH("s_mail.r") = ? THEN
  DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE "El Sistema NO Est† Compilado !!!" SKIP "No se puede ejecutar la aplicaci¢n !!!" VIEW-AS ALERT-BOX ERROR.
    OS-DELETE "Test".
    QUIT.
  END.
  PROPATH = aux.
&ENDIF

/*--- VERIFICO QUE LOS COMPILADOS ESTEN DENTRO DE LAS LIBRERIAS ---*/
&IF "{&MODALIDAD}" = "no" &THEN
  IF SEARCH(vlc_dir_objetos + tipo + "supervisor/supervisor.pl") = ? THEN
  DO:
    BELL.
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE "El sistema se est† ejecutando en un Entorno SIN Librer°as (Progress .PL) !!!" SKIP
            "Es probable que la velocidad de ejecuci¢n de las aplicaciones sea menor a la normal !!!"
            VIEW-AS ALERT-BOX WARNING.
  END.
&ENDIF
  
/*--- VERIFICO INSTALACI‡N: EJECUTABLES WINDOWS ---*/
ASSIGN aux = (IF OPSYS = "UNIX" THEN "/" ELSE "~\").
IF OPSYS = "WIN32" AND x_parametros = "" THEN
  DO:
    dirwin = ?.
    IF OS-GETENV("windir") <> ? THEN /* Solo para Windows compatibles con NT Kernel */
      dirwin = OS-GETENV("windir") + "~\".
    IF SEARCH(dirwin + "explorer.exe") = ? THEN
      dirwin = ?.
    IF dirwin = ? THEN
      DO:
        IF SEARCH("c:~\windows~\win.ini") <> ? THEN
          dirwin = "c:~\windows~\".
        IF SEARCH("d:~\windows~\win.ini") <> ? THEN
          dirwin = "d:~\windows~\".
        IF SEARCH("c:~\winnt~\win.ini") <> ? THEN
          dirwin = "c:~\winnt~\".
        IF SEARCH("d:~\winnt~\win.ini") <> ? THEN
          dirwin = "d:~\winnt~\".
      END.
    IF dirwin <> ? THEN
      DO:
        IF SEARCH(dirwin + "blat.exe") = ? THEN                  OS-COPY value(vlc_dir_objetos + "install~\blat.exe")        value(dirwin + "blat.exe").
        IF SEARCH(dirwin + "blat.dll") = ? THEN                  OS-COPY value(vlc_dir_objetos + "install~\blat.dll")        value(dirwin + "blat.dll").
        IF SEARCH(dirwin + "blat.lib") = ? THEN                  OS-COPY value(vlc_dir_objetos + "install~\blat.lib")        value(dirwin + "blat.lib").
        IF SEARCH(dirwin + "getmail.exe") = ? THEN               OS-COPY value(vlc_dir_objetos + "install~\getmail.exe")     value(dirwin + "getmail.exe").
        IF SEARCH(dirwin + "pkzip.exe") = ? THEN                 OS-COPY value(vlc_dir_objetos + "install~\pkzip.exe")       value(dirwin + "pkzip.exe").
        IF SEARCH(dirwin + "pkunzip.exe") = ? THEN               OS-COPY value(vlc_dir_objetos + "install~\pkunzip.exe")     value(dirwin + "pkunzip.exe").
        IF SEARCH(dirwin + "rar.exe") = ? THEN                   OS-COPY value(vlc_dir_objetos + "install~\rar.exe")         value(dirwin + "rar.exe").
        IF SEARCH(dirwin + "unrar.exe") = ? THEN                 OS-COPY value(vlc_dir_objetos + "install~\unrar.exe")       value(dirwin + "unrar.exe").
        IF SEARCH(dirwin + "quoter.exe") = ? THEN                OS-COPY value(vlc_dir_objetos + "install~\quoter.exe")      value(dirwin + "quoter.exe").
        IF SEARCH(dirwin + "zip.exe") = ? THEN                   OS-COPY value(vlc_dir_objetos + "install~\zip.exe")         value(dirwin + "zip.exe").
        IF SEARCH(dirwin + "unzip.exe") = ? THEN                 OS-COPY value(vlc_dir_objetos + "install~\unzip.exe")       value(dirwin + "unzip.exe").
        IF SEARCH(dirwin + "gzip.exe") = ? THEN                  OS-COPY value(vlc_dir_objetos + "install~\gzip.exe")        value(dirwin + "gzip.exe").
        IF SEARCH(dirwin + "ttf2pt1.exe") = ? THEN               OS-COPY value(vlc_dir_objetos + "install~\ttf2pt1.exe")     value(dirwin + "ttf2pt1.exe").
        IF SEARCH(dirwin + "md5.exe") = ? THEN                   OS-COPY value(vlc_dir_objetos + "install~\md5.exe")         value(dirwin + "md5.exe").
        IF SEARCH(dirwin + "system32~\procryptlib.dll") = ? THEN OS-COPY value(vlc_dir_objetos + "install~\procryptlib.dll") value(dirwin + "system32~\procryptlib.dll").
        IF SEARCH(dirwin + "system32~\zlib1.dll") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\zlib1.dll")       value(dirwin + "system32~\zlib1.dll").
        IF SEARCH(dirwin + "Fonts~\code39.ttf") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\code39.ttf")      value(dirwin + "Fonts~\code39.ttf").

        IF SEARCH(dirwin + "Fonts~\PF_C39.ttf") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_C39.ttf")       value(dirwin + "Fonts~\PF_C39.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_C128.ttf") = ? THEN        OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_C128.ttf")      value(dirwin + "Fonts~\PF_C128.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_EAN_P36.ttf") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_EAN_P36.ttf")   value(dirwin + "Fonts~\PF_EAN_P36.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_EAN_P72.ttf") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_EAN_P72.ttf")   value(dirwin + "Fonts~\PF_EAN_P72.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_I2OF5.ttf") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_I2OF5.ttf")     value(dirwin + "Fonts~\PF_I2OF5.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_I2OF5_NT.ttf") = ? THEN    OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_I2OF5_NT.ttf")  value(dirwin + "Fonts~\PF_I2OF5_NT.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_I2OF5_TXT.ttf") = ? THEN   OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_I2OF5_TXT.ttf") value(dirwin + "Fonts~\PF_EAN_P72.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_EAN_P72.ttf") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_EAN_P72.ttf")   value(dirwin + "Fonts~\PF_I2OF5_TXT.ttf").
        IF SEARCH(dirwin + "Fonts~\PF_I2OF5_W.ttf") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\fuentes~\PF_I2OF5_W.ttf")   value(dirwin + "Fonts~\PF_I2OF5_W.ttf").

        IF SEARCH(dirwin + "~\system32~\lpr.exe") = ? THEN       OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\lpr*.* " + dirwin + "~\system32 >NUL").

        IF SEARCH(dirwin + "system32~\ftp_ori.exe") = ? THEN     
          DO:
            OS-COPY value(dirwin + "system32~\ftp.exe")         value(dirwin + "system32~\ftp_ori.exe").
            OS-COPY value(vlc_dir_objetos + "install~\ftp.exe") value(dirwin + "ServicePackFiles~\i386~\ftp.exe").
            OS-COPY value(vlc_dir_objetos + "install~\ftp.exe") value(dirwin + "LastGood~\system32~\ftp.exe").
            OS-COPY value(vlc_dir_objetos + "install~\ftp.exe") value(dirwin + "system32~\dllcache~\ftp.exe").
            OS-COPY value(vlc_dir_objetos + "install~\ftp.exe") value(dirwin + "system32~\ftp.exe").
            OS-COPY value(vlc_dir_objetos + "install~\ftp.exe") value(dirwin + "ftp.exe").
          END.
      END.

    /*--- POR PROBLEMAS VINCULADOS CON SEGURIDAD, PERMISOS Y PATH DE WINDOWS HAGO ESTO
          Por defecto todos los programas Windows se ejecutan desde donde estoy parado
          (no as° en Unix), es por eso que copio los ejecutables que necesito a los
          directorios "usuarios" de "sistemas" y/o "desarrollo" seg£n corresponda ---*/

    &IF "{&MODALIDAD}" = "no" &THEN
      ASSIGN dirusr = vlc_dir_objetos + "usuarios" + aux.
    &ELSE
      ASSIGN dirusr = vlc_dir_fuentes + "usuarios" + aux.
    &ENDIF

    IF SEARCH(dirusr + "blat.exe") = ? THEN        OS-COPY value(vlc_dir_objetos + "install~\blat.exe")        value(dirusr + "blat.exe").
    IF SEARCH(dirusr + "blat.dll") = ? THEN        OS-COPY value(vlc_dir_objetos + "install~\blat.dll")        value(dirusr + "blat.dll").
    IF SEARCH(dirusr + "blat.lib") = ? THEN        OS-COPY value(vlc_dir_objetos + "install~\blat.lib")        value(dirusr + "blat.lib").
    IF SEARCH(dirusr + "getmail.exe") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\getmail.exe")     value(dirusr + "getmail.exe").
    IF SEARCH(dirusr + "pkzip.exe") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\pkzip.exe")       value(dirusr + "pkzip.exe").
    IF SEARCH(dirusr + "pkunzip.exe") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\pkunzip.exe")     value(dirusr + "pkunzip.exe").
    IF SEARCH(dirusr + "rar.exe") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\rar.exe")         value(dirusr + "rar.exe").
    IF SEARCH(dirusr + "unrar.exe") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\unrar.exe")       value(dirusr + "unrar.exe").
    IF SEARCH(dirusr + "quoter.exe") = ? THEN      OS-COPY value(vlc_dir_objetos + "install~\quoter.exe")      value(dirusr + "quoter.exe").
    IF SEARCH(dirusr + "zip.exe") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\zip.exe")         value(dirusr + "zip.exe").
    IF SEARCH(dirusr + "unzip.exe") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\unzip.exe")       value(dirusr + "unzip.exe").
    IF SEARCH(dirusr + "gzip.exe") = ? THEN        OS-COPY value(vlc_dir_objetos + "install~\gzip.exe")        value(dirusr + "gzip.exe").
    IF SEARCH(dirusr + "ttf2pt1.exe") = ? THEN     OS-COPY value(vlc_dir_objetos + "install~\ttf2pt1.exe")     value(dirusr + "ttf2pt1.exe").
    IF SEARCH(dirusr + "md5.exe") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\md5.exe")         value(dirusr + "md5.exe").
    IF SEARCH(dirusr + "procryptlib.dll") = ? THEN OS-COPY value(vlc_dir_objetos + "install~\procryptlib.dll") value(dirusr + "procryptlib.dll").
    IF SEARCH(dirusr + "zlib1.dll") = ? THEN       OS-COPY value(vlc_dir_objetos + "install~\zlib1.dll")       value(dirusr + "zlib1.dll").
    IF SEARCH(dirusr + "ftp.exe") = ? THEN         OS-COPY value(vlc_dir_objetos + "install~\ftp.exe")         value(dirusr + "ftp.exe").

  END.

/*--- VERIFICO INSTALACI‡N: ARCHIVO 'statup.pf' DE PROGRESS ---*/

/******** PABLO: ANULADO 
if search(x_dlc + aux + "startup.pf") <> ? and search(x_dlc + aux + "startup.skn") = ? and x_parametros = "" then
  if opsys = "UNIX" then
    do:
      if unixusr = "root" then
        do:
          os-rename value(x_dlc + aux + "startup.pf")                value(x_dlc + aux + "startup.skn").
          os-copy   value(vlc_dir_objetos + "install/startup.linux") value(x_dlc + aux + "startup.pf").
        end.
    end.
  else
    do:
      os-rename value(x_dlc + aux + "startup.pf")                   value(x_dlc + aux + "startup.skn").
      os-copy   value(vlc_dir_objetos + "install~\startup.windows") value(x_dlc + aux + "startup.pf").
    end.
    
************************/    

/*--- VERIFICO INSTALACI‡N: ARCHIVO 'protermcap' DE PROGRESS ---*/
IF OPSYS = "UNIX" AND x_parametros = "" THEN
  IF SEARCH(x_dlc + aux + "protermcap") <> ? AND search(x_dlc + aux + "protermcap.skn") = ? THEN
    DO:
      IF unixusr = "root" THEN
        DO:
          OS-RENAME value(x_dlc + aux + "protermcap")                   value(x_dlc + aux + "protermcap.skn").
          OS-COPY   value(vlc_dir_objetos + "install/protermcap.linux") value(x_dlc + aux + "protermcap").
        END.
    END.

/*--- VERIFICO INSTALACI‡N: LENGUAJE DE REPORT BUILDER ENGINE DE PROGRESS ---*/
IF OPSYS <> "UNIX" THEN
  IF SEARCH(x_dlc + aux + "bin" + aux + "rbres32.dll") <> ? AND search(x_dlc + aux + "prolang" + aux + "spa" + aux + "rbspa32.dll") <> ? AND 
     search(x_dlc + aux + "bin" + aux + "rbres32.dll.skn") = ? AND x_parametros = "" THEN
    DO:
      OS-COPY value(x_dlc + aux + "bin" + aux + "rbres32.dll")                   value(x_dlc + aux + "bin" + aux + "rbres32.dll.skn").
      OS-COPY value(x_dlc + aux + "prolang" + aux + "spa" + aux + "rbspa32.dll") value(x_dlc + aux + "bin" + aux + "rbres32.dll").
    END.

/*--- VERIFICO INSTALACI‡N: ARCHIVO 'services' DEL SISTEMA OPERATIVO ---*/
IF OPSYS = "UNIX" AND x_parametros = "" THEN
  IF SEARCH("/etc/services.skn") = ? THEN
    IF unixusr = "root" THEN
      DO:
        OS-COPY "/etc/services" "/etc/services.skn".
        OS-COMMAND SILENT VALUE("cat " + vlc_dir_objetos + "install/services.linux >> /etc/services >/dev/null 2>/dev/null").
      END.
IF OPSYS = "WIN32" AND x_parametros = "" THEN
  DO:
    dirsvs = SEARCH(dirwin + "services").                           /* WIN 95-98-ME */
    IF dirsvs = ? THEN
      dirsvs = SEARCH(dirwin + "system32~\drivers~\etc~\services"). /* WIN NT-2000-XP-2003-VISTA */
    IF dirsvs <> ? THEN
      DO:
        dirsvs = SUBSTRING(dirsvs,1,LENGTH(dirsvs) - 8).
        IF SEARCH(dirsvs + "services.skn") = ? THEN
          DO:
            OS-COPY           value(dirsvs + "services") value(dirsvs + "services.skn").
            OS-COMMAND SILENT VALUE("copy " + dirsvs + "services + " + vlc_dir_objetos + "install~\services.windows " + dirsvs + "services >NUL").
          END.
      END.
  END.

/*--- VERIFICO INSTALACI‡N: SmartGraph ---*/
IF OPSYS = "WIN32" AND x_parametros = "" THEN
  IF SEARCH(x_dlc + "~\gui~\adm~\objects~\smartgrp.r") = ? THEN
    DO:
      OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\objects~\*.r "   + x_dlc + "~\gui~\adm~\objects >nul").
      OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\objects~\*.wrx " + x_dlc + "~\gui~\adm~\objects >nul").
      OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\objects~\*.wrx " + x_dlc + " >nul").
      OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\adeicon "        + x_dlc + "~\gui~\adeicon >nul").
      OS-COMMAND SILENT VALUE("copy " + vlc_dir_objetos + "install~\sauken.cst "     + x_dlc + "~\src~\template >nul").
    END.

/*--- VERIFICO INSTALACI‡N: VARIABLES DE ENTORNO ---*/ /*** PABLO ***/
IF OPSYS = "WIN32" AND x_dlc = ? AND search("c:~\autoexec.bat") <> ? AND x_parametros = "" THEN
  DO:
    OS-COMMAND SILENT "attrib -s -r -h c:~\autoexec.bat >NUL".
    OS-COPY "c:~\autoexec.bat" "c:~\autoexec.skn".
    OS-COMMAND SILENT VALUE("copy c:~\autoexec.skn + " + vlc_dir_objetos + "install~\entorno.txt c:~\autoexec.bat >NUL").
    HIDE FRAME inicio NO-PAUSE.
    MESSAGE "Configuraci¢n del sistema. Variables de Entorno de Windows. Faltan definir variables !!!" SKIP(1)
            "Por favor, salga del sistema, edite el archivo 'c:~\autoexec.bat', verifique los cambios y reinicie el mismo." SKIP(1)
            "Si no utiliza Windows 95/98 o Milenium, cargue estas variables de entorno editando las propiedades de Mi PC." SKIP
            "(s¢lo la variable 'DLC' y la entrada en 'PATH' son altamente recomendables de definir)"
            VIEW-AS ALERT-BOX WARNING.
  END.

/*--- VERIFICO INSTALACI‡N: REGISTRO DE WINDOWS ---*/
IF OPSYS = "WIN32" AND SESSION:DISPLAY-TYPE = "GUI" AND x_parametros = "" THEN
  DO:
    aux = SEARCH("ini2reg.exe").
    IF aux = ? THEN
      aux = x_dlc + "~\bin~\ini2reg.exe".
    IF SEARCH(aux) = ? THEN
      aux = "ini2reg.exe".
    ASSIGN v1 = ? v2 = ? aux = '"' + aux + '"'.
/*    
    /* VERIFICACION DE FONT */
    LOAD "Software/PSC/Progress/" + proversion + "/fonts" BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
    USE "Software/PSC/Progress/" + proversion + "/fonts" NO-ERROR.
    GET-KEY-VALUE SECTION "" KEY "font32" VALUE v1.
    UNLOAD "Software/PSC/Progress/" + proversion + "/fonts" NO-ERROR.
     
    /* VERIFICACION DE DISE•O */
    LOAD "Software/PSC/Progress/" + proversion + "/Startup" BASE-KEY "HKEY_CURRENT_USER" NO-ERROR.
    USE "Software/PSC/Progress/" + proversion + "/Startup" NO-ERROR.
    GET-KEY-VALUE SECTION "" KEY "Keep3DFillinBorder" VALUE v2.
    UNLOAD "Software/PSC/Progress/" + proversion + "/Startup" NO-ERROR.

    /* PROCESO */
    if v1 = ? or v1 = "" or v2 = ? or v2 = "" or v2 = "no" then
      do:
        /* SE DEBE EJECUTAR:
           C:\Progress\OpenEdge\bin\ini2reg -i sauken.ini -b HKEY_CURRENT_USER  -s SOFTWARE\PSC\PROGRESS\10.1A -ao
           C:\Progress\OpenEdge\bin\ini2reg -i sauken.ini -b HKEY_LOCAL_MACHINE -s SOFTWARE\PSC\PROGRESS\10.1A -ao
        */
        os-command silent value(aux + " -i " + vlc_dir_objetos + "install~\sauken.ini -b HKEY_CURRENT_USER  -s SOFTWARE~\PSC~\PROGRESS~\" + proversion + " -ao >NUL").
        os-command silent value(aux + " -i " + vlc_dir_objetos + "install~\sauken.ini -b HKEY_LOCAL_MACHINE -s SOFTWARE~\PSC~\PROGRESS~\" + proversion + " -ao >NUL").
        hide frame inicio no-pause.
        message "Configuraci¢n del sistema. Verificaci¢n del Registro de Windows. Faltan par†metros !!!" skip(1)
                "Por favor, para que la aplicaci¢n funcione correctamente, vuelva a ejecutarla." view-as alert-box warning.
        os-delete "Test".        
        quit.
      end.
      */
  END.
/*
/*--- ACTUALIZO BLAT Y GETMAIL ---*/
&IF "{&SISTEMA}" <> "core" &THEN
  IF OPSYS <> "UNIX" AND x_parametros = "" THEN
    DO:
      OS-COMMAND SILENT VALUE("blat -install "    + a_smtp_server + " " + a_smtp_sender).
      OS-COMMAND SILENT VALUE("getmail -install " + a_pop3_server + " default_user default_password").
    END.
&ENDIF
*/
/*--- EJECUTO LOGIN DE LOS SISTEMAS ---*/
HIDE FRAME inicio NO-PAUSE.
/* session:time-source = "userdb". */

IF x_parametros = "" THEN /* ARRANQUE NORMAL */
  DO:

    &IF "{&MODALIDAD}" = "yes" &THEN
       MESSAGE "Est† accediendo al Sistema como 'Programador de Aplicaciones' !!!" SKIP(1)
               "Si Usted es un Usuario Final, por favor realice lo siguiente:" SKIP(1)
               "  a) Salga del Sistema," SKIP 
               "  b) Edite las Propiedades del Icono que inici¢ la Aplicaci¢n, y" SKIP
               "  c) Cambie el Directorio de Inicio de la misma" SKIP(1)
               "Debe cambiar la palabra 'desarrollo' por 'sistemas' en el Directorio de Inicio (Iniciar en:)."
               "En caso de tener s¢lo una Letra de Unidad debe cambiar la misma por la Letra de Unidad que"
               "corresponda a 'sistemas' (por ejemplo de N:~\ a O:~\)." SKIP(1)
               "Ante cualquier duda, por favor, cont†ctese con la Gerencia de Sistemas. Muchas gracias."
               VIEW-AS ALERT-BOX WARNING.
    &ENDIF
 
    IF SESSION:DISPLAY-TYPE = "TTY" THEN
       RUN ../s_login.p.
    ELSE
      DO:

        /*--- ALERTA SOBRE RESOLUCION DE VIDEO ---*/
        &IF "{&SISTEMA}" = "contacto" &THEN
        IF SESSION:WIDTH-PIXELS < 1000 THEN
          DO:
            MESSAGE "La Resuluci¢n de Video que est† utilizando es Baja. Es probable que algunos programas no se visualicen correctamente." SKIP(1)
                    "Recomendamos utilizar una Resoluci¢n de Video de 1024 x 768 p°xeles y un monitor de al menos 15 pulgadas." SKIP(1)
                    "Para cambiar la Resoluci¢n de Video debe Editar las Propiedades de su Escritorio, luego abrir la solapa Configuraci¢n y"
                    "all° cambiar la Resoluci¢n de Pantalla al valor por Usted deseado." SKIP(1)
                    "Ante cualquier duda, por favor, cont†ctese con la Gerencia de Sistemas. Muchas gracias."
                    VIEW-AS ALERT-BOX WARNING.
          END.
        &ENDIF

        SESSION:DATA-ENTRY-RETURN  = YES.
        SESSION:APPL-ALERT-BOXES   = NO.
        SESSION:SYSTEM-ALERT-BOXES = YES.
        RUN ../z_login.w.

      END.
  END.
ELSE /* ARRANQUE PARA COMPILACI‡N */
  DO:
    IF ENTRY(1,x_parametros) = "compilar" AND entry(2,x_parametros) <> "" THEN
      DO:
        PROPATH = vlc_dir_fuentes + "especificos," + vlc_dir_fuentes + "supervisor," + 
                  vlc_dir_fuentes + lc(ENTRY(2,x_parametros)) + ","
                  &IF "{&SISTEMA}" = "contacto" &THEN
                    + vlc_dir_fuentes + "contacto,"
                  &ENDIF
                  .
        RUN value(vlc_dir_fuentes + lc(ENTRY(2,x_parametros)) + "/compila.p").
      END.
  END.

  
IF ENTRY(1,x_parametros) = 'normal' OR entry(1,x_parametros) = 'normal&fuentes' THEN
DO:
    &IF "{&MODALIDAD}" = "yes" &THEN
       MESSAGE "Est† accediendo al Sistema como 'Programador de Aplicaciones' !!!" SKIP(1)
               "Si Usted es un Usuario Final, por favor realice lo siguiente:" SKIP(1)
               "  a) Salga del Sistema," SKIP 
               "  b) Edite las Propiedades del Icono que inici¢ la Aplicaci¢n, y" SKIP
               "  c) Cambie el Directorio de Inicio de la misma" SKIP(1)
               "Debe cambiar la palabra 'desarrollo' por 'sistemas' en el Directorio de Inicio (Iniciar en:)."
               "En caso de tener s¢lo una Letra de Unidad debe cambiar la misma por la Letra de Unidad que"
               "corresponda a 'sistemas' (por ejemplo de N:~\ a O:~\)." SKIP(1)
               "Ante cualquier duda, por favor, cont†ctese con la Gerencia de Sistemas. Muchas gracias."
               VIEW-AS ALERT-BOX WARNING.
    &ENDIF
 
    IF SESSION:DISPLAY-TYPE = "TTY" THEN
       RUN ../s_login.p.
    ELSE
      DO:

        /*--- ALERTA SOBRE RESOLUCION DE VIDEO ---*/
        &IF "{&SISTEMA}" = "contacto" &THEN
        IF SESSION:WIDTH-PIXELS < 1000 THEN
          DO:
            MESSAGE "La Resuluci¢n de Video que est† utilizando es Baja. Es probable que algunos programas no se visualicen correctamente." SKIP(1)
                    "Recomendamos utilizar una Resoluci¢n de Video de 1024 x 768 p°xeles y un monitor de al menos 15 pulgadas." SKIP(1)
                    "Para cambiar la Resoluci¢n de Video debe Editar las Propiedades de su Escritorio, luego abrir la solapa Configuraci¢n y"
                    "all° cambiar la Resoluci¢n de Pantalla al valor por Usted deseado." SKIP(1)
                    "Ante cualquier duda, por favor, cont†ctese con la Gerencia de Sistemas. Muchas gracias."
                    VIEW-AS ALERT-BOX WARNING.
          END.
        &ENDIF

        SESSION:DATA-ENTRY-RETURN  = YES.
        SESSION:APPL-ALERT-BOXES   = NO.
        SESSION:SYSTEM-ALERT-BOXES = YES.
        RUN ../z_login.w.
      END.
END.
{../s_appsvr.clo} /* DESCONECTA EL APPSERVER */

RETURN.


/*--- PROCEDIMIENTO: VALIDACION DE DIRECTORIOS ---*/
PROCEDURE valido_directorio:

DEFINE INPUT  PARAMETER directorio AS CHARACTER.
DEFINE OUTPUT PARAMETER resultado  AS LOGICAL.
DEFINE VARIABLE stat AS INTEGER INITIAL 999 NO-UNDO.

FILE-INFO:FILE-NAME = directorio.
IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    resultado = FALSE.
ELSE
    resultado = TRUE.
/*      
OS-COPY "Test" value(directorio + "Test").
stat = OS-ERROR.
IF stat = 0 THEN
  IF SEARCH(directorio + "Test") = ? THEN
    stat = 2.
IF stat = 0 THEN
  DO:
    OS-DELETE value(directorio + "Test").
    resultado = YES.
  END.
ELSE
  resultado = NO.
*/
END PROCEDURE.


/*--- PROCEDIMIENTO: MUESTRO VALORES CARGADOS (IGUAL A "s_varsis.p") ---*/
PROCEDURE muestro_valores:

DEFINE VARIABLE letras    AS CHARACTER.
DEFINE VARIABLE renglones AS INTEGER.
DEFINE VARIABLE aux       AS INTEGER.

letras = PROPATH.
DO aux = 1 TO LENGTH(letras):
  IF SUBSTRING(letras,aux,1) = "," THEN
    SUBSTRING(letras,aux,1) = " ".
END.
RUN formato (78,NO,INPUT-OUTPUT letras, OUTPUT renglones).
DO aux = 1 TO LENGTH(letras):
  IF SUBSTRING(letras,aux,1) = " " THEN
    SUBSTRING(letras,aux,1) = ";".
END.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    DISPLAY vlc_host vlc_host_app vlc_file_server vlc_print_server 
            vlc_dir_fuentes vlc_dir_objetos vlc_dir_spool vlc_dir_temp
            vlc_dir_i-o vlc_dir_bkp vlc_comando vlc_web vlc_email
       WITH CENTERED 1 COLUMNS OVERLAY ROW 1 COLOR white/green
            title " Variables de los Sistemas ".
    REPEAT aux = 1 TO renglones:
      DISPLAY ENTRY(aux, letras) FORMAT "x(78)"
              WITH 5 DOWN ROW 16 NO-LABELS COLOR white/red overlay
              title " PROPATH (comas reemplazadas por punto y coma) ".
    END.
    PAUSE.
&ELSE    
    DEFINE BUTTON bf_aceptar LABEL "&Aceptar" TOOLTIP "Sale del programa".

    DEFINE RECTANGLE re-vs          SIZE 82 BY 14 EDGE-PIXELS 2 BGCOLOR 8 NO-FILL.

    DEFINE VARIABLE a AS CHARACTER VIEW-AS TEXT 
           INITIAL " Variables de los Sistemas " FORMAT "x(26)".
    
    DEFINE RECTANGLE re-pp          SIZE 82 BY 5 EDGE-PIXELS 2 BGCOLOR 8 NO-FILL.

    DEFINE VARIABLE b AS CHARACTER VIEW-AS TEXT 
           INITIAL " PROPATH (siguiendo orden de b£squeda) " FORMAT "x(42)".
    
    DEFINE VARIABLE ed_pro AS CHARACTER VIEW-AS EDITOR 
           INNER-CHARS 74 INNER-LINES 4 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL.
                      
    DEFINE FRAME informacion SKIP(0.5) re-vs 
                            a NO-LABEL
                            AT ROW 1.3 COL 1.5 FGCOLOR 1 FONT 7

                            "vlc_host:"             AT ROW 2.3 COL 18 RIGHT-ALIGNED
                            vlc_host NO-LABEL       AT ROW 2.3 COL 18 COLON-ALIGNED       

                            "vlc_host_app:"         AT ROW 3.3 COL 18 RIGHT-ALIGNED
                            vlc_host_app NO-LABEL   AT ROW 3.3 COL 18 COLON-ALIGNED 

                            "vlc_file_server:"       AT ROW 4.3 COL 18 RIGHT-ALIGNED
                            vlc_file_server NO-LABEL AT ROW 4.3 COL 18 COLON-ALIGNED 

                            "vlc_print_server:"       AT ROW 5.3 COL 18 RIGHT-ALIGNED
                            vlc_print_server NO-LABEL AT ROW 5.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_fuentes:"       AT ROW 6.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_fuentes NO-LABEL AT ROW 6.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_objetos:"       AT ROW 7.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_objetos NO-LABEL AT ROW 7.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_spool:"        AT ROW 8.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_spool NO-LABEL  AT ROW 8.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_temp:"         AT ROW 9.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_temp NO-LABEL   AT ROW 9.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_i-o:"          AT ROW 10.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_i-o NO-LABEL    AT ROW 10.3 COL 18 COLON-ALIGNED 

                            "vlc_dir_bkp:"          AT ROW 11.3 COL 18 RIGHT-ALIGNED
                            vlc_dir_bkp NO-LABEL    AT ROW 11.3 COL 18 COLON-ALIGNED 

                            "vlc_comando:"          AT ROW 12.3 COL 18 RIGHT-ALIGNED
                            vlc_comando NO-LABEL    AT ROW 12.3 COL 18 COLON-ALIGNED

                            "vlc_web:"              AT ROW 13.3 COL 18 RIGHT-ALIGNED
                            vlc_web NO-LABEL        AT ROW 13.3 COL 18 COLON-ALIGNED

                            "vlc_email:"            AT ROW 14.3 COL 18 RIGHT-ALIGNED
                            vlc_email NO-LABEL      AT ROW 14.3 COL 18 COLON-ALIGNED

                            b NO-LABEL  AT ROW 16.1 COL 1.5 FGCOLOR 1 FONT 7
                            re-pp       AT ROW 16.3 COL 1

                            ed_pro NO-LABEL AT ROW 17.3 COL 2.3
                            
                            bf_aceptar 
                            AT ROW 21.5 COL 37

                            WITH WIDTH 85 SIDE-LABELS CENTERED
                                 TITLE ' Informaci¢n de Sistema ' 
                                 THREE-D VIEW-AS DIALOG-BOX.

    /* ASI SE HAC÷A ANTES - JUAN CARLOS 2005
    repeat aux = 1 to renglones:
      ed_pro = ed_pro + entry(aux, letras) + "~n".
    end.
    */
    ed_pro = REPLACE(PROPATH, ",", "~n").

    DISPLAY a 
            b
            vlc_host
            vlc_host_app 
            vlc_file_server 
            vlc_print_server 
            vlc_dir_fuentes
            vlc_dir_objetos 
            vlc_dir_spool 
            vlc_dir_temp 
            vlc_dir_i-o 
            vlc_dir_bkp 
            vlc_comando
            vlc_web
            vlc_email
            ed_pro
            bf_aceptar
       WITH FRAME informacion.

    ENABLE bf_aceptar WITH FRAME informacion.
    ENABLE ed_pro WITH FRAME informacion.

    IF SESSION:HEIGHT-PIXELS >= 600 AND SESSION:WIDTH-PIXELS >= 800 THEN
      ASSIGN FRAME informacion:height = FRAME informacion:height + 3
                   bf_aceptar:row     =  bf_aceptar:row + 3
                   re-pp:height       = re-pp:height + 3
                   ed_pro:inner-lines = 8.

    ON "choose" OF bf_aceptar
    DO:
      APPLY "window-close" TO FRAME informacion.
    END.

    WAIT-FOR WINDOW-CLOSE OF FRAME informacion FOCUS bf_aceptar PAUSE 90.

&ENDIF

END PROCEDURE.


/*--- PROCEDIMIENTO: FORMATO DE TEXTO (IGUAL A "s_format.p") ---*/
PROCEDURE formato:

DEFINE INPUT        PARAMETER tamano      AS INTEGER.
DEFINE INPUT        PARAMETER justificado AS LOGICAL.
DEFINE INPUT-OUTPUT PARAMETER letras      AS CHARACTER.
DEFINE OUTPUT       PARAMETER renglones   AS INTEGER.

DEFINE VARIABLE c AS INTEGER.
DEFINE VARIABLE d AS INTEGER.
DEFINE VARIABLE u AS INTEGER.

DEFINE VARIABLE a AS INTEGER.
DEFINE VARIABLE b AS INTEGER.

DEFINE VARIABLE aux AS CHARACTER.
DEFINE VARIABLE tra AS CHARACTER.


/*--- VERIFICACION ---*/
IF tamano = 0 OR tamano = ? OR letras = ? OR letras = "" THEN
  DO:
    renglones = ?.
    RETURN.
  END.

/*--- FORMATO ---*/
ASSIGN c = 0 d = 0 renglones = 1.
DO WHILE c < length(letras):
  ASSIGN c = c + 1 d = d + 1.
  IF SUBSTRING(letras,c,1) = " " THEN
    u = c.
  IF d > tamano THEN
    DO:
      SUBSTRING(letras,u,1) = ",".
      ASSIGN c = u d = 0 renglones = renglones + 1.
    END.
END.

/*--- SIN JUSTIFICACION ---*/
IF justificado = NO OR justificado = ? THEN
  RETURN.

/*--- JUSTIFICACION ---*/
DO c = 1 TO renglones - 1:
  ASSIGN aux = ENTRY(c,letras) d = 0.
  DO u = 1 TO LENGTH(aux):
    IF SUBSTRING(aux,u,1) = " " THEN
      d = d + 1.
  END.
  ASSIGN u = tamano - length(aux).
  ASSIGN a = TRUNCATE(u / d, 0) b = u MODULO IF d = 0 THEN 1 ELSE d.
  DO u = 1 TO LENGTH(aux):
    IF SUBSTRING(aux,u,1) = " " THEN
      DO:
        SUBSTRING(aux,u,1) = FILL(" ",a + 1).
        u = u + a.
      END.
  END.
  u = LENGTH(aux).
  DO WHILE b > 0:
    u = u - 1.
    IF SUBSTRING(aux,u,1) = " " THEN
      DO:
        SUBSTRING(aux,u,0) = " ".
        b = b - 1.
      END.
  END.
  aux = aux + ",".
  tra = tra + aux.
END.

letras = tra + entry(renglones,letras).

END PROCEDURE.


/*--- PROCEDIMIENTO: AVERIGUA DATOS DE APLICACIONES WINDOWS (IGUAL A "s_appwin.p") ---*/
PROCEDURE s_appwin:

DEFINE INPUT  PARAMETER nombre_aplicacion  AS CHARACTER.
DEFINE OUTPUT PARAMETER path_aplicacion    AS CHARACTER INITIAL ?.
DEFINE OUTPUT PARAMETER version_aplicacion AS INTEGER   INITIAL ?.
DEFINE OUTPUT PARAMETER nombre_comercial   AS CHARACTER INITIAL ?.

DEFINE VARIABLE clsid  AS CHARACTER.
DEFINE VARIABLE curver AS CHARACTER.

IF OPSYS <> "WIN32" THEN
  RETURN.

LOAD nombre_aplicacion + "/CLSID" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE nombre_aplicacion + "/CLSID".
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE clsid.
UNLOAD nombre_aplicacion + "/CLSID" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.

LOAD nombre_aplicacion + "/CurVer" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE nombre_aplicacion + "/CurVer".
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE curver.
UNLOAD nombre_aplicacion + "/CurVer" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
version_aplicacion = INTEGER(SUBSTRING(curver,R-INDEX(curver,".") + 1)).

LOAD curver BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE curver.
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE nombre_comercial.
UNLOAD curver NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.

clsid = "SOFTWARE/Classes/CLSID/"+ clsid + "/LocalServer32".
LOAD clsid BASE-KEY "HKEY_LOCAL_MACHINE" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE clsid.
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE path_aplicacion.
UNLOAD clsid NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.

path_aplicacion = TRIM(REPLACE(path_aplicacion,'"'," ")).
IF INDEX(path_aplicacion,".exe") <> 0 THEN
  path_aplicacion = SUBSTRING(path_aplicacion,1,INDEX(path_aplicacion,".exe") + 3).
path_aplicacion = TRIM(path_aplicacion).

RETURN.

END PROCEDURE.


/*--- PROCEDIMIENTO: AVERIGUA DATOS DE STARTOFFICE / OPENOFFICE ---*/
PROCEDURE s_staroffice:

DEFINE INPUT  PARAMETER nombre_aplicacion  AS CHARACTER.
DEFINE OUTPUT PARAMETER path_aplicacion    AS CHARACTER INITIAL ?.
DEFINE OUTPUT PARAMETER version_aplicacion AS INTEGER   INITIAL ?.
DEFINE OUTPUT PARAMETER nombre_comercial   AS CHARACTER INITIAL ?.

DEFINE VARIABLE clsid  AS CHARACTER.
DEFINE VARIABLE curver AS CHARACTER.

IF OPSYS <> "WIN32" THEN
  RETURN.

LOAD nombre_aplicacion + "/CurVer" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE nombre_aplicacion + "/CurVer".
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE curver.
UNLOAD nombre_aplicacion + "/CurVer" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
version_aplicacion = INTEGER(SUBSTRING(curver,R-INDEX(curver,".") + 1)).

LOAD curver BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE curver.
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE nombre_comercial.
UNLOAD curver NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.

clsid = curver + "/shell/open/command".
LOAD clsid BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.
USE clsid.
GET-KEY-VALUE SECTION "" KEY DEFAULT VALUE path_aplicacion.
UNLOAD clsid NO-ERROR.
IF ERROR-STATUS:ERROR THEN
  RETURN.

RETURN.

END PROCEDURE.

/*--- PROCEDIMIENTO: CAMBIO VALORES DE TABLAS ---*/
PROCEDURE cambio_tablas:

  DEFINE VARIABLE x_passwd AS CHARACTER FORMAT "x(20)" LABEL "Contrase§a".
  DEFINE VARIABLE x_nuevo  AS LOGICAL FORMAT "Si/No" INITIAL YES.

  UPDATE SKIP(1) SPACE(3) 
         "Puede a Editar a Continuaci¢n las Tablas de Configuraci¢n del Sistema"
         SPACE(3) SKIP(1) SPACE(3)
         "Debe ingresar la Contrase§a para Acceder a Desarrollo de Aplicaciones"
         SPACE(3) SKIP(1) SPACE(18)
         x_passwd HELP " Contrase§a del Usuario 'computos'"

         &IF integer(substring(proversion,1, index(proversion,".") - 1)) >= 10 &THEN
            PASSWORD-FIELD
         &ELSE
            BLANK
         &ENDIF
         
         SKIP(1)
         WITH FRAME x_passwd OVERLAY CENTERED ROW 10 SIDE-LABELS TITLE " Tablas de Configuraci¢n del Sistema "

         &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
           COLOR white/cyan.
         &ELSE
           width 76 view-as dialog-box three-d.
         &ENDIF

  HIDE FRAME x_passwd NO-PAUSE.

  IF SETUSERID("computos",x_passwd,"userdb") THEN
    DO:
      HIDE FRAME inicio NO-PAUSE.
      MESSAGE "Desea crear un registro en blanco en la tabla 'par_file_server' ?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE x_nuevo AUTO-RETURN.
      IF x_nuevo THEN
        DO TRANSACTION:
          CREATE par_file_server.
          ASSIGN par_file_server.id_opsys      = OPSYS
                 par_file_server.dir_temp      = (IF OPSYS <> "UNIX" THEN "%TEMP%" ELSE "/tmp")
                 par_file_server.autenticacion = (IF OPSYS <> "UNIX" THEN "C:~\windows~\explorer.exe" ELSE "").
        END.
      FOR EACH par_file_server:
        UPDATE par_file_server EXCEPT c_usuario c_fecha c_hora
          WITH FRAME x_fs 1 COLUMNS 1 DOWN CENTERED OVERLAY ROW 8 TITLE " Tabla 'par_file_server' "

          &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
            COLOR white/green.
          &ELSE
            width 82 view-as dialog-box three-d.
          &ENDIF

        ASSIGN par_file_server.c_usuario = "computos"
               par_file_server.c_fecha   = TODAY
               par_file_server.c_hora    = STRING(TIME,"HH:MM:SS").

      END.
    END.

END PROCEDURE.
