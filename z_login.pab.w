&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS login 
/* ***************************  Definitions  ************************** */

/*--- SISTEMAS: (contacto, gessi, posmaster, nudelman, core) ---*/
{../sistema.i}

/*--- MODALIDAD: YES=desarrollo; NO=sistemas ---*/
{../modalidad.i}

/*--- VARIABLES GLOBALES DE TODOS LOS SISTEMAS ---*/
define shared variable vlc_host          as character.
define shared variable vlc_misc          as character no-undo initial ?.
define shared variable vlc_file_server   as character.
define shared variable vlc_print_server  as character.
define shared variable vlc_dir_fuentes   as character.
define shared variable vlc_dir_objetos   as character.
define shared variable vlc_dir_spool     as character.
define shared variable vlc_comando       as character.
define shared variable x_path            as character.

/*--- VARIABLES DEL PROGRAMA ---*/
define variable vlc_archivo  as character.
define variable vlc_datos    as character format "x(73)".
define variable vli_contador as integer   initial 0.
define variable vli_posicion as integer   initial 0.
define variable vli_intentos as integer   no-undo initial 3.
define variable vlc_basedato as character.
define variable vlc_mensaje  as character.
define variable vlc_formato  as character format "x(80)" extent 22.
define variable vlc_logueado as character format "x(30)".
define variable vlc_tabla    as character extent 40.
define variable salida       as logical.
define variable path         as character.
define variable lista_grupos as character.
define variable cc           as integer.
define variable stat         as logical.
define variable vlc_grupo    as character.
define variable aux          as character.
define variable defpropath   as character.
define variable dirpro       as character.
define variable letra        like par_grupos.letra_inicial.
define variable tipo_mensaje like par_estado.tipo_login.
define variable id_grupo     like par_grupos.letra_inicial.
define variable x_proversion as integer.
define variable c1           as integer.
define variable c2           as integer.
define variable c3           as character.

define temp-table auxlic like est_grupos.

/* Local Variable Definitions ---*/
define variable intentos         as integer initial 0.
define variable malo             as logical initial false.

define buffer xxx_grupos for par_grupos.

define temp-table archivos
    field t-arch as character
    field t-farch as character
    field t-tipo as character.

define new shared var vlc_empresa as integer initial 1.

define variable c-sep as char no-undo.
define variable c-tit as char no-undo.
define variable sal   as logical.

define variable respuesta    as logical   no-undo.
define variable archivo      as character no-undo.
define variable conexion     as character no-undo.
define variable esdemo       as logical   no-undo.
define variable contador     as integer   no-undo.
define variable lista        as character no-undo.

define variable auxpwd as character no-undo initial "m71t134y51p28".

session:time-source = "general".

/* DEFINO LA BARRA SEPARADORA SEGUN EL SISTEMA OPERATIVO */
if opsys = "UNIX" then
    assign c-sep = "/".
else
    assign c-sep = "~\".

/*--- VERSION DE PROGRESS EJECUTµNDOSE ---*/
x_proversion = integer(substring(proversion,1, index(proversion,".") - 1)).

/*--- DEFAULT PROPATH PROGRESS (puede variar conforme a la versi¢n de Progress que se utilize) ---*/
assign dirpro     = os-getenv("DLC")
       defpropath = ?.

if dirpro <> ? and x_proversion < 10 then
  do:

    if session:display-type = "GUI" then
      do:
        defpropath = dirpro + c-sep + "gui," +
                     dirpro + c-sep + "gui" + c-sep + "adecomm.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adecomp.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adedesk.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adedict.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adeedit.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adeicon.pl," +
                     dirpro + c-sep + "gui" + c-sep + "aderes.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adeshar.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adeuib.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adeweb.pl," +
                     dirpro + c-sep + "gui" + c-sep + "adexml.pl," +
                     dirpro + c-sep + "gui" + c-sep + "as4dict.pl," +
                     dirpro + c-sep + "gui" + c-sep + "prodict.pl," +
                     dirpro + c-sep + "gui" + c-sep + "protools.pl," +
                     dirpro + "," +
                     dirpro + c-sep + "bin," +
                     dirpro + c-sep + "PROBUILD" + c-sep + "EUCAPP" + c-sep + "EUC.PL," +
                     dirpro + c-sep + "PROBUILD" + c-sep + "EUCAPP".
      end.
    else
      do:
        if opsys = "UNIX" then
          defpropath = dirpro + c-sep + "tty," +
                       dirpro + c-sep + "tty" + c-sep + "as4dict.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeedit.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeshar.pl," +
                       dirpro + c-sep + "tty" + c-sep + "prodict.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adecomm.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adecomp.pl," +
                       dirpro + "," +
                       dirpro + c-sep + "bin".
        else
          defpropath = dirpro + c-sep + "tty," +
                       dirpro + c-sep + "tty" + c-sep + "adecomm.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adecomp.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeedit.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeshar.pl," +
                       dirpro + c-sep + "tty" + c-sep + "as4dict.pl," +
                       dirpro + c-sep + "tty" + c-sep + "prodict.pl," +
                       dirpro + c-sep + "PROBUILD" + c-sep + "EUCAPP" + c-sep + "EUC.PL," +
                       dirpro + c-sep + "PROBUILD" + c-sep + "EUCAPP," +
                       dirpro + "," +
                       dirpro + c-sep + "bin".
      end.

  end.

&IF "{&MODALIDAD}" = "yes" AND "{&SISTEMA}" = "core" &THEN
   defpropath = vlc_dir_fuentes + "," + vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "custom," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "gui," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "src," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                (if defpropath <> ? then defpropath else "").
&ENDIF

&IF "{&MODALIDAD}" = "no" AND "{&SISTEMA}" = "core" &THEN
   defpropath = vlc_dir_objetos + "," + vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "custom," +
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "gui," +
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "src," +
                vlc_dir_objetos + "supervisor" + c-sep + "core," + 
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                (if defpropath <> ? then defpropath else "").
&ENDIF

&IF "{&SISTEMA}" = "gessi" OR "{&SISTEMA}" = "posmaster" OR "{&SISTEMA}" = "core" OR "{&SISTEMA}" = "contacto" &THEN
  defpropath = (if defpropath <> ? then defpropath + "," else "") + 
               &IF "{&MODALIDAD}" = "yes" &THEN vlc_dir_fuentes &ELSE vlc_dir_objetos + "gui" + (if opsys = "UNIX" then "/" else "~\") &ENDIF
               + "supervisor" + (if opsys = "UNIX" then "/" else "~\") + "triggers,".
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME login

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 RECT-1 vcc_grupo vlc_usuario ~
vlc_clave Btn_OK Btn_Ayuda Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS vcc_grupo vlc_usuario vlc_clave 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda DEFAULT 
     LABEL "&Ayuda" 
     SIZE 15 BY 1.19 TOOLTIP "Ayuda para ingresar al sistema".

DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.19 TOOLTIP "Sale del sistema".

DEFINE BUTTON Btn_OK DEFAULT 
     LABEL "&OK" 
     SIZE 15 BY 1.19 TOOLTIP "Confirma datos ingresados y accede al sistema".

DEFINE VARIABLE vcc_grupo AS CHARACTER FORMAT "X(35)":U 
     LABEL "Sistema" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 43 BY 1 TOOLTIP "Sistema, Grupo o M¢dulo sobre el cual desea operar" NO-UNDO.

DEFINE VARIABLE vlc_clave AS CHARACTER FORMAT "X(30)":U 
     LABEL "Clave" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .95 TOOLTIP "Clave, Contrase¤a o Password del Usuario para permitir el acceso" NO-UNDO.

DEFINE VARIABLE vlc_usuario AS CHARACTER FORMAT "X(30)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .95 TOOLTIP "Nombre de Usuario con el cual se identifica en el sistema" NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "../login.jpg":U
     STRETCH-TO-FIT
     SIZE 55 BY 12.38 TOOLTIP "Copyrigth Grupo Sauken S.A. - Todos los derechos reservados".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 4.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME login
     vcc_grupo AT ROW 14.81 COL 10.6 COLON-ALIGNED
     vlc_usuario AT ROW 16 COL 4.2 HELP
          " Nombre o Usuario con el cual se identifica"
     vlc_clave AT ROW 17.19 COL 6 HELP
          " Clave o Contrase¤a para permitir su acceso" PASSWORD-FIELD 
     Btn_OK AT ROW 18.95 COL 2.6
     Btn_Ayuda AT ROW 18.95 COL 23
     Btn_Cancel AT ROW 18.95 COL 43.2
     IMAGE-4 AT ROW 1.48 COL 2.8
     RECT-1 AT ROW 14.33 COL 2.8
     SPACE(1.79) SKIP(1.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE " - Login (z_login.w)"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX login
   FRAME-NAME                                                           */
ASSIGN 
       FRAME login:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN vlc_clave IN FRAME login
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN vlc_usuario IN FRAME login
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX login
/* Query rebuild information for DIALOG-BOX login
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX login */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME login
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL login login
ON WINDOW-CLOSE OF FRAME login /*  - Login (z_login.w) */
DO:
    quit.
    /*---- APPLY "END-ERROR":U TO SELF.        -----*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda login
ON CHOOSE OF Btn_Ayuda IN FRAME login /* Ayuda */
DO:
  bell.
  message 
    "Este programa permite ingresar al sistema. Para hacerlo debe seleccionar el"      skip
    "m¢dulo o grupo de m¢dulos en la posici¢n indicada con el nombre 'Sistema', y"     skip
    "luego ingresar en las posiciones 'Usuario' y 'Clave', el nombre de usuario y"     skip
    "clave de acceso que le entreg¢ la Gerencia de Inform tica cuando lo habilit¢"     skip
    "para acceder a estas aplicaciones. Luego de ingresar estos datos, debe presionar" skip
    "el bot¢n 'OK' para entrar, o bien presionar el bot¢n 'Salir' para abandonar."     skip
    "S¢lo los m¢dulos o grupos de m¢dulos disponibles aparecer n en la lista que se"   skip
    "despliega en la posici¢n 'Sistema'. Si el nombre indicado est  precedido por"     skip
    "un asterisco '*', esto indica que se trata de un grupo de m¢dulos. Si est "       skip
    "precedido por un numeral '#' implica que se trata de un m¢dulo especial, el"      skip
    "cual s¢lo ser  accesible por la Gerencia de Inform tica. Muchas Gracias."
  view-as alert-box message.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel login
ON CHOOSE OF Btn_Cancel IN FRAME login /* Salir */
DO:
    quit.
    /*---- APPLY "END-ERROR":U TO SELF.        -------*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK login
ON CHOOSE OF Btn_OK IN FRAME login /* OK */
do:
    apply "VALUE-CHANGED" to vcc_grupo IN FRAME login.
    on return return.
    
    assign  intentos     = intentos + 1
            malo         = false
            vlc_usuario  = lc(input vlc_usuario)
            vlc_clave    = lc(input vlc_clave)
            vlc_misc     = ?.
    if vlc_grupo <> "" and vlc_usuario <> "" then
    do:
        find first par_grupos where par_grupos.nombre_grupo = vlc_grupo 

                                    &IF "{&MODALIDAD}" = "no" &THEN
                                      and par_grupos.estado = yes
                                    &ENDIF

                             no-lock no-error.
        if available par_grupos then
        do:
             if vlc_grupo <> "administrador" then
                 vlc_usuario = par_grupos.letra_inicial + "_" + vlc_usuario.

             if tipo_mensaje = no and vlc_usuario <> "computos" and id_grupo <> "" and substring(vlc_usuario,1,1) <> id_grupo then
               assign vlc_usuario = id_grupo + substring(vlc_usuario,2).

             if vlc_clave = "" then
               do:
                 find userdb._user where userdb._user._userid = vlc_usuario no-lock no-error.
                 if available userdb._user then
                   if userdb._user._passw = encode(auxpwd) then
                     vlc_clave = auxpwd.
                 release userdb._user.
               end.
               if setuserid (vlc_usuario,vlc_clave,"userdb") then
                do:
                  vlc_misc = vlc_clave.
                  if userid("userdb") = "computos" then
                   do:
                      
                      {../s_login.iii}

                      &IF "{&MODALIDAD}" = "no" &THEN
                        {../z_login.s1}
                      &ELSE
                        {../z_login.d1}
                      &ENDIF

                      RUN s_mail.p.
                      RUN disable_UI.
                      RUN _desk.p.
                      assign vlc_grupo   = ""
                             vlc_usuario = ""
                             vlc_clave   = ""
                             vlc_misc    = ?
                             id_grupo    = "".
                      RUN enable_UI.
                      on return return.
                      apply "ENTRY" to vcc_grupo IN FRAME login.
                   end.
                  else
                   do:
                      assign lista_grupos = "" id_grupo = "".
                      for each par_agrupados use-index orden where par_agrupados.letra_grupo = par_grupos.letra_inicial no-lock:
                        find xxx_grupos where xxx_grupos.letra_inicial = par_agrupados.letra_inicial and 
                                              xxx_grupos.estado = yes and (xxx_grupos.id_entorno = 1 or xxx_grupos.id_entorno = 2)
                                        no-lock no-error.
                        if available xxx_grupos then
                          do:
                            if lookup(lc(trim(xxx_grupos.nombre_grupo)), lista_grupos) = 0 then
                              lista_grupos = lista_grupos + lc(trim(xxx_grupos.nombre_grupo)) + ",".
                            id_grupo = id_grupo + lc(trim(xxx_grupos.letra_inicial)) + ",".
                          end.
                      end.
                      if lista_grupos = "" then
                        lista_grupos = vlc_grupo.
                      else
                        assign lista_grupos = substring(lista_grupos,1,length(lista_grupos) - 1)
                               id_grupo     = substring(id_grupo,1,length(id_grupo) - 1).
                      RUN disable_UI.
                      {../s_entorno.i}
                      {../s_login.iii}

                      &IF "{&MODALIDAD}" = "no" &THEN
                        {../z_login.s2}
                      &ELSE
                        {../z_login.d2}
                      &ENDIF

                      {../s_login.ii}
                      RUN s_mail.p.

                      /*--- SI LA PASSWORD ES LA POR DEFECTO OBLIGA A CAMBIARLA ---*/
                      if vlc_misc = auxpwd then
                        do:
                          message "Debe Asignar una Contrase¤a a su Cuenta !!!" skip(1)
                                  "A continuaci¢n se le pedir  que escriba una contrase¤a." skip
                                  "Luego deber  reingresarla para verificar la misma." skip
                                  "Por favor, recuerde la contrase¤a ingresada."
                                  view-as alert-box information.
                          run h_passwd.p.
                        end.
                      /*--- EJECUTA MENU DE OPCIONES ---*/
                      if id_grupo = "" then
                      
                        /*-- MENU INTERFAZ GRAFICA TRADICIONAL --*/
                        RUN z_menu.p.
                        
                      else

                        /*-- MENU INTERFAZ GRAFICA NUEVA --*/
                        if session:height-pixels >= 768 and session:width-pixels >= 1024 then
                          if session:height-pixels >= 1024 and session:width-pixels >= 1280 then
                            run w_inixlt.w (input id_grupo).
                          else
                            run w_inibig.w (input id_grupo).
                        else
                          run w_inicio.w (input id_grupo).

                      {../s_login.i} 
                      assign vlc_grupo   = ""
                             vlc_usuario = ""
                             vlc_clave   = ""
                             vlc_misc    = ?
                             id_grupo    = "".
                      RUN enable_UI.
                      on return return.
                      apply "ENTRY" to vcc_grupo IN FRAME login.
                   end.
                end.
               else 
                malo = true.
        end.
        else malo = true.
    end.
    else malo = true.    
    
    if malo then
    do:
       if intentos >= 3 then 
       DO:
         run s_inform.p (4, input vlc_logueado).
         APPLY "END-ERROR":U TO SELF.       
       END.
    
       bell.
       input clear.
       on return return.
       MESSAGE "­ Sistema/M¢dulo, Usuario o Contrase¤a Incorrectas !" skip
               " Le quedan " + string(3 - intentos) + " intentos. "
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
       assign   vcc_grupo:screen-value  = ""
                vlc_usuario:screen-value  = ""
                vlc_clave:screen-value = ""
                vlc_clave:sensitive = false
                vlc_usuario:sensitive = false.
       APPLY "BACK-TAB":U TO SELF.
       assign   vlc_clave:sensitive = true
                vlc_usuario:sensitive = true.
       
    end.
    else
      intentos = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-4 login
ON MOUSE-SELECT-CLICK OF IMAGE-4 IN FRAME login
DO:
  run ../copyrigth.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vcc_grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vcc_grupo login
ON VALUE-CHANGED OF vcc_grupo IN FRAME login /* Sistema */
DO:
  vlc_grupo = lc(trim(vcc_grupo:screen-value)).
  if vlc_grupo begins "*" then
    vlc_grupo = substring(vlc_grupo,3).
  if vlc_grupo begins "#" then
    vlc_grupo = substring(vlc_grupo,3).
  if tipo_mensaje = no then
    do:
      find first par_grupos where par_grupos.descripcion_grupo begins vlc_grupo no-lock.
      assign vlc_grupo = trim(lc(par_grupos.nombre_grupo))
             id_grupo  = trim(lc(par_grupos.letra_inicial)).
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK login 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/*--- VERIFICO LA EXISTENCIA DEL USUARIO "computos" EN LA BASE DE DATOS DE USUARIOS ---*/
if not can-find(first userdb._user where userdb._user._userid = "computos") then
  do transaction:
    create userdb._user.
    assign userdb._user._userid    = "computos"
           userdb._user._user-name = "Usuario para el desarrollo de sistemas"
           userdb._user._password  = encode("wifaro").
    release userdb._user.
  end.
release userdb._user.

/*--- SE ESTA CORRIENDO EN MODALIDAD DEMO? ---*/
find first par_estado no-lock.
if par_estado.accesos = ? then esdemo = no. else esdemo = yes.
tipo_mensaje = par_estado.tipo_login.
release par_estado.

/*--- DEFINO TITULO ---*/
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
  c-tit = "SIE-Core".
&ENDIF
if esdemo then c-tit = "DEMO - " + c-tit.

IMAGE-4:tooltip = "Copyrigth " + string(year(today)) + " Software Solutions. - Todos los derechos reservados".

/*--- PROCESO MODULOS HABILITADOS ---*/
for each par_grupos where (par_grupos.id_entorno = 1 or par_grupos.id_entorno = 2)
                           
                           &IF "{&MODALIDAD}" = "no" &THEN
                             and par_grupos.letra_inicial <> " " and par_grupos.estado = yes
                           &ENDIF
                           
                    no-lock:
  if tipo_mensaje then
    aux = par_grupos.nombre_grupo.
  else
    aux = par_grupos.descripcion_grupo.
  if can-find(first par_agrupados where par_agrupados.letra_grupo = par_grupos.letra_inicial) then
    aux = "* " + aux.
  if par_grupos.nombre_grupo = "Administrador" then
    aux = "# " + aux.
  stat = vcc_grupo:add-first(aux).
end.

&IF "{&MODALIDAD}" = "no" &THEN
  {../z_login.s3}
&ELSE
  {../z_login.d3}
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, retry MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, leave MAIN-BLOCK:

  SESSION:DATA-ENTRY-RETURN = TRUE.
  on return return.

  RUN enable_UI.

  &IF "{&MODALIDAD}" = "yes" &THEN
    if tipo_mensaje then
      vcc_grupo:screen-value = "# Administrador" no-error.
    else
      vcc_grupo:screen-value = "# Desarrollo de Sistemas" no-error.
  &ENDIF

  if not (frame {&FRAME-NAME}:title begins c-tit) then
    frame {&FRAME-NAME}:title = c-tit + frame {&FRAME-NAME}:title.

  {../z_setfun.i}  /*---SETEA FUNCIONES IDEM TTY ----*/ 

  WAIT-FOR GO OF FRAME {&FRAME-NAME} focus vcc_grupo pause 300.

END.
RUN disable_UI.
quit.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI login  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME login.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI login  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY vcc_grupo vlc_usuario vlc_clave 
      WITH FRAME login.
  ENABLE IMAGE-4 RECT-1 vcc_grupo vlc_usuario vlc_clave Btn_OK Btn_Ayuda 
         Btn_Cancel 
      WITH FRAME login.
  {&OPEN-BROWSERS-IN-QUERY-login}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

