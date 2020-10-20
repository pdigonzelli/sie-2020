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
DEFINE SHARED VARIABLE vlc_host          AS CHARACTER.
DEFINE SHARED VARIABLE vlc_misc          AS CHARACTER NO-UNDO INITIAL ?.
DEFINE SHARED VARIABLE vlc_file_server   AS CHARACTER.
DEFINE SHARED VARIABLE vlc_print_server  AS CHARACTER.
DEFINE SHARED VARIABLE vlc_dir_fuentes   AS CHARACTER.
DEFINE SHARED VARIABLE vlc_dir_objetos   AS CHARACTER.
DEFINE SHARED VARIABLE vlc_dir_spool     AS CHARACTER.
DEFINE SHARED VARIABLE vlc_comando       AS CHARACTER.
DEFINE SHARED VARIABLE h_sesion          AS HANDLE.

/*--- VARIABLES DEL PROGRAMA ---*/
DEFINE VARIABLE vlc_archivo  AS CHARACTER.
DEFINE VARIABLE vlc_datos    AS CHARACTER FORMAT "x(73)".
DEFINE VARIABLE vli_contador AS INTEGER   INITIAL 0.
DEFINE VARIABLE vli_posicion AS INTEGER   INITIAL 0.
DEFINE VARIABLE vli_intentos AS INTEGER   NO-UNDO INITIAL 3.
DEFINE VARIABLE vlc_basedato AS CHARACTER.
DEFINE VARIABLE vlc_mensaje  AS CHARACTER.
DEFINE VARIABLE vlc_formato  AS CHARACTER FORMAT "x(80)" EXTENT 22.
DEFINE VARIABLE vlc_logueado AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE vlc_tabla    AS CHARACTER EXTENT 40.
DEFINE VARIABLE salida       AS LOGICAL.
DEFINE VARIABLE path         AS CHARACTER.
DEFINE VARIABLE lista_grupos AS CHARACTER.
DEFINE VARIABLE cc           AS INTEGER.
DEFINE VARIABLE stat         AS LOGICAL.
DEFINE VARIABLE vlc_grupo    AS CHARACTER.
DEFINE VARIABLE aux          AS CHARACTER.
DEFINE VARIABLE defpropath   AS CHARACTER.
DEFINE VARIABLE dirpro       AS CHARACTER.
DEFINE VARIABLE letra        LIKE par_grupos.letra_inicial.
DEFINE VARIABLE tipo_mensaje LIKE par_estado.tipo_login.
DEFINE VARIABLE id_grupo     LIKE par_grupos.letra_inicial.
DEFINE VARIABLE x_proversion AS INTEGER.
DEFINE VARIABLE c1           AS INTEGER.
DEFINE VARIABLE c2           AS INTEGER.
DEFINE VARIABLE c3           AS CHARACTER.
DEFINE VARIABLE hMnu AS WIDGET-HANDLE     NO-UNDO.

DEFINE TEMP-TABLE auxlic LIKE est_grupos.

/* Local Variable Definitions ---*/
DEFINE VARIABLE intentos         AS INTEGER INITIAL 0.
DEFINE VARIABLE malo             AS LOGICAL INITIAL FALSE.

DEFINE BUFFER xxx_grupos FOR par_grupos.

DEFINE TEMP-TABLE archivos
    FIELD t-arch AS CHARACTER
    FIELD t-farch AS CHARACTER
    FIELD t-tipo AS CHARACTER.

DEFINE NEW SHARED VAR vlc_empresa AS INTEGER INITIAL 1.

DEFINE VARIABLE c-sep AS CHAR NO-UNDO.
DEFINE VARIABLE c-tit AS CHAR NO-UNDO.
DEFINE VARIABLE sal   AS LOGICAL.

DEFINE VARIABLE respuesta    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE archivo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE conexion     AS CHARACTER NO-UNDO.
DEFINE VARIABLE esdemo       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE contador     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lista        AS CHARACTER NO-UNDO.

DEFINE VARIABLE auxpwd AS CHARACTER NO-UNDO INITIAL "m71t134y51p28".

SESSION:TIME-SOURCE = "general".

/* DEFINO LA BARRA SEPARADORA SEGUN EL SISTEMA OPERATIVO */
IF OPSYS = "UNIX" THEN
    ASSIGN c-sep = "/".
ELSE
    ASSIGN c-sep = "~\".

/*--- VERSION DE PROGRESS EJECUTµNDOSE ---*/
x_proversion = INTEGER(SUBSTRING(PROVERSION,1, INDEX(PROVERSION,".") - 1)).

/*--- DEFAULT PROPATH PROGRESS (puede variar conforme a la versi¢n de Progress que se utilize) ---*/
ASSIGN dirpro     = OS-GETENV("DLC")
       defpropath = ?.

IF dirpro <> ? AND x_proversion < 10 THEN
  DO:

    IF SESSION:DISPLAY-TYPE = "GUI" THEN
      DO:
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
      END.
    ELSE
      DO:
        IF OPSYS = "UNIX" THEN
          defpropath = dirpro + c-sep + "tty," +
                       dirpro + c-sep + "tty" + c-sep + "as4dict.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeedit.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adeshar.pl," +
                       dirpro + c-sep + "tty" + c-sep + "prodict.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adecomm.pl," +
                       dirpro + c-sep + "tty" + c-sep + "adecomp.pl," +
                       dirpro + "," +
                       dirpro + c-sep + "bin".
        ELSE
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
      END.

  END.

&IF "{&MODALIDAD}" = "yes" AND "{&SISTEMA}" = "core" &THEN
   defpropath = vlc_dir_fuentes + "," + vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "custom," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "gui," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "src," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core," +
                vlc_dir_fuentes + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                (IF defpropath <> ? THEN defpropath ELSE "").
&ENDIF

&IF "{&MODALIDAD}" = "no" AND "{&SISTEMA}" = "core" &THEN
   defpropath = vlc_dir_objetos + c-sep + vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "custom," +
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "gui," +
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "src," +
                vlc_dir_objetos + "supervisor" + c-sep + "core," + 
                vlc_dir_objetos + "supervisor" + c-sep + "core" + c-sep + "supervisor," +
                (IF defpropath <> ? THEN defpropath ELSE "").
&ENDIF

&IF "{&SISTEMA}" = "gessi" OR "{&SISTEMA}" = "posmaster" OR "{&SISTEMA}" = "core" OR "{&SISTEMA}" = "contacto" &THEN
  defpropath = (IF defpropath <> ? THEN defpropath + "," ELSE "") + 
               &IF "{&MODALIDAD}" = "yes" &THEN vlc_dir_fuentes &ELSE vlc_dir_objetos + "gui" + (IF OPSYS = "UNIX" THEN "/" ELSE "~\") &ENDIF
               + "supervisor" + (IF OPSYS = "UNIX" THEN "/" ELSE "~\") + "triggers,".
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
    QUIT.
    /*---- APPLY "END-ERROR":U TO SELF.        -----*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda login
ON CHOOSE OF Btn_Ayuda IN FRAME login /* Ayuda */
DO:
  BELL.
  MESSAGE 
    "Este programa permite ingresar al sistema. Para hacerlo debe seleccionar el"      SKIP
    "m¢dulo o grupo de m¢dulos en la posici¢n indicada con el nombre 'Sistema', y"     SKIP
    "luego ingresar en las posiciones 'Usuario' y 'Clave', el nombre de usuario y"     SKIP
    "clave de acceso que le entreg¢ la Gerencia de Inform tica cuando lo habilit¢"     SKIP
    "para acceder a estas aplicaciones. Luego de ingresar estos datos, debe presionar" SKIP
    "el bot¢n 'OK' para entrar, o bien presionar el bot¢n 'Salir' para abandonar."     SKIP
    "S¢lo los m¢dulos o grupos de m¢dulos disponibles aparecer n en la lista que se"   SKIP
    "despliega en la posici¢n 'Sistema'. Si el nombre indicado est  precedido por"     SKIP
    "un asterisco '*', esto indica que se trata de un grupo de m¢dulos. Si est "       SKIP
    "precedido por un numeral '#' implica que se trata de un m¢dulo especial, el"      SKIP
    "cual s¢lo ser  accesible por la Gerencia de Inform tica. Muchas Gracias."
  VIEW-AS ALERT-BOX MESSAGE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel login
ON CHOOSE OF Btn_Cancel IN FRAME login /* Salir */
DO:
    QUIT.
    /*---- APPLY "END-ERROR":U TO SELF.        -------*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK login
ON CHOOSE OF Btn_OK IN FRAME login /* OK */
DO:
    APPLY "VALUE-CHANGED" TO vcc_grupo IN FRAME login.
    ON RETURN RETURN.
    
    
    ASSIGN  intentos     = intentos + 1
            malo         = FALSE
            vlc_usuario  = LC(INPUT vlc_usuario)
            vlc_clave    = LC(INPUT vlc_clave)
            vlc_misc     = ?.
    IF vlc_grupo <> "" AND vlc_usuario <> "" THEN
    DO:
        FIND FIRST par_grupos WHERE par_grupos.nombre_grupo = vlc_grupo 
                                    &IF "{&MODALIDAD}" = "no" &THEN
                                      AND par_grupos.estado = YES
                                    &ENDIF
                             NO-LOCK NO-ERROR.
        IF AVAILABLE par_grupos THEN
        DO:
             IF vlc_grupo <> "administrador" THEN
                 vlc_usuario = par_grupos.letra_inicial + "_" + vlc_usuario.

             IF tipo_mensaje = NO AND vlc_usuario <> "computos" AND id_grupo <> "" AND substring(vlc_usuario,1,1) <> id_grupo THEN
               ASSIGN vlc_usuario = id_grupo + substring(vlc_usuario,2).

               IF vlc_clave = "" THEN
               DO:
                 FIND userdb._user WHERE userdb._user._userid = vlc_usuario NO-LOCK NO-ERROR.
                 IF AVAILABLE userdb._user THEN
                   IF userdb._user._passw = encode(auxpwd) THEN
                     vlc_clave = auxpwd.
                 RELEASE userdb._user.
               END.
               IF SETUSERID (vlc_usuario,vlc_clave,"userdb") THEN
                DO:
                  vlc_misc = vlc_clave.
                  IF USERID("userdb") = "computos" THEN
                   DO:
                      {../s_login.iii}

                      &IF "{&MODALIDAD}" = "no" &THEN
                        {../z_login.s1}
                      &ELSE
                        {../z_login.d1}
                      &ENDIF

                      
                      
                      RUN s_mail.p.
                      
                      
                      
/*                      RUN supervisor/gPerfil.w. */                      
                      RUN disable_UI.
                      RUN _desk.p.
                      ASSIGN vlc_grupo   = ""
                             vlc_usuario = ""
                             vlc_clave   = ""
                             vlc_misc    = ?
                             id_grupo    = "".
                      RUN enable_UI.
                      ON RETURN RETURN.
                      APPLY "ENTRY" TO vcc_grupo IN FRAME login.
                   END.
                  ELSE
                   DO:
                      ASSIGN lista_grupos = "" id_grupo = "".
                      FOR EACH par_agrupados USE-INDEX orden WHERE par_agrupados.letra_grupo = par_grupos.letra_inicial NO-LOCK:
                        FIND xxx_grupos WHERE xxx_grupos.letra_inicial = par_agrupados.letra_inicial AND 
                                              xxx_grupos.estado = YES AND (xxx_grupos.id_entorno = 1 OR xxx_grupos.id_entorno = 2)
                                        NO-LOCK NO-ERROR.
                        IF AVAILABLE xxx_grupos THEN
                          DO:
                            IF LOOKUP(LC(TRIM(xxx_grupos.nombre_grupo)), lista_grupos) = 0 THEN
                              lista_grupos = lista_grupos + lc(TRIM(xxx_grupos.nombre_grupo)) + ",".
                            id_grupo = id_grupo + lc(TRIM(xxx_grupos.letra_inicial)) + ",".
                          END.
                      END.
                      IF lista_grupos = "" THEN
                        lista_grupos = vlc_grupo.
                      ELSE
                        ASSIGN lista_grupos = SUBSTRING(lista_grupos,1,LENGTH(lista_grupos) - 1)
                               id_grupo     = SUBSTRING(id_grupo,1,LENGTH(id_grupo) - 1).
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
                      IF vlc_misc = auxpwd THEN
                        DO:
                          MESSAGE "Debe Asignar una Contrase¤a a su Cuenta !!!" SKIP(1)
                                  "A continuaci¢n se le pedir  que escriba una contrase¤a." SKIP
                                  "Luego deber  reingresarla para verificar la misma." SKIP
                                  "Por favor, recuerde la contrase¤a ingresada."
                                  VIEW-AS ALERT-BOX INFORMATION.
                          RUN h_passwd.p.
                        END.

                      /*--- EJECUTA MENU DE OPCIONES ---*/
                      IF id_grupo = "" THEN
                      DO:
                      
                        /*-- MENU INTERFAZ GRAFICA TRADICIONAL --*/
                        IF vlc_grupo = "industria" THEN DO:
                          RUN ..\industria\wInicioIndustria.w PERSISTENT SET hMnu.
                          RUN initializeObject IN hMnu.
                          WAIT-FOR CLOSE OF hMnu.
                        END.
                        ELSE
                          RUN ../supervisor/z_menu.p.
                      END.
                      ELSE
                       
                        IF vlc_grupo = "industria" THEN DO:
                          RUN ..\industria\wInicioIndustria.w PERSISTENT SET hMnu.
                          RUN initializeObject IN hMnu.
                          WAIT-FOR CLOSE OF hMnu.
                        END.
                        ELSE
                        DO:                      

                        /*-- MENU INTERFAZ GRAFICA NUEVA --*/
                            IF SESSION:HEIGHT-PIXELS >= 768 AND SESSION:WIDTH-PIXELS >= 1024 THEN
                              IF SESSION:HEIGHT-PIXELS >= 1024 AND SESSION:WIDTH-PIXELS >= 1280 THEN
                                RUN w_inixlt.w (INPUT id_grupo).
                              ELSE
                                RUN w_inibig.w (INPUT id_grupo).
                            ELSE
                              RUN w_inicio.w (INPUT id_grupo).
                        END.
                        {../s_login.i} 
                        ASSIGN  vlc_grupo   = ""
                                vlc_usuario = ""
                                vlc_clave   = ""
                                vlc_misc    = ?
                                id_grupo    = "".
                      RUN enable_UI.
                      ON RETURN RETURN.
                      APPLY "ENTRY" TO vcc_grupo IN FRAME login.
                   END.
                END.
               ELSE 
                malo = TRUE.
        END.
        ELSE malo = TRUE.
    END.
    ELSE malo = TRUE.    
    
    IF malo THEN
    DO:
       IF intentos >= 3 THEN 
       DO:
         RUN s_inform.p (4, INPUT vlc_logueado).
         APPLY "END-ERROR":U TO SELF.       
       END.
    
       BELL.
       input clear.
       ON RETURN RETURN.
       MESSAGE "­ Sistema/M¢dulo, Usuario o Contrase¤a Incorrectas !" SKIP
               " Le quedan " + string(3 - intentos) + " intentos. "
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
       ASSIGN   vcc_grupo:screen-value  = ""
                vlc_usuario:screen-value  = ""
                vlc_clave:screen-value = ""
                vlc_clave:sensitive = FALSE
                vlc_usuario:sensitive = FALSE.
       APPLY "BACK-TAB":U TO SELF.
       ASSIGN   vlc_clave:sensitive = TRUE
                vlc_usuario:sensitive = TRUE.
       
    END.
    ELSE
      intentos = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME IMAGE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL IMAGE-4 login
ON MOUSE-SELECT-CLICK OF IMAGE-4 IN FRAME login
DO:
  RUN ../copyrigth.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vcc_grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vcc_grupo login
ON VALUE-CHANGED OF vcc_grupo IN FRAME login /* Sistema */
DO:
  vlc_grupo = LC(TRIM(vcc_grupo:screen-value)).
  IF vlc_grupo BEGINS "*" THEN
    vlc_grupo = SUBSTRING(vlc_grupo,3).
  IF vlc_grupo BEGINS "#" THEN
    vlc_grupo = SUBSTRING(vlc_grupo,3).
  IF tipo_mensaje = NO THEN
    DO:
      FIND FIRST par_grupos WHERE par_grupos.descripcion_grupo BEGINS vlc_grupo NO-LOCK.
      ASSIGN vlc_grupo = TRIM(LC(par_grupos.nombre_grupo))
             id_grupo  = TRIM(LC(par_grupos.letra_inicial)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK login 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/*--- VERIFICO LA EXISTENCIA DEL USUARIO "computos" EN LA BASE DE DATOS DE USUARIOS ---*/
/*
if not can-find(first userdb._user where userdb._user._userid = "computos") then
  do transaction:
    create userdb._user.
    assign userdb._user._userid    = "computos"
           userdb._user._user-name = "Usuario para el desarrollo de sistemas"
           userdb._user._password  = encode("wifaro").
    release userdb._user.
  end.
release userdb._user.
*/
/*--- SE ESTA CORRIENDO EN MODALIDAD DEMO? ---*/
FIND FIRST par_estado NO-LOCK.
IF par_estado.accesos = ? THEN esdemo = NO. ELSE esdemo = YES.
tipo_mensaje = par_estado.tipo_login.
RELEASE par_estado.

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
IF esdemo THEN c-tit = "DEMO - " + c-tit.

IMAGE-4:tooltip = "Copyrigth " + string(YEAR(TODAY)) + " Software Solutions. - Todos los derechos reservados".

/*--- PROCESO MODULOS HABILITADOS ---*/
FOR EACH par_grupos WHERE (par_grupos.id_entorno = 1 OR par_grupos.id_entorno = 2)
                           &IF "{&MODALIDAD}" = "no" &THEN
                             AND par_grupos.letra_inicial <> " " AND par_grupos.estado = YES
                           &ENDIF
                    NO-LOCK:
  IF tipo_mensaje THEN
    aux = par_grupos.nombre_grupo.
  ELSE
    aux = par_grupos.descripcion_grupo.
  IF CAN-FIND(FIRST par_agrupados WHERE par_agrupados.letra_grupo = par_grupos.letra_inicial) THEN
    aux = "* " + aux.
  IF par_grupos.nombre_grupo = "Administrador" THEN
    aux = "# " + aux.
  stat = vcc_grupo:add-first(aux).
END.

&IF "{&MODALIDAD}" = "no" &THEN
  {../z_login.s3}
&ELSE
  {../z_login.d3}
&ENDIF


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, RETRY MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  SESSION:DATA-ENTRY-RETURN = TRUE.
  ON RETURN RETURN.

  RUN enable_UI.

  &IF "{&MODALIDAD}" = "yes" &THEN
    IF tipo_mensaje THEN
      vcc_grupo:screen-value = "# Administrador" NO-ERROR.
    ELSE
      vcc_grupo:screen-value = "# Desarrollo de Sistemas" NO-ERROR.
  &ENDIF

  IF NOT (FRAME {&FRAME-NAME}:title BEGINS c-tit) THEN
    FRAME {&FRAME-NAME}:title = c-tit + FRAME {&FRAME-NAME}:title.

  {../z_setfun.i}  /*---SETEA FUNCIONES IDEM TTY ----*/ 

  WAIT-FOR GO OF FRAME {&FRAME-NAME} FOCUS vcc_grupo PAUSE 300.

  RUN wperfil.w.
    
END.
RUN disable_UI.
QUIT.

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

