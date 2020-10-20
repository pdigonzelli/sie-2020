/*********************************************/
/* LIBRERIA EN BASE DE BROWSE en modo GRafico*/
/* AUTOR: CACHA                              */
/* FECHA: 28/08/01                           */
/*********************************************/

/* PARAMETROS *********************************************************
    &tablas-query = Nombre de tablas para el Query principal
    &tabla     = Nombre de tabla principal a controlar con este ABM 
    &baja-logica = Valores "no" o "yes" Determina Si sera Baja Logica
    &defvari   = Definicion de Variables
    &funciones = Definicion de funciones del usuario
    &condeach  = Condicion FOR EACH para el query
    &dispbrow  = Display de la Sentencia BROWSE Cpos a Mostrar
    &posibrow  = Indicaciones de la posicion del BROWSE principal
    &cposform  = Campos del FORM para Alta Y Modificacion
    &with-form = Caracteristicas del FORM de Alta y Modificacion
    &dispform  = Display de los Campos No Editables en la Modificacion
    &cposupda  = Campos Update, Usados en el Update de Alta
    &cposupdm  = Campos Update, Usados en el Update de Modificacion
    &ampliainf = Ampliacion de Informacacion Sobre el BROWSE
    &precreate = Procesos antes del CREATE (alta)
    &poscreate = Procesos despues del UPDATE (alta)
    &preupdate = Procesos antes del UPDATE (modificacion)
    &posupdate = Procesos despues del UPDATE (modificacion)
    &prepredel = Procesos antes de la pregunta del DELETE (baja)
    &predelete = Procesos antes del DELETE (baja)
    &posdelete = Procesos despues del DELETE (baja)
    &eventos   = Eventos Particulares y Especiales
    &finprog   = Procesos despues del fin del programa
    &helpbrow  = Help del Browse se eliminan message para modo grafico
    
    NOTA :
    En la Creacion de eventos particulares (&eventos) estos deben
    referenciarse SIEMPRE a objetos que esten contenidos en el 
    marco _M-ALTMOD.
    
    Tambien se pueden aplicar eventos sobre el browse principal 
    _B-BROWSE en el marco _TRABAJO.
    
    Lista de Objetos definidos en la Libreria:
        
    Query Principal  = _Q-QUERY
    Browse Principal = _B-BROWSE
    Frame Principal  = _TRABAJO
    Variable ROWID   = CURR-RECORD
***********************************************************************/

DEFINE VARIABLE V-CONSULTA AS LOGICAL.
DEFINE VARIABLE VAR-LOGICA AS LOGICAL FORMAT "Si/No".
DEFINE VARIABLE CURR-RECORD AS ROWID.
DEFINE VARIABLE SEQUENCIA AS LOGICAL.

    
{{&defvari}}
{{&funciones}}

/**************  BROWSES **********************/
DEFINE QUERY _Q-QUERY FOR {&tablas-query} SCROLLING. 
DEFINE BROWSE _B-BROWSE QUERY _Q-QUERY
                    {{&dispbrow}}.

/*********** FORMULARIOS  DEL BROWSE PRINCIPAL **************/
DEFINE RECTANGLE vrect SIZE 65 BY 12 EDGE-PIXELS 2.

 DEFINE FRAME _TRABAJO
     _B-BROWSE HELP {{&helpbrow}}
     BACKGROUND
     vrect 
     WITH {&posibrow}.

/* FORMS PARTICULARES PARA ALTA Y MODIFICACION */
FORM
{{&cposform}} WITH FRAME _M-ALTMOD {&with-form}.

FORM
    VAR-LOGICA 
               LABEL "Ingrese ""S"" para confirmar eliminacion."
    WITH FRAME _M-BAJA SIDE-LABELS TITLE " Eliminacion de Datos " 
    CENTERED ROW 14 OVERLAY FONT 10 THREE-D.


/*******  EVENTOS ***********/
{{&eventos}}

/********* TECLAS DE AYUDA *************/
ON end-error OF FRAME _M-ALTMOD
    DO:
        HIDE FRAME _M-ALTMOD.
/*        RUN MENSAJE.
        PAUSE 0.*/
    END. 

ON end-error OF FRAME _TRABAJO
    DO:
        HIDE FRAME _TRABAJO.
    END.

ON end-error OF FRAME _M-BAJA
    DO:
        HIDE FRAME _M-BAJA.
    END.
   
    
ON F7 OF _B-BROWSE /* AMPLIA INFORMACION */
    DO:                        
         HIDE MESSAGE.
         {{&ampliainf}}
         RUN MENSAJE.
    END.
    

/***********************************/

ON VALUE-CHANGED OF _B-BROWSE 
    DO:
        ASSIGN CURR-RECORD = ROWID({&tabla}).
        ASSIGN SEQUENCIA = TRUE.
    END.

ON "INSERT-MODE" OF _B-BROWSE /* ALTA */
      DO:                      
        HIDE MESSAGE.
        REPEAT:
         ASSIGN CURR-RECORD = ?.
         {{&precreate}}
         IF SEQUENCIA THEN DO:
             CREATE {&tabla}.
             UPDATE
                 {{&cposupda}}
             WITH FRAME _M-ALTMOD.
         END.             
         {{&poscreate}}
         CURR-RECORD = ROWID({&tabla}).
         RUN AUDITA.    
         HIDE FRAME _M-ALTMOD.       
         OPEN QUERY _Q-QUERY {{&condeach}}.
         REPOSITION _Q-QUERY TO ROWID(CURR-RECORD) NO-ERROR. 
         ENABLE _B-BROWSE WITH FRAME _TRABAJO OVERLAY. 
         PAUSE 0.
        END.
        IF CURR-RECORD = ? AND AVAILABLE {&tabla} THEN DELETE {&tabla}. 
        ASSIGN SEQUENCIA = TRUE.
        APPLY "VALUE-CHANGED" TO _B-BROWSE.
        RUN MENSAJE. 
   END.

ON "DELETE-CHARACTER" OF _B-BROWSE  /* BAJA */
   DO:                         
         HIDE MESSAGE.
         FIND {&tabla} WHERE ROWID({&tabla}) = CURR-RECORD 
                         EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE {&tabla} THEN DO:
            RUN MENSAJE.
            RETURN NO-APPLY.
         END.
         VAR-LOGICA = FALSE.
         {{&prepredel}}
         REPEAT:
         UPDATE VAR-LOGICA 
                WITH FRAME _M-BAJA. 
             
         IF VAR-LOGICA = TRUE THEN 
            DO:
              {{&predelete}}
              IF {&baja-logica} then
                    RUN AUDITA.
               ELSE
                    DELETE {&tabla}.
              {{&posdelete}}
              HIDE FRAME _M-BAJA.
              GET NEXT _Q-QUERY.
              CURR-RECORD = ROWID({&tabla}).
              REPOSITION _Q-QUERY TO ROWID(CURR-RECORD) NO-ERROR. 
              IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 3165 THEN
                 DO:
                        GET PREV _Q-QUERY.
                        GET PREV _Q-QUERY.
                        CURR-RECORD = ROWID({&tabla}). 
                 END. 
              OPEN QUERY _Q-QUERY {{&condeach}}.
              ENABLE _B-BROWSE WITH FRAME _TRABAJO OVERLAY.
              IF CURR-RECORD = ? THEN LEAVE. /* TABLA VACIA */  
              REPOSITION _Q-QUERY TO ROWID(CURR-RECORD).
            END.
            LEAVE.                
         END.                
         HIDE FRAME _M-BAJA.                
         RUN MENSAJE.
         
   END.


ON "RETURN" OF _B-BROWSE
   DO:                          /* MODIFICACION */
         HIDE MESSAGE.
         REPOSITION _Q-QUERY TO ROWID(CURR-RECORD).
         FIND {&tabla} WHERE ROWID({&tabla}) = CURR-RECORD
         EXCLUSIVE-LOCK.
         REPEAT:
         {{&preupdate}}         
         IF SEQUENCIA THEN DO:
             display {{&dispform}} with frame _M-ALTMOD.
             UPDATE
                 {{&cposupdm}}
             WITH FRAME _M-ALTMOD.
         END.             
         {{&posupdate}}
/**********PERMITE AVANZAR EN EL BROWSE ***********/
         GET NEXT _Q-QUERY.                                              
         CURR-RECORD = ROWID({&tabla}).                              
         REPOSITION _Q-QUERY TO ROWID(CURR-RECORD) NO-ERROR.             
         IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 3165 THEN                DO:                                                                                 GET PREV _Q-QUERY.                                       
                CURR-RECORD = ROWID({&tabla}).                                            END.                                                           
/*************************************/         
         
         LEAVE.
         END.
         OPEN QUERY _Q-QUERY {{&condeach}}.
         ENABLE _B-BROWSE WITH FRAME _TRABAJO OVERLAY.
         REPOSITION _Q-QUERY TO ROWID(CURR-RECORD).
         HIDE FRAME _M-ALTMOD.       
         RUN MENSAJE.
         ASSIGN SEQUENCIA = TRUE.
         RUN AUDITA.
    END.    

ASSIGN SEQUENCIA = TRUE.
OPEN QUERY _Q-QUERY {{&condeach}}.
 ENABLE _B-BROWSE WITH FRAME _TRABAJO OVERLAY.
APPLY "ENTRY" TO _B-BROWSE.
APPLY "VALUE-CHANGED" TO _B-BROWSE.
RUN MENSAJE.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
{{&finprog}}
PROCEDURE MENSAJE.
/*
MESSAGE SUBSTRING(" " + kblabel("INS")           + "=Inserta  " +
                        kblabel("DEL")           + "=Elimina  " +
                        kblabel("ENTER")         + "=Modifica  " +
                        kblabel("F7")            + "=Amplia " +
                        kblabel("F6")            + "=Busca " +
                        kblabel("END-ERROR")     + "=Sale" +
                        FILL(" ",80), 1, 79).                  

MESSAGE SUBSTRING(" " + {{&mensaje}} + FILL(" ",80), 1, 79). 
  */                      
END.

PROCEDURE AUDITA.
    FIND {&tabla} where ROWID({&tabla}) = CURR-RECORD 
    EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN {&tabla}.c_fecha = today
           {&tabla}.c_usuario = userid("userdb")
           {&tabla}.c_hora = STRING(TIME,"hh:mm:ss").
END PROCEDURE.


 
                
