
/*------------------------------------------------------------------------
    File        : perfil.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Dec 04 09:30:36 ART 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE NEW SHARED VARIABLE appperfil AS CLASS Perfil NO-UNDO.

appperfil = Perfil:Instance.

&IF "{&vperfil}" <> "" &THEN
    appperfil:NombrePerfil = {&vperfil}.
&ENDIF


