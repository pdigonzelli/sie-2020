
/*------------------------------------------------------------------------
    File        : volcws.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 29 00:44:49 ART 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE INPUT PARAMETER CEMBALADOR       AS CHARACTER NO-UNDO.
DEFINE BUFFER aux_volcado FOR volcado_packing.

DEFINE VAR  PCRESPUESTA AS CHARACTER NO-UNDO.
DEFINE VAR  I AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
 SESSION:TIME-SOURCE = "produccion".
 
 
 FIND FIRST CONSULTAEMBALADOR NO-ERROR.
 IF NOT AVAILABLE CONSULTAEMBALADO THEN
    CREATE CONSULTAEMBALADOR.
 ASSIGN CONSULTAEMBALADOR.ID_EMBALADOR = INTEGER(CEMBALADOR).
 
 RELEASE CONSULTAEMBALADOR.
 
  
 
 CATCH E AS Progress.Lang.Error :
     
     DO I = 1 TO E:NumMessages:
        PCRESPUESTA = PCRESPUESTA + CHR(13) + E:GetMessage(I).
     END.
         
 END CATCH.
 
 FINALLY:
    RETURN PCRESPUESTA.
 END FINALLY.
 


/* **********************  Internal Procedures  *********************** */


 