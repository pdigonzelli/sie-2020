
/*------------------------------------------------------------------------
    File        : dataset1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 11 17:45:27 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE TT 
    FIELD I AS INTEGER.
    
    
DEFINE DATASET DS 
    FOR TT.
    
    
CREATE  TT.
ASSIGN TT.I = 1000.

PAUSE.


    
  