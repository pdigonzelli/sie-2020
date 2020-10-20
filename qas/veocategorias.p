
/*------------------------------------------------------------------------
    File        : veocategorias.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Mar 04 16:17:36 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

for last balanza_tickets no-lock where loteagricolasap <> ''.
    disp balanza_tickets.loteagricolasap format 'x(20)'
         balanza_tickets.id_lote_senasa.
