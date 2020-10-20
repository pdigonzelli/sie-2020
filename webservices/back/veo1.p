
/*------------------------------------------------------------------------
    File        : veo1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Feb 16 17:00:15 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
for each balanza_tickets where fecha_entrada >= date('01/12/2015').
    disp balanza_tickets with 2 columns. 
    for each items_stock where items_stock.nro_partida =  balanza_tickets.nro_partida no-lock.
        disp items_stock.bultos items_stock.peso items_stock.cantidad with 2 columns.
    end.
end.