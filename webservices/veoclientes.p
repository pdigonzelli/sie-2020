
/*------------------------------------------------------------------------
    File        : veoclientes.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Mar 04 13:59:26 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
for each clientes_ventas where id_cliente >=  0001100026 no-lock.
    disp id_cliente nombre id_cliente_sap.