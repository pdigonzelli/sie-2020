
/*------------------------------------------------------------------------
    File        : veopallets.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Fri Apr 29 09:23:10 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


for each pallets where (id_suc_trabajo = 97 OR id_suc_trabajo = 98) and pallets.fecha_prod >= TODAY and pallets.hora_prod < string(TIME - 600 , 'HH:MM:SS') and
    pallets.id_pallet_sap = '' no-lock.
    disp pallets.fecha_operativa pallets.fecha_prod pallets.hora_prod with 2 columns.