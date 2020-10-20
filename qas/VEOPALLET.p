
/*------------------------------------------------------------------------
    File        : VEOPALLET.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Sun Mar 06 23:00:36 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

  DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

/*FOR EACH PRODUCTOS_TERMINADOS.
    DISP    productos_terminados.DESCRIPCION
            productos_terminados.id_articulo_sap FORMAT 'X(20)'.
    UPDATE ID_articulo_SAP. 
    
FOR EACH envases_prod.
    DISP envases_prod.      
*/
/*
DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK(). */
FOR LAST PALLETS WHERE pallets.fecha_prod < DATE('31/12/2015') NO-LOCK.
    /*RUN PP3778NGP.P (PALLETS.ID_SUC_TRABAJO, pallets.id_pallet , '10' , '1' , '20' , '2' , OUTPUT CSTATUS) NO-ERROR.*/
    RUN PP377INGP.P (PALLETS.ID_SUC_TRABAJO,  pallets.id_pallet , OUTPUT CSTATUS).
END.

MESSAGE CSTATUS VIEW-AS ALERT-BOX TITLE 'epa'.
