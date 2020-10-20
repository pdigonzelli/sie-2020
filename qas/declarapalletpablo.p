
/*------------------------------------------------------------------------
    File        : declarapalletpablo.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Tue Apr 11 18:59:56 ACT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


FIND PALLETS WHERE pallets.id_pallet = 17000245 AND ID_SUC_TRABAJO = 98.

RUN DECLARAPALLETSAPNUEVO(PALLETS.ID_SUC_TRABAJO, PALLETS.ID_PALLET).



CATCH e AS Progress.Lang.Error :
    MESSAGE E:GETMESSAGE(1) VIEW-AS ALERT-BOX.
    
    UNDO, RETURN.
		
END CATCH.

   
{declarapalletn.i}