
/*------------------------------------------------------------------------
    File        : exportaXML.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : pdigonzelli
    Created     : Mon Mar 07 21:42:22 ACT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ttpallet.i}

DEFINE INPUT PARAMETER ARCHIVO AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR TTPALLET.
DEFINE INPUT PARAMETER TABLE FOR TTPOSICIONPALLET. 

DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hRow AS HANDLE NO-UNDO.
DEFINE VARIABLE hRowp AS HANDLE NO-UNDO.
DEFINE VARIABLE hRowp1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hField AS HANDLE NO-UNDO.
DEFINE VARIABLE hText AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld1 AS HANDLE NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hRow.
CREATE X-NODEREF hRowP.
CREATE X-NODEREF hRowP1.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.
hBuf = BUFFER TTPALLET:HANDLE.
hBuf1 = BUFFER TTPOSICIONPALLET:HANDLE.

hDoc:CREATE-NODE(hRoot,"Pallets","ELEMENT").
hDoc:APPEND-CHILD(hRoot).


FOR EACH TTPALLET.
    hDoc:CREATE-NODE(hRow,"PALLET","ELEMENT"). /* create  a row node */
    hRoot:APPEND-CHILD(hRow). /* put the row in the tree */    
    REPEAT ix = 1 TO hBuf:NUM-FIELDS:
        hDBFld = hBuf:BUFFER-FIELD(ix).
        /* create a tag with the field name */
        hDoc:CREATE-NODE(hField, hDBFld:NAME, "ELEMENT").
        /* Put the new field as next child of row */
        hRow:APPEND-CHILD(hField).
        /* Add a node to hold field value. The empty string (““) represents the
        value that will be set later. */
        hDoc:CREATE-NODE(hText, "", "TEXT").
        /* Attach the text to the field */
        hField:APPEND-CHILD(hText).
        hText:NODE-VALUE = STRING(hDBFld:BUFFER-VALUE).
    END.    
    hDoc:CREATE-NODE(hRowP,"POSICIONES","ELEMENT"). /* create  a row node */
    hRow:APPEND-CHILD(hRowP). /* put the row in the tree */    
    FOR EACH TTPOSICIONPALLET.
        hDoc:CREATE-NODE(hRowP1,"POSICION","ELEMENT"). /* create  a row node */
        hRowP:APPEND-CHILD(hRowP1). /* put the row in the tree */    
        REPEAT ix = 1 TO hBuf1:NUM-FIELDS:
            hDBFld = hBuf1:BUFFER-FIELD(ix).
            /* create a tag with the field name */
            hDoc:CREATE-NODE(hField, hDBFld:NAME, "ELEMENT").
            /* Put the new field as next child of row */
            hRowP1:APPEND-CHILD(hField).
            /* Add a node to hold field value. The empty string (““) represents the
            value that will be set later. */
            hDoc:CREATE-NODE(hText, "", "TEXT").
            /* Attach the text to the field */
            hField:APPEND-CHILD(hText).
            hText:NODE-VALUE = STRING(hDBFld:BUFFER-VALUE).
        END.    
    END.
    
END.  


hDoc:SAVE("file", ARCHIVO).

DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hRow.
DELETE OBJECT hRowP.
DELETE OBJECT hRowP1.
DELETE OBJECT hField.
DELETE OBJECT hText.
      