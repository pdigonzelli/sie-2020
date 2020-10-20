DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hTable AS HANDLE.
DEFINE VARIABLE hField AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hBuf AS HANDLE.
DEFINE VARIABLE hDBFld AS HANDLE.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.


CREATE X-DOCUMENT hDoc.
CREATE X-NODEREF hRoot.
CREATE X-NODEREF hTable.
CREATE X-NODEREF hField.
CREATE X-NODEREF hText.


/*read in the file created in the last example*/
hDoc:LOAD("file", "c:\temp\sucursalescliente.xml", FALSE).
hDoc:GET-DOCUMENT-ELEMENT(hRoot).

/*read each from the root*/
REPEAT i = 1 TO hRoot:NUM-CHILDREN:
  hRoot:GET-CHILD(hTable,i).
  REPEAT j = 1 TO hTable:NUM-CHILDREN:
    hTable:GET-CHILD(hField,j).
    IF hField:NUM-CHILDREN < 1 THEN NEXT.
    /*skip any null value*/
    hField:GET-CHILD(hText,1).
    /*get the text value of the field*/
    CASE hField:NAME:

    END CASE.

    MESSAGE hField:NAME hText:NODE-VALUE
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

DELETE OBJECT hDoc.
DELETE OBJECT hRoot.
DELETE OBJECT hTable.
DELETE OBJECT hField.
DELETE OBJECT hText.

