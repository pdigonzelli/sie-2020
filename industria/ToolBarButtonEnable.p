DEFINE INPUT PARAMETER hToolBar         AS HANDLE NO-UNDO.
DEFINE INPUT PARAMETER pcActionName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonOnChoose AS CHARACTER NO-UNDO.
/*
DYNAMIC-FUNCTION('createToolbar' IN hToolBar, pcActionName).
*/
DYNAMIC-FUNCTION('enableActions' IN hToolBar, pcActionName).

SUBSCRIBE TO pcButtonOnChoose IN hToolBar.

