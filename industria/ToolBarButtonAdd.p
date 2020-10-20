DEFINE INPUT PARAMETER hToolBar         AS HANDLE    NO-UNDO.
DEFINE INPUT PARAMETER pcActionName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonName     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonCaption  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonImage    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonOnChoose AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcButtonParent   AS CHARACTER NO-UNDO.

DEFINE VARIABLE xcColumns AS CHARACTER INITIAL "Name,Caption,Image,Type,OnChoose,AccessType,Parent".

&SCOP dlmt + CHR(1) +
 
/*
/* only to define a new Action Group - NO FUNCIONA            */
IF pcButtonParent <> "FUNCTION" THEN DO:
  DYNAMIC-FUNCTION("defineAction" IN hToolBar,
                                     pcButtonParent,               /* action group */  
                                     "Name,Caption",
                                     pcButtonParent {&dlmt}          /* Name    */
                                     pcButtonParent {&dlmt}         /* Caption */
                                     "").
END.
*/

/* define an action for my button */
DYNAMIC-FUNCTION("defineAction" IN hToolBar,
                                  pcActionName,
                                  xcColumns,
                                  pcButtonName     {&dlmt}   /* ButtonName*/
                                  pcButtonCaption  {&dlmt}   /* Caption*/
                                  pcButtonImage    {&dlmt}   /* Image */
                                  "PUBLISH"        {&dlmt}   /* TYPE */
                                  pcButtonOnChoose {&dlmt}   /* OnChoose */
                                  "READ"           {&dlmt}   /* AccessType */
                                  pcButtonParent)           /* parent *//* Parent - change it to FUNCTION if you don't want a new group */ 
  NO-ERROR.


                                                                                                     

