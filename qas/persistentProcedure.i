{2} = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE({2}) AND NOT {2}:FILE-NAME matches('*{1}*'):
    {2} = {2}:NEXT-SIBLING.
END.
IF NOT VALID-HANDLE ({2}) THEN 
    RUN {1} PERSISTENT SET {2}.

/*
REPEAT :
    IF VALID-HANDLE({2}) THEN
        IF {2}:FILE-NAME matches('*{1}*') THEN
            LEAVE.
    ELSE 
        LEAVE.
END.

IF NOT VALID-HANDLE ({2}) THEN 
    RUN {1} PERSISTENT SET {2}.
ELSE
    IF NOT {2}:FILE-NAME matches('*{1}*') THEN
        RUN {1} PERSISTENT SET {2}.
*/
