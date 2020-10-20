FUNCTION getColumnLetter RETURNS CHARACTER ( idx AS INTEGER ) .

DEFINE VARIABLE lista AS CHARACTER  NO-UNDO INITIAL "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".
DEFINE VARIABLE pos   AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1   AS INTEGER NO-UNDO.


pos = TRUNCATE(idx / 26 , 0).
pos1 = idx MOD 26.

IF pos1 = 0 THEN
    ASSIGN 
        pos = pos - 1 
        pos1 = 26.

RETURN trim(( IF pos > 0 THEN ENTRY( pos,lista ) ELSE "" ) + 
                IF pos1 > 0 THEN ENTRY( pos1 , lista) ELSE "" ).

END FUNCTION.
