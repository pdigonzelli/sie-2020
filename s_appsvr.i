define variable ret as logical no-undo.

create server hserver.
ret = hserver:connect("{1}") no-error.
if not hserver:connected() then
do:
    message "Error de Conexi�n con AppServer - Avise a la Gerencia de Sistemas !!!" view-as alert-box error.
    return.
end.
