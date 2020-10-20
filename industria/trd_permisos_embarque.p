TRIGGER PROCEDURE FOR DELETE OF permisos_embarque.
define var h1 as handle no-undo.

h1 = buffer permisos_embarque:handle.

run n:\audit\ptrDelete.p (h1).
