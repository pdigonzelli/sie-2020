TRIGGER PROCEDURE FOR DELETE OF lotes_aceite.
define var h1 as handle no-undo.

h1 = buffer lotes_aceite:handle.

run n:\audit\ptrDelete.p (h1).
