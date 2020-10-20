TRIGGER PROCEDURE FOR DELETE OF lotes_jugo.
define var h1 as handle no-undo.

h1 = buffer lotes_jugo:handle.

run n:\audit\ptrDelete.p (h1).
