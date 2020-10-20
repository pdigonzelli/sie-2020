TRIGGER PROCEDURE FOR DELETE OF general.items_mov_transp.
/*
    find movimientos_envases where 
          movimientos_envases.nromov  = items_mov_transp.nromov and
          movimientos_envases.item    = items_mov_transp.item
          exclusive-lock no-error.

    if available movimientos_envases then
        delete movimientos_envases.

    release movimientos_envases.
*/
