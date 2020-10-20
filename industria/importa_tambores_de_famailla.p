define variable mensaje  as character format "x(50)".
define variable ind      as integer.
define variable respuesta as logical format "Si/No" initial no.
define variable c_tuc as logical initial no.
define variable c_fam as logical initial no.

message " Confirma Importacion de Remitos de Famailla (S/N)?" update respuesta auto-return.
if respuesta = yes then

  do:

    if not connected("industria") then
      do:
        connect -db industria -ld industria -N tcp -H tucuman1 -S industria no-error.
        c_tuc = yes.
      end.

    if not connected("famindust") then
      do:
        connect -db indust -ld famindust -N tcp -H famailla -S faindustsm no-error. 
        c_fam = yes.
      end.

    if connected("industria") and connected("famctacte") then
      do:
        form mensaje
             with frame liquid2 row 16 no-labels centered
             overlay color blink-yellow/blue.
        mensaje = " Importando... ".
        ind = (50 - length(trim(mensaje))) / 2.
        mensaje = fill(" " , ind) + trim(mensaje).
        display mensaje with frame liquid2.
            
        run remfam1.p.

        if c_tuc then disconnect general.
        if c_fam then disconnect famgeneral.
      end.
  end.
