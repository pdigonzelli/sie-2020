define input parameter x1 as integer.
define input parameter x2 as integer.
define input parameter x3 as integer.
define input parameter x4 as integer.
define input parameter pdesde as integer.
define input parameter phasta as integer.
define input parameter xtipo like tipos_movi.id_tipo_movimiento.


for each tambores_industria where
            tambores_industria.id_empresa    = x1 and
            tambores_industria.id_sucursal   = x2 and
            tambores_industria.id_tipotambor = x3 and
            tambores_industria.nromov        = x4 AND
            tambores_industria.id_tambor     >= pdesde AND
            tambores_industria.id_tambor     <= phasta.
    
    MESSAGE tambores_industria.id_tambor VIEW-AS ALERT-BOX.

END.
