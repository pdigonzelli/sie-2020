TRIGGER PROCEDURE FOR REPLICATION-DELETE OF arrastre_lote.
define buffer b_tambores for tambores_industria.

if available arrastre_lote Then
  do:
     for each b_tambores where b_tambores.id_empresa = arrastre_lote.id_empresa and
                               b_tambores.id_sucursal = arrastre_lote.id_sucursal and
                               b_tambores.id_tipotambor = arrastre_lote.id_tipotambor_arrastre and
                               b_tambores.nromov = arrastre_lote.nromov_arrastre.
        delete b_tambores.   
     end.
  end.
