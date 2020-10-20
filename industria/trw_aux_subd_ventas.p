TRIGGER PROCEDURE FOR REPLICATION-WRITE OF aux_subd_ventas.
define buffer b_pol for polizas_seguro.

if available aux_subd_ventas Then
  do:
      IF aux_subd_ventas.id_ciaseg <> 0 THEN
      DO:
          FIND FIRST b_pol OF aux_subd_ventas NO-ERROR.
          IF NOT AVAILABLE b_pol THEN
          DO:
              CREATE b_pol.
              BUFFER-COPY aux_subd_ventas TO b_pol.
          END.
      END.


  end.
