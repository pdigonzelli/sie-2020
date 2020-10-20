TRIGGER PROCEDURE FOR REPLICATION-WRITE OF r_seguros_oe.
define buffer b_pol for polizas_seguro.

if available r_seguros_oe Then
  do:
      IF r_seguros_oe.id_ciaseg <> 0 THEN
      DO:
          FIND FIRST b_pol OF r_seguros_oe NO-ERROR.
          IF NOT AVAILABLE b_pol THEN
          DO:
              CREATE b_pol.
              BUFFER-COPY r_seguros_oe TO b_pol.
          END.
      END.
  end.
