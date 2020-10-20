TRIGGER PROCEDURE FOR REPLICATION-WRITE OF subd_vtas_re.
define buffer b_auxsub for aux_subd_ventas_re.

if available subd_vtas_re Then
  do:
      FIND FIRST b_auxsub OF subd_vtas_re NO-ERROR.
      IF NOT AVAILABLE b_auxsub THEN
      DO:
          CREATE b_auxsub.
          BUFFER-COPY subd_vtas_re TO b_auxsub.
      END.
  end.
