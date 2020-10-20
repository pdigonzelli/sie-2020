TRIGGER PROCEDURE FOR REPLICATION-DELETE OF packing_list.
define buffer b_items for items_packing_list.

if available packing_list Then
  do:
     FOR EACH b_items OF packing_list :
         DELETE b_items.
     END.

     IF packing_list.id_tipo_pack_list = 2 THEN /* Fruta */
     DO:
     /* Proceso CONS */ 
     RUN ..\cons\conecta-cons-fruta.p.
     IF RETURN-VALUE <> "" THEN
        DO:
          MESSAGE RETURN-VALUE
          VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "A T E N C I O N !!".
           UNDO , RETURN.
        END.
      ELSE
        DO:
          RUN ..\cons\eliminaViajesPacking.p  (input ROWID(packing_list)).
          RUN ..\cons\desconecta-cons.p.
        END.
     END.
  end.