 /*RB-MEMO-FILE  = SESSION:TEMP-DIRECTORY + STRING(RANDOM(1,1000000))  +  '.TXT'. */

 /*
 output to c:\temp\resumen.txt.
   put 'NEWMEMO v_resumen:' skip '~{' skip.
   for each t-personal no-lock:
      find first conceptos_abacus of t-personal no-lock.
      put t-personal.id_concepto format "999" space(2) 
          conceptos_abacus.descripcion format "x(30)" space(2)
          t-personal.cantidad '~~n' skip.                           
   end.
   put '~}' skip.
 output close.
 */
