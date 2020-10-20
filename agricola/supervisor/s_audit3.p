  find userdb._file use-index _file-name                                  
       where userdb._file._file-name = "{1}" no-lock no-error.            
  if available userdb._file then                                          
    do:                                                                   
      find userdb._field use-index _file/field                            
           where userdb._field._file-recid = recid(userdb._file) and      
                 userdb._field._field-name = "c_usuario" no-lock no-error.
      if available userdb._field then                                     
        run s_audit2.p "{1}".   /*--- COMPILA EN TIEMPO DE EJECUCION ---*/
    end.                                                                  
