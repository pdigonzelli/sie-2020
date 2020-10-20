/* -----------------------------------------------------------------------------
   A procedure that creates a custom include file to run by ptmspec2.p 
   to permit escaping of special characters
   called by:   ptmspec.p
   inputs:      the table to check
   outputs:     a debug log

   Copyright (c)2001 KM Newnham mark@newnhams.com
   Version 1.0 Original release 21st Dec 2001

   ---------------------------------------------------------------------
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
   -------------------------------------------------------------------------- */

/* -------------------------- Temporary variables  -------------------------- */
def var intTemp         as integer                                      NO-UNDO.


/* ------------------------------ MAIN -------------------------------------- */

output to "specinc1.i".

/* -----------------------------
   Cycle the schema, looking for
   fields to check
   ----------------------------- */
status default "{1}".
for each _file where _file-name="{1}", 
    each _field of _file where _data-type = "character":

    if _extent = 0 then do:
        /* -----------------
           NOT an array
           ----------------- */

        /* ---------------------------------------------------------------------
           This escaping may look strange, but the characters need to be
           written into a file and then reread by progress so everything has
           to be escaped twice.
           you can add your own replacements here as well.
           ------------------------------------------------------------------ */
        put unformatted  
        _field-name " = replace(" _field-name ",~"~~~\~",~"~~~\~~~\~")." skip.
        put unformatted
        _field-name " = replace(" _field-name ",~"~'~",~"~~~\~'~")." skip.  
    end.
    else do:
        /* -----------------
           An array
           ----------------- */
        do intTemp = 1 to _extent:
            put unformatted 
                _field-name "[" intTemp  "] = 
                replace(" _field-name "[" intTemp "],~"~\~",~"~\~\~")." skip.
            put unformatted
                _field-name "[" intTemp  "] =                                    
                replace(" _field-name "[" intTemp "],~"~'~",~"~\~'~")." skip.    
        end.            
    end.

end.
output close.
/* ---------------------------
   we have built the include,
   now run the program that 
   uses it
   --------------------------- */
run ptmspec3.p "{1}".

/* ----------------------------- THE END ------------------------------------ */
