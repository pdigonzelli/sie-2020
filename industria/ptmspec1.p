/* -----------------------------------------------------------------------------
   A procedure that takes searches all character fields in the database to 
   permit escaping of special characters
   called by:   The procedure editor or your program
   inputs:      the table to check or "ALL"
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

/* -----------------------------------------------------
   cycle through every character field in the database.
   Add backslashes to the characters that cause a load
   failure on Mysql. However, you
   only really need to do it once, before conversion
   ---------------------------------------------------- */

define input parameter ipFile as character format "x(20)"               NO-UNDO.


if ipFile = "ALL" then do:
    for each _file where _file-name < "_" and _hidden = false no-lock:
        output to "ptmspec.log" append.
        put unformatted "checking " _file-name skip.
        output close.

        run ptmspec2.p _file-name.
    end.
end.

else do:
    output to "ptmspec.log" append.
    put unformatted "checking " ipFile skip.
    output close.
    run ptmspec2.p ipFile.
end.

