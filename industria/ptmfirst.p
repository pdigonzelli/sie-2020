/* -----------------------------------------------------------------------------
   A procedure that takes checks to see if there are any records in 
   the file we want to dump. if there are it touches a file in the
   /tmp directory, which ptmdump2.p looks for
   called by:   ptmdump2.p
   inputs:      the table to check
   outputs:     a temp file

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




/* -----------------------------------
   See if there is a record in the file
   we are about to dump
   ---------------------------------- */

find first {1} no-error.
if not available {1} then return.


output to "ptm_records".
put unformatted "{1}".
output close.
/* -------------------------------- THE END --------------------------------- */