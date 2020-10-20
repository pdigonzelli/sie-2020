/* -----------------------------------------------------------------------------
   A procedure that uses the include file generated by ptmdump.p
   inputs:      the table name to dump
   outputs:     The mysql fast load format data file
   called by:   ptmdump.p

   Copyright (c)2001 KM Newnham mark@newnhams.com
   Version 1.0 Original release 21st Dec 2001

   -----------------------------------------------------------------------------
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

/* -------------------------- STREAMS --------------------------------------- */
def shared stream MAINSTREAM.
/* --------------------Temporary variables ---------------------------------- */
def var strDate         as character format "x(20)" extent 100          NO-UNDO.
def var decRecordCount  as decimal initial 0                            NO-UNDO.
def var decLoop         as decimal initial 0                            NO-UNDO.

/* --------------------------- MAIN ----------------------------------------- */
/* ------------------------
   write a debug message 
   ------------------------ */
output to "ptmdump.log" append.
put unformatted "dumping  {1}" skip.
output close.
/* ----------------------------
   Count the records so
   that we know when to close
   the output
   ---------------------------- */
for each {1} NO-LOCK:
    decRecordCount = decRecordCount + 1.
end.
/* -------------------
   Now extract the data
   from the database
   --------------------- */
status default "dumping data for {1}".
for each {1} :
    
    /* -------------------------
       These include files where
       dynamically created by 
       ptmdump.p
       ------------------------- */
    {inc2.i}
    {inc1.i}.
    
    decLoop = decLoop + 1.
    if decLoop = decRecordCount  
        then put stream mainstream unformatted ");" skip.
        else put stream mainstream unformatted "),".
end.
/* -------------------------------- The end -------------------------------- */