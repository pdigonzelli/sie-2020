/* -----------------------------------------------------------------------------
   A procedure that initiates the data dump for all tables, or a
   particular table. This is mostly achieved by dynamically generataing some 
   include files, which are then executed by ptmdump2.p
   called by:   The procedure editor or your program
   inputs:      the table to dump or the word "ALL"
   outputs:     a test file with the required data

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

/* -------------------------------------------------------------------------- */
def input parameter ipFile 	as char format "x(30)"		                NO-UNDO.
/* --------------------------- STREAMS -------------------------------------- */
define			stream INC1.
define			stream INC2.
define new shared 	stream MAINSTREAM.
/* -------------------------------------------------------------------------- */
&GLOBAL-DEFINE PSMU  PUT STREAM MAINSTREAM UNFORMATTED 
&GLOBAL-DEFINE PSI1U PUT STREAM INC1 UNFORMATTED 
&GLOBAL-DEFINE PSI2U PUT STREAM INC2 UNFORMATTED 
/* -------------------------------------------------------------------------- */
def var intCurrentExtent	as integer			                        NO-UNDO.
def var logIsArray 		    as logical			                        NO-UNDO.
def var intExtents		    as integer			                        NO-UNDO.
def var charFieldName 		as char				                        NO-UNDO.
def var charLastFieldName 	as char format "x(50)"		                NO-UNDO.
def var charFileName 		as character format "x(50)" 	            NO-UNDO.
def var intFieldCount 		as integer 			                        NO-UNDO.
def var logFirstField 		as logical initial yes 		                NO-UNDO.


/* ---------------------------------------------------------------------------*/
if ipFile = "ALL" then charFileName = ldbname(1)  + "_data.sql".
                  else charFileName = ipFile + "_data.sql".

run dashtous(input-output charFileName).

output stream MAINSTREAM to value(charFileName).

charFileName = LDBNAME(1).
run dashtous(input-output charFileName).
{&PSMU} "USE " charFileName ";" skip.  

for each _file where (( _file-name  < "_" and ipFile  = "ALL" ) 
                  or  ( _file-name = ipFile    and ipFile <> "ALL")) 
                 AND _hidden = FALSE no-lock:

    /* -------------------------------------------
       This stops the dump generating invalid SQL
       for empty files
       -------------------------------------------- */ 
    os-delete "ptm_records".
    run ptmfirst.p _file-name.
    if search("ptm_records") = ? then next.
    /* -------------------------------------------- */
    logFirstField = yes.
    intFieldCount = 0.
    for each _field of _file.
        intFieldCount = intFieldCount + 1.
    end.

    charFileName = _file-name.
    run dashtous (input-output charFileName).

    {&PSMU} skip "LOCK TABLE " trim(charFileName) " WRITE;" skip.
    {&PSMU} skip "INSERT INTO " trim(charFileName) " VALUES ".


   
    intCurrentExtent = 0.
    for each _field of _file.
         charLastFieldName = _field-name.
    end.
    
    output stream INC1 to "inc1.i".
    output stream INC2 to "inc2.i".

    {&PSI1U} "PUT STREAM MAINSTREAM UNFORMATTED ~"(~" ".
    
    /* -------------------------------------------
       Add the recid to create a primary index
       ------------------------------------------- */
       
   {&PSI1U} " RECID(" _file-name ") ~",~"".  
    
    for each _field of _file.
    
        
        intExtents = 0.
        logIsArray = false.
        if _field._extent > 0 then do:
            intExtents = 1.
            logIsArray = true.
        end.
        
        repeat while intExtents <= _field._extent:
        
            if logFirstField = no then do:
                if _data-type = "character" or _data-type = "date" then do:
                    {&PSI1U} " ~",~'~" ".
                end.
                else do:
                    {&PSI1U} " ~",~" ".
                end.
            end.
            else do:
                if _data-type = "character" or _data-type = "date" then do:
                    {&PSI1U} " ~"~'~" ".
                end.
                else do:
                    {&PSI1U} " ".
                end.
            end.
            if logIsArray = false then do:
                intExtents = 1.
                charFieldName = _field-name.
            end.
            else do:
                charFieldName = _field-name + "[" + string(intExtents) + "]".
                intExtents = intExtents + 1.
            end.
        
	    /* -----------------------------------------------
               Extract the date so that is can be built in ISO 
	       format. This should allow European format dates
	       to be processed correctly as well as US
               ----------------------------------------------- */
            if _data-type = "date" then do:
                intCurrentExtent = intCurrentExtent + 1.
                {&PSI2U} " strDate[" + string(intCurrentExtent) + "] 
			 = string(year(" + charFieldName + "),~"9999~") + ~"-~" 
			 + string(month(" + charFieldName + "),~"99~") 
			 + ~"-~" + string(day(" + charFieldName + "),~"99~").".

                {&PSI1U} "strDate[" + string(intCurrentExtent) + "]".
            end.
            else if _data-type = "character" then do:
                {&PSI1U} "trim(" charFieldName ")".
            end.
	    /* ------------------------------------------
               Convert logicals to integers because we
	       are going to put them in a tinyint field
	       ------------------------------------------ */
            else if _data-type = "logical" then do:
                    {&PSI1U} "integer(" charFieldName ")".
            end.
         /* ------------------------------------------
               Currently Convert version 9 RAW DATA to
               nothing
	       ------------------------------------------ */
            else if _data-type = "raw" then do:
                    {&PSI1U}  "NULL" .
            end.

            else do:
		/* ---------------------------------------
                   Convert an unknown value to zero,
		   otherwise, just write the value
           this is for decimals, integers and recids
		   -------------------------------------- */
                {&PSI1U} 
                "if " charFieldName "=? then 0 else " charFieldName.
            end.
            
            run WriteStream (_data-type).
            /*
            if _field-name <> charLastFieldName then do:
               run WriteStream (_data-type).
            end.
            else do:
                if logIsArray = false then
                    run WriteStream(_data-type).
                else do:
                    if intExtents > _field._extent then
                        
                        {&PSI1U} " ~"'~" ".
                    else
                        {&PSI1U} "~"',~" ". 
                end.
            end.
            */
            logFirstField = no.
        end.
        logFirstField = no.
    end.
    /*
    put stream inc1 " ~"),~".".
    */
    output stream INC1 close.
    output stream INC2 close.

    /* ---------------------------------------------------------
       Now we have built the includes, lets run the program to
       extract the data
       --------------------------------------------------------- */
    
    run ptmdump2.p _file-name.         

    /* -----------------------------
       put in a note for debugging
       ----------------------------- */

    {&PSMU} skip "## END OF " _file-name skip.

end.
output stream MAINSTREAM close.
/* ------------------------------------
   routine to parse db/file/field names
   ------------------------------------ */
{dashtous.i}

/* -------------------------------------
   routine to write confusing streamed
   output
   ------------------------------------- */
PROCEDURE  WriteStream:
    define input parameter data-type as character format "x(10)".
    
    if data-type = "character" or data-type = "date" then do:
    
        {&PSI1U} " ~"'~" ".
    
    end.
    else do:
    
        {&PSI1U} " ~" ~" ".  
    
    end.

end procedure.
