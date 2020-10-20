/* --------------------------------------------------------------------
   ptmschema.p - A routine to dump a data schema from a progress database
   into a format loadable by MySQL.
   
   inputs: a parameter containing either the word "ALL" or the
           name of a table.

   outputs: A file named either MYSQL_'table'.SQL for one table
                             or SCH_'dbname'.SQL for all tables

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

/* -------------------------- Parameters ------------------------------------ */
def input parameter ipType as char format "x(30)"                       NO-UNDO.
/* --------------------------   Streams  ------------------------------------ */
define new shared stream MAINSTREAM.
/* -------------------------- DEFINES --------------------------------------- */
&GLOBAL-DEFINE PSMU PUT STREAM MAINSTREAM UNFORMATTED 
/* ---------------------- Temporary variables ------------------------------- */
def var logIsArray 		        as logical			                    NO-UNDO.
def var charFileName            as char format "x(60)"  	            NO-UNDO.
def var charFieldName           as char format "x(60)"  	            NO-UNDO.
def var charIndexName           as char format "x(60)"  	            NO-UNDO.
def var charTextSize            as char 			                    NO-UNDO.
def var charTextField           as char format "x(60)"  	            NO-UNDO.
def var charFormat              as char format "x(60)"  	            NO-UNDO.
def var intIdx                  as int                    	            NO-UNDO.
def var intIdxCount             as int                       	        NO-UNDO.
def var intIdxFld               as int                        	        NO-UNDO.
def var intIdxFldCount          as int                        	        NO-UNDO.
def var intExtents              as int                        	        NO-UNDO.
def var intTemp                 as int                        	        NO-UNDO.
def var charTemp                as character format "x(80)"             NO-UNDO.
/* ---------------------------- MAIN ---------------------------------------- */
if ipType = "ALL" then do:

    charFileName="mysql_" + ldbname(1) + ".sql".

    run dashtous(input-output charFileName).

    output stream MAINSTREAM to value(charFileName).

    {&PSMU} "DROP DATABASE IF EXISTS " caps(ldbname(1))  ";" skip.
    {&PSMU} "CREATE DATABASE " caps(ldbname(1)) ";" skip.
    {&PSMU} "USE " caps(ldbname(1)) ";" skip.

end.
else do:

   charFileName="sch_" + ipType + ".sql".
   run dashtous(input-output charFileName).
   output stream MAINSTREAM to value(charFileName).
   
end.   

/* --------------------------------------
   Loop through schema and extract the
   data
   -------------------------------------- */

for each _file where ((_file-name < "_" and ipType = "ALL") 
                or (_file-name = ipType and _file-name <> "ALL")) 
                and _hidden = false NO-LOCK:

    charFileName=caps(_file-name).
    run dashtous (input-output charFileName).
    
    {&PSMU} "DROP TABLE IF EXISTS " charFileName ";" skip.
    {&PSMU} "CREATE TABLE " charFileName "(" skip.
    
    /* ---------------------------------------
       Add a field with the progress recid to 
       ensure a unique, non-null primary index
       --------------------------------------- */

    {&PSMU} "~tPROGRESS_RECID bigint NOT NULL," skip.

    /* ----------------------------------------
       Now loop through each field, and process
       based on type
       ----------------------------------------- */
    for each _field of _file NO-LOCK break by _field-name:

        /* ---------------------
               see if we need to process
           an array
           -------------------- */
        logIsArray = false.
        intExtents = 0.
        if _field._extent > 0 then do:
            logIsArray = true.
            intExtents = 1.
        end.
        repeat while intExtents <= _field._extent:
            charFieldName=_field-name.
            run dashtous (input-output charFieldName).
            if logIsArray = false then do:
                {&PSMU} "~t" charFieldName "~t".
                intExtents = 1.
            end.
            else do:
                /* ------------------------------------
                   convert array field x[1],x[2] to
                   x__1 x__2 etc
                   ------------------------------------ */
                {&PSMU} "~t" charFieldName "__" string(intExtents) "~t".
                intExtents = intExtents + 1.
            end.

             if _data-type="character" then do:
            
                /* -----------------------------------------------
                   Try and determine the format of a character
                   field. Any field bigger than 255 is converted
                   to a TEXT type, otherwise it is a VARCHAR
                   ---------------------------------------------- */

                charFormat = _format.
                intTemp = 0.
                intTemp = index(charFormat,"(").
                if intTemp > 0 then do:
                    /* --------------------------------------
                       we have probably found a format that 
                       looks like "X(40)" or similar
                       -------------------------------------- */
                
                    charFormat = substring(charFormat,intTemp + 1).
                    
                    intTemp = index(charFormat,")").
                    
                    if intTemp > 0 then do:
                        charFormat = substring(charFormat,1,intTemp - 1).
                        /* -----------------------------------------
                           We have removed the characters () so we 
                           should be left with a number. So let's 
                           try it and see
                           ----------------------------------------- */
                        intTemp = integer(charFormat) no-error.
                        
                        if error-status:error then intTemp = 0.
                        else do:

                            /* ----------------------------------
                               We will force a VARCHAR, make
                               the routine check the format again,
                               or leave intTemp to tell the next 
                               routine its a TEXT field
                               ---------------------------------- */
                            if intTemp <= 255 then intTemp = 0.
                            
                        end.
                    end.
                end.
            
                charTextSize = "".
                charTextField = "".
            
                if intTemp > 0 then do:
            
                    /* ------------------------------------
                       it's a TEXT FIELD
                       ----------------------------------- */
                    
                    charTextSize = "(" + string(intTemp) + ")" .
                    charFormat = "TEXT".
                    charTextField = charFieldName.
                end.
                else do:
                
                    /* -------------------------------------
                       lets redo the format analysis
                       charFormat = _format.
                       intTemp = index(charFormat,"(").
                    ---------------------------------------- */
                
                    {&PSMU} "varchar".
                    if intTemp > 0 then do:
                        if substring(charFormat,1,1) = "X" then do:
                            charFormat = substring(charFormat,intTemp).
                        end.
                        else do:
                            intTemp = length(charFormat) - 2.
                            charFormat = "(" + string(intTemp) + ")".
                        end.
                    end.
                    else do:
                        /* try and work out how long the string is */
                        charFormat = "(" + string(length(_format)) + ")".
                    end.
                end.
                {&PSMU} charFormat.
                /* ----------------------------------
                    Set a default character value if required
                   --------------------------------- */
                if _initial <> ? then 
                    charTemp = _initial.
                    /* ----------------------
                       Escape any " or \
                       ---------------------- */
                    charTemp = replace(charTemp,"~\","~\~\").
                    charTemp = replace(charTemp,"~"","~\~"").
                    {&PSMU} " DEFAULT ~"" trim(charTemp) "~"".
            end.
            /* ----------------------------------------------------------
               DECIMAL field types stay as DECIMAL
               --------------------------------------------------------- */
            else if _data-type="decimal" then do:
                {&PSMU} "decimal(" length(_format)  - 1 ","  _decimals  ")".
                /* ----------------------------------
                    Set a default decimal value if required
                   --------------------------------- */
                if _initial <> ? then {&PSMU} " DEFAULT " trim(_initial).
            end.
            /* ----------------------------------------------------------
               INTEGER fields - We try to make the most efficient use of
               MySQL datattypes
               --------------------------------------------------------- */
            else if _data-type="integer" then do:
                intTemp = length(_format).
                if intTemp < 3 then do:
                    {&PSMU} "tinyint".
                end.
                else if intTemp < 5 then do:
                    {&PSMU} "smallint".
                end.
                else if intTemp < 7 then do:
                    {&PSMU} "mediumint".
                end.
                else do:
                    {&PSMU} "integer".
                end.
                /* -------------------------------------
                    Add an integer default if required
                    ------------------------------------- */
                if _initial <> ? then {&PSMU} " DEFAULT " trim(_initial).
            end.
            /* ------------------------------------------
               LOGICAL data types become TINYINT because
               there is no MySQL boolean data type
               ------------------------------------------ */
            else if _data-type="logical" then do:
                {&PSMU} "tinyint".
                if _initial = "no" then {&PSMU} " DEFAULT 0".
                else                    {&PSMU} " DEFAULT 1".
     
            end.
            /* --------------------------------------------
               DATES stay as date, but you can't carry over 
               the classic 'default TODAY'
               -------------------------------------------- */
            else if _data-type="date" then do:
                {&PSMU} "date".
            end.
            /* --------------------------------------------
               RAW is converted, but the routines can't convert
               the data in the fields
               -------------------------------------------- */
            else if _data-type="raw" then do:
                {&PSMU} "blob".
            end.
            /* --------------------------------------------
               RECID (version 9 only)
               -------------------------------------------- */
            else if _data-type="recid" then do:
                {&PSMU} "bigint".
            end.
            /* -----------------------------------------------
               Any other type - this probably shouldnt happen
               ----------------------------------------------- */
            else do:
                {&PSMU} "## Warning - writing an unparsed data type" skip.
                {&PSMU} "## That MySQL might not know about" skip.
                {&PSMU} _data-type.
            end.
            /* ------------------------------
               A mandatory field?
               ------------------------------ */
            if _mandatory then {&PSMU} " NOT NULL".
            
            /* ---------------------------------- 
               Add a comma if not the last field,
               or still processing an array
               ---------------------------------- */
            if not last(_field-name) or intExtents <= _extent 
                then {&PSMU} "," skip.
        end.
    end.
    
    /* --------------------------------------------------------
       primary index is always the progress recid 
       this is because mySQL demands a non-null, unique
       primary index - progress databases don't. 
       -------------------------------------------------------- */
       
    {&PSMU} "," skip "~tprimary key~t CONV_PRIMARY (PROGRESS_RECID)".

    /* -------------------------------
       Count the number of indices so
       that we know when to add commas
       -------------------------------- */
    intIdxCount = 0.
    for each _index of _file:
        /* ----- Version 8 -------- */
        find first _index-field of _index no-error.
        if not available _index-field then next.

        /* ----- Version 9 -------- */
        find _field where recid(_field) = _index-field._field-recid no-error.
        if not available _field then next.
        if _field-name = "" then next.

        /* ----------------------------------------
           If there are no indices, we still get a 
           count of 1, so check for fields
           e.g. SPORTS.LOCAL-DEFAULTS
           ----------------------------------------- */
        intIdxCount = intIdxCount + 1.
    end.

    if intIdxCount > 0 then {&PSMU} ",".
    {&PSMU} skip.

    /* --------------------------------------------------
       Now loop through all the indices
       -------------------------------------------------- */
    intIdx = 0.
    if intIdxCount > 0 then
        for each _index of _file where can-find (first _index-field of _index)  
        break by  _index-name.
    
        /* ------------------------------------------
           Make a note of the original primary index
           ------------------------------------------ */
        if recid(_index) =  _file._prime-index  then
        {&PSMU} "## The PROGRESS database primary index" skip.
        charIndexName=caps(_index._index-name).
        run dashtous(input-output charIndexName).
        /* -----------------------------------------
           A standard index
           ----------------------------------------- */
        if _index._Wordidx = ? then do:
            {&PSMU} "~tkey~t" charIndexName " (" skip.
            /* ----------------------------------------
               Count the number of fields in the index
               ---------------------------------------- */
            intIdxFldCount = 0.
            for each _index-field of _index.
                intIdxFldCount = intIdxFldCount + 1.
            end.
            intIdxFld = 0.
            for each _index-field of _index.
                find _field where recid(_field) = _index-field._field-recid.
                charFieldName=caps(_field._field-name).
                run dashtous(input-output charFieldName).
                {&PSMU} "~t~t~t" charFieldName.
                if charTextSize <> "" and charTextField = charFieldName then 
                put stream MAINSTREAM unformatted charTextSize.
                intIdxFld = intIdxFld + 1.
                if intIdxFld < intIdxFldCount then {&PSMU} "," skip.
            end.
        
            put stream MAINSTREAM unformatted ")".
        end.
        /* ---------------------------------------------------
           A WORD index, that we will convert to FULLTEXT
           There is only one field in a word index
           --------------------------------------------------- */
        else do:
            {&PSMU} "# "  _index-name skip.
            find first _index-field of _index NO-LOCK.
            find _field where recid(_field) = _index-field._field-recid NO-LOCK.
            charFieldName=caps(_field._field-name).
            run dashtous(input-output charFieldName).
            
            {&PSMU} "~t FULLTEXT(" charFieldName  ")".
        end.
        intIdx = intIdx + 1.
        if intIdx < intIdxCount then {&PSMU} ",".
        {&PSMU} skip. 
        if last(_index-name) then {&PSMU} ")" skip.
    end.
    
    
    if intIdxCount = 0 then {&PSMU} ")" skip. 
    /* ----------------------------------------------
       Derive the comment from the table description
       ---------------------------------------------- */
    {&PSMU} "~tcomment = ~"" substring(_file._desc,1,60) "~";" skip.

end.

/* -----------------------------------------------
   Include the subroutine for converting and checking
   field names
   --------------------------------------------------- */
{dashtous.i}

/* ------------------------------- THE END ----------------------------- */
