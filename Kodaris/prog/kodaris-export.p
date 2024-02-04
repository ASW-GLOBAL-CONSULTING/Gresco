/*h****************************************************************************
  PROCEDURE      : kodaris-export.p
  DESCRIPTION    : Export Data to Kodaris
  AUTHOR         : Robert Cloutier
  DATE WRITTEN   : 11/23/19
  CHANGES MADE   :
    05/12/21 rac; finish logic
    05/19/21 rac; send files to Kodaris via API/curl
    05/24/21 rac; add options, remove ini file requirement
    07/06/21 rac; send in batches of 500 records
    07/24/21 rac; send OEEL in batches of 500 independant of OEEH
    08/26/21 rac; add data cleansing
    09/09/21 rac; strip out double quote symbol too
    09/12/21 rac; scanString not returning true in all cases that need fixing
    09/12/21 rac; run fixString on array fields too
    03/14/22 rac; add recid for aret
    03/16/22 rac; add recid to column headers for aret
    10/14/22 rac; send header data before line data
******************************************************************************/
/* {p-rptbeg.i} */

/* RANGE AND OPTION VARIABLES */
def var p-datadir       as char                 no-undo.
def var p-tablename     as char                 no-undo.
def var p-lastsentfl    as log                  no-undo.
def var p-date          as date                 no-undo.

def var g-cono      like icsp.cono  init 1      no-undo.
def var i           as int                      no-undo.
def var v-datein    as char                 no-undo.
def var v-exportstr as char                 no-undo.

/* configuration variables */
def var v-ini       as char                 no-undo.
def var v-str       as char                 no-undo.
def var v-key       as char                 no-undo.
def var v-keyvalue  as char                 no-undo.

def var v-reccnt    as int                  no-undo.
def var v-linecnt   as int                  no-undo.

def var v-modtime   as char                 no-undo.
def var v-curl      as char                 no-undo.
def var v-curl1     as char                 no-undo.
def var v-curl2     as char                 no-undo.
def var v-curl3     as char                 no-undo.

def var v-lastrundt as date                 no-undo.
def var v-lastruntm as char                 no-undo.
def var v-lasttwldttm as char               no-undo.

/* 08/26/21 rac; add data cleansing */
def var v-newval        as char         no-undo.

def var v-params    as char                 no-undo.

def var v-tablelist as char                 no-undo.
v-tablelist =
    "ICSP,ICSW,ICSS,ICSEU,ICSEC,ICETS,ARSC,ARSS,ARET,APSV,APSS,SASTA,SASGM,ARSPT,PDSC,OE,COM,NOTES,ADDON,PO,PDSV".

define temp-table t-keyvalues   no-undo
    field keyname   as char
    field keyvalue  as char
    index k-key is primary unique
        keyname.
        
define temp-table t-icsp like icsp.

define stream datain.
define stream dataout.
define stream dataout2.
define stream s-log.

disable triggers for load of notes.

propath = "F:\NxT\rd\library\appcode.pl," + propath.

assign
    p-datadir        = "F:\Kodaris\data\"
    p-lastsentfl    = yes
    v-datein        = ""
    p-date          = ?.
    
v-params = session:parameter.

if v-params = "" then return.

do i = 1 to num-entries(v-params):
    if i = 1 then
        p-tablename  = entry(1, v-params).
    if i = 2 then
        p-lastsentfl = if entry(2, v-params) = "no" then no else yes.
    if i = 3 then
        v-datein     = entry(3, v-params).
end.

p-date = date(v-datein) no-error.
if p-lastsentfl = no and p-date = ? then return.

function scanString returns logical (input str as character):
   
   def var n as integer.
   def var l as logical.
   def var i as integer.
   def var c as character.

   n = length(str).
   
   do i = 1 to n:
      c = substring(str, i, 1 ).
      if asc(c) = 9 or asc(c) = 10 or asc(c) = 13 then
         l = true.
   end.

   return l.

end function.

procedure fixString:

    def input parameter v-value     as char     no-undo.

    def var i               as int          no-undo.
    def var v-len           as int          no-undo.
    def var v-chr           as char         no-undo.

    v-len = length(v-value).
    v-newval = "".
    do i = 1 to v-len:
        v-chr = substring(v-value, i, 1).
        if asc(v-chr) < 32 then do:
            /* nothing, don't bring to v-newval */
        end.
        else if asc(v-chr) >= 127 then do:
            case asc(v-chr):
                when 190 then
                    v-newval = v-newval + "3/4".
                when 189 then
                    v-newval = v-newval + "1/2".
                when 176 then
                    v-newval = v-newval + "deg".
                when 191 then
                    v-newval = v-newval + " ".
                when 232 then
                    v-newval = v-newval + "e".
                when 233 then
                    v-newval = v-newval + "e".
                when 150 then
                    v-newval = v-newval + "-".
                when 151 then
                    v-newval = v-newval + "-".
                when 145 then
                    v-newval = v-newval + "'".
                when 146 then
                    v-newval = v-newval + "'".
                when 147 then
                    v-newval = v-newval + chr(34).
                when 148 then
                    v-newval = v-newval + chr(34).
                when 248 then
                    v-newval = v-newval + "o".
                when 216 then
                    v-newval = v-newval + "o".
            end case.
        end.
        /* 09/09/21 rac; strip out double quote symbol too */
        else if asc(v-chr) = 34 then do:
            /* nothing, don't bring to v-newval */
        end.
        else
            v-newval = v-newval + v-chr.
    end.

end procedure.

procedure write-log:
    
    define input parameter v-msg    as char         no-undo.

    output stream s-log to value(p-datadir + "kodaris.log") append.
    put stream s-log unformatted
        string(today) + " " + string(time, "hh:mm") + " " + v-msg skip.
    output stream s-log close.

end procedure.

procedure dynExport:

    def input parameter hRecord as handle.

    def var hFld    as handle       no-undo.
    def var cName   as char         no-undo.
    def var cValue  as char         no-undo.
    def var cTmp    as char         no-undo.
    def var i       as int          no-undo.
    def var iExtnt  as int          no-undo.
    
    do i = 1 to hRecord:num-fields:
        hFld = hRecord:buffer-field(i).
                     
        if hFld:extent = 0 then do:
            cName = hFld:name.
            cValue = if hFld:buffer-value = ? then "" else
                     string(hFld:buffer-value).

            if hFld:data-type = 'character' then do:
                /* 09/12/21 rac; scanString not returning true in all cases that need fixing */
                /* if scanString(cValue) = yes then do: */
                    run fixString (cValue).
                    cValue = v-newval.
                /* end. */
            end.

        end.
        else do:
            cName = hFld:name.
            do iExtnt = 1 to hFld:extent:
                cTmp = if hFld:buffer-value(iExtnt) = ? then "" else
                       string(hFld:buffer-value(iExtnt)).
                
                /* 09/12/21 rac; run fixString on array fields too */
                if hFld:data-type = 'character' then do:
                    run fixString (cTmp).
                    cTmp = v-newval.
                end.
                
                cValue = if iExtnt = 1 then cTmp else
                         cValue + "|" + cTmp.
            end.
        end.
        
        put stream dataout unformatted cValue chr(9).
         
    end.

end procedure. /* dynExport */

procedure dynExport2:

    def input parameter hRecord as handle.

    def var hFld    as handle       no-undo.
    def var cName   as char         no-undo.
    def var cValue  as char         no-undo.
    def var cTmp    as char         no-undo.
    def var i       as int          no-undo.
    def var iExtnt  as int          no-undo.
    
    do i = 1 to hRecord:num-fields:
        hFld = hRecord:buffer-field(i).
                     
        if hFld:extent = 0 then do:
            cName = hFld:name.
            cValue = if hFld:buffer-value = ? then "" else
                     string(hFld:buffer-value).

            if hFld:data-type = 'character' then do:
                /* 09/12/21 rac; scanString not returning true in all cases that need fixing */
                /* if scanString(cValue) = yes then do: */
                    run fixString (cValue).
                    cValue = v-newval.
                /* end. */
            end.

        end.
        else do:
            cName = hFld:name.
            do iExtnt = 1 to hFld:extent:
                cTmp = if hFld:buffer-value(iExtnt) = ? then "" else
                       string(hFld:buffer-value(iExtnt)).
                
                /* 09/12/21 rac; run fixString on array fields too */
                if hFld:data-type = 'character' then do:
                    run fixString (cTmp).
                    cTmp = v-newval.
                end.
                       
                cValue = if iExtnt = 1 then cTmp else
                         cValue + "|" + cTmp.
            end.
        end.
        
        put stream dataout2 unformatted cValue chr(9).
         
    end.

end procedure. /* dynExport */

procedure exp-header-row:

    define input parameter v-table  as char         no-undo.
    def var v-field                 as char         no-undo.
    
    for each _File where _File._File-Name = v-table:
        for each _Field where _Field._File-Recid = recid(_File) by _order:
            v-field = _Field._Field-Name.
            
            put stream dataout unformatted
                v-field     chr(9).
        end.

        /* 03/16/22 rac; add recid to column headers for aret */
        if v-table = "aret" then
            put stream dataout unformatted
                "recid"     chr(9).
        
        put stream dataout unformatted
            skip.
    end.
    
end procedure. /* exp-header-row */

procedure exp-header-row2:

    define input parameter v-table  as char         no-undo.
    def var v-field                 as char         no-undo.
    
    for each _File where _File._File-Name = v-table:
        for each _Field where _Field._File-Recid = recid(_File) by _order:
            v-field = _Field._Field-Name.
            
            put stream dataout2 unformatted
                v-field     chr(9).
        end.

        put stream dataout2 unformatted
            skip.
    end.
    
end procedure. /* exp-header-row */

procedure exp-icsp:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each icsp where 
        icsp.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icsp.transdt     > v-lastrundt)   or
          (icsp.transdt     = v-lastrundt  and
           icsp.transtm     ge v-lastruntm)) else
         icsp.transdt       ge p-date)
        no-lock:
        
        buffer-copy icsp to t-icsp.
        find first t-icsp exclusive-lock no-error.
        
        run dynExport (input buffer t-icsp:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
        
        empty temp-table t-icsp.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-icsw:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each icsw where 
        icsw.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icsw.transdt     > v-lastrundt)   or
          (icsw.transdt     = v-lastrundt  and
           icsw.transtm     ge v-lastruntm)) else
         icsw.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer icsw:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-icss:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each icss where 
        icss.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icss.transdt     > v-lastrundt)   or
          (icss.transdt     = v-lastrundt  and
           icss.transtm     ge v-lastruntm)) else
         icss.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer icss:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-icets:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).
    
    v-reccnt = 0.
    for each icets where
        icets.cono          = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icets.transdt    > v-lastrundt)   or
          (icets.transdt    = v-lastrundt  and
           icets.transtm    ge v-lastruntm)) else
         icets.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer icets:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-icseu:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).
    
    v-reccnt = 0.
    for each icseu where
        icseu.cono          = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icseu.transdt    > v-lastrundt)   or
          (icseu.transdt    = v-lastrundt  and
           icseu.transtm    ge v-lastruntm)) else
         icseu.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer icseu:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-icsec:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).
    
    v-reccnt = 0.
    for each icsec where
        icsec.cono          = g-cono            and
        (if p-lastsentfl    = yes           then
         ((icsec.transdt    > v-lastrundt)   or
          (icsec.transdt    = v-lastrundt  and
           icsec.transtm    ge v-lastruntm)) else
         icsec.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer icsec:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-arsc:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each arsc where 
        arsc.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((arsc.transdt     > v-lastrundt)   or
          (arsc.transdt     = v-lastrundt  and
           arsc.transtm     ge v-lastruntm)) else
         arsc.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer arsc:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-arss:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each arss where 
        arss.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((arss.transdt     > v-lastrundt)   or
          (arss.transdt     = v-lastrundt  and
           arss.transtm     ge v-lastruntm)) else
         arss.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer arss:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-aret:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each aret where 
        aret.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((aret.transdt     > v-lastrundt)   or
          (aret.transdt     = v-lastrundt  and
           aret.transtm     ge v-lastruntm)) else
         aret.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer aret:handle).

        /* 03/14/22 rac; add recid for aret */
        put stream dataout unformatted recid(aret).

        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-apsv:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each apsv where 
        apsv.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((apsv.transdt     > v-lastrundt)   or
          (apsv.transdt     = v-lastrundt  and
           apsv.transtm     ge v-lastruntm)) else
         apsv.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer apsv:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-apss:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each apss where 
        apss.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((apss.transdt     > v-lastrundt)   or
          (apss.transdt     = v-lastrundt  and
           apss.transtm     ge v-lastruntm)) else
         apss.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer apss:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-sasta:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).
    
    v-reccnt = 0.
    for each sasta where
        sasta.cono          = g-cono            and
        sasta.codeiden      ne "BR"             and
        (if p-lastsentfl    = yes           then
         ((sasta.transdt    > v-lastrundt)   or
          (sasta.transdt    = v-lastrundt  and
           sasta.transtm    ge v-lastruntm)) else
         sasta.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer sasta:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-sasgm:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each sasgm where
        sasgm.cono          = g-cono            and
        (if p-lastsentfl    = yes           then
         ((sasgm.transdt    > v-lastrundt)   or
          (sasgm.transdt    = v-lastrundt  and
           sasgm.transtm    ge v-lastruntm)) else
         sasgm.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer sasgm:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-arspt:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each arspt where
        arspt.cono      = g-cono        and
        (if p-lastsentfl = yes then
         (arspt.transdt ge v-lastrundt and
          arspt.transtm ge v-lastruntm) else
         arspt.transdt  ge p-date)
        no-lock:
        
        run dynExport (input buffer arspt:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-pdsc:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each pdsc where
        pdsc.cono           = g-cono            and
        pdsc.statustype     = yes               and
        (if p-lastsentfl    = yes           then
         ((pdsc.transdt     > v-lastrundt)   or
          (pdsc.transdt     = v-lastrundt  and
           pdsc.transtm     ge v-lastruntm)) else
         pdsc.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer pdsc:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-oe:
    output stream dataout to value(p-datadir + "oeeh.txt").
    output stream dataout2 to value(p-datadir + "oeel.txt").

    run exp-header-row (input "oeeh").
    run exp-header-row2 (input "oeel").

    v-reccnt = 0.
    v-linecnt = 0.
    for each oeeh where
        oeeh.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((oeeh.transdt     > v-lastrundt)   or
          (oeeh.transdt     = v-lastrundt  and
           oeeh.transtm     ge v-lastruntm)) else
         oeeh.transdt       ge p-date)
        no-lock:

        run dynExport (input buffer oeeh:handle).
        put stream dataout skip.

        v-reccnt = v-reccnt + 1.

        for each oeel where
            oeel.cono       = g-cono        and
            oeel.orderno    = oeeh.orderno  and
            oeel.ordersuf   = oeeh.ordersuf
            no-lock:

            run dynExport2 (input buffer oeel:handle).
            put stream dataout2 skip.

            v-linecnt = v-linecnt + 1.

            /* 10/14/22 rac; send header data before line data */
            /*
            if v-linecnt = 500 then do:
                output stream dataout2 close.
    
                run send-file(input "oeel").
                pause 1.
    
                output stream dataout2 to value(p-datadir + "oeel.txt").
                run exp-header-row2 (input "oeel").
    
                v-linecnt = 0.
            end.
            */
        end.

        /* 10/14/22 rac; send header data before line data */
        if v-reccnt = 500 or v-linecnt ge 500 then do:
            output stream dataout close.

            run send-file(input "oeeh").
            pause 1.

            output stream dataout to value(p-datadir + "oeeh.txt").
            run exp-header-row (input "oeeh").
            
            v-reccnt = 0.
        end.
            
        /* 10/14/22 rac; send header data before line data */
        if v-linecnt ge 500 then do:
            output stream dataout2 close.

            run send-file(input "oeel").
            pause 1.

            output stream dataout2 to value(p-datadir + "oeel.txt").
            run exp-header-row2 (input "oeel").

            v-linecnt = 0.
        end.
    end.

    output stream dataout close.
    output stream dataout2 close.

    if v-linecnt > 0 then do:
        run send-file(input "oeel").
        pause 1.
    end.

    if v-reccnt > 0 then do:
        run send-file(input "oeeh").
        pause 1.
    end.
end procedure.

procedure exp-com:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each com where
        com.cono            = g-cono            and
        com.comtype         = "OE"              and
        (if p-lastsentfl    = yes           then
         ((com.transdt      > v-lastrundt)   or
          (com.transdt      = v-lastrundt  and
           com.transtm      ge v-lastruntm)) else
         com.transdt        ge p-date)
        no-lock:
        
        run dynExport (input buffer com:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-notes:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each notes where
        notes.cono          = g-cono            and
        notes.notesty       = "O"               and
        (if p-lastsentfl    = yes           then
         ((notes.transdt    > v-lastrundt)   or
          (notes.transdt    = v-lastrundt  and
           notes.transtm    ge v-lastruntm)) else
         notes.transdt      ge p-date)
        no-lock:
        
        run dynExport (input buffer notes:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-addon:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each addon where
        addon.cono          = g-cono            and
        (if p-lastsentfl    = yes           then
         ((addon.transdt    > v-lastrundt)   or
          (addon.transdt    = v-lastrundt  and
           addon.transtm    ge v-lastruntm)) else
         addon.transdt      ge p-date)          and
        addon.addonnet      ne 0
        no-lock:
        
        run dynExport (input buffer addon:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-po:
    output stream dataout to value(p-datadir + "poeh.txt").
    output stream dataout2 to value(p-datadir + "poel.txt").

    run exp-header-row (input "poeh").
    run exp-header-row2 (input "poel").

    v-reccnt = 0.
    for each poeh where
        poeh.cono           = g-cono            and
        (if p-lastsentfl    = yes           then
         ((poeh.transdt     > v-lastrundt)   or
          (poeh.transdt     = v-lastrundt  and
           poeh.transtm     ge v-lastruntm)) else
         poeh.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer poeh:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.

        for each poel where
            poel.cono       = g-cono        and
            poel.pono       = poeh.pono     and
            poel.posuf      = poeh.posuf
            no-lock:
            
            run dynExport2 (input buffer poel:handle).
            put stream dataout2 skip.
        end.

        if v-reccnt = 500 then do:
            output stream dataout close.
            output stream dataout2 close.

            run send-file(input "poeh").
            pause 1.
            run send-file(input "poel").
            pause 1.

            output stream dataout to value(p-datadir + "poeh.txt").
            output stream dataout2 to value(p-datadir + "poel.txt").

            run exp-header-row (input "poeh").
            run exp-header-row2 (input "poel").

            v-reccnt = 0.
        end.
    end.

    output stream dataout close.
    output stream dataout2 close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure exp-pdsv:
    output stream dataout to value(p-datadir + p-tablename + ".txt").
    run exp-header-row (input p-tablename).

    v-reccnt = 0.
    for each pdsv where
        pdsv.cono           = g-cono            and
        pdsv.statustype     = yes               and
        (if p-lastsentfl    = yes           then
         ((pdsv.transdt     > v-lastrundt)   or
          (pdsv.transdt     = v-lastrundt  and
           pdsv.transtm     ge v-lastruntm)) else
         pdsv.transdt       ge p-date)
        no-lock:
        
        run dynExport (input buffer pdsv:handle).
        put stream dataout skip.
        
        v-reccnt = v-reccnt + 1.
        if v-reccnt = 500 then do:
            output stream dataout close.

            run send-file(input p-tablename).

            output stream dataout to value(p-datadir + p-tablename + ".txt").
            run exp-header-row (input p-tablename).
            
            v-reccnt = 0.
        end.
    end.

    output stream dataout close.

    if v-reccnt > 0 then do:
        run send-file(input p-tablename).
        pause 1.
    end.
end procedure.

procedure send-file:
    define input parameter v-file   as char     no-undo.

    /*
    v-curl = "cd " + p-datadir + chr(10) +
        v-curl1 + v-file + ".txt" + v-curl2 + v-file + v-curl3.
    */
    
    /* unix silent value(v-curl). */
    /* dos value(v-curl). */
    
    /* run write-log("Curl command; " + v-curl). */
    
    os-command value("f:/kodaris/scripts/send-scripts/send-" + v-file + ".bat").
    pause 1.
end.

procedure log-run-time:
    if index(v-tablelist, p-tablename) > 0 then do:
        find notes where
            notes.cono          = g-cono            and
            notes.notestype     = "zz"              and
            notes.primarykey    = "asw-connector"   and
            notes.secondarykey  = p-tablename
            exclusive-lock no-error.
    
        if not avail notes then do:
            create notes.
            assign
                notes.cono          = g-cono
                notes.notestype     = "zz"
                notes.primarykey    = "asw-connector"
                notes.secondarykey  = p-tablename.
        end.
        
        v-lastrundt = ?.
        v-lastruntm = "".
        v-lasttwldttm = "".
        v-lastrundt = date(notes.noteln[1]) no-error.
        v-lastruntm = substring(notes.noteln[2], 1, 2) + substring(notes.noteln[2], 4, 2).
        
        if v-lastrundt ne ? then
            v-lasttwldttm = string(year(v-lastrundt)) + "-" + string(month(v-lastrundt), '99') 
                + "-" + string(day(v-lastrundt), '99') + " " + v-lastruntm.
    
        notes.noteln[1] = string(today).
        notes.noteln[2] = string(time, 'hh:mm').
    end.
end.

/*** MAIN BLOCK ***/

run write-log("Starting export of table " + p-tablename).

run log-run-time.

case p-tablename:
    when "ICSP" then do:
        run exp-icsp.
    end.
    when "ICSW" then do:
        run exp-icsw.
    end.
    when "ICSS" then do:
        run exp-icss.
    end.
    when "ICETS" then do:
        run exp-icets.
    end.
    when "ICSEU" then do:
        run exp-icseu.
    end.
    when "ICSEC" then do:
        run exp-icsec.
    end.
    when "ARSC" then do:
        run exp-arsc.
    end.
    when "ARSS" then do:
        run exp-arss.
    end.
    when "ARET" then do:
        run exp-aret.
    end.
    when "APSV" then do:
        run exp-apsv.
    end.
    when "APSS" then do:
        run exp-apss.
    end.
    when "SASTA" then do:
        run exp-sasta.
    end.
    when "SASGM" then do:
        run exp-sasgm.
    end.
    when "ARSPT" then do:
        run exp-arspt.
    end.
    when "PDSC" then do:
        run exp-pdsc.
    end.
    when "OE" then do:
        run exp-oe.
    end.
    when "COM" then do:
        run exp-com.
    end.
    when "NOTES" then do:
        run exp-notes.
    end.
    when "ADDON" then do:
        run exp-addon.
    end.
    when "PO" then do:
        run exp-po.
    end.
    when "PDSV" then do:
        run exp-pdsv.
    end.
    otherwise
        return.
        
end case.

run write-log("Finished export of table " + p-tablename).
