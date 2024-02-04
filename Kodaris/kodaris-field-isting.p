procedure dynExport:
    def input parameter hRecord as handle       no-undo.
    
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

        display
            cName
            hFld:data-type.

    end.
    else do:
        cName = hFld:name.
    end.
end.

end.

find first apsv where
    apsv.cono = 5 
    no-lock no-error.

run dynExport (input buffer apsv:handle).
