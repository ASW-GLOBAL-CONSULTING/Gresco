/*******************************************************************************
  PROCEDURE   : po-duedt-wt-oe.p
  DESCRIPTION : Update WT/OE line dates from PO
  AUTHOR      : Robert Cloutier
  DATE CREATED: 03/23/22
  CHANGES MADE:
    04/24/22 rac; store po on oeel too
    05/31/22 rac; update oeel.promisedt too, unless JIT (unless new date is greater)
    07/21/22 rac; don't mark so will update again
    07/21/22 rac; eliminate requirement of EDI acknowledge
    07/21/22 rac; add days from Ship Point SASTZ table
    08/25/22 rac; add create-oefill-k for kits
    09/13/22 rac; add break by and don't clear until last-of
    12/21/22 rac; PO must be in same warehouse
    04/27/23 rac; uplift to CSD SARSU
    08/30/23 rac; option to run for one PO
*******************************************************************************/

def shared var g-cono       like sasc.cono                  no-undo.
def shared var g-today      as date format "99/99/99"       no-undo.
def shared var g-operinit   like sasoo.operinit             no-undo.
def shared var g-operinits  like sasoo.operinit             no-undo.
def        var i            as int                          no-undo.
def shared var v-sapbid     as recid                        no-undo.
def shared var t-transcnt   as int                          no-undo.

def input param cOption1        as char     no-undo.
def input param cOption2        as char     no-undo.
def input param cOption3        as char     no-undo.
def input param cOption4        as char     no-undo.
def input param cOption5        as char     no-undo.
def input param cOption6        as char     no-undo.
def input param cOption7        as char     no-undo.
def input param cOption8        as char     no-undo.
def input param cOption9        as char     no-undo.
def input param cOption10       as char     no-undo.
def input param cOption11       as char     no-undo.
def input param cOption12       as char     no-undo.
def input param cOption13       as char     no-undo.
def input param cOption14       as char     no-undo.
def input param cOption15       as char     no-undo.
def input param cOption16       as char     no-undo.
def input param cOption17       as char     no-undo.
def input param cOption18       as char     no-undo.
def input param cOption19       as char     no-undo.

def var v-invalid   as c format "x(13)" init "** Invalid **" no-undo.
def var v-reportnm      like sapb.reportnm              no-undo.

/*o Define Range And Option Variables */
def var p-whse          like poeh.whse                  no-undo.
def var p-pono          like poeh.pono                  no-undo.
def var p-posuf         like poeh.posuf                 no-undo.
def var p-updatefl      as log                          no-undo.

def var v-qtyavail      like poel.stkqtyord             no-undo.
def var v-seqno         like poele.seqno                no-undo.

def var v-ordertype     like poelo.ordertype            no-undo.
def var v-holdsuf       like poelo.orderaltsuf          no-undo.
def var v-seqaltno      like poelo.seqaltno             no-undo.

def var v-processty     like oefill.processty           no-undo.

def var v-stkqtyord     like poel.stkqtyord             no-undo.
def var v-duedt         like poel.duedt                 no-undo.
def var v-qtyneeded     like oeel.stkqtyship            no-undo.
def var v-ponum         as char                         no-undo.

def var v-descrip       as char                         no-undo.
def var v-custname      like arsc.name                  no-undo.
def var v-whsename      like icsd.name                  no-undo.

/* 07/21/22 rac; add days from Ship Point SASTZ table */
def var v-adddays       as int                          no-undo.

/* 12/21/22 rac; don't use PO if tied to a different order */
def var v-fill          as char                         no-undo.

form header
    'PO#'               at 1
    'Ln#'               at 12
    'Product'           at 16
    'Description'       at 41
    '   Qty Ord'        at 92
    'Unit'              at 104
    'Stk Qty Ord'       at 109
    'Due Date'          at 121
    'Order #'           at 11
    'Ln#'               at 23
    'Customer #'        at 28
    'Name'              at 42
    'Stk Qty Ord'       at 73
with frame f-hdr no-box no-labels page-top width 132.

form
    poel.pono           at 1
    "-"                 at 8
    poel.posuf          at 9
    poel.lineno         at 12
    poel.shipprod       at 16
    v-descrip           at 41   format 'x(49)'
    poel.qtyord         at 92
    poel.unit           at 104
    poel.stkqtyord      at 109
    poel.duedt          at 121
with frame f-det1 width 132 no-box no-labels down.

form
    oeel.orderno        at 11
    '-'                 at 19
    oeel.ordersuf       at 20   format '99'
    oeel.lineno         at 23
    oeel.custno         at 28
    v-custname          at 42
    oeel.stkqtyord      at 73
with frame f-det2 width 132 no-box no-labels down.

form
    wtel.wtno           at 12
    '-'                 at 19
    wtel.wtsuf          at 20   format '99'
    wtel.lineno         at 23
    wtel.shiptowhse     at 28
    v-whsename          at 42
    wtel.stkqtyord      at 73
with frame f-det3 width 132 no-box no-labels down.

form
    "------------------------------"      at 1
    "------------------------------"      at 31
    "------------------------------"      at 61
    "------------------------------"      at 91
    "------------"                        at 121
with frame f-under page-top no-box width 132.

view frame f-hdr.
view frame f-under.

assign
    /* 08/30/23 rac; option to run for one PO */
    /*
    p-whse      = cOption1
    */
    p-pono      = integer(cOption1)
    p-posuf     = integer(cOption2)
    p-updatefl  = if cOption3 = "yes" then yes else no.

find sapb where
    recid(sapb) = v-sapbid
    no-lock no-error.
if not avail sapb then return.

v-reportnm = sapb.reportnm.

run clear-oefill.

/* 08/30/23 rac; option to run for one PO */
/*
for each poeh use-index k-convert where
    poeh.cono       = g-cono        and
    poeh.transtype  = "PO"          and
    poeh.stagecd    ge 2            and
    poeh.stagecd    le 3             and
    (if p-whse  = "" then true else
     poeh.whse      = p-whse)
    no-lock,
*/    
for each poeh use-index k-convert where
    poeh.cono       = g-cono        and
    poeh.pono       = p-pono        and
    poeh.posuf      = p-posuf
    no-lock,
each poel where
    poel.cono       = g-cono        and
    poel.pono       = poeh.pono     and
    poel.posuf      = poeh.posuf    and
    poel.statustype ne "c"
    no-lock break by poel.whse by poel.shipprod by poel.duedt
                by poel.pono by poel.posuf:

    /* 09/13/22 rac; add break by and don't clear until last-of */
    if first-of(poel.shipprod) then do:
        find icsp where
            icsp.cono   = g-cono        and
            icsp.prod   = poel.shipprod 
            no-lock no-error.

        if avail icsp then
            v-descrip = icsp.descrip[1] + " " + icsp.descrip[2].
        else
            v-descrip = poel.proddesc + " " + poel.proddesc2.

        run fill-oe.
    end.

    display
        poel.pono
        poel.posuf
        poel.lineno
        poel.shipprod
        v-descrip
        poel.qtyord
        poel.unit
        poel.stkqtyord
        poel.duedt
    with frame f-det1.
    down with frame f-det1.

    /* 07/21/22 rac; add days from Ship Point SASTZ table */
    v-adddays = 0.
    if poel.user1 ne "" then do:
        find sastaz where
            sastaz.cono         = g-cono        and
            sastaz.codeiden =    "Ship Point"   and
            sastaz.primarykey   = poel.user1
            no-lock no-error.
            
        if avail sastaz then
            v-adddays = integer(sastaz.codeval[1]) no-error.
    end.
    else do:
        find sastaz where
            sastaz.cono         = g-cono        and
            sastaz.codeiden     = "Ship Point"  and
            sastaz.primarykey   = "Default"
            no-lock no-error.
            
        if avail sastaz then
            v-adddays = integer(sastaz.codeval[1]) no-error.
    end.

    v-stkqtyord = poel.stkqtyord.
    v-duedt = poel.duedt + v-adddays.
    
    /* 04/24/22 rac; store po on oeel too */
    v-ponum = string(poel.pono) + "-" + string(poel.posuf, '99').
    
    run allocate-oefill.

    /* 09/13/22 rac; add break by and don't clear until last-of */
    if last-of(poel.shipprod) then do:
        run clear-oefill.
    end.

    t-transcnt = t-transcnt + 1.

    /* 07/21/22 rac; don't mark so will update again */
    /* poele.user1 = "Y". */
end. /* for each poeh */

if t-transcnt = 0 then do:
    display 
        skip(1)
        "** No Records Were Found **" 
    with frame f-norec no-box.
end.
hide all no-pause.

procedure fill-oe:

    define buffer b-poeh for poeh.
    define buffer wteh for wteh.
    define buffer b-wteh for wteh.
    define buffer vaeh for vaeh.
    define buffer kpet for kpet.
    define buffer b-kpet for kpet.
        
    /* find ties */
    for each poelo use-index k-poelo where
        poelo.cono     = g-cono     and
        poelo.pono     = poel.pono  and
        poelo.posuf    = 0          and
        poelo.lineno   = poel.lineno
        no-lock:

        assign
            v-ordertype = poelo.ordertype
            v-holdsuf   = poelo.orderaltsuf
            v-seqaltno  = poelo.seqaltno.
            
        if v-ordertype = "o" then do:
            find oeeh where
                oeeh.cono       = g-cono            and
                oeeh.orderno    = poelo.orderaltno  and
                oeeh.ordersuf   = v-holdsuf
                no-lock no-error.
            if not avail oeeh then next.
            
            find oeel where
                oeel.cono       = g-cono            and
                oeel.orderno    = poelo.orderaltno  and
                oeel.ordersuf   = v-holdsuf         and
                oeel.lineno     = poelo.linealtno
                no-lock no-error.
            if not avail oeel then next.
            
            v-processty = "1".
            run create-oefill.
        end.
        
        if v-ordertype = "t" then do:
            find wteh where
                wteh.wtno   = poelo.orderaltno  and
                wteh.wtsuf  = v-holdsuf
                no-lock no-error.
            if not avail wteh then next.
            
            find wtel where
                wtel.wtno   = poelo.orderaltno  and
                wtel.wtsuf  = v-holdsuf         and
                wtel.lineno = poelo.linealtno
                no-lock no-error.
            if not avail wtel then next.
            
            v-processty = "1".
            run create-oefill-wt.
        end.
    end.
    
    /* regular orders */
    v-seqaltno = 0.
    
    main:
    for each oeel no-lock use-index k-fill where
         oeel.cono          = g-cono        and
         oeel.statustype    = "a":u         and
         oeel.whse          = poeh.whse     and
         oeel.invoicedt     = ?             and
         oeel.shipprod      = poel.shipprod and
         (oeel.specnstype = "" or
         (oeel.specnstype = "s":u and 
          can-find(icsw use-index k-icsw where
                   icsw.cono       = g-cono        and
                   icsw.prod       = oeel.shipprod and
                   icsw.whse       = oeel.whse     and 
                can-do("s,o":u,icsw.statustype))))
                                            and
         oeel.bono          = 0             and
         oeel.kitfl         = no            and
         oeel.tallyfl       = no            and
         oeel.returnfl      = no            and
         can-do("so,br":u,oeel.transtype)   and
         oeel.altwhse       = ""            and
         oeel.qtyship       < oeel.qtyord   and
         oeel.delayresrvfl  = no            and
         oeel.botype        <> "d":u,
    first oeeh use-index k-oeeh where
          oeeh.cono          = g-cono        and
          oeeh.orderno       = oeel.orderno  and
          oeeh.ordersuf      = oeel.ordersuf and
          (oeeh.openinit      = ""            or
           oeeh.openinit      = string(g-operinit,"xxxx") + "y":u or
           oeeh.openinit      = string(g-operinit,"xxxx") + "b":u) and
          ( (can-do("s,t":u,oeeh.orderdisp) and oeeh.stagecd = 2) or
             oeeh.stagecd = 1) and
            (can-do("s,t":u,oeeh.orderdisp) or
            (not can-do("s,t":u,oeeh.orderdisp) and 
             oeel.botype  <> "n":u))
    no-lock:
    
        /*d***** If po/wt/va not yet received, don't fill the order ****/
        if oeel.ordertype = "p":u then do for b-poeh:
            find last b-poeh where
                b-poeh.cono = g-cono and
                b-poeh.pono = oeel.orderaltno 
                no-lock no-error.

            if avail b-poeh and b-poeh.jrnlno = 0 then next main.
        end. /* if oeel.ordertype = "p" */
    
        /*tb 14233 02/22/95 tdd; Checking incorrect WT journal */
        else if oeel.ordertype = "t":u then do for wteh:
            find last wteh where
                wteh.wtno = oeel.orderaltno 
                no-lock no-error.

            if avail wteh and wteh.jrnlno2 = 0 then next main.
        end. /* else if oeel.ordertype = "t" */
    
        /*tb 21254 12/23/96 jms; Develop Value Add Module */
        else if oeel.ordertype = "f":u then do for vaeh:
            find last vaeh where
                vaeh.cono = g-cono and
                vaeh.vano = oeel.orderaltno 
                no-lock no-error.

            if avail vaeh and vaeh.jrnlno = 0 then next main.
        end. /* if oeel.ordertype = "f" */
    
        else if oeel.ordertype = "w":u then do for kpet:
            find last kpet where
                kpet.cono = g-cono and
                kpet.wono = oeel.orderaltno 
                no-lock no-error.

            if avail kpet and kpet.jrnlno = 0 then next main.
        end. /* if oeel.ordertype = "w" */
    
        if can-find(first icsp use-index k-icsp where
            icsp.cono       = g-cono            and
            icsp.prod       = oeel.shipprod     and
            icsp.prodtype   = 'I'               and
            icsp.statustype = 'L')
        then
            next main.

        v-processty = "2".
        run create-oefill.

    end.

    /* BOD kits */
    kit:
    for each oeelk no-lock use-index k-prod where
         oeelk.cono       = g-cono          and
         oeelk.whse       = poeh.whse       and
         oeelk.shipprod   = poel.shipprod   and
         oeelk.statustype = "a":u           and
         oeelk.ordertype  = "o":u           and
         oeelk.comptype   <> "r":u          and
         oeelk.qtyreservd < oeelk.stkqtyord and
         oeelk.qtyneeded  >= 0              and
         oeelk.delayresrvfl = no            and
         (oeelk.specnstype = "" or
         (oeelk.specnstype = "s":u and 
          can-find(icsw use-index k-icsw where
                   icsw.cono       = g-cono         and
                   icsw.prod       = oeelk.shipprod and
                   icsw.whse       = oeelk.whse     and 
                   icsw.statustype = "o":u))),
    first oeel no-lock use-index k-oeel where
          oeel.cono          = g-cono         and
          oeel.orderno       = oeelk.orderno  and
          oeel.ordersuf      = oeelk.ordersuf and
          oeel.lineno        = oeelk.lineno   and
          oeel.statustype    = "a":u          and
          oeel.invoicedt     = ?              and
          can-do(",n":u,oeel.specnstype)      and
          oeel.bono          = 0              and
          oeel.returnfl      = no             and
          can-do("so,br":u,oeel.transtype)    and
          oeel.altwhse       = ""             and
          oeel.qtyship       < oeel.qtyord    and
          oeel.botype        <> "d":u,
    first oeeh use-index k-oeeh where
          oeeh.cono          = g-cono        and
          oeeh.orderno       = oeel.orderno  and
          oeeh.ordersuf      = oeel.ordersuf and
         (oeeh.openinit      = ""            or
           oeeh.openinit      = string(g-operinit,"xxxx") + "y":u or
           oeeh.openinit      = string(g-operinit,"xxxx") + "b":u) and
          ( (can-do("s,t":u,oeeh.orderdisp) and oeeh.stagecd = 2) or
             oeeh.stagecd = 1) and
            (can-do("s,t":u,oeeh.orderdisp) or
            (not can-do("s,t":u,oeeh.orderdisp) and 
             oeel.botype  <> "n":u))
    no-lock:
    
        /* If PO/WT/VA not yet received then don't fill the component. */
        if oeelk.orderalttype = "p":u then do for b-poeh:
        
            find last b-poeh where
                b-poeh.cono = g-cono and
                b-poeh.pono = oeelk.orderaltno 
                no-lock no-error.
            
            if avail b-poeh and b-poeh.jrnlno = 0 then next kit.
            
        end. /* if oeelk.orderalttype = "p" */
    
        else if oeelk.orderalttype = "t":u then do for wteh:
        
            find last wteh where
                wteh.wtno = oeelk.orderaltno 
                no-lock no-error.
            
            if avail wteh and wteh.jrnlno2 = 0 then next kit.
            
        end. /* else if oeelk.orderalttype = "t" */
    
        else if oeelk.orderalttype = "f":u then do for vaeh:
        
            find last vaeh where
                vaeh.cono = g-cono and
                vaeh.vano = oeelk.orderaltno 
                no-lock no-error.
            
            if avail vaeh and vaeh.jrnlno = 0 then next kit.
     
        end. /* if oeelk.orderalttype = "f" */
     
        else if oeelk.orderalttype = "w":u then do for kpet:
            find last kpet where
                kpet.cono = g-cono and
                kpet.wono = oeelk.orderaltno 
                no-lock no-error.
            
            if avail kpet and kpet.jrnlno = 0 then next kit.
        end. /* if oeelk.orderalttype = "w" */

        v-processty = "2".
        run create-oefill.

    end. /* kit */
   
    /* Prebuilt Kits */
    kit:
    for each oeelk no-lock use-index k-prod where
         oeelk.cono       = g-cono          and
         oeelk.whse       = poeh.whse       and
         oeelk.shipprod   = poel.shipprod   and
         oeelk.statustype = "a":u           and
         oeelk.ordertype  = "w":u           and
         oeelk.comptype   <> "r":u          and
         oeelk.qtyreservd < oeelk.stkqtyord and
         oeelk.qtyneeded  >= 0              and
         oeelk.delayresrvfl = no            and
         (oeelk.specnstype = "" or
         (oeelk.specnstype = "s":u and 
          can-find(icsw use-index k-icsw where
                   icsw.cono       = g-cono         and
                   icsw.prod       = oeelk.shipprod and
                   icsw.whse       = oeelk.whse     and 
                   icsw.statustype = "o":u))),
    first kpet use-index k-kpet where
          kpet.cono          = g-cono and
          kpet.wono          = oeelk.orderno and
          kpet.wosuf         = oeelk.ordersuf and
          kpet.statustype    = yes and
          kpet.bono          = 0 and
          kpet.stkqtyord     > 0 and
          kpet.qtyship       < kpet.qtyord and
          (kpet.openinit      = ""            or
           kpet.openinit      = string(g-operinit,"xxxx") + "y":u or
           kpet.openinit      = string(g-operinit,"xxxx") + "b":u) and
          kpet.stagecd        = 1 and
          ( (kpet.bofl = yes) or
            (kpet.bofl = no and kpet.bostage > 1) )
    no-lock:

        find oeel where
            oeel.cono       = g-cono        and
            oeel.orderno    = oeelk.orderno and
            oeel.ordersuf   = oeelk.ordersuf and
            oeel.lineno     = oeelk.lineno
            no-lock no-error.
    
        /* If PO/WT/VA not yet received then don't fill the component. */
        if oeelk.orderalttype = "p":u then do for b-poeh:
        
            find last b-poeh where
                b-poeh.cono = g-cono and
                b-poeh.pono = oeelk.orderaltno 
                no-lock no-error.
            
            if avail b-poeh and b-poeh.jrnlno = 0 then next kit.
            
        end. /* if oeelk.orderalttype = "p" */
    
        else if oeelk.orderalttype = "t":u then do for wteh:
        
            find last wteh where
                wteh.wtno = oeelk.orderaltno 
                no-lock no-error.
            
            if avail wteh and wteh.jrnlno2 = 0 then next kit.
            
        end. /* else if oeelk.orderalttype = "t" */
    
        else if oeelk.orderalttype = "f":u then do for vaeh:
        
            find last vaeh where
                vaeh.cono = g-cono and
                vaeh.vano = oeelk.orderaltno 
                no-lock no-error.
            
            if avail vaeh and vaeh.jrnlno = 0 then next kit.
     
        end. /* if oeelk.orderalttype = "f" */
     
        else if oeelk.orderalttype = "w":u then do for b-kpet:
            find last b-kpet where
                b-kpet.cono = g-cono and
                b-kpet.wono = oeelk.orderaltno 
                no-lock no-error.
            
            if avail b-kpet and b-kpet.jrnlno = 0 then next kit.
        end. /* if oeelk.orderalttype = "w" */

        v-processty = "2".
        run create-oefill-k.

    end. /* kit */

    /*o************ Find WT lines to fill ********************************/
    main2:
    for each wtel no-lock use-index k-fill where
        wtel.cono       = g-cono        and
        wtel.statustype = "a":u         and
        wtel.shipfmwhse = poeh.whse     and
        wtel.shipprod   = poel.shipprod and
        (wtel.nonstockty = "" or
        (wtel.nonstockty = "s":u and 
        can-find(icsw use-index k-icsw where
            icsw.cono       = g-cono          and
            icsw.prod       = wtel.shipprod   and
            icsw.whse       = wtel.shipfmwhse and 
            icsw.statustype = "o":u)))
                                           and
        wtel.bono       = 0           and
        wtel.approvety  = "y":u       and
        wtel.delayresrvfl = no        and
        wtel.qtyship    < wtel.qtyord,
    first wteh use-index k-wteh where
        wteh.cono  = g-cono     and
        wteh.wtno  = wtel.wtno  and
        wteh.wtsuf = wtel.wtsuf and
        can-do("1",string(wteh.stagecd)) and
        ((wtel.bofl = yes) or
         (wtel.bofl = no and wteh.bostage > 1)) and
          (wteh.openinit      = ""            or
           wteh.openinit      = string(g-operinit,"xxxx") + "b":u)
    no-lock:

        /*d****** If po/wt not yet received, don't fill the order ****/
        if wtel.ordertype = "p":u then do for b-poeh:
            find last b-poeh where
                b-poeh.cono = g-cono and
                b-poeh.pono = wtel.orderaltno 
                no-lock no-error.

            if avail b-poeh and b-poeh.jrnlno = 0 then next main2.
        end. /* if wtel.ordertype = "p" */

        /*tb 14233 02/22/95 tdd; Check WT receiving journal */
        else if wtel.ordertype = "t":u then do for b-wteh:
            find last b-wteh where
                b-wteh.wtno = wtel.orderaltno 
                no-lock no-error.

            if avail b-wteh and b-wteh.jrnlno2 = 0 then next main2.
        end. /* else if wtel.ordertype = "t" */

        /*tb 21254 12/23/96 jms; Develop Value Add Module */
        /*tb 21254 03/06/97 jms; Corrected error, oeel changed to wtel */
        else if wtel.ordertype = "f":u then do for vaeh:
            find last vaeh where
                vaeh.cono = g-cono and
                vaeh.vano = wtel.orderaltno 
                no-lock no-error.

            if avail vaeh and vaeh.jrnlno = 0 then next main2.
        end. /* if wtel.ordertype = "f" */

        else if wtel.ordertype = "w":u then do for kpet:
            find last kpet where
                kpet.cono = g-cono and
                kpet.wono = wtel.orderaltno 
                no-lock no-error.

            if avail kpet and kpet.jrnlno = 0 then next main2.
        end. /* if wtel.ordertype = "w" */

        /* Implied core items are processed with the Reman core items that are
           being filled. */
        if can-find(first icsp use-index k-icsp where
            icsp.cono       = g-cono            and
            icsp.prod       = wtel.shipprod     and
            icsp.prodtype   = 'I'               and
            icsp.statustype = 'L')
        then
            next main2.

        v-processty = "2".
        run create-oefill-wt.

    end. /* for each wtel */

end procedure. /* fill-oe */

procedure allocate-oefill:
        
    define buffer b-oeel for oeel.
    define buffer b-wtel for wtel.

    fill-loop:
    for each oefill use-index k-oefill where
        oefill.cono      = g-cono and
        oefill.oper2     = g-operinit and
        oefill.reportnm  = v-reportnm and
        oefill.processty ne "p" 
    exclusive-lock
    break by oefill.cono
          by oefill.oper2
          by oefill.reportnm
          by oefill.processty
          by oefill.pickprno descending
          by oefill.reqshipdt
          by oefill.ordertype
          by oefill.orderno
          by oefill.ordersuf
          by oefill.lineno
          by oefill.seqno
          by oefill.compseqno
          by oefill.subty descending:

        v-qtyneeded = oefill.qtyord - oefill.qtyship.
        
        if v-stkqtyord ge v-qtyneeded then do:
            oefill.qtyalloc = v-qtyneeded.
            v-stkqtyord = v-stkqtyord - v-qtyneeded.
        end.
        else do:
            oefill.qtyalloc = v-stkqtyord.
            v-stkqtyord = 0.
        end.
        oefill.processty = "p".

        if oefill.ordertype = "o" then do for b-oeel:
            find b-oeel where
                b-oeel.cono     = g-cono            and
                b-oeel.orderno  = oefill.orderno    and
                b-oeel.ordersuf = oefill.ordersuf   and
                b-oeel.lineno   = oefill.lineno
                exclusive-lock no-error.
            
            if avail b-oeel then do:
                /* 04/24/22 rac; store po on oeel too */
                assign
                   b-oeel.user8 = v-duedt
                   b-oeel.user10 = v-ponum.
                   
                /* 05/31/22 rac; update oeel.promisedt too, unless JIT (unless new date is greater) */
                if b-oeel.botype ne "j" then do:
                    b-oeel.promisedt = v-duedt.

                    if b-oeel.reqshipdt > b-oeel.promisedt then
                        b-oeel.reqshipdt = v-duedt.
                end.
                else do:
                    if v-duedt > b-oeel.promisedt then do:
                        b-oeel.promisedt = v-duedt.

                        if b-oeel.reqshipdt > b-oeel.promisedt then
                            b-oeel.reqshipdt = v-duedt.
                    end.
                end.

                display
                    b-oeel.orderno      @ oeel.orderno
                    b-oeel.ordersuf     @ oeel.ordersuf
                    b-oeel.lineno       @ oeel.lineno
                    b-oeel.custno       @ oeel.custno
                    oefill.lookupnm     @ v-custname
                    b-oeel.stkqtyord    @ oeel.stkqtyord
                with frame f-det2.
                down with frame f-det2.

            end. /* avail b-oeel */
            
        end. /* if oefill.ordertype = "o" */

        if oefill.ordertype = "t" then do for b-wtel:
            find b-wtel where
                b-wtel.wtno     = oefill.orderno    and
                b-wtel.wtsuf    = oefill.ordersuf   and
                b-wtel.lineno   = oefill.lineno
                exclusive-lock no-error.
            
            if avail b-wtel then do:
                /* 04/24/22 rac; store po on oeel too */
                assign
                    b-wtel.user8 = v-duedt
                    b-wtel.user10 = v-ponum.

                find icsd where
                    icsd.cono   = g-cono            and
                    icsd.whse   = b-wtel.shiptowhse 
                    no-lock no-error.
                v-whsename = if avail icsd then icsd.name else v-invalid.

                display
                    b-wtel.wtno         @ wtel.wtno
                    b-wtel.wtsuf        @ wtel.wtsuf
                    b-wtel.lineno       @ wtel.lineno
                    b-wtel.shiptowhse   @ wtel.shiptowhse
                    v-whsename
                    b-wtel.stkqtyord    @ wtel.stkqtyord
                with frame f-det3.
                down with frame f-det3.

                for each wtelo where
                    wtelo.wtno      = b-wtel.wtno   and
                 /* wtelo.wtsuf     = b-wtel.wtsuf  and */
                    wtelo.lineno    = b-wtel.lineno and
                    wtelo.ordertype = "o"
                    no-lock:
                    
                    do for b-oeel:
                        find b-oeel where
                            b-oeel.cono     = g-cono            and
                            b-oeel.orderno  = wtelo.orderaltno  and
                            b-oeel.ordersuf = wtelo.orderaltsuf and
                            b-oeel.lineno   = wtelo.linealtno
                            exclusive-lock no-error.
                        
                        if avail b-oeel then do:
                            /* 04/24/22 rac; store po on oeel too */
                            assign
                                b-oeel.user8 = v-duedt
                                b-oeel.user10 = v-ponum.

                            /* 05/31/22 rac; update oeel.promisedt too, unless JIT (unless new date is greater) */
                            if b-oeel.botype ne "j" then do:
                                b-oeel.promisedt = v-duedt.

                                if b-oeel.reqshipdt > b-oeel.promisedt then
                                    b-oeel.reqshipdt = v-duedt.
                            end.
                            else do:
                                if v-duedt > b-oeel.promisedt then do:
                                    b-oeel.promisedt = v-duedt.

                                    if b-oeel.reqshipdt > b-oeel.promisedt then
                                        b-oeel.reqshipdt = v-duedt.
                                end.
                            end.

                            find arsc where
                                arsc.cono   = g-cono        and
                                arsc.custno = b-oeel.custno 
                                no-lock no-error.
                            v-custname = if avail arsc then arsc.name else v-invalid.
                            
                            display
                                b-oeel.orderno      @ oeel.orderno
                                b-oeel.ordersuf     @ oeel.ordersuf
                                b-oeel.lineno       @ oeel.lineno
                                b-oeel.custno       @ oeel.custno
                                v-custname
                                b-oeel.stkqtyord    @ oeel.stkqtyord
                            with frame f-det2.
                            down with frame f-det2.
            
                        end. /* avail b-oeel */
                    end.
                end.
            end. /* avail b-wtel */

        end. /* if oefill.ordertype = "t" */
        
        if v-stkqtyord le 0 then
            leave fill-loop.
    end.

end procedure. /* allocate-oefill */

procedure create-oefill:
        
    find arsc where
        arsc.cono   = g-cono        and
        arsc.custno = oeeh.custno 
        no-lock no-error.
    create oefill.
    assign
        oefill.cono      = g-cono
        oefill.oper2     = g-operinit
        oefill.reportnm  = v-reportnm
        oefill.processty = v-processty
        oefill.prod      = oeel.shipprod
        oefill.descrip   = ""
        oefill.orderno   = oeel.orderno
        oefill.ordersuf  = oeel.ordersuf
        oefill.lineno    = oeel.lineno
        oefill.seqno     = v-seqaltno
        oefill.notesfl   = oeeh.notesfl
        oefill.transtype = oeeh.transtype
        oefill.orderdisp = oeeh.orderdisp
        oefill.pickprno  = oeeh.pickprno
        oefill.reqshipdt = if oeeh.orderdisp = "j":u then oeel.reqshipdt
                           else oeeh.reqshipdt
        oefill.qtyord    = oeel.stkqtyord
        oefill.qtyship   = oeel.stkqtyship
        oefill.unit      = oeel.unit
        oefill.subty     = ""
        oefill.price     = oeel.price - oeel.discamt
        oefill.lookupnm  = if avail arsc then arsc.name
                           else v-invalid
        oefill.ordertype = "o":u.

end procedure. /* create-oefill */

procedure create-oefill-k:

    find arsc where
        arsc.cono   = g-cono        and
        arsc.custno = oeeh.custno 
        no-lock no-error.
    create oefill.
    assign
        oefill.cono      = g-cono
        oefill.oper2     = g-operinit
        oefill.reportnm  = v-reportnm
        oefill.processty = v-processty
        oefill.prod      = oeelk.shipprod
        oefill.descrip   = ""
        oefill.orderno   = oeelk.orderno
        oefill.ordersuf  = oeelk.ordersuf
        oefill.lineno    = oeelk.lineno
        oefill.seqno     = v-seqaltno
        oefill.notesfl   = oeeh.notesfl
        oefill.transtype = oeeh.transtype
        oefill.orderdisp = oeeh.orderdisp
        oefill.pickprno  = oeeh.pickprno
        oefill.reqshipdt = if oeeh.orderdisp = "j":u then oeel.reqshipdt
                           else oeeh.reqshipdt
        oefill.qtyord    = oeelk.stkqtyord
        oefill.qtyship   = oeelk.stkqtyship
        oefill.unit      = oeelk.unit
        oefill.subty     = ""
        oefill.price     = oeelk.price
        oefill.lookupnm  = if avail arsc then arsc.name
                           else v-invalid
        oefill.ordertype = "o":u.

end procedure. /* create-oefill-k */

procedure create-oefill-wt:

    define buffer b-wteh for wteh.
    find b-wteh where
        b-wteh.wtno     = wtel.wtno     and
        b-wteh.wtsuf    = wtel.wtsuf
        no-lock no-error.
        
    create oefill.
    assign
        oefill.cono      = g-cono
        oefill.oper2     = g-operinit
        oefill.reportnm  = v-reportnm
        oefill.processty = v-processty
        oefill.prod      = wtel.shipprod
        oefill.descrip   = ""
        oefill.orderno   = wtel.wtno
        oefill.ordersuf  = wtel.wtsuf
        oefill.lineno    = wtel.lineno
        oefill.notesfl   = b-wteh.notesfl
        oefill.transtype = b-wteh.transtype
        oefill.orderdisp = ""
        oefill.pickprno  = 0
        oefill.seqno     = 0
        oefill.reqshipdt = b-wteh.reqshipdt
        oefill.qtyord    = wtel.stkqtyord
        oefill.qtyship   = wtel.stkqtyship
        oefill.unit      = wtel.unit
        oefill.subty     = ""
        oefill.price     = wtel.prodcost
        oefill.lookupnm  = b-wteh.shiptowhse
        oefill.ordertype = "t":u.

end procedure. /* create-oefill-wt */

procedure clear-oefill:

    for each oefill where
        oefill.cono     = g-cono        and
        oefill.oper2    = g-operinit    and
        oefill.reportnm = v-reportnm
        exclusive-lock:
        
        delete oefill.
    end.

end procedure. /* clear-oefill */
