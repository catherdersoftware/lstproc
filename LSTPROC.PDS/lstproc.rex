/*%NOCOMMENT -------------  rexx procedure  ----------------- */
_version_ = "3.4.1"
signal skipsubs

 /* --------------------------------------------------------- */
 /*  getaddr                                                  */
 /*     Function to return address stored at address passed   */
 /*                                                           */
 /*     Input:  addr = decimal address of storage where       */
 /*                    address is                             */
 /*             m    = mask to AND against returned address   */
 /*                    (optional)                             */
 /*             l    = length of storage to return (optional) */
 /*                                                           */
 /*     Output: l bytes at addr in decimal                    */
 /*                                                           */
 /* ----------------------------------------------------------*/
getaddr: procedure expose validhex
arg a, m, l
if symbol('VALIDHEX') == "LIT" then
  validhex = xrange('a', 'i')xrange('j', 'r')xrange('s', 'z')||,
    xrange('A', 'I')xrange('J', 'R')xrange('S', 'Z')xrange('0', '9')
if l == '' then
  l = 4
if datatype(a) == "NUM" then
  a = d2x(a, 8)
else if verify(a, validhex) \= 0 then
  a = right(c2x(a), 8, '0')
s = storage(a, l)
if m \== '' then
  s = bitand(s, m)
return c2d(s) /* getaddr */

/* Name:      lstproc                                         *
 *                                                            *
 * Function:  Display active JES proclibs                     *
 *                                                            *
 * Syntax:    %lstproc proc                                   *
 *                                                            *
 *            Where proc is the name of the JES proc or       *
 *            blank to default to the primary subsystem.      *
 *                                                            *
 * Usage:     Under ISPF display the ISRDDN display filtered  *
 *            with the PROC DDnames ($PROC or IAT)            *
 *                                                            *
 *            Outside of ISPF use the echo TSO command to     *
 *            display the info (echo is trappable while say   *
 *            is not).  ECHO can be found in File312 on the   *
 *            CBTTape site as EKKO.                           *
 *                                                            *
 *            If invoked as a function (e.g. x=lstproc)       *
 *            then act as if under native TSO and not ISPF.   *
 *                                                            *
 * Assumptions and Caveats:                                   *
 *            1. if proc not specified then use primary       *
 *               subsystem                                    *
 *            2. only //PROCxx will be looked at              *
 *            3. comments will be ignored                     *
 *            4. symbolics are supported (mostly)             *
 *            5. system symbolics are supported               *
 *            6. only cataloged datasets are supported        *
 *           **. JES2 Dynalloc procs are not supported        *
 *            8. nested symbolics may not work                *
 *            9. if the mstrjcl or jes2 proc changes that     *
 *               is what you will see reflected               *
 *           10. This does NOT report the actual proc usage   *
 *               but what is found from mstrjcl and the       *
 *               jes proc.                                    *
 *           11. *IMPORTANT* It is assumed you have RACF      *
 *               read for the LISTDATA IDCAMS command.        *
 *               If not find *LISTDATA* and make the          *
 *               noted changes.  You will then be limited     *
 *               to the cataloged iplparm dataset.            *
 *           12. SDSF REXX is used to capture JES2 defined    *
 *               Proclibs.                                    *
 *           13. The TSO CONSOLE command and SDSF REXX are    *
 *               used to capture dynamically defined          *
 *               PROCLIBs.                                    *
 *                                                            *
 * Author:    Lionel B. Dyck                                  *
 *            Internet: lbdyck@gmail.com                      *
 *                                                            *
 * History:                                                   *
 *                                                            *
 *          2024-03-14 - Use primary subsystem by default     *
 *                       (allows JES3(Plus) usage)            *
 *                     - Add automatic volume check if        *
 *                       IPLPARM data set is not cataloged    *
 *                     - Improve function invocation check    *
 *                     - Support live PROCLIB values via      *
 *                       console.                             *
 *                     - Technical debt cleanup               *
 *          2023-04-10 - Check for invocation as Function     *
 *                       and set mode as non-ISPF             *
 *          2023-03-31 - Correct symbolic replacement         *
 *          2023-03-29 - Add dataset in IEFJOBS from MSTRJCL  *
 *                       and allocated to $PROCJOB            *
 *                     - Dynamically get JES2 Defined Procs   *
 *          2023-03-28 - Correct symbolic processing          *
 *                       if symbolic starts with .&           *
 *          2021-09-10 - Updates from Ray Mullins to fix 0C4  *
 *                       when compiled                        *
 *          2014-01-31 - Don't alloc/open/read the iplparm    *
 *                       if it can't be found                 *
 *          2009-07-08 - Change # to $ (thx hartmut)          *
 *          2009-07-08 - MSG("OFF") in FREE routine           *
 *                     - use _junk_ instead of RC             *
 *          2009-05-11 - Check whether proclib member exists  *
 *                       SYS1.PROCLIB(JES2) default may fail. *
 *          2008-11-05 - Correction in handling symbols       *
 *                       custplex and lpar                    *
 *          2008-10-15 - Info about old sysmvs                *
 *                     - Note:                                *
 *                       LISTDATA/LDATA in AUTHCMD in IKJTSO00*
 *                     - substitute vars  left  and  right    *
 *                               by vars _left_ and _right_   *
 *                                                            *
 *          2007-10-16 - If not under ISPF just write         *
 *                       dd and dsnames to screen             *
 *                                                            *
 *          2007-02-20 - Correction from Ian Ramage           *
 *                       Fix if JES2 Proc in >2 proclib       *
 *                                                            *
 *          2006-11-27 - Hartmut Beckmann                     *
 *                       Changes for better member support    *
 *                                                            *
 *          2006-09-25 - Peggy Norton                         *
 *                       Fix call to subroutine for symbolics *
 *                                                            *
 *          2006-05-17 - Jeff Dixon                           *
 *                       Support //PROCxxxx instead of PROCxx *
 *                                                            *
 *          2006-05-11 - Peggy Norton                         *
 *                       Fix if LOADxx does not have defined  *
 *                       SYS1.PARMLIB                         *
 *                                                            *
 *          2006-01-26 - Ian Ramage                           *
 *                       Add Numeric Digits 10 to resolve     *
 *                       issue under z/OS 1.6                 *
 *                                                            *
 *          2003-09-02 - John Bloniarz                        *
 *                       McDonald's Corporation               *
 *                       630-623-3224                         *
 *                       john.bloniarz@mcd.com                *
 *                                                            *
 *                       make callable as a function from     *
 *                       REXX to allocate system Parmlibs,    *
 *                       system Proclibs and/or PROCxx's.     *
 *                       Changed DD name $PROCMST to          *
 *                       $PROCLIB when called as a function.  *
 *                                                            *
 *          2003-08-28 - John Bloniarz                        *
 *                       McDonald's Corporation               *
 *                       630-623-3224                         *
 *                       john.bloniarz@mcd.com                *
 *                                                            *
 *                       add code to resolve all system       *
 *                       symbols before allocation.           *
 *                                                            *
 *          2002-08-19 - correct to work with OS/390 below    *
 *                       2.10 (broke on 8/18 change)          *
 *          2002-08-18 - pass ONLY $PROC to ISRDDN            *
 *                       and bypass former ispf msg           *
 *          2001-08-15 - support INCLUDE in the Proc          *
 *          2001-04-25 - fix from Iam Ramage to correctly     *
 *                       find the parmlib volser.             *
 *                       ian.ramage@rs-components.com         *
 *          2001-02-21 - fix if proc symbol has quotes        *
 *          2000-10-24 - update to get iplparm vol dyn        *
 *                       thx to Philippe Richard of IBM       *
 *          2000-10-23 - update to get iplparm dsn dyn        *
 *                       thx to Philippe Richard of IBM       *
 *          2000-06-20 - general release                      *
 *                     - Change dds to $PROCMST & $PROCnn     *
 *          2000-06-15 - minor clean up                       *
 *          2000-06-14 - various changes                      *
 *                     - add code to dynamically find mstjclxx*
 *                       from Todd Burrell (zpn6@cdc.gov)     *
 *          2000-06-13 - creation                             *
 *                                                            *
 * ---------------------------------------------------------- *
 *                                                            *
 * ---------------------------------------------------------- */
skipsubs:
signal on novalue name sub_novalue
/* --------------------------------- *
 | Change if SDSF REXX not available |
 * --------------------------------- */
sdsf_jes_procs = 'Y'     /* Change to N if no SDSF REXX */
cons_jes_procs = 'Y'     /* Change to N if no CONSOLE   */
/* ----------------------------------------------------- *
 * Setup defaults in sub routine                         *
 * ----------------------------------------------------- */
_x_ = sub_init()
/* -------------------------------------------- *
 * Check for any passed options. The only       *
 * supported option at this time is the name of *
 * the JES proc to look for.                    *
 * -------------------------------------------- */
arg options

ddn = "JCL"random(9999)
Numeric Digits 10
!console = 0; xrc = 0
jes2 = "JES2"; jes3 = "JES3"

/* --------------------------------------------------------------- *
 * test options and use for procname or use the primary subsystem  *
 * --------------------------------------------------------------- */
if substr(options,1,1) = "." then do
  parse value options with "." lstprocm .
  options = null
end
if words(options) > 1 then do
  parse value options with opt1 " " opt2 " " opt3 " " opt4
  if opt1 = "*" then
    options = null
  else
    options = opt1
end
else if options = "*" then
  options = null
if options = null then do
   ssid = storage(d2x(getaddr(getaddr(getaddr(c2d('218'x)) +,
     c2d('B4'x)) + c2d('13c'x)) + c2d('8'x), 8), 4)
end
else
  ssid = options
if sdsf_jes_procs = 'Y' then
  if ssid = "JES3" then do
    sdsf_jes_procs = 'N'
    procddpfx = 'IAT'
  end
  else do
    procddpfx = '$PROC'
  end
isfunc = "N"
allocdd = null
retstr  = null
if length(opt2) > 0 then do
  if abbrev("FUNCTION",opt2,1) = 1 then do
    isfunc = "F"
    if length(opt3) > 0 then do
      allocdd = opt3
    end
  end
  else if abbrev("NOSDSF", opt2, 3) then
    sdsf_jes_procs = 'N'
end

/* ----------------------------- *
 * Test for ISPF and if not then *
 * set flag to only echo Parmlib *
 * info to the "screen".         *
 * ----------------------------- */
parse source x1 env .
sysispf = sysvar("sysispf")
if env /= 'FUNCTION' then do
  ispf = (sysispf == "ACTIVE")
end
else do
  ispf = 0
  isfunc = 'Y'
end
/* ----------------------------------------------------- *
 * Code from Todd Burrell to get mstjcl info             *
 * Enhanced by Ian Ramage                                *
 * ----------------------------------------------------- */
CVT=STORAGE("10",4)
CVTECVT=STORAGE(D2X(C2D(CVT)+140),4)
/* GET THE IPL LOADPARMS */
LOADPARM=STORAGE(D2X(C2D(CVTECVT)+168),8)
/* address of IHAIPA control block */
CVTIPA=STORAGE(D2X(C2D(CVTECVT)+392),4)
/* mstjcl section in IPA */
MJC=STORAGE(D2X(C2D(CVTIPA)+2448),4)
/* mstjcl section in IPA length */
MJCLEN=C2D(STORAGE(D2X(C2D(CVTIPA)+2452),2))
/* ieasys source in mstjclxx */
MJCSYS=STORAGE(D2X(C2D(CVTIPA)+2454),2)
/* mstjcl xx value */
MJCXX=STORAGE(D2X(C2D(MJC)+0),MJCLEN)
if left(mjcxx,1) = "(" then
  parse value mjcxx with "(" mjcxx ")" .

/* address of IHAIPA control block */
IPLPARM = Strip(LOADPARM)
LOADADR = Substr(IPLPARM,1,4)

/* mstjcl section in IPA */
ipalpar  = storage(d2x(c2d(cvtipa)+32), 8)
ipalpdsn = storage(d2x(c2d(cvtipa)+48),44)
parmaddr = storage(d2x(c2d(cvtipa)+92), 4)

 /* *LOADDATA* - Important information   *
  *  if you don't have RACF Read for the *
  *  LISTDATA IDCAMS command then set    *
  *  LOADVOL = ""                        */
 /*  LOADVOL  = chk_volser(parmaddr)   */
loadvol = null
lparm = ""strip(IPALPDSN)"(load"substr(loadparm,5,2)")"
lparm = ""_apost_""lparm""_apost_""
if sysdsn(lparm) \== 'OK' then
  loadvol = chk_volser(parmaddr)

/* ----------------------------------------------------- *
 * Read loadparm member to find parmlibs                 *
 * ----------------------------------------------------- */
if loadvol <> null then do
  "Alloc f("ddn") ds("lparm") shr reuse volume("LOADVOL")",
   "unit(sysallda)"
  "Execio * diskr" ddn "(open finis stem in."
  'Free f('ddn')'
end
else do
  if sysdsn(lparm) = 'OK' then do
    "Alloc f("ddn") ds("lparm") shr reuse"
    "Execio * diskr" ddn "(open finis stem in."
    'Free f('ddn')'
  end
  else do
    in.0 = 0
    rc = 4
  end
end
_rc_ = rc
_LPAR_ = "true"
do i = 1 to in.0
  if word(in.i,1) = "LPARNAME" then
    if word(in.i,2) = ipalpar then _LPAR_ = "true"
    else _LPAR_ = "false"
  if word(in.i,1) = "PARMLIB" & _LPAR_ = "true" then
    parmlibs = parmlibs word(in.i,2)
end i
if parmlibs = null then
  parmlibs = "SYS1.PARMLIB"
if wordpos("SYS1.PARMLIB",parmlibs) = 0 then
  parmlibs = parmlibs" SYS1.PARMLIB"

/* ----------------------------------------------------- *
 * Alloc all Parmlibs to DD: #PARMLIB (If Appropriate)   *
 * ----------------------------------------------------- */
if isfunc \= "N" then do
  if allocdd = sub_allocdd("#PARMLIB") then do
    alloc_parms = null
    do i = 1 to words(parmlibs)
      parm  = ""_apost_""word(parmlibs,i)""_apost_""
      alloc_parms = alloc_parms parm
    end i
    if pos("&",alloc_parms) > 0 then do
      alloc_parms = fix_sym1(alloc_parms)
    end
    _rc_ = msg("OFF")
    "Free  f(#PARMLIB)"
    _rc_ = msg("ON")
    "Alloc f(#PARMLIB) ds("alloc_parms") shr reuse"
    retstr = strip(retstr "#PARMLIB")
  end
  if allocdd <> null then
    if allocdd = retstr then
      signal Finish
end

/* ----------------------------------------------------- *
 * Find PARMLIB with MSTJCLxx                            *
 * ----------------------------------------------------- */
do i = 1 to words(parmlibs)
  mstrjcl = ""strip(word(parmlibs,i))"(MSTJCL"mjcxx")"
  mstrjcl = ""_apost_""mstrjcl""_apost_""
  if "OK" = sysdsn(mstrjcl) then
    leave i
end i

/* ----------------------------------------------------- *
 * Alloc and read master jcl parmlib member              *
 * ----------------------------------------------------- */
if "OK" <> sysdsn(mstrjcl) then do
  in.0 = 1
  in.1 = "//IEFPDSI DSN=SYS1.PROCLIB  "
end
else do
  if pos("&",mstrjcl) > 0 then
    mstrjcl = fix_sym1(mstrjcl)
  "Alloc f("ddn") ds("mstrjcl") shr reuse"
  "Execio * diskr" ddn "(open finis stem in."
  _rc_ = rc
  "Free  f("ddn")"
end

/* ----------------------------------------------------- *
 * Find all proclibs in IEFPDSI and save them            *
 * Updates by ian.ramage@rs-components.com               *
 * ----------------------------------------------------- */
hit = 0
mstdd = null
do i = 1 to in.0
  in.i  = TRANSLATE(in.i," ",",")
  if hit = 1 then do
     if pos("&",dsn) > 0 then
       call fix_sym
     if substr(in.i,3,1) = " " then do
       parse value in.i with . "DSN=" dsn " " .
       if mstdd /= 'IEFJOBS' then
         proclibs = proclibs dsn
       else
         procjobs = procjobs dsn
     end
     else
       hit = 0
  end
  if left(in.i,9) = "//IEFJOBS" then do
    mstdd = 'IEFJOBS'
    hit = 1
    parse value in.i with . "DSN=" dsn " " .
    dsn = word(strip(dsn),1)
    if pos("&",dsn) > 0 then
      call fix_sym
    procjobs = procjobs dsn
  end
  if left(in.i,9) = "//IEFPDSI" then do
    hit = 1
    mstdd = null
    parse value in.i with . "DSN=" dsn " " .
    dsn = word(strip(dsn),1)
    if pos("&",dsn) > 0 then
      call fix_sym
    proclibs = proclibs dsn
  end
end i

/* ----------------------------------------------------- *
 * Alloc all Master JCL Proclibs to DD: $PROCMST         *
 * (or DD: $PROCLIB if called as a function)             *
 * ----------------------------------------------------- */
alloc_procs = null
do i = 1 to words(proclibs)
  $proc = ""_apost_""word(proclibs,i)""_apost_""
  alloc_procs = alloc_procs $proc
end i
if pos("&",alloc_procs) > 0 then do
  alloc_parms = fix_sym1(alloc_parms)
end
procddn = procddpfx"MST"
if isfunc \= "N" then do
  if allocdd = sub_allocdd(procddpfx"LIB") then do
    procddn = procddpfx"LIB"
  end
end
_rc_ = msg("OFF")
"Free  f("procddn")"
_rc_ = msg("ON")
if ispf = 1 | isfunc = "Y" then
  "Alloc f("procddn") ds("alloc_procs") shr reuse"
else call echo procddn alloc_procs
if isfunc \= "N" then do
   if allocdd = sub_allocdd(procddpfx"LIB") then do
      retstr = strip(retstr procddpfx"LIB")
      end
   if allocdd <> null &,
      allocdd = retstr then signal Finish
   end

/* ----------------------------------------------------- *
 * Alloc all Master JCL IEFJOBS datasets to $PROCJOB     *
 * ----------------------------------------------------- */
alloc_procs = null
if procjobs /= null then do
  do i = 1 to words(procjobs)
    $proc = ""_apost_""word(procjobs,i)""_apost_""
    alloc_procs = alloc_procs $proc
  end i
  if pos("&",alloc_procs) > 0 then do
    alloc_parms = fix_sym1(alloc_parms)
  end
  procddn = procddpfx"JOB"
  _rc_ = msg("OFF")
  "Free  f("procddn")"
  _rc_ = msg("ON")
  if ispf = 1 | isfunc = "Y" then
     "Alloc f("procddn") ds("alloc_procs") shr reuse"
  else
    call echo procddn alloc_procs
end

if ssid = "JES3" then
  call procjes3
else
  call procjes2

/* ----------------------------------------------------- *
 * Now alloc DD: ddpfx...                                *
 * ----------------------------------------------------- */

/* ----------------------------------------------------- *
 * CALL STEMVIEW "VIEW",in.,,,"Debugging mode: STEM IN." *
 * ----------------------------------------------------- */
alloc_procs = null
do i = 1 to words(procs)
  if alloc_procs <> null then do
    if pos("&",alloc_procs) > 0 then do
      alloc_parms = fix_sym1(alloc_parms)
    end
    doalloc = "N"
    if isfunc \= "N" then do
      if allocdd = sub_allocdd(procddpfx""nn) then do
         doalloc = "Y"
       end
    end
    else
      doalloc = "Y"
    if doalloc = "Y" then do
      _rc_ = msg("OFF")
      "Free  f("procddpfx||nn")"
      _rc_ = msg("ON")
      if ispf = 1 then
        "Alloc f("procddpfx||nn") ds("alloc_procs")",
         "shr reuse"
      else
        call echo procddpfx||nn alloc_procs
    end
    if isfunc \= "N" then do
       if allocdd = sub_allocdd(procddpfx||nn) then do
          retstr = strip(retstr procddpfx||nn)
       end
       if allocdd <> null &,
          allocdd = retstr then signal Finish
     end
  end
  nn = word(procs,i)
  alloc_procs = null
  do j = 1 to words(proc.nn)
    alloc_procs = alloc_procs word(proc.nn,j)
  end j
end i
if alloc_procs <> null then do
   if pos("&",alloc_procs) > 0 then do
      alloc_parms = fix_sym1(alloc_parms)
      end
   doalloc = "N"
   if isfunc \= "N" then do
      if allocdd = sub_allocdd(procddpfx||nn) then do
         doalloc = "Y"
         end
      end
    else do
      doalloc = "Y"
      end
   if doalloc = "Y" then do
      _rc_ = msg("OFF")
      "Free  f("procddpfx||nn")"
      _rc_ = msg("ON")
      if ispf = 1 then
        "Alloc f("procddpfx||nn") ds("alloc_procs")",
         "shr reuse"
      else
        call echo procddpfx||nn alloc_procs
      end
   if isfunc \= "N" then do
      if allocdd = sub_allocdd(procddpfx||nn) then do
         retstr = strip(retstr procddpfx||nn)
         end
      if allocdd <> null &,
         allocdd = retstr then signal Finish
      end
   end

/* ----------------------------------------------------- *
 * If not called as a function, invoke ISRDDN to display *
 * allocations and then free DD names.  if called as a   *
 * function, simply return the names of the allocated    *
 * files to the caller.                                  *
 * ----------------------------------------------------- */
Finish:
call consdeact
if isfunc = "N" then do
  Address ISPExec
  lev = mvsvar("sysmvs")
  lev = substr(lev,3,1)
  if lev < 6 then do
    zedsmsg = null
    zedlmsg = "System and" ssid "Proclibs",
              "have been identified and allocated",
              "using" procddpfx"MST for the Master JCL",
              "Proclibs and" procddpfx"xx for the" ssid,
              "Proclibs.               ",
              "Issue ONLY" procddpfx,
              "to display only DDs with" procddpfx "in the",
              "ddname."
    "Setmsg msg(isrz001)"
    "Select cmd(isrddn)"
  end
  else do
    zdel = null
    "vget (zdel)"
    zopt = "Only" procddpfx||zdel'PARM'
    if lstprocm /= null then do
       zopt = zopt""zdel"long"zdel" member "lstprocm
    end
    "control errors return"
    zmsg000l = _my_env_""copies(" ",80)""_sysenv_""
    "setmsg msg(ispz000) cond"
    "Select cmd(isrddn" zopt")"
  end

 /* -------------------------- *
  * Now free all allocations   *
  * -------------------------- */
  if ispf = 0 & isfunc = "N" then
    exit 0
  _rc_ = msg("OFF")
  Address TSO
  "Free f($PROCMST $PROCJOB)"
  if sdsf_jes_procs = 'Y' then do
    DO J = 1 TO SDSFPROC.0
      "FREE FI("SDSFPROC.J")"
    END J
  end
  do i = 1 to words(procs)
    "Free f("procddpfx||word(procs,i)")"
  end i
  _rc_ = msg("ON")
  retstr = 0
end
else do
  _rc_ = msg("OFF")
  "Free  f($PROCMST $PROCJOB)"
  if sdsf_jes_procs = 'Y' then do
    DO J = 1 TO SDSFPROC.0
       "FREE FI("SDSFPROC.J")"
    END J
  end
  do i = 1 to words(procs)
     "Free f("procddpfx||word(procs,i)")"
  end i
  _rc_ = msg("ON")
end
if retstr = null then
  retstr = 16
Exit retstr

/* ----------------------------------------------------- *
 * Process JES2                                          *
 * ----------------------------------------------------- */
procjes2:
/* ---------------------------------------------------- *
* Now get JES2 PROCLIBS using SDSF REXX Interface       *
* **Comment or delete if you don't have SDSF**          *
* ----------------------------------------------------- */
sdsfproc.0 = 0
IF (sdsf_jes_procs = "Y") THEN DO
  RC = ISFCALLS("ON")
  isfrows = 0
  ADDRESS SDSF "ISFEXEC PROC"
  RC = ISFCALLS("OFF")
  if rc = 0 then do
    ALLOC_PROCS = ""
    K = 1
    TMPDD = DDNAME.1
    SDSFPROC.1 = '$'TMPDD
    DO J = 1 TO ISFROWS
      IF (TMPDD = DDNAME.J) THEN Do
        if wordpos("'"dsname.j"'",alloc_procs) = 0
        Then
        ALLOC_PROCS = ALLOC_PROCS "'"DSNAME.J"'"
      End
      ELSE DO
        if ispf = 1 | isfunc = "Y" then
          "Alloc f($"TMPDD")",
            "ds("alloc_procs") shr reuse"
        else
          call echo TMPDD alloc_procs
        TMPDD = DDNAME.J
        ALLOC_PROCS = "'"DSNAME.J"'"
        K = K + 1
        SDSFPROC.K = '$'TMPDD
      END /* ELSE */
      IF (J = ISFROWS) THEN DO
        if ispf = 1 | isfunc = "Y" then
          "Alloc f($"TMPDD")",
           "ds("alloc_procs") shr reuse"
        else
          call echo TMPDD alloc_procs
        ALLOC_PROCS = ""
        TMPDD = ""
      END /* IF J */
    END J
    DROP ISFROWS.
    SDSFPROC.0 = K
  end
END /* IF */
else do
  call processjesproc
end /* else */
return

/* ----------------------------------------------------- *
 * Process JES3                                          *
 * ----------------------------------------------------- */
procjes3:
/* ---------------------------------------------------- *
* Now get JES3 PROCLIBs using operator command and      *
* SDSF Rexx interface                                   *
*                                                       *
* We can't use TSO CONSOLE because it only returns the  *
* first line of the *I,PROCLIB output.                  *
* ----------------------------------------------------- */
procs = null
if cons_jes_procs = 'Y' then do
  call consoleproc '*I,PROCLIB'
end /* if cons_jes_procs */
if procs = ' ' then do
  call processjesproc
end /* if procs */
return

/* ----------------------------------------------------- *
 * Now look thru Master JCL Proclibs for JESx Proc       *
 * ----------------------------------------------------- */
processjesproc: procedure expose null _apost_ !console consname ssid ddn,
 proc. procs proclibs (global_vars)
ddpfx.jes2 = 'PROC'; ddpfx.jes3 = 'IATPLB'
inputdd.jes2 = 'HASPPARM'; inputdd.jes3 = 'JES3IN'
ddpfx = ddpfx.ssid
inputdd = inputdd.ssid
member = ssid
jes_proc = null
do i = 1 to words(proclibs)
  $proc = ""_apost_""word(proclibs,i)"("member")"_apost_""
  if pos("&",$proc) > 0 then do
    $proc = fix_sym1($proc)
  end
  if "OK" = sysdsn($proc) then do
    jes_proc = $proc
    leave i
  end
end i

/* --------------------------------------------------------- *
 * Read in the JES Proc                                      *
 * --------------------------------------------------------- */
if jes_proc = null then
   return
"Alloc f("ddn") ds("$proc") shr reuse"
"Execio * diskr" ddn "(open finis stem in."
_rc_ = rc
"Free  f("ddn")"

call scanjesjcl
trace ?i

if symbol('PROC.'inputdd) == 'VAR' then do
  inputdsn = proc.inputdd
  drop proc.inputdd
  wp = wordpos(inputdd, procs)
  procs = subword(procs, 1, wp - 1) subword(procs, wp + 1)
  drop in.
  in.0 = 0
  do i = 1 to words(inputdsn)
    dsn = word(inputdsn, i)
    "Alloc f("ddn") ds("dsn") shr reuse"
    "Execio * diskr" ddn "(open finis stem prm."
    _rc_ = rc
    "Free  f("ddn")"
    if in.0 = 0 then
      do # = 0 to prm.0
        in.# = prm.#
      end #
    else do
      c = in.0
      do # = 1 to prm.0
        c = c + 1
        in.c = prm.#
      end #
      in.0 = c
    end
  end
  drop prm.
  if ssid = 'JES' then
    call jes3procs
  else
    call jes2procs
end /* if proc.inputdd */
drop in.
return /* processjesproc */

scanjesjcl:
/* ----------------------------------------------------- *
 * Now find all //ddpfx and save PROCLIB names, and      *
 * collect SYSIN data sets, processing as if a PROCLIB,  *
 * but removing it at return.                            *
 * ----------------------------------------------------- */
procs = null
syms = null
hit = 0
namechk = ddpfx inputdd
/* ----------------------------------------------------- *
 * Hit state meanings:                                   *
 *   1 = PROC found                                      *
 *   3 = Not processing a DD                             *
 *   4 = Processing a desired DD                         *
 * ----------------------------------------------------- */
do i = 1 to in.0
  in.i = strip(left(in.i, 72), 'T')
  if left(in.i,3) == "//*" then
    iterate i
  if left(in.i, 2) \== "//" then
    iterate i
  parse var in.i '//' 3 c1 4 . 3 w1 w2 w3
  if c1 == ' ' then do
    w3 = strip(w2)
    w2 = strip(w1)
    w1 = null
  end /* if c1 */
  else do
    w1 = strip(w1)
    w2 = strip(w2)
    w3 = strip(w3)
  end /* else */
  if w2 == "INCLUDE" then do
    parse value in.i with . "MEMBER="member .
    in.i = "//******** INCLUDED MEMBER="member
    if pos("&",member) > 0 then do
      member = fix_sym1(member)
    end
    call scanjesjcl /* recursion */
    leave i
  end /* if word(in.i */
  If hit = 1 then do
    if substr(in.i,3,1) >= "A" then
      hit = 3
  end
  If w2 == "PROC" then
    hit = 1
  if hit = 3 then
    if pos(w1, namechk) \= 0 then
      if length(w1) < 9 then
        hit = 4
  if hit = 4 then do
    if pos(w1, namechk) = 0 then do
      name1 = left(w1, 1)
      if name1 >>= "A" | pos(name1, '$#@') > 0 then
        hit = 3
      if w1 == null then
        w1 = currdd
    end
    else do
      if length(w1) > 8 then
        hit = 3
    end
    if hit = 3 then
      iterate i
    if pos(w1, namechk) > 0 then
      if symbol('PROC.'w1) == 'LIT' then do
        proc.w1 = null
        procs = procs w1
        currdd = w1
      end
    parse value in.i with . "DSN=" dsn ","
    dsn = word(strip(dsn),1)
    if dsn <> null then do
      if pos("&",dsn) > 0 then
        call fix_sym
      if left(dsn,1) <> _apost_ then ,
        dsn = ""_apost_""dsn""_apost_""
      proc.w1 = proc.w1 dsn
    end
  end
  If hit = 1 then do
    If w2 == "PROC" then
      test = w3
    else
      test = w2
    test = translate(test," ",",")" "
    test = left(test, min(pos(' ', test), 71) - 1)
    if words(test) > 1 then
      do j = 1 to words(test)
        parse value word(test,j) with symb "=" dsn ","
        sym.symb = strip(dsn)
        syms = syms symb
      end j
    else do
      parse value test with  symb "=" dsn ","
      sym.symb = strip(dsn)
      syms = syms symb
    end
  end
end i
return /* scanjesjcl */

consoleproc:
arg opcmd
$userid = userid()
address TSO 'CONSPROF SOLDISPLAY(NO) UNSOLDISPLAY(NO)'
!consprof = rc = 0
if !consprof then do
  /* Activate CONSOLE first */
  if length($userid) = 8 then
    $userid = left($userid,7)
  consname = left($userid||random(, 10 ** (8 - length($userid))), 8)
  call outtrap('O.')
  address TSO 'CONSOLE ACTIVATE NAME('consname')'
  call outtrap 'OFF'
  !console = rc = 0
  if !console then do
    if rc = 56 then do
      parse var o.2 'CONSOLE NAME IS' consname '.'
      consname = strip(consname)
      /* say 'Console' consname 'already active' */
      !console = 1
    end /* if rc */
    cmdtso = 'CONSOLE SYSCMD('opcmd') CART('consname')'
    address TSO cmdtso
    tsorc = rc
    if tsorc \= 0 then
      call cons_syscmd_failure
    else do
      /*say opcmd 'command issued'*/
      rc=isfcalls('ON')
      isflinelim=1000
      isfscrolltype = 'FINDPREV'
      st = (time('S') - 10 + 86400) // 86400
      isfstarttime = format(st % 3600)':'||,
        right(st // 3600 % 60, 2, '0')':'right(st // 60, 2, '0')
      isffind = substr(opcmd, 2)
      Address SDSF "ISFLOG READ TYPE(SYSLOG)"
      if rc > 4 then do
        say isfmsg
        do ix=1 to isfmsg2.0
          say isfmsg2.ix
        end ix
      end /* if rc */
      else do
        /* say isfline.1 */
        nn =
        do ix=2 to isfline.0 /* Process the returned variables */
          il = isfline.ix
          if ssid == 'JES3' then do
            if word(il, 1) \= consname then
              iterate ix
            w4 = word(il, 4)
            if w4 = 'IAT8952' then do
              parse var il 'PROCLIB' nn .
              nn = substr(strip(nn), 7)
              proc.nn = ""
              procs = space(procs nn)
              iterate ix
            end
            if w4 = 'IAT8954' then do
              parse var il dd ', DSN=' dsn .
              if nn = '' then do
                say 'Missing IAT8952'
                iterate ix
              end
              proc.nn = space(proc.nn "'"dsn"'")
              say 'proc.'nn '=' proc.nn
            end
          end /* if JES3 */
          else do
            if pos('$HASP319', isfline.ix) > 0 then do
              msgtext = substr(isfline.ix, 67)
              if left(msgtext, 1) \== ' ' then do
                parse var msgtext msgid w1 w2 w3
                if datatype(w2) == 'NUM' then
                  iterate ix
                parse var w1 with 'PROCLIB('nn')'
                procs = procs nn
              end /* if ' ' */
              if pos('DD(', msgtext) > 0 then do
                parse var msgtext with 'DSNAME=('dsn','
                proc.nn = proc.nn dsn
              end
              else do
                parse var msgtext with 'VOLSER='vol')'
                proc.nn = proc.nn'/'vol
              end
            end /* if $HASP319 */
          end /* else JES2 */
          /* say isfline.ix */
        end ix
      end
      rc=isfcalls('OFF')
    end /* else tsorc = 0 */
  end /* if \!console */
  else do
    say 'Unable to execute CONSOLE, terminating - RC' rc
    if o.0 > 0 then do #o = 1 to o.0
      say o.#o
    end #o /* if o.0 */
    if rc > xrc then
      xrc = rc
  end /* else */
end /* if consprof */
else do
  say 'CONSPROF RC' rc
end /* else */
return

/* ----------------------------------------------------- *
 * Fix up symbolics in the dsname                        *
 * ----------------------------------------------------- */
Fix_Sym: procedure expose (global_vars) dsn syms sym.
do while pos("&", dsn) > 0
  parse value dsn with _left_ "&" symbol "." _right_
  if pos('(', symbol) > 0 then do
    parse value dsn with _left_ "&" symbol "(" _right_
    _right_ = '('_right_
  end /* if ( */
  syssym = mvsvar("symdef",symbol)
  if syssym <> null then do
    if right(_left_,1) = '.' then
      dsn = _left_""syssym""_right_
    else
      dsn = _left_"."syssym""_right_
  end
  else do
    wp = wordpos(symbol,syms)
    if wp = 0 then
      leave /* while dsn */
    symb = word(syms,wp)
    hlq  = sym.symb
    if left(hlq,1) = _apost_ then
      parse value hlq with (_apost_) hlq (_apost_)
    dsn  = _left_""hlq""_right_
  end
end /* do while pos */
return

/* ---------------------------------- *
 * Fix up symbolics in an expression  *
 * ---------------------------------- */
Fix_Sym1:
arg ?
/* shift the symbols and their values to an array */
symb.0 = words(syms)
do i = 1 to symb.0
  jj = word(syms,i)
  symb.i.1 = word(syms,i)
  symb.i.2 = sym.jj
end i
fixstart = 1
do forever
  p1 = pos("&",?,fixstart)
  if p1 = 0 then
    leave /* forever */
  parse value ? with _left_"&"fixsymb
  if left(fixsymb,1) = "&" then
    p1 = p1 + 1        /* ignore "&&" */
  else do
    _right_ = null
    symdone = "N"
    do r=1 to length(fixsymb) until symdone = "Y"
      if datatype(substr(fixsymb,r,1),"ALPHA") = 0 then do
        _right_ = substr(fixsymb,r)
        if left(_right_,1) = "." then
          _right_ = substr(_right_,2)
        fixsymb = substr(fixsymb,1,r-1)
        symdone = "Y"
      end
    end r
    if length(fixsymb) > 0 then do
      syssym = null
      do symidx = 1 to symb.0
        if fixsymb = symb.symidx.1 then do
          syssym = symb.symidx.2
          leave symidx
        end
      end symidx
      if syssym = null then
        syssym = mvsvar("symdef",fixsymb)
      if syssym <> null then do
        if right(_left_,1) = '.' then
          ? = _left_""syssym""_right_
        else
          ? = _left_"."syssym""_right_
      end
    end
  end
  fixstart = p1 + 1
  if fixstart > length(?) then
    leave /* forever */
end /* do forever */
return ?

/* ----------------------------------------------------- *
 * Fix volser for IPLPARM volume                         *
 * ----------------------------------------------------- */
chk_volser: procedure expose (global_vars)
parse arg unitnbr
stat. = null
_rc_ = outtrap("stat.")
"LISTDATA STATUS UNITNUMBER("strip(UNITNBR)")"
parse var stat.3   "VOLUME" volser "DEVICE" .
_rc_ = outtrap("off")
return strip(volser)

/* --------------------------------------------------------- *
 * Find Library for Proc                                     *
 * --------------------------------------------------------- */
/* -------------------------------- *
 * Echo to the Terminal all DSnames *
 * -------------------------------- */
Echo: Procedure expose (global_vars) ,
                       echo. _my_env_ _sysenv_
if isfunc = 'Y' then
  return
Parse Arg dd dsns
echo.0 = echo.0 + 1
if echo.0 = 1 then do
  hl = 62
  _info_ = copies("=",hl)
  info.1 = "*"left(_info_,hl)"*"
  _info_ = null
  _info_ = _info_ _my_env_
  info.2 = "*"left(_info_,hl)"*"
  _info_ = null
  _info_ = _info_ _sysenv_
  info.3 = "*"left(_info_,hl)"*"
  info.4 = info.1
  _info_ = null
  _info_ = _info_""left("*DD",10)
  _info_ = _info_""left("VOLSER",10)
  _info_ = _info_"DSNAME"
  info.5 = _info_
  _info_ = null
  _info_ = _info_""left("*"copies("-",08),10)
  _info_ = _info_""left(copies("-",06),10)
  _info_ = _info_""copies("-",44)
  info.6 = _info_
  info.0 = 6
  do i = 1 to info.0
     say info.i
  end i
end
dsn.0 = words(dsns)
do i = 1 to dsn.0
  if i = 1 then
    dsn.i.1 = dd
  else
    dsn.i.1 = null
  dsn.i.2 = word(dsns,i)
  sysvolume = null
  _rc_ = listdsi(""dsn.i.2"")
  if _rc_ = 0 then
    dsn.i.3 = sysvolume
  else
    dsn.i.3 = "N/A"
end i
do i = 1 to dsn.0
    say left(dsn.i.1,10)""left(dsn.i.3,10)""dsn.i.2
end i
return /* Echo */

cons_syscmd_failure:
say 'Unable to issue' opcmd 'command'
if o.0 > 0 then do #o = 1 to o.0
  call _say o.#o
end #o /* if o.0 */
if tsorc > xrc then
  xrc = tsorc
return

sub_allocdd: procedure expose (global_vars)
parse arg alloc_dd
select
  when ( symbol("alloc_dd") == "LIT") then
    r_string = alloc_dd
  when ( alloc_dd = null) then
    r_string = alloc_dd
  otherwise
    r_string = null
end
return r_string

sub_init:
/* to get the correct name for MSGID don't use other cmds before */
parse source ,
  rexxenv rexxinv rexxname rexxdd rexxdsn . rexxenv addrspc .
myname = rexxname
if myname = "?" then do
  myname = sysvar("sysicmd")
if length(myname) = 0  then
  myname = sysvar("syspcmd")
end
msgid = left(myname": ",10)

parse value "" with null ddn test mstrjcl proclibs,
 $proc syms procs parmlibs procjobs ,
 opt1 opt2 opt3 opt4 lstprocm isfmsg isfmsg2.
_apost_ = "'"
parse source xenv xtype xmyname xddname xdsname .
_sysplex_ = mvsvar("sysplex")
_sysname_ = mvsvar("sysname")
_sysenv_  = "SYSPLEX="_sysplex_" "
_sysenv_  = space(_sysenv_"LPAR/SYSNAME="_sysname_)
_my_env_  = space(myname" "_version_)
echo.0 = 0
global_vars = "null _apost_ isfunc jes2 jes3"
return 0

consdeact:
if !console then do
  address TSO 'CONSOLE DEACTIVATE'
  consname =
  !console = 0
end /* if consname */
return /* consdeact */

/**************************************************************
* Trap uninitialized variables                                *
***************************************************************/
sub_novalue:
Say ""
Say "Variable" ,
  condition("Description") "undefined in line" sigl":"
Say sourceline(sigl)
if sysvar("sysenv") <> "FORE" then exit 8
say "Report the error in this application along with the",
    "syntax used."
call consdeact
exit 8
